open Ppxlib
open Printf
open Util

module Sig = struct
  type kind = Pure | Http_request
  type param_data = { name : string; descr : string option }

  type param =
    | Labelled of param_data * Ast.core_type
    | Nolabel of Ast.core_type
    | Optional of param_data * Ast.core_type

  type return = Simple of Ast.core_type | Async of Ast.core_type

  type t = {
    name : string;
    params : param list;
    return : return;
    kind : kind;
    descr : string list;
  }

  let param_descr = function
    | Nolabel _
    | Labelled ({ descr = None; _ }, _)
    | Optional ({ descr = None; _ }, _) ->
        None
    | Labelled ({ name; descr = Some descr }, _) ->
        Some (sprintf "%s %s" name descr)
    | Optional ({ name; descr = Some descr }, _) ->
        Some (sprintf "%s %s" name descr)

  let create ?descr kind name params return =
    let descr = descr :: List.map param_descr params |> keep_some in
    { name; params; return; kind; descr }

  let labelled ?descr name type_ = Labelled ({ name; descr }, type_)
  let nolabel type_ = Nolabel type_
  let optional ?descr name type_ = Optional ({ name; descr }, type_)
  let pure ?descr name params ret = create ?descr Pure name params (Simple ret)

  let field_setter ?descr name params ret =
    create ?descr Pure ("set_" ^ name) params (Simple ret)

  (* Here "constants" are actually [unit -> string] functions to satisfy
   * OCaml's recursive module safety requirements.
   *
   * See https://stackoverflow.com/q/4239045 *)
  let constant name = pure name [ Nolabel [%type: unit] ] [%type: string]

  let http_request ?descr name params ret =
    create ?descr Http_request name params (Async ret)

  let make_fun params k =
    let to_ptyp_arrow k = function
      | Nolabel t -> Ast_builder.(ptyp_arrow Nolabel t k)
      | Labelled ({ name; _ }, t) ->
          Ast_builder.(ptyp_arrow (Labelled name) t k)
      | Optional ({ name; _ }, t) ->
          Ast_builder.(ptyp_arrow (Optional name) t k)
    in
    let rec aux acc = function
      | [] -> acc
      | [ ((Optional _ | Labelled _) as p) ] ->
          (* Extra unit param if last one is optional or labelled *)
          to_ptyp_arrow [%type: unit -> [%t k]] p
      | p :: ps -> to_ptyp_arrow (aux acc ps) p
    in
    aux k params

  let to_sig { name; params; return; kind; descr } =
    let params =
      match kind with
      | Pure when params = [] -> [ nolabel [%type: unit] ]
      | Pure -> params
      | Http_request ->
          let ctx = optional "ctx" [%type: Cohttp_lwt_unix.Client.ctx] in
          let headers = optional "headers" [%type: Cohttp.Header.t] in
          let uri = nolabel [%type: Uri.t] in
          params @ [ ctx; headers; uri ]
    in
    let doc =
      match descr with
      | [] -> None
      | descr :: params ->
          let descr =
            descr
            :: List.map (fun d -> sprintf "@param %s" (format_comment d)) params
          in
          Some (Util.ocaml_doc (String.concat "\n" descr))
    in
    let open Ast_builder in
    let name = Located.mk name in
    let return =
      match return with
      | Simple t -> t
      | Async t -> [%type: ([%t t], string) result Lwt.t]
    in
    let type_ = make_fun params return in
    let value_description =
      Ast_builder.value_description ~name ~type_ ~prim:[]
    in
    let pval_attributes = Util.opt_cons doc value_description.pval_attributes in
    { value_description with pval_attributes }
end

module Impl = struct
  type http_verb = Get | Put | Post | Delete | Head | Patch | Options
  type return = Module of string | Type of Ast.core_type

  type kind =
    | Record_constructor
    | Field_getter
    | Field_setter
    | Identity
    | Constant of string
    | Http_request of http_verb * return

  type origin = { location : Swagger_t.location; orig_name : string }
  type param_data = { name : string; type_ : Ast.core_type }

  type param =
    | Labelled of param_data * origin option
    | Nolabel of param_data
    | Optional of param_data * origin option

  let module_ name = Module name
  let type_ name = Type name

  let origin (p : Swagger_t.parameter) =
    { location = p.location; orig_name = p.name }

  let labelled ?origin name type_ = Labelled ({ name; type_ }, origin)
  let nolabel name type_ = Nolabel { name; type_ }
  let optional ?origin name type_ = Optional ({ name; type_ }, origin)

  type t = { name : string; params : param list; kind : kind }

  let create kind name params = { kind; name; params }
  let record_constructor = create Record_constructor
  let field_getter = create Field_getter
  let field_setter = create Field_setter
  let identity = create Identity

  let constant name value =
    create (Constant value) name [ nolabel "()" [%type: unit] ]

  let http_request ~return verb = create (Http_request (verb, return))

  let param_name = function
    | Labelled (p, _) | Nolabel p | Optional (p, _) -> p.name

  let param_type = function
    | Labelled (p, _) | Nolabel p | Optional (p, _) -> p.type_

  let param_location = function
    | Labelled (_, Some origin) -> Some origin.location
    | Labelled (_, None) -> None
    | Optional (_, Some origin) -> Some origin.location
    | Optional (_, None) -> None
    | Nolabel _ -> None

  let http_verb_of_string = function
    | "get" -> Get
    | "put" -> Put
    | "post" -> Post
    | "delete" -> Delete
    | "head" -> Head
    | "patch" -> Patch
    | "options" -> Options
    | op -> failwith ("unknown HTTP verb: " ^ op)

  let record_constructor_body = function
    | [] -> Ast_builder.eunit
    | params ->
        let fields =
          List.fold_left
            (fun acc param ->
              let ident = Ast_builder.Located.lident (param_name param) in
              let exp = Ast_builder.pexp_ident ident in
              (ident, exp) :: acc)
            [] params
        in
        Ast_builder.pexp_record fields None

  (* FIXME: take into account collectionFormat *)

  let assoc_string_with f params = Ast_builder.elist (List.map f params)

  let assoc_opt_string params =
    let string_of name =
      let name = Ast_builder.evar name in
      let conv = function
        | [%type: string] -> name
        | { Ast.ptyp_desc = Ptyp_constr (lident, []); _ } ->
            let conv lident =
              Longident.parse ("string_of_" ^ Longident.name lident)
            in
            let lident = Ast_builder.Located.map conv lident in
            let e = Ast_builder.(pexp_ident lident) in
            [%expr [%e e] [%e name]]
        | _ -> assert false
      in
      function
      | [%type: [%t? t] array] ->
          let conv = conv t in
          [%expr String.concat "," (Array.to_list [%e conv])]
      | t -> conv t
    in
    let string_opt_to_string name =
      let name = Ast_builder.(pexp_ident (Located.lident name)) in
      [%expr match [%e name] with Some s -> s | None -> ""]
    in
    let opt_to_string name = function
      | { Ast.ptyp_desc = Ptyp_constr (lident, []); _ } ->
          let name = Ast_builder.(pexp_ident (Located.lident name)) in
          let conv =
            Ast_builder.type_constr_conv lident ~f:(sprintf "string_of_%s")
              [ [%expr x] ]
          in
          [%expr match [%e name] with Some x -> [%e conv] | None -> ""]
      | _ -> assert false
    in
    assoc_string_with
      (fun p ->
        let orig_name, value =
          match p with
          | Labelled ({ name; type_ = [%type: string] }, Some origin) ->
              (origin.orig_name, Ast_builder.evar name)
          | Labelled ({ name; type_ }, Some origin) ->
              (origin.orig_name, string_of name type_)
          | Labelled ({ name; type_ = [%type: string] }, None) ->
              (name, Ast_builder.evar name)
          | Labelled ({ name; type_ }, None) -> (name, string_of name type_)
          | Optional ({ name; type_ = [%type: string] }, Some origin) ->
              (origin.orig_name, string_opt_to_string name)
          | Optional ({ name; type_ }, Some origin) ->
              (origin.orig_name, opt_to_string name type_)
          | Optional ({ name; type_ }, None) -> (name, opt_to_string name type_)
          | Nolabel _ -> failwith "positional parameters don't go in requests"
        in
        let orig_name = Ast_builder.estring orig_name in
        [%expr [%e orig_name], [%e value]])
      params

  let assoc_string =
    assoc_string_with (fun p ->
        let open Ast_builder in
        let name = param_name p in
        let value =
          let name = evar name in
          match param_type p with
          | [%type: string] -> name
          | { Ast.ptyp_desc = Ptyp_constr (lident, []); _ } ->
              type_constr_conv lident ~f:(sprintf "string_of_%s") [ name ]
          | _ -> assert false
        in
        let name = estring name in
        [%expr [%e name], [%e value]])

  let make_query params =
    params
    |> List.filter (fun p -> param_location p = Some `Query)
    |> assoc_opt_string

  let make_path params =
    let path_params =
      params
      |> List.filter (fun p -> param_location p = Some `Path)
      |> assoc_string
    in
    [%expr
      let path_params = [%e path_params] in
      List.fold_left
        (fun path (name, value) ->
          let re = Re.Pcre.regexp (Printf.sprintf "\\{%s\\}" name) in
          Re.replace_string re ~by:value path)
        (request_path_template ()) path_params]

  let make_headers params =
    params |> List.filter (fun p -> param_location p = Some `Header) |> function
    | [] -> Ast_builder.evar "headers"
    | hs ->
        let headers = assoc_string hs in
        [%expr
          let headers =
            match headers with Some hs -> hs | None -> Header.init ()
          in
          Some (Cohttp.Header.add_list headers [%e headers])]

  let make_body params =
    let body_params =
      params |> List.filter (fun p -> param_location p = Some `Body)
    in
    match body_params with
    | [] -> [%expr None]
    | [ p ] -> (
        let lident path ident =
          let last = Longident.last_exn path in
          let name = Longident.name path in
          String.(sub name 0 (length name - length last) ^ ident)
        in
        let name = Ast_builder.evar (param_name p) in
        let conv =
          let convs = function
            | { Ast.ptyp_desc = Ptyp_constr ({ txt; _ }, []); _ } ->
                ( Ast_builder.(
                    pexp_ident (Located.lident (lident txt "yojson_of_t"))),
                  Ast_builder.(
                    pexp_ident (Located.lident (lident txt "t_of_yojson"))) )
            | _ -> assert false
          in
          match param_type p with
          | [%type: [%t? t] list] ->
              let yojson_of_t, _ = convs t in
              [%expr `List (List.map [%e yojson_of_t] [%e name])]
          | { Ast.ptyp_desc = Ptyp_constr _; _ } as t ->
              let yojson_of_t, _t_of_yojson = convs t in
              [%expr [%e yojson_of_t] [%e name]]
          | _ -> assert false
        in
        match p with
        | Optional _ ->
            [%expr
              Option.map
                (fun body ->
                  Cohttp_lwt.Body.of_string (Yojson.Safe.to_string [%e conv]))
                body]
        | _ ->
            [%expr
              Some (Cohttp_lwt.Body.of_string (Yojson.Safe.to_string [%e conv]))]
        )
    | _ -> failwith "Val.Impl.make_body: there can be only one body parameter"

  let string_of_http_verb = function
    | Get -> "get"
    | Put -> "put"
    | Post -> "post"
    | Delete -> "delete"
    | Head -> "head"
    | Patch -> "patch"
    | Options -> "options"

  let client_function_of_http_verb = function
    | Options -> [%expr Cohttp_lwt_unix.Client.call `OPTIONS]
    | verb ->
        Ast_builder.evar ("Cohttp_lwt_unix.Client." ^ string_of_http_verb verb)

  let continuation_of_http_verb k = function
    | Head ->
        [%expr
          fun resp ->
            let code =
              resp |> Cohttp_lwt_unix.Response.status
              |> Cohttp.Code.code_of_status
            in
            let body = "Ok" in
            [%e k]]
    | _ ->
        [%expr
          fun (resp, body) ->
            let code =
              resp |> Cohttp_lwt_unix.Response.status
              |> Cohttp.Code.code_of_status
            in
            Cohttp_lwt.Body.to_string body >>= fun body -> [%e k]]

  let make_response_code ?(body_param = false) ~return verb params =
    let params =
      let open Ast in
      (Optional "ctx", [%expr ctx])
      :: (Optional "headers", [%expr headers])
      :: (if body_param then List.cons (Optional "body", make_body params)
         else Fun.id)
           [ (Nolabel, [%expr uri]) ]
    in
    let client_fun =
      Ast_builder.pexp_apply (client_function_of_http_verb verb) params
    in
    let return =
      (* FIXME: handle the response schema. *)
      match return with
      | Module module_name ->
          let t_of_yojson =
            Ast_builder.(
              pexp_ident (Located.lident (module_name ^ ".t_of_yojson")))
          in
          [%expr
            let json = Yojson.Safe.from_string body in
            Lwt.return
              (if code >= 200 && code < 300 then Ok ([%e t_of_yojson] json)
              else Error body)]
      | Type type_name ->
          let conv_result =
            let conv str = function
              | { Ast.ptyp_desc = Ptyp_constr (lident, _); _ } ->
                  let conv lident =
                    Longident.parse (str (Longident.name lident))
                  in
                  let lident = Ast_builder.Located.map conv lident in
                  Ast_builder.(pexp_ident lident)
              | _ -> assert false
            in
            match type_name with
            | [%type: unit] -> [%expr ()]
            | [%type: string] -> [%expr body]
            | [%type: [%t? t] list] ->
                let conv = conv (sprintf "%s_of_yojson") t in
                [%expr
                  Yojson.Safe.(Util.convert_each [%e conv] (from_string body))]
            | { Ast.ptyp_desc = Ptyp_constr _; _ } as t ->
                let conv = conv (sprintf "%s_of_yojson") t in
                [%expr [%e conv] (Yojson.Safe.from_string body)]
            | _ -> assert false
          in
          [%expr
            ignore body;
            Lwt.return
              (if code >= 200 && code < 300 then Ok [%e conv_result]
              else Error (string_of_int code))]
    in
    let result = continuation_of_http_verb return verb in
    [%expr
      let open Lwt.Infix in
      [%e client_fun] >>= [%e result]]

  let build_http_request ?body_param ~return verb params =
    let query = make_query params
    and path = make_path params
    and headers = make_headers params
    and response_code = make_response_code ?body_param ~return verb params in
    [%expr
      let query = [%e query] in
      let path = [%e path] in
      let full_path = Uri.path uri ^ path in
      let uri = Uri.with_path uri full_path in
      let uri =
        Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
      in
      let headers = [%e headers] in
      [%e response_code]]

  let http_get ~return = build_http_request ~return Get
  let http_put ~return = build_http_request ~body_param:true ~return Put
  let http_post ~return = build_http_request ~body_param:true ~return Post
  let http_delete ~return = build_http_request ~body_param:true ~return Delete
  let http_head ~return = build_http_request ~return Head
  let http_patch ~return = build_http_request ~body_param:true ~return Patch
  let http_options ~return = build_http_request ~return Options

  let body t =
    match t.kind with
    | Record_constructor -> record_constructor_body t.params
    | Field_getter -> Ast_builder.evar ("t." ^ t.name)
    | Field_setter ->
        let ident = Ast_builder.Located.lident t.name in
        let exp = Ast_builder.pexp_ident ident in
        Ast_builder.pexp_record [ (ident, exp) ] (Some [%expr t])
    | Identity -> [%expr t]
    | Constant v -> Ast_builder.estring v
    | Http_request (Get, return) -> http_get ~return t.params
    | Http_request (Put, return) -> http_put ~return t.params
    | Http_request (Post, return) -> http_post ~return t.params
    | Http_request (Delete, return) -> http_delete ~return t.params
    | Http_request (Head, return) -> http_head ~return t.params
    | Http_request (Patch, return) -> http_patch ~return t.params
    | Http_request (Options, return) -> http_options ~return t.params

  let make_fun params k =
    let to_pexp_fun k = function
      | Labelled (p, _) ->
          Ast_builder.(pexp_fun (Labelled p.name) None (pvar p.name) k)
      | Nolabel p -> Ast_builder.(pexp_fun Nolabel None (pvar p.name) k)
      | Optional (p, _) ->
          Ast_builder.(pexp_fun (Optional p.name) None (pvar p.name) k)
    in
    let rec aux acc = function
      | [] when params = [] -> [%expr fun () -> ()]
      | [] -> acc
      | [ ((Optional _ | Labelled _) as p) ] ->
          (* extra () param if last one is optional or labelled *)
          to_pexp_fun [%expr fun () -> [%e k]] p
      | p :: ps -> to_pexp_fun (aux acc ps) p
    in
    aux k params

  let to_impl ({ kind; name; params } as value) =
    let params =
      match kind with
      | Http_request _ ->
          let ctx = optional "ctx" [%type: Cohttp_lwt_unix.Client.ctx] in
          let headers = optional "headers" [%type: Cohttp.Header.t] in
          let uri = nolabel "uri" [%type: Uri.t] in
          params @ [ ctx; headers; uri ]
      | _ -> params
    in
    let name = match kind with Field_setter -> "set_" ^ name | _ -> name in
    let name = Ast_builder.pvar name in
    let e = make_fun params (body value) in
    [%stri let [%p name] = [%e e]]
end

type t = { signature : Sig.t; implementation : Impl.t }

let create signature implementation = { signature; implementation }
let signature t = t.signature
let implementation t = t.implementation

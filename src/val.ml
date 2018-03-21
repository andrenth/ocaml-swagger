open Printf
open Util

module Sig = struct
  type kind =
    | Pure
    | Http_request

  type param_data =
    { name : string
    ; descr : string option
    }

  type param =
    | Unnamed of string
    | Named of param_data * string
    | Optional of param_data * string

  type return =
    | Simple of string
    | Async of string

  type t =
    { name      : string
    ; params    : param list
    ; return    : return
    ; kind      : kind
    ; descr     : string list
    }

  let param_descr = function
    | Unnamed _
    | Named ({ descr = None }, _)
    | Optional ({ descr = None }, _) ->
        None
    | Named ({ name; descr = Some descr }, _) ->
        Some (sprintf "%s %s" name descr)
    | Optional ({ name; descr = Some descr }, _) ->
        Some (sprintf "%s %s" name descr)

  let create ?descr kind name params return =
    let descr = descr :: List.map param_descr params |> keep_some in
    { name; params; return; kind; descr }

  let named ?descr name type_ = Named ({ name; descr }, type_)
  let unnamed type_ = Unnamed type_
  let optional ?descr name type_ = Optional ({ name; descr }, type_)

  let pure ?descr name params ret =
    create ?descr Pure name params (Simple ret)

  (* Here "constants" are actually [unit -> string] functions to satisfy
   * OCaml's recusive module safety requirements.
   *
   * See https://stackoverflow.com/q/4239045 *)
  let constant ?descr name =
    pure name [Unnamed "unit"] "string"

  let http_request ?descr name params ret =
    create ?descr Http_request name params (Async ret)

  let param_to_string = function
    | Named ({ name }, type_) -> sprintf "%s:%s" name type_
    | Unnamed type_ -> type_
    | Optional ({ name }, type_) -> sprintf "?%s:%s" name type_

  let return_to_string = function
    | Simple t -> t
    | Async t -> sprintf "(%s, string) result Lwt.t" t

  let params_to_string params =
    let rec go acc = function
      | [] -> acc
      (* extra unit param if last one is optional *)
      | [Optional _ as p] -> sprintf "%s%s -> unit -> " acc (param_to_string p)
      | p :: ps -> go (sprintf "%s%s -> " acc (param_to_string p)) ps in
    go "" params

  let to_string ?(indent = 0) { name; params; return; kind; descr } =
    let pad = String.make indent ' ' in
    let params =
      match kind with
      | Pure -> params
      | Http_request -> params @ [named "uri" "Uri.t"] in
    let doc =
      match descr with
      | [] -> ""
      | _ ->
          let comment_pad = pad ^ String.make 3 ' ' in
          let descr =
            List.mapi
              (fun i d ->
                let d =
                  d
                  |> String.split_on_char ' '
                  |> List.map snake_case
                  |> String.concat " " in
                 if i = 0
                 then sprintf "%s(** %s\n" pad (String.capitalize_ascii d)
                 else sprintf "%s @param %s" comment_pad d)
              descr in
          "\n" ^ String.concat "\n" descr ^ " *)\n" in
    sprintf "%s%sval %s : %s%s\n"
      doc
      pad
      name
      (params_to_string params)
      (return_to_string return)
end

module Impl = struct
  type http_verb = Get | Put | Post | Delete | Head | Patch | Options

  type return =
    | Module of string
    | Type of string

  type kind =
    | Record_constructor
    | Record_accessor
    | Identity
    | Constant of string
    | Http_request of http_verb * return
    | Derived

  type origin =
    { location : Swagger_j.location
    ; orig_name : string
    }

  type param_data =
    { name : string
    ; type_ : string
    }

  type param =
    | Named of param_data * origin option
    | Unnamed of param_data
    | Optional of param_data * origin option

  let origin (p : Swagger_j.parameter) =
    { location = p.location; orig_name = p.name }

  let named ?origin name type_ =
    Named ({ name; type_ }, origin)

  let unnamed name type_ =
    Unnamed { name; type_ }

  let optional ?origin name type_ =
    Optional ({ name; type_ }, origin)

  type t =
    { name   : string
    ; params : param list
    ; kind   : kind
    }

  let create kind name params = { kind; name; params }

  let record_constructor = create Record_constructor
  let record_accessor = create Record_accessor
  let identity = create Identity
  let constant name value = create (Constant value) name [unnamed "()" "unit"]
  let http_request ~return verb = create (Http_request (verb, return))
  let derived = create Derived "" []

  let param_name = function
    | Named (p, _) | Unnamed p | Optional (p, _) -> p.name

  let param_type = function
    | Named (p, _) | Unnamed p | Optional (p, _) -> p.type_

  let param_orig_name = function
    | Named (_, Some origin) -> Some origin.orig_name
    | Optional (_, Some origin) -> Some origin.orig_name
    | Named (_, None) | Optional (_, None) | Unnamed _ -> None

  let param_location = function
    | Named (_, Some origin) -> Some origin.location
    | Named (_, None) -> None
    | Optional (_, Some origin) -> Some origin.location
    | Optional (_, None) -> None
    | Unnamed _ -> None

  let is_optional = function
    | Optional _ -> true
    | _ -> false

  let http_verb_of_string = function
    | "get"     -> Get
    | "put"     -> Put
    | "post"    -> Post
    | "delete"  -> Delete
    | "head"    -> Head
    | "patch"   -> Patch
    | "options" -> Options
    | op -> failwith ("unknown HTTP verb: " ^ op)

  let record_constructor_body ~pad params =
    let param_names = List.map param_name params in
    sprintf "%s{ %s }" pad (String.concat "; " param_names)

  let assoc_string_with f params =
    params
    |> List.map f
    |> String.concat "; "
    |> sprintf "[%s]"

  let assoc_opt_string =
    let string_of = sprintf "string_of_%s %s" in
    let string_opt_to_string =
      sprintf {| match %s with Some s -> s | None -> "" |} in
    let opt_to_string =
      sprintf {| match %s with Some x -> string_of_%s x | None -> "" |} in
    assoc_string_with
      (fun p ->
        let orig_name, value =
          match p with
          | Named ({ name; type_ = "string"}, Some origin) ->
              (origin.orig_name, name)
          | Named ({ name; type_ }, Some origin) ->
              (origin.orig_name, string_of type_ name)
          | Named ({ name; type_ = "string"}, None) ->
              (name, name)
          | Named ({ name; type_ }, None) ->
              (name, string_of type_ name)
          | Optional ({ name; type_ = "string"}, Some origin) ->
              (origin.orig_name, string_opt_to_string name)
          | Optional ({ name; type_ }, Some origin) ->
              (origin.orig_name, opt_to_string name type_)
          | Optional ({ name; type_ }, None) ->
              (name, opt_to_string name type_)
          | Unnamed _ ->
              failwith "unnamed parameters don't go in requests" in
        sprintf "(\"%s\", %s)" orig_name value)

  let assoc_string =
    assoc_string_with
      (fun p ->
        let name = param_name p in
        let type_ = param_type p in
        let value =
          if type_ = "string"
            then name
            else sprintf "string_of_%s %s" type_ name in
        sprintf "(\"%s\", %s)" name value)

  let make_query params =
    params
    |> List.filter (fun p -> param_location p = Some `Query)
    |> assoc_opt_string

  let is_space = function
    | ' ' | '\012' | '\n' | '\r' | '\t' -> true
    | _ -> false

  let trim_at_most n s =
    let n = min n (String.length s) in
    let rec go k s =
      if s = "" then s
      else if k < n then
        let last = String.length s - 1 in
        if is_space s.[0] then
          let len = max 0 (last - if is_space s.[last] then 1 else 0) in
          go (k + 1) (String.sub s 1 len)
        else
          s
      else
        s in
    go 0 s

  let make_path params =
    let path_params =
      params
      |> List.filter (fun p -> param_location p = Some `Path)
      |> assoc_string in
    String.trim @@ sprintf {|
          let open Printf in
          let path_params = %s in
          List.fold_left
            (fun path (name, value) ->
              let re = Re.Pcre.regexp (sprintf "\\{%%s\\}" name) in
              Re.replace_string re ~by:value path)
            (request_path_template ())
            path_params
    |} path_params

  let make_headers params =
    params
    |> List.filter (fun p -> param_location p = Some `Header)
    |> function
       | [] ->
           "None"
       | hs ->
           sprintf {| Some (Header.add_list (Head.init ()) %s) |}
             (assoc_string hs)

  let make_body params =
    let body_params =
      params
      |> List.filter (fun p -> param_location p = Some `Header) in
    match body_params with
    | [] -> "None"
    | [p] ->
        let to_yojson =
          param_type p
          |> String.split_on_char '.'
          |> unsnoc
          |> some
          |> fst
          |> String.concat "."
          |> sprintf "%s.to_yojson" in
        sprintf {| Some (Body.of_string (Yojson.to_string (%s %s))) |}
          to_yojson
          (param_name p)
    | _ ->
        failwith "Val.Impl.make_body: there can be only one body parameter"

  let string_of_http_verb = function
    | Get     -> "get"
    | Put     -> "put"
    | Post    -> "post"
    | Delete  -> "delete"
    | Head    -> "head"
    | Patch   -> "patch"
    | Options -> "options"

  let client_function_of_http_verb = function
    | Options -> "call `OPTIONS"
    | verb -> string_of_http_verb verb

  let continuation_of_http_verb = function
    | Head -> "fun resp ->let code = resp |> Response.status |> Code.code_of_status in\nlet body = \"Ok\" in"
    | _ -> "fun (resp, body) ->\nlet code = resp |> Response.status |> Code.code_of_status in\nBody.to_string body >>= fun body ->"

  let build_http_request ~pad ?(body_param = false) ~return verb params =
    let client_fun = client_function_of_http_verb verb in
    let result_cont = continuation_of_http_verb verb in
    let body_param =
      if body_param
      then sprintf "?body:(%s) " (make_body params)
      else "" in
    let response_code =
      match return with
      | Module module_name -> sprintf {|
        Client.%s %s?headers uri >>= %s
        let str = Yojson.Safe.from_string body in
        Lwt.return (if code >= 200 && code < 300 then %s.of_yojson str else Error body)
      |} client_fun body_param result_cont module_name
      | Type type_name ->
          let conv_result = function
            | "unit"   -> "()"
            | "string" -> "body"
            | other    -> sprintf "(%s_of_string body)" other in
          sprintf {|
        Client.%s %s?headers uri >>= %s
        Lwt.return (if code >= 200 && code < 300 then Ok %s else Error (string_of_int code))
      |} client_fun body_param result_cont (conv_result type_name) in
    let code =
      String.trim @@ sprintf {|
        let open Lwt.Infix in
        let open Cohttp in
        let open Cohttp_lwt_unix in
        let module Body = Cohttp_lwt.Body in
        let query = %s in
        let path =
          %s in
        let uri = Uri.with_path uri path in
        let uri = Uri.with_query' uri (List.filter (fun (k, v) -> v <> "") query) in
        let headers = %s in
        %s
      |} (make_query params) (make_path params) (make_headers params) (String.trim response_code) in
    code
    |> String.split_on_char '\n'
    |> List.map (fun l -> sprintf "%s%s" pad (trim_at_most 10 l))
    |> String.concat "\n"

  let http_get     ~return = build_http_request                  ~return Get
  let http_put     ~return = build_http_request ~body_param:true ~return Put
  let http_post    ~return = build_http_request ~body_param:true ~return Post
  let http_delete  ~return = build_http_request ~body_param:true ~return Delete
  let http_head    ~return = build_http_request                  ~return Head
  let http_patch   ~return = build_http_request ~body_param:true ~return Patch
  let http_options ~return = build_http_request                  ~return Options

  let body_to_string ?(indent = 0) t =
    let pad = String.make indent ' ' in
    match t.kind with
    | Record_constructor -> record_constructor_body ~pad t.params
    | Record_accessor -> sprintf "%st.%s" pad t.name
    | Identity -> sprintf "%st" pad
    | Constant v -> sprintf "%s\"%s\"" pad v
    | Http_request (Get, return) -> http_get ~pad ~return t.params
    | Http_request (Put, return) -> http_put ~pad ~return t.params
    | Http_request (Post, return) -> http_post ~pad ~return t.params
    | Http_request (Delete, return) -> http_delete ~pad ~return t.params
    | Http_request (Head, return) -> http_head ~pad ~return t.params
    | Http_request (Patch, return) -> http_patch ~pad ~return t.params
    | Http_request (Options, return) -> http_options ~pad ~return t.params
    | Derived ->
        failwith "Val.Impl.body_to_string: derived functions have no body"

  let param_to_string = function
    | Named (p, _) -> sprintf "~%s" p.name
    | Unnamed p -> p.name
    | Optional (p, _) -> sprintf "?%s" p.name

  let params_to_string params =
    let rec go acc = function
      | [] -> acc
      | [Optional _ as p] -> sprintf "%s%s () " acc (param_to_string p)
      | p::ps -> go (sprintf "%s%s " acc (param_to_string p)) ps in
    go "" params

  let to_string ?uri ?(indent = 0) ({ kind; name; params } as value) =
    let pad = String.make indent ' ' in
    let params =
      match kind with
      | Http_request _ -> params @ [named "uri" "Uri.t"]
      | _ -> params in
    match kind with
    | Derived -> ""
    | _ ->
        sprintf "%slet %s %s=\n%s\n"
          pad
          name
          (params_to_string params)
          (body_to_string ~indent:(indent + 2) value)
end

type t =
  { signature : Sig.t
  ; implementation : Impl.t
  }

let create signature implementation = { signature; implementation }
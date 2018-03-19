open Printf

let unsnoc l =
  let rec go acc = function
    | [] -> None
    | [x] -> Some (List.rev acc, x)
    | x::xs -> go (x::acc) xs in
  go [] l

let some = function
  | Some x -> x
  | None -> failwith "some: None"

let default z = function
  | Some x -> x
  | None -> z

let rec keep_some = function
  | [] -> []
  | Some x :: xs -> x :: keep_some xs
  | None :: xs -> keep_some xs

let snake_case =
  let re1 = Re_pcre.regexp "([A-Z]+)([A-Z][a-z]{2,})" in
  let re2 = Re_pcre.regexp "([a-z0-9])([A-Z])" in
  let re3 = Re.compile (Re_pcre.re "-") in
  let underscore re s =
    let replace groups = sprintf "%s_%s" (Re.Group.get groups 1) (Re.Group.get groups 2) in
    Re.replace re replace s in
  fun s ->
    let s = underscore re1 s in
    let s = underscore re2 s in
    let s = Re.replace_string re3 ~by:"_" s in
    String.lowercase_ascii s

let rec item_kind_to_string (items : Swagger_j.items option) = function
  | `String  -> "string"
  | `Number  -> "float"
  | `Integer -> "int"
  | `Boolean -> "bool"
  | `Array   ->
      let open Swagger_j in
      match items with
      | Some is -> item_kind_to_string is.items is.kind ^ " list"
      | None -> failwith "item_kind_to_string: array type must have an 'items' field"

module Type = struct
  module Sig = struct
    type t =
      | Abstract of string
      | Phantom of string

    let abstract name = Abstract name
    let phantom name = Phantom name

    let to_string ?(indent = 0) t =
      let pad = String.make indent ' ' in
      let name, attr =
        match t with
        | Abstract name -> name, " [@@deriving yojson]\n"
        | Phantom name -> name, "" in
      sprintf "%stype %s%s\n" pad name attr
  end

  module Impl = struct
    type record_field =
      { name : string
      ; orig_name : string
      ; type_ : string
      }

    type t =
      | Alias of string * string
      | Record of string * record_field list
      | Phantom of string

    let alias name target = Alias (name, target)
    let record name fields = Record (name, fields)
    let phantom name = Phantom name

    let to_string ?(indent = 0) t =
      let pad = String.make indent ' ' in
      match t with
      | Phantom name -> sprintf "%stype %s\n" pad name
      | Alias (name, target) -> sprintf "%stype %s = %s [@@deriving yojson]\n" pad name target
      | Record (name, fields) ->
          let s =
            List.fold_left
              (fun acc { name; orig_name; type_ } ->
                 let attr =
                   if name = orig_name then ""
                   else sprintf " [@key \"%s\"]" orig_name in
                 sprintf "%s %s : %s%s;" acc name type_ attr)
              ""
              fields in
          sprintf "%stype %s = {%s } [@@deriving yojson]\n" pad name s
  end

  type t =
    { signature : Sig.t
    ; implementation : Impl.t
    }

  let create signature implementation = { signature; implementation }
end

module Val = struct
  module Sig = struct
    type kind =
      | Pure
      | Http_request

    type param =
      | Unnamed of string
      | Named of string * string * string option
      | Optional of string * string * string option

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
      | Unnamed _ -> None
      | Named (_, _, d) -> d
      | Optional (_, _, d) -> d

    let create ?descr kind name params return =
      let descr = descr :: List.map param_descr params |> keep_some in
      { name; params; return; kind; descr }

    let named ?descr name type_ = Named (name, type_, descr)
    let unnamed type_ = Unnamed type_
    let optional ?descr name type_ = Optional (name, type_, descr)

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
      | Named (n, t, _) -> sprintf "%s:%s" n t
      | Unnamed t -> t
      | Optional (n, t, _) -> sprintf "?%s:%s" n t

    let return_to_string = function
      | Simple t -> t
      | Async t -> sprintf "(%s, string) result Lwt.t" t

    let params_to_string params =
      let rec go acc = function
        | [] -> acc
        | [Optional _ as p] -> sprintf "%s%s -> unit -> " acc (param_to_string p) (* extra unit param if last is optional *)
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
                   let d = String.capitalize_ascii d in
                   if i = 0 then sprintf "%s(** %s\n" pad d
                   else sprintf "@param %s %s" comment_pad d)
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

    let origin (p : Swagger_j.parameter_or_reference) =
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
      let string_opt_to_string = sprintf {| match %s with Some s -> s | None -> "" |} in
      let opt_to_string = sprintf {| match %s with Some x -> string_of_%s x | None -> "" |} in
      assoc_string_with
        (fun p ->
          let orig_name, value =
            match p with
            | Named ({ name; type_ = "string"}, Some origin) -> (origin.orig_name, name)
            | Named ({ name; type_ }, Some origin) -> (origin.orig_name, string_of type_ name)
            | Named ({ name; type_ = "string"}, None) -> (name, name)
            | Named ({ name; type_ }, None) -> (name, string_of type_ name)
            | Optional ({ name; type_ = "string"}, Some origin) -> (origin.orig_name, string_opt_to_string name)
            | Optional ({ name; type_ }, Some origin) -> (origin.orig_name, opt_to_string name type_)
            | Optional ({ name; type_ }, None) -> (name, opt_to_string name type_)
            | Unnamed _ -> failwith "unnamed parameters don't go in requests" in
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
                let re = Re_pcre.regexp (sprintf "\\{%%s\\}" name) in
                Re.replace_string re ~by:value path)
              (request_path_template ())
              path_params
      |} path_params

    let make_headers params =
      params
      |> List.filter (fun p -> param_location p = Some `Header)
      |> function
         | [] -> "None"
         | hs -> sprintf {| Some (Header.add_list (Head.init ()) %s) |} (assoc_string hs)

    let make_body params =
      let body_params = params |> List.filter (fun p -> param_location p = Some `Header) in
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
          sprintf {| Some (Body.of_string (Yojson.to_string (%s %s))) |} to_yojson (param_name p)
      | _ -> failwith "Val.Impl.make_body: there can be only one body parameter"

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
      | Derived -> failwith "Val.Impl.body_to_string: derived functions have no body"

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
end

module StringMap = Map.Make (struct
  type t = string
  let compare = compare
end)

module StringSet = Set.Make (struct
  type t = string
  let compare = compare
end)

let strip_base base path =
  let plen = String.length path in
  let blen = String.length base in
  if plen >= blen then
    let pref = String.sub path 0 blen in
    if String.lowercase_ascii base = String.lowercase_ascii pref then
      String.sub path blen (plen - blen)
    else
      path
  else
    path

module Mod = struct
  type t =
    { name : string
    ; path : string list
    ; types : Type.t list
    ; values : Val.t list
    ; submodules : t StringMap.t
    ; recursive : bool
    }

  let module_name s =
    let s =
      let last = String.length s - 1 in
      if s.[0] = '{' && s.[last] = '}' then "By_" ^ String.sub s 1 (last - 1)
      else s in
    s
    |> snake_case
    |> String.capitalize_ascii
    |> String.split_on_char '.'
    |> List.hd

  let create ~name ?(recursive = false) ?(path = []) ?(types = []) ?(submodules = StringMap.empty) ?(values = []) () =
    { name = module_name name
    ; path = List.map module_name path
    ; types
    ; values
    ; submodules
    ; recursive
    }

  let empty name ?(recursive = false) ?(path = []) () =
    create ~name ~recursive ~path ()

  let with_values name ?(recursive = false) ?(path = []) values =
    create ~name ~recursive ~path ~values ()

  let name m = m.name

  let submodules m =
    m.submodules
    |> StringMap.bindings
    |> List.map snd

  let add_type t m =
    { m with types = t :: m.types }

  let add_val v m =
    { m with values = v :: m.values }

  let add_types ts m =
    { m with types = m.types @ ts }

  let add_vals vs m =
    { m with values = m.values @ vs }

  let add_mod subm m =
    { m with submodules = StringMap.add subm.name subm m.submodules }

  let map_submodules f m =
    { m with submodules = StringMap.map f m.submodules }

  let has_submodules m =
    StringMap.is_empty m.submodules

  let find_submodule name m =
    StringMap.find_opt (module_name name) m.submodules

  let iter f m =
    f m;
    StringMap.iter
      (fun _name sub -> f sub)
      m.submodules

  let path m =
    m.path

  let qualified_name m =
    match m.path with
    | [] -> m.name
    | p -> sprintf "%s.%s" (String.concat "." m.path) m.name

  let qualified_path m =
    m.path @ [m.name]

  let rec sig_to_string ?(indent = 0) m =
    let pad = String.make indent ' ' in
    let submods =
      m.submodules
      |> StringMap.bindings
      |> List.fold_left
           (fun acc (name, m) ->
             let s = sig_to_string ~indent:(indent + 2) m in
             (* Definitions first to simplify references *)
             if name = "Definitions" then s ^ acc
             else acc ^ s)
           "" in
    sprintf "%smodule%s%s : sig\n%s%s%s%send\n"
      pad
      (if m.recursive then " rec " else " ")
      m.name
      submods
      (String.concat "\n" (List.map (fun t -> Type.Sig.to_string ~indent:(indent + 2) t.Type.signature) m.types))
      (String.concat "" (List.map (fun v -> Val.Sig.to_string ~indent:(indent + 2) v.Val.signature) m.values))
      pad

  let rec impl_to_string ?(indent = 0) m =
    let pad = String.make indent ' ' in
    let submods =
      m.submodules
      |> StringMap.bindings
      |> List.fold_left
           (fun acc (name, m) ->
             let s = impl_to_string ~indent:(indent + 2) m in
             (* Definitions first to simplify references *)
             if name = "Definitions" then s ^ acc
             else acc ^ s)
           "" in
    let decl =
      if m.recursive
      then ""
      else sprintf "%smodule %s " pad m.name in

    sprintf "%s= struct\n%s%s%s%send\n"
      decl
      submods
      (String.concat "\n" (List.map (fun t -> Type.Impl.to_string ~indent:(indent + 2) t.Type.implementation) m.types))
      (String.concat "" (List.map (fun v -> Val.Impl.to_string ~indent:(indent + 2) v.Val.implementation) m.values))
      pad

  let to_string ?indent m =
    sprintf "%s %s"
      (sig_to_string ?indent m)
      (impl_to_string ?indent m)
end

let split_ref ref =
  ref
  |> String.split_on_char '.'
  |> List.filter ((<>)"")
  |> List.map Mod.module_name

let reference_module_path ~reference_base ~reference_root ref =
  let path =
    ref
    |> strip_base reference_base
    |> split_ref in
  Mod.qualified_name reference_root :: path

let reference_module ~reference_base ~reference_root ref =
  reference_module_path ~reference_base ~reference_root ref
  |> String.concat "."

let reference_type ~reference_base ~reference_root ref =
  reference_module ~reference_base ~reference_root ref ^ ".t"

module Schema = struct
  type t =
    { raw            : Swagger_j.schema
    ; reference_base : string
    ; reference_root : Mod.t
    }

  let create ~reference_base ~reference_root raw =
    { raw; reference_base; reference_root }

  let reference t =
    t.raw.ref

  let rec kind_to_string t =
    let reference_base = t.reference_base in
    let reference_root = t.reference_root in
    match t.raw.ref with
    | Some r -> reference_type ~reference_base ~reference_root r
    | None ->
        match some t.raw.kind with
        | `String  -> "string"
        | `Number  -> "float"
        | `Integer -> "int"
        | `Boolean -> "bool"
        | `Object  ->
            let open Swagger_j in
            (match t.raw.additional_properties with
            | Some props -> sprintf "(string * %s) list" (kind_to_string (create ~reference_base ~reference_root props))
            | None -> failwith "Schema.kind_to_string: object without additional_properties")
        | `Array   ->
            let open Swagger_j in
            match t.raw.items with
            | Some s -> kind_to_string (create ~reference_base ~reference_root s) ^ " list"
            | None -> failwith "Schema.kind_to_string: array type must have an 'items' field"

  let to_string t =
    let reference_base = t.reference_base in
    let reference_root = t.reference_root in
    match t.raw.ref with
    | Some r -> reference_type ~reference_base ~reference_root r
    | None -> kind_to_string t
end

module Param = struct
  type t = Swagger_j.parameter_or_reference

  let rec kind_to_string (p : t) =
    match some p.kind with
    | `String  -> "string"
    | `Number  -> "float"
    | `Integer -> "int"
    | `Boolean -> "bool"
    | `File -> "file"
    | `Array   ->
        let open Swagger_j in
        match p.items with
        | Some items -> item_kind_to_string items.items items.kind ^ " array"
        | None -> failwith "Param.kind_to_string: array type must have an 'items' field"

  let is_keyword = function
    | "object"
    | "to"
    | "type" -> true
    | _ -> false

  let name n =
    let n =
      if n.[0] = '$' then String.sub n 1 (String.length n - 1)
      else n in
    let n = snake_case n in
    if is_keyword n then n ^ "_"
    else n

  let prefix_strings required =
    if required
    then ("", "~")
    else ("?", "?")

  let kind = function
    | true -> `Named
    | false -> `Optional

  let string_of_location = function
    | `Query    -> "query"
    | `Header   -> "header"
    | `Path     -> "path"
    | `FormData -> "formData"
    | `Body     -> "body"

  let create ?(duplicate = false) ~reference_base ~reference_root (p : t) =
    let t =
      match p.location with
      | `Body -> Schema.to_string (Schema.create ~reference_base ~reference_root (some p.schema))
      | _     -> kind_to_string p in
    let n =
      let n = name p.name in
      let loc = string_of_location p.location in
      if duplicate && n <> loc
      then sprintf "%s_%s" loc n
      else n in
    let descr =
      match p.description with
      | Some d -> Some (sprintf "%s %s" n d)
      | None -> None in
    if p.required
    then (Val.Sig.named ?descr n t, Val.Impl.named n t ~origin:(Val.Impl.origin p))
    else (Val.Sig.optional ?descr n t, Val.Impl.optional n t ~origin:(Val.Impl.origin p))
end

let merge_params (ps1 : Swagger_j.parameter_or_reference list) (ps2 : Swagger_j.parameter_or_reference list) =
  let open Swagger_j in
  let rec merge acc = function
    | [] -> acc
    | (p : Swagger_j.parameter_or_reference)::ps ->
        if List.exists (fun (q : Swagger_j.parameter_or_reference) -> p.name = q.name) acc
        then merge acc ps
        else merge (p::acc) ps in
  merge ps2 ps1

let operation_params = function
  | None, None -> []
  | Some ps, None -> ps
  | None, Some ps -> ps
  | Some ps, Some ps' -> merge_params ps ps'

let reference_module_and_type ~reference_base ~reference_root r =
  let ref_module = reference_module ~reference_base ~reference_root r in
  let ref_type = sprintf "%s.t" ref_module in
  (Some ref_module, ref_type)

let resp_type ~reference_base ~reference_root (resp : Swagger_j.response_or_reference) =
  match resp.ref, resp.schema with
  | None, None -> (None, "unit")
  | Some _, Some _ -> failwith "response cannot be a reference and a schema simultaneously"
  | Some r, None -> reference_module_and_type ~reference_base ~reference_root r
  | None, Some s ->
      let s = Schema.create ~reference_base ~reference_root s in
      match Schema.reference s with
      | Some r -> reference_module_and_type ~reference_base ~reference_root r
      | None -> (None, Schema.to_string s)

let rec return_type ~reference_root ~reference_base resps =
  let is_error code =
    let code = int_of_string code in
    code < 200 || code >= 300 in
  let responses_match (r1 : Swagger_j.response_or_reference) (r2 : Swagger_j.response_or_reference) =
    match r1.schema, r2.schema with
    | Some s1, Some s2 -> s1 = s2
    | _ -> false in
  match resps with
  | [] -> None, "unit"
  | (code, _)::rs when is_error code ->
      return_type ~reference_root ~reference_base rs (* ignore errors; assume strings *)
  | (_code, resp)::rs ->
      (* check all 2xx responses return the same type *)
      let rec check first = function
        | [] -> ()
        | (code, _)::res when is_error code -> check first res
        | (_code', resp')::rs when responses_match first resp' -> check first rs
        | (c, (r:Swagger_j.response_or_reference))::_ -> failwith @@ sprintf "multiple response types are not supported: %s - %s" (Swagger_j.string_of_response_or_reference resp) (Swagger_j.string_of_response_or_reference r) in
      check resp rs;
      resp_type ~reference_base ~reference_root resp
(*       "(string, string) result Lwt.t" *)

let make_dups params =
  List.fold_left
    (fun dups (p : Swagger_j.parameter_or_reference) ->
      match StringMap.find_opt p.name dups with
      | Some count -> StringMap.add p.name (count + 1) dups
      | None -> StringMap.add p.name 1 dups)
    StringMap.empty
    params

let operation_val ~root ~reference_base ~reference_root name params = function
  | Some (op : Swagger_j.operation) ->
      let params = operation_params (params, op.parameters) in
      let dups = make_dups params in
      let param_sigs, param_impls =
        params
        |> List.map
             (fun (p : Swagger_j.parameter_or_reference) ->
               let duplicate = StringMap.find p.name dups > 1 in
               Param.create ~duplicate ~reference_base ~reference_root p)
        |> List.split in
      (* TODO op.responses *)
      let return_module, return_type = return_type ~reference_root ~reference_base op.responses in
      let verb = Val.Impl.http_verb_of_string name in
      let signature = Val.Sig.http_request ?descr:op.description name param_sigs return_type in
      let return =
        match return_module with
        | Some module_name -> Val.Impl.Module module_name
        | None -> Val.Impl.Type return_type in
      let implementation = Val.Impl.http_request verb name param_impls ~return in
      Some (Val.create signature implementation)
  | None ->
      None

let path_val path =
  Val.create
    (Val.Sig.constant "request_path_template")
    (Val.Impl.constant "request_path_template" path)

let path_item_vals ~root ~reference_base ~reference_root ~path (item : Swagger_j.path_item) : Val.t list =
  let get     = operation_val ~root ~reference_base ~reference_root "get"     item.parameters item.get in
  let put     = operation_val ~root ~reference_base ~reference_root "put"     item.parameters item.put in
  let post    = operation_val ~root ~reference_base ~reference_root "post"    item.parameters item.post in
  let delete  = operation_val ~root ~reference_base ~reference_root "delete"  item.parameters item.delete in
  let options = operation_val ~root ~reference_base ~reference_root "options" item.parameters item.options in
  let head    = operation_val ~root ~reference_base ~reference_root "head"    item.parameters item.head in
  let patch   = operation_val ~root ~reference_base ~reference_root "patch"   item.parameters item.patch in
  path_val path :: keep_some [get; put; post; delete; options; head; patch]

let definition_module ?(path = []) ~root ~reference_base ~name (schema : Swagger_j.schema) : Mod.t =
  let required = default [] schema.required in
  let properties = default [] schema.properties in

  let create_param name type_ required_params =
    let n = Param.name name in
    if List.mem name required_params
    then (Val.Sig.named n type_, Val.Impl.named n type_)
    else (Val.Sig.optional n type_, Val.Impl.optional n type_) in

  let create_params =
    List.fold_left
      (fun params (name, schema) ->
        let s = Schema.create ~reference_base ~reference_root:root schema in
        let param_type = Schema.to_string s in
        let param_sig, param_impl = create_param name param_type required in
        (param_sig, param_impl) :: params)
      [] in

  let alias_type () =
    let param_type = Schema.kind_to_string (Schema.create ~reference_base ~reference_root:root schema) in
    let typ = Type.create (Type.Sig.abstract "t") (Type.Impl.alias "t" param_type) in
    let create =
      Val.create
        (Val.Sig.(pure "create" [unnamed param_type] "t"))
        (Val.Impl.(identity "create" [unnamed "t" "t"])) in
    ([typ], [create]) in

  let record_type () =
    let params = create_params properties in
    let sig_params, impl_params = params |> List.split in
    let create =
      Val.create
        (Val.Sig.pure "create" sig_params "t")
        (Val.Impl.record_constructor "create" impl_params) in
    let fields, values =
      List.fold_left
        (fun (fields, values) (name, schema) ->
          let opt = if List.mem name required then "" else " option" in
          let s = Schema.create ~reference_base ~reference_root:root schema in
          let ret = sprintf "%s%s" (Schema.to_string s) opt in
          let param_name = Param.name name in
          let field = Type.Impl.{ name = param_name; orig_name = name; type_ = ret } in
          let value =
            Val.create
              (Val.Sig.pure param_name [Val.Sig.Unnamed "t"] ret)
              (Val.Impl.record_accessor param_name [Val.Impl.unnamed "t" "t"]) in
          (field :: fields, value :: values))
        ([], [])
        properties in
    let values = create :: List.rev values in
    let type_sig = Type.Sig.abstract "t" in
    let type_impl = Type.Impl.record "t" fields in
    let typ = Type.create type_sig type_impl in
    ([typ], values) in

  let phantom_type () =
    let typ = Type.create (Type.Sig.phantom "t") (Type.Impl.phantom "t") in
    ([typ], []) in

  let types, values =
    match schema.kind, schema.properties with
    | Some _, _ -> alias_type ()
    | None, Some _ -> record_type ()
    | None, None -> phantom_type () in

  Mod.create ~name ~path ~types ~values ()

let rec insert_module m root = function
  | [] -> Mod.add_mod m root
  | p::ps ->
      match Mod.find_submodule p root with
      | Some subm ->
          Mod.add_mod (insert_module m subm ps) root
      | None ->
          let subm = Mod.empty p ~path:(Mod.qualified_path root) () in
          Mod.add_mod (insert_module m subm ps) root

let remove_base base segments =
  match base, segments with
  | Some base, s::ss when base = s -> ss
  | _ -> segments

let rec build_paths ~root ~path_base ~reference_base ~reference_root = function
  | [] -> root
  | (path, item) :: paths ->
      let parents_and_child =
        path
        |> strip_base path_base
        |> String.split_on_char '/'
        |> List.filter ((<>)"")
        |> unsnoc in
      match parents_and_child with
      | Some (parents, child) ->
          let child_values =
            path_item_vals ~root ~reference_base ~reference_root ~path item in
          let child_module = Mod.with_values ~path:parents child child_values in
          let root = insert_module child_module root parents in
          build_paths ~root ~path_base ~reference_base ~reference_root paths
      | None ->
          let child_values =
            path_item_vals ~root ~reference_base ~reference_root ~path item in
          let root = Mod.add_vals child_values root in
          build_paths ~root ~path_base ~reference_base ~reference_root paths

let rec build_definitions ~root ~definition_base ~reference_base l =
  match l with
  | [] -> root
  | (name, (schema : Swagger_j.schema)) :: defs when schema.ref = None ->
      let parents_and_child =
        name
        |> strip_base definition_base
        |> split_ref
        |> unsnoc in
      (match parents_and_child with
      | Some (parents, child) ->
          let def = definition_module ~root ~reference_base ~path:parents ~name:child schema in
          let root = insert_module def root parents in
          build_definitions ~root ~definition_base ~reference_base defs
      | None ->
          let root = Mod.add_mod (definition_module ~root ~reference_base ~name schema) root in
          build_definitions ~root ~definition_base ~reference_base defs)
  (* XXX ignore schemas that are simply references? just use the referenced module?
   * in the kubernetes API this seems to be only for deprecated stuff. *)
  | (name, (schema : Swagger_j.schema)) :: defs ->
      build_definitions ~root ~definition_base ~reference_base defs

let of_swagger ?(path_base = "") ?(definition_base = "") ?(reference_base = "") ~reference_root s =
  let open Swagger_j in
  let definitions = default [] s.definitions in
  let title = s.info.title in
  let defs =
    build_definitions
      ~root:(Mod.empty reference_root ~path:[title] ())
      ~definition_base
      ~reference_base
      definitions in
  let root =
    build_paths
      ~root:(Mod.empty ~recursive:true title ())
      ~path_base
      ~reference_base
      ~reference_root:defs
      s.paths in
  Mod.add_mod defs root

let to_string = Mod.to_string

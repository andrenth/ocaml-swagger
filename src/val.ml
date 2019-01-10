open Printf
open Util

module Sig = struct
  type kind =
    | Pure
    | Http_request

  type io = [`Lwt | `Async]

  type param_data =
    { name : string
    ; descr : string option
    }

  type param =
    | Named of param_data * string
    | Positional of string
    | Optional of param_data * string

  type return =
    | Simple of string
    | Async of io * string

  type t =
    { name      : string
    ; params    : param list
    ; return    : return
    ; kind      : kind
    ; descr     : string list
    }

  let param_descr = function
    | Positional _
    | Named ({ descr = None; _ }, _)
    | Optional ({ descr = None; _ }, _) ->
        None
    | Named ({ name; descr = Some descr }, _) ->
        Some (sprintf "%s %s" name descr)
    | Optional ({ name; descr = Some descr }, _) ->
        Some (sprintf "%s %s" name descr)

  let create ?descr kind name params return =
    let descr = descr :: List.map param_descr params |> keep_some in
    { name; params; return; kind; descr }

  let named ?descr name type_ = Named ({ name; descr }, type_)
  let positional type_ = Positional type_
  let optional ?descr name type_ = Optional ({ name; descr }, type_)

  let pure ?descr name params ret =
    create ?descr Pure name params (Simple ret)

  (* Here "constants" are actually [unit -> string] functions to satisfy
   * OCaml's recusive module safety requirements.
   *
   * See https://stackoverflow.com/q/4239045 *)
  let constant name =
    pure name [Positional "unit"] "string"

  let http_request ?descr name params ret io =
    create ?descr Http_request name params (Async (io, ret))

  let param_to_string = function
    | Named ({ name; _ }, type_) -> sprintf "%s:%s" name type_
    | Positional type_ -> type_
    | Optional ({ name; _ }, type_) -> sprintf "?%s:%s" name type_

  let name t = t.name
  let return t = t.return

  let return_to_string = function
    | Simple t -> t
    | Async (io, t) ->
      match io with
      | `Lwt -> sprintf "(%s, string) result Lwt.t" t
      | `Async -> sprintf "(%s, string) result Async.Deferred.t" t

  let params_to_string params =
    let rec go acc = function
      | [] -> acc
      (* extra unit param if last one is optional or named *)
      | [(Optional _ | Named _) as p] -> sprintf "%s%s -> unit -> " acc (param_to_string p)
      | p :: ps -> go (sprintf "%s%s -> " acc (param_to_string p)) ps in
    go "" params

  let to_string ?(indent = 0) { name; params; return; kind; descr } =
    let pad = String.make indent ' ' in
    let params =
      match kind with
      | Pure ->
          params
      | Http_request ->
        let ctx =
          match return with
          | Simple (_ : string) -> assert false
          | Async (`Lwt, (_ : string)) ->
            [optional "ctx" "Cohttp_lwt_unix.Client.ctx"]
          | Async (`Async, (_ : string)) ->
            [] in
        let headers = optional "headers" "Cohttp.Header.t" in
        let uri = positional "Uri.t" in
        params @ ctx @ [headers; uri] in
    let doc =
      match descr with
      | [] -> ""
      | _ ->
          let comment_pad = pad ^ String.make 3 ' ' in
          let descr =
            List.mapi
              (fun i d ->
                let d = format_comment d in
                if i = 0
                then sprintf "%s(** %s" pad d
                else sprintf "\n%s @param %s" comment_pad d)
              descr in
          String.concat "\n" descr ^ " *)\n" in
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
    | Module_custom_of_json of ((string -> string) * string)

  type io = [`Lwt | `Async]

  type http_request =
    { verb : http_verb
    ; io : io
    ; return : return
    ; streaming : bool
    }

  type kind =
    | Record_constructor
    | Record_accessor
    | Identity
    | Constant of string
    | Http_request of http_request
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
    | Positional of param_data
    | Optional of param_data * origin option

  let module_ name = Module name
  let type_ name = Type name

  let origin (p : Swagger_j.parameter) =
    { location = p.location; orig_name = p.name }

  let named ?origin name type_ =
    Named ({ name; type_ }, origin)

  let positional name type_ =
    Positional { name; type_ }

  let optional ?origin name type_ =
    Optional ({ name; type_ }, origin)

  type t =
    { name      : string
    ; params    : param list
    ; kind      : kind
    }

  let name t = t.name
  let kind t = t.kind

  let create kind name params = { kind; name; params }

  let record_constructor = create Record_constructor
  let record_accessor = create Record_accessor
  let identity = create Identity
  let constant name value =
    create (Constant value) name [positional "()" "unit"]
  let http_request ?(streaming=false) ~return verb io = create (Http_request {verb; io; return; streaming})

  let param_name = function
    | Named (p, _) | Positional p | Optional (p, _) -> p.name

  let param_type = function
    | Named (p, _) | Positional p | Optional (p, _) -> p.type_

  let param_location = function
    | Named (_, Some origin) -> Some origin.location
    | Named (_, None) -> None
    | Optional (_, Some origin) -> Some origin.location
    | Optional (_, None) -> None
    | Positional _ -> None

  [@@@ocaml.warning "-32"]

  let param_orig_name = function
    | Named (_, Some origin) -> Some origin.orig_name
    | Optional (_, Some origin) -> Some origin.orig_name
    | Named (_, None) | Optional (_, None) | Positional _ -> None

  let derived = create Derived "" []

  let is_optional = function
    | Optional _ -> true
    | _ -> false

  [@@@end]

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
          | Positional _ ->
              failwith "positional parameters don't go in requests" in
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
           "headers"
       | hs ->
           sprintf {|
             let headers =
               match headers with
               | Some hs -> hs
               | None -> Header.init () in
             Some (Header.add_list headers %s)
           |} (assoc_string hs)

  let make_body params =
    let body_params =
      params
      |> List.filter (fun p -> param_location p = Some `Body) in
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
        String.trim @@
          sprintf {| Some (Body.of_string (Yojson.Safe.to_string (%s %s))) |}
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

  let client_function_of_http_verb verb =
    match verb with
    | Head -> string_of_http_verb verb
    | verb -> sprintf "call `%s" (string_of_http_verb verb |> String.uppercase_ascii)

  let continuation_of_http_verb io streaming = function
    | Head -> {|
        fun resp ->
          let code =
            resp
            |> Response.status
            |> Code.code_of_status in
          let body = "Ok" in
      |}
    | _ ->
      let body_ret =
        if streaming then
          match io with
          | `Async -> "Async.Deferred.return (Body.to_pipe body)"
          | `Lwt -> "Lwt.return (Body.to_streaming body)"
        else
          "Body.to_string body" in
      sprintf {|
        fun (resp, body) ->
          let code =
            resp
            |> Response.status
            |> Code.code_of_status in
          %s >>= fun body ->
      |} body_ret

  let build_http_request ~pad ?(body_param = false) ~io ~return verb streaming params =
    let client_fun = client_function_of_http_verb verb in
    let result_cont = continuation_of_http_verb io streaming verb in
    let body_param =
      if body_param
      then sprintf " ?body:(%s)" (make_body params)
      else "" in
    let to_json =
      if streaming
      then "Io_helper.stream_json"
      else "Yojson.Safe.from_string" in
    let chunked = if streaming then "~chunked:true" else "" in
    let response_code =
      let io_module, ctx =
        match io with
        | `Lwt -> "Lwt", "?ctx"
        | `Async -> "Async.Deferred", "" in
      match return with
      | Module_custom_of_json (of_json, m) ->
          sprintf {|
        Client.%s %s %s ?headers%s uri >>= %s
        let json = %s body in
        %s.return (if code >= 200 && code < 300 then Ok (%s) else Error "Unable to start stream")
      |} client_fun ctx chunked body_param result_cont to_json io_module (of_json m)
      | Module module_name ->
          let of_json module_name =
            let pure = sprintf "%s.of_yojson json" module_name in
            if streaming
            then sprintf "Ok (Io_helper.map_stream (fun json -> %s |> function Ok x -> x | Error _ -> failwith (Yojson.Safe.to_string json)) json)" pure
            else pure in
          sprintf {|
        Client.%s %s %s ?headers%s uri >>= %s
        let json = %s body in
        %s.return (if code >= 200 && code < 300 then %s else Error "Unable to start stream for %s")
      |} client_fun ctx chunked body_param result_cont to_json io_module (of_json module_name) module_name
      | Type type_name ->
          let conv_result = function
            | "unit"   -> "()"
            | "string" -> "body"
            | other    -> sprintf "(%s_of_string body)" other in
          sprintf {|
        Client.%s %s %s ?headers%s uri >>= %s
        %s.return (if code >= 200 && code < 300 then Ok %s else Error (string_of_int code))
      |} client_fun ctx chunked body_param result_cont io_module (conv_result type_name) in
    let code =
      let io_preamble =
        match io with
        | `Lwt -> {|
          let open Lwt.Infix in
          let open Cohttp in
          let open Cohttp_lwt_unix in
          let module Body = Cohttp_lwt.Body in
        |}
        | `Async -> {|
          let open Async in
          let open Cohttp in
          let open Cohttp_async in
          let module Body = Cohttp_async.Body in
        |} in
      String.trim @@ sprintf {|
        %s
        let query = %s in
        let path =
          %s in
        let uri = Uri.with_path uri path in
        let uri = Uri.with_query' uri (List.filter (fun (k, v) -> v <> "") query) in
        let headers = %s in
        %s
      |} io_preamble (make_query params) (make_path params) (make_headers params) (String.trim response_code) in
    code
    |> String.split_on_char '\n'
    |> List.map (fun l -> sprintf "%s%s" pad (trim_at_most 10 l))
    |> String.concat "\n"

  let http_get     ~io ~return = build_http_request                  ~io ~return Get
  let http_put     ~io ~return = build_http_request ~body_param:true ~io ~return Put
  let http_post    ~io ~return = build_http_request ~body_param:true ~io ~return Post
  let http_delete  ~io ~return = build_http_request ~body_param:true ~io ~return Delete
  let http_head    ~io ~return = build_http_request                  ~io ~return Head
  let http_patch   ~io ~return = build_http_request ~body_param:true ~io ~return Patch
  let http_options ~io ~return = build_http_request                  ~io ~return Options

  let body_to_string ?(indent = 0) t =
    let pad = String.make indent ' ' in
    match t.kind with
    | Record_constructor -> record_constructor_body ~pad t.params
    | Record_accessor -> sprintf "%st.%s" pad t.name
    | Identity -> sprintf "%st" pad
    | Constant v -> sprintf "%s\"%s\"" pad v
    | Http_request {verb=Get; io; return; streaming} -> http_get ~pad ~io ~return streaming t.params
    | Http_request {verb=Put; io; return; streaming} -> http_put ~pad ~io ~return streaming t.params
    | Http_request {verb=Post; io; return; streaming} -> http_post ~pad ~io ~return streaming t.params
    | Http_request {verb=Delete; io; return; streaming} -> http_delete ~pad ~io ~return streaming t.params
    | Http_request {verb=Head; io; return; streaming} -> http_head ~pad ~io ~return streaming t.params
    | Http_request {verb=Patch; io; return; streaming} -> http_patch ~pad ~io ~return streaming t.params
    | Http_request {verb=Options; io; return; streaming} -> http_options ~pad ~io ~return streaming t.params
    | Derived ->
        failwith "Val.Impl.body_to_string: derived functions have no body"

  let param_to_string = function
    | Named (p, _) -> sprintf "~%s" p.name
    | Positional p -> p.name
    | Optional (p, _) -> sprintf "?%s" p.name

  let params_to_string params =
    let rec go acc = function
      | [] -> acc
      (* extra () param if last one is optional or named *)
      | [(Optional _ | Named _) as p] -> sprintf "%s%s () " acc (param_to_string p)
      | p::ps -> go (sprintf "%s%s " acc (param_to_string p)) ps in
    go "" params

  let to_string ?(indent = 0) ({ kind; name; params } as value) =
    let pad = String.make indent ' ' in
    let params =
      match kind with
      | Http_request {verb=_; return=_; io; streaming=_} ->
          let ctx =
            match io with
            | `Lwt ->
              [optional "ctx" "Cohttp_lwt_unix.Client.ctx"]
            | `Async ->
              [] in
          let headers = optional "headers" "Cohttp.Header.t" in
          let uri = positional "uri" "Uri.t" in
          params @ ctx @ [headers; uri]
      | _ ->
          params in
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

let signature t =
  t.signature

let implementation t =
  t.implementation

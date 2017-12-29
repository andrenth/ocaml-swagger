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

let camelize s =
  let buf = Buffer.create (String.length s) in
  String.iteri
    (fun i c ->
      match c with
      | '-' -> Buffer.add_char buf '_'
      | 'A' .. 'Z' ->
          if i > 0 then Buffer.add_char buf '_';
          Buffer.add_char buf (Char.lowercase_ascii c)
      | _ ->
        Buffer.add_char buf c)
    s;
  Buffer.contents buf

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
  type t =
    | Abstract of string

  let abstract name = Abstract name

  let to_string = function
    | Abstract name -> sprintf "type %s" name
end

module Val = struct
  type t = string * string list * string

  let create name params ret = (name, params, ret)

  let to_string ?(indent = 0) (name, params, ret) =
    let pad = String.make indent ' ' in
    let params =
      match params with
      | [] -> ["unit"]
      | _  -> params in
    sprintf "%sval %s : %s -> %s\n" pad name (String.concat " -> " params) ret
end

module StringMap = Map.Make (struct
  type t = string
  let compare = compare
end)

module Mod = struct
  type t =
    { name : string
    ; types : Type.t list
    ; sub_modules : t StringMap.t
    ; values : Val.t list
    }

  let module_name s =
    let s =
      let last = String.length s - 1 in
      if s.[0] = '{' && s.[last] = '}' then "By_" ^ String.sub s 1 (last - 1)
      else s in
    s
    |> String.lowercase_ascii
    |> String.capitalize_ascii
    |> String.map (fun c -> if c = '-' then '_' else c)
    |> String.split_on_char '.'
    |> List.hd

  let create ~name ?(types = []) ?(sub_modules = StringMap.empty) ?(values = []) () =
    { name
    ; types
    ; sub_modules
    ; values
    }

  let empty name =
    { name = module_name name
    ; types = []
    ; sub_modules = StringMap.empty
    ; values = []
    }

  let with_types name types =
    { name = module_name name
    ; types = types
    ; sub_modules = StringMap.empty
    ; values = []
    }

  let with_values name values =
    { name = module_name name
    ; types = []
    ; sub_modules = StringMap.empty
    ; values
    }

  let name m = m.name

  let add_type t m =
    { m with types = t :: m.types }

  let add_mod subm m =
    { m with sub_modules = StringMap.add subm.name subm m.sub_modules }

  let add_val v m =
    { m with values = v :: m.values }

  let add_types ts m =
    { m with types = m.types @ ts }

  let add_vals vs m =
    { m with values = m.values @ vs }

  let map_submodules f m =
    { m with sub_modules = StringMap.map f m.sub_modules }

  let has_submodules m =
    StringMap.is_empty m.sub_modules

  let find_submodule name m =
    StringMap.find_opt name m.sub_modules

  let rec to_string ?(indent = 0) m =
    let pad = String.make indent ' ' in
    let submods =
      StringMap.fold
        (fun name m acc ->
          let s = to_string ~indent:(indent + 2) m in
          (* Definitions first so that references work *)
          if name = "Definitions" then s ^ acc
          else acc ^ s)
        m.sub_modules
        "" in
    sprintf "%smodule %s : sig\n%s%s%send\n"
      pad
      m.name
      submods
      (String.concat "" (List.map (Val.to_string ~indent:(indent + 2)) m.values))
      pad
end

let strip_base base path =
  let plen = String.length path in
  let blen = String.length base in
  if plen >= blen then
    if base = String.sub path 0 blen then
      String.sub path blen (plen - blen)
    else
      path
  else
    path

let split_ref ref =
  ref
  |> String.split_on_char '.'
  |> List.filter ((<>)"")
  |> List.map Mod.module_name

let definition_type ?(parent_path = "") ~base ref =
  let ref =
    ref
    |> String.lowercase_ascii
    |> strip_base (String.lowercase_ascii base)
    |> strip_base (String.lowercase_ascii parent_path)
    |> split_ref
    |> String.concat "." in
  ref ^ ".t"

module Schema = struct
  type t = Swagger_j.schema

  let rec kind_to_string ?parent_path ~reference_base (schema : t) =
    match schema.ref with
    | Some r -> definition_type ?parent_path ~base:reference_base r
    | None ->
        match some schema.kind with
        | `String  -> "string"
        | `Number  -> "float"
        | `Integer -> "int"
        | `Boolean -> "bool"
        | `Object  ->
            let open Swagger_j in
            (match schema.additional_properties with
            | Some props -> sprintf "(string * %s) list" (kind_to_string ?parent_path ~reference_base props)
            | None -> failwith "Schema.kind_to_string: object without additional_properties")
        | `Array   ->
            let open Swagger_j in
            match schema.items with
            | Some s -> kind_to_string ?parent_path ~reference_base s ^ " list"
            | None -> failwith "Schema.kind_to_string: array type must have an 'items' field"

  let to_string ?parent_path ~reference_base (schema : t) =
    match schema.ref with
    | Some r -> definition_type ?parent_path ~base:reference_base r
    | None -> kind_to_string ?parent_path ~reference_base schema
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
    let n = camelize n in
    if is_keyword n then n ^ "_"
    else n

  let to_string ~reference_base (p : t) =
    let open Swagger_j in
    let prefix = if p.required then "" else "?" in
    let kind =
      match p.location with
      | `Body -> Schema.to_string ~reference_base (some p.schema)
      | _     -> kind_to_string p in
    sprintf "%s%s:%s" prefix (name p.name) kind
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

let resp_type ~base (resp : Swagger_j.response_or_reference) =
  let ref_type = definition_type ~base in
  match resp.ref with
  | Some r -> ref_type r
  | None ->
      match resp.schema with
      | Some s -> "Definitions." ^ Schema.to_string ~reference_base:base s
      | None -> "unit"

let rec return_type ~base resps =
  let is_error code =
    let code = int_of_string code in
    code < 200 || code >= 300 in
  let responses_match (r1 : Swagger_j.response_or_reference) (r2 : Swagger_j.response_or_reference) =
    match r1.schema, r2.schema with
    | Some s1, Some s2 -> s1 = s2
    | _ -> false in
  match resps with
  | [] ->
      "unit" (* XXX error? *)
  | (code, _)::rs when is_error code ->
      return_type ~base rs (* ignore errors; assume strings *)
  | (_code, resp)::rs ->
      (* check all 2xx responses return the same type *)
      let rec check first = function
        | [] -> ()
        | (code, _)::res when is_error code -> check first res
        | (_code', resp')::rs when responses_match first resp' -> check first rs
        | (c, (r:Swagger_j.response_or_reference))::_ -> failwith @@ sprintf "multiple response types are not supported: %s - %s" (Swagger_j.string_of_response_or_reference resp) (Swagger_j.string_of_response_or_reference r) in
      check resp rs;
      sprintf "(%s, string) result" (resp_type ~base resp)

let operation_val ~reference_base name params = function
  | Some (op : Swagger_j.operation) ->
      let params = List.map (Param.to_string ~reference_base) (operation_params (params, op.parameters)) in
      (* TODO op.responses *)
      let ret = return_type ~base:reference_base op.responses in
      Some (Val.create name params ret)
  | None ->
      None

let rec keep_some = function
  | [] -> []
  | Some x :: xs -> x :: keep_some xs
  | None :: xs -> keep_some xs

let path_item_vals ~reference_base (item : Swagger_j.path_item) : Val.t list =
  let get     = operation_val ~reference_base "get"     item.parameters item.get in
  let put     = operation_val ~reference_base "put"     item.parameters item.put in
  let post    = operation_val ~reference_base "post"    item.parameters item.post in
  let delete  = operation_val ~reference_base "delete"  item.parameters item.delete in
  let options = operation_val ~reference_base "options" item.parameters item.options in
  let head    = operation_val ~reference_base "head"    item.parameters item.head in
  let patch   = operation_val ~reference_base "patch"   item.parameters item.patch in
  keep_some [get; put; post; delete; options; head; patch]

let definition_module ?parent_path ~reference_base ~name (schema : Swagger_j.schema) : Mod.t =
  let typ = Type.abstract "t" in
  let required = default [] schema.required in
  let properties = default [] schema.properties in
  let prefix p = if List.mem p required then "" else "?" in
  let rec create_params = function
    | [] -> []
    | (name, schema)::ps ->
        let s =
          sprintf "%s%s:%s"
            (prefix name)
            (Param.name name)
            (Schema.to_string ?parent_path ~reference_base schema) in
        s::create_params ps in
  let create = Val.create "create" (create_params properties) "t" in
  let values =
    List.map
      (fun (name, schema) ->
        let opt = if List.mem name required then "" else " option" in
        let ret = sprintf "%s%s" (Schema.to_string ?parent_path ~reference_base schema) opt in
        Val.create (Param.name name) ["t"] ret)
      properties in
  Mod.create ~name ~types:[typ] ~values:(create::values) ()

let rec insert_module m root = function
  | [] -> Mod.add_mod m root
  | p::ps when Mod.name root = p ->
      insert_module m root ps
  | p::ps ->
      match Mod.find_submodule p root with
      | Some subm ->
          Mod.add_mod (insert_module m subm ps) root
      | None ->
          let pmod = insert_module m (Mod.empty p) ps in
          Mod.add_mod pmod root

let remove_base base segments =
  match base, segments with
  | Some base, s::ss when base = s -> ss
  | _ -> segments

let rec build_paths ~root ~path_base ~reference_base = function
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
          let child_module = Mod.with_values child (path_item_vals ~reference_base item) in
          let root = insert_module child_module root parents in
          build_paths ~root ~path_base ~reference_base paths
      | None ->
          let root = Mod.add_vals (path_item_vals ~reference_base item) root in
          build_paths ~root ~path_base ~reference_base paths

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
          let parent_path = (String.concat "." parents) ^ "." in
          let def = definition_module ~reference_base ~parent_path ~name:child schema in
          let root = insert_module def root parents in
          build_definitions ~root ~definition_base ~reference_base defs
      | None ->
          let root = Mod.add_mod (definition_module ~reference_base ~name schema) root in
          build_definitions ~root ~definition_base ~reference_base defs)
  (* XXX ignore schemas that are simply references? just use the referenced module?
   * in the kubernetes API this seems to be only for deprecated stuff. *)
  | (name, (schema : Swagger_j.schema)) :: defs ->
      build_definitions ~root ~definition_base ~reference_base defs

let of_swagger ?(path_base = "") ?(definition_base = "") ?(reference_base = "") s =
  let open Swagger_j in
  let definitions = default [] s.definitions in
  let defs =
    build_definitions
      ~root:(Mod.empty "Definitions")
      ~definition_base
      ~reference_base
      definitions in
  let root = Mod.add_mod defs (Mod.empty s.info.title) in
  build_paths ~root ~path_base ~reference_base s.paths

let to_string = Mod.to_string

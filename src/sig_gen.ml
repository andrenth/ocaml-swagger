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

module Schema = struct
  type t = Swagger_j.schema

  let rec kind_to_string (schema : t) =
    match schema.ref with
    | Some r -> "$ref" (* TODO *)
    | None ->
        match some schema.kind with
        | `String  -> "string"
        | `Number  -> "float"
        | `Integer -> "int"
        | `Boolean -> "bool"
        | `Object  ->
            let open Swagger_j in
            (match schema.additional_properties with
            | Some props -> sprintf "(string * %s) list" (kind_to_string props)
            | None -> failwith "Schema.kind_to_string: object without additional_properties")
        | `Array   ->
            let open Swagger_j in
            match schema.items with
            | Some s -> kind_to_string s ^ " list"
            | None -> failwith "Schema.kind_to_string: array type must have an 'items' field"

  let to_string (schema : t) =
    match schema.ref with
    | Some r -> "$ref" (* TODO *)
    | None -> kind_to_string schema
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

  let to_string (p : t) =
    let open Swagger_j in
    let prefix = if p.required then "~" else "?" in
    let name = camelize p.name in
    let kind =
      match p.location with
      | `Body -> Schema.to_string (some p.schema)
      | _     -> kind_to_string p in
    sprintf "%s%s:%s" prefix name kind
end

module Type = struct
  type t =
    | Abstract of string

  let abstract name = Abstract name

  let to_string = function
    | Abstract name -> sprintf "type %s" name
end

module Val = struct
  type t = string * string list

  let create name typ = (name, typ)

  let to_string ?(indent = 0) (name, typ) =
    let pad = String.make indent ' ' in
    let parms, ret =
      match unsnoc typ with
      | Some (parms, ret) -> (parms, "xxx_t") (* TODO return type *)
      | None -> ["unit"], "unit" in
    sprintf "%sval %s : %s -> %s\n" pad name (String.concat " -> " parms) ret
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
    s |> String.lowercase_ascii |> String.capitalize_ascii

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
        (fun _ m s -> s ^ to_string ~indent:(indent + 2) m)
        m.sub_modules
        "" in
    sprintf "%smodule %s : sig\n%s%s%send\n"
      pad
      m.name
      submods
      (String.concat "" (List.map (Val.to_string ~indent:(indent + 2)) m.values))
      pad
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

let operation_val name params = function
  | Some (op : Swagger_j.operation) ->
      Some (Val.create name (List.map Param.to_string (operation_params (params, op.parameters))))
  | None ->
      None

let rec keep_some = function
  | [] -> []
  | Some x :: xs -> x :: keep_some xs
  | None :: xs -> keep_some xs

let path_item_vals (item : Swagger_j.path_item) : Val.t list =
  let get     = operation_val "get"     item.parameters item.get in
  let put     = operation_val "put"     item.parameters item.put in
  let post    = operation_val "post"    item.parameters item.post in
  let delete  = operation_val "delete"  item.parameters item.delete in
  let options = operation_val "options" item.parameters item.options in
  let head    = operation_val "head"    item.parameters item.head in
  let patch   = operation_val "patch"   item.parameters item.patch in
  keep_some [get; put; post; delete; options; head; patch]

let definition_module name (schema : Swagger_j.schema) : Mod.t =
  let typ = Type.abstract "t" in
  let required = default [] schema.required in
  let properties = default [] schema.properties in
  let prefix p = if List.mem p required then "~" else "?" in
  let rec create_params = function
    | [] -> ["t"] (* create returns t *)
    | (name, schema)::ps ->
        let s =
          sprintf "%s%s:%s"
            (prefix name)
            (camelize name)
            (Schema.to_string schema) in
        s::create_params ps in
  let create = Val.create "create" (create_params properties) in
  let values =
    List.map
      (fun (name, schema) ->
        let opt = if List.mem name required then "" else " option" in
        Val.create (camelize name) ["t"; sprintf "%s%s" (Schema.to_string schema) opt])
      properties in
  Mod.create ~name ~types:[typ] ~values:(create::values) ()

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

let rec insert_module m root = function
  | [] -> Mod.add_mod m root
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

let rec build_paths ~root ~base = function
  | [] -> root
  | (path, item) :: paths ->
      let parents_and_child =
        path
        |> strip_base base
        |> String.split_on_char '/'
        |> List.filter ((<>)"")
        |> List.map Mod.module_name
        |> unsnoc in
      match parents_and_child with
      | Some (parents, child) ->
          let child_module = Mod.with_values child (path_item_vals item) in
          let root = insert_module child_module root parents in
          build_paths ~root ~base paths
      | None ->
          let root = Mod.add_vals (path_item_vals item) root in
          build_paths ~root ~base paths

let rec build_definitions ~root ~base l =
  match l with
  | [] -> root
  | (name, (schema : Swagger_j.schema)) :: defs when schema.ref = None ->
      let parents_and_child =
        name
        |> strip_base base
        |> String.split_on_char '.'
        |> List.filter ((<>)"")
        |> List.map Mod.module_name
        |> unsnoc in
      (match parents_and_child with
      | Some (parents, child) ->
          let def = definition_module child schema in
          let root = insert_module def root ("Definitions" :: parents) in
          build_definitions ~root ~base defs
      | None ->
          let root = Mod.add_mod (definition_module name schema) root in
          build_definitions ~root ~base defs)
  (* XXX ignore schemas that are simply references? just use the referenced module? *)
  | (name, (schema : Swagger_j.schema)) :: defs ->
      build_definitions ~root ~base defs

let of_swagger ?(path_base = "") ?(definition_base = "") s =
  let open Swagger_j in
  let definitions = default [] s.definitions in
  let defs = build_definitions ~root:(Mod.empty "Definitions") ~base:definition_base definitions in
  let root = Mod.add_mod defs (Mod.empty s.info.title) in
  build_paths ~root ~base:path_base s.paths

let to_string = Mod.to_string

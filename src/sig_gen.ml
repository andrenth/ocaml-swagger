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

module Param = struct
  type t = Swagger_j.parameter_or_reference

  let is_upper = function
    | 'A' .. 'Z' -> true
    | _ -> false

  let camelize s =
    let buf = Buffer.create (String.length s) in
    String.iteri
      (fun i c ->
        if is_upper c then begin
          if i > 0 then Buffer.add_char buf '_';
          Buffer.add_char buf (Char.lowercase_ascii c)
        end else
          Buffer.add_char buf c)
      s;
    Buffer.contents buf

  let rec item_kind_to_string (items:Swagger_j.items option) = function
    | `String  -> "string"
    | `Number  -> "float"
    | `Integer -> "int"
    | `Boolean -> "bool"
    | `Array   ->
        let open Swagger_j in
        match items with
        | Some is -> item_kind_to_string is.items is.kind ^ " array"
        | None -> failwith "item_kind_of_string: array type must have an 'items' field"

  let kind_to_string (items:Swagger_j.items option) = function
    | `String  -> "string"
    | `Number  -> "float"
    | `Integer -> "int"
    | `Boolean -> "bool"
    | `File    -> "file"
    | `Array   ->
        let open Swagger_j in
        match items with
        | Some is -> item_kind_to_string is.items is.kind ^ " array"
        | None -> failwith "kind_to_string: array type must have an 'items' field"

  let schema_to_string (items:Swagger_j.items option) (s : Swagger_j.schema) =
    match s.ref with
    | Some r -> "$ref"
    | None -> kind_to_string items (some s.kind)

  let to_string (p : Swagger_j.parameter_or_reference) =
    let open Swagger_j in
    match p.ref with
    | Some r ->
        sprintf "$ref"
    | None ->
        let prefix = if p.required then "~" else "?" in
        let name = camelize p.name in
        let kind =
          if p.location <> `Body then
            kind_to_string p.items (some p.kind)
          else
            let schema = some p.schema in
            schema_to_string p.items schema in
        sprintf "%s%s:%s" prefix name kind
end

module Val = struct
  type t = string * Param.t list

  let to_string ?(indent = 0) (name, typ) =
    let pad = String.make indent ' ' in
    let parms, ret =
      match unsnoc typ with
      | Some (parms, ret) -> List.map Param.to_string parms, "xxx_t"
      | None -> ["unit"], "unit" in
    sprintf "%sval %s : %s -> %s\n" pad name (String.concat " -> " parms) ret
end

module Mod = struct
  type t = M of string * t list * Val.t list

  let module_name s =
    s |> String.lowercase_ascii |> String.capitalize_ascii

  let create name =
    M (module_name name, [], [])

  let with_values name values =
    M (module_name name, [], values)

  let name (M (n, _, _)) = n

  let add_mod m (M (n, ms, vs)) =
    M (n, m::ms, vs)

  let add_val v (M (n, ms, vs)) =
    M (n, ms, v::vs)

  let add_vals vs' (M (n, ms, vs)) =
    M (n, ms, vs @ vs')

  let map_submodules f (M (n, ms, vs)) =
    M (n, List.map f ms, vs)

  let has_submodules = function
    | M (_, [], _) -> false
    | _ -> true

  let rec to_string ?(indent = 0) (M (n, ms, vs)) =
    let pad = String.make indent ' ' in
    sprintf "%smodule %s : sig\n%s%s%send\n"
      pad
      n
      (String.concat "" (List.map (to_string ~indent:(indent + 2)) ms))
      (String.concat "" (List.map (Val.to_string ~indent:(indent + 2)) vs))
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
  | Some (op:Swagger_j.operation) -> Some (name, operation_params (params, op.parameters))
  | None -> None

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

let strip_base path base =
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
  | [] ->
      root
  | [n] when n = Mod.name root ->
      Mod.add_mod m root
  | n::ns when n = Mod.name root ->
      if Mod.has_submodules root then
        Mod.map_submodules (fun submod -> insert_module m submod ns) root
      else
        Mod.add_mod m root
  | n::ns ->
      root

let remove_base base segments =
  match base, segments with
  | Some base, s::ss when base = s -> ss
  | _ -> segments

let rec of_paths ~root ~base = function
  | [] -> root
  | (path, item) :: paths ->
      let parents_and_child =
        path
        |> String.split_on_char '/'
        |> List.filter (fun s -> s <> "" && s.[0] <> '{' && s.[String.length s - 1] <> '}')
        |> remove_base base
        |> List.map Mod.module_name
        |> unsnoc in
      match parents_and_child with
      | Some (parents, child) ->
          let root = insert_module (Mod.with_values child (path_item_vals item)) root (Mod.name root :: parents) in
          of_paths ~root ~base paths
      | None ->
          let root = Mod.add_vals (path_item_vals item) root in
          of_paths ~root ~base paths

let of_swagger ?base s =
  let open Swagger_j in
  let root = Mod.create s.info.title in
  of_paths ~root ~base s.paths

let to_string = Mod.to_string

(* TODO Generate Definitions module; generate response types *)

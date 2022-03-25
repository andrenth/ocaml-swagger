open Printf
open Ppxlib
open Util

type t = {
  name : string;
  path : string list;
  types : Type.t list;
  values : Val.t list;
  submodules : t StringMap.t;
  recursive : bool;
  descr : string option;
}

let module_name s =
  let s =
    let last = String.length s - 1 in
    if s.[0] = '{' && s.[last] = '}' then "By_" ^ String.sub s 1 (last - 1)
    else s
  in
  s |> snake_case |> String.capitalize_ascii |> String.split_on_char '.'
  |> List.hd

let create ~name ?descr ?(recursive = false) ?(path = []) ?(types = [])
    ?(submodules = StringMap.empty) ?(values = []) () =
  {
    name = module_name name;
    path = List.map module_name path;
    types;
    values;
    submodules;
    recursive;
    descr;
  }

let empty name ?(recursive = false) ?(path = []) () =
  create ~name ~recursive ~path ()

let with_values name ?(recursive = false) ?(path = []) values =
  create ~name ~recursive ~path ~values ()

let name m = m.name
let submodules m = m.submodules |> StringMap.bindings |> List.map snd
let add_vals vs m = { m with values = m.values @ vs }

let add_mod subm m =
  { m with submodules = StringMap.add subm.name subm m.submodules }

let find_submodule name m = StringMap.find_opt (module_name name) m.submodules
let path m = m.path

let qualified_name m =
  match m.path with
  | [] -> m.name
  | _p -> sprintf "%s.%s" (String.concat "." m.path) m.name

let qualified_path m = m.path @ [ m.name ]
let has_type_named n m = List.exists (fun t -> Type.name t = n) m.types
let object_module_intf = [%sigi: module Object : Object.S with type value := t]

let object_module_impl =
  [%stri
    module Object = Object.Make (struct
      type value = t [@@deriving yojson]
    end)]

let rec to_module_type m =
  let open Ast_builder in
  let definitions, submods =
    Util.fold_left_map'
      (fun defs submods (name, m) ->
        let type_ = to_module_type m in
        let decl = module_declaration ~name:(Located.mk (Some m.name)) ~type_ in
        let s = psig_module decl in
        (* Definitions first to simplify references *)
        if name = "Definitions" then (Some s, submods) else (defs, s :: submods))
      (m.submodules |> StringMap.bindings)
  in
  let types =
    List.rev_map
      (fun t ->
        Ast_builder.psig_type Recursive [ Type.Sig.to_sig (Type.signature t) ])
      m.types
  in
  let values =
    List.rev_map
      (fun v -> Ast_builder.psig_value (Val.Sig.to_sig (Val.signature v)))
      m.values
  in
  let items =
    (if has_type_named "t" m then [ object_module_intf ] else [])
    |> List.rev_append values |> List.append types |> List.rev_append submods
    |> Util.opt_cons definitions
  in
  let t = pmty_signature items in
  match m.descr with
  | None -> t
  | Some descr ->
      let pmty_attributes = Util.ocaml_doc descr :: t.pmty_attributes in
      { t with pmty_attributes }

let rec to_mod_structure m =
  let submods =
    m.submodules |> StringMap.bindings
    |> List.rev_map (fun (name, m) ->
           let name = Ast_builder.Located.mk (Some name) in
           Ast_builder.(
             pstr_module (module_binding ~name ~expr:(to_mod_structure m))))
  in
  let types =
    List.concat_map (fun t -> Type.Impl.to_impl (Type.implementation t)) m.types
  in
  let values =
    List.rev_map (fun v -> Val.Impl.to_impl (Val.implementation v)) m.values
  in
  let items =
    (if has_type_named "t" m then [ object_module_impl ] else [])
    |> List.rev_append values |> List.append types |> List.rev_append submods
  in
  Ast_builder.pmod_structure items

let to_mod m =
  let open Ast_builder in
  let name = Located.mk (Some m.name) in
  let module_type = to_module_type m in
  let mod_structure = to_mod_structure m in
  let binding =
    module_binding ~name ~expr:(pmod_constraint mod_structure module_type)
  in
  [ (if m.recursive then pstr_recmodule [ binding ] else pstr_module binding) ]

let strip_base base path =
  let plen = String.length path in
  let blen = String.length base in
  if plen >= blen then
    let pref = String.sub path 0 blen in
    if String.lowercase_ascii base = String.lowercase_ascii pref then
      String.sub path blen (plen - blen)
    else path
  else path

let split_ref ref =
  ref |> String.split_on_char '.'
  |> List.filter (( <> ) "")
  |> List.map module_name

let reference_module_path ~reference_base ~reference_root ref =
  let path = ref |> strip_base reference_base |> split_ref in
  qualified_name reference_root :: path

let reference_module ~reference_base ~reference_root ref =
  reference_module_path ~reference_base ~reference_root ref |> String.concat "."

let reference_type ~reference_base ~reference_root ref =
  reference_module ~reference_base ~reference_root ref ^ ".t"

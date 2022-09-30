open Ppxlib
open Util

let sprintf = Printf.sprintf

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
    name = (if name <> "" then module_name name else "");
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
let submodules m = StringMap.fold (fun _ -> List.cons) m.submodules []
let add_vals vs m = { m with values = m.values @ vs }

let add_mod subm m =
  { m with submodules = StringMap.add subm.name subm m.submodules }

let find_submodule name m = StringMap.find_opt (module_name name) m.submodules

let compose ~name mods =
  let find_split modname p = function
    | t :: l when p t -> (t, l)
    | _ ->
        failwith
          (sprintf "Couldn't find ident from %s, maybe it was reordered?"
             modname)
  in
  let compose split compose xs =
    let found_idents, idents = xs |> List.map split |> List.split in
    let ident =
      match found_idents with
      | [] -> assert false
      | [ x ] -> x
      | x :: l -> List.fold_left (fun acc x -> compose acc x) x l
    in
    ident :: List.flatten idents
  in
  let types =
    compose
      (fun { name; types; _ } ->
        find_split name (fun t -> Type.name t = "t") types)
      Type.compose mods
  in
  let values =
    compose
      (fun { name; values; _ } ->
        find_split name (fun t -> Val.name t = "make") values)
      Val.compose mods
  in
  let submodules =
    List.fold_left
      (fun acc m ->
        StringMap.union
          (fun _ y ->
            failwith
              (sprintf "Submodule %s already exists in composition." y.name))
          acc m.submodules)
      StringMap.empty mods
  in
  {
    name;
    path = [];
    types;
    values;
    submodules;
    recursive = false;
    descr = None;
  }

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
    StringMap.fold
      (fun name m (defs, submods) ->
        let type_ = to_module_type m in
        let decl = module_declaration ~name:(Located.mk (Some m.name)) ~type_ in
        let s = psig_module decl in
        (* Definitions first to simplify references *)
        if name = "Definitions" then (Some s, submods) else (defs, s :: submods))
      m.submodules (None, [])
  in
  let types =
    List.concat_map (fun t -> Type.Sig.to_sig (Type.signature t)) m.types
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
    StringMap.fold
      (fun name m mods ->
        let name = Ast_builder.Located.mk (Some name) in
        Ast_builder.(
          pstr_module (module_binding ~name ~expr:(to_mod_structure m)))
        :: mods)
      m.submodules []
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

let split_ref reference =
  reference |> String.split_on_char '.'
  |> List.filter (( <> ) "")
  |> List.map module_name

let reference_module_path ~reference_base ~reference_root reference =
  let path = reference |> strip_base reference_base |> split_ref in
  qualified_name reference_root :: path

let reference_module ~reference_base ~reference_root reference =
  reference_module_path ~reference_base ~reference_root reference
  |> String.concat "."

let reference_type ~reference_base ~reference_root reference =
  reference_module ~reference_base ~reference_root reference ^ ".t"
open Printf
open Util

let default z = function
  | Some x -> x
  | None -> z

module StringSet = Set.Make (struct
  type t = string
  let compare = compare
end)

(* Unused types. *)
[@@@ocaml.warning "-34"]

type parameter_or_reference =
  [ `Parameter of Swagger_j.parameter
  | `Reference of Swagger_j.reference
  ]

type response_or_reference =
  [ `Response of Swagger_j.response
  | `Reference of Swagger_j.reference
  ]

[@@@end]

let merge_params (ps1 : Swagger_j.parameter list)
                 (ps2 : Swagger_j.parameter list) =
  let rec merge acc = function
    | [] -> acc
    | (p : Swagger_j.parameter)::ps ->
        let same_name (q : Swagger_j.parameter) =
          let open Swagger_j in
          p.name = q.name in
        if List.exists same_name acc
        then merge acc ps
        else merge (p::acc) ps in
  merge ps2 ps1

let reference_module_and_type ~reference_base ~reference_root r =
  let ref_module = Mod.reference_module ~reference_base ~reference_root r in
  let ref_type = sprintf "%s.t" ref_module in
  (Some ref_module, ref_type)

let parse_or_reference f json =
  let open Yojson.Basic.Util in
  let str = Yojson.Safe.to_string json in
  match json |> Yojson.Safe.to_basic |> member "$ref" with
  | `Null -> f str
  | _ -> failwith "reference not supported"

let parse_parameters = function
  | Some ps -> List.map (parse_or_reference Swagger_j.parameter_of_string) ps
  | None -> []

let parse_response r =
  parse_or_reference Swagger_j.response_of_string r

let parse_responses =
  List.map (fun (s, r) -> (s, parse_response r))

let resp_type ~reference_base ~reference_root (resp : Swagger_j.response) =
  match resp.schema with
  | None -> (None, "unit")
  | Some s ->
      let s = Schema.create ~reference_base ~reference_root s in
      match Schema.reference s with
      | Some r -> reference_module_and_type ~reference_base ~reference_root r
      | None -> (None, Schema.to_string s)

let rec return_type ~reference_root ~reference_base (resps : Swagger_j.responses) =
  let is_error code =
    if String.lowercase_ascii code = "default"
    then true
    else
      let code = int_of_string code in
      code < 200 || code >= 300 in
  let responses_match (r1 : Swagger_j.response)
                      (r2 : Swagger_j.response) =
    r1.schema = r2.schema in
  match resps with
  | [] -> None, "unit"
  | (code, _)::rs when is_error code ->
      (* ignore errors; assume strings *)
      return_type ~reference_root ~reference_base rs
  | (_code, resp)::rs ->
      (* check all 2xx responses return the same type *)
      let rec check first = function
        | [] ->
            ()
        | (code, _)::res when is_error code ->
            check first res
        | (_code', resp')::rs when responses_match first resp' ->
            check first rs
        | (_c, (_r : Swagger_j.response))::_ ->
            failwith "multiple response types are not supported" in
      let resp = parse_response resp in
      check resp (parse_responses rs);
      resp_type ~reference_base ~reference_root resp

let make_dups params =
  List.fold_left
    (fun dups (p : Swagger_j.parameter) ->
      match StringMap.find_opt p.name dups with
      | Some count -> StringMap.add p.name (count + 1) dups
      | None -> StringMap.add p.name 1 dups)
    StringMap.empty
    params

let operation_val ~root:_ ~reference_base ~reference_root name (params : Swagger_j.parameter list) = function
  | Some (op : Swagger_j.operation) ->
      let op_params = parse_parameters op.parameters in
      let params = merge_params params op_params in
      let dups = make_dups params in
      let param_sigs, param_impls =
        params
        |> List.map
             (fun (p : Swagger_j.parameter) ->
               let duplicate = StringMap.find p.name dups > 1 in
               Param.create ~duplicate ~reference_base ~reference_root p)
        |> List.split in
      let return_module, return_type =
        return_type ~reference_root ~reference_base op.responses in
      let verb = Val.Impl.http_verb_of_string name in
      let signature =
        let descr = op.description in
        Val.Sig.http_request ?descr name param_sigs return_type in
      let return =
        match return_module with
        | Some module_name -> Val.Impl.module_ module_name
        | None -> Val.Impl.type_ return_type in
      let implementation =
        Val.Impl.http_request verb name param_impls ~return in
      Some (Val.create signature implementation)
  | None ->
      None

let path_val path =
  Val.create
    (Val.Sig.constant "request_path_template")
    (Val.Impl.constant "request_path_template" path)

let path_item_vals ~root ~reference_base ~reference_root ~path (item : Swagger_j.path_item) : Val.t list =
  let params = parse_parameters item.parameters in
  let operation_val name =
    operation_val ~root ~reference_base ~reference_root name params in
  let get     = operation_val "get"     item.get in
  let put     = operation_val "put"     item.put in
  let post    = operation_val "post"    item.post in
  let delete  = operation_val "delete"  item.delete in
  let options = operation_val "options" item.options in
  let head    = operation_val "head"    item.head in
  let patch   = operation_val "patch"   item.patch in
  path_val path :: keep_some [get; put; post; delete; options; head; patch]

let definition_module ?(path = [])
                      ~root
                      ~reference_base
                      ~name
                      (schema : Swagger_j.schema) =
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
        let param_sig, param_impl =
          create_param name param_type required in
        (param_sig, param_impl) :: params)
      [] in

  let alias_type () =
    let param_type =
      Schema.kind_to_string
        (Schema.create ~reference_base ~reference_root:root schema) in
    let int_or_string =
      match schema.format with
      | Some "int-or-string" -> true
      | _ -> false in
    let typ =
      Type.create
        (Type.Sig.abstract "t")
        (Type.Impl.alias "t" param_type ~int_or_string) in
    let create =
      Val.create
        (Val.Sig.(pure "make" [positional param_type] "t"))
        (Val.Impl.(identity "make" [positional "t" "t"])) in
    ([typ], [create]) in

  let record_type () =
    let params = create_params properties in
    let sig_params, impl_params = params |> List.split in
    let create =
      Val.create
        (Val.Sig.pure "make" sig_params "t")
        (Val.Impl.record_constructor "make" impl_params) in
    let fields, values =
      List.fold_left
        (fun (fields, values) (name, schema) ->
          let s = Schema.create ~reference_base ~reference_root:root schema in
          let s = Schema.to_string s in
          let sig_type, impl_type =
            if List.mem name required then
              let type_ = sprintf "%s" s in
              (type_, type_)
            else
              let type_ = sprintf "%s option" s in
              (type_, sprintf "(%s [@default None])" type_) in
          let pname = Param.name name in
          let field =
            Type.Impl.record_field
              ~name:pname
              ~orig_name:name
              ~type_:impl_type in
          let value =
            let descr = schema.description in
            Val.create
              (Val.Sig.pure ?descr pname [Val.Sig.positional "t"] sig_type)
              (Val.Impl.record_accessor pname [Val.Impl.positional "t" "t"]) in
          (field :: fields, value :: values))
        ([], [])
        properties in
    let values = create :: List.rev values in
    let type_sig = Type.Sig.abstract "t" in
    let type_impl = Type.Impl.record "t" fields in
    let typ = Type.create type_sig type_impl in
    ([typ], values) in

  let unspec_type () =
    let typ =
      Type.create (Type.Sig.unspecified "t") (Type.Impl.unspecified "t") in
    ([typ], []) in

  let types, values =
    match schema.kind, schema.properties with
    | Some `Object, _ -> record_type ()
    | Some _, _ -> alias_type ()
    | None, Some _ -> record_type ()
    | None, None -> unspec_type () in

  let descr = schema.description in
  Mod.create ?descr ~name ~path ~types ~values ()

let rec insert_module m root = function
  | [] ->
      Mod.add_mod m root
  | p::ps ->
      match Mod.find_submodule p root with
      | Some subm ->
          Mod.add_mod (insert_module m subm ps) root
      | None ->
          let subm = Mod.empty p ~path:(Mod.qualified_path root) () in
          Mod.add_mod (insert_module m subm ps) root

(* Unused values. *)
[@@@ocaml.warning "-32"]

let remove_base base segments =
  match base, segments with
  | Some base, s::ss when base = s -> ss
  | _ -> segments

[@@@end]

let rec build_paths ~root ~path_base ~reference_base ~reference_root = function
  | [] ->
      root
  | (path, item) :: paths ->
      let parents_and_child =
        path
        |> Mod.strip_base path_base
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
        |> Mod.strip_base definition_base
        |> Mod.split_ref
        |> unsnoc in
      (match parents_and_child with
      | Some (parents, child) ->
          let def =
            definition_module
              ~root ~reference_base ~path:parents ~name:child schema in
          let root = insert_module def root parents in
          build_definitions ~root ~definition_base ~reference_base defs
      | None ->
          let root =
            Mod.add_mod
              (definition_module ~root ~reference_base ~name schema)
              root in
          build_definitions ~root ~definition_base ~reference_base defs)
  (* XXX Ignore schemas that are simply references? Just use the referenced
   * module? In the kubernetes API this seems to be only for deprecated
   * stuff. *)
  | (_name, (_schema : Swagger_j.schema)) :: defs ->
      build_definitions ~root ~definition_base ~reference_base defs

let of_swagger ?(path_base = "")
               ?(definition_base = "")
               ?(reference_base = "")
               ~reference_root s =
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

let object_module = String.trim {|
module Object = struct
  module type Value = sig
    type value
    val value_of_yojson : Yojson.Safe.t -> (value, string) result
    val value_to_yojson : value -> Yojson.Safe.t
  end

  module type S = sig
    type value
    type t = (string * value) list [@@deriving yojson]
  end

  module Make (V : Value) : S with type value := V.value = struct
    type t = (string * V.value) list [@@deriving yojson]

    let to_yojson obj =
      `Assoc (List.map (fun (k, v) -> (k, V.value_to_yojson v)) obj)

    let of_yojson (obj : Yojson.Safe.t) : (t, string) result =
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | (k, v) :: obj ->
            match V.value_of_yojson v with
            | Ok v -> loop ((k, v) :: acc) obj
            | Error e -> Error ("invalid object:" ^ e) in
      match obj with
      | `Assoc obj -> loop [] obj
      | _ -> Error "invalid object"
  end

  module Of_strings = Make (struct type value = string [@@deriving yojson] end)
  module Of_floats  = Make (struct type value = float  [@@deriving yojson] end)
  module Of_ints    = Make (struct type value = int    [@@deriving yojson] end)
  module Of_bools   = Make (struct type value = bool   [@@deriving yojson] end)
end
|}

let to_string m =
  sprintf "%s\n\n%s" object_module (Mod.to_string m)

open Util

let sprintf = Printf.sprintf

type t = {
  raw : Swagger_t.schema;
  reference_base : string;
  reference_root : Mod.t;
}

let create ~reference_base ~reference_root raw =
  { raw; reference_base; reference_root }

let reference t = t.raw.reference

let rec to_type t =
  let reference_base = t.reference_base in
  let reference_root = t.reference_root in
  match t.raw.reference with
  | Some r ->
      let t = Mod.reference_type ~reference_base ~reference_root r in
      Ast_builder.(ptyp_constr (Located.lident t) [])
  | None -> (
      match Option.get t.raw.kind with
      | `String -> [%type: string]
      | `Number -> [%type: float]
      | `Integer -> [%type: int]
      | `Boolean -> [%type: bool]
      | `Object -> (
          let open Swagger_t in
          match t.raw.additional_properties with
          | Some props -> (
              match (props.reference, props.kind) with
              | Some r, _ ->
                  let t =
                    Mod.reference_module ~reference_base ~reference_root r
                  in
                  Ast_builder.(
                    ptyp_constr (Located.lident (sprintf "%s.Object.t" t)) [])
              | None, Some `String -> [%type: Object.Of_strings.t]
              | None, Some `Number -> [%type: Object.Of_floats.t]
              | None, Some `Integer -> [%type: Object.Of_ints.t]
              | None, Some `Boolean -> [%type: Object.Of_bools.t]
              | None, _ ->
                  let t =
                    to_type (create ~reference_base ~reference_root props)
                  in
                  [%type: (string * [%t t]) list])
          | None -> [%type: unit])
      | `Array -> (
          let open Swagger_t in
          match t.raw.items with
          | Some s ->
              let t = to_type (create ~reference_base ~reference_root s) in
              [%type: [%t t] list]
          | None ->
              failwith
                "Schema.kind_to_string: array type must have an 'items' field"))

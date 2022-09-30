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
  match (t.raw.discriminator, t.raw.all_of, t.raw.reference, t.raw.kind) with
  | Some discriminator, None, None, None ->
      polymorphism ~reference_base ~reference_root t discriminator
  | None, Some all_of, None, None ->
      composition ~reference_base ~reference_root t all_of
  | None, None, Some reference, None ->
      let t = Mod.reference_type ~reference_base ~reference_root reference in
      Ast_builder.(ptyp_constr (Located.lident t) [])
  | None, None, None, Some kind ->
      plain_type ~reference_base ~reference_root t kind
  | _ -> assert false

and polymorphism ~reference_base ~reference_root t discriminator =
  ignore reference_base;
  ignore reference_root;
  ignore t;
  failwith
    (sprintf "allOf polymorphism (discriminator %s) isn't implemented."
       discriminator)

and composition ~reference_base ~reference_root t all_of =
  ignore reference_base;
  ignore reference_root;
  ignore t;
  ignore all_of;
  failwith (sprintf "allOf composition isn't implemented.")

and plain_type ~reference_base ~reference_root t kind =
  match kind with
  | `String -> (
      match t.raw.format with
      | Some `Byte | Some `Binary | Some `Password | None -> [%type: string]
      | Some `Date -> [%type: float]
      | Some `Date_time -> [%type: float]
      | Some (`Other format) -> (
          match format with
          | "ip-address" -> [%type: string] (* Docker *)
          | "CIDR" -> [%type: string] (* Docker *)
          | "dateTime" ->
              [%type: float] (* Docker, alternative spelling for "date-time" *)
          | _ -> failwith (sprintf "Unsupported %s string format." format))
      | _ -> failwith "Invalid format for string data type.")
  | `Number -> [%type: float]
  | `Integer -> (
      match t.raw.format with
      | Some `Int32 -> [%type: int32]
      | Some `Int64 -> [%type: int64]
      | None -> [%type: int]
      | Some (`Other format) -> (
          match format with
          | "uint8" -> [%type: Stdint.Uint8.t] (* Docker *)
          | "uint16" -> [%type: Stdint.Uint16.t] (* Docker *)
          | "uint32" -> [%type: Stdint.Uint32.t] (* Docker *)
          | "uint64" -> [%type: Stdint.Uint64.t] (* Docker *)
          | _ -> failwith (sprintf "Unsupported %s integer format." format))
      | _ -> failwith "Invalid format for integer data type.")
  | `Boolean -> [%type: bool]
  | `Object -> (
      match t.raw.additional_properties with
      | Some props -> (
          match (props.reference, props.kind) with
          | Some r, _ ->
              let t = Mod.reference_module ~reference_base ~reference_root r in
              Ast_builder.(
                ptyp_constr (Located.lident (sprintf "%s.Object.t" t)) [])
          | None, Some `String -> (
              match t.raw.format with
              | Some `Byte | Some `Binary | Some `Password | None ->
                  [%type: Object.Of_strings.t]
              | Some `Date -> [%type: float]
              | Some `Date_time -> [%type: float]
              | Some (`Other format) ->
                  failwith (sprintf "Unsupported %s string format." format)
              | _ -> failwith "Invalid format for string data type.")
          | None, Some `Number -> [%type: Object.Of_floats.t]
          | None, Some `Integer -> (
              match props.format with
              | Some `Int32 -> [%type: Object.Of_ints32.t]
              | Some `Int64 -> [%type: Object.Of_ints64.t]
              | None -> [%type: Object.Of_ints.t]
              | Some (`Other format) ->
                  failwith (sprintf "Unsupported %s integer format." format)
              | _ -> failwith "Invalid format for integer data type.")
          | None, Some `Boolean -> [%type: Object.Of_bools.t]
          | None, _ ->
              let t = to_type (create ~reference_base ~reference_root props) in
              [%type: (string * [%t t]) list])
      | None -> [%type: unit])
  | `Array -> (
      match t.raw.items with
      | Some s ->
          let t = to_type (create ~reference_base ~reference_root s) in
          [%type: [%t t] list]
      | None ->
          failwith
            "Schema.kind_to_string: array type must have an 'items' field")

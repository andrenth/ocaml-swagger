open Printf
open Util

type t = Swagger_t.parameter

let rec item_kind_to_ptyp (items : Swagger_t.items option) kind =
  let open Ppxlib in
  match kind with
  | `String -> [%type: string]
  | `Number -> [%type: float]
  | `Integer -> [%type: int]
  | `Boolean -> [%type: bool]
  | `Array -> (
      match items with
      | Some is ->
          let t = item_kind_to_ptyp is.items is.kind in
          [%type: [%t t] list]
      | None ->
          failwith
            "Param.item_kind_to_ptyp: array type must have an 'items' field")

let kind_to_ptyp (p : t) =
  let open Ppxlib in
  match Option.get p.kind with
  | `String -> [%type: string]
  | `Number -> [%type: float]
  | `Integer -> [%type: int]
  | `Boolean -> [%type: bool]
  | `File -> [%type: file]
  | `Array -> (
      match p.items with
      | Some items ->
          let t = item_kind_to_ptyp items.items items.kind in
          [%type: [%t t] array]
      | None ->
          failwith "Param.kind_to_ptyp: array type must have an 'items' field")

let name n =
  let n = if n.[0] = '$' then String.sub n 1 (String.length n - 1) else n in
  let n = snake_case n |> String.lowercase_ascii in
  if Ppxlib.Keyword.is_keyword n then n ^ "_" else n

let string_of_location = function
  | `Query -> "query"
  | `Header -> "header"
  | `Path -> "path"
  | `FormData -> "formData"
  | `Body -> "body"

let create ?(duplicate = false) ~reference_base ~reference_root (p : t) =
  let t =
    match p.location with
    | `Body ->
        Codegen_schema.create ~reference_base ~reference_root
          (Option.get p.schema)
        |> Codegen_schema.to_type
    | _ -> kind_to_ptyp p
  in
  let n =
    let n = name p.name in
    let loc = string_of_location p.location in
    if duplicate && n <> loc then sprintf "%s_%s" loc n else n
  in
  let descr = p.description in
  let create_sig, create_impl =
    if p.required then (Val.Sig.labelled, Val.Impl.labelled)
    else (Val.Sig.optional, Val.Impl.optional)
  in
  (create_sig ?descr n t, create_impl n t ~origin:(Val.Impl.origin p))

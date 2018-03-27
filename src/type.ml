open Printf
open Util

module Sig = struct
  type t =
    | Abstract of string * string option
    | Unspecified of string * string option

  let abstract ?descr name = Abstract (name, descr)
  let unspecified ?descr name = Unspecified (name, descr)

  let to_string ?(indent = 0) t =
    let pad = String.make indent ' ' in
    let name, doc, rest =
      match t with
      | Abstract (name, Some descr) ->
          let descr = format_comment descr in
          name, sprintf "%s(** %s *)\n" pad descr, " [@@deriving yojson]"
      | Abstract (name, None) ->
          name, "", " [@@deriving yojson]"
      | Unspecified (name, Some descr) ->
          let descr = format_comment descr in
          name, sprintf "%s(** %s *)\n" pad descr, " = Yojson.json"
      | Unspecified (name, None) ->
          name, "", " = Yojson.json" in
    sprintf "%s%stype %s%s\n" doc pad name rest
end

module Impl = struct
  type record_field =
    { name : string
    ; orig_name : string
    ; type_ : string
    }

  type t =
    | Alias of string * string
    | Record of string * record_field list
    | Unspecified of string

  let record_field ~name ~orig_name ~type_ =
    { name; orig_name; type_ }

  let alias name target = Alias (name, target)
  let record name fields = Record (name, fields)
  let unspecified name = Unspecified name

  let to_string ?(indent = 0) t =
    let pad = String.make indent ' ' in
    match t with
    | Unspecified name ->
        sprintf "%stype %s = Yojson.json\n" pad name
    | Alias (name, target) ->
        sprintf "%stype %s = %s [@@deriving yojson]\n" pad name target
    | Record (name, fields) ->
        let s =
          List.fold_left
            (fun acc { name; orig_name; type_ } ->
               let attr =
                 if name = orig_name then ""
                 else sprintf " [@key \"%s\"]" orig_name in
               sprintf "%s %s : %s%s;" acc name type_ attr)
            ""
            fields in
        sprintf "%stype %s = {%s } [@@deriving yojson]\n" pad name s
end

type t =
  { signature : Sig.t
  ; implementation : Impl.t
  }

let create signature implementation = { signature; implementation }

let name t =
  match t.signature with
  | Sig.Abstract (n, _) | Sig.Unspecified (n, _) -> n

let signature t =
  t.signature

let implementation t =
  t.implementation

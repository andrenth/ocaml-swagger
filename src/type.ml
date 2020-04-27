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
          name, sprintf "%s(** %s *)\n" pad descr, ""
      | Abstract (name, None) ->
          name, "", ""
      | Unspecified (name, Some descr) ->
          let descr = format_comment descr in
          let doc = sprintf "%s(** %s *)\n" pad descr in
          name, doc, " = Yojson.Safe.t"
      | Unspecified (name, None) ->
          name, "", " = Yojson.Safe.t" in
    sprintf "%s%stype %s%s [@@deriving yojson]\n" doc pad name rest
end

module Impl = struct
  type record_field =
    { name : string
    ; orig_name : string
    ; type_ : string
    }

  type t =
    | Alias of { name : string; target : string; int_or_string : bool }
    | Record of string * record_field list
    | Unspecified of string

  let record_field ~name ~orig_name ~type_ =
    { name; orig_name; type_ }

  let alias ?(int_or_string=false) name target =
    if int_or_string && target <> "string" then begin
      invalid_arg "Type.alias: int_or_string only supported for string types"
    end;
    Alias {name; target; int_or_string}

  let record name fields = Record (name, fields)
  let unspecified name = Unspecified name

  let int_or_string_to_yojson = {|
    let to_yojson t =
      match Yojson.Safe.from_string t with
      (* Valid JSON integer. *)
      | (`Int _) as int -> int

      (* Not a valid JSON integer, use original to_yojson function. *)
      | _ -> to_yojson t
      | exception Yojson.Json_error _ -> to_yojson t
  |}

  let int_or_string_of_yojson = {|
    let of_yojson j =
      match j with
      | `Int _ -> Ok (Yojson.Safe.to_string j)
      | _ -> of_yojson j
  |}

  let to_string ?(indent = 0) t =
    let pad = String.make indent ' ' in
    match t with
    | Unspecified name ->
      let type_ = sprintf "%stype %s = Yojson.Safe.t" pad name in
      sprintf "%s [@@deriving yojson]\n" type_

    | Alias {name; target; int_or_string = false} ->
      let type_ = sprintf "%stype %s = %s" pad name target in
      sprintf "%s [@@deriving yojson]\n" type_

    | Alias {name; target; int_or_string = true} ->
      (* Aliases for string types with "int-or-string" format, in addition to
         the automatically generated yojson functions, will generate helper
         wrappers that check if the string value represents a valid integer. If
         that is the case the equivvalent JSON values will be encoded as ints. *)

      (* Only string aliases with int_or_string can be constructed. *)
      assert (target = "string");

      let type_ = sprintf "%stype %s = %s" pad name target in
      sprintf "%s [@@deriving yojson]\n\n%s\n\n%s" type_
        int_or_string_to_yojson
        int_or_string_of_yojson

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
      let type_ = sprintf "%stype %s = {%s }" pad name s in
      sprintf "%s [@@deriving yojson]\n" type_
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

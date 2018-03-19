open Printf

module Sig = struct
  type t =
    | Abstract of string
    | Phantom of string

  let abstract name = Abstract name
  let phantom name = Phantom name

  let to_string ?(indent = 0) t =
    let pad = String.make indent ' ' in
    let name, attr =
      match t with
      | Abstract name -> name, " [@@deriving yojson]\n"
      | Phantom name -> name, "" in
    sprintf "%stype %s%s\n" pad name attr
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
    | Phantom of string

  let alias name target = Alias (name, target)
  let record name fields = Record (name, fields)
  let phantom name = Phantom name

  let to_string ?(indent = 0) t =
    let pad = String.make indent ' ' in
    match t with
    | Phantom name -> sprintf "%stype %s\n" pad name
    | Alias (name, target) -> sprintf "%stype %s = %s [@@deriving yojson]\n" pad name target
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

module Object = struct
  module type Value = sig
    type value

    val value_of_yojson : Yojson.Safe.t -> value
    val yojson_of_value : value -> Yojson.Safe.t
  end

  module type S = sig
    type value
    type t = (string * value) list [@@deriving yojson]
  end

  module Make (V : Value) : S with type value := V.value = struct
    type t = (string * V.value) list [@@deriving yojson]

    let yojson_of_t obj =
      `Assoc (List.map (fun (k, v) -> (k, V.yojson_of_value v)) obj)

    let t_of_yojson (obj : Yojson.Safe.t) : t =
      let rec loop acc = function
        | [] -> List.rev acc
        | (k, v) :: obj ->
            let v = V.value_of_yojson v in
            loop ((k, v) :: acc) obj
      in
      match obj with
      | `Assoc obj -> loop [] obj
      | _ -> invalid_arg "invalid object"
  end

  module Of_strings = Make (struct
    type value = string [@@deriving yojson]
  end)

  module Of_floats = Make (struct
    type value = float [@@deriving yojson]
  end)

  module Of_ints = Make (struct
    type value = int [@@deriving yojson]
  end)

  module Of_bools = Make (struct
    type value = bool [@@deriving yojson]
  end)
end

module rec Composition : sig
  module Definitions : sig
    module Error_model : sig
      type t [@@deriving yojson]

      val make : code:int -> message:string -> unit -> t
      val message : t -> string

      val set_message : string -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the message field.|ocamlswagger}]

      val code : t -> int

      val set_code : int -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the code field.|ocamlswagger}]

      module Object : Object.S with type value := t
    end

    module ExtendedErrorModel : sig
      type t [@@deriving yojson]

      val make : code:int -> message:string -> root_cause:string -> unit -> t
      val message : t -> string

      val set_message : string -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the message field.|ocamlswagger}]

      val code : t -> int

      val set_code : int -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the code field.|ocamlswagger}]

      val root_cause : t -> string

      val set_root_cause : string -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the root_cause field.|ocamlswagger}]

      module Object : Object.S with type value := t
    end
  end
end = struct
  module Definitions = struct
    module Error_model = struct
      type t = { message : string; code : int } [@@deriving yojson]

      let make ~code ~message () = { message; code }
      let message t = t.message

      let set_message message t =
        { t with message } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let code t = t.code

      let set_code code t =
        { t with code } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      module Object = Object.Make (struct
        type value = t [@@deriving yojson]
      end)
    end

    module ExtendedErrorModel = struct
      type t = {
        root_cause : string; [@key "rootCause"]
        message : string;
        code : int;
      }
      [@@deriving yojson]

      let make ~code ~message ~root_cause () = { root_cause; message; code }
      let message t = t.message

      let set_message message t =
        { t with message } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let code t = t.code

      let set_code code t =
        { t with code } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let root_cause t = t.root_cause

      let set_root_cause root_cause t =
        { t with root_cause } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      module Object = Object.Make (struct
        type value = t [@@deriving yojson]
      end)
    end
  end
end

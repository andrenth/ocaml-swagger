module Sig : sig
  type param
  type kind
  type io = [`Lwt | `Async]
  type return =
    | Simple of string
    | Async of io * string
    | Custom_async of (io * (string -> string) * string)
  type t =
    { name      : string
    ; params    : param list
    ; return    : return
    ; kind      : kind
    ; descr     : string list
    }

  val name : t -> string
  val return : t -> return
  val return_to_string : return -> string

  val named : ?descr:string -> string -> string -> param
  val optional : ?descr:string -> string -> string -> param
  val positional : string -> param

  val constant : string -> t
  val pure : ?descr:string -> string -> param list -> string -> t
  val http_request : ?descr:string -> string -> param list -> string -> io -> t

  val to_string : ?indent:int -> t -> string
end

module Impl : sig
  type param
  type origin
  type http_verb = Get | Put | Post | Delete | Head | Patch | Options
  type return =
    | Module of string
    | Type of string
    | Custom of
      (
        (  string
        -> client_fun:string
        -> ctx:string
        -> body_param:string
        -> verb:http_verb
        -> string
        )
        * string
      )
  type io = [`Lwt | `Async]
  type http_request =
    { verb      : http_verb
    ; io        : io
    ; return    : return
    }
  type kind =
    | Record_constructor
    | Record_accessor
    | Identity
    | Constant of string
    | Http_request of http_request
    | Derived
  type t =
    { name      : string
    ; params    : param list
    ; kind      : kind
    }

  val name : t -> string
  val kind : t -> kind

  val named : ?origin:origin -> string -> string -> param
  val optional : ?origin:origin -> string -> string -> param
  val positional : string -> string -> param

  val constant : string -> string -> t
  val identity : string -> param list -> t
  val record_constructor : string -> param list -> t
  val record_accessor : string -> param list -> t
  val http_request : return:return -> http_verb -> io -> string -> param list -> t

  val origin : Swagger_t.parameter -> origin

  val http_verb_of_string : string -> http_verb

  val module_ : string -> return
  val type_ : string -> return

  val to_string : ?indent:int -> t -> string
end

type t =
  { signature : Sig.t
  ; implementation : Impl.t
  }

val create : Sig.t -> Impl.t -> t

val signature : t -> Sig.t
val implementation : t -> Impl.t

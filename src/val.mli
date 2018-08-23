module Sig : sig
  type t
  type param
  type io = [`Lwt | `Async]

  val named : ?descr:string -> string -> string -> param
  val optional : ?descr:string -> string -> string -> param
  val positional : string -> param

  val constant : string -> t
  val pure : ?descr:string -> string -> param list -> string -> t
  val http_request : ?descr:string -> string -> param list -> string -> io -> t

  val to_string : ?indent:int -> t -> string
end

module Impl : sig
  type t
  type param
  type origin
  type http_verb
  type return
  type io = [`Lwt | `Async]

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

type t

val create : Sig.t -> Impl.t -> t

val signature : t -> Sig.t
val implementation : t -> Impl.t

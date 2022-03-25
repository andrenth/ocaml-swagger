type t

val empty : string -> ?recursive:bool -> ?path:string list -> unit -> t

val create :
  name:string ->
  ?descr:string ->
  ?recursive:bool ->
  ?path:string list ->
  ?types:Type.t list ->
  ?submodules:t Util.StringMap.t ->
  ?values:Val.t list ->
  unit ->
  t

val with_values :
  string -> ?recursive:bool -> ?path:string list -> Val.t list -> t

val name : t -> string
val qualified_name : t -> string
val submodules : t -> t list
val path : t -> string list
val qualified_path : t -> string list
val add_mod : t -> t -> t
val add_vals : Val.t list -> t -> t
val find_submodule : string -> t -> t option

val reference_module :
  reference_base:string -> reference_root:t -> string -> string

val reference_type :
  reference_base:string -> reference_root:t -> string -> string

val split_ref : string -> string list
val strip_base : string -> string -> string
val to_mod : t -> Ppxlib.Ast.structure

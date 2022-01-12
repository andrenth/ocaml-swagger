(** Generate OCaml libraries from Swagger 2.0 specs, from various sources.  *)

val codegen_from_string :
  path_base:string ->
  definition_base:string ->
  reference_base:string ->
  reference_root:string ->
  ?output:out_channel ->
  string ->
  unit
(** Generate an OCaml library from a Swagger spec read from a string. *)

val codegen_from_channel :
  path_base:string ->
  definition_base:string ->
  reference_base:string ->
  reference_root:string ->
  ?output:out_channel ->
  in_channel ->
  unit
(** Generate an OCaml library from a Swagger spec read from a channel. *)

val codegen_from_file :
  path_base:string ->
  definition_base:string ->
  reference_base:string ->
  reference_root:string ->
  ?output:out_channel ->
  string ->
  unit
(** Generate an OCaml library from a Swagger spec read from a file. *)

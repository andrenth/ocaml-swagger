open Printf

let snake_case =
  let re1 = Re.Pcre.regexp "([A-Z]+)([A-Z][a-z]{2,})" in
  let re2 = Re.Pcre.regexp "([a-z0-9])([A-Z])" in
  let re3 = Re.Pcre.regexp "-| " in
  let re4 = Re.Pcre.regexp "^\\." in
  let underscore re s =
    let replace groups =
      sprintf "%s_%s" (Re.Group.get groups 1) (Re.Group.get groups 2)
    in
    Re.replace re ~f:replace s
  in
  fun s ->
    let len = String.length s in
    if len > 1 then
      let s = underscore re1 s in
      let s = underscore re2 s in
      let s = Re.replace_string re3 ~by:"_" s in
      let s = Re.replace_string re4 ~by:"dot_" s in
      sprintf "%c" s.[0]
      ^ String.lowercase_ascii (String.sub s 1 (String.length s - 1))
    else s

let format_comment =
  let re = Re.Pcre.regexp "[{}@\\[\\]]" in
  let snake_case = function
    | "CamelCase" -> "CamelCase"
    | w when String.length w > 6 && String.sub w 0 7 = "http://" -> w
    | w when String.length w > 7 && String.sub w 0 8 = "https://" -> w
    | w -> snake_case w
  in
  fun text ->
    text
    |> Re.replace re ~f:(fun g -> "\\" ^ Re.Group.get g 0)
    |> String.split_on_char ' ' |> List.map snake_case |> String.concat " "

let loc = Ppxlib.Location.none

module Ast_builder = Ppxlib.Ast_builder.Make (struct
  let loc = loc
end)

let ocaml_doc descr =
  let open Ast_builder in
  let name = Located.mk "ocaml.doc" in
  let doc = pexp_constant (Pconst_string (descr, loc, Some "ocamlswagger")) in
  let doc = pstr_eval [%expr [%e doc]] [] in
  attribute ~name ~payload:(PStr [ doc ])

let ocaml_warning ~number =
  let open Ast_builder in
  let name = Located.mk "ocaml.warning" in
  let doc =
    pexp_constant
      (Pconst_string ("-" ^ string_of_int number, loc, Some "ocamlswagger"))
  in
  let doc = pstr_eval [%expr [%e doc]] [] in
  attribute ~name ~payload:(PStr [ doc ])

let unsnoc l =
  let rec go acc = function
    | [] -> None
    | [ x ] -> Some (List.rev acc, x)
    | x :: xs -> go (x :: acc) xs
  in
  go [] l

let keep_some l = List.filter_map Fun.id l
let opt_cons o l = match o with Some o -> o :: l | None -> l

module StringMap = Map.Make (struct
  type t = string

  let compare = compare
end)

module StringSet = Set.Make (struct
  type t = string

  let compare = compare
end)

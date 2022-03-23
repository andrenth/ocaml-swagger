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

let unsnoc l =
  let rec go acc = function
    | [] -> None
    | [ x ] -> Some (List.rev acc, x)
    | x :: xs -> go (x :: acc) xs
  in
  go [] l

let keep_some l = List.filter_map Fun.id l

module StringMap = Map.Make (struct
  type t = string

  let compare = compare
end)

open Printf

module StringMap = Map.Make (struct
  type t = string
  let compare = compare
end)

let rec keep_some = function
  | [] -> []
  | Some x :: xs -> x :: keep_some xs
  | None :: xs -> keep_some xs

let snake_case =
  let re1 = Re.Pcre.regexp "([A-Z]+)([A-Z][a-z]{2,})" in
  let re2 = Re.Pcre.regexp "([a-z0-9])([A-Z])" in
  let re3 = Re.compile (Re.Pcre.re "-") in
  let underscore re s =
    let replace groups =
      sprintf "%s_%s"
        (Re.Group.get groups 1)
        (Re.Group.get groups 2) in
    Re.replace re replace s in
  fun s ->
    let len = String.length s in
    if len > 1 then
      let s = underscore re1 s in
      let s = underscore re2 s in
      let s = Re.replace_string re3 ~by:"_" s in
      sprintf "%c" s.[0]
      ^ String.lowercase_ascii (String.sub s 1 (String.length s - 1))
    else
      s

let unsnoc l =
  let rec go acc = function
    | [] -> None
    | [x] -> Some (List.rev acc, x)
    | x::xs -> go (x::acc) xs in
  go [] l

let some = function
  | Some x -> x
  | None -> failwith "some: None"

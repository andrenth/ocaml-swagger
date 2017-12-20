open Printf

let () =
  let ic = open_in "swagger.json" in
  let s = really_input_string ic (in_channel_length ic) in
  let swagger = Swagger_j.swagger_of_string s in
  let s = Swagger_j.string_of_swagger swagger in
  printf "%b\n%!" (swagger = Swagger_j.swagger_of_string s)

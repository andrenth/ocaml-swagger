open Printf

let () =
  let ic = open_in "src/swagger.json" in
  let s = really_input_string ic (in_channel_length ic) in
  let swagger = Swagger_j.swagger_of_string s in
  printf "%s\n%!" (Sig_gen.of_swagger ~base:"api" swagger |> Sig_gen.to_string)

open Printf

let codegen_from_string ~path_base ~definition_base ~reference_base
    ~reference_root ?(output = stdout) input =
  let swagger = Swagger_j.swagger_of_string input in
  Gen.of_swagger ~path_base ~definition_base ~reference_base ~reference_root
    swagger
  |> Gen.to_string |> fprintf output "%s\n%!"

let read_channel input =
  let buffer = Buffer.create 4096 in
  let rec aux () =
    try
      Buffer.add_channel buffer input 4096;
      aux ()
    with End_of_file -> Buffer.contents buffer
  in
  aux ()

let codegen_from_channel ~path_base ~definition_base ~reference_base
    ~reference_root ?output input =
  codegen_from_string ~path_base ~definition_base ~reference_base
    ~reference_root ?output (read_channel input)

let codegen_from_file ~path_base ~definition_base ~reference_base
    ~reference_root ?output input =
  let ic = open_in_bin input in
  let input = really_input_string ic (in_channel_length ic) in
  codegen_from_string ~path_base ~definition_base ~reference_base
    ~reference_root ?output input

let usage_msg = "ocaml-swagger -reference-root <root> [-o <output>] [<input>]"
let path_base = ref ""
let definition_base = ref ""
let reference_base = ref ""
let reference_root = ref ""
let output_file = ref ""
let input_file = ref ""
let anon_fun filename = input_file := filename

let speclist =
  [
    ("-path", Arg.Set_string path_base, " Path base");
    ("-definition", Arg.Set_string definition_base, " Definition base");
    ("-reference-base", Arg.Set_string reference_base, " Reference base");
    ("-reference-root", Arg.Set_string reference_root, " Reference root");
    ("-o", Arg.Set_string output_file, " Set output file name");
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;
  if !reference_root = "" then (
    prerr_endline "-reference-root cannot be empty.";
    exit 1);
  let output = if !output_file = "" then stdout else open_out !output_file in
  if !input_file = "" then
    Swagger.codegen_from_channel ~path_base:!path_base
      ~definition_base:!definition_base ~reference_base:!reference_base
      ~reference_root:!reference_root ~output stdin
  else
    Swagger.codegen_from_file ~path_base:!path_base
      ~definition_base:!definition_base ~reference_base:!reference_base
      ~reference_root:!reference_root ~output !input_file

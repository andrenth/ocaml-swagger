open! Core
open! Async

let rec loop w p acc =
  let finish () = Pipe.close w; return () in
  match%bind Pipe.read p with
  | `Eof -> finish ()
  | `Ok chunk ->
    let chunk = acc ^ chunk in
    let lexer_state = Yojson.init_lexer () in
    let lexbuf = Lexing.from_string chunk in
    let rec emit () =
      let start = lexbuf.lex_curr_pos in
      try
        let json = Yojson.Safe.from_lexbuf ~stream:true lexer_state lexbuf in
        Pipe.write w json >>= emit
      with (Yojson.Json_error _) ->
          let len = lexbuf.lex_buffer_len - start in
          let incomplete = Bytes.sub lexbuf.lex_buffer ~pos:start ~len:(len - start) in
          return incomplete
    in
    try_with ~extract_exn:true (fun () ->
      emit () >>= (fun s -> loop w p (Bytes.to_string s))
    ) >>= function
    | Ok () -> finish ()
    | Error Yojson.End_of_input -> finish ()
    | Error e -> raise e

let json_pipe_of_fname x =
  let r,w = Pipe.create () in
  don't_wait_for (
    Reader.with_file x ~f:(fun reader ->
      loop w (Reader.pipe reader) ""
    )
  );
  r

let main () =
  json_pipe_of_fname "aoeu.json"
  |> Pipe.iter ~f:(fun json ->
    printf "~ %s\n" (Yojson.Safe.to_string json) |> return)

let () =
  Command.Param.return main
  |> Command.async ~summary:"the game"
  |> Command.run

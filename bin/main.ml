open Core
open Spit

let eval_filename filename =
  let ast =
    if String.equal filename "-"
    then Parser.parse_chan In_channel.stdin
    else Parser.parse_file filename
  in
  try
    let ress = List.map ~f:Evaluator.eval ast in
    List.last ress |> Option.value ~default:Value.Nil |> Value.to_string |> print_endline
  with
  | err ->
    print_string (Stdlib.Printexc.to_string err);
    Out_channel.newline stdout;
    Out_channel.flush stdout;
    exit 1
;;

let repl () =
  let rec user_input prompt cb =
    match LNoise.linenoise prompt with
    | None -> ()
    | Some v ->
      cb v;
      user_input prompt cb
  in
  LNoise.history_load ~filename:".spit-history" |> ignore;
  LNoise.history_set ~max_length:10000 |> ignore;
  let env = Evaluator.stdlib () in
  user_input "spit -> " (fun input ->
    if String.equal input "quit" then exit 0;
    LNoise.history_add input |> ignore;
    LNoise.history_save ~filename:".spit-history" |> ignore;
    try
      let ast = Parser.parse_string input in
      match List.map ~f:(Evaluator.eval ~env) ast with
      | [] -> print_endline ""
      | res :: [] -> Value.to_string res |> print_endline
      | ress ->
        List.map ress ~f:(fun res -> Value.to_string res |> print_endline) |> ignore
    with
    | err ->
      print_string (Stdlib.Printexc.to_string err);
      Out_channel.newline stdout;
      Out_channel.flush stdout)
;;

let command =
  Command.basic
    ~summary:"A simple Lisp interpreter, in OCaml"
    (let%map_open.Command filename =
       anon (maybe ("filename" %: Filename_unix.arg_type))
     in
     fun () ->
       match filename with
       | Some fn -> eval_filename fn
       | None -> repl ())
;;

let () =
  Parser.pp_exceptions ();
  Command_unix.run ~version:"1.0" ~build_info:"RWO" command
;;

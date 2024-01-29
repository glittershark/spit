open Core

let eval_filename filename =
  let ast =
    if String.equal filename "-" then Spit.Parser.parse_chan In_channel.stdin
    else Spit.Parser.parse_file filename
  in
  let ress = List.map ~f:Spit.Evaluator.eval ast in
  List.last ress
  |> Option.value ~default:Spit.Value.Nil
  |> Printf.printf !"%{sexp:Spit.Value.t}"

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
  let env = Spit.Evaluator.stdlib () in
  user_input "spit -> " (fun input ->
      if String.equal input "quit" then exit 0;
      LNoise.history_add input |> ignore;
      LNoise.history_save ~filename:".spit-history" |> ignore;
      try
        let ast = Spit.Parser.parse_string input in
        match List.map ~f:(Spit.Evaluator.eval ~env) ast with
        | [] -> print_endline ""
        | res :: [] ->
            Printf.sprintf !"%{sexp:Spit.Value.t}\n" res
            |> print_endline
        | ress ->
            List.map ress ~f:(fun res ->
                Printf.sprintf !"%{sexp:Spit.Value.t}\n" res
                |> print_endline)
            |> ignore
      with err ->
        print_string (Stdlib.Printexc.to_string err);
        Out_channel.newline stdout;
        Out_channel.flush stdout)

let command =
  Command.basic ~summary:"A simple Lisp interpreter, in OCaml"
    (let%map_open.Command filename =
       anon (maybe ("filename" %: Filename_unix.arg_type))
     in

     fun () ->
       match filename with Some fn -> eval_filename fn | None -> repl ())

let () =
  Spit.Parser.pp_exceptions ();
  Command_unix.run ~version:"1.0" ~build_info:"RWO" command

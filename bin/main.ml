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

let repl () = Evaluator.stdlib () |> Repl.run |> Lwt_main.run

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

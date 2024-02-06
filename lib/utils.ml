open! Core

let in_frame stack frame f =
  Stack.push stack frame;
  Exn.protect ~f ~finally:(fun () -> Stack.pop stack |> ignore)
;;

let quote_string_literal s =
  s |> String.substr_replace_all ~pattern:"\"" ~with_:"\\\"" |> Printf.sprintf "\"%s\""
;;

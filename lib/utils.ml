open! Core

let quote_string_literal s =
  s |> String.substr_replace_all ~pattern:"\"" ~with_:"\\\"" |> Printf.sprintf "\"%s\""
;;

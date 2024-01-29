open Core

type error =
  | UnknownIdentifier of Ast.Ident.t
  | WrongType of string * string
  | WrongExprType of string * string
  | WrongArgCount of int * int
  | CantUnquoteFunctions
  | CantCompareFunctions
[@@deriving sexp]

exception Error of error [@@deriving sexp]

let () =
  Stdlib.Printexc.register_printer (function exn ->
      (let open Printf in
       try
         Stdlib.Format.flush_str_formatter () |> ignore;
         match exn with
         | Error (UnknownIdentifier (Ast.Ident.Id id)) ->
             sprintf "Unknown identifier %s" id |> Some
         | Error CantUnquoteFunctions ->
             "Cannot unquote function values" |> Some
         | Error (WrongType (ty, expected)) ->
             sprintf "Wrong type %s, expected %s" ty expected |> Some
         | Error (WrongExprType (ty, expected)) ->
             sprintf "Wrong expression type %s, expected %s" ty expected |> Some
         | Error (WrongArgCount (args, expected)) ->
             sprintf "Wrong number of arguments %d, expected %d" args expected
             |> Some
         | Error CantCompareFunctions ->
             sprintf "Can't compare functions" |> Some
         | _ -> None
       with _ -> None))

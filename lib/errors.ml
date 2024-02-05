open Core

exception UnknownIdentifier of Ast.Ident.t [@@deriving sexp]
exception WrongType of string * string [@@deriving sexp]
exception WrongExprType of string * string [@@deriving sexp]
exception WrongArgCount of int * int [@@deriving sexp]
exception CantUnquoteFunctions
exception CantCompareFunctions
exception UnquoteSplicingOutsideList

let () =
  Stdlib.Printexc.register_printer (function exn ->
    let open Printf in
    (try
       Stdlib.Format.flush_str_formatter () |> ignore;
       match exn with
       | UnknownIdentifier (Ast.Ident.Id id) -> sprintf "Unknown identifier %s" id |> Some
       | CantUnquoteFunctions -> "Cannot unquote function values" |> Some
       | WrongType (ty, expected) ->
         sprintf "Wrong type %s, expected %s" ty expected |> Some
       | WrongExprType (ty, expected) ->
         sprintf "Wrong expression type %s, expected %s" ty expected |> Some
       | WrongArgCount (args, expected) ->
         sprintf "Wrong number of arguments %d, expected %d" args expected |> Some
       | CantCompareFunctions -> sprintf "Can't compare functions" |> Some
       | UnquoteSplicingOutsideList ->
         sprintf "unquote-splicing encountered outside quasiquote list" |> Some
       | _ -> None
     with
     | _ -> None))
;;

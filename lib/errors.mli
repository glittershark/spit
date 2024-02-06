open Core

module Frame : sig
  type t =
    | Expanding_macro of string
    | Calling_function of string
    | Evaluating_expr of string
    | Evaluating_special_form of string
  [@@deriving sexp]

  type trace = t Stack.t

  val to_string : t -> string
  val trace_to_string : t Stack.t -> string
end

exception UnknownIdentifier of Ast.Ident.t [@@deriving sexp]
exception WrongType of string * string [@@deriving sexp]
exception WrongExprType of string * string [@@deriving sexp]
exception WrongArgCount of int * int [@@deriving sexp]
exception CantUnquoteFunctions
exception CantCompareFunctions
exception UnquoteSplicingOutsideList
exception WithTrace of exn * Frame.trace

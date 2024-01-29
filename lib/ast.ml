open Base

module Ident = struct
  type t = Id of string [@@deriving compare, hash, sexp]
  let of_s s = Id s
end

type literal = LInt of int | LString of string [@@deriving sexp]

type sexp =
  | Atom of string
  | Literal of literal
  | List of sexp list
  | Cons of (sexp * sexp)
  | Quote of sexp
[@@deriving sexp]

let type_name = function
  | Atom _ -> "atom"
  | Literal _ -> "literal"
  | List _ -> "list"
  | Cons _ -> "cons"
  | Quote _ -> "quote"

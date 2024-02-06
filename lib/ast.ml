open Base
open Core

module Ident = struct
  type t = Id of string [@@deriving compare, hash, sexp]

  let of_s s = Id s
end

type literal =
  | LInt of int
  | LString of
      (string
      [@quickcheck.generator
        Base_quickcheck.Generator.string_of Base_quickcheck.Generator.char_print])
[@@deriving sexp, compare, quickcheck]

let string_of_literal = function
  | LInt i -> Int.to_string i
  | LString s -> Utils.quote_string_literal s
;;

type sexp =
  | Atom of
      (string
      [@quickcheck.generator
        Base_quickcheck.Generator.string_non_empty_of Base_quickcheck.Generator.char_alpha])
  | Literal of literal
  | List of sexp list
  | Cons of (sexp * sexp) [@quickcheck.do_not_generate]
  | Quote of sexp
  | Quasiquote of sexp
  | Unquote of sexp
  | UnquoteSplicing of sexp
[@@deriving sexp, compare, quickcheck]

let rec string_of_sexp =
  let open Printf in
  function
  | Atom s -> s
  | Literal lit -> string_of_literal lit
  | List l -> l |> List.map ~f:string_of_sexp |> String.concat ~sep:" " |> sprintf "(%s)"
  | Cons (s1, s2) -> sprintf "(%s . %s)" (string_of_sexp s1) (string_of_sexp s2)
  | Quote q -> sprintf "'%s" (string_of_sexp q)
  | Quasiquote qq -> sprintf "`%s" (string_of_sexp qq)
  | Unquote u -> sprintf ",%s" (string_of_sexp u)
  | UnquoteSplicing us -> sprintf ",@%s" (string_of_sexp us)
;;

let type_name = function
  | Atom _ -> "atom"
  | Literal _ -> "literal"
  | List _ -> "list"
  | Cons _ -> "cons"
  | Quote _ -> "quote"
  | Quasiquote _ -> "quasiquote"
  | Unquote _ -> "unquote"
  | UnquoteSplicing _ -> "unquote-splicing"
;;

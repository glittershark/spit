open Base
open Core

module Ident = struct
  type t = Id of string [@@deriving compare, hash, sexp]

  let of_s s = Id s
  let to_string (Id s) = s
end

module Literal = struct
  type t =
    | Int of int
    | String of
        (string
        [@quickcheck.generator
          Base_quickcheck.Generator.string_of Base_quickcheck.Generator.char_print])
  [@@deriving sexp, compare, quickcheck]

  let to_string = function
    | Int i -> Int.to_string i
    | String s -> Utils.quote_string_literal s
  ;;
end

module Sexp = struct
  type t =
    | Atom of
        (string
        [@quickcheck.generator
          Base_quickcheck.Generator.string_non_empty_of
            Base_quickcheck.Generator.char_alpha])
    | Literal of Literal.t
    | List of t list
    | Cons of (t * t) [@quickcheck.do_not_generate]
    | Quote of t
    | Quasiquote of t
    | Unquote of t
    | UnquoteSplicing of t
  [@@deriving sexp, compare, quickcheck]

  let rec to_string =
    let open Printf in
    function
    | Atom s -> s
    | Literal lit -> Literal.to_string lit
    | List l -> l |> List.map ~f:to_string |> String.concat ~sep:" " |> sprintf "(%s)"
    | Cons (s1, s2) -> sprintf "(%s . %s)" (to_string s1) (to_string s2)
    | Quote q -> sprintf "'%s" (to_string q)
    | Quasiquote qq -> sprintf "`%s" (to_string qq)
    | Unquote u -> sprintf ",%s" (to_string u)
    | UnquoteSplicing us -> sprintf ",@%s" (to_string us)
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
end

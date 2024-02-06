open Core

type t =
  | Nil
  | Cons of (t * t)
  | Int of int
  | String of string
  | Sym of Ast.Ident.t
  | Fun of fn
[@@deriving sexp]

and fn = t list -> t

val list : t list -> t
val of_literal : Ast.Literal.t -> t
val of_bool : bool -> t
val type_name : t -> string
val as_cons : t -> t * t
val as_int : t -> int
val as_string : t -> string
val as_symbol : t -> Ast.Ident.t
val as_function : t -> fn
val as_list : t -> t list option
val as_list_exn : t -> t list
val quote : Ast.Sexp.t -> t
val unquote : t -> Ast.Sexp.t
val to_string : t -> string
val is_truthy : t -> bool
val compare : t -> t -> int
val gen : t Quickcheck.Generator.t

module Ident : sig
  type t = Id of string [@@deriving compare, hash, sexp]

  val of_s : string -> t
  val to_string : t -> string
end

module Literal : sig
  type t =
    | Int of int
    | String of string
  [@@deriving sexp, compare, quickcheck]

  val to_string : t -> string
end

module Sexp : sig
  type t =
    | Atom of string
    | Literal of Literal.t
    | List of t list
    | Cons of (t * t)
    | Quote of t
    | Quasiquote of t
    | Unquote of t
    | UnquoteSplicing of t
  [@@deriving sexp, compare, quickcheck]

  val to_string : t -> string
  val type_name : t -> string
end

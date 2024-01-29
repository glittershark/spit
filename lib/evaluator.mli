module Ident : sig
  type t = Ident of string

  val of_s : string -> t
end

type error =
  | UnknownIdentifier of Ident.t
  | WrongType of string * string
  | WrongExprType of string * string
  | WrongArgCount of int * int
  | CantUnquoteFunctions
  | CantCompareFunctions

exception Error of error

module Value : sig
  type fn = t list -> t

  and t =
    | Nil
    | Cons of (t * t)
    | Int of int
    | String of string
    | Sym of Ident.t
    | Fun of fn
  [@@deriving sexp_of]

  val list : t list -> t
  val of_literal : Ast.literal -> t
  val type_name : t -> string
  val wrong_type : t -> string -> exn
  val as_cons : t -> t * t
  val as_int : t -> int
  val as_string : t -> string
  val as_symbol : t -> Ident.t
  val as_function : t -> fn
  val unquote : t -> Ast.sexp
end

module Env : sig
  type t

  val lookup : t -> Ident.t -> Value.t option
  val set : t -> Ident.t -> Value.t -> unit
  val set_toplevel : t -> Ident.t -> Value.t -> unit
  val set_is_macro : t -> Ident.t -> unit
  val is_macro : t -> Ident.t -> bool
  val push_frame : t -> unit
  val pop_frame : t -> unit
  val in_frame : t -> (unit -> 'a) -> 'a
  val of_vars : (Ident.t * Value.t) list -> t
  val copy : t -> t
end

val builtins : Env.t
val stdlib : unit -> Env.t
val eval : ?env:Env.t -> Ast.sexp -> Value.t

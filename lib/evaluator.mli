type error =
  | UnknownIdentifier of Ast.Ident.t
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
    | Sym of Ast.Ident.t
    | Fun of fn
  [@@deriving sexp_of]

  val list : t list -> t
  val of_literal : Ast.literal -> t
  val type_name : t -> string
  val wrong_type : t -> string -> exn
  val as_cons : t -> t * t
  val as_int : t -> int
  val as_string : t -> string
  val as_symbol : t -> Ast.Ident.t
  val as_function : t -> fn
  val unquote : t -> Ast.sexp
end

module Env : sig
  type t

  val lookup : t -> Ast.Ident.t -> Value.t option
  val set : t -> Ast.Ident.t -> Value.t -> unit
  val set_toplevel : t -> Ast.Ident.t -> Value.t -> unit
  val set_is_macro : t -> Ast.Ident.t -> unit
  val is_macro : t -> Ast.Ident.t -> bool
  val push_frame : t -> unit
  val pop_frame : t -> unit
  val in_frame : t -> (unit -> 'a) -> 'a
  val of_vars : (Ast.Ident.t * Value.t) list -> t
  val copy : t -> t
end

val builtins : Env.t
val stdlib : unit -> Env.t
val eval : ?env:Env.t -> Ast.sexp -> Value.t

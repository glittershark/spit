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
val eval : ?env:Env.t -> Ast.Sexp.t -> Value.t

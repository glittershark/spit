open Core

module Frame = struct
  type t =
    | Expanding_macro of string
    | Calling_function of string
    | Evaluating_expr of string
    | Evaluating_special_form of string
  [@@deriving sexp]

  type trace = t Stack.t

  let to_string =
    let open Printf in
    function
    | Expanding_macro s -> sprintf "       while expanding macro %s" s
    | Calling_function s -> sprintf "       while calling %s" s
    | Evaluating_expr s -> sprintf "       while evaluating %s" s
    | Evaluating_special_form s -> sprintf "       while evaluating special form %s" s
  ;;

  let trace_to_string stack =
    let open List.Monad_infix in
    stack |> Stack.to_list >>| to_string |> String.concat ~sep:"\n"
  ;;
end

let trace : Frame.t Stack.t = Stack.create ()
let in_frame f = Utils.in_frame trace f
let current_trace () = Stack.copy trace

exception UnknownIdentifier of Ast.Ident.t [@@deriving sexp]
exception WrongType of string * string [@@deriving sexp]
exception WrongExprType of string * string [@@deriving sexp]
exception WrongArgCount of int * int [@@deriving sexp]
exception CantUnquoteFunctions
exception CantCompareFunctions
exception UnquoteSplicingOutsideList
exception WithTrace of exn * Frame.trace

let with_current_trace ex = WithTrace (ex, current_trace ())
let throw ex = raise (with_current_trace ex)

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
       | WithTrace (ex, trace) ->
         sprintf
           "error: %s\n%s"
           (Stdlib.Printexc.to_string ex)
           (Frame.trace_to_string trace)
         |> Some
       | _ -> None
     with
     | _ -> None))
;;

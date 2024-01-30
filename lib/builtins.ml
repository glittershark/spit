open Core
open Errors

module Helpers = struct
  let check_args_count n args =
    if not (List.length args = n) then raise (Error (WrongArgCount (List.length args, n)))
  ;;

  let singleton f args =
    check_args_count 1 args;
    List.hd_exn args |> f
  ;;
end

open Helpers

(* Builtin functions *)

let cons args =
  check_args_count 2 args;
  Value.Cons (List.nth_exn args 0, List.nth_exn args 1)
;;

let car =
  singleton
  @@ fun arg ->
  let v, _ = Value.as_cons arg in
  v
;;

let cdr =
  singleton
  @@ fun arg ->
  let _, v = Value.as_cons arg in
  v
;;

let plus args = Value.Int (List.sum (module Int) ~f:Value.as_int args)

let minus = function
  | [] -> raise (Error (WrongArgCount (0, 1)))
  | x :: xs ->
    Value.Int
      (List.fold_right xs ~f:(fun v acc -> acc - Value.as_int v) ~init:(Value.as_int x))
;;

let cmp args =
  check_args_count 2 args;
  Value.Int (Value.compare (List.nth_exn args 0) (List.nth_exn args 1))
;;

let zerop =
  singleton
  @@ fun arg -> arg |> Value.compare (Value.Int 0) |> (fun x -> x = 0) |> Value.of_bool
;;

let negp =
  singleton
  @@ fun arg -> arg |> Value.compare (Value.Int 0) |> Int.is_positive |> Value.of_bool
;;

let posp =
  singleton
  @@ fun arg -> arg |> Value.compare (Value.Int 0) |> Int.is_negative |> Value.of_bool
;;

let type_ = singleton @@ fun arg -> Value.type_name arg |> Ast.Ident.of_s |> Value.Sym
let to_string_ = singleton @@ fun arg -> arg |> Value.to_string |> Value.String

let print_string_ =
  singleton
  @@ fun arg ->
  Value.as_string arg |> print_string;
  Value.Nil
;;

let vars =
  let open Value in
  [ "nil", Nil
  ; "cons", Fun cons
  ; "car", Fun car
  ; "cdr", Fun cdr
  ; "+", Fun plus
  ; "-", Fun minus
  ; "compare", Fun cmp
  ; "zero?", Fun zerop
  ; "neg?", Fun negp
  ; "pos?", Fun posp
  ; "type", Fun type_
  ; "to-string", Fun to_string_
  ; "print-string", Fun print_string_
  ]
  |> List.map ~f:(fun (id, v) -> Ast.Ident.of_s id, v)
;;

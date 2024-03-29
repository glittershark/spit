open Base
open Core
open Ast
open Errors

module Env = struct
  type vars = (Ident.t, Value.t) Hashtbl.Poly.t [@@deriving sexp]

  type var_frame =
    { vars : vars
    ; macros : Ident.t Hash_set.Poly.t
    }
  [@@deriving sexp]

  let var_frame_of_vars vars = { vars; macros = Hash_set.create (module Ident) }

  type t =
    { vars : var_frame ref Stack.t
    ; toplevel : var_frame ref
    }
  [@@deriving sexp]

  let lookup env v = Stack.find_map env.vars ~f:(fun t -> Hashtbl.find !t.vars v)

  let vars env =
    let open List.Monad_infix in
    env.vars
    |> Stack.to_list
    >>= (fun frame -> Hashtbl.keys !frame.vars)
    |> Hash_set.of_list (module Ident)
    |> Hash_set.to_list
  ;;

  let set env id v = Hashtbl.set !(Stack.top_exn env.vars).vars ~key:id ~data:v
  let set_toplevel env id v = Hashtbl.set !(env.toplevel).vars ~key:id ~data:v

  let set_is_macro env id =
    match Stack.find env.vars ~f:(fun t -> Hashtbl.mem !t.vars id) with
    | Some fr -> Hash_set.add !fr.macros id
    | None -> ()
  ;;

  let is_macro env id =
    Stack.find_map env.vars ~f:(fun t ->
      Hashtbl.find !t.vars id |> Option.map ~f:(fun _ -> Hash_set.mem !t.macros id))
    |> Option.value ~default:false
  ;;

  let push_frame env =
    Stack.push env.vars (ref (var_frame_of_vars (Hashtbl.create (module Ident))))
  ;;

  let pop_frame env = Stack.pop_exn env.vars |> ignore

  let in_frame env f =
    push_frame env;
    Exn.protect ~f ~finally:(fun () -> pop_frame env)
  ;;

  let of_vars vars_alist =
    let toplevel =
      ref (var_frame_of_vars (Hashtbl.of_alist_exn (module Ident) vars_alist))
    in
    { vars = Stack.of_list [ toplevel ]; toplevel }
  ;;

  let copy env = { env with vars = Stack.copy env.vars }

  let%test_module _ =
    (module struct
      let%expect_test "toplevel macro" =
        let x = Ident.Id "x" in
        let env = of_vars [] in
        push_frame env;
        set_toplevel env x Value.Nil;
        set env x Value.Nil;
        is_macro env x |> sexp_of_bool |> print_s;
        [%expect {| false |}];
        set_is_macro env x;
        is_macro env x |> sexp_of_bool |> print_s;
        [%expect {| true |}];
        pop_frame env;
        is_macro env x |> sexp_of_bool |> print_s;
        [%expect {| false |}]
      ;;
    end)
  ;;
end

let builtins = Env.of_vars Builtins.vars

let rec eval ?(env = stdlib ()) expr =
  in_frame (Frame.Evaluating_expr (Ast.Sexp.to_string expr)) (fun () -> eval' ~env expr)

and eval' ?(env = stdlib ()) =
  let open Sexp in
  function
  | Atom id ->
    let ident = Ident.Id id in
    (match Env.lookup env ident with
     | Some v -> v
     | None -> throw (UnknownIdentifier ident))
  | Literal lit -> Value.of_literal lit
  | List lst ->
    let rec special_form (Ident.Id form) args =
      let singleton f =
        args
        |> List.hd
        |> Option.value_or_thunk ~default:(fun () ->
          throw (WrongArgCount (List.length args, 1)))
        |> f
        |> Some
      in
      match form with
      | "vars" -> Some (eval_vars ~env)
      | "def" -> Some (eval_def ~env args)
      | "lambda" -> Some (eval_lambda ~env args)
      | "make-macro" -> Some (eval_make_macro ~env args)
      | "is-macro" -> Some (eval_is_macro ~env args)
      | "quote" -> singleton Value.quote
      | "quasiquote" -> singleton @@ quasiquote ~env
      | "macroexpand-1" -> Some (eval_macroexpand_1 ~env args)
      | "if" -> Some (eval_if ~env args)
      | _ -> None
    and maybe_eval_special_form = function
      | Atom id :: t when String.is_prefix id ~prefix:"." ->
        in_frame
          (Frame.Evaluating_special_form (Ast.Sexp.to_string (List lst)))
          (fun () -> special_form (Ident.Id (String.drop_prefix id 1)) t)
      | _ -> None
    in
    let eval_macro id args =
      let macro = Env.lookup env id |> Option.value_exn |> Value.as_function in
      in_frame
        (Frame.Expanding_macro (Ast.Sexp.to_string (List lst)))
        (fun () -> List.map ~f:Value.quote args |> macro |> Value.unquote |> eval ~env)
    in
    let maybe_eval_macro = function
      | Atom id :: args when Env.is_macro env (Ident.Id id) ->
        Some (eval_macro (Ident.Id id) args)
      | _ -> None
    in
    maybe_eval_special_form lst
    |> Option.map ~f:Option.return
    |> Option.value_or_thunk ~default:(fun () -> maybe_eval_macro lst)
    |> Option.value_or_thunk ~default:(fun () ->
      let vals = List.map ~f:(eval ~env) lst in
      match List.nth vals 0 with
      | None -> Value.Nil
      | Some f ->
        in_frame
          (Frame.Calling_function (Ast.Sexp.to_string (List lst)))
          (fun () -> (Value.as_function f) (List.drop vals 1)))
  | Cons (e1, e2) ->
    let v1 = eval ~env e1 in
    let v2 = eval ~env e2 in
    Value.Cons (v1, v2)
  | Quote v -> Value.quote v
  | Quasiquote v -> quasiquote ~env v
  | Unquote _ | UnquoteSplicing _ -> failwith "todo"

and stdlib () =
  let env = builtins in
  Stdlib_lisp.src |> Parser.parse_string |> List.map ~f:(eval ~env) |> ignore;
  env

and quasiquote ~env = function
  | Atom s -> Value.Sym (Ident.Id s)
  | Literal (Literal.Int i) -> Int i
  | Literal (Literal.String s) -> String s
  | List [ Atom ".unquote"; v ] | Unquote v -> eval ~env v
  | List l ->
    Value.list
      List.(
        l
        >>= function
        | UnquoteSplicing v | List [ Atom ".unquote-splicing"; v ] ->
          eval ~env v |> Value.as_list_exn
        | v -> [ quasiquote ~env v ])
  | Cons (x, y) -> Cons (quasiquote ~env x, quasiquote ~env y)
  | Quote v -> Value.list [ Value.Sym (Ident.Id ".quote"); quasiquote ~env v ]
  | Quasiquote v -> Value.list [ Sym (Ident.Id ".quasiquote"); quasiquote ~env v ]
  | UnquoteSplicing _ -> throw UnquoteSplicingOutsideList

and eval_vars ~env = Env.vars env |> List.map ~f:(fun id -> Value.Sym id) |> Value.list

and eval_def ~env = function
  | [ Sexp.Atom vname; expr ] ->
    let value = eval ~env expr in
    Env.set_toplevel env (Ident.Id vname) value;
    Value.Nil
  | [ arg1; _ ] -> throw (WrongType (Sexp.type_name arg1, "atom"))
  | args -> throw (WrongArgCount (List.length args, 2))

and eval_lambda ~env = function
  | [ ((Sexp.Atom _ | Sexp.List _) as arg); body ] ->
    let closure = Env.copy env in
    let rec bind_env v = function
      | Sexp.Atom argname -> Env.set closure (Ident.Id argname) v
      | Sexp.List argnames ->
        (match Value.as_list v with
         | Some l ->
           (match List.map2 argnames l ~f:(fun argname v -> bind_env v argname) with
            | List.Or_unequal_lengths.Ok _ -> ()
            | List.Or_unequal_lengths.Unequal_lengths ->
              throw (WrongArgCount (List.length l, List.length argnames)))
         | None -> throw (WrongType (Value.type_name v, "proper list")))
      | _ -> failwith "unreachable"
    in
    Value.Fun
      (fun args ->
        Env.in_frame closure (fun () ->
          bind_env (Value.list args) arg;
          eval ~env:closure body))
  | [ arg; _ ] -> throw (WrongType (Sexp.type_name arg, "list or atom"))
  | args -> throw (WrongArgCount (List.length args, 2))

and eval_make_macro ~env = function
  | Sexp.Atom vname :: [] ->
    Env.set_is_macro env (Ident.Id vname);
    Value.Nil
  | arg1 :: [] -> throw (WrongType (Sexp.type_name arg1, "atom"))
  | args -> throw (WrongArgCount (List.length args, 1))

and eval_is_macro ~env = function
  | Sexp.Atom vname :: [] -> vname |> Ident.of_s |> Env.is_macro env |> Value.of_bool
  | arg1 :: [] -> throw (WrongType (Sexp.type_name arg1, "atom"))
  | args -> throw (WrongArgCount (List.length args, 1))

and eval_macroexpand_1 ~env = function
  | Sexp.List (Sexp.Atom name :: args) :: [] when Env.is_macro env (Ident.of_s name) ->
    let macro =
      Env.lookup env (Ident.of_s name) |> Option.value_exn |> Value.as_function
    in
    List.map ~f:Value.quote args |> macro
  | [ ast ] -> Value.quote ast
  | args -> throw (WrongArgCount (List.length args, 1))

and eval_if ~env = function
  | [ cond_e; then_e; else_e ] ->
    let cond = eval ~env cond_e in
    if Value.is_truthy cond then eval ~env then_e else eval ~env else_e
  | args -> throw (WrongArgCount (List.length args, 3))
;;

let%test_module _ =
  (module struct
    let eval_expr s = Parser.parse_string s |> List.map ~f:eval |> List.last_exn
    let eval_print s = eval_expr s |> Value.to_string |> print_endline

    let%expect_test "eval_int" =
      eval_print "1";
      [%expect {| 1 |}]
    ;;

    let%expect_test "eval_plus_two_args" =
      eval_print "(+ 1 1)";
      [%expect {| 2 |}]
    ;;

    let%expect_test "eval_plus_many_args" =
      eval_print "(+ 1 2 3)";
      [%expect {| 6 |}]
    ;;

    let%expect_test "eval_plus_nested" =
      eval_print "(+ 1 2 (+ 3 4))";
      [%expect {| 10 |}]
    ;;

    let%expect_test "list" =
      eval_print "(cons 1 (+ 2 2))";
      [%expect {| (cons 1 4) |}]
    ;;

    let%expect_test "car and cdr" =
      eval_print "(cons (car (cons 1 2)) (cdr (cons 1 2)))";
      [%expect {| (cons 1 2) |}]
    ;;

    let%expect_test "quote" =
      eval_print "'a";
      [%expect {| a |}]
    ;;

    let%expect_test "quasiquote" =
      eval_print "`(1 ,(+ 1 1) ,@(list 3 4))";
      [%expect {| (1 2 3 4) |}]
    ;;

    let%expect_test ".def" =
      eval_print {|
        (.def x 1)
        x
      |};
      [%expect {| 1 |}]
    ;;

    let%expect_test ".lambda" =
      eval_print "((.lambda x (car x)) 1)";
      [%expect {| 1 |}]
    ;;

    let%expect_test "lambda with list args" =
      eval_print "((.lambda (x) x) 1)";
      [%expect {| 1 |}]
    ;;

    let%expect_test "lambda with multiple args" =
      eval_print "((.lambda (x y) (+ x y)) 1 2)";
      [%expect {| 3 |}]
    ;;

    let%expect_test "simplest possible macros" =
      eval_print
        {|
        (.def make-id
         (.lambda args
          (cons '.lambda
                (cons '__args__
                      (cons (cons 'car (cons '__args__ nil))
                            nil)))))
        (.make-macro make-id)
        ((make-id) 1)
      |};
      [%expect {| 1 |}]
    ;;

    let%expect_test "simple if " =
      eval_print "(.if 1 'a 'b)";
      [%expect {| a |}];
      eval_print "(.if nil 'a 'b)";
      [%expect {| b |}]
    ;;
  end)
;;

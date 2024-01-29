open Base
open Core

module Ident = struct
  type t = Ident of string [@@deriving compare, hash, sexp]

  let of_s s = Ident s
end

type error =
  | UnknownIdentifier of Ident.t
  | WrongType of string * string
  | WrongExprType of string * string
  | WrongArgCount of int * int
  | CantUnquoteFunctions
  | CantCompareFunctions
[@@deriving sexp]

exception Error of error [@@deriving sexp]

let () =
  Stdlib.Printexc.register_printer (function exn ->
      (let open Printf in
       try
         Stdlib.Format.flush_str_formatter () |> ignore;
         match exn with
         | Error (UnknownIdentifier (Ident.Ident id)) ->
             sprintf "Unknown identifier %s" id |> Some
         | Error CantUnquoteFunctions ->
             "Cannot unquote function values" |> Some
         | Error (WrongType (ty, expected)) ->
             sprintf "Wrong type %s, expected %s" ty expected |> Some
         | Error (WrongExprType (ty, expected)) ->
             sprintf "Wrong expression type %s, expected %s" ty expected |> Some
         | Error (WrongArgCount (args, expected)) ->
             sprintf "Wrong number of arguments %d, expected %d" args expected
             |> Some
         | Error CantCompareFunctions ->
             sprintf "Can't compare functions" |> Some
         | _ -> None
       with _ -> None))

module Value = struct
  type t =
    | Nil
    | Cons of (t * t)
    | Int of int
    | String of string
    | Sym of Ident.t
    | Fun of fn
  [@@deriving sexp]

  and fn = t list -> t

  let list = List.fold_right ~f:(fun x y -> Cons (x, y)) ~init:Nil
  let of_literal = function Ast.LInt i -> Int i | Ast.LString s -> String s
  let of_bool = function true -> Sym (Ident.of_s "t") | false -> Nil

  let type_name = function
    | Nil | Cons _ -> "list"
    | Int _ -> "int"
    | String _ -> "string"
    | Sym _ -> "symbol"
    | Fun _ -> "function"

  let wrong_type v expected = Error (WrongType (type_name v, expected))

  let as_cons = function
    | Cons (x, y) -> (x, y)
    | v -> raise (wrong_type v "cons")

  let as_int = function Int i -> i | v -> raise (wrong_type v "int")
  let as_string = function String s -> s | v -> raise (wrong_type v "string")
  let as_symbol = function Sym s -> s | v -> raise (wrong_type v "symbol")
  let as_function = function Fun f -> f | v -> raise (wrong_type v "function")

  let rec as_list = function
    | Cons (x, y) -> as_list y |> Option.map ~f:(fun l -> x :: l)
    | Nil -> Some []
    | _ -> None

  let rec unquote =
    let open Ast in
    function
    | Nil -> List []
    | Cons (hd, tl) -> (
        match unquote tl with
        | Ast.List t -> Ast.List (unquote hd :: t)
        | expr -> Ast.Cons (unquote hd, expr))
    | Int i -> Ast.Literal (LInt i)
    | String s -> Ast.Literal (LString s)
    | Sym (Ident.Ident n) -> Ast.Atom n
    | Fun _ -> raise (Error CantUnquoteFunctions)

  let is_truthy = function Nil -> false | _ -> true

  let rec compare x y =
    match (x, y) with
    | Nil, Nil -> 0
    | Nil, _ -> -1
    | Cons (x1, y1), Cons (x2, y2) -> (
        match compare x1 x2 with 0 -> compare y1 y2 | c -> c)
    | Cons _, Nil -> 1
    | Cons _, _ -> -1
    | String s1, String s2 -> String.compare s1 s2
    | String _, (Nil | Cons _) -> 1
    | String _, _ -> -1
    | Int i1, Int i2 -> Int.compare i1 i2
    | Int _, (Nil | String _ | Cons _) -> 1
    | Int _, _ -> -1
    | Sym i1, Sym i2 -> Ident.compare i1 i2
    | Sym _, (Nil | String _ | Cons _ | Int _) -> 1
    | Sym _, _ -> -1
    | Fun _, _ -> raise (Error CantCompareFunctions)

  let (gen : t Quickcheck.Generator.t) =
    let open Quickcheck.Generator in
    union
      [
        singleton Nil;
        (Int.gen_incl Int.min_value Int.max_value >>| fun i -> Int i);
        (String.gen' Char.gen_print >>| fun s -> String s);
        (String.gen' Char.gen_alpha >>| Ident.of_s >>| fun id -> Sym id);
      ]

  let%test_module "compare properties" =
    (module struct
      open Quickcheck.Generator

      let%test_unit "equal reflexive" =
        Quickcheck.test ~sexp_of:[%sexp_of: t] gen ~f:(fun v ->
            [%test_eq: int] (compare v v) 0)

      let%test_unit "equal transitive" =
        Quickcheck.test ~sexp_of:[%sexp_of: t * t * t] (tuple3 gen gen gen)
          ~f:(fun (v1, v2, v3) ->
            if compare v1 v2 = 0 && compare v2 v3 = 0 then
              [%test_eq: int] (compare v1 v3) 0)

      let%test_unit "symmetry" =
        Quickcheck.test ~sexp_of:[%sexp_of: t * t] (tuple2 gen gen)
          ~f:(fun (v1, v2) ->
            [%test_eq: int] (compare v1 v2) (neg (compare v2 v1)))

      let%test_unit "lt transitive" =
        Quickcheck.test ~sexp_of:[%sexp_of: t * t * t] (tuple3 gen gen gen)
          ~f:(fun (v1, v2, v3) ->
            if compare v1 v2 < 0 && compare v2 v3 < 0 then
              [%test_pred: int] (fun r -> r < 0) (compare v1 v3))

      let%test_unit "gt transitive" =
        Quickcheck.test ~sexp_of:[%sexp_of: t * t * t] (tuple3 gen gen gen)
          ~f:(fun (v1, v2, v3) ->
            if compare v1 v2 > 0 && compare v2 v3 > 0 then
              [%test_pred: int] (fun r -> r > 0) (compare v1 v3))
    end)
end

module Env = struct
  type vars = (Ident.t, Value.t) Hashtbl.Poly.t [@@deriving sexp]

  type frame = { vars : vars; macros : Ident.t Hash_set.Poly.t }
  [@@deriving sexp]

  let frame_of_vars vars = { vars; macros = Hash_set.create (module Ident) }

  type t = { vars : frame ref Stack.t; toplevel : frame ref } [@@deriving sexp]

  let lookup env v =
    Stack.find_map env.vars ~f:(fun t -> Hashtbl.find !t.vars v)

  let vars env =
    let open List.Monad_infix in
    env.vars |> Stack.to_list
    >>= (fun frame -> Hashtbl.keys !frame.vars)
    |> Hash_set.of_list (module Ident)
    |> Hash_set.to_list

  let set env id v = Hashtbl.set !(Stack.top_exn env.vars).vars ~key:id ~data:v
  let set_toplevel env id v = Hashtbl.set !(env.toplevel).vars ~key:id ~data:v

  let set_is_macro env id =
    match Stack.find env.vars ~f:(fun t -> Hashtbl.mem !t.vars id) with
    | Some fr -> Hash_set.add !fr.macros id
    | None -> ()

  let is_macro env id =
    Stack.find_map env.vars ~f:(fun t ->
        Hashtbl.find !t.vars id
        |> Option.map ~f:(fun _ -> Hash_set.mem !t.macros id))
    |> Option.value ~default:false

  let push_frame env =
    Stack.push env.vars (ref (frame_of_vars (Hashtbl.create (module Ident))))

  let pop_frame env = Stack.pop_exn env.vars |> ignore

  let in_frame env f =
    push_frame env;
    Exn.protect ~f ~finally:(fun () -> pop_frame env)

  let of_vars vars_alist =
    let toplevel =
      ref (frame_of_vars (Hashtbl.of_alist_exn (module Ident) vars_alist))
    in
    { vars = Stack.of_list [ toplevel ]; toplevel }

  let copy env = { vars = Stack.copy env.vars; toplevel = env.toplevel }

  let%test_module _ =
    (module struct
      let%expect_test "toplevel macro" =
        let x = Ident.Ident "x" in
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
    end)
end

module Builtins = struct
  let check_args_count n args =
    if not (List.length args = n) then
      raise (Error (WrongArgCount (List.length args, n)))

  let cons args =
    check_args_count 2 args;
    Value.Cons (List.nth_exn args 0, List.nth_exn args 1)

  let car args =
    check_args_count 1 args;
    let v, _ = List.nth_exn args 0 |> Value.as_cons in
    v

  let cdr args =
    check_args_count 1 args;
    let _, v = List.nth_exn args 0 |> Value.as_cons in
    v

  let plus args = Value.Int (List.sum (module Int) ~f:Value.as_int args)

  let minus = function
    | [] -> raise (Error (WrongArgCount (0, 1)))
    | x :: xs ->
        Value.Int
          (List.fold_right xs
             ~f:(fun v acc -> acc - Value.as_int v)
             ~init:(Value.as_int x))

  let cmp args =
    check_args_count 2 args;
    Value.Int (Value.compare (List.nth_exn args 0) (List.nth_exn args 1))

  let zerop args =
    check_args_count 1 args;
    List.nth_exn args 0
    |> Value.compare (Value.Int 0)
    |> (fun x -> x = 0)
    |> Value.of_bool

  let negp args =
    check_args_count 1 args;
    List.nth_exn args 0
    |> Value.compare (Value.Int 0)
    |> Int.is_positive |> Value.of_bool

  let posp args =
    check_args_count 1 args;
    List.nth_exn args 0
    |> Value.compare (Value.Int 0)
    |> Int.is_negative |> Value.of_bool

  let env =
    let id n = Ident.Ident n in
    let open Value in
    Env.of_vars
      [
        (id "nil", Nil);
        (id "cons", Fun cons);
        (id "car", Fun car);
        (id "cdr", Fun cdr);
        (id "+", Fun plus);
        (id "-", Fun minus);
        (id "compare", Fun cmp);
        (id "zero?", Fun zerop);
        (id "neg?", Fun negp);
        (id "pos?", Fun posp);
      ]
end

let builtins = Builtins.env

let rec eval ?(env = stdlib ()) = function
  | Ast.Atom id -> (
      let ident = Ident.Ident id in
      match Env.lookup env ident with
      | Some v -> v
      | None -> raise (Error (UnknownIdentifier ident)))
  | Ast.Literal lit -> Value.of_literal lit
  | Ast.List lst ->
      let rec special_form (Ident.Ident form) args =
        match form with
        | "vars" -> Some (eval_vars ~env)
        | "let" -> Some (eval_let ~env args)
        | "def" -> Some (eval_def ~env args)
        | "lambda" -> Some (eval_lambda ~env args)
        | "make-macro" -> Some (eval_make_macro ~env args)
        | "is-macro" -> Some (eval_is_macro ~env args)
        | "quote" ->
            args |> List.hd
            |> Option.value_or_thunk ~default:(fun () ->
                   raise (Error (WrongArgCount (List.length args, 1))))
            |> eval_quote |> Some
        | "macroexpand-1" -> Some (eval_macroexpand_1 ~env args)
        | "if" -> Some (eval_if ~env args)
        | _ -> None
      and maybe_eval_special_form = function
        | Ast.Atom id :: t when String.is_prefix id ~prefix:"." ->
            special_form (Ident.Ident (String.drop_prefix id 1)) t
        | _ -> None
      in

      let eval_macro id args =
        let macro =
          Env.lookup env id |> Option.value_exn |> Value.as_function
        in
        List.map ~f:eval_quote args |> macro |> Value.unquote |> eval ~env
      in
      let maybe_eval_macro = function
        | Ast.Atom id :: args when Env.is_macro env (Ident.Ident id) ->
            Some (eval_macro (Ident.Ident id) args)
        | _ -> None
      in

      maybe_eval_special_form lst
      |> Option.map ~f:Option.return
      |> Option.value_or_thunk ~default:(fun () -> maybe_eval_macro lst)
      |> Option.value_or_thunk ~default:(fun () ->
             let vals = List.map ~f:(eval ~env) lst in
             match List.nth vals 0 with
             | None -> Value.Nil
             | Some f -> (Value.as_function f) (List.drop vals 1))
  | Ast.Cons (e1, e2) ->
      let v1 = eval ~env e1 in
      let v2 = eval ~env e2 in
      Value.Cons (v1, v2)
  | Ast.Quote v -> eval_quote v

and stdlib () =
  let env = builtins in
  Stdlib_lisp.src |> Parser.parse_string |> List.map ~f:(eval ~env) |> ignore;
  env

and eval_vars ~env =
  Env.vars env |> List.map ~f:(fun id -> Value.Sym id) |> Value.list

and eval_let ~env = function
  | [] -> Value.Nil
  | [ Ast.List vars; body ] ->
      Env.in_frame env (fun () ->
          List.map vars ~f:(function
            | Ast.List [ vname; vexpr ] ->
                let vname =
                  match vname with
                  | Ast.Atom v -> v
                  | _ -> raise (Error (WrongType (Ast.type_name vname, "atom")))
                in
                let v = eval ~env vexpr in
                Env.set env (Ident.Ident vname) v
            | v ->
                raise
                  (Error (WrongType (Ast.type_name v, "list with two elements"))))
          |> ignore;
          eval ~env body)
  | hd :: _ -> raise (Error (WrongType (Ast.type_name hd, "list")))

and eval_def ~env = function
  | [ Ast.Atom vname; expr ] ->
      let value = eval ~env expr in
      Env.set_toplevel env (Ident.Ident vname) value;
      Value.Nil
  | [ arg1; _ ] -> raise (Error (WrongType (Ast.type_name arg1, "atom")))
  | args -> raise (Error (WrongArgCount (List.length args, 2)))

and eval_lambda ~env = function
  | [ ((Ast.Atom _ | Ast.List _) as arg); body ] ->
      let closure = Env.copy env in
      let rec bind_env v = function
        | Ast.Atom argname -> Env.set closure (Ident.Ident argname) v
        | Ast.List argnames -> (
            match Value.as_list v with
            | Some l -> (
                match
                  List.map2 argnames l ~f:(fun argname v -> bind_env v argname)
                with
                | List.Or_unequal_lengths.Ok _ -> ()
                | List.Or_unequal_lengths.Unequal_lengths ->
                    raise
                      (Error
                         (WrongArgCount (List.length l, List.length argnames))))
            | None ->
                raise (Error (WrongType (Value.type_name v, "proper list"))))
        | _ -> failwith "unreachable"
      in
      Value.Fun
        (fun args ->
          Env.in_frame closure (fun () ->
              bind_env (Value.list args) arg;
              eval ~env:closure body))
  | [ arg; _ ] -> raise (Error (WrongType (Ast.type_name arg, "list or atom")))
  | args -> raise (Error (WrongArgCount (List.length args, 2)))

and eval_make_macro ~env = function
  | Ast.Atom vname :: [] ->
      Env.set_is_macro env (Ident.Ident vname);
      Value.Nil
  | arg1 :: [] -> raise (Error (WrongType (Ast.type_name arg1, "atom")))
  | args -> raise (Error (WrongArgCount (List.length args, 1)))

and eval_is_macro ~env = function
  | Ast.Atom vname :: [] ->
      vname |> Ident.of_s |> Env.is_macro env |> Value.of_bool
  | arg1 :: [] -> raise (Error (WrongType (Ast.type_name arg1, "atom")))
  | args -> raise (Error (WrongArgCount (List.length args, 1)))

and eval_quote = function
  | Ast.Atom s -> Value.Sym (Ident.Ident s)
  | Ast.Literal (Ast.LInt i) -> Value.Int i
  | Ast.Literal (Ast.LString s) -> Value.String s
  | Ast.List l -> Value.list (List.map ~f:eval_quote l)
  | Ast.Cons (x, y) -> Value.Cons (eval_quote x, eval_quote y)
  | Ast.Quote v ->
      Value.list [ Value.Sym (Ident.Ident ".quote"); eval_quote v; Value.Nil ]

and eval_macroexpand_1 ~env = function
  | Ast.List (Ast.Atom name :: args) :: []
    when Env.is_macro env (Ident.of_s name) ->
      let macro =
        Env.lookup env (Ident.of_s name)
        |> Option.value_exn |> Value.as_function
      in
      List.map ~f:eval_quote args |> macro
  | [ ast ] -> eval_quote ast
  | args -> raise (Error (WrongArgCount (List.length args, 1)))

and eval_if ~env = function
  | [ cond_e; then_e; else_e ] ->
      let cond = eval ~env cond_e in
      if Value.is_truthy cond then eval ~env then_e else eval ~env else_e
  | args -> raise (Error (WrongArgCount (List.length args, 3)))

let%test_module _ =
  (module struct
    let () = Printexc.record_backtrace false
    let eval_expr s = Parser.parse_string s |> List.map ~f:eval |> List.last_exn
    let eval_print s = eval_expr s |> Printf.printf !"%{sexp:Value.t}"

    let%expect_test "eval_int" =
      eval_print "1";
      [%expect {| (Int 1) |}]

    let%expect_test "eval_plus_two_args" =
      eval_print "(+ 1 1)";
      [%expect {| (Int 2) |}]

    let%expect_test "eval_plus_many_args" =
      eval_print "(+ 1 2 3)";
      [%expect {| (Int 6) |}]

    let%expect_test "eval_plus_nested" =
      eval_print "(+ 1 2 (+ 3 4))";
      [%expect {| (Int 10) |}]

    let%expect_test "list" =
      eval_print "(cons 1 (+ 2 2))";
      [%expect {| (Cons ((Int 1) (Int 4))) |}]

    let%expect_test "car and cdr" =
      eval_print "(cons (car (cons 1 2)) (cdr (cons 1 2)))";
      [%expect {| (Cons ((Int 1) (Int 2))) |}]

    let%expect_test "quote" =
      eval_print "'a";
      [%expect {| (Sym (Ident a)) |}]

    let%expect_test ".let" =
      eval_print "(.let ((x 1)) x)";
      [%expect {| (Int 1) |}]

    let%expect_test ".let with multiple vars" =
      eval_print "(.let ((x 1) (y 2)) (+ x y))";
      [%expect {| (Int 3) |}]

    let%expect_test ".let referencing earlier vars" =
      eval_print "(.let ((x 1) (y (+ x 1))) (+ x y))";
      [%expect {| (Int 3) |}]

    let%expect_test ".let shadowing" =
      eval_print
        {|
          (.let ((x 1)
                 (y (+ x 1)))
           (+ (.let ((x 4)) x)
              x))
        |};
      [%expect {| (Int 5) |}]

    let%expect_test ".def" =
      eval_print {|
        (.def x 1)
        x
      |};
      [%expect {| (Int 1) |}]

    let%expect_test ".lambda" =
      eval_print "((.lambda x (car x)) 1)";
      [%expect {| (Int 1) |}]

    let%expect_test "lambda with list args" =
      eval_print "((.lambda (x) x) 1)";
      [%expect {| (Int 1) |}]

    let%expect_test "lambda with multiple args" =
      eval_print "((.lambda (x y) (+ x y)) 1 2)";
      [%expect {| (Int 3) |}]

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
      [%expect {| (Int 1) |}]

    let%expect_test "simple if " =
      eval_print "(.if 1 'a 'b)";
      [%expect {| (Sym (Ident a)) |}];
      eval_print "(.if nil 'a 'b)";
      [%expect {| (Sym (Ident b)) |}]
  end)

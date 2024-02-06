open Core
open Errors

type t =
  | Nil
  | Cons of (t * t)
  | Int of int
  | String of string
  | Sym of Ast.Ident.t
  | Fun of fn
[@@deriving sexp]

and fn = t list -> t

let list = List.fold_right ~f:(fun x y -> Cons (x, y)) ~init:Nil

let of_literal = function
  | Ast.Literal.Int i -> Int i
  | Ast.Literal.String s -> String s
;;

let of_bool = function
  | true -> Sym (Ast.Ident.of_s "t")
  | false -> Nil
;;

let type_name = function
  | Nil | Cons _ -> "list"
  | Int _ -> "int"
  | String _ -> "string"
  | Sym _ -> "symbol"
  | Fun _ -> "function"
;;

let wrong_type v expected = WrongType (type_name v, expected)

let as_cons = function
  | Cons (x, y) -> x, y
  | v -> raise (wrong_type v "cons")
;;

let as_int = function
  | Int i -> i
  | v -> raise (wrong_type v "int")
;;

let as_string = function
  | String s -> s
  | v -> raise (wrong_type v "string")
;;

let as_symbol = function
  | Sym s -> s
  | v -> raise (wrong_type v "symbol")
;;

let as_function = function
  | Fun f -> f
  | v -> raise (wrong_type v "function")
;;

let rec as_list = function
  | Cons (x, y) -> as_list y |> Option.map ~f:(fun l -> x :: l)
  | Nil -> Some []
  | _ -> None
;;

let as_list_exn v =
  match as_list v with
  | Some r -> r
  | None -> raise (wrong_type v "proper list")
;;

let rec quote =
  let open Ast in
  let open Sexp in
  function
  | Atom s -> Sym (Ident.Id s)
  | Literal (Literal.Int i) -> Int i
  | Literal (Literal.String s) -> String s
  | List l -> list (List.map ~f:quote l)
  | Cons (x, y) -> Cons (quote x, quote y)
  | Quote v -> list [ Sym (Ident.Id ".quote"); quote v ]
  | Quasiquote v -> list [ Sym (Ident.Id ".quasiquote"); quote v ]
  | Unquote v -> list [ Sym (Ident.Id ".unquote"); quote v ]
  | UnquoteSplicing v -> list [ Sym (Ident.Id ".unquote-splicing"); quote v ]
;;

let rec unquote =
  let open Ast in
  let open Sexp in
  function
  | Nil -> List []
  | Cons (hd, tl) ->
    (match unquote tl with
     | List t -> List (unquote hd :: t)
     | expr -> Cons (unquote hd, expr))
  | Int i -> Literal (Literal.Int i)
  | String s -> Literal (Literal.String s)
  | Sym (Ident.Id n) -> Atom n
  | Fun _ -> raise CantUnquoteFunctions
;;

let rec to_string = function
  | Nil -> "nil"
  | Int i -> string_of_int i
  | Cons (hd, tl) as v ->
    (match as_list v with
     | Some l ->
       l |> List.map ~f:to_string |> String.concat ~sep:" " |> Printf.sprintf "(%s)"
     | None -> Printf.sprintf "(cons %s %s)" (to_string hd) (to_string tl))
  | String s -> Utils.quote_string_literal s
  | Sym (Id s) -> s
  | Fun _ -> "<function>"
;;

let is_truthy = function
  | Nil -> false
  | _ -> true
;;

let rec compare x y =
  match x, y with
  | Nil, Nil -> 0
  | Nil, _ -> -1
  | Cons (x1, y1), Cons (x2, y2) ->
    (match compare x1 x2 with
     | 0 -> compare y1 y2
     | c -> c)
  | Cons _, Nil -> 1
  | Cons _, _ -> -1
  | String s1, String s2 -> String.compare s1 s2
  | String _, (Nil | Cons _) -> 1
  | String _, _ -> -1
  | Int i1, Int i2 -> Int.compare i1 i2
  | Int _, (Nil | String _ | Cons _) -> 1
  | Int _, _ -> -1
  | Sym i1, Sym i2 -> Ast.Ident.compare i1 i2
  | Sym _, (Nil | String _ | Cons _ | Int _) -> 1
  | Sym _, _ -> -1
  | Fun _, _ -> raise CantCompareFunctions
;;

let (gen : t Quickcheck.Generator.t) =
  let open Quickcheck.Generator in
  union
    [ singleton Nil
    ; (Int.gen_incl Int.min_value Int.max_value >>| fun i -> Int i)
    ; (String.gen' Char.gen_print >>| fun s -> String s)
    ; (String.gen' Char.gen_alpha >>| Ast.Ident.of_s >>| fun id -> Sym id)
    ]
;;

let%test_module "to_string" =
  (module struct
    let parse_print s =
      Parser.parse_string s |> List.last_exn |> quote |> to_string |> print_endline
    ;;

    let%expect_test _ =
      parse_print "()";
      [%expect {| nil |}]
    ;;

    let%expect_test _ =
      parse_print "1";
      [%expect {| 1 |}]
    ;;

    let%expect_test _ =
      parse_print {| "asdf" |};
      [%expect {| "asdf" |}]
    ;;

    let%expect_test _ =
      parse_print {| (1 2 3) |};
      [%expect {| (1 2 3) |}]
    ;;

    let%expect_test _ =
      parse_print {| (a b c) |};
      [%expect {| (a b c) |}]
    ;;

    let%expect_test _ =
      parse_print {| foobar |};
      [%expect {| foobar |}]
    ;;

    let%expect_test _ =
      Fun (fun _ -> Nil) |> to_string |> print_endline;
      [%expect {| <function> |}]
    ;;
  end)
;;

let%test_module "compare properties" =
  (module struct
    open Quickcheck.Generator

    let%test_unit "equal reflexive" =
      Quickcheck.test ~sexp_of:[%sexp_of: t] gen ~f:(fun v ->
        [%test_eq: int] (compare v v) 0)
    ;;

    let%test_unit "equal transitive" =
      Quickcheck.test
        ~sexp_of:[%sexp_of: t * t * t]
        (tuple3 gen gen gen)
        ~f:(fun (v1, v2, v3) ->
          if compare v1 v2 = 0 && compare v2 v3 = 0 then [%test_eq: int] (compare v1 v3) 0)
    ;;

    let%test_unit "symmetry" =
      Quickcheck.test ~sexp_of:[%sexp_of: t * t] (tuple2 gen gen) ~f:(fun (v1, v2) ->
        [%test_eq: int] (compare v1 v2) (-compare v2 v1))
    ;;

    let%test_unit "lt transitive" =
      Quickcheck.test
        ~sexp_of:[%sexp_of: t * t * t]
        (tuple3 gen gen gen)
        ~f:(fun (v1, v2, v3) ->
          if compare v1 v2 < 0 && compare v2 v3 < 0
          then [%test_pred: int] (fun r -> r < 0) (compare v1 v3))
    ;;

    let%test_unit "gt transitive" =
      Quickcheck.test
        ~sexp_of:[%sexp_of: t * t * t]
        (tuple3 gen gen gen)
        ~f:(fun (v1, v2, v3) ->
          if compare v1 v2 > 0 && compare v2 v3 > 0
          then [%test_pred: int] (fun r -> r > 0) (compare v1 v3))
    ;;
  end)
;;

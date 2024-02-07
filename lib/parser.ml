open Core

include Nice_parser.Make (struct
    type result = Ast.Sexp.t list
    type token = Menhir_parser.token

    exception ParseError = Menhir_parser.Error

    let parse = Menhir_parser.sexp_eof

    include Lexer
  end)

let%test_module _ =
  (module struct
    let () = Printexc.record_backtrace false

    let parse_and_print s =
      let toplevels = parse_string s in
      if not (List.length toplevels = 1) then failwith "Found multiple toplevels";
      List.hd_exn toplevels |> Ast.Sexp.to_string |> print_endline
    ;;

    let%expect_test "atom" =
      parse_and_print "this_is_An_At0m";
      [%expect {| this_is_An_At0m |}]
    ;;

    let%expect_test "list" =
      parse_and_print "(a bBBb)";
      [%expect {| (a bBBb) |}]
    ;;

    let%expect_test "nested list" =
      parse_and_print "\n(a x (y Zzz (q)) (e)\n\t   )    ";
      [%expect {|
      (a x (y Zzz (q)) (e)) |}]
    ;;

    let%expect_test "empty list" =
      parse_and_print "( () (  ) (\t) (\n\t))";
      [%expect {| (() () () ()) |}]
    ;;

    let%expect_test "cons" =
      parse_and_print "(x . y)";
      [%expect {| (x . y) |}]
    ;;

    let%expect_test "string literals" =
      parse_and_print {| "s" |};
      [%expect {|"s"|}];
      parse_and_print {| "\n" |};
      [%expect {|
        "
        "|}];
      parse_and_print {| "\"" |};
      [%expect {|"\""|}]
    ;;

    let%expect_test "integer literals" =
      parse_and_print "123";
      [%expect {|123|}];
      parse_and_print "-123";
      [%expect {|-123|}]
    ;;

    let%expect_test "many literals" =
      parse_and_print {|
      (1 2 (s "three" four))
    |};
      [%expect {|
      (1 2 (s "three" four)) |}]
    ;;

    let%expect_test "fancy atoms" =
      parse_and_print "(+ (/ 4 5 (* 7 8)) 9 (namespace/do-a-thing 10))";
      [%expect {|
      (+ (/ 4 5 (* 7 8)) 9 (namespace/do-a-thing 10)) |}]
    ;;

    let%expect_test "quote" =
      parse_and_print "'(foo 'bar)";
      [%expect {| '(foo 'bar) |}]
    ;;

    let%expect_test "comments" =
      parse_and_print
        {|
        ;; this is a function
        (.lambda x
          ; with a comment
        x) ; and another comment
      |};
      [%expect {| (.lambda x x) |}]
    ;;

    let%expect_test "quasiquote" =
      parse_and_print "`(1)";
      [%expect {| `(1) |}];
      parse_and_print "`(1 ,(+ 1 1) ,@(list 3 4))";
      [%expect {|
        `(1 ,(+ 1 1) ,@(list 3 4)) |}]
    ;;

    (* Failed parsing *)

    let%expect_test "illegal list" =
      parse_and_print "(wrong (right)";
      [%expect.unreachable]
    [@@expect.uncaught_exn {| ("Nice_parser.Make(P).ParseError(3, _)") |}]
    ;;

    let%test_unit "parse and Ast.string_of_sexp round-trip" =
      Quickcheck.test
        ~sexp_of:[%sexp_of: Ast.Sexp.t]
        Ast.Sexp.quickcheck_generator
        ~f:(fun s ->
          let rt = s |> Ast.Sexp.to_string |> parse_string |> List.hd_exn in
          [%test_eq: Ast.Sexp.t] s rt)
    ;;
  end)
;;

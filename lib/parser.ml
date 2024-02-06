include Nice_parser.Make (struct
    type result = Ast.sexp list
    type token = Menhir_parser.token

    exception ParseError = Menhir_parser.Error

    let parse = Menhir_parser.sexp_eof

    include Lexer
  end)

let%test_module _ =
  (module struct
    let () = Printexc.record_backtrace false
    let print_sexp s = Printf.printf !"%{sexp:Ast.sexp}" s

    let parse_and_print s =
      let toplevels = parse_string s in
      if not (List.length toplevels = 1) then failwith "Found multiple toplevels";
      List.hd toplevels |> print_sexp
    ;;

    let%expect_test "atom" =
      parse_and_print "this_is_An_At0m";
      [%expect {| (Atom this_is_An_At0m) |}]
    ;;

    let%expect_test "list" =
      parse_and_print "(a bBBb)";
      [%expect {| (List ((Atom a) (Atom bBBb))) |}]
    ;;

    let%expect_test "nested list" =
      parse_and_print "\n(a x (y Zzz (q)) (e)\n\t   )    ";
      [%expect
        {|
      (List
       ((Atom a) (Atom x) (List ((Atom y) (Atom Zzz) (List ((Atom q)))))
        (List ((Atom e))))) |}]
    ;;

    let%expect_test "empty list" =
      parse_and_print "( () (  ) (\t) (\n\t))";
      [%expect {| (List ((List ()) (List ()) (List ()) (List ()))) |}]
    ;;

    let%expect_test "cons" =
      parse_and_print "(x . y)";
      [%expect {| (Cons ((Atom x) (Atom y))) |}]
    ;;

    let%expect_test "string literals" =
      parse_and_print {| "s" |};
      [%expect {|(Literal (LString s))|}];
      parse_and_print {| "\n" |};
      [%expect {|(Literal (LString "\n"))|}]
    ;;

    let%expect_test "integer literals" =
      parse_and_print "123";
      [%expect {|(Literal (LInt 123))|}];
      parse_and_print "-123";
      [%expect {|(Literal (LInt -123))|}]
    ;;

    let%expect_test "many literals" =
      parse_and_print {|
      (1 2 (s "three" four))
    |};
      [%expect
        {|
      (List
       ((Literal (LInt 1)) (Literal (LInt 2))
        (List ((Atom s) (Literal (LString three)) (Atom four))))) |}]
    ;;

    let%expect_test "fancy atoms" =
      parse_and_print "(+ (/ 4 5 (* 7 8)) 9 (namespace/do-a-thing 10))";
      [%expect
        {|
      (List
       ((Atom +)
        (List
         ((Atom /) (Literal (LInt 4)) (Literal (LInt 5))
          (List ((Atom *) (Literal (LInt 7)) (Literal (LInt 8))))))
        (Literal (LInt 9))
        (List ((Atom namespace/do-a-thing) (Literal (LInt 10)))))) |}]
    ;;

    let%expect_test "quote" =
      parse_and_print "'(foo 'bar)";
      [%expect {| (Quote (List ((Atom foo) (Quote (Atom bar))))) |}]
    ;;

    let%expect_test "comments" =
      parse_and_print
        {|
        ;; this is a function
        (.lambda x
          ; with a comment
        x) ; and another comment
      |};
      [%expect {| (List ((Atom .lambda) (Atom x) (Atom x))) |}]
    ;;

    let%expect_test "quasiquote" =
      parse_and_print "`(1)";
      [%expect {| (Quasiquote (List ((Literal (LInt 1))))) |}];
      parse_and_print "`(1 ,(+ 1 1) ,@(list 3 4))";
      [%expect
        {|
        (Quasiquote
         (List
          ((Literal (LInt 1))
           (Unquote (List ((Atom +) (Literal (LInt 1)) (Literal (LInt 1)))))
           (UnquoteSplicing
            (List ((Atom list) (Literal (LInt 3)) (Literal (LInt 4)))))))) |}]
    ;;

    (* Failed parsing *)

    let%expect_test "illegal list" =
      parse_and_print "(wrong (right)";
      [%expect.unreachable]
    [@@expect.uncaught_exn {| ("Nice_parser.Make(P).ParseError(3, _)") |}]
    ;;
  end)
;;

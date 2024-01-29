open Core

let () = Printexc.record_backtrace false

let eval_expr s =
  Spit.Parser.parse_string s |> List.map ~f:Spit.Evaluator.eval |> List.last_exn

let eval_print s =
  eval_expr s |> Printf.printf !"%{sexp:Spit.Evaluator.Value.t}"

let%test_module "ints" =
  (module struct
    let%expect_test "minus" =
      eval_print "(- 1 1)";
      [%expect {| (Int 0) |}]
  end)

let%test_module "lists" =
  (module struct
    let%expect_test "nth" =
      eval_print "(nth (list 1 2 3) 0)";
      [%expect {| (Int 1) |}];

      eval_print "(nth (list 1 2 3) 1)";
      [%expect {| (Int 2) |}];

      eval_print "(nth (list 1 2 3) 2)";
      [%expect {| (Int 3) |}];

      eval_print "(nth (list 1 2 3) 3)";
      [%expect {| Nil |}]

    let%expect_test "concat" =
      eval_print "(concat (list 1 2 3) (list 4 5 6))";
      [%expect
        {|
        (Cons
         ((Int 1)
          (Cons
           ((Int 2)
            (Cons ((Int 3) (Cons ((Int 4) (Cons ((Int 5) (Cons ((Int 6) Nil)))))))))))) |}]
  end)

let%test_module "booleans" =
  (module struct
    let%expect_test "and" =
      eval_print "(and* 't 1)";
      [%expect {| (Int 1) |}];

      eval_print "(and nil 1 2)";
      [%expect {| Nil |}];

      eval_print "(and 1 2 3 4)";
      [%expect {| (Int 4) |}]

    let%expect_test "or" =
      eval_print "(or* 't 1)";
      [%expect {| (Sym (Id t)) |}];

      eval_print "(or nil 1 2)";
      [%expect {| (Int 1) |}];

      eval_print "(or nil nil 2)";
      [%expect {| (Int 2) |}];

      eval_print "(or nil nil nil)";
      [%expect {| Nil |}]

    let%expect_test "cond" =
      eval_print "(cond t 1)";
      [%expect {| (Int 1) |}];

      eval_print "(cond nil 1 t 2)";
      [%expect {| (Int 2) |}];

      eval_print "(cond nil 1 nil 2)";
      [%expect {| (Sym (Id t)) |}]
  end)

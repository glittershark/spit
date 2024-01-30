open Core

let () = Printexc.record_backtrace false

let eval_expr s =
  Spit.Parser.parse_string s |> List.map ~f:Spit.Evaluator.eval |> List.last_exn
;;

let eval_print s = eval_expr s |> Spit.Value.to_string |> print_endline

let%test_module "control flow" =
  (module struct
    let%expect_test "let" =
      eval_print "(let* () 1)";
      [%expect {| 1 |}];
      eval_print "(let* ((x 1)) x)";
      [%expect {| 1 |}];
      eval_print "(let* ((x 1) (y 2)) (+ x y))";
      [%expect {| 3 |}];
      eval_print "(let* ((x 1) (y (+ x 1))) (+ x y))";
      [%expect {| 3 |}];
      eval_print
        {|
          (let* ((x 1)
                 (y (+ x 1)))
           (+ (let* ((x 4)) x)
              x))
        |};
      [%expect {| 5 |}]
    ;;
  end)

let%test_module "ints" =
  (module struct
    let%expect_test "minus" =
      eval_print "(- 1 1)";
      [%expect {| 0 |}]
    ;;
  end)
;;

let%test_module "lists" =
  (module struct
    let%expect_test "nth" =
      eval_print "(nth (list 1 2 3) 0)";
      [%expect {| 1 |}];
      eval_print "(nth (list 1 2 3) 1)";
      [%expect {| 2 |}];
      eval_print "(nth (list 1 2 3) 2)";
      [%expect {| 3 |}];
      eval_print "(nth (list 1 2 3) 3)";
      [%expect {| nil |}]
    ;;

    let%expect_test "concat" =
      eval_print "(concat (list 1 2 3) (list 4 5 6))";
      [%expect {| (1 2 3 4 5 6) |}]
    ;;
  end)
;;

let%test_module "booleans" =
  (module struct
    let%expect_test "and" =
      eval_print "(and* 't 1)";
      [%expect {| 1 |}];
      eval_print "(and nil 1 2)";
      [%expect {| nil |}];
      eval_print "(and 1 2 3 4)";
      [%expect {| 4 |}]
    ;;

    let%expect_test "or" =
      eval_print "(or* 't 1)";
      [%expect {| t |}];
      eval_print "(or nil 1 2)";
      [%expect {| 1 |}];
      eval_print "(or nil nil 2)";
      [%expect {| 2 |}];
      eval_print "(or nil nil nil)";
      [%expect {| nil |}]
    ;;

    let%expect_test "cond" =
      eval_print "(cond t 1)";
      [%expect {| 1 |}];
      eval_print "(cond nil 1 t 2)";
      [%expect {| 2 |}];
      eval_print "(cond nil 1 nil 2)";
      [%expect {| t |}]
    ;;
  end)
;;

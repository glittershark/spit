open Core

let () = Printexc.record_backtrace false

let eval_expr s =
  Spit.Parser.parse_string s |> List.map ~f:Spit.Evaluator.eval |> List.last_exn

let eval_print s =
  eval_expr s |> Printf.printf !"%{sexp:Spit.Evaluator.Value.t}"

let%expect_test "and" =
  eval_print "(and* 't 1)";
  [%expect {| (Int 1) |}];

  eval_print "(and nil 1 2)";
  [%expect {| Nil |}];

  eval_print "(and 1 2 3 4)";
  [%expect {| (Int 4) |}]

let%expect_test "or" =
  eval_print "(or* 't 1)";
  [%expect {| (Sym (Ident t)) |}];

  eval_print "(or nil 1 2)";
  [%expect {| (Int 1) |}];

  eval_print "(or nil nil 2)";
  [%expect {| (Int 2) |}];

  eval_print "(or nil nil nil)";
  [%expect {| Nil |}];

open Core
open React
open Lwt
open LTerm_style
open LTerm_text

let prompt =
  eval [ B_bold true; B_fg green; S "spit"; E_fg; B_fg blue; S " λ) "; E_fg; E_bold ]
;;

module Completion = struct
  let complete env input =
    let _, last_paren, _ =
      Zed_rope.fold
        (fun c (opens, last_paren, pos) ->
          match Zed_char.to_utf8 c with
          | "(" -> opens + 1, Some pos, pos + 1
          | ")" -> opens - 1, Some pos, pos + 1
          | _ -> opens, last_paren, pos + 1)
        input
        (0, None, 0)
    in
    let last_paren = Option.value_map ~default:0 ~f:(fun x -> x + 1) last_paren in
    let _, to_complete =
      Tuple2.map ~f:Zed_rope.to_string (Zed_rope.break input last_paren)
    in
    let candidates =
      let open Zed_string in
      env
      |> Evaluator.Env.vars
      |> List.map ~f:(fun s -> s |> Ast.Ident.to_string |> Zed_string.of_utf8)
      |> List.filter ~f:(starts_with ~prefix:to_complete)
    in
    candidates, last_paren
  ;;

  let%expect_test "complete function name" =
    let env =
      Evaluator.Env.of_vars
        [ Ast.Ident.of_s "foo", Value.Nil; Ast.Ident.of_s "foobar", Value.Nil ]
    in
    let complete_from_input i =
      let candidates, last_paren =
        complete env (i |> Zed_string.of_utf8 |> Zed_rope.of_string)
      in
      Printf.printf
        !"%d %{sexp:string list}"
        last_paren
        (List.map ~f:Zed_string.to_utf8 candidates)
    in
    complete_from_input "foo";
    [%expect {| 0 (foo foobar) |}];
    complete_from_input "(foo";
    [%expect {| 1 (foo foobar) |}];
    complete_from_input "((bar) (foo";
    [%expect {| 8 (foo foobar) |}]
  ;;
end

class read_line ~term ~history ~env =
  object (self)
    inherit LTerm_read_line.read_line ~history ()
    inherit [Zed_string.t] LTerm_read_line.term term

    method! completion =
      let candidates, pos = Completion.complete env self#input_prev in
      self#set_completion
        pos
        (List.map ~f:(fun file -> file, Zed_string.unsafe_of_utf8 " ") candidates)

    initializer self#set_prompt (S.const prompt)
  end

let rec loop term history env =
  Lwt.catch
    (fun () ->
      (new read_line ~term ~history:(LTerm_history.contents history) ~env)#run
      >|= fun input -> Some input)
    (function
      | Sys_unix.Break -> return None
      | exn -> Lwt.fail exn)
  >>= function
  | Some input ->
    Lwt.catch
      (fun () ->
        let ast = input |> Zed_string.to_utf8 |> Parser.parse_string in
        let result_s =
          match List.map ~f:(Evaluator.eval ~env) ast with
          | [] -> ""
          | [ res ] -> Value.to_string res
          | ress -> ress |> List.map ~f:Value.to_string |> String.concat_lines
        in
        LTerm.fprintl term result_s)
      (fun err -> LTerm.fprintl term (* TODO: Style? *) (Stdlib.Printexc.to_string err))
    >>= fun () ->
    LTerm_history.add history input;
    loop term history env
  | None -> loop term history env
;;

let run env =
  LTerm_inputrc.load ()
  >>= fun () ->
  Lwt.catch
    (fun () ->
      Lazy.force LTerm.stdout >>= fun term -> loop term (LTerm_history.create []) env)
    (function
      | LTerm_read_line.Interrupt -> Lwt.return ()
      | exn -> Lwt.fail exn)
;;

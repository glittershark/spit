open Core
open Errors

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

let vars =
  let open Value in
  [
    ("nil", Nil);
    ("cons", Fun cons);
    ("car", Fun car);
    ("cdr", Fun cdr);
    ("+", Fun plus);
    ("-", Fun minus);
    ("compare", Fun cmp);
    ("zero?", Fun zerop);
    ("neg?", Fun negp);
    ("pos?", Fun posp);
  ]
  |> List.map ~f:(fun (id, v) -> (Ast.Ident.of_s id, v))

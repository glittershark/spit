let rec sum = function [] -> 0 | x :: xs -> x + sum xs
let%test _ = sum [ 1; 2; 3 ] = 6

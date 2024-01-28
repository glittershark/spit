  $ spit - <<EOF
  > (.def id (.lambda (x) x))
  > (id 1)
  > EOF
  (Int 1)

  $ spit - <<EOF
  > (.def plus (.lambda (x y) (+ x y)))
  > (plus 1 2)
  > EOF
  (Int 3)

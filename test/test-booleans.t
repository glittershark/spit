  $ spit - <<EOF
  > (and nil 1 2)
  > EOF
  Nil

  $ spit - <<EOF
  > (and 1 2 3)
  > EOF
  (Int 3)

  $ spit - <<EOF
  > (or nil 1 2)
  > EOF
  (Int 1)

  $ spit - <<EOF
  > (or nil nil 2)
  > EOF
  (Int 2)

  $ spit - <<EOF
  > (or nil nil nil)
  > EOF
  Nil

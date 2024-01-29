  $ spit - <<EOF
  > (and nil 1 2)
  > EOF
  nil

  $ spit - <<EOF
  > (and 1 2 3)
  > EOF
  3

  $ spit - <<EOF
  > (or nil 1 2)
  > EOF
  1

  $ spit - <<EOF
  > (or nil nil 2)
  > EOF
  2

  $ spit - <<EOF
  > (or nil nil nil)
  > EOF
  nil

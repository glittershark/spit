  $ spit - <<EOF
  > (=* 1 1)
  > EOF
  't

  $ spit - <<EOF
  > (=* 1 2)
  > EOF
  nil

  $ spit - <<EOF
  > (<* 1 2)
  > EOF
  't

  $ spit - <<EOF
  > (>* 1 2)
  > EOF
  nil

  $ spit - <<EOF
  > (>* 2 1)
  > EOF
  't

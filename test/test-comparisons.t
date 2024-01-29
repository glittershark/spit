  $ spit - <<EOF
  > (=* 1 1)
  > EOF
  (Sym (Ident t))

  $ spit - <<EOF
  > (=* 1 2)
  > EOF
  Nil

  $ spit - <<EOF
  > (<* 1 2)
  > EOF
  (Sym (Ident t))

  $ spit - <<EOF
  > (>* 1 2)
  > EOF
  Nil

  $ spit - <<EOF
  > (>* 2 1)
  > EOF
  (Sym (Ident t))

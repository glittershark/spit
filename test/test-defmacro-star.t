Test stdlib defmacro*

  $ spit - <<EOF
  > (defmacro* my-def args (list '.def (car args) (car (cdr args))))
  > (my-def x 1)
  > x
  > EOF
  1

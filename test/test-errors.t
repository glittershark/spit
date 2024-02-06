  $ spit - <<EOF
  > (+ x x)
  > EOF
  error: Unknown identifier x
         while evaluating x
         while evaluating (+ x x)
  [1]

  $ spit - <<EOF
  > (defun* bad (x) (+ x y))
  > (bad 1)
  > EOF
  error: Unknown identifier y
         while evaluating y
         while evaluating (+ x y)
         while calling (bad 1)
         while evaluating (bad 1)
  [1]

  $ spit - <<EOF
  > (defmacro* bad (x) \`(+ ,x ,y))
  > (bad 1)
  > EOF
  error: Unknown identifier y
         while evaluating y
         while evaluating special form (.quasiquote (+ (.unquote x) (.unquote y)))
         while evaluating (.quasiquote (+ (.unquote x) (.unquote y)))
         while expanding macro (bad 1)
         while evaluating (bad 1)
  [1]

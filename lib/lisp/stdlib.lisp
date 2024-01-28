(.def list (.lambda x x))

;; (defmacro* name arg ret)
;; -> (list (.def name (.lambda arg ret)) (.make-macro name))
(.def
 defmacro*
 (.lambda
  args
  (list
   'list
   (list
    '.def
    (car args)
    (list
     '.lambda
     (car (cdr args))
     (car (cdr (cdr args)))))
   (list '.make-macro (car args)))))
(.make-macro defmacro*)

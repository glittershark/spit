(.def list (.lambda x x))

;; (defmacro* name arg ret)
;; -> (list (.def name (.lambda arg ret)) (.make-macro name))
(.def defmacro*
      (.lambda
       (name args ret)
       (list 'list
             (list '.def name (list '.lambda args ret))
             (list '.make-macro name))))
(.make-macro defmacro*)

(defmacro* defmacro-alias
  (name alias-for)
  (list 'defmacro* name 'args (list 'cons (list '.quote alias-for) 'args)))

(defmacro-alias lambda .lambda)
(defmacro-alias quote .quote)

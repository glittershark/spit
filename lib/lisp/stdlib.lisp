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
(defmacro-alias def .def)

(defmacro* defun* (name arg ret)
  (list 'def name (list 'lambda arg ret)))

(defun* <* (x1 x2) (neg? (compare x1 x2)))
(defun* >* (x1 x2) (pos? (compare x1 x2)))
(defun* =* (x1 x2) (zero? (compare x1 x2)))

;; Lists

(defun* nil? (xs) (.if xs nil t))

(defun* length (xs)
  (.if xs (+ 1 (length (cdr xs))) 0))

;; Booleans

(defun* not (x) (nil? x))

(.def t 't)
(defmacro* and* (x y) (list '.if x y nil))
(defmacro* or* (x y) (list '.if x x y))

(defmacro* and xs
  (.if (nil? xs)
       t
       (.if (=* 1 (length xs))
            (car xs)
            (list 'and* (car xs) (cons 'and (cdr xs))))))

(defmacro* or xs
  (.if (nil? xs)
       nil
       (.if (=* 1 (length xs))
            (car xs)
            (list 'or* (car xs) (cons 'or (cdr xs))))))

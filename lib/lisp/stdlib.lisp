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

(defun* nth (xs n)
  (and xs
       (.if (zero? n) (car xs)
            (nth (cdr xs) (- n 1)))))

(defun* caar (x) (car (car x)))
(defun* cadr (x) (car (cdr x)))
(defun* cdar (x) (cdr (car x)))
(defun* cddr (x) (cdr (cdr x)))
(defun* caaar (x) (car (car (car x))))
(defun* caadr (x) (car (car (cdr x))))
(defun* cadar (x) (car (cdr (car x))))
(defun* caddr (x) (car (cdr (cdr x))))
(defun* cdaar (x) (cdr (car (car x))))
(defun* cdadr (x) (cdr (car (cdr x))))
(defun* cddar (x) (cdr (cdr (car x))))
(defun* cdddr (x) (cdr (cdr (cdr x))))
(defun* caaaar (x) (car (car (car (car x)))))
(defun* caaadr (x) (car (car (car (cdr x)))))
(defun* caadar (x) (car (car (cdr (car x)))))
(defun* caaddr (x) (car (car (cdr (cdr x)))))
(defun* cadaar (x) (car (cdr (car (car x)))))
(defun* cadadr (x) (car (cdr (car (cdr x)))))
(defun* caddar (x) (car (cdr (cdr (car x)))))
(defun* cadddr (x) (car (cdr (cdr (cdr x)))))
(defun* cdaaar (x) (cdr (car (car (car x)))))
(defun* cdaadr (x) (cdr (car (car (cdr x)))))
(defun* cdadar (x) (cdr (car (cdr (car x)))))
(defun* cdaddr (x) (cdr (car (cdr (cdr x)))))
(defun* cddaar (x) (cdr (cdr (car (car x)))))
(defun* cddadr (x) (cdr (cdr (car (cdr x)))))
(defun* cdddar (x) (cdr (cdr (cdr (car x)))))
(defun* cddddr (x) (cdr (cdr (cdr (cdr x)))))

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

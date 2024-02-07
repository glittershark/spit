(.def list (.lambda x x))

;; (defmacro* name arg ret)
;; -> (list (.def name (.lambda arg ret)) (.make-macro name))
(.def defmacro*
      (.lambda
       (name args ret)
       `(list (.def ,name (.lambda ,args ,ret))
              (.make-macro ,name))))
(.make-macro defmacro*)

(defmacro* defmacro-alias (name alias-for)
  `(defmacro* ,name args (cons (.quote ,alias-for) args)))

(defmacro-alias lambda .lambda)
(defmacro-alias quote .quote)
(defmacro-alias def .def)

(defmacro* defun* (name arg ret)
  `(def ,name (lambda ,arg ,ret)))

(defmacro* defmacro args
  (let ((name args & body) args)
    `(defmacro* ,name __args__
       (destructuring-bind ,args __args__
         ,@body))))

;; Control flow

(defmacro* if args
  (.if (cdr (cdr args))
       (cons '.if args)
       `(.if ,(car args) ,(cdr args) nil)))

(defmacro* let* (vars body)
  (if (nil? vars)
      body
      `((lambda (,(caar vars))
          (let* ,(cdr vars) ,body))
        ,(cadar vars))))

;; Evaluate body in a context where vars is bound to vals, supporting
;; destructuring
(defmacro* destructuring-bind (vars vals body)
  (cond
    (nil? vars) nil

    (symbol? vars)
    `(let* ((,vars ,vals)) ,body)

    (=* '& (car vars))
    `(let* ((,(cadr vars) ,vals)) ,body)

    (symbol? (car vars))
    `(let* ((,(car vars) (car ,vals)))
       (destructuring-bind
           ,(cdr vars)
           (cdr ,vals)
         ,body))

    `(destructuring-bind ,(car vars) ,())))

(defmacro* let args
  (let* ((vars (car args))
         (body (cdr args)))
    (if (nil? vars)
        *(do body)
        `(destructuring-bind
             ,(caar vars)
             ,(cadar vars)
           (let ,(cdr vars) ,@body)))))

(defmacro* do body
  `(last (list ,@body)))

(defmacro* defun args
  (let* ((name (car args))
         (fn-args (cadr args))
         (body (cddr args)))
    `(defun* ,name __args__
       (destructuring-bind ,fn-args __args__ ,@body))))

;; IO

(defun* print (x)
  (if (=* 'string (type x))
      (print-string x)
      (print-string (to-string x))))

(defun* println (x)
  (do
   (print x)
   (print "\n")))

;; Comparisons

(defun* <* (x1 x2) (neg? (compare x1 x2)))
(defun* >* (x1 x2) (pos? (compare x1 x2)))
(defun* =* (x1 x2) (zero? (compare x1 x2)))

(defmacro* deftypecheck (name ty)
  `(defun* ,name (x) (=* (.quote ,ty) (type x))))
(deftypecheck list? list)
(deftypecheck int? int)
(deftypecheck string? string)
(deftypecheck symbol? symbol)
(deftypecheck function? function)

;; Lists

(defun* nil? (xs) (if xs nil t))

(defun* length (xs)
  (.if xs (+ 1 (length (cdr xs))) 0))

(defun* nth (xs n)
  (and xs
       (if (zero? n)
           (car xs)
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

(defun* foldl (f acc xs)
  (if (nil? xs) acc (foldl f (f acc (car xs)) (cdr xs))))

(defun* foldr (f acc xs)
  (if (nil? xs) acc (f (car xs) (foldr f acc (cdr xs)))))

(defun* concat* (x1 x2)
  (if (nil? x1) x2
      (cons
       (car x1)
       (concat* (cdr x1) x2))))
(defun* concat xs
  (foldr concat* nil xs))

(defun* last (l)
  (cond
    (nil? l) nil
    (=* 1 (length l)) (car l)
    'else (last (cdr l))))

(defun* map (f l)
  (if (nil? l) l
      (cons (f (car l))
            (map f (cdr l)))))

;; Booleans

(defun* not (x) (nil? x))

(.def t 't)
(defmacro* and* (x y) `(if ,x ,y nil))
(defmacro* or* (x y) `((lambda (__x__) (if __x__ __x__ ,y)) ,x))

(defmacro* and xs
  (.if (nil? xs)
       t
       (.if (=* 1 (length xs))
            (car xs)
            `(and* ,(car xs) (and ,@(cdr xs))))))

(defmacro* or xs
  (.if (nil? xs)
       nil
       (.if (=* 1 (length xs))
            (car xs)
            `(or* ,(car xs) (or ,@(cdr xs))))))

(defmacro* cond args
  (.if (nil? args)
       t
       (if (=* 1 (length args))
           (car args)
           `(if ,(car args)
                ,(cadr args)
                (cond ,@(cddr args))))))

(defun foo (x) 42)
(defmacro foo (x) 42)
(defun plusf (a b) (+ a b))
(defun plusl (a b) `(+ ,a ,b))
(defmacro plusm (a b) `(+ ,a ,b))
(if t (progn (princ "Hola") (princ "Mundo")))
(when t (princ "Hola") (princ "Mundo"))
(macroexpand-1 '(when t (princ "Hola") (princ "Mundo")))


(let ((x x-value))
    (if x true-expr false-expr))

(if-let (x x-value) true-expr false-expr)


(defmacro if-let (binding true-expr false-expr)
    `(let (,bindidng)
        (if ,(first binding) ,true-expr ,false-expr)))

(macroexpand-1 '(if-let (x 42) 'true 'false))


(if-swapped condidtion true-expr false-expr)
(if condition true-expr false-expr)
;; Ejemplos de uso
(if-let (x 42)
    (princ x) (princ "Ir a Nil"))

;; Implementaciones
(defmacro if-let ((x x-val) true-expr false-expr)
  `(let ((,x ,x-val))
     (if ,x ,true-expr ,false-expr)))

(defmacro if-let (bindig ture-expr false-expr)
  `(let ,bindig
     (if ,(first (first bindig)) ,ture-expr ,false-expr)))

;;macro definida anteriormente
(defmacro if-let ((x x-val) true-expr &optional false-expr)
    `(let ((,x ,x-val))
        (if ,x , true-expr ,false-expr)))
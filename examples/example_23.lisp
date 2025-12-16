;;copiar y pegar este codigo en el interprete de lisp para probarlo
(defmacro for ((var from to) &rest body)
 `(do ((,var ,from (1+ ,var))
        (limit ,to))
    ((>= ,var limit))
    ,@body))
;;intente despues con la siguiente instruccion
;;* (for (i 0 10) (princ i))

(do ((limit o (1+ limit)))
    ((>= limit 5))
  (princ limit))
;;redefinimos la macro para evitar el problema de variable libre
(defmacro for ((var from to) &rest body)
  (let ((sym-to (gensym))) 
   `(do ((,var ,from (1+ ,var))
        (,sym-to ,to))
    ((>= ,var ,sym-to))
    ,@body)))

;;intente despues con la siguiente instruccion
;;* (for (limit 0 10) (princ limit))
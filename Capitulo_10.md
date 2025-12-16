# Prevent Macro Name Conflict with Genysim
Escriba el siguiente código en el interprete (puede compiarlo del archivo de ejemplo_22.lisp también)
```
* (defmacro for ((var from to) &rest body)
 `(do ((,var ,from (1+ ,var))
        (limit ,to))
    ((>= ,var limit))
    ,@body))
FOR
* (for (i 0 10) (princ i))
0123456789
NIL
```
Si intentamos usar dos veces la variable "limit" encontramos errores.
```
* (for (limit 0 10) (princ i))
; in: FOR (LIMIT 0 10)
;     (LIMIT 10)
;
; caught ERROR:
;   The variable LIMIT occurs more than once in the LET.
; 
; compilation unit finished
;   caught 1 ERROR condition
```
No es tan simple el uso de nombres de variables dentro de una macro, utilizando la instruccion "(symbol)", ahora podemos
hacer un espacio de nombres para evitar la cosilión.

```
* (defmacro for ((var from to) &rest body)
  (let ((sym-to (gensym))) 
   `(do ((,var ,from (1+ ,var))
        (,sym-to ,to))
    ((>= ,var ,sym-to))
    ,@body)))
FOR
* (for (limit 0 10) (princ limit))
0123456789
NIL
```


## Archivos de ejemplo:

[example_22.lisp](./examples/example_22.lisp)
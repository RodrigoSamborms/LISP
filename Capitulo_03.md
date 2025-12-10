# Loops
El equivalente en LISP para el For es (Dotimes (var repeticiones valor_retorno) expr1 expr2 ... exprN )
```
(dotimes (i 5)
  (format t "La iteración numero: ~a~%" i))
```
El equivalente en LISP  para el Do Loop es: 
(do  ((var1 iniciar1 paso1) (var2 iniciar2 paso2)) (eval1 resultado_parcial) sentencias_a_repetir )
```
(do ((i 0 (1+ i))  ; Inicializar i a 0, incrementar por 1 cada iteración
     (sum 0 (+ sum i))) ; Inicializar suma a 0, agregar i a la suma en cada iteración
    ((>= i 5) sum) ; repetir hasta que i es mayor o igual a 5, regresar sum
  (format t "Iteración i: ~a, Suma acumulada: ~a~%" i sum))
```

## lista de archivos con código de ejemplo:

 [example_02.lisp](.\examples\example_02.lisp)
 <br>
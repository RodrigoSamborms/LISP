# Métodos para construir listas
El operador ', evita que la lista sea evaluada y solo muestra la lista en sí, es una abreviación de la función "quote":
```
* '(+ (* 2 3) 1)
(+ (* 2 3) 1)
* (quote (+ (* 2 3) 1))
(+ (* 2 3) 1)
*
```
Un error comun es intentar usar el operador ' para asignar un valor a un elemento de la lista como muestra el siguiente ejemplo:
```
* (defun exprq (x) '(+ (* 2 x) 1))
; in: DEFUN EXPRQ
;     (SB-INT:NAMED-LAMBDA EXPRQ
;         (X)
;       (BLOCK EXPRQ '(+ (* 2 X) 1)))
; 
; caught STYLE-WARNING:
;   The variable X is defined but never used.
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
EXPRQ
* (exprq 3)
(+ (* 2 X) 1)
*
```
Podemos apreciar como nos envia la advertencia "The variable X is defined but never used", para poder obtener el efecto deseado
debemos utilizar la función 'cons', como se muestra: 
```
* (defun exprc (x) (cons '+ (cons (cons '* (cons 2 (cons x nil))) (cons 1 nil))))
WARNING: redefining COMMON-LISP-USER::EXPRC in DEFUN
EXPRC
* (exprc 3)
(+ (* 2 3) 1)
*
```
El problema del uso de esta función es evidente, se requiere "detallar" más la lista al definirla.
Para solucionar este problema tenemos la función lista, la cual genera la lista y después la evalua:
```
* (defun expr1 (x) (list '+ (list '* 2 x) 1))
EXPR1
* (expr1 3)
(+ (* 2 3) 1)
*
```
Una mejor forma es utilizar el operador ` en conjunto con el operador , como vemos en este ejemplo:
```
* (defun exprb (x) `(+ (* 2 ,x) 1))
EXPRB
* (exprb 3)
(+ (* 2 3) 1)
*
```
En la definición de la función marcamos con la , al elmento que queremos sea evaluado antes de la construcción de la lista,
también contamos con una forma de agregar varios elementos a la vez:

```
* (defun exprbis (&rest xs) `(+ (* 2 ,@xs) 1))
EXPRBIS
* (exprbis 3 4 5 6 7)
(+ (* 2 3 4 5 6 7) 1)
*
```

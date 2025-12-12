# Destructores y más
Definimos una funcion con tres parametros, mostramos un ejemplo de uso con todos sus parametros funcionara correctamente, si tratamos de utilizar la función con menos parametros de los que pide, recibiremos un error:
```
* (defun foo (a b c) (list a b c))
FOO
* (foo 1 2 3)
(1 2 3)
* (foo 1 2)

debugger invoked on a SB-INT:SIMPLE-PROGRAM-ERROR @535D2DC7 in thread
#<THREAD "main thread" RUNNING {10048C8253}>:
  invalid number of arguments: 2
```
Para evitar este error, podemos indicar que el tercer parametro es opcional:
```
* (defun foo (a b &optional c) (list a b c))
* (foo 1 2)

(1 2 NIL)
```
Ahora cuando agregamos solo dos parametros, vemos que el tercer parametro se ha llenado con NIL de manera automatica,
es posible asignar otro valor predeterminado aparte de NIL, como muestra el siguiente ejemplo:
```
* (defun foo (a b &optional (c 42)) (list a b c))
* (foo 1 2)

(1 2 42)
```
Podemos apreciar que ahora el valor por defecto es 42 en lugar de NIL, es posible asignar el resto de parametros como una
lista también:
```
* (defun foo (a &rest params) (list a params))
* (foo 1 2 3)

(1 (2 3))
```
Es posible utilizar la notación de pares (Clave : Valor), lo cual nos ayuda a dar más flexibilidad a la asignación de valores a elementos especificos.
```
* (defun foo (&key a b c) (list a b c))
* (foo :c 3)

(NIL NIL 3)
```
Podemos combinar la asignación (Clave : Valor) con parametros con valores predefinidos
```
* (defun foo (&key (a 42) b c) (list a b c))
* (foo :c 3)

(42 NIL 3)
```
Si queremos aplicar los mismos conceptos a las macros, encontraremos problemas, ya que las macros a diferencia de las funciones
se expanden en el código y no pueden quedar sin referencias.
```
* (defmacro if-let ((x x-val) true-expr &optional false-expr)
    `(let ((,x ,x-val))
        (if ,x , true-expr ,false-expr)))
* (if-let x x)
; in: FOO 1
;     (IF-LET X X)
;
; caught ERROR:
;   during macroexpansion of (IF-LET X X). Use *BREAK-ON-SIGNALS* to intercept.
;
;    Error while parsing arguments to DEFMACRO IF-LET:
;      invalid number of elements in
;        X
;      to satisfy lambda list
;        (X X-VAL):
;      exactly 2 expected, but got a non-list
```
## Código de Ejemplo:

[example_19.lisp](./examples/example_19.lisp)
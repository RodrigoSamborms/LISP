# Macros
Una idea practica de lo que es una macro si definimos una funcion con solo un valor constante:
```
* (defun foo (x) 42)
; in: DEFUN FOO
;     (SB-INT:NAMED-LAMBDA FOO
;         (X)
;       (BLOCK FOO 42))
;
; caught STYLE-WARNING:
;   The variable X is defined but never used.
;
; compilation unit finished
;   caught 1 STYLE-WARNING condition
FOO
```
Y posteriormente intentamos ejecutarla:
```
* (foo x)

debugger invoked on a UNBOUND-VARIABLE @52B4FD3F in thread
#<THREAD "main thread" RUNNING {10048C8253}>:
  The variable X is unbound.
```
Generar el error de variable no enlazada, pero si lo definimos como una macro:
```
* (defmacro foo (x) 42)
; in: DEFMACRO FOO
;     (SB-INT:NAMED-DS-BIND (:MACRO FOO . DEFMACRO)
;         (X)
;         (CDR #:EXPR)
;       (DECLARE (SB-C::CONSTANT-VALUE X))
;       (BLOCK FOO 42))
; ==>
;   (LET* ((#:G0 (SB-C::CHECK-DS-LIST (CDR #:EXPR) 1 1 '(# X))) (X (POP #:G0)))
;     (DECLARE (SB-C::CONSTANT-VALUE X))
;     (BLOCK FOO 42))
;
; caught STYLE-WARNING:
;   The variable X is defined but never used.
;
; compilation unit finished
;   caught 1 STYLE-WARNING condition
STYLE-WARNING:
   FOO is being redefined as a macro when it was previously defined to be a function.
WARNING: redefining COMMON-LISP-USER::FOO in DEFMACRO
FOO
```
Y ahora usamos la macrodefinicion:
```
* (foo x)
42
```
Obtenemos el valor constante asignado, la razón es que las macros se "expanden" a diferencia de las funciones, salta a la vista
inmediatamente el estilo de la sintaxis de LISP, que al manejar listas hace que todo aparente ser uniforme y dificulta distiguir
de manera directa a un principiante que es cada cosa.
Ahora definimos una función y la usamos:
```
* (defun plus (a b) (+ a b))
PLUS
* (plus 2 3)
5
```
El resultado es el esperado, la funcion retorna la suma de los dos valores. Ahora redifinimos la función como un generador de listas:
```
* (defun plus (a b) `(+ ,a ,b))
WARNING: redefining COMMON-LISP-USER::PLUS in DEFUN
PLUS
* (plus 2 3)
(+ 2 3)
```
Ahora definimos la función como una macro
```
* (defmacro plusm (a b) `(+ ,a ,b))
PLUSM
* (plusm 2 3)
5
``` 
No parece haber diferencia alguna entre definir una funcion que genera una lista como una macro esta vez, si utilizamos la funcion "macroexpand", podemos ver como la macro se expande primero y no se evalua.
```
* (macroexpand-1 '(plusm 2 3))
(+ 2 3)
T
*
```
El proposito de las macros es tomar el hecho de que se expande el código para formar "sintaxis sugar", lo cual ayuda
a mejorar la escritura de funciones.
```
* (if t (progn (princ "Hola") (princ "Mundo")))
HolaMundo
"Mundo"
* (when t (princ "Hola") (princ "Mundo"))
HolaMundo
"Mundo"
* (macroexpand-1 '(when t (princ "Hola") (princ "Mundo")))
(IF T
    (PROGN (PRINC "Hola") (PRINC "Mundo")))
T
*
```
Podemos apreciar que "When" es una macro de "If" al usar la funcion "Macroexpand". Tambien podemos definir una macro para crear una sentencia IF THEN ELSE:
```
* (defmacro if-let (binding true-expr false-expr)
    `(let (,bindidng)
        (if ,(first binding) ,true-expr ,false-expr)))
; in: DEFMACRO IF-LET
;     `(LET (,BINDIDNG)
;        (IF ,(FIRST BINDING)
;            ,TRUE-EXPR
;            ,FALSE-EXPR))
; ==>
;   (SB-IMPL::|List| BINDIDNG)
;
; caught WARNING:
;   undefined variable: COMMON-LISP-USER::BINDIDNG
;
; compilation unit finished
;   Undefined variable:
;     BINDIDNG
;   caught 1 WARNING condition
IF-LET
```
Si expandemos esta macro veremos que:
```
 (macroexpand-1 '(if-let (x 42) 'true 'false))


debugger invoked on a UNBOUND-VARIABLE @535D2C3D in thread
#<THREAD "main thread" RUNNING {10048C8253}>:
  The variable BINDIDNG is unbound.

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [CONTINUE   ] Retry using BINDIDNG.
  1: [USE-VALUE  ] Use specified value.
  2: [STORE-VALUE] Set specified value and use it.
  3: [ABORT      ] Reduce debugger level (to debug level 1).
  4:               Exit debugger, returning to top level.

((MACRO-FUNCTION IF-LET) (IF-LET (X 42) (QUOTE TRUE) (QUOTE FALSE)) #<unused argument>)
   source: `(LET (,BINDIDNG)
              (IF ,(FIRST BINDING)
                  ,TRUE-EXPR
                  ,FALSE-EXPR))
0[2]
```
Las macros tambien permiten usar funciones de manera diferente a al que se tenía en mente, por ejemplo para intercambiar las
condiciones de una sentencia "IF", podemos usar:
```
(if-swapped condidtion true-expr false-expr)
(if condition true-expr false-expr)
```
Ahora tenemos un "IF" donde las condiciones estan intercambiadas.

## Programas de Ejemplo:

 [example_18.lisp](./examples/example_18.lisp)
<br>
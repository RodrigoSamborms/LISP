# Packages
Para evitar la colision de nombres, utilizamos los paquetes, los cuales limitan el alcance del espacio de nombres, verificamos el tipo de dato *PACKAGE* :
```
* *package*
#<PACKAGE "COMMON-LISP-USER">
* (type-of *package*)
PACKAGE
```
El paquete CL es el que por defecto usa el intreprete de LISP, podemos verificar que el simbolo T ("usado anteriormente como 't'"), existe dentro del paquete:
```
* :cl
:CL
* (eql :cl keyword:cl)
T
* (eql :cl ':cl)
T
```
También podemos ver definiciones de simbolos dentro del paquete:
```
* (describe 'and)
COMMON-LISP:AND
  [symbol]

AND names a macro:
  Lambda-list: (&REST FORMS)
  Source file: SYS:SRC;CODE;MACROS.LISP

Symbol-plist:
  SB-DISASSEM::INSTRUCTIONS -> (#<SB-DISASSEM:INSTRUCTION AND(..
```
Podemos cambiar de package usando "in-package":
```
* (in-package :cl)
#<PACKAGE "COMMON-LISP">
* *package*
#<PACKAGE "COMMON-LISP">
```
Podemos definir un paquete usando "defpackage" y cambiarnos al paquete definido con "in-package" si intentamos utilizar funciones
que no esten definidas dentro del paquete nos marcara un error (en el ejemplo tratamos de utilizar  "test ()")

```
* (defpackage :com.example
  (:use :cl))
#<PACKAGE "COM.EXAMPLE">
* (in-package :com.example)
#<PACKAGE "COM.EXAMPLE">
* (test)
; in: TEST
;     (COM.EXAMPLE::TEST)
;
; caught STYLE-WARNING:
;   undefined function: COM.EXAMPLE::TEST
```

```
* *package*
#<PACKAGE "COMMON-LISP-USER">
* (defpackage :com.example (:use :cl))
#<PACKAGE "COM.EXAMPLE">
* (defun test () "Probando desde dentro del paquete com.example")
TEST
* (in-package :com.example)
#<PACKAGE "COM.EXAMPLE">
* *package*
#<PACKAGE "COM.EXAMPLE">
* (cl-user::test)
"Probando desde dentro del paquete com.example"
* (in-package :cl-user)
#<PACKAGE "COMMON-LISP-USER">
```
El siguiente ejemplo utiliza los conceptos anteriores:
```
* ;;verificamos en que paquete estamos
*package*
#<PACKAGE "COMMON-LISP-USER">
* ;;definimos un paquete para usar
(defpackage :com.example (:use :cl))
#<PACKAGE "COM.EXAMPLE">
* ;;definimos una funcion en el paquete actual ("COMMON-LISP-USER")
(defun test () "Probando desde dentro del paquete com.example")
TEST
* ;;ingresamos al paquete creado con anterioridad 
(in-package :com.example)
#<PACKAGE "COM.EXAMPLE">
* ;;verificamos el paquete en el que estamos
*package*
#<PACKAGE "COM.EXAMPLE">
* ;;llamamos a la función que definimos fuera del paquete antes
(cl-user::test)
"Probando desde dentro del paquete com.example"
* ;;Regresamos al paquete anterior
(in-package :cl-user)
#<PACKAGE "COMMON-LISP-USER">
*
```
Puede encontrar el código anterior en el archivo de ejemplo numero 20.


```
* :cl
:CL
* (describe 'cl)
COMMON-LISP-USER::CL
  [symbol]
* (in-package :cl)
#<PACKAGE "COMMON-LISP">
* *package*
#<PACKAGE "COMMON-LISP">
* (defpackage :com.example
        (:use :cl))
#<PACKAGE "COM.EXAMPLE">
* (in-package :com.example)
#<PACKAGE "COM.EXAMPLE">
* *package*
#<PACKAGE "COM.EXAMPLE">
* (defun test () "Prueba dentro del paquete de ejemplo")
TEST
* (in-package :cl)
#<PACKAGE "COMMON-LISP">
* *package*
#<PACKAGE "COMMON-LISP">
* (com.example::test)
"Prueba dentro del paquete de ejemplo"
* (defpackage :com.example
        (:use :cl)
        (:export :test))
#<PACKAGE "COM.EXAMPLE">
* *package*
#<PACKAGE "COMMON-LISP">
* (com.example:test)
"Prueba dentro del paquete de ejemplo"
* (in-package :com.example)
#<PACKAGE "COM.EXAMPLE">
* (in-package :cl)
#<PACKAGE "COMMON-LISP">
* (defpackage :com.example2 (:use cl) (:import-from :com.example :test))
#<PACKAGE "COM.EXAMPLE2">
* (in-package :com.example2)
#<PACKAGE "COM.EXAMPLE2">
* (defun test2 () (test))
TEST2
* (in-package :cl)
#<PACKAGE "COMMON-LISP">
* (com.example2::test2)
"Prueba dentro del paquete de ejemplo"
*package*
#<PACKAGE "COMMON-LISP">
* (defpackage :com.example2 (:use cl) (:import-from :com.example :test) (:export :test2))
#<PACKAGE "COM.EXAMPLE2">
* (com.example2:test2)
"Prueba dentro del paquete de ejemplo"
;;la siguiente instruccion evita que se importen todos los simbolos al paquete, solo las referencias quedan
* (defpackage :com.example2 (:use cl) (:import-from :com.example #:test) (:export #:test2))
#<PACKAGE "COM.EXAMPLE2">
* (com.example2:test2)
"Prueba dentro del paquete de ejemplo"
;importar todos los simbolos puede crear duplicados y la función test no es utilizara demasiado por lo que
;se marca con # si se requiren los simbolos se tomaran del paquete original lo cual genera una carga adicional
```
Como nota adicional el uso de ":use paquete_simbolos" solo se reserva para librerias comunes, no para las propias
## Código de Ejemplo:

[example_20.lisp](./examples/example_20.lisp)
<br>
(defpackage :com.example
  (:use :cl))

;;definimos la funcion fuera del paquete que hemos creado anteriormente
(defun test () "Probando desde dentro del paquete com.example")
;;ingresamos al a paquete com.example
(in-package :com.example)
;;el resultado de la función es error porque la función test no esta definida dentro del paquete com.example
(test)
;;para usar una funcion definida fuera del paquete, debemos usar el nombre completo
(cl-user::test)

;;para regresar al paquete principal
(in-package :cl-user)


;;una mejora a lo anterior es exportar la función test al paquete com.example
(defpackage :com.example
  (:use :cl)
  (:export :test))
;;
(defpackage :com.example2
  (:use :cl)
  (:import-from :com.example :test))

;;La siguiente insturccion es a revisar
(com.example2::test2)
;;
(in-package :com.example2)
;;
(defun test2 () (test))
;;
(defpackage :com.example2
  (:use :cl)
  (:import-from :com.example :test)
  (:export :test2))
;;
(com.example2:test2)
;;el simbolo # es para marcar funciones que seran excluidas de la exportacion o importacion
(defpackage :com.example2
  (:use :cl)
  (:import-from :com.example #:test)
  (:export #:test2))
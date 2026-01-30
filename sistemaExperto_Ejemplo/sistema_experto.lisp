;; Este es un ejemplo simple de un sistema experto en LISP.
;; El sistema experto responderá preguntas sobre un animal basado en sus características.

;; Definimos una función que determina el tipo de animal basado en sus características.
(defun identificar-animal (tamaño color sonido)
  ;; Comenzamos con una serie de condiciones para identificar el animal.
  (cond
    ;; Si el tamaño es grande y el color es marrón, es un oso.
    ((and (equal tamaño 'grande) (equal color 'marrón))
     'oso)
    ;; Si el tamaño es pequeño y el color es blanco, es un ratón.
    ((and (equal tamaño 'pequeño) (equal color 'blanco))
     'ratón)
    ;; Si el tamaño es mediano y el sonido es "miau", es un gato.
    ((and (equal tamaño 'mediano) (equal sonido 'miau))
     'gato)
    ;; Si el tamaño es mediano y el sonido es "ladrido", es un perro.
    ((and (equal tamaño 'mediano) (equal sonido 'ladrido))
     'perro)
    ;; Si no se cumple ninguna de las condiciones anteriores, no se puede identificar el animal.
    (t 'desconocido)))

;; Ejemplo de uso de la función.
;; Llamamos a la función con diferentes características para ver qué animal identifica.
(format t "El animal es: ~a~%" (identificar-animal 'grande 'marrón 'silencio))  ;; Debería imprimir "El animal es: oso"
(format t "El animal es: ~a~%" (identificar-animal 'pequeño 'blanco 'silencio))  ;; Debería imprimir "El animal es: ratón"
(format t "El animal es: ~a~%" (identificar-animal 'mediano 'blanco 'miau))  ;; Debería imprimir "El animal es: gato"
(format t "El animal es: ~a~%" (identificar-animal 'mediano 'marrón 'ladrido))  ;; Debería imprimir "El animal es: perro"
(format t "El animal es: ~a~%" (identificar-animal 'grande 'negro 'silencio))  ;; Debería imprimir "El animal es: desconocido"

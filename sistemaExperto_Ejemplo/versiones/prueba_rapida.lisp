;; Cargar el sistema experto V6
(load "/mnt/c/Users/sambo/Documents/GitHub/LISP/sistemaExperto_Ejemplo/versiones/sistema_experto_V6.lisp")

;; ====================================================================
;; PRUEBAS AUTOMATIZADAS
;; ====================================================================

(defparameter *escenarios-prueba*
  '(
    ("Caballo"
     ((hace-sonido relincha)
      (tiene-pelo si)
      (tamaño grande)
      (es-domestico si)
      (vive-en-tierra si)
      (tiene-patas si)
      (numero-patas 4)
      (es-herbivoro si))
     "Caso esperado: CABALLO")

    ("Perro"
     ((hace-sonido ladrido)
      (tiene-pelo si)
      (tamaño mediano)
      (es-domestico si)
      (vive-en-tierra si)
      (tiene-patas si)
      (numero-patas 4)
      (es-carnivoro si))
     "Caso esperado: PERRO")

    ("Gato"
     ((hace-sonido maullido)
      (tiene-pelo si)
      (tamaño pequeño)
      (es-domestico si)
      (vive-en-tierra si)
      (tiene-patas si)
      (numero-patas 4)
      (es-carnivoro si))
     "Caso esperado: GATO")

    ("Pez"
     ((vive-en-agua si)
      (tiene-escamas si)
      (tiene-aletas si)
      (puede-nadar si)
      (tiene-branquias si)
      (tiene-patas no)
      (pone-huevos si))
     "Caso esperado: PEZ")

    ("Inconsistente"
     ((hace-sonido relincha)
      (vive-en-agua si)
      (tiene-escamas si))
     "Caso inconsistente (debe penalizar o descartar)")

    ("No identificado"
     ((hace-sonido trompeta)
      (tamaño pequeño)
      (vive-en-agua no)
      (tiene-plumas si))
     "Caso probablemente no identificable")
    )
  "Escenarios de prueba para el sistema experto")

(defun ejecutar-escenario (nombre hechos &optional nota)
  "Ejecuta un escenario de prueba con los hechos dados"
  (setf *hechos* hechos)
  (format t "~%~%=== PRUEBA AUTOMATIZADA - ~a ===~%" (string-upcase nombre))
  (when nota
    (format t "~a~%" nota))
  (format t "~%Características ingresadas:~%")
  (dolist (hecho *hechos*)
    (format t "  • ~a: ~a~%" (car hecho) (cadr hecho)))
  (format t "~%Analizando...~%~%")
  (let* ((candidatos (identificar-animales))
         (mejor (obtener-mejor-candidato)))
    (format t "=== RESULTADO ===~%~%")
    (if candidatos
        (progn
          (format t "Candidatos encontrados: ~d~%~%" (length candidatos))
          (let ((posicion 1))
            (dolist (candidato candidatos)
              (format t "  ~d. ~a: ~,2f%% ~a~%"
                      posicion
                      (string-upcase (car candidato))
                      (cadr candidato)
                      (simbolo-confianza (cadr candidato)))
              (incf posicion))))
        (format t "No se encontraron candidatos.~%"))
    (if mejor
        (progn
          (format t "~%Animal identificado: ~a~%" (string-upcase (car mejor)))
          (format t "Porcentaje de coincidencia: ~,2f%%~%" (cadr mejor))
          (format t "Confiabilidad: ~a~%" (grado-confiabilidad (cadr mejor)))
          (format t "Símbolo: ~a~%" (simbolo-confianza (cadr mejor))))
        (format t "~%No se pudo identificar ningún animal.~%"))
    (format t "~%============================================~%")))

(defun seleccionar-escenario-aleatorio ()
  "Selecciona un escenario aleatorio de *escenarios-prueba*"
  (let* ((total (length *escenarios-prueba*))
         (indice (random total)))
    (nth indice *escenarios-prueba*)))

;; Semilla aleatoria
(setf *random-state* (make-random-state t))

;; Ejecutar 4 pruebas deterministas (caballo + 3 animales)
(dolist (escenario (subseq *escenarios-prueba* 0 4))
  (destructuring-bind (nombre hechos nota) escenario
    (ejecutar-escenario nombre hechos nota)))

;; Ejecutar 1 prueba aleatoria (puede incluir inconsistencia o no identificado)
(let ((escenario (seleccionar-escenario-aleatorio)))
  (destructuring-bind (nombre hechos nota) escenario
    (format t "~%~%=== PRUEBA ALEATORIA ===~%")
    (ejecutar-escenario nombre hechos nota)))

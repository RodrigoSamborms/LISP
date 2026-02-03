;; Script de prueba interactiva simulada
(load "/mnt/c/Users/sambo/Documents/GitHub/LISP/sistemaExperto_Ejemplo/versiones/sistema_experto_V6.lisp")

(format t "~%============================================~%")
(format t "  PRUEBA INTERACTIVA - CABALLO~%")
(format t "============================================~%")
(format t "~%Simulando respuestas del usuario para identificar un CABALLO~%~%")

;; Simular respuestas interactivas
(setf *hechos* nil)

;; Pregunta 1: ¿Qué sonido hace?
(format t "1. ¿Qué sonido hace? (ej: ladrido, maullido, relincha, rugido, etc.)~%")
(format t "   > relincha~%")
(agregar-hecho '(hace-sonido relincha))
(let ((mejor (obtener-mejor-candidato 50)))
  (when mejor
    (format t "   → Candidato probable: ~a (~,1f%%)~%~%" 
            (string-upcase (car mejor))
            (cadr mejor))))

;; Pregunta 2: ¿Tiene pelo?
(format t "2. ¿Tiene pelo? (si/no)~%")
(format t "   > si~%")
(agregar-hecho '(tiene-pelo si))
(let ((mejor (obtener-mejor-candidato 50)))
  (when mejor
    (format t "   → Candidato probable: ~a (~,1f%%)~%~%" 
            (string-upcase (car mejor))
            (cadr mejor))))

;; Pregunta 3: ¿Cuál es su tamaño?
(format t "3. ¿Cuál es su tamaño? (pequeño/mediano/grande)~%")
(format t "   > grande~%")
(agregar-hecho '(tamaño grande))
(let ((mejor (obtener-mejor-candidato 50)))
  (when mejor
    (format t "   → Candidato probable: ~a (~,1f%%)~%~%" 
            (string-upcase (car mejor))
            (cadr mejor))))

;; Pregunta 4: ¿Es doméstico?
(format t "4. ¿Es doméstico? (si/no)~%")
(format t "   > si~%")
(agregar-hecho '(es-domestico si))
(let ((mejor (obtener-mejor-candidato 50)))
  (when mejor
    (format t "   → Candidato probable: ~a (~,1f%%)~%~%" 
            (string-upcase (car mejor))
            (cadr mejor))))

;; Pregunta 5: ¿Vive en tierra?
(format t "5. ¿Vive en tierra? (si/no)~%")
(format t "   > si~%")
(agregar-hecho '(vive-en-tierra si))
(let ((mejor (obtener-mejor-candidato 50)))
  (when mejor
    (format t "   → Candidato probable: ~a (~,1f%%)~%~%" 
            (string-upcase (car mejor))
            (cadr mejor))))

;; Pregunta 6: ¿Tiene patas?
(format t "6. ¿Tiene patas? (si/no)~%")
(format t "   > si~%")
(agregar-hecho '(tiene-patas si))
(let ((mejor (obtener-mejor-candidato 50)))
  (when mejor
    (format t "   → Candidato probable: ~a (~,1f%%)~%~%" 
            (string-upcase (car mejor))
            (cadr mejor))))

;; Pregunta 7: ¿Cuántas patas?
(format t "7. ¿Cuántas patas? (2/4)~%")
(format t "   > 4~%")
(agregar-hecho '(numero-patas 4))
(let ((mejor (obtener-mejor-candidato 50)))
  (when mejor
    (format t "   → Candidato probable: ~a (~,1f%%)~%~%" 
            (string-upcase (car mejor))
            (cadr mejor))))

;; Pregunta 8: ¿Es herbívoro?
(format t "8. ¿Es herbívoro? (si/no)~%")
(format t "   > si~%")
(agregar-hecho '(es-herbivoro si))
(let ((mejor (obtener-mejor-candidato 50)))
  (when mejor
    (format t "   → Candidato probable: ~a (~,1f%%)~%~%" 
            (string-upcase (car mejor))
            (cadr mejor))))

;; Resultado final
(format t "~%~%============================================~%")
(let* ((mejor-candidato (obtener-mejor-candidato))
       (candidatos-70 (obtener-candidatos-por-umbral 70.0)))
  
  (cond
    (candidatos-70
     (let ((mejor (car candidatos-70)))
       (format t "  ~a~%" (determinar-nivel-confianza (cadr mejor)))
       (format t "============================================~%")
       (explicar-resultado (car mejor) (cadr mejor))
       (when (> (length candidatos-70) 1)
         (format t "~%Otros candidatos compatibles (≥ 70%%):~%")
         (dolist (c (cdr candidatos-70))
           (format t "  • ~a (~,2f%%)~%" (string-upcase (car c)) (cadr c))))
       (mostrar-candidatos-ordenados)))
    
    (mejor-candidato
     (format t "  IDENTIFICACIÓN INCIERTA~%")
     (format t "============================================~%")
     (format t "~%No hay suficiente coincidencia (~,2f%%).~%"
             (cadr mejor-candidato)))
    
    (t
     (format t "  NO SE PUDO IDENTIFICAR~%")
     (format t "============================================~%"))))

(format t "~%============================================~%")

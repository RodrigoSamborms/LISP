;; ====================================================================
;; SISTEMA EXPERTO V6 PARA IDENTIFICACIÓN DE ANIMALES
;; ====================================================================
;; Versión mejorada con:
;; - Interfaz de preguntas ordenadas y claras
;; - Solo hace preguntas relevantes (criba)
;; - Mejor experiencia de usuario
;; - Ponderación de características
;; - Detección de incompatibilidades
;; 
;; Características principales:
;; 1. Secuencia ordenada de preguntas
;; 2. Base de conocimientos ponderada
;; 3. Motor de inferencia ponderado
;; 4. Detector de incompatibilidades
;; ====================================================================

(defvar *hechos* nil
  "Lista de hechos conocidos durante la consulta actual")

;; Matriz de incompatibilidades
(defvar *incompatibilidades*
  '(
    (hace-sonido relincha vive-en-agua si)
    (hace-sonido relincha tiene-escamas si)
    (hace-sonido relincha puede-nadar si)
    (hace-sonido ladrido vive-en-agua si)
    (hace-sonido maullido vive-en-agua si)
    (hace-sonido trompeta vive-en-agua si)
    (tiene-plumas si vive-en-agua si)
    (vive-en-agua si tiene-pelo si)
    (es-reptil si tiene-pelo si)
    (es-reptil si tiene-plumas si)
    (es-anfibio si tiene-pelo si)
    )
  "Lista de características incompatibles")

;; Base de conocimientos con pesos
(defvar *base-conocimientos*
  '(
    (perro 
      ((hace-sonido ladrido 1.0)
       (tiene-pelo si 0.8)
       (tamaño mediano 0.6)
       (es-domestico si 0.7)
       (vive-en-tierra si 0.8)
       (tiene-patas si 0.7)
       (es-carnivoro si 0.5)
       (numero-patas 4 0.6)))
    
    (gato
      ((hace-sonido maullido 1.0)
       (tiene-pelo si 0.9)
       (tamaño pequeño 0.8)
       (es-domestico si 0.7)
       (vive-en-tierra si 0.9)
       (tiene-patas si 0.8)
       (es-carnivoro si 0.7)
       (numero-patas 4 0.6)))
    
    (caballo
      ((hace-sonido relincha 1.0)
       (tiene-pelo si 0.9)
       (tamaño grande 0.9)
       (es-domestico si 0.8)
       (vive-en-tierra si 0.9)
       (tiene-patas si 0.9)
       (numero-patas 4 0.8)
       (es-herbivoro si 0.7)))
    
    (elefante
      ((tiene-trompa si 1.0)
       (tiene-pelo no 0.8)
       (tamaño grande 0.95)
       (hace-sonido trompeta 0.9)
       (vive-en-tierra si 0.9)
       (tiene-patas si 0.8)
       (numero-patas 4 0.7)
       (es-herbivoro si 0.6)))
    
    (leon
      ((hace-sonido rugido 1.0)
       (tiene-pelo si 0.9)
       (tamaño grande 0.95)
       (es-carnivoro si 0.9)
       (vive-en-tierra si 0.8)
       (tiene-patas si 0.8)
       (numero-patas 4 0.7)
       (habilidad-caza si 0.8)))

    (tigre
      ((hace-sonido rugido 0.9)
       (tiene-pelo si 0.9)
       (tamaño grande 0.9)
       (es-carnivoro si 0.9)
       (vive-en-tierra si 0.8)
       (tiene-patas si 0.8)
       (numero-patas 4 0.7)
       (habilidad-caza si 0.9)
       (es-domestico no 0.6)))

    (lobo
      ((hace-sonido aulla 1.0)
       (tiene-pelo si 0.9)
       (tamaño mediano 0.7)
       (es-carnivoro si 0.8)
       (vive-en-tierra si 0.8)
       (tiene-patas si 0.8)
       (numero-patas 4 0.7)
       (habilidad-caza si 0.8)
       (es-domestico no 0.7)))

    (oso
      ((hace-sonido grunido 0.8)
       (tiene-pelo si 0.9)
       (tamaño grande 0.9)
       (vive-en-tierra si 0.8)
       (tiene-patas si 0.8)
       (numero-patas 4 0.7)
       (es-carnivoro si 0.7)))

    (vaca
      ((hace-sonido muuu 1.0)
       (tiene-pelo si 0.8)
       (tamaño grande 0.9)
       (es-domestico si 0.9)
       (vive-en-tierra si 0.9)
       (tiene-patas si 0.8)
       (numero-patas 4 0.7)
       (es-herbivoro si 0.8)))

    (oveja
      ((hace-sonido beee 1.0)
       (tiene-pelo si 0.8)
       (tamaño mediano 0.7)
       (es-domestico si 0.9)
       (vive-en-tierra si 0.8)
       (tiene-patas si 0.8)
       (numero-patas 4 0.7)
       (es-herbivoro si 0.8)))

    (cerdo
      ((hace-sonido oink 1.0)
       (tiene-pelo si 0.6)
       (tamaño mediano 0.7)
       (es-domestico si 0.9)
       (vive-en-tierra si 0.8)
       (tiene-patas si 0.8)
       (numero-patas 4 0.7)
       (es-herbivoro si 0.6)))
    
    (pajaro
      ((tiene-plumas si 1.0)
       (puede-volar si 0.9)
       (tamaño pequeño 0.8)
       (tiene-alas si 0.95)
       (vive-en-tierra si 0.7)
       (tiene-patas si 0.6)
       (numero-patas 2 0.8)
       (pone-huevos si 0.7)))

    (gallina
      ((hace-sonido cacarea 0.9)
       (tiene-plumas si 1.0)
       (puede-volar no 0.8)
       (tamaño pequeño 0.7)
       (tiene-alas si 0.8)
       (vive-en-tierra si 0.8)
       (tiene-patas si 0.7)
       (numero-patas 2 0.8)
       (pone-huevos si 0.9)))
    
    (pez
      ((vive-en-agua si 1.0)
       (tiene-escamas si 0.95)
       (tiene-aletas si 0.95)
       (puede-nadar si 0.9)
       (puede-volar no 1.0)
       (tiene-branquias si 0.9)
       (tiene-patas no 0.8)
       (pone-huevos si 0.7)))

    (tiburon
      ((vive-en-agua si 1.0)
       (tiene-escamas si 0.9)
       (tiene-aletas si 0.95)
       (puede-nadar si 0.9)
       (tiene-branquias si 0.9)
       (tiene-patas no 0.8)
       (es-carnivoro si 0.8)))

    (delfin
      ((vive-en-agua si 1.0)
       (tiene-aletas si 0.9)
       (puede-nadar si 0.9)
       (tiene-pelo no 0.7)
       (tiene-patas no 0.8)
       (tamaño mediano 0.6)
       (hace-sonido chillido 0.7)))
    
    (serpiente
      ((tiene-pelo no 1.0)
       (es-reptil si 0.95)
       (tamaño mediano 0.6)
       (tiene-patas no 1.0)
       (vive-en-tierra si 0.8)
       (es-carnivoro si 0.7)
       (tiene-escamas si 0.9)
       (puede-nadar si 0.6)))

    (cocodrilo
      ((es-reptil si 0.9)
       (vive-en-agua si 0.8)
       (tiene-escamas si 0.9)
       (tiene-patas si 0.8)
       (numero-patas 4 0.7)
       (tamaño grande 0.8)
       (puede-nadar si 0.8)
       (es-carnivoro si 0.8)))

    (tortuga
      ((es-reptil si 0.9)
       (vive-en-agua si 0.7)
       (tiene-escamas si 0.8)
       (tiene-patas si 0.8)
       (numero-patas 4 0.7)
       (tamaño pequeño 0.6)
       (puede-nadar si 0.7)
       (es-herbivoro si 0.5)))
    
    (rana
      ((es-anfibio si 1.0)
       (vive-en-agua si 0.9)
       (tamaño pequeño 0.8)
       (puede-saltar si 0.95)
       (tiene-patas si 0.8)
       (numero-patas 4 0.7)
       (puede-nadar si 0.8)
       (pone-huevos si 0.7)))
    
    (raton
      ((hace-sonido chillido 0.9)
       (tiene-pelo si 0.9)
       (tamaño pequeño 1.0)
       (es-domestico si 0.6)
       (vive-en-tierra si 0.8)
       (tiene-patas si 0.7)
       (numero-patas 4 0.7)
       (tiene-cola si 0.8)
       (es-nocturno si 0.1)))
    ))

;; ====================================================================
;; ORDEN DE PREGUNTAS (CRIBA)
;; ====================================================================

(defvar *orden-preguntas*
  '(
    (hace-sonido "¿Qué sonido hace?")
    (tiene-pelo "¿Tiene pelo?")
    (tamaño "¿Cuál es su tamaño?")
    (es-domestico "¿Es doméstico?")
    (vive-en-tierra "¿Vive en tierra?")
    (tiene-patas "¿Tiene patas?")
    (numero-patas "¿Cuántas patas?")
    (es-herbivoro "¿Es herbívoro?")
    )
  "Orden de preguntas a realizar (criba)")

;; ====================================================================
;; FUNCIONES AUXILIARES
;; ====================================================================

(defun agregar-hecho (hecho)
  "Agrega un nuevo hecho a la base de hechos conocidos"
  (push hecho *hechos*))

(defun verificar-incompatibilidades (caracteristica valor)
  "Verifica si una característica es incompatible con los hechos actuales"
  (let ((incompatible nil))
    (dolist (incompatibilidad *incompatibilidades*)
      (destructuring-bind (car1 val1 car2 val2) incompatibilidad
        (when (and (equal caracteristica car1)
                   (equal valor val1)
                   (member (list car2 val2) *hechos* :test #'equal))
          (setf incompatible t))
        (when (and (equal caracteristica car2)
                   (equal valor val2)
                   (member (list car1 val1) *hechos* :test #'equal))
          (setf incompatible t))))
    incompatible))

(defun verificar-incompatibilidades-hechos (hechos)
  "Verifica si hay incompatibilidades en los hechos proporcionados"
  (let ((hay-incompatibilidad nil))
    (dolist (inc *incompatibilidades*)
      (let ((car1 (first inc))
            (val1 (second inc))
            (car2 (third inc))
            (val2 (fourth inc))
            (tiene-car1 nil)
            (tiene-car2 nil))
        (dolist (hecho hechos)
          (when (equal (first hecho) car1)
            (when (equal (second hecho) val1)
              (setf tiene-car1 t))))
        (dolist (hecho hechos)
          (when (equal (first hecho) car2)
            (when (equal (second hecho) val2)
              (setf tiene-car2 t))))
        (when (and tiene-car1 tiene-car2)
          (setf hay-incompatibilidad t))))
    hay-incompatibilidad))

(defun calcular-porcentaje-ponderado (animal hechos)
  "Calcula el porcentaje de coincidencia ponderado con penalización"
  (let ((caracteristicas-ponderadas (cadr animal))
        (peso-total 0.0)
        (peso-coincidencias 0.0)
        (penalizacion 0.0))
    
    (when (verificar-incompatibilidades-hechos hechos)
      (return-from calcular-porcentaje-ponderado 0.0))
    
    ;; Calcular coincidencias y penalizaciones
    (dolist (caracteristica caracteristicas-ponderadas)
      (let ((nombre (first caracteristica))
            (valor (second caracteristica))
            (peso (third caracteristica)))
        (let ((hay-hecho nil))
          (dolist (hecho hechos)
            (when (equal nombre (car hecho))
              (setf hay-hecho t)
              (incf peso-total peso)
              (when (equal valor (cadr hecho))
                (incf peso-coincidencias peso))
              ;; Penalizar si no coincide
              (unless (equal valor (cadr hecho))
                ;; Penalizar más si es un peso alto (característica importante)
                (incf penalizacion (* peso 0.5)))))
          ;; Penalizar características importantes que no se proporcionan
          (unless hay-hecho
            (when (>= peso 0.8)
              (incf penalizacion (* peso 0.3)))))))
    
    (when (= peso-total 0.0)
      (return-from calcular-porcentaje-ponderado 0.0))
    
    (let ((porcentaje (- (* (/ peso-coincidencias peso-total) 100.0) penalizacion)))
      (max 0.0 porcentaje))))

(defun identificar-animales ()
  "Identifica animales y retorna lista con porcentajes ponderados"
  (let ((resultados nil))
    (dolist (animal *base-conocimientos*)
      (let ((porcentaje (calcular-porcentaje-ponderado animal *hechos*)))
        (when (> porcentaje 0)
          (push (list (car animal) porcentaje) resultados))))
    (sort resultados #'> :key #'cadr)))

(defun obtener-mejor-candidato (&optional (umbral-minimo 0.0))
  "Retorna el animal con mayor porcentaje ponderado si supera el umbral"
  (let ((candidatos (identificar-animales)))
    (when candidatos
      (let ((mejor (car candidatos)))
        (when (>= (cadr mejor) umbral-minimo)
          mejor)))))

(defun obtener-candidatos-por-umbral (umbral)
  "Retorna todos los animales que superan el umbral especificado"
  (let ((candidatos (identificar-animales))
        (resultado nil))
    (dolist (candidato candidatos)
      (when (>= (cadr candidato) umbral)
        (push candidato resultado)))
    (nreverse resultado)))

(defun determinar-nivel-confianza (porcentaje)
  "Retorna el nivel de confianza basado en el porcentaje"
  (cond
    ((>= porcentaje 100) "IDENTIFICACIÓN EXACTA")
    ((>= porcentaje 90) "IDENTIFICACIÓN MUY PROBABLE")
    ((>= porcentaje 80) "IDENTIFICACIÓN PROBABLE")
    ((>= porcentaje 70) "IDENTIFICACIÓN POSIBLE")
    (t "IDENTIFICACIÓN INCIERTA")))

(defun simbolo-confianza (porcentaje)
  "Retorna un símbolo representativo del nivel de confianza"
  (cond
    ((>= porcentaje 100) "✓✓✓")
    ((>= porcentaje 90) "✓✓")
    ((>= porcentaje 80) "✓")
    ((>= porcentaje 70) "◐")
    (t "○")))

(defun grado-confiabilidad (porcentaje)
  "Retorna un grado educativo de confiabilidad"
  (cond
    ((>= porcentaje 100) "EXCELENTE")
    ((>= porcentaje 98) "BUENO")
    ((>= porcentaje 90) "REGULAR")
    ((>= porcentaje 85) "MALO")
    ((>= porcentaje 80) "ACEPTABLE")
    (t "INSUFICIENTE")))

;; ====================================================================
;; INTERFAZ DE USUARIO
;; ====================================================================

(defun hacer-pregunta-criba (numero pregunta caracteristica)
  "Hace una pregunta numerada de la criba"
  (format t "~%~d. ~a " numero pregunta)
  (case caracteristica
    (hace-sonido (format t "(ej: ladrido, maullido, relincha, rugido, etc.)"))
    (tiene-pelo (format t "(si/no)"))
    (tamaño (format t "(pequeño/mediano/grande)"))
    (es-domestico (format t "(si/no)"))
    (vive-en-tierra (format t "(si/no)"))
    (tiene-patas (format t "(si/no)"))
    (numero-patas (format t "(2/4)"))
    (es-herbivoro (format t "(si/no)")))
  (format t "~%   > ")
  (force-output)
  (read))

(defun mostrar-candidatos-ordenados ()
  "Muestra candidatos ordenados por porcentaje"
  (let ((candidatos (identificar-animales)))
    (if candidatos
        (progn
          (format t "~%=== CANDIDATOS ENCONTRADOS ===~%")
          (let ((posicion 1))
            (dolist (candidato candidatos)
              (format t "  ~d. ~a - ~,2f%% ~a~%"
                      posicion
                      (string-upcase (car candidato))
                      (cadr candidato)
                      (simbolo-confianza (cadr candidato)))
              (incf posicion))))
        (format t "~%No se encontraron candidatos compatibles.~%"))))

(defun explicar-resultado (animal porcentaje)
  "Explica el resultado de la identificación"
  (format t "~%~%=== RESULTADO FINAL ===~%")
  (format t "Animal identificado: ~a~%" (string-upcase animal))
  (format t "Porcentaje de coincidencia: ~,2f%%~%" porcentaje)
  (format t "Confiabilidad: ~a ~a~%" 
          (simbolo-confianza porcentaje)
          (grado-confiabilidad porcentaje))
  (format t "~%Características recopiladas:~%")
  (if *hechos*
      (dolist (hecho *hechos*)
        (format t "  • ~a: ~a~%" (car hecho) (cadr hecho)))
      (format t "  (Ninguna)~%")))

;; ====================================================================
;; SISTEMA PRINCIPAL
;; ====================================================================

(defun sistema-experto ()
  "Sistema experto mejorado con criba ordenada"
  (format t "~%============================================~%")
  (format t "  SISTEMA EXPERTO V6 - IDENTIFICACIÓN~%")
  (format t "     DE ANIMALES (CRIBA ORDENADA)~%")
  (format t "============================================~%")
  
  (setf *hechos* nil)
  
  (let ((numero-pregunta 1))
    ;; Ciclo de preguntas ordenadas
    (dolist (pregunta-info *orden-preguntas*)
      (destructuring-bind (caracteristica texto-pregunta) pregunta-info
        (let ((respuesta (hacer-pregunta-criba numero-pregunta texto-pregunta caracteristica)))
          (unless (equal respuesta 'no-se)
            ;; Verificar incompatibilidades
            (if (verificar-incompatibilidades caracteristica respuesta)
                (progn
                  (format t "~%⚠ ADVERTENCIA: '~a' es incompatible con anteriores.~%" respuesta)
                  (format t "Se ignora esta respuesta.~%"))
                (progn
                  (agregar-hecho (list caracteristica respuesta))
                  ;; Mostrar candidato probable
                  (let ((mejor (obtener-mejor-candidato 50)))
                    (when mejor
                      (format t "   → Candidato probable: ~a (~,1f%%)~%" 
                              (string-upcase (car mejor))
                              (cadr mejor)))))))
          (incf numero-pregunta)))))
  
  ;; Mostrar resultado final
  (format t "~%~%============================================~%")
  (let* ((mejor-candidato (obtener-mejor-candidato))
         (candidatos-70 (obtener-candidatos-por-umbral 70.0)))
    
    (cond
      ;; Si hay candidatos ≥ 70%
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
      
      ;; Si hay candidatos pero < 70%
      (mejor-candidato
       (format t "  IDENTIFICACIÓN INCIERTA~%")
       (format t "============================================~%")
       (format t "~%No hay suficiente coincidencia (~,2f%%).~%"
               (cadr mejor-candidato))
       (format t "Se necesita al menos 70%% de coincidencia.~%")
       (mostrar-candidatos-ordenados))
      
      ;; Sin candidatos
      (t
       (format t "  NO SE PUDO IDENTIFICAR~%")
       (format t "============================================~%")
       (format t "~%Lo siento, no pude identificar el animal.~%")
       (format t "~%Características recopiladas:~%")
       (if *hechos*
           (dolist (hecho *hechos*)
             (format t "  • ~a: ~a~%" (car hecho) (cadr hecho)))
           (format t "  (Ninguna)~%")))))
  
  (format t "~%============================================~%")
  (format t "~%Saliendo del sistema...~%")
  (quit))

;; ====================================================================
;; EJECUCIÓN
;; ====================================================================

(format t "~%Sistema Experto V6 cargado correctamente.~%")
(format t "Para iniciar, escribe: (sistema-experto)~%~%")

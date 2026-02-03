;; ====================================================================
;; SISTEMA EXPERTO PARA IDENTIFICACIÓN DE ANIMALES
;; ====================================================================
;; Este sistema experto identifica animales mediante un proceso de
;; inferencia basado en preguntas al usuario.
;;
;; Componentes principales:
;; 1. Base de conocimientos (hechos y reglas)
;; 2. Motor de inferencia (procesamiento de reglas)
;; 3. Interfaz de usuario (interacción)
;; ====================================================================

;; --------------------------------------------------------------------
;; 1. BASE DE CONOCIMIENTOS
;; --------------------------------------------------------------------

;; Base de hechos dinámicos (se van agregando durante la consulta)
(defvar *hechos* nil
  "Lista de hechos conocidos durante la consulta actual")

;; Base de conocimientos de animales con sus características
(defvar *base-conocimientos*
  '(
    ;; (animal características-requeridas)
    (perro (tiene-pelo si) (tamaño mediano) (hace-sonido ladrido) (es-domestico si))
    (gato (tiene-pelo si) (tamaño pequeño) (hace-sonido maullido) (es-domestico si))
    (elefante (tiene-pelo no) (tamaño grande) (hace-sonido trompeta) (tiene-trompa si))
    (leon (tiene-pelo si) (tamaño grande) (hace-sonido rugido) (es-carnivoro si))
    (pajaro (tiene-plumas si) (tamaño pequeño) (puede-volar si) (tiene-alas si))
    (pez (vive-en-agua si) (tiene-escamas si) (puede-volar no) (tiene-aletas si))
    (serpiente (tiene-pelo no) (tamaño mediano) (tiene-patas no) (es-reptil si))
    (rana (vive-en-agua si) (tamaño pequeño) (puede-saltar si) (es-anfibio si))
    (caballo (tiene-pelo si) (tamaño grande) (hace-sonido relincho) (es-domestico si))
    (raton (tiene-pelo si) (tamaño pequeño) (hace-sonido chillido) (tiene-cola si))
    ))

;; --------------------------------------------------------------------
;; 2. MOTOR DE INFERENCIA
;; --------------------------------------------------------------------

;; Función para agregar un hecho a la base de hechos
(defun agregar-hecho (hecho)
  "Agrega un nuevo hecho a la base de hechos conocidos"
  (push hecho *hechos*))

;; Función para verificar si un hecho existe en la base de hechos
(defun existe-hecho (caracteristica valor)
  "Verifica si existe un hecho con la característica y valor dados"
  (member (list caracteristica valor) *hechos* :test #'equal))

;; Función para obtener todas las características únicas
(defun obtener-caracteristicas ()
  "Extrae todas las características únicas de la base de conocimientos"
  (let ((caracteristicas nil))
    (dolist (animal *base-conocimientos*)
      (dolist (caracteristica (cdr animal))
        (let ((nombre-car (car caracteristica)))
          (unless (member nombre-car caracteristicas)
            (push nombre-car caracteristicas)))))
    (nreverse caracteristicas)))

;; Función para verificar si un animal coincide con los hechos conocidos
(defun coincide-animal (animal hechos)
  "Verifica si todas las características del animal coinciden con los hechos"
  (let ((caracteristicas (cdr animal)))
    (every #'(lambda (car)
               (member car hechos :test #'equal))
           caracteristicas)))

;; Función para calcular porcentaje de coincidencia
(defun calcular-porcentaje-coincidencia (animal hechos)
  "Calcula el porcentaje de coincidencia entre los hechos y las características del animal"
  (let* ((caracteristicas-animal (cdr animal))
         (total-caracteristicas (length caracteristicas-animal))
         (coincidencias 0)
         (contradicciones 0))
    
    ;; Contar coincidencias y contradicciones
    (dolist (caracteristica caracteristicas-animal)
      (cond
        ;; Si el hecho coincide exactamente, es una coincidencia
        ((member caracteristica hechos :test #'equal)
         (incf coincidencias))
        ;; Si existe un hecho con la misma característica pero diferente valor, es contradicción
        ((find (car caracteristica) hechos :key #'car)
         (incf contradicciones))))
    
    ;; Si hay contradicciones, el porcentaje se reduce
    (if (> contradicciones 0)
        0.0  ; Cualquier contradicción descalifica al animal
        (if (> total-caracteristicas 0)
            (* (/ coincidencias total-caracteristicas) 100.0)
            0.0))))

;; Función para obtener animales con porcentajes de coincidencia
(defun identificar-animales-con-porcentaje ()
  "Identifica animales y retorna lista ordenada con porcentajes de coincidencia"
  (let ((resultados nil))
    ;; Calcular porcentaje para cada animal
    (dolist (animal *base-conocimientos*)
      (let ((porcentaje (calcular-porcentaje-coincidencia animal *hechos*)))
        (when (> porcentaje 0)
          (push (list (car animal) porcentaje) resultados))))
    ;; Ordenar por porcentaje descendente
    (sort resultados #'> :key #'cadr)))

;; Función principal de inferencia (actualizada con porcentaje)
(defun identificar-animal ()
  "Motor de inferencia que identifica el animal basándose en los hechos"
  (dolist (animal *base-conocimientos*)
    (when (coincide-animal animal *hechos*)
      (return-from identificar-animal (car animal))))
  nil)

;; Función para obtener el mejor candidato con umbral mínimo
(defun obtener-mejor-candidato (&optional (umbral-minimo 0.0))
  "Retorna el animal con mayor porcentaje de coincidencia si supera el umbral mínimo"
  (let ((candidatos (identificar-animales-con-porcentaje)))
    (when candidatos
      (let ((mejor (car candidatos)))
        (when (>= (cadr mejor) umbral-minimo)
          mejor)))))

;; Función para obtener los mejores candidatos por encima de un umbral
(defun obtener-candidatos-por-umbral (umbral)
  "Retorna todos los animales que superan el umbral especificado"
  (let ((candidatos (identificar-animales-con-porcentaje))
        (resultado nil))
    (dolist (candidato candidatos)
      (when (>= (cadr candidato) umbral)
        (push candidato resultado)))
    (nreverse resultado)))

;; Función para obtener animales posibles basándose en hechos parciales
(defun animales-posibles ()
  "Retorna lista de animales que aún son posibles con los hechos actuales"
  (let ((posibles nil))
    (dolist (animal *base-conocimientos*)
      (let ((caracteristicas (cdr animal))
            (es-posible t))
        ;; Verificar si algún hecho contradice este animal
        (dolist (hecho *hechos*)
          (when (and (member (car hecho) (mapcar #'car caracteristicas))
                     (not (member hecho caracteristicas :test #'equal)))
            (setf es-posible nil)
            (return)))
        (when es-posible
          (push (car animal) posibles))))
    (nreverse posibles)))

;; --------------------------------------------------------------------
;; 3. INTERFAZ DE USUARIO
;; --------------------------------------------------------------------

;; Función para hacer una pregunta al usuario
(defun hacer-pregunta (caracteristica)
  "Hace una pregunta al usuario sobre una característica"
  (format t "~%¿~a? (opciones posibles): " (formatear-pregunta caracteristica))
  (format t "~%  Responde con el valor apropiado o 'no-se' si no sabes: ")
  (force-output)
  (read))

;; Función para formatear el nombre de una característica en pregunta
(defun formatear-pregunta (caracteristica)
  "Convierte el nombre de una característica en una pregunta legible"
  (case caracteristica
    (tiene-pelo "El animal tiene pelo")
    (tiene-plumas "El animal tiene plumas")
    (tiene-escamas "El animal tiene escamas")
    (tiene-patas "El animal tiene patas")
    (tiene-aletas "El animal tiene aletas")
    (tiene-alas "El animal tiene alas")
    (tiene-trompa "El animal tiene trompa")
    (tiene-cola "El animal tiene cola")
    (tamaño "El tamaño del animal es (pequeño/mediano/grande)")
    (hace-sonido "Qué sonido hace el animal (ladrido/maullido/trompeta/rugido/relincho/chillido)")
    (vive-en-agua "El animal vive en el agua")
    (puede-volar "El animal puede volar")
    (puede-saltar "El animal puede saltar")
    (es-domestico "El animal es doméstico")
    (es-carnivoro "El animal es carnívoro")
    (es-reptil "El animal es un reptil")
    (es-anfibio "El animal es un anfibio")
    (t (format nil "~a" caracteristica))))

;; Función para explicar el razonamiento con porcentaje
(defun explicar-razonamiento (animal &optional porcentaje)
  "Explica por qué se llegó a esta conclusión"
  (format t "~%~%=== EXPLICACIÓN DEL RAZONAMIENTO ===~%")
  (format t "Se identificó al animal como: ~a~%" (string-upcase animal))
  
  ;; Mostrar porcentaje de coincidencia si está disponible
  (when porcentaje
    (format t "Porcentaje de coincidencia: ~,2f%~%" porcentaje))
  
  (format t "~%Basándose en los siguientes hechos:~%")
  (dolist (hecho *hechos*)
    (format t "  - ~a: ~a~%" (car hecho) (cadr hecho)))
  
  ;; Mostrar las características del animal en la base de conocimientos
  (let ((info-animal (assoc animal *base-conocimientos*)))
    (when info-animal
      (format t "~%Características del ~a en la base de conocimientos:~%" animal)
      (dolist (car (cdr info-animal))
        (format t "  - ~a: ~a~%" (car car) (cadr car))))))

;; Función para mostrar todos los candidatos con porcentajes
(defun mostrar-candidatos-con-porcentajes ()
  "Muestra todos los animales candidatos con sus porcentajes de coincidencia"
  (let ((candidatos (identificar-animales-con-porcentaje)))
    (if candidatos
        (progn
          (format t "~%=== CANDIDATOS IDENTIFICADOS ===~%")
          (format t "~%Animales ordenados por porcentaje de coincidencia:~%~%")
          (let ((posicion 1))
            (dolist (candidato candidatos)
              (format t "  ~d. ~a - ~,2f% ~a~%"
                      posicion
                      (string-upcase (car candidato))
                      (cadr candidato)
                      (simbolo-confianza (cadr candidato)))
              (incf posicion))))
        (format t "~%No se encontraron candidatos compatibles.~%"))))

;; Función para determinar el nivel de confianza
(defun determinar-nivel-confianza (porcentaje)
  "Retorna el nivel de confianza basado en el porcentaje"
  (cond
    ((>= porcentaje 100) "IDENTIFICACIÓN EXACTA")
    ((>= porcentaje 90) "IDENTIFICACIÓN MUY PROBABLE")
    ((>= porcentaje 80) "IDENTIFICACIÓN PROBABLE")
    ((>= porcentaje 70) "IDENTIFICACIÓN POSIBLE")
    (t "IDENTIFICACIÓN INCIERTA")))

;; Función para obtener el símbolo de confianza
(defun simbolo-confianza (porcentaje)
  "Retorna un símbolo representativo del nivel de confianza"
  (cond
    ((>= porcentaje 100) "✓✓✓")
    ((>= porcentaje 90) "✓✓")
    ((>= porcentaje 80) "✓")
    ((>= porcentaje 70) "◐")
    (t "○")))

;; --------------------------------------------------------------------
;; 4. SISTEMA PRINCIPAL
;; --------------------------------------------------------------------

;; Función principal que ejecuta el sistema experto
(defun sistema-experto ()
  "Función principal del sistema experto"
  (format t "~%============================================~%")
  (format t "  SISTEMA EXPERTO DE IDENTIFICACIÓN~%")
  (format t "     DE ANIMALES EN LISP~%")
  (format t "============================================~%")
  (format t "~%Este sistema te ayudará a identificar un animal~%")
  (format t "basándose en sus características.~%")
  (format t "~%Responde las preguntas con valores como:~%")
  (format t "  - si, no (para preguntas de sí/no)~%")
  (format t "  - pequeño, mediano, grande (para tamaño)~%")
  (format t "  - ladrido, maullido, trompeta, etc. (para sonidos)~%")
  (format t "  - no-se (si no conoces la respuesta)~%")
  
  ;; Reiniciar la base de hechos
  (setf *hechos* nil)
  
  ;; Obtener las características a preguntar
  (let ((caracteristicas (obtener-caracteristicas))
        (animal-identificado nil))
    
    ;; Ciclo de preguntas
    (dolist (caracteristica caracteristicas)
      ;; Verificar si ya identificamos al animal
      (let ((animal (identificar-animal)))
        (when animal
          (setf animal-identificado animal)
          (return)))
      
      ;; Si aún hay múltiples animales posibles, seguir preguntando
      (let ((posibles (animales-posibles)))
        (when (> (length posibles) 1)
          (let ((respuesta (hacer-pregunta caracteristica)))
            (unless (equal respuesta 'no-se)
              (agregar-hecho (list caracteristica respuesta)))))))
    
    ;; Intentar identificar el animal final
    (unless animal-identificado
      (setf animal-identificado (identificar-animal)))
    
    ;; Mostrar resultado con porcentajes
    (format t "~%~%============================================~%")
    (if animal-identificado
        (progn
          (format t "  ¡IDENTIFICACIÓN EXITOSA!~%")
          (format t "============================================~%")
          (format t "~%El animal es: ~a (100%% de coincidencia)~%" (string-upcase animal-identificado))
          (explicar-razonamiento animal-identificado 100.0))
        (progn
          ;; Intentar con porcentajes parciales y umbral de 70%
          (let* ((mejor-candidato (obtener-mejor-candidato))
                 (candidatos-70 (obtener-candidatos-por-umbral 70.0)))
            (cond
              ;; Si hay candidatos con 70% o más de coincidencia
              (candidatos-70
               (let ((mejor (car candidatos-70)))
                 (format t "  ~a~%" (determinar-nivel-confianza (cadr mejor)))
                 (format t "============================================~%")
                 (format t "~%El animal identificado es: ~a (~,2f%% de coincidencia)~%"
                         (string-upcase (car mejor))
                         (cadr mejor))
                 (when (> (length candidatos-70) 1)
                   (format t "~%Otros candidatos compatibles (≥ 70%%):~%")
                   (dolist (candidato (cdr candidatos-70))
                     (format t "  - ~a (~,2f%%)~%" 
                             (string-upcase (car candidato))
                             (cadr candidato))))
                 (mostrar-candidatos-con-porcentajes)
                 (explicar-razonamiento (car mejor) (cadr mejor))))
              ;; Si el mejor candidato está entre 50% y 70%
              (mejor-candidato
               (format t "  IDENTIFICACIÓN INCIERTA~%")
               (format t "============================================~%")
               (format t "~%No hay suficiente coincidencia (máximo ~,2f%%).~%"
                       (cadr mejor-candidato))
               (format t "~%Se requiere más información para una identificación fiable.~%")
               (format t "~%Se necesita al menos 70%% de coincidencia.~%")
               (mostrar-candidatos-con-porcentajes))
              ;; Si no hay candidatos o no hay información
              (t
               (format t "  NO SE PUDO IDENTIFICAR~%")
               (format t "============================================~%")
               (format t "~%Lo siento, no pude identificar el animal.~%")
               (format t "~%Hechos recopilados:~%")
               (if *hechos*
                   (dolist (hecho *hechos*)
                     (format t "  - ~a: ~a~%" (car hecho) (cadr hecho)))
                   (format t "  (No hay hechos recopilados)~%")))))))    
    (format t "~%============================================~%")))

;; --------------------------------------------------------------------
;; 5. FUNCIÓN DE DEMOSTRACIÓN (SIN INTERACCIÓN)
;; --------------------------------------------------------------------

;; Función de demostración automática con porcentajes y umbral 70%
(defun demo-automatica ()
  "Ejecuta una demostración automática del sistema con porcentajes y umbral 70%"
  (format t "~%=== DEMOSTRACIÓN AUTOMÁTICA CON PORCENTAJES Y UMBRAL 70%% ===~%~%")
  
  ;; Ejemplo 1: Perro (100% de coincidencia)
  (format t "Ejemplo 1: Identificando un perro (coincidencia completa 100%%)~%")
  (setf *hechos* '((tiene-pelo si) (tamaño mediano) (hace-sonido ladrido) (es-domestico si)))
  (let ((animal (identificar-animal)))
    (if animal
        (format t "Resultado: ~a (100%% de coincidencia)~%~%" animal)
        (progn
          (format t "No hay coincidencia exacta. Candidatos:~%")
          (mostrar-candidatos-con-porcentajes)
          (format t "~%"))))
  
  ;; Ejemplo 2: Elefante (100% de coincidencia)
  (format t "Ejemplo 2: Identificando un elefante (coincidencia completa 100%%)~%")
  (setf *hechos* '((tiene-pelo no) (tamaño grande) (hace-sonido trompeta) (tiene-trompa si)))
  (let ((animal (identificar-animal)))
    (if animal
        (format t "Resultado: ~a (100%% de coincidencia)~%~%" animal)
        (progn
          (format t "No hay coincidencia exacta. Candidatos:~%")
          (mostrar-candidatos-con-porcentajes)
          (format t "~%"))))
  
  ;; Ejemplo 3: Pájaro (100% de coincidencia)
  (format t "Ejemplo 3: Identificando un pájaro (coincidencia completa 100%%)~%")
  (setf *hechos* '((tiene-plumas si) (tamaño pequeño) (puede-volar si) (tiene-alas si)))
  (let ((animal (identificar-animal)))
    (if animal
        (format t "Resultado: ~a (100%% de coincidencia)~%~%" animal)
        (progn
          (format t "No hay coincidencia exacta. Candidatos:~%")
          (mostrar-candidatos-con-porcentajes)
          (format t "~%"))))
  
  ;; Ejemplo 4: Coincidencia parcial (solo algunas características)
  (format t "Ejemplo 4: Identificación parcial (solo 2 características - 50%%)~%")
  (setf *hechos* '((tiene-pelo si) (tamaño grande)))
  (format t "Hechos conocidos: tiene-pelo=si, tamaño=grande~%")
  (let ((animal (identificar-animal)))
    (if animal
        (format t "Resultado: ~a (100%% de coincidencia)~%~%" animal)
        (progn
          (format t "No hay coincidencia exacta. Candidatos parciales:~%")
          (mostrar-candidatos-con-porcentajes)
          (format t "~%"))))
  
  ;; Ejemplo 5: Identificación que supera 70% (éxito con umbral)
  (format t "Ejemplo 5: Identificación que supera el umbral de 70%%~%")
  (setf *hechos* '((tiene-pelo si) (tamaño grande) (hace-sonido relincho)))
  (format t "Hechos conocidos: tiene-pelo=si, tamaño=grande, hace-sonido=relincho~%")
  (format t "Esto debe identificar un CABALLO con ~,2f%% de coincidencia~%" (/ 300 400))
  (let ((animal (identificar-animal)))
    (if animal
        (format t "Resultado: ~a (100%% de coincidencia)~%~%" animal)
        (progn
          (let ((candidatos-70 (obtener-candidatos-por-umbral 70.0)))
            (if candidatos-70
                (progn
                  (format t "Candidatos que superan 70%%:~%")
                  (dolist (c candidatos-70)
                    (format t "  - ~a: ~,2f%%~%" (car c) (cadr c)))
                  (format t "~%"))
                (progn
                  (format t "No hay coincidencia exacta. Todos los candidatos:~%")
                  (mostrar-candidatos-con-porcentajes)
                  (format t "~%")))))))
  
  ;; Ejemplo 6: Contradicción (no existe tal animal)
  (format t "Ejemplo 6: Caso con contradicción (0%% de coincidencia)~%")
  (setf *hechos* '((tiene-pelo si) (tiene-plumas si) (vive-en-agua si)))
  (format t "Hechos contradictorios: pelo + plumas + agua~%")
  (let ((animal (identificar-animal)))
    (if animal
        (format t "Resultado: ~a~%~%" animal)
        (progn
          (format t "No se encontraron candidatos compatibles.~%")
          (mostrar-candidatos-con-porcentajes)
          (format t "~%")))))

;; --------------------------------------------------------------------
;; 6. EJECUCIÓN
;; --------------------------------------------------------------------

;; Para ejecutar el sistema interactivo, descomenta la siguiente línea:
;; (sistema-experto)

;; Para ejecutar la demostración automática:
(demo-automatica)

(format t "~%~%Para ejecutar el sistema interactivo, escribe: (sistema-experto)~%")
(format t "Para ejecutar la demo nuevamente, escribe: (demo-automatica)~%~%")

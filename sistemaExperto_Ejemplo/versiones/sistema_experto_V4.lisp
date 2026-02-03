;; ====================================================================
;; SISTEMA EXPERTO V4 PARA IDENTIFICACIÓN DE ANIMALES
;; ====================================================================
;; Sistema mejorado con:
;; - Más características detalladas por animal
;; - Ponderación/peso de características (algunas más importantes)
;; - Detección de incompatibilidades
;; - Cálculo de coincidencia ponderado
;; - Grado de confiabilidad para resultados ≥ 80%
;;
;; Componentes principales:
;; 1. Base de conocimientos con pesos
;; 2. Motor de inferencia ponderado
;; 3. Detector de incompatibilidades
;; 4. Interfaz mejorada
;; 5. Clasificación educativa de confiabilidad
;; ====================================================================

;; Base de hechos dinámicos (se van agregando durante la consulta)
(defvar *hechos* nil
  "Lista de hechos conocidos durante la consulta actual")

;; Matriz de incompatibilidades entre características
(defvar *incompatibilidades*
  '(
    ;; (característica1 valor1 característica2 valor2)
    (hace-sonido relincha vive-en-agua si)
    (hace-sonido relincha tiene-escamas si)
    (hace-sonido relincha puede-nadar si)
    (hace-sonido ladrido vive-en-agua si)
    (hace-sonido maullido vive-en-agua si)
    (hace-sonido trompeta vive-en-agua si)
    (tiene-plumas si vive-en-agua si)  ;; Las aves no viven en agua profunda
    (vive-en-agua si tiene-pelo si)    ;; Contradicción: mamíferos terrestres no nadan
    (es-reptil si tiene-pelo si)
    (es-reptil si tiene-plumas si)
    (es-anfibio si tiene-pelo si)
    )
  "Lista de características incompatibles")

;; Base de conocimientos con pesos (importancia de cada característica)
;; Formato: (animal ((característica valor peso) ...))
;; Peso: 1.0 = definitorio, 0.8 = muy importante, 0.5 = importante, 0.3 = moderado, 0.1 = leve
(defvar *base-conocimientos*
  '(
    (perro 
      ((hace-sonido ladrido 1.0)        ;; Definitorio
       (tiene-pelo si 0.8)
       (tamaño mediano 0.6)
       (es-domestico si 0.7)
       (vive-en-tierra si 0.8)
       (tiene-patas si 0.7)
       (es-carnivoro si 0.5)
       (puede-nadar si 0.3)
       (numero-patas 4 0.6)))
    
    (gato
      ((hace-sonido maullido 1.0)       ;; Definitorio
       (tiene-pelo si 0.9)
       (tamaño pequeño 0.8)
       (es-domestico si 0.7)
       (vive-en-tierra si 0.9)
       (tiene-patas si 0.8)
       (es-carnivoro si 0.7)
       (numero-patas 4 0.6)))
    
    (caballo
      ((hace-sonido relincha 1.0)       ;; Definitorio - muy característico
       (tiene-pelo si 0.9)
       (tamaño grande 0.9)
       (es-domestico si 0.8)
       (vive-en-tierra si 0.9)
       (tiene-patas si 0.9)
       (numero-patas 4 0.8)
       (es-herbivoro si 0.7)))
    
    (elefante
      ((tiene-trompa si 1.0)            ;; Definitorio
       (tiene-pelo no 0.8)
       (tamaño grande 0.95)
       (hace-sonido trompeta 0.9)
       (vive-en-tierra si 0.9)
       (tiene-patas si 0.8)
       (numero-patas 4 0.7)
       (es-herbivoro si 0.6)))
    
    (leon
      ((hace-sonido rugido 1.0)         ;; Definitorio
       (tiene-pelo si 0.9)
       (tamaño grande 0.95)
       (es-carnivoro si 0.9)
       (vive-en-tierra si 0.8)
       (tiene-patas si 0.8)
       (numero-patas 4 0.7)
       (habilidad-caza si 0.8)))
    
    (pajaro
      ((tiene-plumas si 1.0)            ;; Definitorio
       (puede-volar si 0.9)
       (tamaño pequeño 0.8)
       (tiene-alas si 0.95)
       (vive-en-tierra si 0.7)
       (tiene-patas si 0.6)
       (numero-patas 2 0.8)
       (pone-huevos si 0.7)))
    
    (pez
      ((vive-en-agua si 1.0)            ;; Definitorio
       (tiene-escamas si 0.95)
       (tiene-aletas si 0.95)
       (puede-nadar si 0.9)
       (puede-volar no 1.0)
       (tiene-branquias si 0.9)
       (tiene-patas no 0.8)
       (pone-huevos si 0.7)))
    
    (serpiente
      ((tiene-pelo no 1.0)              ;; Definitorio - sin pelo
       (es-reptil si 0.95)
       (tamaño mediano 0.6)
       (tiene-patas no 1.0)             ;; Definitorio - sin patas
       (vive-en-tierra si 0.8)
       (es-carnivoro si 0.7)
       (tiene-escamas si 0.9)
       (puede-nadar si 0.6)))
    
    (rana
      ((es-anfibio si 1.0)              ;; Definitorio
       (vive-en-agua si 0.9)
       (tamaño pequeño 0.8)
       (puede-saltar si 0.95)
       (tiene-patas si 0.8)
       (numero-patas 4 0.7)
       (puede-nadar si 0.8)
       (pone-huevos si 0.7)))
    
    (raton
      ((hace-sonido chillido 0.9)       ;; Característico
       (tiene-pelo si 0.9)
       (tamaño pequeño 1.0)             ;; Definitorio - muy pequeño
       (es-domestico si 0.6)
       (vive-en-tierra si 0.8)
       (tiene-patas si 0.7)
       (numero-patas 4 0.7)
       (tiene-cola si 0.8)
       (es-nocturno si 0.1)))
    ))

;; ====================================================================
;; FUNCIONES AUXILIARES
;; ====================================================================

;; Función para agregar un hecho a la base de hechos
(defun agregar-hecho (hecho)
  "Agrega un nuevo hecho a la base de hechos conocidos"
  (push hecho *hechos*))

;; Función para verificar incompatibilidades
(defun verificar-incompatibilidades (caracteristica valor)
  "Verifica si una característica es incompatible con los hechos actuales"
  (let ((incompatible nil))
    (dolist (incompatibilidad *incompatibilidades*)
      (destructuring-bind (car1 val1 car2 val2) incompatibilidad
        ;; Verificar si hay contradicción
        (when (and (equal caracteristica car1)
                   (equal valor val1)
                   (member (list car2 val2) *hechos* :test #'equal))
          (setf incompatible t))
        (when (and (equal caracteristica car2)
                   (equal valor val2)
                   (member (list car1 val1) *hechos* :test #'equal))
          (setf incompatible t))))
    incompatible))

;; Función mejorada para verificar incompatibilidades
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
        ;; Buscar si la característica 1 está en los hechos con el valor correcto
        (dolist (hecho hechos)
          (when (equal (first hecho) car1)
            (if (= (length hecho) 1)
                ;; Si solo tiene nombre (tipo de característica booleana)
                (setf tiene-car1 t)
                ;; Si tiene nombre y valor
                (when (equal (second hecho) val1)
                  (setf tiene-car1 t)))))
        ;; Buscar si la característica 2 está en los hechos con el valor correcto
        (dolist (hecho hechos)
          (when (equal (first hecho) car2)
            (if (= (length hecho) 1)
                (setf tiene-car2 t)
                (when (equal (second hecho) val2)
                  (setf tiene-car2 t)))))
        ;; Si ambas están presentes, hay incompatibilidad
        (when (and tiene-car1 tiene-car2)
          (setf hay-incompatibilidad t))))
    hay-incompatibilidad))

;; Función para calcular el porcentaje de coincidencia ponderado (V4)
(defun calcular-porcentaje-ponderado (animal hechos)
  "Calcula el porcentaje de coincidencia ponderado con detección de incompatibilidades"
  (let ((caracteristicas-ponderadas (cadr animal))
        (peso-total 0.0)
        (peso-coincidencias 0.0))
    
    ;; Primero, verificar incompatibilidades en los hechos mismos
    (when (verificar-incompatibilidades-hechos hechos)
      ;; Si hay incompatibilidades globales, ningún animal es válido
      (return-from calcular-porcentaje-ponderado 0.0))
    
    ;; Calcular suma de pesos RELEVANTES y contar coincidencias
    (dolist (caracteristica caracteristicas-ponderadas)
      (let ((nombre (first caracteristica))
            (valor (second caracteristica))
            (peso (third caracteristica)))
        ;; Buscar si esta característica existe en los hechos
        (dolist (hecho hechos)
          (when (equal nombre (car hecho))
            ;; Sumar peso total (solo características que aparecen en hechos)
            (incf peso-total peso)
            ;; Verificar si el valor coincide
            (when (equal valor (cadr hecho))
              (incf peso-coincidencias peso))))))
    
    ;; Si no hay características coincidentes, retornar 0
    (when (= peso-total 0.0)
      (return-from calcular-porcentaje-ponderado 0.0))
    
    ;; Retornar porcentaje ponderado
    (* (/ peso-coincidencias peso-total) 100.0)))

;; Función para obtener animales con porcentajes ponderados
(defun identificar-animales-con-porcentaje-ponderado ()
  "Identifica animales y retorna lista con porcentajes ponderados"
  (let ((resultados nil))
    (dolist (animal *base-conocimientos*)
      (let ((porcentaje (calcular-porcentaje-ponderado animal *hechos*)))
        (when (> porcentaje 0)
          (push (list (car animal) porcentaje) resultados))))
    (sort resultados #'> :key #'cadr)))

;; Función para obtener el mejor candidato
(defun obtener-mejor-candidato (&optional (umbral-minimo 0.0))
  "Retorna el animal con mayor porcentaje ponderado si supera el umbral"
  (let ((candidatos (identificar-animales-con-porcentaje-ponderado)))
    (when candidatos
      (let ((mejor (car candidatos)))
        (when (>= (cadr mejor) umbral-minimo)
          mejor)))))

;; Función para obtener candidatos por encima de un umbral
(defun obtener-candidatos-por-umbral (umbral)
  "Retorna todos los animales que superan el umbral especificado"
  (let ((candidatos (identificar-animales-con-porcentaje-ponderado))
        (resultado nil))
    (dolist (candidato candidatos)
      (when (>= (cadr candidato) umbral)
        (push candidato resultado)))
    (nreverse resultado)))

;; Función para determinar nivel de confianza
(defun determinar-nivel-confianza (porcentaje)
  "Retorna el nivel de confianza basado en el porcentaje"
  (cond
    ((>= porcentaje 100) "IDENTIFICACIÓN EXACTA")
    ((>= porcentaje 90) "IDENTIFICACIÓN MUY PROBABLE")
    ((>= porcentaje 80) "IDENTIFICACIÓN PROBABLE")
    ((>= porcentaje 70) "IDENTIFICACIÓN POSIBLE")
    (t "IDENTIFICACIÓN INCIERTA")))

;; Función para obtener símbolo de confianza
(defun simbolo-confianza (porcentaje)
  "Retorna un símbolo representativo del nivel de confianza"
  (cond
    ((>= porcentaje 100) "✓✓✓")
    ((>= porcentaje 90) "✓✓")
    ((>= porcentaje 80) "✓")
    ((>= porcentaje 70) "◐")
    (t "○")))

;; Grado de confiabilidad educativo (≥ 80%)
(defun grado-confiabilidad (porcentaje)
  "Retorna un grado educativo de confiabilidad para porcentajes ≥ 80%"
  (cond
    ((>= porcentaje 100) "EXCELENTE")
    ((>= porcentaje 98) "BUENO")
    ((>= porcentaje 90) "REGULAR")
    ((>= porcentaje 85) "MALO")
    ((>= porcentaje 80) "ACEPTABLE")
    (t "INSUFICIENTE")))

;; Función para obtener características únicas
(defun obtener-caracteristicas ()
  "Extrae todas las características únicas de la base de conocimientos"
  (let ((caracteristicas nil))
    (dolist (animal *base-conocimientos*)
      (dolist (caracteristica (cdr animal))
        (let ((nombre-car (car caracteristica)))
          (unless (member nombre-car caracteristicas)
            (push nombre-car caracteristicas)))))
    (nreverse caracteristicas)))

;; ====================================================================
;; INTERFAZ DE USUARIO
;; ====================================================================

;; Función para hacer una pregunta
(defun hacer-pregunta (caracteristica)
  "Hace una pregunta al usuario sobre una característica"
  (format t "~%¿~a? (opciones posibles): " (formatear-pregunta caracteristica))
  (format t "~%  Responde con el valor o 'no-se': ")
  (force-output)
  (read))

;; Función para formatear preguntas
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
    (tiene-branquias "El animal tiene branquias")
    (tamaño "El tamaño del animal es (pequeño/mediano/grande)")
    (hace-sonido "Qué sonido hace el animal (ladrido/maullido/relincha/rugido/trompeta/chillido)")
    (vive-en-agua "El animal vive en el agua")
    (vive-en-tierra "El animal vive en tierra")
    (puede-volar "El animal puede volar")
    (puede-nadar "El animal puede nadar")
    (puede-saltar "El animal puede saltar")
    (es-domestico "El animal es doméstico")
    (es-carnivoro "El animal es carnívoro")
    (es-herbivoro "El animal es herbívoro")
    (es-reptil "El animal es un reptil")
    (es-anfibio "El animal es un anfibio")
    (habilidad-caza "El animal tiene habilidad de caza")
    (numero-patas "Número de patas (2/4)")
    (pone-huevos "El animal pone huevos")
    (es-nocturno "El animal es nocturno")
    (t (format nil "~a" caracteristica))))

;; Función para mostrar candidatos con porcentajes
(defun mostrar-candidatos-con-porcentajes ()
  "Muestra candidatos con porcentajes, símbolos y grado educativo"
  (let ((candidatos (identificar-animales-con-porcentaje-ponderado)))
    (if candidatos
        (progn
          (format t "~%=== CANDIDATOS IDENTIFICADOS ===~%")
          (format t "~%Animales ordenados por porcentaje ponderado:~%~%")
          (let ((posicion 1))
            (dolist (candidato candidatos)
              (format t "  ~d. ~a - ~,2f%% ~a | Grado: ~a~%"
                      posicion
                      (string-upcase (car candidato))
                      (cadr candidato)
                      (simbolo-confianza (cadr candidato))
                      (grado-confiabilidad (cadr candidato)))
              (incf posicion))))
        (format t "~%No se encontraron candidatos compatibles.~%"))))

;; Función para explicar razonamiento
(defun explicar-razonamiento (animal &optional porcentaje)
  "Explica el razonamiento de la identificación"
  (format t "~%~%=== EXPLICACIÓN DEL RAZONAMIENTO ===~%")
  (format t "Se identificó al animal como: ~a~%" (string-upcase animal))
  
  (when porcentaje
    (format t "Porcentaje de coincidencia ponderada: ~,2f%%~%" porcentaje)
    (format t "Grado de confiabilidad: ~a~%" (grado-confiabilidad porcentaje)))
  
  (format t "~%Hechos proporcionados:~%")
  (if *hechos*
      (dolist (hecho *hechos*)
        (format t "  - ~a: ~a~%" (car hecho) (cadr hecho)))
      (format t "  (Ninguno)~%"))
  
  ;; Mostrar características del animal
  (let ((info-animal (assoc animal *base-conocimientos*)))
    (when info-animal
      (format t "~%Características del ~a en la base de conocimientos:~%" animal)
      (dolist (caracteristica (cadr info-animal))
        (let ((nombre (first caracteristica))
              (valor (second caracteristica))
              (peso (third caracteristica)))
          (format t "  - ~a: ~a (peso: ~,1f)~%" nombre valor peso))))))

;; ====================================================================
;; SISTEMA PRINCIPAL
;; ====================================================================

(defun sistema-experto ()
  "Sistema experto principal con ponderación V4"
  (format t "~%============================================~%")
  (format t "  SISTEMA EXPERTO V4 - IDENTIFICACIÓN~%")
  (format t "     DE ANIMALES CON PONDERACIÓN~%")
  (format t "============================================~%")
  (format t "~%Sistema mejorado con:~%")
  (format t "  - Ponderación de características~%")
  (format t "  - Detección de incompatibilidades~%")
  (format t "  - Identificación más rápida~%")
  (format t "  - Grado educativo de confiabilidad~%")
  
  (setf *hechos* nil)
  
  (let ((caracteristicas (obtener-caracteristicas))
        (animal-identificado nil)
        (intentos 0))
    
    ;; Ciclo de preguntas
    (dolist (caracteristica caracteristicas)
      (incf intentos)
      
      ;; Verificar si ya tenemos identificación exacta
      (let ((candidatos (identificar-animales-con-porcentaje-ponderado)))
        (when (and candidatos (>= (cadar candidatos) 100))
          (setf animal-identificado (caar candidatos))
          (return)))
      
      ;; Hacer pregunta
      (let ((respuesta (hacer-pregunta caracteristica)))
        (unless (equal respuesta 'no-se)
          ;; Verificar incompatibilidades
          (if (verificar-incompatibilidades caracteristica respuesta)
              (progn
                (format t "~%⚠ ADVERTENCIA: '~a' es incompatible con características anteriores.~%" respuesta)
                (format t "Se ignora esta característica.~%"))
              (progn
                (agregar-hecho (list caracteristica respuesta))
                ;; Mostrar progreso
                (let ((mejor (obtener-mejor-candidato 70)))
                  (when mejor
                    (format t "~%→ Candidato probable: ~a (~,1f%%)~%" 
                            (string-upcase (car mejor))
                            (cadr mejor)))))))))
    
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
           (format t "~%El animal identificado es: ~a (~,2f%% ponderado)~%"
                   (string-upcase (car mejor))
                   (cadr mejor))
           (format t "Grado de confiabilidad: ~a~%" (grado-confiabilidad (cadr mejor)))
           (when (> (length candidatos-70) 1)
             (format t "~%Otros candidatos compatibles (≥ 70%%):~%")
             (dolist (c (cdr candidatos-70))
               (format t "  - ~a (~,2f%%)~%" (string-upcase (car c)) (cadr c))))
           (mostrar-candidatos-con-porcentajes)
           (explicar-razonamiento (car mejor) (cadr mejor))))
        
        ;; Si hay candidatos pero < 70%
        (mejor-candidato
         (format t "  IDENTIFICACIÓN INCIERTA~%")
         (format t "============================================~%")
         (format t "~%No hay suficiente coincidencia ponderada (~,2f%%).~%"
                 (cadr mejor-candidato))
         (format t "Se necesita al menos 70%% de coincidencia.~%")
         (mostrar-candidatos-con-porcentajes))
        
        ;; Sin candidatos
        (t
         (format t "  NO SE PUDO IDENTIFICAR~%")
         (format t "============================================~%")
         (format t "~%Lo siento, no pude identificar el animal.~%")
         (format t "~%Hechos recopilados (~d):~%" (length *hechos*))
         (if *hechos*
             (dolist (hecho *hechos*)
               (format t "  - ~a: ~a~%" (car hecho) (cadr hecho)))
             (format t "  (No hay hechos)~%")))))
    
    (format t "~%============================================~%")))

;; ====================================================================
;; FUNCIÓN DE DEMOSTRACIÓN
;; ====================================================================

(defun demo-automatica ()
  "Demostración automática con ponderación V4 y grado de confiabilidad"
  (format t "~%=== DEMOSTRACIÓN AUTOMÁTICA V4 CON PONDERACIÓN ===~%~%")
  (format t "(Grado educativo de confiabilidad para compatibilidades ≥ 80%%)~%~%")
  (format t "Tabla rápida de grados:~%")
  (format t "  - 100%%  => EXCELENTE~%")
  (format t "  - 98–99%% => BUENO~%")
  (format t "  - 90–97%% => REGULAR~%")
  (format t "  - 85–89%% => MALO~%")
  (format t "  - 80–84%% => ACEPTABLE~%~%")
  
  ;; Ejemplo 1: Excelente (100%)
  (format t "Ejemplo 1: Excelente (100%%) - Caballo~%")
  (setf *hechos* '((hace-sonido relincha)))
  (let ((candidatos (identificar-animales-con-porcentaje-ponderado)))
    (format t "Hechos: hace-sonido=relincha~%")
    (format t "Candidato principal: ~a - ~,2f%% | Grado: ~a~%"
            (string-upcase (caar candidatos))
            (cadar candidatos)
            (grado-confiabilidad (cadar candidatos)))
    (format t "~%"))
  
  ;; Ejemplo 2: Bueno (≈ 98%)
  (format t "Ejemplo 2: Bueno (≈ 98%%) - Ratón~%")
  (setf *hechos* '((tamaño pequeño)
                  (hace-sonido chillido)
                  (tiene-cola si)
                  (numero-patas 4)
                  (tiene-patas si)
                  (tiene-pelo si)
                  (es-nocturno no)
                  (vive-en-agua no)))
  (let ((candidatos (identificar-animales-con-porcentaje-ponderado)))
    (format t "Hechos: tamaño=pequeño, hace-sonido=chillido, tiene-cola=si, numero-patas=4, tiene-patas=si, tiene-pelo=si, es-nocturno=no, vive-en-agua=no~%")
    (format t "Candidato principal: ~a - ~,2f%% | Grado: ~a~%"
            (string-upcase (caar candidatos))
            (cadar candidatos)
            (grado-confiabilidad (cadar candidatos)))
    (format t "~%"))
  
  ;; Ejemplo 3: Regular (90%)
  (format t "Ejemplo 3: Regular (90%%) - Perro~%")
  (setf *hechos* '((hace-sonido ladrido)
                  (tiene-pelo si)
                  (tamaño mediano)
                  (es-domestico si)
                  (vive-en-tierra si)
                  (es-carnivoro no)
                  (numero-patas 4)))
  (let ((candidatos (identificar-animales-con-porcentaje-ponderado)))
    (format t "Hechos: hace-sonido=ladrido, tiene-pelo=si, tamaño=mediano, es-domestico=si, vive-en-tierra=si, es-carnivoro=no, numero-patas=4~%")
    (format t "Candidato principal: ~a - ~,2f%% | Grado: ~a~%"
            (string-upcase (caar candidatos))
            (cadar candidatos)
            (grado-confiabilidad (cadar candidatos)))
    (format t "~%"))
  
  ;; Ejemplo 4: Malo (85%)
  (format t "Ejemplo 4: Malo (85%%) - Ratón~%")
  (setf *hechos* '((tamaño pequeño)
                  (hace-sonido chillido)
                  (tiene-cola si)
                  (numero-patas 4)
                  (es-domestico no)
                  (vive-en-agua no)))
  (let ((candidatos (identificar-animales-con-porcentaje-ponderado)))
    (format t "Hechos: tamaño=pequeño, hace-sonido=chillido, tiene-cola=si, numero-patas=4, es-domestico=no, vive-en-agua=no~%")
    (format t "Candidato principal: ~a - ~,2f%% | Grado: ~a~%"
            (string-upcase (caar candidatos))
            (cadar candidatos)
            (grado-confiabilidad (cadar candidatos)))
    (format t "~%"))
  
  ;; Ejemplo 5: Incompatibilidad
  (format t "Ejemplo 5: Detección de incompatibilidad~%")
  (setf *hechos* '((hace-sonido relincha) (vive-en-agua si)))
  (let ((candidatos (identificar-animales-con-porcentaje-ponderado)))
    (format t "Hechos: hace-sonido=relincha, vive-en-agua=si (INCOMPATIBLES)~%")
    (format t "Candidatos encontrados: ~d~%" (length candidatos))
    (if candidatos
        (dolist (c candidatos)
          (format t "  - ~a: ~,2f%%~%" (string-upcase (car c)) (cadr c)))
        (format t "  (Ninguno - incompatibilidad detectada)~%"))
    (format t "~%")))

;; ====================================================================
;; EJECUCIÓN
;; ====================================================================

(demo-automatica)

(format t "~%Para ejecutar el sistema interactivo, escribe: (sistema-experto)~%")
(format t "Para ejecutar la demo nuevamente, escribe: (demo-automatica)~%~%")

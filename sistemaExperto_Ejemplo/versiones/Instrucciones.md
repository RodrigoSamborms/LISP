# Instrucciones para Ejecutar el Sistema Experto V5 en LISP

## Descripción
Este sistema experto avanzado en LISP identifica animales mediante un proceso interactivo de preguntas y respuestas. Incluye un motor de inferencia, base de conocimientos con más animales, explicación detallada del razonamiento utilizado y un **grado educativo de confiabilidad** para resultados ≥ 80%.

## Características Principales
- **Motor de Inferencia**: Deduce conclusiones basándose en hechos conocidos
- **Base de Conocimientos**: 10 animales con múltiples características
- **Modo Interactivo**: Hace preguntas inteligentes al usuario
- **Modo Demostración**: Ejecuta ejemplos automáticos
- **Explicación de Razonamiento**: Muestra cómo llegó a la conclusión
- **Sistema de Ponderación con Porcentajes**: Calcula el porcentaje de coincidencia exacto para cada identificación
- **Umbral de Confianza (70%)**: Identifica automáticamente cuando un animal supera el 70% de concordancia, incluso sin coincidencia exacta
- **Niveles de Confianza**: Clasifica resultados como "Exacta", "Muy Probable", "Probable", "Posible" e "Incierta"
- **Grado de Confiabilidad (≥ 80%)**: Etiqueta el acierto como **Malo (85%)**, **Regular (90%)**, **Bueno (≈98%)** y **Excelente (100%)**
- **Más Animales (V5)**: Se amplió la base con nuevos animales terrestres, aves y acuáticos
- **Identificación Parcial**: Muestra candidatos ordenados por probabilidad cuando no hay coincidencia exacta (100%)

## Requisitos
- Tener instalado SBCL (Steel Bank Common Lisp) en tu entorno WSL Debian
- Opcional: `rlwrap` para mejorar la experiencia en el REPL

## Instalación de Requisitos (si no los tienes)
```bash
# Instalar SBCL
sudo apt update
sudo apt install sbcl

# Instalar rlwrap (opcional, pero recomendado)
sudo apt install rlwrap
```

## Formas de Ejecución

### Opción 1: Demostración Automática (Más Rápida)

Esta opción ejecuta el programa y muestra ejemplos automáticos sin interacción.

1. **Abre tu terminal WSL Debian**

2. **Navega a la carpeta del sistema experto:**
   ```bash
   cd /mnt/c/Users/sambo/Documents/Programacion/GitHub/LISP/sistemaExperto_Ejemplo/versiones/
   ```

3. **Ejecuta el archivo directamente:**
   ```bash
   sbcl --script sistema_experto_V5.lisp
   ```

4. **Observa la salida:**
   El programa mostrará ejemplos automáticos con **grado de confiabilidad**:
   - Excelente (100%)
   - Bueno (≈98%)
   - Regular (90%)
   - Malo (85%)
   - Un ejemplo de incompatibilidad (0% por contradicción)

### Opción 2: Modo Interactivo (Recomendado)

Esta opción te permite interactuar con el sistema experto respondiendo preguntas.

1. **Abre tu terminal WSL Debian**

2. **Navega a la carpeta del sistema experto:**
   ```bash
   cd /mnt/c/Users/sambo/Documents/Programacion/GitHub/LISP/sistemaExperto_Ejemplo/versiones/
   ```

3. **Inicia el intérprete de SBCL con rlwrap:**
   ```bash
   rlwrap sbcl
   ```
   
   O sin rlwrap:
   ```bash
   sbcl
   ```

4. **Carga el archivo:**
   ```lisp
   (load "sistema_experto_V5.lisp")
   ```

5. **Ejecuta el sistema experto interactivo:**
   ```lisp
   (sistema-experto)
   ```

6. **Responde las preguntas:**
   - Para preguntas de sí/no, responde: `si` o `no`
   - Para tamaño, responde: `pequeño`, `mediano`, o `grande`
   - Para sonidos, responde: `ladrido`, `maullido`, `trompeta`, `rugido`, `relincho`, o `chillido`
   - Si no sabes una respuesta, escribe: `no-se`

7. **Para ejecutar la demostración automática:**
   ```lisp
   (demo-automatica)
   ```

8. **Para salir del REPL de SBCL:**
   ```lisp
   (quit)
   ```
   O presiona `Ctrl+D`

## Ejemplos de Uso

### Ejemplo 1: Sesión Interactiva con Coincidencia Exacta (100%)
```
============================================
  SISTEMA EXPERTO DE IDENTIFICACIÓN
     DE ANIMALES EN LISP
============================================

Este sistema te ayudará a identificar un animal
basándose en sus características.

¿El animal tiene pelo? (opciones posibles): 
  Responde con el valor apropiado o 'no-se' si no sabes: 
si

¿El tamaño del animal es (pequeño/mediano/grande)? 
  Responde con el valor apropiado o 'no-se' si no sabes: 
mediano

¿Qué sonido hace el animal?
  Responde con el valor apropiado o 'no-se' si no sabes: 
ladrido

============================================
  ¡IDENTIFICACIÓN EXITOSA!
============================================

El animal es: PERRO (100% de coincidencia)

=== EXPLICACIÓN DEL RAZONAMIENTO ===
Se identificó al animal como: PERRO
Porcentaje de coincidencia: 100.00%

Basándose en los siguientes hechos:
  - tiene-pelo: si
  - tamaño: mediano
  - hace-sonido: ladrido
  - es-domestico: si
```

### Ejemplo 2: Identificación Parcial con Porcentajes (< 70%)
Cuando solo proporcionas algunas características y no hay candidatos que superen el 70%, el sistema muestra todos los candidatos posibles con sus porcentajes:

```
=== CANDIDATOS IDENTIFICADOS ===

Animales ordenados por porcentaje de coincidencia:

  1. CABALLO - 50.00% ○
  2. LEÓN - 50.00% ○
```

### Ejemplo 3: Identificación que Supera el Umbral de 70%
Cuando un animal coincide con el 70% o más de sus características, el sistema lo propone automáticamente como resultado identificado:

```
IDENTIFICACIÓN POSIBLE
============================================

El animal identificado es: CABALLO (75.00% de coincidencia)

Otros candidatos compatibles (≥ 70%):
  - (ninguno en este ejemplo)

=== CANDIDATOS IDENTIFICADOS ===

Animales ordenados por porcentaje de coincidencia:

  1. CABALLO - 75.00% ◐
```

## Cómo Funciona el Umbral de 70%

El sistema utiliza un **umbral mínimo de 70% de concordancia** para proponer identificaciones confiables:

### Niveles de Confianza

| Porcentaje | Nivel de Confianza | Símbolo | Acción del Sistema |
|---|---|---|---|
| 100% | IDENTIFICACIÓN EXACTA | ✓✓✓ | Propone resultado directamente |
| 90-99% | MUY PROBABLE | ✓✓ | Propone resultado con alta confianza |
| 80-89% | PROBABLE | ✓ | Propone resultado con confianza |
| 70-79% | POSIBLE | ◐ | Propone resultado como "posible" |
| 50-69% | INCIERTA | ○ | Muestra candidatos sin proponer |
| < 50% | Sin identificación | ○ | No hay suficiente información |

### Algoritmo de Evaluación

1. **Si hay coincidencia exacta (100%)**:
   - Se propone el animal identificado directamente
   - Nivel: IDENTIFICACIÓN EXACTA

2. **Si NO hay coincidencia exacta, pero hay candidatos ≥ 70%**:
   - Se propone el mejor candidato como "POSIBLE/PROBABLE/MUY PROBABLE"
   - Se muestran otros candidatos que superen 70%
   - Nivel: Varía según el porcentaje

3. **Si NO hay candidatos ≥ 70%**:
   - Se muestra la lista de candidatos
   - Se requiere más información
   - Nivel: IDENTIFICACIÓN INCIERTA

## Cómo Funciona el Sistema de Porcentajes

El sistema calcula el porcentaje de coincidencia de la siguiente manera:

1. **Coincidencia Exacta (100%)**: Todas las características del animal coinciden con los hechos proporcionados.

2. **Coincidencia Parcial (1-99%)**: 
   - Se calcula: `(Características que coinciden / Total características del animal) × 100`
   - Ejemplo: Si un león tiene 4 características y 2 coinciden = 50%

3. **Sin Coincidencia (0%)**:
   - Ocurre cuando hay contradicciones (ej: pelo=si pero también plumas=si)
   - El animal se descarta automáticamente

4. **Ordenamiento**: Los candidatos se muestran ordenados del mayor al menor porcentaje.

## Grado Educativo de Confiabilidad (V4)

Cuando el porcentaje de coincidencia es **≥ 80%**, el sistema V4 agrega un grado educativo:

| Porcentaje | Grado de Confiabilidad |
|---|---|
| 100% | Excelente |
| 98–99% | Bueno |
| 90–97% | Regular |
| 85–89% | Malo |
| 80–84% | Aceptable |

> Nota: Este etiquetado es **solo educativo** y no pretende uso real.

## Animales en la Base de Conocimientos

El sistema puede identificar los siguientes animales:

1. **Perro**: Tiene pelo, tamaño mediano, ladra, es doméstico
2. **Gato**: Tiene pelo, tamaño pequeño, maúlla, es doméstico
3. **Caballo**: Tiene pelo, tamaño grande, relincha, es doméstico
4. **Elefante**: Sin pelo, tamaño grande, hace trompeta, tiene trompa
5. **León**: Tiene pelo, tamaño grande, ruge, es carnívoro
6. **Tigre**: Tiene pelo, tamaño grande, ruge, es carnívoro
7. **Lobo**: Tiene pelo, tamaño mediano, aúlla, es carnívoro
8. **Oso**: Tiene pelo, tamaño grande, gruñe, es carnívoro
9. **Vaca**: Tiene pelo, tamaño grande, muge, es herbívoro
10. **Oveja**: Tiene pelo, tamaño mediano, bala, es herbívoro
11. **Cerdo**: Tiene pelo, tamaño mediano, gruñe, es herbívoro
12. **Pájaro**: Tiene plumas, tamaño pequeño, puede volar, tiene alas
13. **Gallina**: Tiene plumas, no vuela, cacarea, pone huevos
14. **Pez**: Vive en agua, tiene escamas, no vuela, tiene aletas
15. **Tiburón**: Vive en agua, tiene escamas, es carnívoro
16. **Delfín**: Vive en agua, tiene aletas, emite chillidos
17. **Serpiente**: Sin pelo, tamaño mediano, sin patas, es reptil
18. **Cocodrilo**: Reptil, vive en agua, es carnívoro
19. **Tortuga**: Reptil, puede nadar, suele ser herbívora
20. **Rana**: Vive en agua, tamaño pequeño, puede saltar, es anfibio
21. **Ratón**: Tiene pelo, tamaño pequeño, chilla, tiene cola

## Modificar y Extender el Sistema

### Agregar un Nuevo Animal

Para agregar un nuevo animal, edita el archivo `sistema_experto_V5.lisp` y modifica la variable `*base-conocimientos*`:

```lisp
(defvar *base-conocimientos*
  '(
    ;; ... animales existentes ...
    (tigre (tiene-pelo si) (tamaño grande) (hace-sonido rugido) (es-carnivoro si))
    ))
```

### Agregar una Nueva Característica

1. Agrega la característica a los animales correspondientes en `*base-conocimientos*`
2. Agrega el formato de pregunta en la función `formatear-pregunta`

## Solución de Problemas

### Error: "command not found: sbcl"
- Solución: Instala SBCL con `sudo apt install sbcl`

### Error: "cannot open the file"
- Solución: Verifica que estés en el directorio correcto con `pwd`

### El programa no responde en modo interactivo
- Solución: Presiona `Enter` después de cada respuesta
- Asegúrate de escribir las respuestas exactamente como se indica (sin comillas)

### Quiero reiniciar la consulta
- En el REPL, simplemente ejecuta `(sistema-experto)` nuevamente

### Ver solo los porcentajes sin interactuar
- Ejecuta `(mostrar-candidatos-con-porcentajes)` después de establecer hechos manualmente

## Notas Adicionales

- **Diferencias con V1**: Esta versión incluye un motor de inferencia completo, múltiples animales, modo interactivo, explicaciones de razonamiento, **sistema de ponderación por porcentajes**, **evaluación automática con umbral de 70%**, **grado educativo de confiabilidad** y **base ampliada de animales**.
- **Umbral de 70%**: El sistema propone identificaciones confiables cuando el porcentaje de coincidencia es ≥ 70%, incluso sin ser exactas.
- **Base de Conocimientos**: Puedes modificar y expandir fácilmente los animales y características.
- **Hechos Dinámicos**: El sistema mantiene una lista de hechos que va descubriendo durante la consulta.
- **Inferencia Inteligente**: El sistema elimina animales imposibles y enfoca las preguntas en características relevantes.
- **Sistema de Porcentajes**: Calcula la exactitud de cada identificación basándose en las características coincidentes.
- **Identificación Parcial**: Cuando no hay coincidencia del 100%, el sistema evalúa automáticamente si hay candidatos ≥ 70%.
- **Manejo de Contradicciones**: Los animales con contradicciones (características incompatibles) reciben 0% de coincidencia y son descartados.
- **Grado Educativo (V4)**: Para porcentajes ≥ 80%, el resultado se etiqueta como Malo/Regular/Bueno/Excelente.

## Ventajas del Sistema de Porcentajes

1. **Transparencia**: El usuario sabe qué tan segura es la identificación
2. **Múltiples Candidatos**: Permite ver alternativas cuando la información es incompleta
3. **Toma de Decisiones**: Facilita decidir si se necesita más información
4. **Debugging**: Ayuda a entender cómo el sistema está procesando los datos

## Recursos Adicionales

- Consulta el archivo `Resumen.md` para entender la teoría detrás de los sistemas expertos
- Revisa el código fuente con comentarios detallados en `sistema_experto_V2.lisp`
- Experimenta agregando tus propios animales y características
- Prueba diferentes combinaciones de características para ver cómo cambian los porcentajes

---

**Autor**: Sistema de Ejemplo en LISP  
**Versión**: 5.0 (más animales + ponderación + grado educativo)  
**Fecha**: Febrero 2026

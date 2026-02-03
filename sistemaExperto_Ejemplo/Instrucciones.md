# Instrucciones para Ejecutar el Sistema Experto en LISP

## Descripción
Este sistema experto en LISP identifica un animal basado en sus características (sonido, tamaño, pelaje, etc.). El código está diseñado para ser simple y fácil de entender, ideal para principiantes en programación LISP.

Hay varias versiones disponibles:
- **V5**: Sistema con ponderación y grado de confiabilidad
- **V6**: Sistema mejorado con criba ordenada de preguntas (RECOMENDADO)

## Requisitos
- Tener instalado un compilador de LISP (SBCL) en tu entorno WSL Debian
- Tener instalado `rlwrap` para mejor experiencia interactiva

## Instalación de Requisitos
```bash
sudo apt-get update
sudo apt-get install sbcl rlwrap
```

## Ejecución del Sistema Experto V6 (RECOMENDADO)

### Opción 1: Prueba Automatizada Rápida
Ejecuta varias pruebas automáticas (caballo + 3 animales) y una prueba aleatoria:
```bash
wsl bash -c "cd /mnt/c/Users/sambo/Documents/GitHub/LISP/sistemaExperto_Ejemplo/versiones/ && sbcl --script prueba_rapida.lisp"
```

### Opción 2: Prueba Interactiva Simulada
Simula las preguntas paso a paso, mostrando cómo sube el porcentaje de 98.4% a 100%:
```bash
wsl bash -c "cd /mnt/c/Users/sambo/Documents/GitHub/LISP/sistemaExperto_Ejemplo/versiones/ && sbcl --script prueba_interactiva_caballo.lisp"
```

### Opción 3: Modo Interactivo Real
Tú respondes las preguntas manualmente en tiempo real:
```bash
wsl bash -c "cd /mnt/c/Users/sambo/Documents/GitHub/LISP/sistemaExperto_Ejemplo/versiones/ && rlwrap sbcl --load sistema_experto_V6.lisp --eval '(sistema-experto)'"
```

## Preguntas de la Criba (V6)

El sistema hace 8 preguntas ordenadas para identificar el animal:

1. **¿Qué sonido hace?** (ej: ladrido, maullido, relincha, rugido, etc.)
2. **¿Tiene pelo?** (si/no)
3. **¿Cuál es su tamaño?** (pequeño/mediano/grande)
4. **¿Es doméstico?** (si/no)
5. **¿Vive en tierra?** (si/no)
6. **¿Tiene patas?** (si/no)
7. **¿Cuántas patas?** (2/4)
8. **¿Es herbívoro?** (si/no)

### Ejemplo: Identificando un CABALLO
Responde así para identificar un caballo:
- Sonido: **relincha**
- Pelo: **si**
- Tamaño: **grande**
- Doméstico: **si**
- Vive en tierra: **si**
- Tiene patas: **si**
- Número de patas: **4**
- Herbívoro: **si**

**Resultado esperado:** CABALLO - 100.00% ✓✓✓ (EXCELENTE)

## Archivos del Sistema

```
sistemaExperto_Ejemplo/
├── Instrucciones.md
├── versiones/
│   ├── sistema_experto_V6.lisp          (Sistema mejorado con criba)
│   ├── sistema_experto_V5.lisp          (Sistema con ponderación)
│   ├── prueba_rapida.lisp               (Prueba automatizada)
│   ├── prueba_interactiva_caballo.lisp  (Prueba paso a paso)
│   └── ...otras versiones
```

## Animales Soportados

El sistema puede identificar los siguientes animales:
- Mamíferos: Perro, Gato, Caballo, Elefante, León, Tigre, Lobo, Oso, Vaca, Oveja, Cerdo, Ratón
- Aves: Pájaro, Gallina
- Peces: Pez, Tiburón, Delfín
- Reptiles: Serpiente, Cocodrilo, Tortuga
- Anfibios: Rana

## Interpretación de Resultados

El sistema muestra:
- **Porcentaje de coincidencia:** Qué tan bien coinciden los datos con el animal identificado
- **Confiabilidad:** Símbolo que indica el grado de certeza:
  - ✓✓✓ (100%) = EXCELENTE
  - ✓✓ (90-99%) = BUENO
  - ✓ (80-89%) = REGULAR
  - ◐ (70-79%) = POSIBLE
  - ○ (<70%) = INCIERTO

## Notas
- Puedes responder `no-se` a cualquier pregunta si no tienes la información
- El sistema penaliza características importantes que faltan
- El sistema detecta incompatibilidades entre características
- Asegúrate de que SBCL esté correctamente instalado: `sbcl --version`

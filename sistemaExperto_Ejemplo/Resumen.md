# Resumen sobre Sistemas Expertos

## Introducción
Un sistema experto es un programa de inteligencia artificial diseñado para emular la capacidad de decisión de un experto humano en un dominio específico. Utiliza una base de conocimientos y reglas de inferencia para resolver problemas complejos, responder preguntas o hacer recomendaciones.

## Construcción
Las partes principales de un sistema experto son:
- **Base de conocimientos:** Contiene hechos y reglas sobre el dominio de aplicación.
- **Motor de inferencia:** Aplica las reglas a los hechos conocidos para deducir nueva información o tomar decisiones.
- **Interfaz de usuario:** Permite la comunicación entre el usuario y el sistema, facilitando la introducción de datos y la presentación de resultados.

## Ejemplos de sistemas expertos
- **MYCIN:** Diagnóstico de enfermedades infecciosas y recomendación de tratamientos médicos.
- **DENDRAL:** Identificación de estructuras moleculares en química orgánica.
- **XCON:** Configuración automática de sistemas informáticos complejos.
- **CLIPS:** Plataforma para desarrollar sistemas expertos en diferentes áreas.

---

# Explicación del Programa de Ejemplo

## Resumen del propósito del ejemplo
El programa de ejemplo identifica un animal basándose en características como tamaño, color y sonido, mostrando cómo funcionan las reglas y la base de conocimientos en un sistema experto sencillo.

## Partes que forman el ejemplo
- **Base de conocimientos:** Define las combinaciones posibles de características para cada animal.
- **Reglas de inferencia:** Evalúan los datos introducidos por el usuario y determinan el animal correspondiente.
- **Interfaz simple:** Solicita al usuario que ingrese las características y muestra el resultado.

### Diagrama simple

Usuario → [Interfaz] → [Motor de inferencia] → [Base de conocimientos] → Resultado

## Ejemplos de uso del programa
- Si el usuario ingresa: tamaño = grande, color = gris, sonido = trompeta, el sistema responde: "El animal es un elefante". Esto ocurre porque las reglas y la base de conocimientos asocian esas características con el elefante.
- Si el usuario ingresa: tamaño = pequeño, color = blanco, sonido = maullido, el sistema responde: "El animal es un gato". El resultado se debe a la coincidencia exacta con la base de conocimientos.

Así, el sistema experto compara los datos proporcionados con las reglas y la base de conocimientos para identificar el animal más probable.

---

## Conclusión
El funcionamiento de un sistema experto es análogo al razonamiento lógico algebraico. Por ejemplo, si sabemos que a = b y b = c, podemos inferir que a = c mediante la propiedad transitiva de la igualdad.

En este caso:
- **Base de conocimientos:** Los hechos (a = b, b = c) y las reglas algebraicas (propiedad transitiva).
- **Regla de inferencia:** Si x = y y y = z, entonces x = z.
- **Motor de inferencia:** Aplica la regla a los hechos conocidos para deducir que a = c.

De manera similar, el sistema experto de identificación de animales no "inventa" información nueva, sino que deduce conclusiones lógicas aplicando reglas establecidas a los datos proporcionados. Este proceso de inferencia es fundamental en todos los sistemas expertos, permitiéndoles resolver problemas específicos mediante la aplicación sistemática de conocimiento experto codificado.
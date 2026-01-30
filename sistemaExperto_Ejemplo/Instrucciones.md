# Instrucciones para Ejecutar el Sistema Experto en LISP

## Descripción
Este sistema experto en LISP identifica un animal basado en sus características como tamaño, color y sonido. El código está diseñado para ser simple y fácil de entender, ideal para principiantes en programación LISP.

## Requisitos
- Tener instalado un compilador de LISP en tu entorno WSL Debian.

## Ejecución del Ejemplo
1. **Abre tu terminal WSL Debian.**

2. **Navega a la carpeta donde se encuentra el archivo.**
   Usa el siguiente comando para cambiar al directorio del sistema experto:
   ```bash
   cd /mnt/c/Users/sambo/Documents/Programacion/GitHub/LISP/sistemaExperto_Ejemplo/
   ```

3. **Ejecuta el intérprete de LISP.**
   Utiliza el siguiente comando para iniciar el intérprete de SBCL con rlwrap:
   ```bash
   rlwrap sbcl
   ```

4. **Ejecuta el archivo LISP.**
   Para ejecutar el archivo `sistema_experto.lisp`, utiliza el siguiente comando:
   ```bash
   sbcl --script sistema_experto.lisp
   ```

5. **Observa la salida.**
   El programa imprimirá el tipo de animal basado en las características que se han definido en el código.

## Notas
- Puedes modificar las características en el archivo `sistema_experto.lisp` para probar diferentes animales.
- Asegúrate de que el compilador de LISP esté correctamente instalado y accesible desde la terminal.

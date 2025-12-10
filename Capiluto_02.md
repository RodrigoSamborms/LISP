# Variables y ramificaciones
Para definir una función el LISP, se utiliza la siguiente sinxasis:
```
(defun funcion-nombre (parametros) 
"caden de documentación opcional"
expresiones-del-cuerpo-de-la-funcion Parametro-Final-Retorno)
````

Ejemplo:
```
(defun sumar-dos (num1 num2)
    "Suma dos numeros y regresa la suma"
    (+ num1 num2))
```
note como (+ num1 num2) es el "valor de retorno" de la función <br>
Reutilización de variables, observe que intentamos usar una varible dentro de la definición 
![Varible no enlazada](./Imagenes/LISP_05.jpg "Variables")
Recibimos un mensaje de error, porque la funcion LET se ejecuta en paralelo, para evitar este error, utilice el simbolo *
despues del nombre de la función:
![Varible ahora enlazada](./Imagenes/LISP_06.jpg "Enlazado de variables")


## Lista de archivos de código de ejemplo:
 [example_02.lisp](.\examples\example_02.lisp)
<br>
 [example_03.lisp](.\examples\example_03.lisp)
<br>
 [example_04.lisp](.\examples\example_04.lisp)
<br>
 [example_05.lisp](.\examples\example_05.lisp)
<br>
 [example_06.lisp](.\example_06.lisp)
<br>
 [example_07.lisp](.\examples\example_07.lisp)
<br>
 [example_08.lisp](.\examples\example_08.lisp)
<br>
 [example_09.lisp](.\examples\example_09.lisp)
<br>
 [example_10.lisp](.\examples\example_10.lisp)
<br>
 [example_11.lisp](.\examples\example_11.lisp)
<br>
 [example_12.lisp](.\examples\example_12.lisp)
<br>
# Practico de Manipulación de Bases de Datos y Análisis Univariado

La base de Latinobarometro es una encuesta de opinión pública que para el año 2023 encuesto a más de $19.000$ personas en 17 países de Latinoamérica. Utilizando esta base deberás resolver los siguientes ejercicios, los cuales te servirán para afianzar el conocimiento adquirido hasta ahora respecto a manipulación de bases de datos y análisis univariado.

### Punto 1:

¿Cuáles son los 17 países en los que se realizó la encuesta durante 2023?

(**pista:** la variable de interés para dar respuesta es `idenpa`)

### Punto 2:

Genera una tabla en la que se muestre cuantos hombres y cuantas mujeres contestaron la encuesta en Colombia, México y Argentina.

(**pista:** debes obtener una frecuencia absoluta teniendo en cuenta dos variables categóricas y generar un filtro para los países especificados)

### Punto 3:

La variable `S10` es la edad a la cual el encuestado terminó su educación de tiempo completo, sin embargo, muchos de los encuestados aún están en etapa de formación, en estos casos se asignó el valor de $-6$. Creé una variable con el nombre `edad_educ` que reemplacé el valor de $-6$ por un $0$. Hecho eso genera el `summary` de la variable.

(**pista:** debes crear una variable a partir de una condición lógica sobre la variable `S10`)

### Punto 4: 

La variable `P61ST` pide una opinión sobre el nivel de desigualdad en el país en una escala del 1 al 10, donde 1 es completamente inaceptable y 10 es completamente aceptable (tiene también valores para 'No sabe' y 'No contesta'). Aunque es una variable categórica, podría ser tratada como numérica, transforme esta variable en numérica con el nombre `des_num` y obtén el promedio por país.

(**pista:** debes trabajar sobre las opciones de respuesta que tienen texto en la variable, terminada esa transformación utiliza `as.numeric` para convertir el valor en numérico)

### Punto 5:

La variable `P16ST` pide al encuestado que se defina ideológicamente entre izquierda y derecha en una escala del 0 al 10, donde 0 es completamente de izquierda y 10 es completamente de derecha, genera una variable categórica con el nombre `cat_ideo` que cumpla con las siguientes condiciones:

| Izquierda | Centro | Derecha |
|:---------:|:------:|:-------:|
| De 0 al 3 | Entre 4 y 6 | De 7 al 10 |

Ten presente que pueden haber valores en caso de que la persona no sepa que responder o no desee contestar.

(**pista:** es conveniente utilizar la función `case_when` para evaluar los valores de `P16ST` y establecer las categorías izquierda, centro y derecha)

### Punto 6:

De las votaciones recientes en distintos países del mundo, se habla de una 'derechización' de hombres jóvenes, la variable `reedad` divide a la población en cuatro rangos etarios, obtén el porcentaje de hombres de derecha, centro e izquierda, desagregándolo por los rangos etarios de `reedad`.

(**pista:** ten presente que te están preguntando solo por los hombres)

### Punto 7:

La variable `P62ST.10` pide evaluar a Donald Trump en una escala del 0 al 10, donde 0 significa 'muy mal' y 10 es 'muy bien'. Esta es una figura vinculada ideológicamente con la derecha, obtén el promedio y el desvió estándar de la calificación desagregando el análisis por la variable `cat_ideo` que construiste en el punto 5.

(**pista:** para obtener ese promedio debes asegurarte de que `P62ST.10` es de tipo numérica)

### Punto 8:

Genera un gráfico de barras que muestre el porcentaje de personas por país, que considera que la llegada de inmigrantes los beneficia (variable `P32INN`).

(**pista:** primero debes calcular los porcentajes, después filtrar los casos de 'Lo beneficia' y por último graficar)

### Punto 9:

Muestra gráficamente la distribución de la calificación de Donald Trump según categoría ideológica.

(**pista:** aprovecha lo que hiciste en el punto 7, si hiciste bien las cosas deberías poder entrar a graficar utilizando la variable `cat_ideo` del punto 5 y la variable en formato numérico de la respuesta a `P62ST.10`)

## Practico 1

En este módulo trabajamos con los datos de Latinobarometro 2023, la base de datos puede descargarse directamente de la pagina [Latinobarometro](https://www.latinobarometro.org/lat.jsp) o de este repositorio en el cúal se encuentra la base en formato **.dta**.\
El practico consta de 4 ejercicios y la resolución esta en el documento **resolucionPractico1.html**, tambien puede consultarse el video en el que se desarrolla la resolución paso a paso en el siguiente enlace [video]()

### Punto 1

La variable `P22ST` mide en una escala del 1 al 10 que tan justificable es para una persona la evasión de impuestos, siendo 1 nada de justificable y 10 totalmente justificable.

Calcula el promedio por país y determina cuales son los tres países que mas justifican
la evasión de impuestos. Y los tres que menos?

Pista: debes usar la funcion `group_by()` y `summarise()`

### Punto 2

La variable `P18ST.A` pregunta si esta **'Muy de acuerdo'**, **'De acuerdo'**, **'En desacuerdo'** o **'Muy en desacuerdo'** con la afirmación: **"La democracia puede tener sus problemas pero es el mejor sistema de gobierno"**.\
Crea una base de datos que contenga solamente aquellas observaciones que contestaron **'Muy de acuerdo'** o **'De acuerdo'** que a su vez sean 'Hombres' y que sean mayores de 60 años.

Pista: Debes usar la función `filter()` o la notacion matricial generando condiciones logicas en la parte de las filas. El siguiente codigo tiene parte del filtro a generar

### Punto 3

Crea una variable que tenga el valor de 1 si la persona es mujer y tiene menos de 30 años y que tome el valor de 0 en cualquier otro caso.

Pista: Debes crear una variable a partir de condiciones logicas, una forma de hacerlo es usando la funcion `ifelse()`. En el test debes evaluar dos condiciones logicas.

### Punto 4

En la `base.x` de abajo, creé el promedio de la variable `P22ST` para **Argentina**, **Colombia** y **México**.

Crea una `base.y` que tenga el promedio de la experiencia, siendo *experiencia = edad - S10*, y pega el dato a base.x usando la funcion `left_join()`.

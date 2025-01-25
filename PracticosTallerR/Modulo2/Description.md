# Módulo 2: Análisis Univariado

Para este ejercicio práctico utilizaremos la base de Latinobarometro de 2023 y una base del Banco Mundial construida con la librería `wbstats`.

## Pregunta 1

Dentro de la base de Latinobaromtero, la variable P6STGBS hace la siguiente pregunta: **¿Considera Ud. que la situación económica actual del país está mucho mejor, un poco mejor, igual, un poco peor, o mucho peor que hace doce meses?**

Los valores posibles de respuesta son:

 - Mucho mejor
 - Un poco mejor
 - Igual
 - Un poco mejor
 - Mucho peor
 
A. Crea una variable categórica de tres valores, a partir de los siguientes criterios:

  - Si P6STGBS es igual a "Mucho mejor" o "Un poco mejor", entonces que sea igual a "Mejor"
  - Si P6STGBS es igual a "Igual", entonces que sea igual a "Igual"
  - Si P6STGBS es igual a "Un poco peor" o "Mucho peor", entonces que sea igual a "Peor"

B. Una vez creada la variable, obten el porcentaje que considera que la economía esta "Mejor" por país y almacenalos en una tabla. Si te anímas, obten una segunda tabla desagregando la información no solo por país sino  tambien por sexo.

## Pregunta 2 

El objeto `baseBM` tiene información del PIB per capita, el porcentaje de población rural, el indice de gini, el comercio como porcentaje del PIB, la expectativa de vida al nacer, el consumo de alcohol per cápita y los homicidios intencionales por casa 100 mil habitantes, para distintos paises del mundo entre 1960 y 2022. Obviamente hay datos perdidos, es decir las mediciones para algunos paises, en algunos años no están. 

Filtra la base para 10 países que te interesen, selecciona la variable que mas te interese y obten las siguientes medidas estadisticas, para los países seleccionados:

- Media 
- Mediana 
- Desvio Estandar 
- Varianza 
- Minimo 
- Maximo 
- Rango 

Si te animas, obten el promedio para todos los países para los distintos años

# Práctico Módulo 4: Inferencia Estadística

## Introducción

El objetivo de este práctico es reforzar los conocimientos y aplicaciones de Inferencia estadística. La base `muestraIcfes.xlsx` es una base que contiene una muestra aleatoria de los resultados de la Prueba Saber 11 para cada uno de los municipios de Cundinamarca en el año 2020.\
Aclaración: (los municipios que cuentan con 100 observaciones son aquellos de los que se obtuvo una muestra, los que tienen menos observaciones son municipios cuya cantidad de estudiantes que presentaron la prueba es menor a 100 por lo tanto tenemos la población total).

## Pasos previos

- Limpieza del ambiente de trabajo
- Definición del directorio de trabajo
- Activación de librerías
- Carga de la base de datos

### Punto 1. 

Calcule los intervalos de confianza de la media del puntaje global (varible `PUNT_GLOBAL`) para cada municipio con un nivel de confianza del 95%.

### Punto 2. 

Debido a que son muchos municipios el gráfico de los intervalos de confianza resulta incomodo, intenta hacerlo, pero sino, grafica solamente los intervalos de confianza de los 10 municipios con mayor y de los 10 municipios con menor puntaje.

### Punto 3. 

La variable `ESTU_GENERO` dicotomiza a los estudiantes en hombres y mujeres, realice una prueba de hipótesis de diferencia de medias para verificar si existen diferencias significativas el puntaje global.

### Punto 4. 

La variable `FAMI_ESTRATOVIVIENDA` nos informa sobre las condiciones socioeconomicas del hogar, a mayor estrato en general, mayores ingresos. Realice una prueba ANOVA para verificar si existen diferencias significativas en el Puntaje Global, tomando como categoria de referencia "Sin Estrato".

### Punto 5. 

**Pregunta Teórica:** ¿porque no tiene sentido calcular los intervalos de confianza en los municipios con menos de 100 observaciones?

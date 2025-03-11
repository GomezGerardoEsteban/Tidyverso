# Practico 3: Analisis de Componentes Principales y Clusters

La base 'basePractico3.RData' contiene información de 6 tipos de problematicas de seguridad en los municipios del departamento de Antioquia.  La idea es replicar el análisis de la clase con estos datos cuya dimensión territorial es menor y por lo tanto puede ser mas pertinente.

## Ejercicio 1

- Calcula las componentes principales de las 6 variables (Extorsión, Hurto, Homicidio, V.Iterpersonal, Secuestro y V.Intrafamiliar).
- Cuales son las variables que mas aportan en la componente principal 1 y en la componente principal 2?
- La variable secuestro tiene muchos ceros, que pasa si la sacamos del análisis, afecta significativamente las conclusiones previas?
- Consideras que existen valores extremos en los datos?

## Ejercicio 2

El método de kmeans para obtener subgrupos es bastante simple, utiliza el criterio de la silueta para determinar cuantos clusters usar y calcula los clusters. Realiza el grafico de biplot coloreando los puntos por cluster.

Si previamente consideraste que habian valores extremos, sacalos y vuelve a calcular los grupos con kmeans, ten cuidado, de pronto con este cambio, el metodo de la silueta pide otro numero de clusters.

## Ejercicio 3

Te animas a calcular un indice a partir de los valores de las componentes principales e identificar los 10 municipios con mas problemas y los 10 con menos?
¿Estaran ubicados cerca desde el punto de vista geografico?

######################################################################################
# Ejercicio practico de manipulacion de bases de datos
# # Autor: Gerardo Esteban Gomez Santiago
# # Fecha: 15/10/2024
#
# Este es un ejercicio practico de tres ejercicios que buscan poner en practica algunos
# de los comandos utilizados en la clase del 14/10/2024.
# Para resolverlo debes descargar la base "basePractico1.RData", es una base con menos
# variables de la que estuvimos trabajando en la clase.
#
#######################################################################################

# Pregunta 1: -------------------------------------------------------------

# La variable P22ST es una variable númerica que mide en una escala del 1 al 10 que tan justificable es para
# una persona la evasión de impuestos, siendo 1 nada de justificable y 10 totalmente justificable

# Calcula el promedio por país y determina cuales son los tres países que mas justifican
# la evasión de impuestos?
# Y los tres que menos?

# Pregunta 2: --------------------------------------------------------------

# La variable P18ST.A pregunta si esta muy de acuerdo (1), de acuerdo (2), en desacuerdo (3)
# o muy en desacuerdo (4) con la afirmación: "La democracia puede tener sus problemas pero es el mejor
# sistema de gobierno"

# Crea una variable que genere dos categorías: "Acuerdo" si respondio (1) o (2) y "Desacuerdo" 
# si respondio (3) o (4).

# Si te animas obten el numero de personas por pais que contestaron Acuerdo o Desacuerdo

# Pregunta 3: -------------------------------------------------------------

# La variable P10STGBS. pregunta con cual frase se esta mas de acuerdo

# 1. La democracia es preferible a cualquier otra forma de gobierno
# 2. En algunas circunstancias, un gobierno autoritario puede ser preferible a uno democratico
# 3. A la gente como uno, nos da lo mismo un regimen democratico que uno no democratico
# Otras opciones de la encuesta son "No sabe" y "No Responde"

# Selecciona un país y averigua cuantos hombres y cuantas mujeres 
# estan mas de acuerdo con la primer afirmación.


# Pregunta 4  -------------------------------------------------------------

# Esta pregunta 4 es opcional, para resolverla debes cargar la base "pib.csv", esta base
# tiene el PIB de los 17 paises que hemos estado analizando para los anios 2020 a 2023
# esta en formato ancho, la idea es que transformes la base a formato largo, y que posteriormente
# pegues en la base que construiste para responder la pregunta 1 el PIB del anio 2023

# En principio tendras que usar los comandos gather, filter y left_join.

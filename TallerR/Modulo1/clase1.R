##########################################################################################################################
# Clase 1: Manipulacion de bases de datos
# Autor: Gerardo Esteban Gomez Santiago
# Date: 14/10/2024
#
# Descripcion:
# En este script estan los comandos desarrollados en la primera clase de R
# en la cual se explicaron cuestiones basicas como manipulacion de formatos,
# definicion del directorio de trabajo, creacion y tipos de variables, filtrado
# de bases, seleccion de variables, pegado de bases, entre otras cosas
#
# Bases utilizadas:
# se utilizaron 5 base en los siguientes formatos:
# - .txt
# - .csv
# - .xlsx
# - .dta
# - .RData
#
# Las mismas se pueden descargar del siguiente enlace.
# https://github.com/GomezGerardoEsteban/Tidyverso/tree/a0123e65cb9cf0480fb2eecd4bb91d2b7297710a/TallerR/Modulo1/bases
#################################################################################################################################

# Limpieza de ambiente

rm(list = ls()) # comando rm permite borrar cualquier elemento del ambiente de trabajo

rm(base1)

# Paqueteria

# install.packages("tidyverse") este comando es para instalar paquetes, debe ejecutarse solo una vez por paquete

# Activacion de paquetes

library(tidyverse)  # Paquete util para manipulacion de bases de datos, visualizacion de datos, etc
library(readxl)     # Paquete para levantar archivos .xlsx
library(foreign)    # Paquete para levantar archivos .dta

# Consulta y cambio del directorio de trabajo

getwd() # Consulta del directorio de trabajo

# Cambio del directorio de trabajo
# Para realizar el cambio la ruta debe tener los slash '/' o '\\' nunca '\'

setwd("E:\\Documents\\Escritorio\\tidyverso\\tidyverso\\cursoRLunes\\RLunes\\bases") 

# Como ver archivos en el directorio de trabajo
# funcion list.files para ver archivos contenidos en mi directorio de trabajo

archivos <- list.files(pattern = "base*")

archivos

# Levantado de las bases de datos -----------------------------------------

base1 <- read.table(file = "base1.txt")

base2 <- read.csv(file = archivos[2])

base3 <- read.dta(file = "base3.dta")

base4 <- read_excel(path = archivos[4])

load(file = "base5.RData")

# Pegado de las bases por filas con la funcion 'rbind()'

baseCompleta <- rbind(base1, 
                      base2[,2:ncol(base2)], 
                      base3, 
                      base4, 
                      base.RData)

# Tipos de variables

# numericas (int) (dbl)
# texto o strings (chr)
# categoricas (factor)
# logicas (logi) TRUE O FALSE

# Otros tipos de variables
# date
# haven_labelled

# Transformación de variables de numericas a texto y viceversa

base1$numinves <- as.character(base1$numinves)

base1$numinves <- as.numeric(base1$numinves)

class(baseCompleta$sexo)  # función para consultar el tipo de variable

unique(baseCompleta$sexo) # función para ver los valores unicos de una variable


# Transformación de una variable de texto a factor ------------------------

baseCompleta$sexo <- factor(x = baseCompleta$sexo, levels = c("Mujer",
                                                              "Hombre"))

class(baseCompleta$sexo)

summary(baseCompleta$sexo)    # funcion para ver resumenes estadisticos breves de las variables Ej con categorica

summary(baseCompleta$idenpa)  # funcion para ver resumenes estadisticos breves de las variables Ej con texto

paises <- unique(baseCompleta$idenpa) # Almaceno en un vector los valores unicos de la varible 'idenpa'

baseCompleta$idenpa <- factor(x = baseCompleta$idenpa, # Vector que contiene los valores que quiero transformar en categorias
                              levels = c(paises))      # Categorias que deseo establecer


# Creación de variables

# R Base

# Ej de creacion de una variable categorica a partir de una condicion logica usando la funcion 'ifelse()'

baseCompleta$menores30 <- ifelse(test = baseCompleta$edad < 30, # función 'ifelse()' sirve para crear variable a partir de condiciones logicas
                                 yes = "menores",
                                 no = "mayores")


# Ej de creacion de una variable numerica, realizando una resta entre variables

baseCompleta$Experiencia <- baseCompleta$edad - baseCompleta$S10

# LLamado de las variables creadas

baseCompleta$menores30
baseCompleta$Experiencia

# Opcion con {dplyr} funcion 'mutate()'

baseCompleta <- baseCompleta %>% # el pipe (%>%) puede generarse rapidamente con Ctrl + Shift + M
  mutate(edad2 = edad*edad,
         experiencia2 = Experiencia*Experiencia,
         esColombia = ifelse(test = idenpa == " Colombia",
                yes = 1,
                no = 0))


# Operadores logicos

# mayor o igual >=
# menor o igual <=
# igual         ==
# diferente     !=
# contenido     %in%
# negacion      !


# Filtrado de bases de datos

# Opcion en R Base 
# Para filtrar bases em R Base es necesario poner una condición logica
# dentro de los corchetes, antes de la coma.

# En el ejemplo de abajo nos quedamos con las filas que corresponden a los paises
# Mexico y Colombia

baseColMex <- baseCompleta[baseCompleta$idenpa %in% c(" Colombia", "Mexico"), ] 

unique(baseCompleta$idenpa)

# En el ejemplo de abajo nos quedamos con las filas que corresponden a la palabra "menores" 
# en la variable 'menores30'

baseMenores <- baseCompleta[baseCompleta$menores30 == "menores", ]

# Opcion con {dplyr} funcion 'filter()'

baseMayores <- baseCompleta %>% 
  filter(menores30 != "menores")


## Selección de variables en bases de datos

# R Base

# Para seleccionar variables con RBase, debo poner el numero de la columna
# despues de la coma dentro de los corchetes. (notacion matricial para consultar base de datos)

base.cols <- baseCompleta[ ,c(2, 5, 8, 9, 275:ncol(baseCompleta))]

# Opcion con {dplyr} funcion 'select()'

# La funcion select, me permite seleccionar las variables por su nombre

base.cols2 <- baseCompleta %>% 
  select(idenpa, edad, edad2, Experiencia, experiencia2, menores30)

# Puedo eliminar variables si antepongo un menos al nombre de la variable

base.cols2 <- base.cols2 %>% 
  select(-experiencia2, -menores30)

# Almacenando los nombres de las variables en un vector, puedo utilizar ese vector para la seleccion
# poniendo dentro del select 'all_of()' (ver ejemplo abajo)

nombresVar <- c("idenpa", "edad", 
                "edad2", "Experiencia", 
                "experiencia2", "menores30")

base.cols3 <- baseCompleta %>% 
  select(all_of(nombresVar))

# Ordenar las filas por los valores de una columna
# {dplyr} funcion 'arrange()'

# Orden ascendente

baseOrden <- base.cols %>% 
  arrange(edad)

baseOrden[1:10, ]

# Orden descendente

baseOrden <- baseOrden %>% 
  arrange(desc(edad))

baseOrden[1:10, ]


# Agrupación por variables categoricas

# group_by (sirve para agrupar por una variable categorica)
# summarise (sirve para generar resumenes o medidas estadísticas)

# guardo en una base pequeña la media de la edad por sexo

mediaEdad <- baseCompleta %>% 
  group_by(sexo) %>% 
  summarise(mediaEdad = mean(edad, na.rm = T))

mediaEdad

# guardo en una base la media de la edad por pais y por sexo

mediaEdadPais <- baseCompleta %>% 
  group_by(idenpa, sexo) %>% 
  summarise(mediaEdad = mean(edad, na.rm = T))

mediaEdadPais

# Calculo de mas de un estadistico por sexo, en este caso
# media, mediana y cantidad de observaciones

estadisticosSexo <- baseCompleta %>% 
  group_by(sexo) %>% 
  summarise(mediaEdad = mean(edad, na.rm = T),
            medianaEdad = median(edad, na.rm = T),
            n = n())

estadisticosSexo

# Bases en formato largo y en formato ancho

# Paso de una base de formato largo a una base de formato ancho

# Ejemplo con sexo en las columnas

mediaEdadPaisAncho <- mediaEdadPais %>% 
  spread(key = sexo, value = mediaEdad)

# Ejemplo con idenpa (pais) en las columnas

mediaEdadPaisAncho <- mediaEdadPais %>% 
  spread(key = idenpa, value = mediaEdad)

# Paso de una base de formato ancho a formato largo

mediaEdadLargo <- mediaEdadPaisAncho %>% 
  gather(key = "pais",                # Nombre de la variable que va a contener lo que estaba en las columnas
         value = "edadPromedio",      # Nombre de la variable que va a contener lo medido
         2:ncol(mediaEdadPaisAncho))  # indico las columnas que deben ser transformadas a formato largo

mediaEdadLargo


# Join de bases de datos con left_join()

# Para este ejemplo creo una baseX y una baseY, la intencion es pegar los valores de la baseY
# a los valores de la baseX.

# Creacion de baseX

baseX <- baseCompleta %>% 
  group_by(idenpa) %>% 
  summarise(mediaEdad = mean(edad, na.rm = T))

# Creacion de baseY

baseY <- baseCompleta %>% 
  group_by(idenpa) %>% 
  summarise(mediaExperiencia = mean(Experiencia, na.rm = T))

# Renombrando la primer columna de baseY (pasa de 'idenpa' a 'pais')
names(baseY)[1] <- "pais"


# Left_join sin usar el pipe

baseJoin1 <- left_join(x = baseX, # Base receptora de datos
                       y = baseY, # Base emisora de datos
                       by = c("idenpa" = "pais")) # Variables llave a partir de las cuales hago el pegado de datos

baseJoin1

# Left_join usando el pipe

baseJoin2 <- baseX %>% 
  left_join(y = baseY, 
            by = c("idenpa" = "pais"))

baseJoin2


# Conveniencias del {dplyr} -----------------------------------------------

# dplyr me permite combinar comandos de distinta naturaleza en uno solo
# esto hace mas legible el codigo y mejora el flujo de trabajo

dplyrCapacidad <- baseCompleta %>% 
  mutate(mayores = ifelse(test = edad > 60,  # Con 'mutate' creo una variable
                          yes = 1,
                          no = 0)) %>% 
  filter(mayores == 1) %>%                   # Con 'filter' filtro la base 
  arrange(desc(edad)) %>%                    # Con 'arrange' ordeno las filas por cierta variable
  select(idenpa, edad, mayores)              # Con 'select' elijo ciertas variables

# Cree una variable, filtre ciertas observaciones, ordene los datos y seleccione ciertas
# variables, en un solo comando

dplyrCapacidad







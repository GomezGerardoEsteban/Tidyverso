

# Modulo 5: Preparacion de los datos para visualizacion ----------------------------------------------------------------

# En este script estan los comandos que utilice para preparar los archivos .RData
# que contenian la información ya procesada y concentrarnos en la visualización.
# Para correr este script es necesario tener descargados los archivos 'tablaIcfes.csv'
# y los archivos geograficos de los municipios de colombia.
# - "MGN_MPIO_POLITICO.cpg"     
# - "MGN_MPIO_POLITICO.dbf"     
# - "MGN_MPIO_POLITICO.prj"    
# - "MGN_MPIO_POLITICO.sbn"     
# - "MGN_MPIO_POLITICO.sbx"     
# - "MGN_MPIO_POLITICO.shp"    
# - "MGN_MPIO_POLITICO.shp.xml" 
# - "MGN_MPIO_POLITICO.shx"

# Como veras solo cargamos el archivo "MGN_MPIO_POLITICO.shp", pero es necesario que este en el directorio
# de trabajo junto con los otros, porque en algun momento puede generar problemas su cargado si no es asi.


# Limpieza del ambiente de trabajo, libreria y directorio de trabajo ----------------------------------------

rm(list = ls())

library(tidyverse)

getwd()
setwd("E:/Documents/Escritorio/tidyverso/tidyverso/cursoRLunes/RLunes/scripts/bases/taller3/bases")


# Carga de los datos del icfes --------------------------------------------


tabla <- read.csv(file = "tablaIcfes.csv")

tabla %>% glimpse()

# Generacion del archivo 'tabla_medias.RData' -----------------------------

# Selecciono las variables a utilizar (genero y puntajes de las materias)
# organizando la base como una tabla larga para facilitar el calculo de los
# intervalos de confianza por genero y por prueba

tabla_larga <- tabla %>% 
  select(ESTU_GENERO, PUNT_MATEMATICAS, PUNT_INGLES, PUNT_LECTURA_CRITICA,
         PUNT_GLOBAL, PUNT_C_NATURALES, PUNT_SOCIALES_CIUDADANAS) %>% 
  gather(key = prueba, value = puntaje, 2:7)

tabla_larga %>% glimpse()  

# Creo la tabla con los intervalos de confianza para graficar

tabla_medias <- tabla_larga %>% 
  filter(!is.na(ESTU_GENERO)) %>% 
  group_by(ESTU_GENERO, prueba) %>% 
  summarise(media = mean(puntaje, na.rm = T),
            desvio = sd(puntaje, na.rm = T),
            n = n()) %>% 
  mutate(margen = qt(p = 0.975, df = n-1)*desvio/sqrt(n),
         Li = media - margen,
         Ls = media + margen)


# Modifico la variable 'prueba'

valor <- unique(tabla_medias$prueba)

etiqueta <- c("Cs. Naturales",
               "Global",
               "Inglés",
               "Lec. Crítica",
               "Matemáticas",
               "Sociales")

tabla_medias <- tabla_medias %>% 
  mutate(prueba = factor(case_when(
    prueba == valor[1] ~ etiqueta[1],
    prueba == valor[2] ~ etiqueta[2],
    prueba == valor[3] ~ etiqueta[3],
    prueba == valor[4] ~ etiqueta[4],
    prueba == valor[5] ~ etiqueta[5],
    prueba == valor[6] ~ etiqueta[6]
  ), levels = c("Global",
                "Cs. Naturales",
                "Matemáticas",
                "Inglés",
                "Lec. Crítica",
                "Sociales")))


# Exporto la base en formato .RData

save(tabla_medias, file = "tablaMedias.RData")


# Creacion del archivo datos_mapa.RData -----------------------------------

# En mapa obtengo la información de las medias por municipio y por genero
# Y tras convertir la tabla en formato ancho, genero la variable 'brecha'

mapa <- tabla %>% 
  filter(!is.na(ESTU_GENERO)) %>% 
  group_by(ESTU_COD_RESIDE_MCPIO, ESTU_GENERO) %>% 
  summarise(media = mean(PUNT_GLOBAL, na.rm = T)) %>% 
  spread(key = ESTU_GENERO, value = media) %>% 
  mutate(brecha = M - `F`)

# Cargo la base que tiene la información georeferenciada de los municipios

geo <- st_read(dsn = "MGN_MPIO_POLITICO.shp")

# Filtro para quedarme solo con la información geografica de los municipios
# de Cundinamarca

geo <- geo %>% 
  filter(DPTO_CNMBR == "CUNDINAMARCA")

# Join de bases 'mapa' con 'geo'

mapa$ESTU_COD_RESIDE_MCPIO <- as.character(mapa$ESTU_COD_RESIDE_MCPIO)

geo <- geo %>% 
  left_join(y = mapa,
            by = c("MPIO_CCNCT" = "ESTU_COD_RESIDE_MCPIO"))


# Guardo la base en formato '.RData' usando el comando save.

save(geo, file = "datos_mapa.RData")


# Modulo 5 preparación de datos para Visualización

# Fecha: 18/05/2025

# Descripción: en este script se encuentra el proceso de limpieza y organización de los
# datos para generar la visualización (tipo infografía) del último modulo del taller 
# introductorio al análisis de datos en R.

# Autor: Gerardo Esteban Gómez-Santiago

# Bases: los datos provienen de la Oficina Nacional de Estadística y Censos de El Salvador
# son los resultados de 2018 y 2023 de la Encuesta de Hogares de Proporsitos Múltiples.
# la pagina de desacarga es la siguiente:

# https://onec.bcr.gob.sv/Repositorio_archivos/

# Una vez completados los datos se les habilitara el enlace de descarga. Yo descargue la base
# de 2018 y de 2023

# limpieza del ambiente de trabajo ----------------------------------------

rm(list = ls())

# Activación de librerias

library(tidyverse) # Manipulación de bases y visualización de datos
library(sf)        # Paquete para trabajar con datos georeferenciados
library(haven)     # Haven para trabajar con archivos en formato SPSS, STATA o .SAV

# Carga de la base de datos con la separación por distritos de El Salvador

geo <- st_read(dsn = "bases/slv_admbnda_adm3_gadm_20240819.shp")

length(unique(geo$ADM2_ES))

# Carga de la base EHPM de 2018

base18 <- haven::read_sav(file = "bases/EHPM 2018.sav")

# Carga de la base EHPM de 2023
  
base23 <- haven::read_sav(file = "bases/Encuesta de Hogares de Propósitos Múltiples 2023.sav")

# Carga de datos desde pagiuna de wikipedia con los resultados por distrito de las votaciones de 2024

library(rvest) # paquete 'rvest' para extraer datos de una pagina web

# url de wikipedia con la tabla deseada

url <- "https://es.wikipedia.org/wiki/Anexo:Elecci%C3%B3n_presidencial_de_El_Salvador_de_2024"

# Leemos la pagina

pagina <- read_html(x = url)

# extraemos las tablas que están almacenadas en la pagina

tablas <- pagina %>% 
  html_elements("table") %>% 
  html_table(fill = T) 

tabla <- tablas[[14]] # la tabla 14 es la que contiene la información que necesitamos

View(tabla)

nombres <- names(tabla) # extraemos los nombres de las columnas

nombres <- nombres[1:4] # Nos quedamos con los priemros 4 elementos de nombres

nombres[3:4] <- "Bukele"  # Modificamos el elemento 3 y 4 del vector nombres

nombres[4] <- str_c(nombres[4],"_2") 

# Renombramos las primeras cuatro columnas de la tabla

names(tabla)[1:4] <- nombres 

# Dada la estructura de los datos, generamos un conjunto de filtros para quedarnos
# con los datos que necesitamos y en el formato que necesitamos

tabla <- tabla %>%
  select(1:4) %>%  # Seleccionamos solo las primeras 4 columnas
  filter(nchar(Departamento) > 2 & nchar(Distrito) > 2) # filtros donde conservamos solo las columnas con mas de dos characteres en las columnas 'Distrito' y 'Departamento'

# Creamos las variables distrito2, votos y porcentaje

tabla <- tabla %>% 
  mutate(distrito2 = ifelse(test = nchar(Bukele_2) == 0,
                            yes = Departamento,
                            no = Distrito),
         votos = ifelse(test = nchar(Bukele_2) == 0,
                        yes = Distrito,
                        no = Bukele),
         porcentaje = ifelse(test = nchar(Bukele_2) == 0,
                             yes = Bukele,
                             no = Bukele_2))

# Nos quedamos solo con esas tres columnas

tabla <- tabla %>% 
  select(distrito2, votos, porcentaje)

# Eliminamos las primeras dos filas que tienen valores equivocados

tabla <- tabla[-c(1:2), ]

# Modificamos la variable porcentaje

tabla <- tabla %>% 
  mutate(porcentaje = str_sub(string = porcentaje,
                              start = 1,
                              end = 5))

tabla$porcentaje <- as.numeric(tabla$porcentaje)

# Vector que define el Estado al que pertenecen los distritos

estado <- c(rep("Ahuachapán",12),
            rep("Cabañas", 9),
            rep("Chalatenango", 33),
            rep("Cuscatlán", 16),
            rep("La Libertad", 22),
            rep("La Paz", 22),
            rep("La Unión", 18),
            rep("Morazán", 26),
            rep("San Miguel", 20),
            rep("San Salvador", 19),
            rep("Santa Ana", 13),
            rep("San Vicente", 13),
            rep("Sonsonate", 16),
            rep("Usulután", 23))

tabla$Estado <- estado

# Obtención de seguridad por distrito -------------------------------------

# Creacion de variable 'aprob_factor' a partir de la variable r1120_1
# la cual pregunta si dados los problemas de seguridad 'pueden salir de noche'

base18 <- base18 %>% 
  mutate(aprob_factor = factor(x = r1120_1, 
                                 levels = c(1,2), 
                                 labels = c("Si", "No")))

## Obtenemos informacion desagregada por distrito para la variable 'aprob_factor'

s_exp <- base18 %>% 
  group_by(r004, codigomunic, aprob_factor) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(r004, codigomunic) %>% 
  mutate(total = sum(n),
         prop = n/total) %>% 
  filter(aprob_factor == "Si")

# Mismo proceso pero ahora con la encuesta del año 2023

base23 <- base23 %>% 
  mutate(aprob_factor = factor(x = r620_1,
                               levels = c(1,2),
                               labels = c("Si", "No")))


s_exp2 <- base23 %>% 
  group_by(r004, codigomunic, aprob_factor) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(r004, codigomunic) %>% 
  mutate(total = sum(n),
         prop = n/total) %>% 
  filter(aprob_factor == "Si")


# Pegamos los datos de 2018 y 2023 en la base de 2018

s_exp <- s_exp %>% 
  left_join(y = s_exp2,
            by = c("codigomunic", "r004"), suffix = c("_18", "_23"))

# Creamos una variable con el cambio porcentual por distrito en la percepcion de 
# seguridad entre 2018 y 2023

s_exp <- s_exp %>% 
  mutate(cambio = (prop_23 - prop_18) / prop_18 * 100)


# Generamos base con nombres y codigos

nombresBase <- names(attr(x = s_exp$codigomunic, which = "labels"))
codigosBase <- attr(x = s_exp$codigomunic, which = "labels")

nombres_codigos <- tibble(codigo = codigosBase,
                          nombre = nombresBase)

# Pegamos los nombres a la base que tiene la información de 2018 y 2023 
# y el cambio porcentual

s_exp <- s_exp %>% 
  left_join(y = nombres_codigos,
            by = c("codigomunic" = "codigo"))

# Creamos base con los nombres de los estados para cada distrito.

estBase <- names(attr(x = s_exp$r004, which = "labels"))
codBase <- attr(x = s_exp$r004, which = "labels")

est_cod <- tibble(codigo = codBase,
                  estado = estBase)

s_exp <- s_exp %>% 
  left_join(y = est_cod,
            by = c("r004" = "codigo"))

# Creamos variables llaves para pegar a la información de percepcion de seguridad
# el dato de la votacion de 2024 que obtuvimos de wikipedia y que esta almacenado 
# en el objeto 'tabla'

s_exp$llave <- str_c(s_exp$estado, s_exp$nombre)

tabla$llave <- str_c(tabla$Estado, tabla$distrito2)

# Pegamos la información

s_exp <- s_exp %>% 
  left_join(y = tabla,
            by = c("llave"))

# Creamos 'cambio2' la cual no tiene el cambio porcentual sino el cambio en terminos
# de 2023 menos 2018

s_exp$cambio2 <- s_exp$prop_23 - s_exp$prop_18

# Pegamos la información por distrito a la base con la información georeferenciada

geo <- geo %>% 
  mutate(llave = str_c(ADM1_ES, ADM3_ES)) %>% 
  left_join(y = s_exp,
            by = c("llave"))

# Agrupamos la información georeferenciada por departamento, para que el mapa no 
# quede con espacios vacios

info_mapa <- geo %>% 
  group_by(ADM1_ES) %>% 
  summarise(media = mean(cambio2, na.rm = T))


# Exportamos la información georeferenciada

save(info_mapa, file = "base_mapa.RData")

# Exportamos la información por municipio

writexl::write_xlsx(x = s_exp,
                    path = "base_municipios.xlsx")


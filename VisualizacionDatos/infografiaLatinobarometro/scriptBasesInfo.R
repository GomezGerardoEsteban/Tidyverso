
#  Procesamiento de bases de Latinobarometro para visualización de datos
# 'Tolerancia a la democracia en América Latina'

rm(list = ls())

setwd("E:/Documents/Escritorio/tidyverso/tidyverso/cursoRLunes/RMiercoles/bases")

library(tidyverse)
library(sf)
library(giscoR)
library(foreign)
library(ggtext)
library(extrafont)
library(ggspatial)
library(patchwork)

# bases -------------------------------------------------------------------

base2020 <- read.dta(file = "Latinobarometro_2020_Esp_Stata_v1_0.dta") # Cargamos base de 2020
base2023 <- read.dta(file = "Latinobarometro_2023_Esp_Stata_v1_0.dta") # cargamos base de 2023

# La democracia puede tener sus problemas pero es el mejor sistema de gobierno
# base2020 = p20st.a
# base2023 = p18st.a


# Procesamiento base 2020 -------------------------------------------------

# Comparación por países y por genero

# Creamos variable que agrupa acuerdo y desacuerdo

vec <- sort(unique(base2020$p20st_a)) # vector con valores unicos de la variable p20st_a

# Generamos agrupación, si es 'Muy de acuerdo' o 'De acuerdo', entonces "Acuerdo"
# Si es 'Muy en desacuerdo' o 'EN desacuerdo', entonces "Desacuerdo"

base2020$democ <- factor(case_when(
  base2020$p20st_a %in% vec[2:3] ~ "Acuerdo",
  base2020$p20st_a %in% vec[4:5] ~ "Desacuerdo",
  TRUE ~ "nr"),
  levels = c("Acuerdo", "Desacuerdo", "nr"))


# t20 contiene la información de la frecuencia relativa por país

t20 <- base2020 %>% 
  group_by(idenpa, democ) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(idenpa) %>% 
  mutate(total = sum(n),
         frecRel = n/total) %>% 
  filter(democ == "Acuerdo") %>% 
  arrange(desc(frecRel))



# Procesamiento de base 2023 ----------------------------------------------

# Mismo proceso que con 2023, pero la variable en este caso es P18ST_A

vec2 <- sort(unique(base2023$P18ST_A))


base2023$democ <- factor(case_when(
  base2023$P18ST_A %in% vec2[2:3] ~ "Acuerdo",
  base2023$P18ST_A %in% vec2[4:5] ~ "Desacuerdo",
  TRUE ~ "nr"),
  levels = c("Acuerdo", "Desacuerdo", "nr"))


t23 <- base2023 %>% 
  group_by(idenpa, democ) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(idenpa) %>% 
  mutate(total = sum(n),
         frecRel = n/total) %>% 
  filter(democ == "Acuerdo") %>% 
  arrange(desc(frecRel))

# Añadimos el año en ambas bases

t20$year <- 2020 # Año 2020
t23$year <- 2023 # Año 2023


# Diferencias por generó --------------------------------------------------

# agregamos en el group_by una variable adicional, en este caso sexo

# Diferencias por genero 2020

t20g <- base2020 %>% 
  group_by(idenpa, sexo, democ) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(idenpa, sexo) %>% 
  mutate(total = sum(n),
         frecRel = n/total) %>% 
  filter(democ == "Acuerdo")

# Diferencias por genero 2023

t23g <- base2023 %>% 
  group_by(idenpa, sexo, democ) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(idenpa, sexo) %>% 
  mutate(total = sum(n),
         frecRel = n/total) %>% 
  filter(democ == "Acuerdo")

t23g$year <- 2023 # Añadimos año 2020
t20g$year <- 2020 # Añadimos año 2023

# Incorporamos esta variable en las tablas generales para no tener problema en el pegado de las
# bases en una sola base

t20$sexo <- "General"
t23$sexo <- "General"

# Reubicamos la variable 'sexo' en las bases generales

t20 <- t20 %>% 
  relocate(sexo, .after = idenpa)

t23 <- t23 %>% 
  relocate(sexo, .after = idenpa)

# Pegamos las bases con rbind()

tFinal <- rbind(t20, t23, t20g, t23g)

# QUitamos espacios de la variable países

tFinal$idenpa <- str_replace(string = tFinal$idenpa, pattern = " ", replacement = "")

# Creamos una base con el identificador de país a tres caracteres

baseIso <- tibble(idenpa = unique(tFinal$idenpa),
                  iso3c = country_codes <- c("URY", "CRI", "CHL", "ARG", "SLV", "DOM", "BRA", "VEN", 
                                             "PRY", "BOL", "MEX", "COL", "PAN", "NIC", "PER", "GTM", 
                                             "ECU", "HND", "MEX", "PER", "PAN"))


# Pegamos el 'iso3c' en la base final

tFinal <- tFinal %>% 
  left_join(y = baseIso,
            by = "idenpa")

# QUitamos tildes de los nombres de los paises

tFinal$idenpa <- str_replace_all(string = tFinal$idenpa, 
                                 c("á" = "a", 
                                   "é" = "e", 
                                   "í" = "i", 
                                   "ó" = "o", 
                                   "ú" = "u"))


# Base tFinal esta lista para su uso la exportamos con el siguiente comando

save(tFinal, file = "baseFinal.RData")


# Base mapa (dataMapa.RData) ----------------------------------------------

mapa <- gisco_countries # creamos 'mapa' a partir de la base que ya viene precargada en la libreria 'gisco_countries'

# Le pegamos a esa base con los poligonos, los datos de 'tFinal'

mapa <- mapa %>% 
  left_join(y = tFinal,
            by = c("ISO3_CODE" = "iso3c"))

# Eliminamos los paises de los que no hay datos

mapa <- mapa %>% 
  filter(!is.na(democ))

# Dejamos la información solo para sexo general y el año 2023
# manteniendo el valor de Nicaragua del 2020

mapa <- mapa %>% 
  filter(year == 2023 & sexo == "General" | (year == 2020 & ISO3_CODE == "NIC" & sexo == "General"))

# YA esta lista la base, exportamos con el siguiente comando

save(mapa, file = "dataMapa.RData")

# Creacion de baseEdades.RData --------------------------------------------

# Creamos una base con las proporciones de aporbacion por edades, esto para infografia Nivel 2
# solo con la información de 2023

baseEdades <- base2023 %>% 
  group_by(idenpa, reedad, sexo, democ) %>%  # Agrupamos por pais, edad, sexo y variable de interes 'democ'
  tally() %>% 
  ungroup() %>% 
  group_by(idenpa, reedad, sexo) %>% 
  mutate(total = sum(n),
         frecRel = n/total) %>% 
  filter(democ == "Acuerdo") 

# Exportamos

save(baseEdades, file = "baseEdades.RData")


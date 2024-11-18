################################################################################
# Módulo 2: Análisis Estadístico Univariado
# Autor: Gerardo Esteban Gomez Santiago
# Fecha: 23/10/2024
# Descripción: En este Script desarrollamos los comandos necesarios para hacer análisis
#              estadísticos de variables cuantitativas, por análisis estadísticos nos 
#              referimos a obtener medidas de posición y de dispersión (media, mediana,
#              varianza, desvio estandar).
#              Revisamos tambien variables categóricas, a las cuales les calculamos frecuencia
#              absoluta y frecuencia relativa.
################################################################################

# limpiar el ambiente -----------------------------------------------------

rm(list = ls())

getwd() # Consulto mi directorio de trabajo

# Cambio mi directorio de trabajo si lo considero necesario con setwd()

setwd("E:/Documents/Escritorio/tidyverso/tidyverso/cursoRLunes/RLunes/scripts/bases")

# Paqueteria

library(tidyverse)

# tipos de estimadores ----------------------------------------------------

# Variables cuantis -------------------------------------------------------

# Medidas de posicion

# Media
# Mediana
# Moda

# Medidas de dispersion

# Desvio Estandar
# Varianza

# Variables categoricas ---------------------------------------------------

# Frecuencias absolutas
# Frecuencias relativas

# Levantado de  base ------------------------------------------------------

base <- foreign::read.dta(file = "scripts/bases/Latinobarometro_2023_Esp_Stata_v1_0.dta")

# Frecuencias relativas ---------------------------------------------------

unique(base$P18ST_A) # Consulto valores unicos de la variable P18ST_A

# Acuerdo = Muy de acuerdo o De acuerdo
# Desacuerdo = Muy en desacuerdo En desacuerdo
# No Sabe / No Responde = NR

base$democ <- case_when(base$P18ST_A == "De acuerdo" | base$P18ST_A == "Muy de acuerdo" ~ "Acuerdo",
                        base$P18ST_A == "En desacuerdo" | base$P18ST_A == "Muy en desacuerdo" ~ "Desacuerdo",
                        base$P18ST_A == "No sabe / No contesta" ~ "NR")


unique(base$democ) # Consulto valores unicos de la variable creada


# frecuencias absolutas ---------------------------------------------------

table(base$sexo, base$democ) # la funcion table devuelve una tabla de fecuencias absolutas

# Calculo frecuencias absolutas con group_by() y tally() ademas de tally()
# existe la posibilidad de usar count() y summarise(frecAbsoluta = n())

# base %>% 
#   group_by(idenpa, democ) %>% 
#   tally()
# 
# base %>% 
#   group_by(idenpa, democ) %>% 
#   count()

frecuencias <- base %>% 
  group_by(idenpa, democ) %>% 
  summarise(frecAbsoluta = n())


# frecuencias relativa ----------------------------------------------------

# Para obtener frecuencias relativas, añado una columna que tenga los totales
# postriormente divido la frecuencia absoluta entre el total.

frecuencias <- frecuencias %>% 
  mutate(total = sum(frecAbsoluta),
         frecRelativa = frecAbsoluta/total,
         porcentaje = frecRelativa*100)


# Filtro por las respuestas de "Acuerdo"

frecuenciasAc <- frecuencias %>% 
  filter(democ == "Acuerdo")

# Ordeno de mayor a menor en proporcion de acuerdo

frecuenciasAc <- frecuenciasAc %>% 
  arrange(desc(porcentaje))


frecuenciasAc # llamo a la tabla frecuenciasAC la cual tiene los calculos realizados

# Exporto la tabla creada en formato excel con la funcion 'write_xlsx' del paquete {writexl}

writexl::write_xlsx(x = frecuenciasAc, path = "frecuenciasDemoc.xlsx")

# ggplot2 dentro del {tidyverse}

# Grafico de columnas mostrando la proporcion de "acuerdo" por país

library(extrafont)
extrafont::loadfonts()
library(ggtext)

frecuenciasAc <- frecuenciasAc %>% 
  mutate(categorica = cut(porcentaje, 
                          breaks = quantile(x = frecuenciasAc$porcentaje, 
                                            probs = c(0,0.25,0.50,0.75,1)),
                          include.lowest = T,
                          labels = c("Bajo", "Medio-Bajo",
                                     "Medio-Alto", "Alto")))


frecuenciasAc %>% 
  ggplot(mapping = aes(x = porcentaje, y = reorder(idenpa, porcentaje))) +
  geom_col(mapping = aes(fill = categorica),
           alpha = 0.8,
           color = "black") +
  geom_text(mapping = aes(x = porcentaje + 5, label = str_c(round(porcentaje, 1), "%")),
            size = 3.5,
            family = "Book Antiqua") +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "Tolerancia a la democracia en America Latina",
       subtitle = "<span style = 'color:grey30;'>Porcentaje de</span> **acuerdo** <span style = 'color:grey30;'>con la pregunta</span> **¿La democracia puede<br>tener problemas pero es el mejor sistema de gobierno?** <span style = 'color:grey30;'>en 2023</span>",
       y = NULL,
       x = "Porcentaje (%)",
       fill = "Grado de\nAcuerdo",
       caption = "Fuente: Latinobarometro 2023\nElaboración: @GEsteban_Gomez") +
  scale_fill_manual(values = c("red4", "red3", "blue3", "blue4"))+
  theme(text = element_text(family = "Book Antiqua"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 9,
                                    margin = margin(20, -5, 0, 0)),
        plot.subtitle = element_markdown(margin = margin(4,0,10,0)),
        axis.title.x = element_text(margin = margin(10,0,0,0),
                                    size=12),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 9),
        legend.background = element_rect(fill = "lightgoldenrod3"),
        legend.position = c("right"),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.1, "cm"),
        legend.title = element_text(hjust = 0.5, size = 9, face = "bold"),
        legend.text = element_text(size = 8),
        plot.background = element_rect(fill = "lightgoldenrod"),
        panel.background = element_rect(fill = "lightgoldenrod"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "longdash")
        )


ggsave(filename = "graficoBarras.png",
       width = 7.35,
       height = 6,
       dpi = 500)

# variables cuantis -------------------------------------------------------

# Para este ejemplo usamos baseClase2.xlsx la cual contiene variables de los indicadores
# del Banco Mundial.

# Promedios o medias

base2 <- readxl::read_excel(path = "baseClase2.xlsx")

base2 %>% glimpse()

# COmo la base esta en formato ancho, la transformo en formato largo

base2 <- base2 %>% 
  gather(key = "year", value = "valor", 5:ncol(base2))

base2 %>% glimpse()

# Medidas de posicion -----------------------------------------------------

# Media

unique(base2$`Indicator Name`) # Valores unicos de la variable `Inicator Name`

# Cuando la variable (columna) contiene espacios debo utilizar el caracter (``)
# por eso es mejor no poner espacios en los nombres de las variables

# Utilizaremos el Indice de Gini por país y para distintos años para ver el análisis

baseGini <- base2 %>% 
  filter(`Indicator Name` == "Gini index")

# Con la funcion mean() obtengo el promedio de una variable

mean(baseGini$valor, na.rm = T)

unique(baseGini$`Country Name`) # Valores unicos de los paises

# creo un vector para filtrar por los paises que me interesan

paises <- c("Colombia",
            "Mexico",
            "Brazil",
            "Peru",
            "Haiti",
            "Argentina",
            "Ecuador",
            "Costa Rica",
            "Chile",
            "Uruguay",
            "Paraguay",
            "Panama",
            "Venezuela, RB",
            "Guatemala",
            "Honduras",
            "El Salvador",
            "Canada",
            "United States",
            "Nicaragua")


# Utilizo el comando logico %in% para decir "contenido en" 
# ademas con !is.na(valor) elimino los NA's de la variable
# valor

baseGini <- baseGini %>% 
  filter(`Country Name` %in% paises,
         !is.na(valor))


names(baseGini)[1] <- "country" # Cambio el nombre de `Country Name` por 'country'
names(baseGini)[2] <- "code"    # Cambio el nombre de `Country Code` por 'code'

# Genero una variable de reagrupación, en este caso region para hacer analisis estadisticos
# agrupados

baseGini <- baseGini %>% 
  mutate(region = case_when(
    country == "Argentina" ~ "Sur",
    country == "Brazil" ~ "Sur",
    country == "Canada" ~ "Norte",
    country == "Chile" ~ "Sur",
    country == "Colombia" ~ "Sur",
    country == "Costa Rica" ~ "Centro",
    country == "Ecuador" ~ "Sur",
    country == "El Salvador" ~ "Centro",
    country == "Guatemala" ~ "Centro",
    country == "Haiti" ~ "Centro",
    country == "Honduras" ~ "Centro",
    country == "Mexico" ~ "Norte",
    country == "Nicaragua" ~ "Centro",
    country == "Panama" ~ "Centro",
    country == "Paraguay" ~ "Sur",
    country == "Peru" ~ "Sur",
    country == "Uruguay" ~ "Sur",
    country == "Venezuela, RB" ~ "Sur",
    country == "United States" ~ "Norte",
  ))


# Obtengo ademas de la media, la mediana y el numero de medidas que existen por 
# pais

GiniPaises <- baseGini %>% 
  group_by(country) %>% 
  summarise(promedioGini = mean(valor),
            medianaGini = median(valor),
            numeroMediciones = n())


# Obtengo ademas de la media, la mediana y el numero de medidas que existen por 
# region (Norte, Centro y Sur)

GiniRegiones <- baseGini %>% 
  group_by(region) %>% 
  summarise(promedioGini = mean(valor),
            medianaGini = median(valor),
            numeroMediciones = n())

GiniRegiones

library(DescTools) # Paquete que sirve para calcular algunas medidas estadisticas que no vienen por 
                   # defecto en el R, ejemplo la moda.

# Calculo medidas estadisticas adicionales por region como 
# minimo min(), 
# maximo max(), 
# varianza var(), 
# desvio estandar sd()

GiniRegiones <- baseGini %>% 
  group_by(region) %>% 
  summarise(promedioGini = mean(valor),
            medianaGini = median(valor),
            numeroMediciones = n(),
            varianzaGini = var(valor),
            desvioGini = sd(valor),
            minimoGini = min(valor),
            maximoGini = max(valor))


GiniRegiones

# Calculo medidas estadisticas adicionales por pais 

GiniPaises <- baseGini %>% 
  group_by(country) %>% 
  summarise(promedioGini = mean(valor),
            medianaGini = median(valor),
            numeroMediciones = n(),
            varianzaGini = var(valor),
            desvioGini = sd(valor),
            minimoGini = min(valor),
            maximoGini = max(valor))

GiniPaises <- GiniPaises %>% 
  mutate(rango = maximoGini - minimoGini)

# Histogramas para conocer distribución -----------------------------------

# Cuando se realizan histogramas, es importante que este analizando distintas unidades de observaciones para un 
# mismo año o la misma unidad de observación en distintos momentos del tiempo, en este caso revisamos el comportamiento
# del indice de Gini para los países que existe registro en el año 2019

base2 <- base2 %>% 
  filter(`Indicator Name` == "Gini index" & year == "2019")

hist(base2$valor) # Comando para generar un histograma facil y rapido

# Histogramas con {ggplot2} (es mas facil manipularlos)

base2 %>% 
  ggplot(mapping = aes(x = valor)) +
  geom_histogram(bins = 15,
                 color = "black",
                 fill = "grey")

# Agrego al histograma la media y el valor maximo como lineas verticales

base2 %>% 
  ggplot(mapping = aes(x = valor)) +
  geom_histogram(bins = 15,
                 color = "black",
                 fill = "grey") +
  geom_vline(mapping = aes(xintercept = mean(valor, na.rm = T)),
             color = "red",
             linetype = "dashed") +
  geom_vline(mapping = aes(xintercept = max(valor, na.rm = T)),
               color = "orange",
               linetype = "dashed")
  

# grafico de cajas y bigotes ----------------------------------------------

# Este sirve para ver y comparar distribuciones, muestra los cuantiles por grupos y 
# posibles outliers()

boxplot(baseGini$valor) # Comando basico para ver un boxplot

# boxplot con paquete {ggplot2}

baseGini %>% 
  ggplot(mapping = aes(x = region, y = valor)) +
  geom_boxplot()

# Buscando identificar un valor extremo en region centro
baseGini[baseGini$region == "Centro" & baseGini$valor < 0.35, ]
# Buscando identificar un valor extremo en region norte
baseGini[baseGini$region == "Norte" & baseGini$valor > 0.51, ]

# Posible criterio de valores extremos, a partir de numeros que estan alejados en mas de dos 
# desvios estandar respecto a la media.

baseGini <- baseGini %>% 
  group_by(region) %>%
  mutate(media = mean(valor,na.rm = T),
         desvio = sd(valor, na.rm = T),
         outlier = ifelse(test = valor < media - desvio*2, yes = "outlierBajo",
                          ifelse(test = valor > media + desvio*2, yes = "outlierAlto", no = "noOutlier")))


baseGini %>% 
  filter(outlier != "noOutlier")


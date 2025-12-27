################################################################################
# Módulo 2: Análisis Estadístico Univariado
# Autor: Gerardo Esteban Gomez Santiago
# Fecha: 21/10/2024
# Descripción: En este Script desarrollamos los comandos necesarios para hacer análisis
#              estadísticos de variables cuantitativas, por análisis estadísticos nos 
#              referimos a obtener medidas de posición y de dispersión (media, mediana,
#              varianza, desvio estandar).
#              Revisamos tambien variables categóricas, a las cuales les calculamos frecuencia
#              absoluta y frecuencia relativa.
################################################################################

# limpiar el ambiente -----------------------------------------------------

rm(list = ls())


getwd()     # Consulto el directorio de trabajo

# setwd("") # pones la ruta de tu escriorio y te aseguras de tener este slash ("/") 
            # el cual obtengo con SHIFT + 7


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



# Paqueteria --------------------------------------------------------------

library(foreign)
library(tidyverse)

# Levantamos la base

base <- read.dta(file = "scripts/bases/Latinobarometro_2023_Esp_Stata_v1_0.dta")

# Revisamos la estrutura de la base

base %>% glimpse()


# Frecuencias relativas en variables categóricas ---------------------------------------------------

unique(base$P18ST_A) # Valores unicos de la variable P18ST_A

# Creo una nueva varible a partir de la variable P18ST_A, la cual tenga solo tres niveles
# acuerdo, desacuerdo o NA en caso de no sabe no contesta

base <- base %>% 
  mutate(democ = case_when(P18ST_A == "En desacuerdo" | P18ST_A == "Muy en desacuerdo" ~ "Desacuerdo",
                           P18ST_A == "De acuerdo" | P18ST_A == "Muy de acuerdo" ~ "Acuerdo",
                           P18ST_A == "No sabe / No contesta" ~ NA))

unique(base$democ) # Reviso los valores unicos obtenidos

# Calculo frecuencias absolutas con group_by() y tally() ademas de tally()
# existe la posibilidad de usar count() y summarise(frecAbsoluta = n())

frecuencias <- base %>% 
  group_by(sexo, democ) %>% 
  summarise(frecAbs = n())


# frecuencias <- base %>% 
#   group_by(sexo, democ) %>% 
#   tally()
# 
# 
# frecuencias <- base %>% 
#   group_by(sexo, democ) %>% 
#   count()

# Ahora, para calcular la frecuencia relativa, añado los totales por genero y luego
# divido frecuencias absolutas entre el total por genero

frecuencias <- frecuencias %>% 
  ungroup() %>% 
  group_by(sexo) %>% 
  mutate(total = sum(frecAbs, na.rm = T),
         frecRel = frecAbs/total)

# Como la frecuencia relativa es una proporcion, puedo expresarla en terminos de 
# porcentaje multiplicandola por 100

frecuencias$porcentaje <- frecuencias$frecRel*100 


# Grafico de las frecuencias relativas por genero -------------------------

# ggplot2 dentro del {tidyverse}

# Grafico con barras rellenadas

frecuencias %>% 
  ggplot(mapping = aes(x = sexo, y = porcentaje, fill = democ)) +
  geom_col() 

# Grafico de columnas mostrando solamente la proporcion de "acuerdo"

frecuencias %>% 
  filter(democ == "Acuerdo") %>% 
  ggplot(mapping = aes(x = sexo, y = porcentaje)) +
  geom_col()

# Otra alternativa para obtener la frecuencia relativa es table()

table(base$sexo, base$democ)


# variables cuantis -------------------------------------------------------

# Para este ejemplo usamos baseClase2.xlsx la cual contiene variables de los indicadores
# del Banco Mundial.

# Promedios o medias

base2 <- readxl::read_excel(path = "scripts/bases/baseClase2.xlsx")

base2 %>% glimpse()

# COmo la base esta en formato ancho, la transformo en formato largo

base2 <- base2 %>% 
  gather(key = "year", value = "valor", 5:ncol(base2))

base2 %>% glimpse()

# Filtro la base por el indicador que me interesa analizar, en este caso 
# "Intentional homicides (per 100,000 people)"

unique(base2$`Indicator Name`) # Valores unicos del indicador


base2

# Ejecuto el filtro

vecFil <- c("Physicians (per 1,000 people)",
"Life expectancy at birth, total (years)",
"Population, total")


base2Gra <- base2 %>% 
  filter(`Indicator Name` %in% vecFil)

base2Gra <- base2Gra %>% 
  select(-`Indicator Name`) %>% 
  spread(key = `Indicator Code`, value = valor)

library(ggExtra)

base2Gra <- base2Gra %>% 
  na.omit() %>% 
  filter(year == 2019) 

idh <- read.csv(file = "scripts/bases/HDR21-22_Composite_indices_complete_time_series.csv")

idh <- idh %>% 
  select(iso3, country, hdi_2019)

base2Gra <- base2Gra %>% 
  left_join(y = idh %>% 
              select(iso3, hdi_2019), by = c("Country Code" = "iso3"))

base2Gra <- base2Gra %>% 
  filter(!is.na(hdi_2019))

cuts <- c(0,0.55,0.7,0.8,1)

base2Gra <- base2Gra %>% 
  mutate(cate = cut(hdi_2019, 
                    breaks = cuts, 
                    labels = c("Low", "Medium", "High", "Very High")))

library(ggtext)
library(extrafont)
loadfonts()

summary(base2Gra$SP.POP.TOTL)

base2Gra %>% 
  ggplot(mapping = aes(x = SH.MED.PHYS.ZS, y = SP.DYN.LE00.IN)) +
  geom_point(mapping = aes(shape = cate, 
                           fill  = cate,
                           size = SP.POP.TOTL),
             color = "black",
             alpha = 0.7,
             show.legend = F) +
  geom_smooth(method = "lm", 
              se = F,
              color = "darkred",
              linetype = "longdash") +
  labs(title = "Relación entre la esperanza de vida al nacer y\nla cantidad de médicos por cada mil habitantes",
       subtitle = "Paises según IDH <span style = 'color:darkmagenta;'>Bajo</span>, <span style = 'color:darkseagreen;'>Medio</span>, <span style = 'color:yellowgreen;'>Alto</span> y <span style = 'color:darkorange;'>Muy Alto</span>. Tamaño de los puntos en función de la población",
       x = "Médicos por cada 1000 habitantes",
       y = "Esperanza de vida al nacer",
       caption = "Fuente: Banco Mundial - 2019 | @GEsteban_Gomez") +
  scale_fill_manual(values = c("darkmagenta",
                               "darkseagreen",
                               "yellowgreen",
                               "darkorange")) +
  scale_shape_manual(values = c(21:24)) +
  scale_size_continuous(range = c(1, 10)) +
  scale_x_continuous(n.breaks = 8) +
  scale_y_continuous(n.breaks = 10) +
  theme(text = element_text(family = "Berlin Sans FB"),
        plot.title = element_text(hjust = 0.5,
                                  margin = margin(0,0,30,0),
                                  size = 16),
        plot.subtitle = element_markdown(size = 12,
                                         hjust = 0,
                                         margin = margin(0,0,10,0)),
        plot.caption = element_text(hjust = 0, 
                                    margin = margin(15,0,0,0)),
        axis.title.y = element_text(size = 12,
                                    margin = margin(0,12,0,0)),
        axis.title.x = element_text(size = 12,
                                    margin = margin(12,0,0,0)),
        axis.text = element_text(size = 11),
        plot.background = element_rect(fill = "lemonchiffon2"),
        panel.background = element_rect(fill = "lemonchiffon"),
        panel.grid.major = element_line(colour = "grey69"),
        panel.grid.minor = element_line(colour = "grey79",
                                        linetype = "dotted")
        )
  
ggsave(filename = "graficoDispersion.png",
       width = 7.49,
       height = 5.75,
       dpi = 500)

# Obtengo las medias por pais

mediasHomic <- base2Homic %>% 
  group_by(`Country Name`) %>% 
  summarise(promedioHomic = mean(valor, na.rm= T))

# Transformo la variable 'year' de character a numeric

base2Homic$year <- as.numeric(base2Homic$year)

# Vector de países para generar filtro

paises <- c("Colombia",
            "Mexico",
            "Panama",
            "Brazil",
            "Peru",
            "Japan",
            "Argentina")

#  utilizo el comando logico %in% para decir "contenido en"

mediasHomic <- mediasHomic %>% 
  filter(`Country Name` %in% paises)

mediasHomic

# Agrego nuevas medidas estadisticas y almaceno en 'medidasHomic' 

medidasHomic <- base2Homic %>% 
  filter(`Country Name` %in% paises) %>% 
  group_by(`Country Name`) %>% 
  summarise(promedioHomic = mean(valor, na.rm= T),   # mean para promedio
            medianaHomic = median(valor, na.rm = T), # median para mediana
            maxHomic = max(valor, na.rm = T),        # max para maximo
            minHomic = min(valor, na.rm = T),        # min para minimo
            desvioHomic = sd(valor, na.rm = T),      # sd para desvio
            varHomic = var(valor, na.rm = T))        # var para varianza

medidasHomic 

# grafico de promedios ----------------------------------------------------

class(medidasHomic) # el objeto medidasHomic es un data.frame o tibble

medidasHomic %>% 
  ggplot(mapping = aes(y = reorder(`Country Name`, promedioHomic),     # reorder permite organizar el eje por algun criterio   
                       x = promedioHomic)) + 
  geom_col() +                                                         # geom_col() genera un gráfico de barras a partir de cierto valor
  geom_point(mapping = aes(y = reorder(`Country Name`, promedioHomic), # geom_point() grafica puntos para una coordenada 'x' y 'y'
                           x = maxHomic),                              # en este caso agrego el valor maximo por país  
             color = "red") +
  geom_point(mapping = aes(y = reorder(`Country Name`, promedioHomic),
                           x = minHomic),                              # en este caso agrego el valor minimo por país
             color = "blue")


# Histogramas para conocer distribución -----------------------------------

# Cuando se realizan histogramas, es importante que este analizando distintas unidades de observaciones para un 
# mismo año o la misma unidad de observación en distintos momentos del tiempo, en este caso revisamos el comportamiento
# de homicidios para los países qu existe registro en el año 2019

baseCorteTransversal <- base2Homic %>% 
  filter(year == 2019)

hist(baseCorteTransversal$valor) # Comando para generar un histograma facil y rapido

# Histogramas con {ggplot2} (es mas facil manipularlos)

baseCorteTransversal %>% 
  ggplot(mapping = aes(x = valor)) +
  geom_histogram(bins = 15,
                 color = "black",
                 fill = "grey") 

# Histograma al que le adicionamos la grafica de una función de densidad

baseCorteTransversal %>% 
  ggplot(mapping = aes(x = valor)) +
  geom_histogram(aes(y = ..density..),
                 bins = 15,
                 color = "black",
                 fill = "grey") +
  geom_density(fill = "blue",
               color = "darkblue",
               alpha = 0.1)

summary(base2Homic$valor)

# Calculo de cuantiles a partir de la función quantile()

quantile(x = baseCorteTransversal$valor, # Vector que contiene los valores de los que quiero obtener los cuantiles
         probs = c(0.25, 0.5, 0.75),     # Cuantiles a obtener, en este caso el cuartil 1 (0.25), 2 (0.5) y 3 (0.75)
         na.rm = T)                      # para que no tenga en cuenta los NA al momento de hacer el análisis.


# Grafico de cajas y bigotes ----------------------------------------------

# Este sirve para ver y comparar distribuciones, muestra los cuantiles por grupos y 
# posibles outliers()

boxplot(baseCorteTransversal$valor) # Comando basico para ver un boxplot

# boxplot con paquete {ggplot2}

baseCorteTransversal %>% 
  ggplot(mapping = aes(x = "Homicidios", y = valor)) +
  geom_boxplot(outliers = F)                          # outliers = F, hace que los outliers no salgan en el gráfico

# Obtengo boxplot por paises

base2Homic %>% 
  filter(`Country Name` %in% paises) %>% 
  group_by(`Country Name`) %>% 
  mutate(mediana = median(valor, na.rm = T)) %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = reorder(`Country Name`, desc(mediana)), 
                             y = valor),
               fill = "orange")


# Como graficar una distribución normal a partir de conocer media y desvio estandar?

# Creo una base de datos que contenga numeros aleatorios que siguen una distribución
# normal y respeten la media y el desvio estandar de la tasa de homicidios para Argentina
# y Mexico.

dfDistribucion <- tibble(pais = c(rep("ARG", 100), rep("MEX", 100)), # Con rep() repito 100 veces el valor que ponga primero
                         simul = c(rnorm(n = 100,                    # Con rnorm() genero un conjunto de numeros aleatorios que se distribuyen como una normal
                                         mean = 6.27,                # necesito decirle cuantoss numeros (n), cual media (mean) y cual desvio (sd)
                                         sd = 1.19),
                                   rnorm(n = 100,
                                         mean = 17.5,
                                         sd = 6.43)))


# Grafico el histograma para ambos paises y observo que Argentina tiene menor media
# y menor desvio estandar, mientras que México tiene mayor media y mayor desvio estandar

dfDistribucion %>% 
  ggplot(mapping = aes(x = simul)) +
  geom_histogram(bins = 15,
                 color = "black",
                 fill = "grey") +
  facet_wrap(~ pais)


################################################################################
# Modulo 3: Análisis bivariado --------------------------------------------
# Date: 28/10/2024
# Author: Gerardo Esteban Gomez Santiago
# Descripcion: En este script se presenta el código para calcular covarianzas,
#              correlaciones y regresiones simples, los cuales son los calculos
#              fundamentales en el análisis de dos variables.
################################################################################


# Limpieza de ambiente ----------------------------------------------------

rm(list = ls())

# Consulta del directorio de trabajo --------------------------------------

getwd()
setwd() # \ / 

# Paqueteria ---------------------------------------------------------------

library(tidyverse)

# Levantado de la base ----------------------------------------------------

base <- readxl::read_xlsx(path = "scripts/bases/baseClase3.xlsx")

base %>% glimpse() # Con glimpse() consulto la estructura de la base de datos
                   # numero y nombre de columnas, tipos de variables

# La pregunta que da inicio al analisi bivariado es:
# ¿Cual es la relacion entre PIB per cápita y democracia?

# Tenemos datos del 2019 para 73 paises, la medida de democracia es un indice de V-DEM
# y los datos de PIB corresponden al Banco Mundial

hist(base$NY.GDP.PCAP.KD) # Como se distribuyen los datos de PIB Per Cápita

hist(base$v2x_polyarchy) # Como se distribuyen los datos de democracia

# Covarianza

# Me dice cual es el sentido de la relacion entre las variables
# Lo que me importa de la covarianza es el signo si es positivo, 
# existe una relación positiva entre las variables y viceversa 
# si es negativo.

# Obtengo el valor de la covarianza con la funcion 'cov()'

covarianza <- cov(base$NY.GDP.PCAP.KD, base$v2x_polyarchy)

# Obtengo la matriz de varianzas y covarianzas

cov(base %>% select(NY.GDP.PCAP.KD, v2x_polyarchy))

# Obtengo la matriz de varianzas y covarianzas para todas las variables disponibles

cov(base[ ,4:ncol(base)])

# Correlato grafico del analisis bivariado --------------------------------

plot(base$NY.GDP.PCAP.KD, base$v2x_polyarchy) # Grafico de dispersión simple

# Grafico de dispersion o diagrama de puntos con {ggplot2}

base %>% 
  ggplot(mapping = aes(x = NY.GDP.PCAP.KD, y = v2x_polyarchy)) +
  geom_point()


# Cambio en el nombre de variables -------------------------------------------

# Utilizando el paquete dplyr() puedo usar la funcion 'rename()' para poner nuevos
# nombres.

?rename # Consulto la sintaxis para el uso de rename

# Ctrl + Shift + M = %>%

# La sintaxis de la funcion rename es 'nuevo_nombre = viejo_nombre'

base <- base %>% 
  rename(pibPerCapita = NY.GDP.PCAP.KD,
         democ = v2x_polyarchy)

# Grafico de dispersion con titulos y añadiendo una cracteristica sobre los puntos
# a partir del numero de medicos por cada 1000 habitantes.

base %>% 
  ggplot(mapping = aes(x = pibPerCapita, y = democ)) +
  geom_point(aes(size = SH.MED.PHYS.ZS)) +
  labs(title = "Relación entre PIB per cápita y Democracia - 2019",
       y = "Democracia (V-Dem Index)",
       x = "PIB per cápita")

# Correlacion -------------------------------------------------------------

covarianza

# Existe un rango de valores posibles para la medida de correlacion
# cualquier calculo de correlación solo puede estar entre -1 y 1

cor(base$pibPerCapita, base$democ) # Esta correlacion entre pibPerCapita y democ
                                   # de 0.60 indica que la relación es positiva y
                                   # relativamente fuerte.

# La correlacion define la magnitud o la fuerza de la relación entre las
# variables

cor(base$pibPerCapita, base$SP.DYN.IMRT.IN) # Aca tenemos una medida de correlacion inversa y de una
                                            # magnitud parecida a la pibPerCapita y democ, en este caso
                                            # es la relacion entre mortalidad infantil y pibPerCapita
                                            # el valor de -0.59 indica que la relación es negativa y relativamente fuerte

# Grafico de dispersion entre PIB y Tasa de mortalidad infantil

base %>% 
  ggplot(mapping = aes(x = pibPerCapita, y = SP.DYN.IMRT.IN)) +
  geom_point()

# Matriz de correlaciones de las tres variables analizadas hasta ahora
# Pib per cápita, Democracia y tasa de mortalidad infantil

cor(base %>% 
      select(pibPerCapita, democ, SP.DYN.IMRT.IN))


# Matriz de correlaciones de las 23 variables disponibles

matrizCorrelaciones <- cor(base[,4:ncol(base)])

# Comvierto la matriz en un data.frame()

matrizCorrelaciones <- as.data.frame(matrizCorrelaciones)

class(matrizCorrelaciones)

# exporto la matriz a excel

writexl::write_xlsx(x = matrizCorrelaciones, 
                    path = "matrizCorrelaciones.xlsx")

# El cor.test() me sirve para corroborar la significatividad estadistica de la correlacion
# en este caso, la correlacion entre pib per cápita y democracia es significativa al 99% de 
# confianza.

cor.test(base$pibPerCapita, base$democ,
         conf.level = 0.99)


# Regresion ---------------------------------------------------------------

# El analisis de regresion me sirve para saber cuanto cambia la variable 'Y' ante cambios en
# la variable 'X'

# Para calcular una regresion en R:

# 1. Utilizar la función lm() especificando la formula a evaluar y la base de datos de la que
#    provienen los datos. En este caso almacene la regresion en un objeeto de nombre 'reg'

reg <- lm(formula = democ ~ pibPerCapita,
          data = base)

# 2. Consulto los resultados usando la función 'summary()', esta me muestra el valor de los coeficientes
#    su significatividad y otros detalles como el R cuadrado (R^2)

summary(reg)

# Por una cuestion de medicion, divido el PIB entre mil, de esa manera la regresión me dice en cuanto se incrementaria
# el indice de democracia si incremento el PIB en mil dolares.

base$pibMil <- base$pibPerCapita/1000

reg2 <- lm(formula = democ ~ pibMil,
          data = base)

summary(reg2)

# Grafico la recta de regresion sobre el diagrama de puntos con {ggplot2}

# Opcion 1, con geom_smooth

base %>% 
  ggplot(mapping = aes(x = pibMil, y = democ)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

# Opcion 2, con geom_abline

base %>% 
  ggplot(mapping = aes(x = pibMil, y = democ)) +
  geom_point() +
  geom_abline(slope = reg2$coefficients[2],
              intercept = reg2$coefficients[1])



# Utilizamos logaritmos cuando las variables no se distribuyen de manera normal, tomando
# el logaritmo es posible que las variables tiendan a una normal.

# ¿Como se ven las variables en logaritmos?

# Grafico de densidad de la variable PIB per cápita en Logaritmo natural

base %>% 
  ggplot(mapping = aes(x = log(pibMil))) +
  geom_density()

# Grafico de densidad de la variable democ en Logaritmo natural

base %>% 
  ggplot(mapping = aes(x = log(democ))) +
  geom_density()

# Otra opcion para buscar normalidad en la variable es tomar las diferencias (la observacion en t, menos
# la observacion en t-1), es impotante hacer esto en series de tiempo, no se debe hacer en datos de corte
# transversal.

base$democL <- base$democ - lag(base$democ)

base %>% 
  ggplot(mapping = aes(x = democL)) +
  geom_density()

# creo las variables en logaritmos para calcular la regresión Log-Log

base$democLog <- log(base$democ)
base$pibLog <- log(base$pibMil)

# regresion Log-Log
# tiene ese nombre porque tanto la variable dependiente como independiente estan en logaritmos
# esto se hace buscando un mejor ajuste de los datos en terminos de linelidad y para tener una interpretacion
# mas intuitiva de los mismos.
# La interpretación de un modelo Log-Log es: 'Un incremento del 1% en la variable X, genera un incremento de
#                                              (el valor del coeficiente)% en la variable Y'.

reg <- lm(formula = democLog ~ pibLog,
          data = base)

summary(reg)

# Este ressultado se interpretaria como:
# 'Un incremento del 1% en el PIB, genera un incremento del 
# 0.16% en el indice de democracia'.

# Grafico de dispersion con la recta de regresion en el modelo log-log

base %>% 
  ggplot(mapping = aes( x = pibLog, y = democLog)) +
  geom_point() +
  geom_smooth(method = "lm")


# Visualización de correlaciones

# debo activar los paquetes: GGally, ggExtra y psych

# install.packages("GGally")
# install.packages("ggExtra")
# install.packages("psych")

# Activo los paquetes

install.packages("GGally")
install.packages("ggExtra")
install.packages("psych")

library(GGally)
library(ggExtra)
library(psych)

# Grafico de dispersión entre dos variables con histograma en los margenes

# Paso 1: crear el grafico de dispersion con la recta de regresion y almacenarlo en un objeto
# en este caso el objeto tiene nombre 'p'

p <- base %>% 
  ggplot(aes(x = pibMil, y = democ)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

# Paso 2: sobreescribo el grafico 'p' añadiendole los histogramas en los margenes con la funcion
# ggMarginal

p <- ggMarginal(p, type = "histogram") 

p

# ggsave sirve para guardar el gráfico en mi directorio de trabajo

ggsave(plot = p,
       filename = "graficoDispersion1.png",
       dpi = 500,
       width = 5.48,
       height = 4.2
         )

# Grafico descirptivo de las varibles de interes con ggpairs

# Este grafico genera un panel de gráficos, en el cual se observa los graficos de dispersion
# entre pares de variables, los graficos de densidad (la distribución de cada variable) y el 
# valor de las correlaciones por pares de variables

# Selecciono un subconjunto de variables de mi base de datos inicial

variables <- c("pibPerCapita", "democ",
               "IC.REG.DURS","EN.ATM.CO2E.PC",
               "MS.MIL.XPND.GD.ZS","SH.ALC.PCAP.LI",
               "SH.DTH.NCOM.ZS","SP.ADO.TFRT",
               "SP.DYN.IMRT.IN","SP.RUR.TOTL.ZS",
               "TX.VAL.MANF.ZS.UN")


baseGraph <- base %>% 
  dplyr::select(all_of(variables))

# Aplico la funcion ggpairs a la nueva base.
# Este es un grafico algo pesado, por lo que debo tener un poco de paciencia hasta ver el resultado

ggpairs(baseGraph)

# Grafico de correlacion, este genera un color para correlaciones positivas y otro color para correlaciones
# negativas, la intensidad del color me dice que tan fuerte es la correlación.
# solo debo poner la base con las variables de interes dentro de la funcion 'corPlot()'

corPlot(baseGraph,
        scale = F)


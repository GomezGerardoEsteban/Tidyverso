
# Módulo 4: Inferencia Estadística ----------------------------------------
# Fecha: 04/11/2024
# Autor: Gerardo Esteban Gómez Santiago
# Descripción: El objetivo de este Script, es introducirnos en los aspectos vinculados
#              a inferencia estadística, especificamente abordar de manera muy completa 
#              lo concerniente a intervalos de confianza y su relación con pruebas de 
#              hipótesis, veremos el código para hacer prueba de diferencia de medias,
#              diferencia de proporciones y test ANOVA.

# Limpieza del ambiente ---------------------------------------------------

rm(list = ls())

getwd() # Consulta del directorio de trabajo

setwd() # Comando para modificar el directorio de trabajo

# Librerias ---------------------------------------------------------------

library(tidyverse)
library(haven)      # Esta libreria se usa en reemplazo de 'foreing' ya que el archivo
                    # .dta de ENOE no se puede levantar con 'foreing'

# Levantado de la base ----------------------------------------------------

base <- haven::read_dta(file = "scripts/bases/ENOE_SDEMT224.dta")


# Intervalos de confianza para promedios ----------------------------------

# 1. La estimación puntual

mediasEntidad <- base %>% 
  filter(ingocup > 0) %>%        # Este filtro nos permite trabajar solo con aquellas observaciones que tienen un ingreso mensual mayor a cero
  group_by(ent) %>%              # Agrupamos por entidad fedeativa de México
  summarise(mediaIngreso = mean(ingocup, na.rm = T))  # Obtenemos el promedio por entidad federativa


# 2. Conocer la desviacion estandar de los datos (o la varianza)
#    conocer el tamaño de la muestra

# A la tabla anterior añadimos el calculo del desvio estandar y del 
# tamaño de muestra, necesarios para calcular intervalos de confianza

mediasEntidad <- base %>% 
  filter(ingocup > 0) %>% 
  group_by(ent) %>% 
  summarise(mediaIngreso = mean(ingocup, na.rm = T),
            desvioIngreso = sd(ingocup, na.rm = T),     
            tamanio = n())

# Con print, podemos pedir que muestre un mayor numero de filas
# en la consola

print(mediasEntidad, n = 32)

# Parentesis, extraigamos las etiquetas por entidad

attr(mediasEntidad$ent) # Esta función permite ver los atributos de una variable


etiquetas <- attr(mediasEntidad$ent, which = "labels") # Vector que contiene los valores posibles de 'ent'
nombresEtiquetas <- names(attr(mediasEntidad$ent, which = "labels")) # Vector que contiene los nombres por entidad de 'ent'

# Peguemos nombres

# Opcion 1: creamos una variable, esto es solo valido si el vector que contiene
# los nombres por entidad esta organizado de la misma manera a como estan organizadas
# las observaciones en la base de datos resumen

mediasEntidad$nombreEntidad <- nombresEtiquetas

# Opcion 2: hacer un Join, en este caso solo debo asegurarme de que existan las 
# variables llave para poder pegar las bases

mediasEntidad <- mediasEntidad %>% 
  left_join(y = data.frame(etiq = etiquetas,
                           nom = nombresEtiquetas), by = c("ent" = "etiq"))


# 3. cuantil de la distribución t-Sudent

# Se calculan los cuantiles de la distribución t-Student
# al 95% de confianza y al 99% de confianza

mediasEntidad <- mediasEntidad %>% 
  mutate(cuantil95 = abs(qt(p = 0.025, df = tamanio-1)),
         cuantil99 = abs(qt(p = 0.005, df = tamanio-1)))

# 4. Calculo del margen

# Matematicamente el margen es la multiplicación del error estandar
# por el cuantil, siendo el error estandar el desvio estandar dividido
# en la raiz del tamaño de muestra

mediasEntidad <- mediasEntidad %>% 
  mutate(margen95 = cuantil95*desvioIngreso/sqrt(tamanio),
         margen99 = cuantil99*desvioIngreso/(tamanio)^(1/2))

# 5. Obtengo limites

# Los limites no son otra cosa que el estimador puntual +/- el margen de error

mediasEntidad <- mediasEntidad %>% 
  mutate(Li95 = mediaIngreso - margen95,
         Ls95 = mediaIngreso + margen95,
         Li99 = mediaIngreso - margen99,
         Ls99 = mediaIngreso + margen99)

# Exportamos la tabla con las medias, los desvios, el tamaño de muestra
# el margen de error y los intervalos de confianza al 95 y al 99 

writexl::write_xlsx(x = mediasEntidad, path = "inferenciaMediaIngresoMEX.xlsx")

# grafico de los intervalos de confianza ----------------------------------

mediasEntidad %>% 
  ggplot(mapping = aes(x = mediaIngreso, y = reorder(nom, desc(mediaIngreso)))) +
  geom_point() +
  geom_errorbar(mapping = aes(xmin = Li95, xmax = Ls95)) +
  geom_errorbar(mapping = aes(xmin = Li99, xmax = Ls99),
                color = "blue",
                linetype = "dashed")


# Prueba de hipotesis de diferencia de medias -----------------------------

# la funcion t.test trabaja de manera similar a la función 'lm()'
# solo requiere una formula y una base de datos, se puede modificar
# el tipo de hipotesís (argumento 'alternative'), y el nivel de confianza (argumento 'conf.level')

t.test(formula = ingocup ~ ent,
       data = base %>% 
         filter(ingocup > 0 & ent %in% c(17, 29)),
       conf.level = 0.95)

t.test(formula = ingocup ~ ent,
       data = base %>% 
         filter(ingocup > 0 & ent %in% c(7, 10)),
       conf.level = 0.95,
       alternative = c("greater"))


# Prueba de hipotesis de diferencia de proporciones -----------------------

# Para realizar la prueba de diferenia de proporciones utilizamos la proporcion
# de subocupados por entidad federativa

# Filtramos valores de ingreso positivo y de dos entidades federativas especificas
# y obtenemos frecuencias absolutas

proporciones <- base %>% 
  filter(ingocup > 0 & ent %in% c(8, 9)) %>% 
  group_by(ent, sub_o) %>% 
  summarise(frecuenciaAbs = n())

# Obtenemos ahora los totales de las entidades federativas y el estimador de proporciones

proporciones <- proporciones %>% 
  mutate(total = sum(frecuenciaAbs),
         proporcion = frecuenciaAbs/total)

# No quedamos con la categoria de interes, en este caso subocupados

proporciones <- proporciones %>% 
  filter(sub_o == 1)

# calculo de la diferencia de proporciones utilizando la función prop.test

# La función prop.test requiere un vector que contenga los casos
# de exito y otro vector que contenga los casos posibles, es decir, un vector con las frecuencias
# absolutas y un vector con las frecuencias totales.

prop.test(x = proporciones$frecuenciaAbs, 
          n = proporciones$total,
          alternative = c("two.sided"),
          conf.level = 0.99)

prop.test(x = c(257, 283), n = c(4904, 2276))

# Prueba Anova ------------------------------------------------------------

# Esta nos permite ver el comportamiento de los promedios para mas de dos categorias
# vamos a analizar como se comporta el ingreso medio a partir del maximo nivel educativo
# alcanzado

baseIng <- base %>% 
  filter(ingocup > 0)

# Eliminamos observaciones que no saben cual es su maximo nivel educativo alcanzado

baseIng <- baseIng %>% 
  filter(cs_p13_1 != 99)

# Con los comandos que vienen creamos una vaiable categorica que contenga las etiquetas
# del maximo nivel educativo alcanzado

numeroEdu <- unique(baseIng$cs_p13_1)
numeroEdu <- sort(numeroEdu)

nombresEdu <- names(attr(x = baseIng$cs_p13_1, which = "labels"))

baseIng <- baseIng %>% 
  mutate(educ = factor(case_when(
    cs_p13_1 == numeroEdu[1] ~ nombresEdu[1],
    cs_p13_1 == numeroEdu[2] ~ nombresEdu[2],
    cs_p13_1 == numeroEdu[3] ~ nombresEdu[3],
    cs_p13_1 == numeroEdu[4] ~ nombresEdu[4],
    cs_p13_1 == numeroEdu[5] ~ nombresEdu[5],
    cs_p13_1 == numeroEdu[6] ~ nombresEdu[6],
    cs_p13_1 == numeroEdu[7] ~ nombresEdu[7],
    cs_p13_1 == numeroEdu[8] ~ nombresEdu[8],
    cs_p13_1 == numeroEdu[9] ~ nombresEdu[9],
    cs_p13_1 == numeroEdu[10] ~ nombresEdu[10]
  ), levels = nombresEdu))

baseIng$educ

# Realizamos la prueba anova de un factor utilizando los comandos de regresion
# lineal

regAnova <- lm(formula = ingocup ~ educ,
               data = baseIng)

# Con summary vemos los resultados

summary(regAnova)

baseIng %>% 
  group_by(educ) %>% 
  summarise(media = mean(ingocup))


# Calculamos los intervalos de confianza del ingreso medio segun maximo
# nivel educativo alcanzado

mediasEduc <- baseIng %>% 
  group_by(educ) %>% 
  summarise(mediaIngreso = mean(ingocup),
            desvioIngreso = sd(ingocup),
            tamanio = n(),
            cuantil95 = qt(p = 0.975, df = tamanio-1),
            se = desvioIngreso/sqrt(tamanio),
            margen = cuantil95*se,
            Li = mediaIngreso - margen,
            Ls = mediaIngreso + margen)

# Graficamos los intervalos

mediasEduc %>% 
  ggplot(mapping = aes(x = educ, y = mediaIngreso)) +
  geom_point() +
  geom_errorbar(mapping = aes(ymin = Li, ymax = Ls)) +
  theme(axis.text.x = element_text(angle = 60))



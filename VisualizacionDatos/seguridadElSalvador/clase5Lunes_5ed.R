
# Modulo 5: visualizacion de datos

# Fecha: 18/05/2025

# Descripción: En este script vamos a elaborar un mapa un grafico de barras
# un grafico de distribución y un grafico de dispersión utilizando los datos
# de El Salvador que procesamos en el archivo 'clase5_desarrollo.R'

# Autor: Gerardo Esteban Gómez-Santiago


# limpieza del ambiente de trabajo ----------------------------------------

rm(list = ls())

# librerias ---------------------------------------------------------------

library(sf)
library(tidyverse)
library(classInt)
library(ggspatial)
library(patchwork)
library(extrafont)
library(ggtext)
library(haven)

# Elaboración de mapa -----------------------------------------------------------

# Cargamos la base de datos con la información por departamento

load(file = "base_mapa.RData")

# Verificamos el contenido, debe ser una base con 14 observaciones y 3 variables
# una de las variables debe contener las geometrias de los departamentos para poder
# mapearlos.

info_mapa %>% glimpse()

# Es muy facil hacer un mapa con ggplot2, solo necesitamos la variable 'geometry' para
# poder dibujar la forma de la unidad geografica deseada

info_mapa %>%                          # partimos de la base
  ggplot() +                           # conectamos con ggplot
  geom_sf(mapping = aes(fill = media)) # el geom para mapas es 'geom_sf' y pedimos que coloree a partir del valor de la variable 'media'


# Creación de paleta de colores

hcl.pals() # Conjunto de paletas predeterminadas del R

# Definimos los quiebres de la variable con la función 'classIntervals'

breaks <- classInt::classIntervals(var = info_mapa$media, # variable para determinar quiebres
                                   n = 8,                 # numero de quiebres
                                   style = "pretty")      # función para establecer el quiebre


breaks <- breaks$brks

# Definición de colores

colores <- hcl.colors(n = length(breaks), # cantidad de colores
                      palette = "Fall")   # paleta deseada (consulta en hcl.pals)

scales::show_col(colores) # Función para ver los colores seleccionados

# Definimos textura, lo que hacemos es incrementar la densidad de colores

textura <- colorRampPalette(colores)(400) 

scales::show_col(textura, labels = F)

# Creación del mapa

mapa <- info_mapa %>%                                     # partimos de la base
  ggplot() +                                              # conectamos con 'ggplot'
  geom_sf(mapping = aes(fill = media)) +                  # conectamos con 'geom_sf' donde especificamos que rellene a partir del valor de 'media'
  
  # Ahora definimos los colores que queremos usar dentro de 'scale_fill_grandientn()'
  
  scale_fill_gradientn(colors = textura,                  # colores a utilizar
                       name = "Proporción") +             # Nombre que queremos que tenga la leyenda
  
  # Incorporamos rosa de los vientos con 'annotation_north_arrow()' un función del paquete 'ggspatial'
  
  annotation_north_arrow(location = "bl",                                 # ubicación de la rosa (b de bottom, l de left)
                         width = unit(0.8, "cm"),                         # definimos un ancho
                         height = unit(1, "cm"),                          # definimos un alto
                         style = north_arrow_fancy_orienteering) +        # definimos un tipo de fleca o rosa de los vientos
  
  # Incorporamos modificaciones de estilo dentro del 'theme'
  
  theme(axis.text = element_blank(),                               # Quitamos texto de los ejes
        axis.ticks = element_blank(),                              # Quitamos rayitas negras de los ejes (ticks)
        legend.position = "inside",                                # redefinimos la ubicación de la leyenda
        legend.position.inside = c(0.8, 0.85),                     # establecemos en que parte interna del gráfico queremos la leyenda
        legend.direction = "horizontal",                           # establecemos que sea horizontal
        legend.title.position = "top",                             # establecemos que el titulo de la leyenda quede arriba
        legend.title = element_text(hjust = 0.5),                  # que el titulo de la leyenda este centrado
        legend.key.width = unit(0.8, "cm"),                        # definimos ancho de la leyenda
        legend.key.height = unit(0.2, "cm"),                       # definimos alto de la leyenda
        legend.background = element_rect(fill = alpha(colores[4],  # definimos color de fondo y contorno de la leyenda
                                                      0.4),
                                         color = colores[4]),
        plot.background = element_rect(fill = colores[6],          # definimos color de fondo de todo el gráfico   
                                       color = colores[6]),
        panel.background = element_rect(fill = colores[5]),        # definimos color de fondo de la zona donde esta el mapa
        panel.grid = element_line(color = colores[3],              # definimos estilo y color de las lineas de la grilla.
                                  linetype = "longdash") 
        )




# elaboración del grafico de barras ---------------------------------------

barras <- info_mapa %>%                                     # partimos de la base de datos
  ggplot(mapping = aes(x = media*100,                       # en el eje x graficamos la variable continua
                       y = reorder(ADM1_ES, media))) +      # en el eje y graficamos la variable con los nombres de los departamentos, reorganizada por el valor de la media
  geom_col(mapping = aes(fill = media),                     # pedimos que se coloreen las barras a partir del valor de la media
           color = "black",                                 # que el borde de las barras sea negro
           show.legend = F,                                 # que no aparezca leyenda
           width = 0.8) +                                   # que el ancho de las barras sea un poco menor al valor por defecto
  
  # Añadimos texto al final de las barras mostrando el valor del porcentaje
  
  geom_text(mapping = aes(x = media*100 + 3,                        # ubicación del texto en el eje x
                          label = str_c(round(media*100, 1), "%")), # texto a colocar en el gráfico
            size = 2.3,                                             # tamaño del texto
            family = "Cambria") +                                   # tipo de fuente
  
  # Seleccionamos los colores a utilizar, deberan ser los mismos del mapa
  
  scale_fill_gradientn(colors = textura) +
  
  # seleccionamos los valores que queremos que aparezcan en el eje x
  
  scale_x_continuous(breaks = c(0,breaks[c(2,4,6,8)])*100,                         # apareceran quiebres en 0, 20, 40, 60 y 80
                     labels = str_c(round(c(0,breaks[c(2,4,6,8)])*100,1), "%"),    # Que aparezcan con el simbolo de '%'
                     limits = c(0, 46)) +                                          # limites que debe tener en cuenta el gráfico
  
  # Titulos
  
  labs(y = NULL,                                         # Sin titulo en el eje y
       x = "Variación en el\nporcentaje de acuerdo") +   # Titulo en el eje x
  
  # Adecuación de estilos con 'theme'
  
  theme(axis.text = element_text(color = "black"),
        plot.background = element_rect(fill = colores[6],
                                       color = colores[6]),
        panel.background = element_rect(fill = colores[5]),
        panel.grid = element_line(color = alpha(colores[3],
                                                0.3),
                                  linetype = "longdash"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


# Libreria para tener a disposición mas tipos de fuente

# install.packages("extrafont")
# extrafont::font_import()

fonts()

# Pegado de la parte superior de los gráficos -----------------------------

# Para esto utiizamos la librería patchwork

compuesto <- wrap_plots(barras, mapa,         # graficos a pegar   
                        ncol = 2,             # especificamos que sea uno al lado del otro al decirle que sean dos columnas
                        widths = c(0.4,1)) +  # Ancho a ocupar por cada grafico
  
  # Introducimos un titulo y subtitulo comun a ambos graficos
  
  plot_annotation(title = "Cambio en la percepción ciudadana de seguridad en El Salvador - 2018 vs 2023\n", 
                  subtitle = "A la pregunta **'Considerando la seguridad en su colonia o comunidad, ¿los mienbros de este hogar pueden salir de noche?'** En todos los<br>departamentos se observó un incrementó en el porcentaje de acuerdo del 2023 respecto al 2018, siendo <span style = 'color:#C7522B;'>**San Salvador**</span> el caso mas significativo.<br>") &
  
  # Introducimos corrección de estilo del grafico con 'theme'
  
  theme(text = element_text(family = "Cambria"),            # tipo de fuente de todo el gráfico
        plot.subtitle = element_markdown(),                 # especificamos 'element_markdown()' para que capture el cambio en el color y negrita de una parte del subtitulo
        plot.background = element_rect(fill = colores[6],   # Color de fondo del grafico
                                       color = colores[6]),
        plot.title = element_text(hjust = 0.5,              # Adecuaciones del formato del titulo
                                  size = 18,
                                  face = "bold"))

# Exportamos con ggsave()

ggsave(filename = "seguridad_El_Salvador2.png",
       plot = compuesto,
       dpi = 500,
       width = 10,
       height = 5)


# Elaboración gráfico de Distribución -----------------------------------------------

# cargamos la base con la información por distritos

base_mun <- readxl::read_excel(path = "base_municipios.xlsx")

base_mun %>% glimpse()

scales::show_col(colores)

distribuciones <- base_mun %>%                    # partimos de la base
  ggplot() +                                      # conectamos con ggplot()
  
  # definimos el primer grafico de densidad con la información de 2018
  
  geom_density(mapping = aes(x = prop_18*100),    # definimos eje x
               fill = colores[1],                 # relleno de la función de densidad
               alpha = 0.8,                       # transparencia
               color = colores[1]) +              # color del borde de la función de densidad
  
  # definimos la segunda función de densidad con la información de 2023
  
  geom_density(mapping = aes(x = prop_23*100), 
               fill = colores[9],
               alpha = 0.7,
               color = colores[9]) +
  
  # Añadimos una linea vertical con el valor medio de 2018
  
  geom_vline(xintercept = mean(base_mun$prop_18)*100, # punto de intercepción en el eje x
             color = colores[1],                      # color de la linea
             linetype = "longdash",                   # tipo de linea
             linewidth = .9) +                        # ancho de la linea
  
  # Añadimos una linea vertical con el valor medio de 2023
  
  geom_vline(xintercept = mean(base_mun$prop_23, 
                               na.rm = T)*100,        # intercepto en el eje x
             color = colores[9],                      # color de la linea
             linetype = "longdash",                   # tipo de linea
             linewidth = .9) +                        # ancho de la linea
  
  # añadimos texto con el valor medio para cada año
  
  annotate(geom = "text",
           x = mean(base_mun$prop_18)*100 - 9,
           y = 0.04,
           label = str_c("Media 2018:\n", round(mean(base_mun$prop_18)*100,1), "%"),
           color = colores[1],
           size = 3.,
           family = "Cambria") +
  annotate(geom = "text",
           x = mean(base_mun$prop_23, na.rm = T)*100 - 10,
           y = 0.07,
           label = str_c("Media 2023:\n", round(mean(base_mun$prop_23, na.rm = T)*100,1), "%"),
           color = colores[9],
           size = 3.,
           family = "Cambria") +
  
  # modificamos escala del eje y y del eje x
  
  scale_y_continuous(n.breaks = 10) +
  scale_x_continuous(breaks = c(0.1,0.3,0.5,0.7,0.9,1)*100,
                     labels = str_c(c(0.1,0.3,0.5,0.7,0.9,1)*100, "%"),
                     limits = c(0,1)*100) +
  
  # Insertamos titulos
  
  labs(y = "Densidad",
       x = "Distribución del porcentaje de acuerdo municipal",
       subtitle = "El cambio en la distribución del porcentaje de acuerdo por municipio<br>del <span style = 'color:#C7522B;'>**2023**</span> respecto al <span style = 'color:#3C5941;'>**2018**</span> hace más evidente el **crecimiento**<br>en la percepción ciudadana de seguridad.") +
  
  # ajustamos estilo del gráfico con comandos dentro del 'theme'
  
  theme(text = element_text(family = "Cambria"),
        axis.text = element_text(color = "black"),
        plot.subtitle = element_markdown(color = "gray40"),
        plot.background = element_rect(fill = colores[6],
                                       color = colores[6]),
        panel.background = element_rect(fill = colores[5]),
        panel.grid = element_line(color = alpha(colores[3],
                                                0.3),
                                  linetype = "longdash"))



# elaboracion grafico de dispersion ---------------------------------------

# calculo de la regresión que deseamos gráficar, en este caso es significativa

reg <- lm(base_mun$porcentaje ~ base_mun$cambio2)
summary(reg)

# elaboración del grafico

dispersiones <- base_mun %>%                     # partimos de la base
  ggplot(mapping = aes(x = cambio2*100,          # definimos eje x
                       y = porcentaje)) +        # definimos eje y
  
  # geom_point para el gráfico de dispersión
  
  geom_point(shape = 21,                   # forma de los puntos
             fill = colores[3],            # relleno de los puntos
             color = "black",              # borde de los puntos
             size = 3,                     # tamaño de los puntos
             alpha = 0.6) +                # transparencia de los puntos
  
  # Añadimos recta de ajuste
  
  geom_smooth(method = "lm", 
              se = F,                 # sin erro estandar
              color = colores[9],     # color de la recta
              linetype = "dashed") +  # tipo de linea
  
  # Titulos y subtitulos
  
  labs(subtitle = "Este incremento en la percepción de seguridad, alcanza a tener un efecto<br>**positivo y significativo** en el porcentaje obtenido por <span style = 'color:#00B0F6;'>**Nayib Bukele**</span> en cada<br>municipio, durante las elecciones de 2024",
       x = "Variación municipal en la percepción de seguridad",
       y = "Porcentaje de votos partido <span style = 'color:#00B0F6;'>**Nuevas Ideas**</span>") +
  
  # ajustes de escala en eje x y eje y
  
  scale_y_continuous(breaks = c(40,50,60,70,80,90)) +
  scale_x_continuous(breaks = seq(0,0.75,0.25)*100,
                     labels = str_c(seq(0,0.75,0.25)*100, "%")) +
  
  # ajuste de estilo dentro del 'theme'
  
  theme(text = element_text(family = "Cambria"),
        axis.title.y = element_markdown(),
        axis.text = element_text(color = "black"),
        plot.subtitle = element_markdown(color = "gray30"),
        plot.background = element_rect(fill = colores[6],
                                       color = colores[6]),
        panel.background = element_rect(fill = colores[5]),
        panel.grid = element_line(color = alpha(colores[3],
                                                0.3),
                                  linetype = "longdash"))
  


# Pegado de la parte inferior de los graficos -----------------------------

compuesto2 <- wrap_plots(distribuciones, dispersiones,   # graficos 
                         ncol = 2,                       # uno al lado del otro al definir 2 columnas
                         widths = c(0.9,1)) +            # ancho a ocupar por cada grafico
  
  # Pie de pagina, en este caso el mismo para ambos graficos
  
  plot_annotation(caption = "Elaboración: @GEsteban_Gomez | Fuente: EPHM 2018 y 2023 El Salvador\n*El análisis incluye 226 de 262 distritos, la unidad territorial analoga a municipios en la mayoria de países de LATAM") &
  
  # Ajuste de estilo para todo el panel inferior
  
  theme(text = element_text(family = "Cambria"),
        plot.subtitle = element_markdown(),
        plot.caption = element_text(hjust = 0),
        plot.background = element_rect(fill = colores[6],
                                       color = colores[6]))


compuesto2


# pegado de los graficos en 4 paneles -------------------------------------


compuesto_final <- wrap_elements(compuesto) / wrap_elements(compuesto2) +
  theme(plot.background = element_rect(fill = colores[6],
                                       color = colores[6]))

# Exportamos el resultado con ggsave()

ggsave(filename = "seguridad_El_Salvador3.png",
       plot = compuesto_final,
       dpi = 500,
       width = 11,
       height = 11)

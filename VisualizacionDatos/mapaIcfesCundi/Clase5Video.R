

# Modulo 5: Visualizacion de datos con ggplot2 ----------------------------
# En este script vemos como generar una infografía estatica usando
# la librería {ggplot2}, analizaremos la brecha de genero en los resultados
# globales de la prueba saber11 en los municipios de Cundinamarca.
# Uno de los gráficos es de intervalos de confianza y el otro es un mapa.

# Limpieza del ambiente de trabajo ----------------------------------------

rm(list = ls())

# Directorio de trabajo ---------------------------------------------------

getwd()
setwd("E:/Documents/Escritorio/tidyverso/tidyverso/cursoRLunes/RLunes/scripts/bases/taller3/bases")

# Librerias ---------------------------------------------------------------

library(tidyverse)
library(ggtext)
library(extrafont) # Paquete para cambiar tipos de fuente
library(classInt)  # Paquete para generar intervalos
library(sf)        # Paquete para cargar datos espaciales  
library(ggspatial) # Extensión de ggplot2 para agregar detalles a mapa
library(patchwork) # Extensión de ggplot2 para pegar graficos.


# carga de datos ----------------------------------------------------------

# Los datos del grafico de intervalos de confianza estan almacenados
# en la base 'tablaMedias.RData', solo debemos cargarla con el comando
# load() y ya esta lista para la visualización.

load(file = "tablaMedias.RData")


# VISUALIZACIÓN -----------------------------------------------------------

intervalos <- tabla_medias %>% # Parto de una base de datos
  ggplot(mapping = aes(x = ESTU_GENERO, y = media, color = ESTU_GENERO)) + # Defino ejes y colores dentro del 'aes()'  
  geom_point(mapping = aes(shape = ESTU_GENERO),                           # Con geom_point() pido que grafique el estimador puntual de la media, con 'shape' pido una forma particular en función del genero
             show.legend = F) +                                            # show.legend = F para que quite la leyenda de la derecha
  geom_errorbar(mapping = aes(ymin = Li, ymax = Ls),                       # geom_errorbar() para graficar los intervalos de confianza
                show.legend = F) +                                         # show.legend = F para que quite la leyenda de la derecha
  scale_color_manual(values = c("#d96831","#274862")) +                    # Elijo los colores para mujeres y para hombres
  scale_shape_manual(values = c(15, 17)) +                                 # Elijo la forma de los puntos
  facet_wrap(~ prueba,                                                     # Con facet_wrap() genero la diferencición del grafico por una variable categorica en paneles, en este caso por la variable 'prueba'
             scales = "free_y",                                            # "free_y" para que las medidas de los paneles en el 'eje y' puedan diferenciarse
             ncol = 6) +                                                   # ncol = 6 para que organice los paneles en 6 columnas
  
  # 'labs' es para añadir titulos, subtitulos, titulos a los ejes y pies de pagina
  
  labs(title = "Brecha de Género en la prueba Saber11 - Cundinamarca 2023",      
       y = "Puntajes promedio*",
       x = NULL,
       subtitle = "En Cundinamarca se observa una brecha en el resultado global de 8.1 puntos<br>entre <span style='color:#274862;'>**Hombres**</span> y <span style='color:#d96831;'>**Mujeres**</span>. Siendo **Matemáticas** la materia con mayor brecha<br>",
       caption = "\n*La puntuación 'Global' es la suma de la puntuación de la totalidad de temáticas\nIntervalos a un nivel de confianza del 95%\nFuente: Data Icfes | Elaboración: Gerardo Esteban Gómez-Santiago") +
  
  # En 'theme' realizo las especificaciones de forma del gráfico
  
  theme(text = element_text(family = "Cambria"),                                 # Tipo de fuente (debe estar activado 'extrafont')
        plot.title = element_text(hjust = 0.5, color = "thistle4", size = 16),   # Manipulación del titulo
        plot.subtitle = element_markdown(color = "gray30",                       # Manipulacion del subtitulo
                                         margin = margin(t = 0, r = 0,           # 'margin' me permite controlar los margenes en torno al subtitulo (es valido tambien en titulos de eje, en titulo y en pie de pagina)
                                                         l =0, b=20)),
        plot.caption = element_text(hjust = 0),                                  # Especifico alineacion del pie de pagina
        axis.title.y = element_text(margin = margin(t = 0, r = 15,               
                                                    l =0, b=0)),
        plot.background = element_rect(fill = "thistle2"),                       # Especifico color de fondo el grafico
        panel.background = element_rect(fill = "thistle3"),                      # Especifico color de la grilla del grafico
        panel.grid = element_line(color = "gray80",                              # Especifico forma y color de las lineas de la grilla del grafico
                                  linetype = "longdash"),                        
        strip.background = element_rect(fill = "black",                          # Especifico color de fondo de los recuadros donde salen los titulos de los recuadros de 'prueba'
                                        color = "gray40"),                       
        strip.text = element_text(color = "white")) +                            # Especifico letra de los recuadros de 'prueba'
  scale_y_continuous(n.breaks = 10)                                              # Añado mas quiebres en el eje y.



# Exporto el grafico con 'ggsave()'

ggsave(filename = "intervalosClase5.png",    # Nombre del archivo en mi directorio de trabajo
       plot = intervalos,                    # Como guarde el grafico en mi ambiente de trabajo (nombre del objeto)
       dpi = 300,                            # Calidad de los pixeles (normalmente uso 500)
       width = 8,                            # Especificaciones de ancho
       height = 5)                           # Especificaciones de alto




# Visualización de mapa ---------------------------------------------------

# Para la realizacion del mapa los datos estan procesados en el archivo 'datos_mapa.RData'
# es un archivo que contiene la variable 'geometry' la cual especifica los poligonos o la 'forma'
# de la unidad territorial bajo analisis, en este caso los municipios del departamento de 
# Cundinamarca.

# Cargamos los datos con 'load()'

load(file = "datos_mapa.RData")


# Creacion de paleta ------------------------------------------------------

# En el mapa lo que haremos es colorear los municipios a partir
# del valor de una variable continua ('brecha'), utilizaremos la 
# paltea 'SunsetDark' de 'hcl.pals'


# Generamos quienres segun valores de la variable continua a graficar

breaks <- classIntervals(var = geo$brecha,  # Variable
                         n = 10,            # Numero de quiebres
                         style = "pretty")  # Estilo

# Me quedo solo con los 'brks'

breaks <- breaks$brks


hcl.pals(type = "sequential")  # Función para consultar paletas disponibles

# Establecimiento de colores, eligiendo uno por quiebre

color <- hcl.colors(n = length(breaks),      # Numero de colores
                    palette = "SunsetDark")  # Paleta seleccionada

scales::show_col(color) # Funcion para ver los colores seleccionados

# Ampliacion de colores posibles, generacion de 'textura'

textura <- colorRampPalette(colors = color)(600)  # El numero entre parentesis es el numero de tonalidades que deseo en este caso 600
textura <- rev(textura)                           # Invierto el orden de los colores

scales::show_col(textura, labels = F)             # Veo las 600 tonalidades a utilizar


# Creacion de una base con la información del valor minimo y el maximo para puntos y etiquetas

coord_min_max <- geo %>% 
  filter(brecha == min(brecha) | brecha == max(brecha)) %>% 
  st_centroid()

# Genero la leyenda ('nombreMunicipio: valor de la brecha')

coord_min_max <- coord_min_max %>% 
  mutate(label = str_c(str_to_title(MPIO_CNMBR), # str_c() permite concatenar texto mientras str_to_title() convierte las palabras en la primera con mayuscula, el resto en minuscula 
                       ": ", 
                       round(brecha, 1)))

mapa <- geo %>%                                                                  # Parto de una base de datos
  ggplot() +                                                                     # No hace falta especificar nada en el {ggplot2}
  geom_sf(mapping = aes(fill = brecha)) +                                        # geom_sf() para visualizar mapa y en 'fill' la variable a partir de la cual quiero colorear el mapa
  scale_fill_gradientn(name = "Brecha",                                          # scale_fill_gradientn() es para poner como color la textura generada previamente como colores dentro del mapa
                       colors = textura,
                       breaks = breaks,
                       label = round(breaks, 1),
                       limits = c(min(geo$brecha),
                                  max(geo$brecha))) +
  geom_sf(data = coord_min_max,                                                  # Este geom_sf adicional es para los puntos en los municipios que presentan el valor maximo y minimo
          color = c(color[1], color[13]),
          shape = 13,
          alpha = 0.8) +
  annotate(geom = "text",                                                        # Este annotate() es para la leyenda de los municipios Jerulasen y Venecia con sus respectivos valores (minimo y maximo) de brecha entre los municipios
           x = c(-74.75, -73.9),                                                 # Coordenadas en x
           y = c(4.08, 3.85),                                                    # Coordenadas en y
           label = coord_min_max$label,                                          # Texto a utilizar
           family = "Cambria",                                                   # Tipo de fuente
           size = 2.5,                                                           # Tamaño de letra
           color = c(color[13], color[1])                                        # Colores de la leyenda
           ) +
  
  # Los siguientes annotate() contienen las especificaciones para generar las flechas
  # que señalan el centroide de los municipios Jerusalen y Venecia y lo conectan con
  # la leyenda del anterior annotate()
  
  annotate(geom = "segment",                                                     # Especifico 'segment' 
           x = -74.68916,                                                        # valor de inicio en x
           y = 4.1,                                                              # valor de inicio en y
           xend = -74.68916,                                                     # valor de llegada en x
           yend = 4.551349,                                                      # Valor de llegada en y
           arrow = arrow(length = unit(0.5, "cm")),                              # especifico uso de flecha
           color = color[1]) +                                                   # especifico color de la flecha
  annotate(geom = "segment",
           xend = -74.4026,    
           yend = 4.063342,     
           x = -74.,       
           y = 3.9,
           arrow = arrow(length = unit(0.5, "cm")), 
           color = color[13]) +
  
  # labs() para titulos como en el grafico anterior
  
  labs(title = "Distribución territorial de la brecha",
       subtitle = "Puntaje global en municipios de Cundinamarca\n2023",
       x = NULL,
       y = NULL) +
  
  # con el siguiente comando añado la rosa de los vientos
  
  annotation_north_arrow(location = "tr",                            # 'tr' de top-rigth si quieren otra localización usen 'tl', 'br' o 'bl'
                         width = unit(0.9, "cm"),                    # Ancho de la rosa
                         height = unit(1.2, "cm"),                   # Alto de la rosa
                         style = north_arrow_fancy_orienteering) +   # Estilo de la rosa
  
  # theme() para especificar cosas de forma en el grafico.
  
  theme(text = element_text(family = "Cambria"),                                # tipo de fuente
        plot.title = element_text(hjust = 0.5,                                  # especificaciones de titulo
                                  size = 16,       
                                  color = "thistle4"),
        plot.subtitle = element_text(hjust = 0.5,                               # especificaciones de subtitulo
                                     color = "gray30"),
        legend.position = "bottom",                                             # posicion de la leyenda
        legend.key.width = unit(1, "cm"),                                       # ancho de la leyenda
        legend.key.height = unit(0.2, "cm"),                                    # alto de la leyenda
        legend.background = element_rect(fill = "gray90"),                      # color de fondo de la leyenda
        plot.background = element_rect(fill = "thistle2"),                      # Color de fondo del grafico 
        panel.background = element_rect(fill = "thistle3"),                     # Color de fondo de la grilla
        panel.grid = element_line(linetype = "longdash",                        # tipo y color de las lineas de la grilla 
                                  color = "gray80"),
        axis.text = element_blank(),                                            # elimino texto de x y y
        axis.ticks = element_blank()                                            # elimino cortes (rayitas negras de los ejes)
        )



# Pegado de los graficos --------------------------------------------------

final <- wrap_plots(intervalos,            # Grafico 1
                    mapa,                  # Grafico 2
                    nrow = 1,              # Organizacion de los graficos en 2 columnas
                    widths = c(1,          # Ancho a ocupar por grafico 1 
                               0.7)        # Ancho a ocupar por grafico 2
                    )

# Exporto el grafico con ggsave()

ggsave(filename = "final3.png",       # Nombre del grafico en el directorio de tabajo
       plot = final,                  # Nombre del objeto a exportar en R
       dpi = 500,                     # Calidad
       width = 12,                    # Ancho
       height = 7)                    # Alto




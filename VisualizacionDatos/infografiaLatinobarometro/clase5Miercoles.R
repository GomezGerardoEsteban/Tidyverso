
# Modulo 5: Visualizacion de datos con ggplot2

# Para esta visualización utilizaremos datos de latinobarometro previamente procesados
# y almacenados en las bases 'baseFinal.RData' y 'dataMapa.RData', el script con los comandos
# previos para preparar esas bases se llama 'scriptBasesInfo.R' y lo pueden encontrar en este
# mismo repositorio de GitHub.

# limpieza del ambiente de trabajo ----------------------------------------

rm(list = ls())


# directorio de trabajo ---------------------------------------------------

setwd("E:/Documents/Escritorio/tidyverso/tidyverso/cursoRLunes/RMiercoles/bases")


# librerias ---------------------------------------------------------------

install.packages("sf")           # para la elaboración de mapas
install.packages("giscoR")       # Contiene información con los polygonos de los paises
install.packages("ggtext")       # Me da ciertas aplicaciones de diseño en el texto de los graficos
install.packages("ggspatial")    # Extensión para incorporar elementos al mapa como rosa de los vientos y escalas
install.packages("patchwork")    # Extensión para pegar mapas
install.packages("extrafont")    # Extensión para cambiar tipos de fuente
install.packages("classInt")     # Paquete para calcular intervalos de manera simple

extrafont::font_import() # Esta funció se encarga de importar los tipos de fuente del computador
                         # al R, su ejecucuón puede tomar unos minutos, una vez ejecutado, no es necesario
                         # volverlo a ejecutar en usos futuros.

# Activación de librerias

library(tidyverse)
library(giscoR)
library(ggtext)
library(ggspatial)
library(patchwork)
library(extrafont)
library(classInt)

# Carga de los datos

load(file = "baseFinal.RData")  # Este archivo carga una base de nombre 'tFinal'
load(file = "dataMapa.RData")   # Este archivo carga una base de nombre 'mapa'


# generación de paleta de colores -----------------------------------------

# Calculo los quiebres para los colores

breaks <- classIntervals(var = mapa$frecRel,   # variable a partir de la cual genero los colores
                         n = 8,                # Cantidad de colores
                         style = "pretty")     # Esta opción de estilo busca generar quiebres para hacer clara la varianza de los datos

breaks <- breaks$brks # Guardo solo los quiebres

# LLamo lo colores, en este caso, un color por cada quiebre generado

colores <- hcl.colors(n = length(breaks),  # Numero de colores
                      palette = "Earth")   # paleta a utilizar, otras opciones de paletas pueden verse en 'hcl.pals()'

scales::show_col(colores) # Me muestra los colores que selecciones

# Genero una textura para reflejar mejor la varianza de una variable continua a partir
# de los colores

textura <- colorRampPalette(colors = colores)(600)

scales::show_col(textura, labels = F) # veo la textura que usare para colorear


# Mapa --------------------------------------------------------------------

# Para elaborar un mapa, debo tener la información geografica de las unidades de observacion
# a graficar, en este caso, la base 'mapa', contiene los poligonos de los países incluidos en
# la encuesta de latinobarometro.

g1 <- mapa %>%                                                                   # Parto de una base de datos
  ggplot() +                                                                     # llamo la funcion ggplot
  geom_sf(                                                                       # especifico geom_sf para mapas
    mapping = aes(fill = frecRel),                                               # Pido que rellene o coloree los paises en funcion de la variable 'frecRel'
    show.legend = F                                                              # Elimino la leyenda del costado
    ) +
  scale_fill_gradientn(                                                          # scale_fill_gradientn me permite colorear el mapa a partir de la textura generada
    name = "% Acuerdo",                                                          # el nombre que deseo que tenga la leyenda
    colors = textura                                                             # especifico los colores
    ) +
  annotation_north_arrow(                                                        # annotation_north_arrow nos permite incorporar la rosa de los vientos
    location = "tr",                                                             # Especifico el lugar, en este caso "tr" de top-right
    width = unit(0.6, "cm"),                                                     # Módifico el ancho
    height = unit(0.7, "cm"),                                                    # Módifico el largo
    style = north_arrow_fancy_orienteering                                       # Especifico un estilo distinto al que viene por defecto
    ) +
  # labs(                                                                        # Si quisieramos añadir titulos lo hariamos con 'labs'
  #   title = "Mapa - Latinobarometro",                                          # titulo
  #   caption = "Fuente: Latinobarometro 2023"                                   # pie de pagina
  # ) +
  theme(                                                                         # El theme me permite manipular toda la parte de diseño del grafico
    axis.text = element_blank(),                                                 # Quito el texto indicando las coordenadas
    axis.ticks = element_blank(),                                                # Quitamos las lineas negras pequeñas que aparecen en el eje
    panel.background = element_rect(fill = alpha(colores[7], 0.3)),              # Especificamos un color de fondo para la parte en que esta dibujado el mapa
    plot.background = element_rect(fill = alpha(colores[5], 0.4),                # Especificamos un color de fondo para todo el gráfico (seria como el marco del grafico)
                                   color = alpha(colores[5], 0.4)),              # Esto modifica el color del borde de la grafico
    panel.grid = element_line(linetype = "longdash",                             # Manipulamos la linea de la grilla donde esta el mapa, cambiamos el tipo de linea
                              color = colores[9])                                # Cambiamos el color de la linea
    )


extrafont::fonts() # Con este comando vemos los tipos de fuentes disponibles


# Grafico de barras al lado del mapa --------------------------------------

g2 <- mapa %>%                                                                   # Parto de una base de datos
  ggplot(mapping = aes(x = frecRel*100,                                          # especifico eje x
                       y = reorder(                                              # especifico eje y, pero reorganizado por el valor de 'frecRel' usando la función 'reorder'
                         idenpa,                                                 # En el eje y apareceran el nombre de los paises
                         frecRel                                                 # Variable a partir de la cual se reorganiza el eje y
                         ))) +
  geom_col(                                                                      # geom_col para generar graficos de barras
    mapping = aes(fill = frecRel),                                               # rellenamos las barras para que tengan el mismo color que el mapa
    color = "black",                                                             # especificamos que el borde de la grafica sea negro
    show.legend = F                                                              # Quitamos leyenda del costado
    ) +                                                           
  geom_text(                                                                     # geom_text para añadir etiquetas de texto con el porcentaje para cada pais
    mapping = aes(x = frecRel*100 + 5,                                           # especificamos que el eje x se desplace algunas unidades hacia la derecha para no estar encima de la barra
                  label = str_c(round(frecRel*100, 1), "%")),                    # La etiqueta que queremos poner es el valor porcentual, con 'round' quitamos decimales y con 'str_c' pegamos el simbolo de %
    size = 2.5,                                                                  # tamaño de la letra
    family = "Century"                                                           # tipo de fuente
    ) +
  labs(                                                                          # labs para titulos
    y = NULL,                                                                    # que no haya titulo en el eje y
    x = "% de Acuerdo"                                                           # cambiamos el titulo del eje x
    ) +
  scale_fill_gradientn(colors = textura) +                                       # especificamos que el color de relleno sea a partir de 'textura'
  scale_x_continuous(n.breaks = 10) +                                            # incrementamos el numero de quiebres en el eje x
  theme(                                                                         # theme para manipular estetica del grafico
    panel.grid.major.x = element_blank(),                                        # quitamos lineas verticales de la grilla
    panel.grid.minor.x = element_blank(),                                 
    panel.grid.major.y = element_line(                                           # modificamos el color y el tipo de linea horizontal de la grilla
      linetype = "longdash",                                                     # tipo de lines
      color = alpha(colores[9], 0.3)),                                           # color de la linea
    plot.background = element_rect(                                              # Color de fondo de todo el grafico (marco del grafico)
      fill = alpha(colores[5], 0.4),
      color = alpha(colores[5], 0.4)),
    panel.background = element_rect(                                             # color de la grilla donde estan las barras
      fill = alpha(colores[7], 0.3)
      )
    )



# pegado de mapa y barras -------------------------------------------------

# para esto utilizamos la función wrap_plots de patchwork

compuesto <- wrap_plots(
  g2,                    # barras
  g1,                    # mapa
  nrow = 1,              # pedimos que lo organice en una sola fila, es decir, uno al lado del otro
  widths = c(0.7,        # en widths especificamos que es de la izquierda (barras) tenga el 70% de ancho, del mapa
             1)          # mapa unidad completa de ancho
  ) +
  plot_annotation(                                                              # plot_annotation me permite poner titulo, subtitulos y pies de pagina comunes a ambos graficos
    title = "Tolerancia a la democracia en América Latina",                     # especificamos el titulo y subtitulo, el subtitulo tiene algunos comandos extraños que permiten manipular el color del texto por partes
    subtitle = "Porcentaje de acuerdo con la afirmacion<br>                     
    <span style = 'color:black;'>La democracia puede tener sus problemas, 
    pero es el mejor sistema de gobierno.</span>") &                            # Note que la conexión con las especificaciones de theme en este caso es con un &
  theme(                                        
    text = element_text(family = "Century"),                                    # Pedimos un tipo de fuente particular para todo el grafico
    plot.title = element_text(hjust = 0.5,                                      # Especificamos el titulo centrado
                              size = 16),                                       # especificamos tamaño 16
    plot.subtitle = element_markdown(color = "gray30"),
    plot.background = element_rect(fill = alpha(colores[5], 0.7),
                                   color = alpha(colores[5], 0.2)),
    panel.background = element_rect(fill = alpha(colores[7], 0.3))
    )

# Exportamos el gráfico

ggsave(filename = "graficoFinal.png",  # nombre del archivo en el directorio de trabajo
       plot = compuesto,               # grafico a exportar (objeto en el ambiente de trabajo)
       dpi = 500,                      # calidad de pixeles, siempre uso 500
       width = 5*1.848291,             # especificaciones de ancho
       height = 5                      # especificaciones de alto
       )

8.65/4.68 # ratio ancho/alto



# visualizaciones inferiores ----------------------------------------------


# Visualización comparando sexo -------------------------------------------

# creamos una base donde no esta nicaragua y cuya organización nos facilita poner los puntos
# por categoria de sexo (General, Hombre, Mujer)

viz <- tFinal %>%                                     # partimos de tFinal
  filter(iso3c != "NIC") %>%                          # Quitamos a Nicaragua por no tener valor para el 2023
  select(idenpa, iso3c, sexo, frecRel, year) %>%      # seleccionamos solo ciertas variables
  spread(key = sexo, value = frecRel)                 # convertimos en formato ancho usando como variable pivote 'sexo'

viz  

g3 <- viz %>% 
  ggplot(mapping = aes(x = General, y = reorder(idenpa, General))) +
  geom_point(                                                          # especificamos un geom_point y lo primero que graficamos es el valor General
    color = alpha("olivedrab4", 0.4)                                   # especificamos como color un verde oliva
    ) +
  geom_point(                                                          # especificamos un segundo geom_point con el valor de las mujeres
    mapping = aes(x = Mujer,                                           
                  y = idenpa),
    color = "maroon"                                                   # Color de los puntos de mujeres
    ) +
  geom_point(                                                          # especificamos un tercer geom_point con el valor de los hombres
    mapping = aes(x = Hombre,                                        
                  y = idenpa),
    color = "slateblue"                                                # Color de los puntos de hombres
    ) +
  geom_segment(                                                        # Añadimos un segmento de linea que una los puntos
    mapping = aes( 
      x = Mujer,                                                       # En el eje x empieza en el valor de mujer
      y = idenpa,                                                      # En el eje y se mantiene siempre en la misma coordenada, pais
      xend = Hombre,                                                   # En el eje y termina en el valor de hombre
      yend = idenpa                                                    # En el eje y se mantiene siempre en la misma coordenada, pais
      ),
    linetype = "dotted",                                               # especificamos que el tipo de linea sea punteada
    color = colores[1]) +                                              # el color es el primer color de nuestra paleta
  facet_wrap(
    ~ year                      # Este facet_wrap es fundamental para la visualización, sin este no podemos hacer la separacion por años
    ) +                                 
  
  # Añadimos titulos 
  
  labs(subtitle = "Mientras en el 2020 el porcentaje de aprobación de los <span style = 'color:slateblue;'>Hombres</span><br> fue en todos los paises mayor al de las <span style = 'color:maroon;'>Mujeres</span>, para el 2023<br>en Chile y en Argentina esto sde invirtió.",
       y = NULL,
       x = "Porcentaje de acuerdo") +
  
  # Manipulamos diseño del grafico
  
  theme(text = element_text(family = "Century"),
        plot.subtitle = element_markdown(size = 10),
        panel.grid.major.x = element_line(linetype = "longdash",
                                          color = alpha(colores[9], 0.3)),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = alpha(colores[5], 0.4)),
        panel.background = element_rect(fill = alpha(colores[7], 0.3)),
        strip.background = element_rect(fill = alpha(colores[1],0.4),     # Esto me permite manipular el color de las cajas donde aparece el valor de los años
                                        color = colores[1]),
        strip.text = element_text(color = colores[9]))                    # Esto me permite manipular el texto



# Visualizacion variacion porcentual --------------------------------------

# Creamos una base que nos permita calcular la variacion general del 2023 respecto al 2020

viz2 <- viz %>%                                  # Partimos de la base para la visualizacion anterior
  select(-Hombre, -Mujer) %>%                    # nos quedamos con la columna que tiene el valor general
  spread(key = year, value = General) %>%        # ahora expandimos la base a partir de la variable año como pivote
  mutate(variacion = (`2023` - `2020`)*100)      # realizamos el calculo de la variación por pais entre 2020 y 2023

viz3 <- viz2 %>%                                        # Añadimos ahora un lugar para las etiquetas
  mutate(lugar_etiqueta = ifelse(test = variacion > 0,  
                                 yes = -1.2,            # si tuvo variación positiva que la leyenda tenga un valor negativo
                                 no = 1.2))             # si tuvo variación negativa que la leyenda tenga un valor positivo


g4 <- viz2 %>% 
  ggplot(mapping = aes(x = variacion, y = reorder(iso3c, variacion))) +
  geom_col(mapping = aes(fill = variacion),
           show.legend = F,
           color = "black") +
  scale_fill_gradientn(colors = textura) +
  geom_text(                                         # con este geom_text ponemos el codigo de los paises en las barras internas del grafico
    data = viz3,                                     # Especificamos los datos que contienen la variable 'lugar_etiqueta'
    mapping = aes(x = lugar_etiqueta,                # Especificamos que esas leyendas aparezcan en lugar_etiqueta
                  y = reorder(iso3c, variacion),     # Pedimos que mantenga la organización de las barras que ya habiamos especificado al inicio del grafico
                  label = iso3c),                    # especificamos la leyenda
    family = "Century",
    size = 2.5,
    color = "gray20"
    ) +
  geom_text(                                         # Con este geom_text especificamos las etiquetas de las barras
    data = viz3,                                     # datos con la variable lugar_etiqueta
    mapping = aes(x = variacion - lugar_etiqueta,    # en x, la etiqueta debe aparecer en el valor de la barra menos el valor de lugar etiqueta
                  y = reorder(iso3c, variacion),     # especificamos orden del eje y que ya habiamos establecido en pasos uno y 2
                  label = str_c(round(variacion,1), "%")), # especificamos etiqueta
    family = "Century",
    size = 3
    ) +
  labs(subtitle = "Aunque el promedio de toda la región creció, en\n7 de 17 países hubo una disminución entre el\n2020 y el 2023",
       y = NULL,
       x = "Variación en el porcentaje de acuerdo") +
  theme(text = element_text(family = "Century"),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(linetype = "longdash",
                                          color = alpha(colores[9], 0.3)),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(linetype = "longdash",
                                          color = alpha(colores[9], 0.3)),
        plot.background = element_rect(fill = alpha(colores[5], 0.4),
                                       color = alpha(colores[5], 0.4)),
        panel.background = element_rect(fill = alpha(colores[7], 0.3)))



# Pegado de graficos inferiores -------------------------------------------

compuesto2 <- wrap_plots(g4, g3, nrow = 1, widths = c(0.8, 1))


# Pegado de grafico con las cuatro visualizaciones ------------------------

compuestoFinal <- wrap_elements(compuesto) / wrap_elements(compuesto2) + # Se debe utilizar el comando 'wrap_elements' cuando pegamos graficos que habian sido pegados antes.
  
  # Añadimos un pie de pagina comun a todos los graficos
  
  plot_annotation(caption = "Fuente: Latinobarometro 2023 | Elaboración: Gerardo Esteban Gómez-Santiago\n*El dato de Nicaragua corresponde al 2020") &
  
  # Acomodamos cuestiones de diseño 
  
  theme(text = element_text(family = "Century"),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_markdown(color = "gray30"),
        plot.caption = element_text(hjust = 0),
        plot.background = element_rect(fill = alpha(colores[5], 0.7),
                                       color = alpha(colores[5], 0.2)),
        panel.background = element_rect(fill = alpha(colores[7], 0.3)))

# Exportamos grafico con ggsave, es importante jugar con las dimensiones
# de width y height hasta obtener el resultado deseado

ggsave(filename = "compuestoFinal.png",
       plot = compuestoFinal,
       dpi = 500,
       width = 10,
       height = 11)

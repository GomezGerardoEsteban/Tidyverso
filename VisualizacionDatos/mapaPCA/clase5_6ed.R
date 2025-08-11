

# Modulo 5: visualizacion de datos con {ggplot2}
# Descripción: En este script abordaremos los comandos necesarios para personalizar 
#              visualizaciones utilizando la librería {ggplot2}, elaboraremos un mapa 
#              y un gráfico de dispersión detallado.
#              las bases necesarias para realizar la visualización estan contenidas en la
#              carpeta 'visualizacion.zip' son:
#              - IPEM.RData
#              - GDL-Subnational-HDI-data.csv
#              - MGN_DPTO_POLITICO.shp

# limpieza del ambiente de trabajo ----------------------------------------

rm(list = ls())

# directorio de trabajo ---------------------------------------------------

setwd("E:/Documents/Escritorio/tidyverso/tidyverso/cursoRLunes/RMiercoles/bases/visualizacion")

getwd()


# librerias ---------------------------------------------------------------

library(tidyverse)  # manipulación de bases y visualización de datos
library(sf)         # visualización de datos georeferenciados
library(ggspatial)  # incorporación de elementos espaciales (rosa de los vientos y escala)
library(extrafont)  # cambios de tipo de fuente en los graficos (si lo vas a instalar por primera vez, debes usar el comando 'extrafont::font_import()')
library(ggrepel)    # Etiquetas sin solapamiento
library(latex2exp)  # Opcional, incorporación de texto en los gráficos utilizando latex
library(patchwork)  # Pegado de gráficos

# carga de la base --------------------------------------------------------

load(file = "IPEM.RData") # Datos con variable 'cluster' e 'ind' a utilizar en los gráficos

mapa <- st_read(dsn = "MGN_DPTO_POLITICO.shp") # Base con los poligonos de los departamentos de Colombia


# join de datos construcBase y mapa ---------------------------------------

# Pegamos los datos de conbstrucBase a la base mapa para visualizar la categoría cluster
# en el mapa.

mapa <- mapa %>% 
  left_join(y = construcBase %>% 
              select(P1_DEPARTAMENTO, cluster),
            by = c("DPTO_CCDGO" = "P1_DEPARTAMENTO"))


# acomodamos datos a visualizar en el mapa --------------------------------

# creamos una variable tipo factor para la visualización del mapa

mapa <- mapa %>% 
  mutate(categoria = factor(ifelse(test = cluster == 1,
                                   yes = "Bajo",
                                   no = ifelse(test = cluster == 2,
                                               yes = "Alto",
                                               no = "Medio")), 
                            levels = c("Alto", "Medio", "Bajo")))


# elaboración del mapa ----------------------------------------------------

# Generamos paleta de colores

colores <- hcl.colors(n = 5, palette = "YlOrRd")

scales::show_col(colores) # Con este comando vemos los colores seleccionados


mapa_grafico <- mapa %>%     # Partimos de la base con la información georeferenciada 
  ggplot() +                 # entramos al ggplot()
  
  # seleccionando geom_sf() visualizamos el mapa, añadimos el argumento fill para que coloree los poligonos en función de la variable 'categoria'
  
  geom_sf(mapping = aes(fill = categoria),   
          show.legend = F) +                 # Con 'show.legend = F' eliminamos las leyendas del costado indicando a que color pertenece cada categoría
  
  # Con scale_fill_manual, seleccionamos los colores que queremos usar
  
  scale_fill_manual(values = colores[1:3]) +
  
  # annotation_north_arrow nos sirve para incorporar la rosa de los vientos
  
  annotation_north_arrow(location = "tr",                         # en que parte del grafico queremos la rosa "tr = top-right", "tl = top-left", "br = bottom-right" y "bl = bottom-left"
                         style = north_arrow_fancy_orienteering,  # el tipo de rosa que queremos
                         height = unit(1.2, "cm"),                # dimensiones de ancho
                         width = unit(0.8, "cm")) +               # dimensiones de alto
  
  # annotation_scale incorpora la escala del mapa
  
  annotation_scale() +
  
  # Con labs controlamos titulo, subtitulo, pie de pagina y titulos de los ejes
  
  labs(subtitle = "Distribución espacial del IASE") +
  
  # En el argumento 'theme', se definen colores tipos y tamaños de letra, alineación, entre otras cosas
  
  theme(axis.text = element_blank(),                             # que no aparezca el texto de los ejes x y y
        axis.ticks = element_blank(),                            # que no aparexca las lineas negras pequeñas que definen los cortes en los ejes x y y
        plot.subtitle = element_text(hjust = 0.5, size = 12),    # controlamos la alineación y el tamaño del subtitulo
        plot.background = element_rect(fill = colores[5]),       # controlamos el color de fondo del gráfico
        panel.background = element_rect(fill = "gray98"),        # controlamos el color de la grilla (la zona gris por defecto donde aparece el gráfico)
        panel.grid = element_line(color = colores[4],            # controlamos las lineas de la grilla
                                  linetype = "dashed"))



# grafico de dispersión ---------------------------------------------------

# Cargamos datos con el idh subnacional calculado por naciones unidas

idh <- read.csv(file = "GDL-Subnational-HDI-data.csv")

# Generamos variable llave en la base idh con el nombre de 'codigo'

idh <- idh %>% 
  mutate(codigo = str_replace_all(string = Region, 
                                  pattern = " ", 
                                  replacement = "_")) %>% 
  mutate(codigo = str_sub(string = codigo, start = 1, end = 5))

# Generamos variable llave en la base construcBase con el nombre 'codigo'

construcBase <- construcBase %>% 
  mutate(codigo = str_sub(string = Depto, start = 1, end = 5))

# Verificamos que todos los elementos de construcBase esten definidos en idh

construcBase$codigo %in% idh$codigo

# Corregimos aquellas observaciones que no están definidas

idh <- idh %>% 
  mutate(codigo = case_when(codigo == "Vaupi" ~ "Vaupe",
                            codigo == "Guaji" ~ "La_Gu",
                            TRUE ~ codigo))

# Volvemos a verificar la correspondencia entre las variables llave creadas

sum(construcBase$codigo %in% idh$codigo)

# Hacemos el join de las bases, nos interesa la variables X2021 para correlacionar
# el indice de acceso a servicios energéticos con el indice de desarrollo humano

construcBase <- construcBase %>% 
  left_join(y = idh %>% 
              select(codigo, X2021),
            by = "codigo")


# Construimos nuevamente la variable categórica 'categoria' para colorear los puntos

construcBase <- construcBase %>% 
  mutate(categoria = factor(ifelse(test = cluster == 1,
                                   yes = "Bajo",
                                   no = ifelse(test = cluster == 2,
                                               yes = "Alto",
                                               no = "Medio")), 
                            levels = c("Alto", "Medio", "Bajo")))


# Calculamos y almacenamos los resultados de la regresión para usarla en la visualización

reg <- lm(formula = construcBase$X2021 ~ construcBase$ind)
sum_reg <- summary(reg)

# Realización del gráfico

dispersion <- construcBase %>%                   # Partimos de la base
  
  # entramos a ggplot y definimos ejes x e y
  
  ggplot(mapping = aes(x = ind, y = X2021)) +
  
  # especificamos el 'geom' y pedimos que el color y la forma de los puntos dependa de la variable categoría
  
  geom_point(mapping = aes(color = categoria,
                           shape = categoria),
             size = 2,                         # que todos los puntos tengan tamaño 2
             alpha = 0.7) +                    # que todos los puntos tengan transparencia 0.7 (valor minimo 0 y maximo 1)
  
  # Incorporamos la recta de regresión
  
  geom_smooth(method = "lm",                    # que sea regresión lineal
              se = F,                           # que no muestre el error estandar
              linetype = "longdash",            # un tipo de linea particular
              color = alpha("gray30", 0.4)) +   # un color particular
  
  # Incorporamos el nombre de los departamentos sobre los puntos
  
  geom_text_repel(mapping = aes(label = codigo,         # que nombre queremos que aparezca
                                color = categoria),     # pedimos que el color de los nombres este en función de categoria
                  size = 2.5,                           # tamaño de la letra
                  show.legend = F,                      # que no se incorpore en la leyenda
                  family = "Times New Roman") +         # tipo de fuente
  
  # Seleccionamos colores y formas en los puntos
  
  scale_color_manual(name = "IASE",             # este argumento es para cambiar el titulo de la leyenda
                     values = colores[1:3]) +   # seleccionamos colores (coinciden con los del mapa)
  scale_shape_manual(name = "IASE",             # este argumento es para cambiar el titulo de la leyenda
                     values = c(15, 17, 19)) +  # seleccionamos formas
  
  # Modificamos los cortes y los limites del eje y, el comando es valido para modificar el eje x (scale_x_continuous())
  
  scale_y_continuous(limits = c(0.6,0.8),
                     breaks = seq(0.6, 0.8, 0.05),
                     labels = seq(0.6, 0.8, 0.05)
  ) +
  
  # Incorporamos texto con el R cuadrado y el resultado de la recta de regresión
  
  annotate(geom = "text",                # especificamos que vamos a incorporar texto
           family = "Times New Roman",   # tipo de fuente a utilizar
           x = c(0.12, 0.15)*1.5,        # ubicación en el eje x
           y = c(0.78, 0.77),            # ubicacion en el eje y
           
           # Etiquetas a incorporar, en este caso usamos la función 'TeX' que pertenece a la libreria 'latex2exp' que sirve para escribir de forma elegante formulas matemáticas
           
           label = c(TeX(str_c("$R^2 = ", round(sum_reg$r.squared, 2), "$")),
                     TeX(str_c("$IDH = ", round(reg$coefficients[1], 2), " + ", round(reg$coefficients[2], 2), "\\cdot IASE$")))
  ) +
  
  # incorporamos subtitutlo y titulos de los ejes
  
  labs(subtitle = "Relación entre el Índice de Acceso a Servicios Energéticos (IASE) y el IDH",
       x = "Índice de Acceso a Servicios Energéticos (IASE)",
       y = "Índice de Desarrollo Humano (IDH)") +
  
  # adecuamos el 'theme'
  
  theme(text = element_text(family = "Times New Roman"),          # tipo de fuente
        legend.position = "inside",                               # posición de la leyenda
        legend.position.inside = c(0.85, 0.2),                    # posición de la leyenda
        legend.title = element_text(hjust = 0.5),                 # titulo de la leyenda centrado
        legend.background = element_rect(fill = alpha(colores[5], # color de fondo y del borde del recuadro de la leyenda 
                                                      0.4),
                                         color = "gray30"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),     # tamaño y orientación del subtitulo
        plot.background = element_rect(fill = colores[5]),        # color de fondo del grafico
        panel.background = element_rect(fill = "gray98"),         # color de la grilla
        panel.grid = element_line(color = colores[4],             # lineas de la grilla
                                  linetype = "dashed"))


dispersion


# Pegado de los gráficos --------------------------------------------------

# creamos el objeto grafico_final, compuesto por 'dispersion' y 'mapa_grafico'

grafico_final <- dispersion + mapa_grafico +        # sumando los graficos el paquete 'patchwork' ya entiende que debe pegar los graficos
  
  # incorporamos titulo y pie de pagina común
  
  plot_annotation(title = "Energía y Desarrollo en los Departamentos de Colombia\t",
                  caption = "\nFuente: DANE (Encuesta de Calidad de Vida - 2022) y PNUD (IDH Subnacional - 2021)\nElaboración: @GEsteban_Gomez") &
  
  # añadimos estilos IMPORTANTE al añadir el theme dentro del grafico compuesto no lo hacemos con el simbolo de '+' sino con un '&'
  
  theme(text = element_text(family = "Times New Roman"),
        plot.background = element_rect(fill = colores[5]),
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.caption = element_text(hjust = 0, 
                                    size = 8,
                                    color = "gray25"))


# Exportamos el gráfico con 'ggsave'

ggsave(filename = "grafico.png",   # Nombre del grafico al momento de exportarlo, importante añadir el argumento '.png'
       plot = grafico_final,       # grafico a exportar
       dpi = 500,                  # dpi para manipular la calidad del grafico a partir de pixeles
       width = 9.49,               # ancho del grafico
       height = 5.15)              # alto del grafico

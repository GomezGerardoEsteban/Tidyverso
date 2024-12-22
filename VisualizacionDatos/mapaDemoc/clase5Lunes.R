

# Módulo 5: Visualización de datos con ggplot2
# En este codigo veremos como generar un mapa utilizando {ggplot2}
# tambien generaremos un gráfico de dispersión y finalmente vincularemos
# ambos graficos usando la libreria {patchwork}
# Las extensiones a utilizar son {ggrepel} para evitar el solapamiento del texto
# con los puntos en el gráfico de dispersión y {ggspatial} para añadir la rosa de
# los vientos en el mapa
# Aprenderemos a generar una paleta de colores para variables continuas

# Limpieza del ambiente ---------------------------------------------------

rm(list = ls())


# Librerias ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(ggspatial)
library(extrafont)
library(patchwork)
library(classInt)

# Levantado de la base ----------------------------------------------------

load(file = "ShapefileDemoc_Gdp.RData")

mapa %>% glimpse()

mapa <- mapa %>% 
  filter(!is.na(v2x_polyarchy))

breaks <- classIntervals(var = mapa$v2x_polyarchy,
                         n = 7,
                         style = "pretty")

breaks <- breaks$brks

?hcl.pals()
?hcl.colors()

hcl.pals(type = "diverging")

color <- hcl.colors(n = length(breaks),
                    palette = "Zissou 1")

scales::show_col(color)

texture <- colorRampPalette(colors = color)(600)

texture <- rev(texture)

scales::show_col(texture, labels = F)

library(extrafont)
library(ggspatial)

extrafont::fonts()

map <- mapa %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = v2x_polyarchy)) +
  scale_fill_gradientn(
    name = "Índice de\nDemocracia",
    colors = texture,
    breaks = breaks,
    label = round(breaks, 2),
    limits = c(min(mapa$v2x_polyarchy),
               max(mapa$v2x_polyarchy))
  ) +
  labs(title = "Democracia y desarrollo económico",
       subtitle = "Índice de democracia electoral (v2x_polyarchy | V-Dem) - 2019") +
  annotation_north_arrow(location = "bl",
                         width = unit(0.9, "cm"),
                         height = unit(1.2, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme(text = element_text(family = "Cambria"),
        plot.title = element_text(hjust = 0.5,
                                  color = "darkslategray"),
        plot.subtitle = element_text(hjust = 0.5,
                                     color = "darkslategray"),
        panel.background = element_rect(fill = "beige"),
        panel.grid = element_line(color = "gray80", 
                                  linetype = "longdash"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "lightcyan"),
        legend.position = "bottom",
        legend.key.height = unit(0.1, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.title = element_text(hjust = 0.5),
        legend.background = element_rect(fill = "lightcyan3"))


library(ggrepel)

continentes <- mapa %>% 
  st_drop_geometry() %>% 
  group_by(continentes) %>% 
  summarise(mean = mean(v2x_polyarchy, na.rm = T)) %>% 
  mutate(label = str_c(continentes, " (", round(mean, 2), ")", sep = ""),
         x = 5.5,
         y = c(0.9, 0.866, 0.833, 0.8, 0.766))

disper <- mapa %>% 
  ggplot(mapping = aes(x = log(NY.GDP.PCAP.KD), y = v2x_polyarchy)) +
  geom_point(mapping = aes(size = SP.POP.TOTL,
                           shape = continentes,
                           color = v2x_polyarchy),
             alpha = 0.7,
             show.legend = F) +
  geom_smooth(method = "lm", 
              se = F,
              color = "olivedrab3",
              linetype = "dashed") +
  geom_text_repel(mapping = aes(label = ISO3_CODE,
                          color = v2x_polyarchy),
            size = 2,
            show.legend = F) +
  geom_point(data = continentes,
             mapping = aes(x = x, y = y, 
                           shape = continentes,
                           color = mean),
             show.legend = F) +
  scale_color_gradientn(
    name = "Índice de\nDemocracia",
    colors = texture,
    breaks = breaks,
    label = round(breaks, 2),
    limits = c(min(mapa$v2x_polyarchy),
               max(mapa$v2x_polyarchy))
  ) +
  scale_shape_manual(values = c(15,16,17,18,3)) +
  scale_size_continuous(range = c(1,8)) +
  labs(x = "Logaritmo del PIB per cápita",
       y = "Índice de democracia",
       caption = "\nFuente: Banco Mundial y V-dem | Elaboración: Esteban Gómez\nLa visualización contiene información de 174 paises | El tamaño de los puntos esta en función de la población total") +
  annotate(geom = "text",
           x = continentes$x + 0.4,
           y = continentes$y,
           label = continentes$label,
           color = "olivedrab3",
           size = 3,
           family = "Cambria") +
  scale_y_continuous(n.breaks = 10) +
  scale_x_continuous(n.breaks = 10) +
  theme(text = element_text(family = "Cambria"),
        axis.title = element_text(color = "darkslategray"),
        plot.caption = element_text(hjust = 0,
                                     color = "darkslategray"),
        panel.background = element_rect(fill = "beige"),
        panel.grid = element_line(color = "gray90", 
                                  linetype = "longdash"),
        plot.background = element_rect(fill = "lightcyan"),
        legend.position = "bottom",
        legend.key.height = unit(0.1, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.title = element_text(hjust = 0.5),
        legend.background = element_rect(fill = "lightcyan3"))

library(patchwork)

grafico <- wrap_plots(map, disper, ncol = 1, heights = c(0.6,1))

ggsave(filename = "disper.png",
       plot = grafico,
       dpi = 500,
       width = 9,
       height = 12)


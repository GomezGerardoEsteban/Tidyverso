#################################################################################################################################################
# Titulo: Precipitaciones pluviales y precio promedio de la electricidad en Colombia                                                                               #
# Tema: Este gráfico muestra la relación observada en Colombia entre el precio mayorista de la electricidad y las precipitaciones pluviales 
#       entre 1996 y 2020. La base utilizada es "precios_year.Rdata" la cual fue construida a partir de los datos de cambio climatico del
#       Banco Mundial y la información del precio mayorista de electricidad de XM. Para tener precios constantes se utilizo el Indice de Precios
#       al consumidor del Banco de la Republica.
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        #
# Fecha: 26 de Septiembre de 2024                                                                                                                   #
# 
#################################################################################################################################################

rm(list = ls())

library(tidyverse)
library(patchwork)
library(extrafont)
library(ggtext)
loadfonts(device = "win")

load("precios_year.Rdata")

correlacion <- cor.test(precios_year$precioMedio, precios_year$Annual.Mean)

graficoDisper <- precios_year %>% 
  filter(year > 1995) %>% 
  ggplot()+
  geom_point(mapping = aes(x = Annual.Mean, y = precioMedio)) +
  geom_smooth(aes(x = Annual.Mean, y = precioMedio), method = "lm", se =F) +
  annotate(geom = "text",
           x = 2750,
           y = 450,
           label = str_c("Correlación lineal de\nPearson = ", round(correlacion$estimate, 2)),
           family = "Lucida Console",
           size = 3) +
  labs(title = "Relación entre precipitaciones promedio \n y precio del kilovatio-hora\n",
       subtitle = NULL,
       y = NULL,
       x = "Milimetros (mm)")+
  theme_bw() +
  theme(text = element_text(family = "Lucida Console"),
        plot.title = element_text(hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.0, size = 7),
        plot.caption = element_text(size=7, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 9, angle = 0),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

graficoPrec_Preci <- precios_year %>% 
  filter(year > 1995) %>% 
  ggplot()+
  geom_line(mapping = aes(x = year, y = Annual.Mean), linetype = "twodash",
            color = "#395FA5", linewidth = 1.2) +
  geom_line(mapping = aes(x = year, y = precioMedio*10), linetype = "solid",
            color = "#A54339", linewidth = 1.2) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~./10,
                                         name = "Precio kWh")) +
  annotate(geom = "text",
           x = 2011.5,
           y = 4500,
           label = "Precio kWh",
           color = "#A54339",
           size = 3.5,
           family = "Lucida Console") +
  annotate(geom = "segment",
           x = 2013.,
           y = 4500,
           xend = 2014.4,
           yend = 4500,
           color = "#A54339",
           linewidth = 0.6,
           arrow = arrow(length = unit(0.3, "cm")), # Especificamos la flecha
           ) +
  annotate(geom = "text",
           x = 2002.5,
           y = 4500,
           label = "Precipitaciones (mm)",
           color = "#395FA5",
           size = 3.5,
           family = "Lucida Console") +
  annotate(geom = "segment",
           x = 2002.5,
           y = 4380,
           xend = 2002.5,
           yend = 2800,
           color = "#395FA5",
           linewidth = 0.6,
           linetype = "twodash",
           arrow = arrow(length = unit(0.3, "cm")), # Especificamos la flecha
  ) +
  labs(title = "Evolución del precio promedio de la electricidad en el mercado mayorista y las \n precipitaciones anuales en Colombia (1996 - 2020)\n",
       subtitle = NULL,
       y = "Milimetros (mm)",
       x = NULL,
       caption = "\n*Los precios estan medidos en pesos constantes a diciembre de 2022\nFuente: XM, Banco Mundial y Banrep | @GEsteban_Gomez - 26/09/2024") +
  theme_bw() +
  theme(text = element_text(family = "Lucida Console"),
        plot.title = element_text(hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.0, size = 7),
        plot.caption = element_text(size=9, hjust=0.0, color="black"),
        axis.text.x = element_text(size = 9, angle = 0),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 11, color = "#395FA5"),
        axis.title.y.right = element_text(size = 11, color = "#A54339", hjust = 0.5, vjust = +3),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")


graficoPrec_Preci

grafico <- wrap_plots(graficoPrec_Preci, graficoDisper, ncol = 2, widths = c(1, 0.45))


ggsave(plot = grafico, filename = "precip_precio_elec.png", 
       units = 'in', 
       width = 14,
       height = 7,
       dpi = 300)

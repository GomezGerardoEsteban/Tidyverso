
# Visualización de grafico de barras

# Grado de acuerdo con la respuesta a la pregunta
# ¿La democracia puede tener problemas pero es el mejor sistema de gobierno?

rm(list = ls())

getwd()

# Librerias

library(haven)
library(tidyverse)
library(ggtext)

# Levantado de la base de datos

base <- read_dta(file = "scripts/bases/Latinobarometro_2023_Esp_Stata_v1_0.dta")

# Variable de interes

base$P18ST_A

# Creacion de variable a partir de condicion logica

base <- base %>% 
  mutate(democ = ifelse(test = P18ST_A == 1 | P18ST_A == 2, 
                        yes = "Acuerdo",
                        ifelse(test = P18ST_A == 3 | P18ST_A == 4, 
                               yes = "Desacuerdo", "NR")))


# Creacion de base con calculos para visualización

frecuencias <- base %>% 
  group_by(idenpa, democ) %>% 
  tally() %>% 
  mutate(total = sum(n, na.rm = T),
         prop = n/total) %>% 
  filter(democ == "Acuerdo")


frecuencias <- frecuencias %>% 
  mutate(categorica = cut(prop,
                          breaks = quantile(x = frecuencias$prop,
                                            probs = c(0,0.25,0.5,0.75,1)),
                          labels = c("Bajo",
                                     "Medio-Bajo",
                                     "Medio-Alto",
                                     "Alto"),
                          include.lowest = T))

# Base con nombres y codigos para pegar a la base de visualización

nombres <- tibble(numero = attr(frecuencias$idenpa, which = "labels"),
                  name = names(attr(frecuencias$idenpa, which = "labels")))


frecuencias <- frecuencias %>% 
  left_join(y = nombres, by = c("idenpa" = "numero"))

# Visualización de datos

library(ggtext)
library(extrafont)

fonts() # Comando para ver los tipos de fuente

grafico <- frecuencias %>% 
  ggplot(mapping = aes(x = prop*100, y = reorder(name, prop))) + # Definicion de ejes x y Y
  geom_col(mapping = aes(fill = categorica),                     # Color a partir de variable categorica 
           color = "black",                                      # Color del borde de las barras
           alpha = 0.8) +                                        # Transparencia
  geom_text(mapping = aes(x = prop*100 + 5,                                  # Añado el valor de los porcentajes con geom_text()
                          label = str_c(round(prop*100, 1), "%", sep = "")), 
            size = 3.5,
            family = "Book Antiqua") +
  scale_x_continuous(n.breaks = 8)  +
  labs(title = "Tolerancia a la democracia en América Latina",          # Titulos, subtitulos, ejes, leyendas y pies de pagina con 'labs'
       subtitle = "<span style = 'color:grey30;'>Porcentaje de</span> **acuerdo** <span style = 'color:grey30;'>con la pregunta</span> **¿la democracia puede<br>tener sus problemas pero es el mejor sistema de gobierno?** <span style = 'color:grey30;'>en 2023</span>",
       x = "Porcentaje (%)",
       y = NULL,
       fill = "Grado de\nAcuerdo",
       caption = "Fuente: Latinobarometro 2023 | @GEsteban_Gomez") +
  scale_fill_manual(values = c("red4", "red3", "blue3", "blue4")) +      # Defino el color de relleno de las barras
  theme(text = element_text(family = "Book Antiqua"),                    # Theme para manipular la parte estetica del gráfico
        plot.title = element_text(hjust = 0.5, 
                                  face = "bold", 
                                  size = 16),
        plot.subtitle = element_markdown(hjust = 0.5,
                                     margin = margin(b = 10)),
        plot.caption = element_text(hjust = 0,
                                    margin = margin(t = 20)),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 9),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.1, "cm"),
        legend.title = element_text(hjust = 0.5, size = 9, face = "bold"),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = "lightgoldenrod3",
                                         color = "grey25"),
        plot.background = element_rect(fill = "lightgoldenrod"),
        panel.background = element_rect(fill = "lightgoldenrod"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "longdash"))



ggsave(filename = "democracia_barras.png",
       plot = grafico,
       dpi = 500,
       height = 5.75,
       width = 7.73
         )

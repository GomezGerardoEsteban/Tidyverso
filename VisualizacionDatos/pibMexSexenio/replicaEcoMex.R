# Limpieza de ambiente

rm(list = ls())

# Paquetes 

library(tidyverse)
library(ggtext)
library(cropcircles)
library(ggimage)

# fuente de us_inflationRate https://data.bls.gov/dataViewer/view/timeseries/CUUR0000SA0;jsessionid=69D3DAAB5B1801495C77E1E540A11AE5

# Levantamos base de inflación de Estados Unidos

us_Inflation <- read.csv(file = "file.csv")

# Creamos la base con el PIB por año y por presidente segun datos del FMI en dolares corrientes

base_imagen <- tribble(~pib, ~presidente,
                       0.38, "ez",
                       0.43, "ez",
                       0.52, "ez",
                       0.55, "ez",
                       0.63, "ez",
                       0.74, "ez",
                       0.79, "vf",
                       0.81, "vf",
                       0.76, "vf",
                       0.81, "vf",
                       0.91, "vf",
                       1.01, "vf",
                       1.10, "fc",
                       1.16, "fc",
                       0.94, "fc",
                       1.11, "fc",
                       1.23, "fc",
                       1.26, "fc",
                       1.33, "ep",
                       1.36, "ep",
                       1.21, "ep",
                       1.11, "ep",
                       1.19, "ep",
                       1.26, "ep",
                       1.31, "al",
                       1.12, "al",
                       1.31, "al",
                       1.46, "al",
                       1.79, "al",
                       2.02, "al")

# Agregamos la variable anio

base_imagen$anio <- 1995:2024

# Filtro de la base de inflacion para deflactar los datos

us_Inflation12 <- us_Inflation %>% 
  filter(Period == "M12")

# Dado que no tenemos a la fecha la inflacion de diciembre de 2024, tomamos el ultimo valor
# de 2024 el cual corresponde a agosto

us_Inflation12[32, ] <- us_Inflation[nrow(us_Inflation), ]

# Para deflactar la prediccion del FMI de 2024, es necesario asumir algun valor de inflación para 
# todo el 2024, lo que hacemos es asumir que el comportamiento de agosto a diciembre, es igual al
# observado entre enero y agosto de 2024

(1 + (314.796/306.746 - 1))^(1/8) - 1 

314.796*(1 + 0.003243344)^(4)

us_Inflation12[32,5] <- 318.899 # Valor del IPC de Estados unidos en Diciembre según supuestos anteriores

# Join de base de PIB y de base de inflación

base_imagen <- base_imagen %>% 
  left_join(y = us_Inflation12 %>% 
              select(Year, Value), by = c("anio" = "Year"))

# Tomaremos como mes base diciembre de 2023

year_base <- us_Inflation12[31,5]

# Actualiación de valores

base_imagen <- base_imagen %>% 
  mutate(pibConst = pib*year_base/Value)

# Generación de base para especificaciones eseticas del grafico

imagenes <- base_imagen %>%
  group_by(presidente) %>% 
  summarise(perMin = min(anio),
            perMax = max(anio)) %>% 
  ungroup() %>% 
  left_join(y = base_imagen %>% 
              select(anio, pibConst), 
            by = c("perMin" = "anio")) %>% 
  left_join(y = base_imagen %>% 
              select(anio, pibConst), 
            by = c("perMax" = "anio"), 
            suffix = c(".st", ".end")) %>% 
  mutate(anio = perMax - 3,
         variacion = pibConst.end/pibConst.st - 1,
         etiqueta = str_c(round(variacion*100, 0), " %", sep = ""),
         presidente = factor(str_c(presidente, ".jpg", sep = ""),
                             levels = c("ez.jpg", 
                                        "vf.jpg", 
                                        "fc.jpg", 
                                        "ep.jpg", 
                                        "al.jpg")),
         pibConst = 5) %>% 
  arrange(desc(anio))


imagenes <- imagenes %>% 
  mutate(nombre = c("AMLO",
                    "ENRIQUE\nPEÑA NIETO",
                    "FELIPE\nCALDERÓN",
                    "VICENTE\nFOX",
                    "ERNESTO\nZEDILLO"),
         tVariacion = ifelse(variacion > 0, "AUMENTÓ", "DESCENDIÓ"))


## Visualización

library(wesanderson)
library(extrafont)
loadfonts(device = "win") # Para ver las alternativas de tipo de letra


colores_ <- wes_palette(name = "BottleRocket2", 5, type = c("discrete")) # Selección de paleta de colores para las barras por sexenio

## Elaboración del gráfico con ggplot2

grafico <- base_imagen %>% 
  ggplot() +
  geom_rect(mapping = aes(xmin = 1994.5, xmax = 2000.5,
                          ymin = -Inf, ymax = Inf),
            fill = "lightgrey",
            alpha = 0.08) +
  geom_rect(mapping = aes(xmin = 2000.5, xmax = 2006.5,
                          ymin = -Inf, ymax = Inf),
            fill = "white") +
  geom_rect(mapping = aes(xmin = 2006.5, xmax = 2012.5,
                          ymin = -Inf, ymax = Inf),
            fill = "lightgrey",
            alpha = 0.08) +
  geom_rect(mapping = aes(xmin = 2012.5, xmax = 2018.5,
                          ymin = -Inf, ymax = Inf),
            fill = "white") +
  geom_rect(mapping = aes(xmin = 2018.5, xmax = 2024.5,
                          ymin = -Inf, ymax = Inf),
            fill = "lightgrey",
            alpha = 0.08) +
  geom_col(mapping = aes(x = anio, 
                         y = pibConst, 
                         fill = presidente),
           show.legend = F,
           width = 0.8) +
  geom_text(mapping = aes(x = anio, 
                          y = pibConst + 0.25, 
                          label = round(pibConst, 2)),
            size = 3.2,
            angle = 90,
            family = "Georgia") +
  geom_text(data = imagenes,
            mapping = aes(x = anio,
                          y = 4,
                          label = nombre),
            size = 3,
            family = "Georgia") +
  geom_text(data = imagenes,
            mapping = aes(x = anio,
                          y = 3.60,
                          label = tVariacion),
            size = 2.5,
            family = "Georgia",
            color = c("darkgreen", "darkred", rep("darkgreen",3))) +
  geom_text(data = imagenes,
            mapping = aes(x = anio,
                          y = 3.40,
                          label = etiqueta),
            size = 4,
            family = "Georgia",
            color = c("darkgreen", "darkred", rep("darkgreen",3))) +
  geom_image(data = imagenes,
             aes(image = circle_crop(as.character(presidente)),
                 x = anio,
                 y = pibConst),
             size = c(0.17, 0.17, 0.17, 0.17, 0.17)) +
  scale_fill_manual(values = sort(colores_)) +
  scale_y_continuous(limits = c(0,5.5)) +
  scale_x_continuous(breaks = 1995:2024) +
  labs(title = "LA ECONOMÍA MEXICANA POR SEXENIO\n",
       subtitle = "PIB a precios constantes de diciembre de 2023\n(billones de dólares)",
       x = NULL,
       y = NULL,
       caption = "Fuente: Fondo Monetario Internacional (PIB) y U.S. Bureau of Labor Statistics (IPC de Estados Unidos)\n           14 de septiembre de 2024\n           @GEsteban_Gomez") +
  theme(text = element_text(family = "Georgia"),
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5),
        panel.background = element_blank(),
        
        plot.caption = element_text(hjust = 0, size = 7),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5))


# Guardado del gráfico

ggsave(filename = "grafico_1.png",
      plot = grafico,
      width = 7.45,
      height = 5.81,
      dpi = 500)

















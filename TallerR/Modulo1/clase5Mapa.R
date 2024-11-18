
rm(list = ls())


# paquetes ----------------------------------------------------------------


library(patchwork)
library(tidyverse)
library(sf)
library(extrafont)
library(ggspatial)

extrafont::font_import()


# Descarga de archivos desde el R

direc <- "https://github.com/GomezGerardoEsteban/Tidyverso/raw/refs/heads/main/TallerR/Modulo1/bases/marcogeoestatal2015_gw"

extensiones <- c(".shp", ".dbf", ".prj", ".shx")

getwd()

for(i in 1:4){
  
  descarga <- paste(direc, extensiones[i], sep = "") 
  
  name <- basename(descarga)
  
  download.file(url = descarga,
                destfile = str_c("scripts/bases/geo/", name, sep = ""),
                mode = "wb")
  
}

# levantado de la base ----------------------------------------------------

base <- haven::read_dta(file = "scripts/bases/ENOE_SDEMT224.dta")

base %>% glimpse()

# Manipulación de base para boxplot

baseIng <- base %>% 
  filter(ingocup > 0)

baseIng$cs_p13_1

baseIng <- baseIng %>% 
  mutate(educ = factor(case_when(cs_p13_1 <= 2 ~ "Primaria",
                                 cs_p13_1 > 2  & cs_p13_1 <= 6 ~ "Bachillerato o Técnica",
                                 cs_p13_1 == 7 ~ "Profesional",
                                 cs_p13_1 >= 8 ~ "Posgrado"),
                       levels = c("Primaria", "Bachillerato o Técnica",
                                  "Profesional", "Posgrado")))


baseIng <- baseIng %>% 
  filter(cs_p13_1 != 99)

baseIng$sexo <- factor(ifelse(baseIng$sex == 1, "Hombre", "Mujer"),
                       levels = c("Mujer", "Hombre"))



# Visualización del box_plot() --------------------------------------------


box <- baseIng %>% 
  ggplot(mapping = aes(x = sexo, y = ingocup)) +
  geom_boxplot(mapping = aes(fill = sexo),
               show.legend = F,
               outliers = F,
               color = "#21918c") +
  facet_wrap(~ educ, ncol = 4) +
  scale_fill_manual(values = c("#fde725", "#440154")) +
  scale_y_continuous(n.breaks = 12) +
  labs(title = "Ingresos por sexo y máximo nivel educativo alcanzado en México",
       subtitle = "Cálculos en base a la ENOE del segundo trimestre de 2024",
       x = NULL,
       y = "Pesos mexicanos") +
  theme(text = element_text(family = element_text(family = "Candara")),
        plot.title = element_text(size = 14,
                                  hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,
                                     margin = margin(0,0,20,0)),
        axis.title.y = element_text(face = "bold",
                                    size = 12,
                                    margin = margin(0,10,0,0)),
        axis.text.y = element_text(family = "Candara",
                                   color = "black"),
        axis.text.x = element_text(family = "Candara",
                                   size = 11,
                                   color = "black"),
        strip.text.x = element_text(size = 12),
        strip.background = element_rect(fill = "white", linetype = "solid",
                                        color = "grey40", linewidth = 1),
        plot.background = element_rect(fill = "beige"),
        panel.background = element_rect(fill = "grey90"),
        panel.grid = element_line(linetype = "longdash",
                                  color = "grey60"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())



# Manipulacion de datos para mapa -----------------------------------------

dataMapa <- baseIng %>% 
  group_by(ent, sexo) %>%
  summarise(mediaIngreso = mean(ingocup)) %>% 
  spread(key = sexo, value = mediaIngreso) %>% 
  mutate(dif = Hombre - Mujer)


# Levantamos el shapefile

shape <- sf::st_read(dsn = "scripts/bases/marcogeoestatal2015_gw.shp")

# Pegado de los datos al shapefile

shape <- shape %>% 
  mutate(ent = as.numeric(CVE_ENT))

shape <- shape %>% 
  left_join(y = dataMapa, by = "ent")



# Visualizacion del mapa --------------------------------------------------


mapa <- shape %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = dif),
          color = "grey60") +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Diferencia del ingreso medio entre hombres y mujeres\nEntidades Federativas de México",
       # subtitle = "Cáculo realizado en base a la ENOE del segundo trimestre de 2024",
       fill = "Diferencia en\nPesos Mexicanos",
       caption = "Fuente: ENOE - INEGI 2024 | @GEsteban_Gomez") +
  theme(text = element_text(family = "Candara"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  margin = margin(t = 20)),
        # plot.subtitle = element_text(hjust = 0.5,
        #                              margin = margin(b = 15)),
        plot.caption = element_text(margin = margin(t = 15),
                                    hjust = 0),
        legend.title = element_text(hjust = 0.5,
                                    size = 11),
        legend.position = c("bottom"),
        panel.background = element_rect(fill = "grey90"),
        plot.background = element_rect(fill = "beige"),
        panel.grid = element_blank(),
        legend.key.height = unit(0.1, "cm")
  ) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         # pad_x = unit(0.65, "in"), pad_y = unit(0.4, "in"),
                         height = unit(1.2, 'cm'), width = unit(0.9, 'cm'),
                         style = north_arrow_fancy_orienteering)



# Pegado de los gráficos

mapaF <- wrap_plots(mapa, box, ncol = 2, nrow = 1,
                    widths = c(0.8,1))

# Guardado del gráfico

ggsave(filename = "graficoMapa.png",
       plot = mapaF,
       width = 13,
       height = 6,
       dpi = 500)






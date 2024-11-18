

rm(list = ls()) # ambiente de trabajo

# paqueteria --------------------------------------------------------------

library(tidyverse)
library(haven)

# base --------------------------------------------------------------------

base <- read_dta(file = "scripts/bases/ENOE_SDEMT224.dta")

base %>% glimpse()


# manipulacion de base ----------------------------------------------------

base$cs_p13_1

base <- base %>% 
  mutate(educacion = factor(case_when(
    cs_p13_1 == 0 | cs_p13_1 == 1 |  cs_p13_1 == 2 ~ "Primaria",
    cs_p13_1 == 3 | cs_p13_1 == 4 | cs_p13_1 == 5 | cs_p13_1 == 6 ~ "Bachillerato o Técnica" ,
    cs_p13_1 == 7 ~ "Profesional" ,
    cs_p13_1 == 8 | cs_p13_1 == 9 ~ "Posgrado"
  ), levels = c("Primaria",
                "Bachillerato o Técnica",
                "Profesional",
                "Posgrado")))


base <- base %>% 
  filter(ingocup > 0 & !is.na(educacion))


base$sexo <- factor(ifelse(test = base$sex == 1, yes = "Hombre", no = "Mujer"),
                    levels = c("Mujer", "Hombre"))


library(extrafont)

extrafont::font_import() # Importacion de los distintos tipos de fuentes del computador

extrafont::fonts()

box <- base %>% 
  ggplot(mapping = aes(x = sexo, y = ingocup)) +
  geom_boxplot(mapping = aes(fill = sexo),
               color = "#21918c",
               outliers = F,
               show.legend = F) +
  facet_wrap(~ educacion, nrow = 1) +
  scale_fill_manual(values = c("#fde725", "#440154")) +
  scale_y_continuous(n.breaks = 15) +
  labs(title = "Ingreso por sexo y máximo nivel educativo alcanzado en Mexico",
       subtitle = "Cálculos en base a la ENOE del segundo trimestre de 2024",
       y = "Pesos mexicanos",
       x = NULL) +
  theme(text = element_text(family = "Candara"),
        plot.title = element_text(hjust = 0.5,
                                  size = 14),
        plot.subtitle = element_text(hjust = 0.5,
                                     margin = margin(b = 15)),
        axis.title.y = element_text(margin = margin(r = 15),
                                    face = "bold"),
        plot.background = element_rect(fill = "beige"),
        panel.background = element_rect(fil = "grey90"),
        panel.grid = element_line(linetype = "longdash",
                                  color = "grey60"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_rect(fill = "white",
                                        linetype = "solid",
                                        color = "grey40",
                                        linewidth = 1))



# Mapa --------------------------------------------------------------------


datosMapa <- base %>% 
  group_by(ent, sexo) %>% 
  summarise(media = mean(ingocup, na.rm = T))


datosMapa <- datosMapa %>% 
  spread(key = sexo, value = media)


datosMapa <- datosMapa %>% 
  mutate(dif = Hombre - Mujer)



url <- "https://github.com/GomezGerardoEsteban/Tidyverso/blob/2909cca4427d6e53f6344f92556d9de602d496d8/TallerR/Modulo1/bases/marcogeoestatal2015_gw"

extenciones <- c(".shp", ".prj", ".shx", ".dbf")

for(i in 1:4){
  
  archivo <-  str_c(url, extenciones[i], sep = "")
  
  nombreArchivo <- basename(archivo)
  
  download.file(url = archivo,
                destfile = nombreArchivo,
                mode = "wb")
  
}


library(sf)

shape <- st_read(dsn = "scripts/bases/marcogeoestatal2015_gw.shp")

shape %>% glimpse()

shape$CVE_ENT <- as.numeric(shape$CVE_ENT)

shape <- shape %>% 
  left_join(y = datosMapa, by = c("CVE_ENT" = "ent")) 


library(ggspatial)

mapa <- shape %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = dif)) +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Diferencia del ingreso medio entre hombres y mujeres\nEntidades Federativas de México",
       fill = "Diferencia en\nPesos mexicanos",
       caption = "Fuente: ENOE - INEGI 2024 | Elaboración: @GEsteban_Gomez") +
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(1.2, "cm"), width = unit(0.9, "cm"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  theme(text = element_text(family = "Candara"),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0,
                                    margin = margin(t=20)),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = c("bottom"),
        legend.key.height = unit(0.1, units = "cm"),
        plot.background = element_rect(fill = "beige"),
        panel.background = element_rect(fill = "grey90"))


ggsave(filename = "mapaPrueba.png",
       plot = mapa,
       dpi = 500)

library(patchwork)

mapa + box

grafico <- wrap_plots(mapa, box, ncol = 2, widths = c(0.8, 1))

ggsave(filename = "graficoFinal.png",
       plot = grafico,
       dpi = 500,
       height = 6,
       width = 13)

































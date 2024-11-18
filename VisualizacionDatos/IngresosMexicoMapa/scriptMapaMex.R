
## Visualización de datos, mapa con la brecha de ingresos por genero y Boxplot con la diferencia de ingresos
## por maximo nivel educativo alcanzado


rm(list = ls()) # ambiente de trabajo

# paqueteria --------------------------------------------------------------

library(tidyverse)
library(haven)

# base --------------------------------------------------------------------

base <- read_dta(file = "scripts/bases/ENOE_SDEMT224.dta") # Levantamos la base con la funcion read_dta del paquete haven

base %>% glimpse()

# manipulacion de base ----------------------------------------------------

base$cs_p13_1 # Variable que contiene la información sobre maximo nivel educativo alcanado

# Creación de variable categorica de cuatro niveles para grafico de cajas

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

# Filtro de la base por observaciones con ingreso mayor a cero y
# educación distinto de "No Sabe"

base <- base %>% 
  filter(ingocup > 0 & !is.na(educacion))

# Modificación de variable sexo a variable categórica

base$sexo <- factor(ifelse(test = base$sex == 1, yes = "Hombre", no = "Mujer"),
                    levels = c("Mujer", "Hombre"))

# Paquete extrafont para manipular la fuente del texto en los gráficos
library(extrafont)

extrafont::font_import() # Importacion de los distintos tipos de fuentes del computador

extrafont::fonts() # Visualización de los tipos de fuentes disponibles

# Grafico del boxplot

box <- base %>% 
  ggplot(mapping = aes(x = sexo, y = ingocup)) + # Entro al ggplot y defino ejes x y y
  geom_boxplot(mapping = aes(fill = sexo),       # defino la forma geometrica a graficar, en este caso geom_boxplot()
               color = "#21918c",                # Color de los bordes de las cajas
               outliers = F,                     # quito los outlers
               show.legend = F) +                # quito la leyenda
  facet_wrap(~ educacion, nrow = 1) +                          # Comando para generar la visualizacion por segmento de nivel educativo
  scale_fill_manual(values = c("#fde725", "#440154")) +        # Elijo los colores de las cajas (segun sexo)
  scale_y_continuous(n.breaks = 15) +                     # Genero mas quiebres en el eje y
  labs(title = "Ingreso por sexo y máximo nivel educativo alcanzado en Mexico",  # Labs para titulos, subtitulos, ejes, leyendas y pies de pagina
       subtitle = "Cálculos en base a la ENOE del segundo trimestre de 2024",
       y = "Pesos mexicanos",
       x = NULL) +
  theme(text = element_text(family = "Candara"),  # theme define la parte estetica del gráfico 
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

# Genero el dato por sexo y por entidad federativa

datosMapa <- base %>% 
  group_by(ent, sexo) %>% 
  summarise(media = mean(ingocup, na.rm = T))

# Modifico el formato de la base de largo a ancho

datosMapa <- datosMapa %>% 
  spread(key = sexo, value = media)

# Creo la variable diferencia de ingreso (dif)

datosMapa <- datosMapa %>% 
  mutate(dif = Hombre - Mujer)

# Para exportar la base, voy a obtenerla a través de una url donde esta almacenada la base
# es decir, descargo los datos desde R

# La url es donde estan almacenados los cuatro archivos espaciales (.shp, .dbf, .shx, .prj)
url <- "https://github.com/GomezGerardoEsteban/Tidyverso/blob/2909cca4427d6e53f6344f92556d9de602d496d8/TallerR/Modulo1/bases/marcogeoestatal2015_gw"
extenciones <- c(".shp", ".prj", ".shx", ".dbf")


# Genero la descrga a traves de un bucle for, va a ir descargando un archivo por vez
for(i in 1:4){
  
  archivo <-  str_c(url, extenciones[i], sep = "") # Pego la url con la extencion correspondiente
  
  nombreArchivo <- basename(archivo)                # Extraigo el nombre del archivo a descargar
  
  download.file(url = archivo,                      # Descargo el archivo en mi directorio de trabajo
                destfile = nombreArchivo,
                mode = "wb")
  
}

# Activo el paquete sf para levantar los archivos espaciales
library(sf)

# Levanto el archivo .shp como una base de datos
shape <- st_read(dsn = "scripts/bases/marcogeoestatal2015_gw.shp")

shape %>% glimpse()

# Paso la variable CV_ENT de texto a numerica para poder pegar las bases
shape$CVE_ENT <- as.numeric(shape$CVE_ENT)

# Pego los datos calculados para cada entidad al shape
shape <- shape %>% 
  left_join(y = datosMapa, by = c("CVE_ENT" = "ent")) 

# Activo ggspatial para añadir al mapa rosa de los vientos y scala
library(ggspatial)

# Visualización del mapa
mapa <- shape %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = dif)) +           # Pido que coloree segun diferencia de ingresos
  scale_fill_continuous(type = "viridis") +      # defino la escala que quiero que utilice para colorear
  labs(title = "Diferencia del ingreso medio entre hombres y mujeres\nEntidades Federativas de México",
       fill = "Diferencia en\nPesos mexicanos",
       caption = "Fuente: ENOE - INEGI 2024 | Elaboración: @GEsteban_Gomez") +
  annotation_north_arrow(location = "tr", which_north = "true",                # Codigo para poner la rosa de los vientos en la esquina superior derecha
                         height = unit(1.2, "cm"), width = unit(0.9, "cm"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.2) +                        # Codigo para poner la escala
  theme(text = element_text(family = "Candara"),                               # theme para manejar la estetica del grafico
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


# Exporto el mapa 
ggsave(filename = "mapaPrueba.png",
       plot = mapa,
       width = 7,
       heigth = 5,
       dpi = 500)

# Paquete para pegar graficos
library(patchwork)

# Pega los graficos de manera horizontal 
mapa + box

# Pega los graficos de manera horizontal o vertical y controlo el ancho de cada gráfico
grafico <- wrap_plots(mapa, box, ncol = 2, widths = c(0.8, 1))


# Exporto el grafico final
ggsave(filename = "graficoFinal.png",
       plot = grafico,
       dpi = 500,
       height = 6,
       width = 13)


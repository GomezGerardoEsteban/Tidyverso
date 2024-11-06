
# MAPA GHSL DATA
# -------------------------

rm(list = ls())

# Paqueteria

library(tidyverse)
library(terra)
library(giscoR)
library(glue)
library(ggtext)
library(ggrepel)

# 1.Descargar los datos
#------------------------

# url <- 
#   "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2025_GLOBE_R2023A_4326_30ss/V1-0/GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0.zip"

# file_name <- "GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0.zip"
# 
# download.file(
#   url = url,
#   path = getwd(),
#   destfile = file_name
# )

# 2. Cargar los datos
#----------------------

# unzip(file_name)        El man da tips para realizar todo desde R, pero esta confuso
#                         hay que estudiarlo mas, por ahora uno tiene que saber si tiene un archivo  
# raster_name <- gsub(    raster o un shapefile y trabajar a partir de eso.
#   ".zip", ".tif",
#   file_name
# )

file_name <- "GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0.tif"

pop <- terra::rast(file_name)

# 3. Colombia SHAPEFILE
#-----------------------

# El paquete giscoR sirve para obtener los poligonos de los paises
# un gillette de programa

get_country_borders <- function(){
  country <- giscoR::gisco_get_countries(
    country = "CO",
    resolution = "3"
  )
  
  return(country)
}

country <- get_country_borders()

# 4. CROP Colombia GHSL
#-----------------------

colombian_pop <- terra::crop(
  x = pop,
  y = terra::vect(country),
  snap = "in",
  mask = T
)

# 5. RASTER TO DATAFRAME
#-----------------------

colombian_pop_df <- as.data.frame(
  colombian_pop,
  xy = T, na.rm = T
)

head(colombian_pop_df)

names(colombian_pop_df)[3] <- "val"

colombian_pop_df <- colombian_pop_df %>% 
  dplyr::mutate(
    cat = dplyr::if_else(
      val > 0, "Si", "No"
    )
  )

colombian_pop_df$cat <- as.factor(
  colombian_pop_df$cat
)

# City names

filename <- "geonames-population.csv"

places_df <- read.csv2(filename)

places_modified_df <- places_df[, c(2, 7, 13, 18)]
names(places_modified_df) <- c(
  "name", "country_code", "pop", "coords")

places_modified_df

places_modified_df[c("lat", "long")] <-
  stringr::str_split_fixed(
    places_modified_df$coords, ",", 2
  )

places_clean_sf <- places_modified_df |>
  dplyr::filter(country_code == "CO") |>
  dplyr::filter(name %in% c( "Bogotá",
                             "Leticia",
                             "Arauca",
                             "Florencia",
                             "Yopal",
                             "Popayán",
                             "Valledupar",
                             "Quibdó",
                             "Montería",
                             "Inírida",
                             "San José del Guaviare",
                             "Riohacha",
                             "Mocoa",
                             "Pasto",
                             "Armenia",
                             "Pereira",
                             "Sincelejo",
                             "Ibagué",
                             "Mitú",
                             "Puerto Carreño",
                             "Cali",
                             "Medellín",
                             "Barranquilla",
                             "Cartagena",
                             "Cúcuta",
                             "Bucaramanga",
                             "Pereira",
                             "Santa Marta",
                             "Manizales",
                             "Neiva",
                             "Villavicencio",
                             "Tunja",
                             "Popayán")) %>% 
  dplyr::select(
    # -coords,
    -country_code,
    # -pop
  ) |>
  sf::st_as_sf(
    coords = c(
      "long",
      "lat"
    ),
    crs = 4326
  )


# 6. MAP
#-------

library(extrafont)
extrafont::loadfonts()


cols <- c("#FFCD00", "#5757ae") # Defino los colores

pob_map <- ggplot() +
  geom_tile(
    data = colombian_pop_df,
    aes(x = x,
        y = y,
        fill = cat  # Que coloreé según exista o no población
    ),
    show.legend = F) +  # Que muestre o no la leyenda
  scale_fill_manual(
    values = cols,
    na.value = "#FFCD00"
  ) +
  geom_sf(data = places_clean_sf %>% 
            filter(pop > 1000000),
          aes(size = pop),
          color = "#C8102E",
          alpha = 0.9,
          show.legend = F) +
  geom_sf_text(data = places_clean_sf %>% 
                  filter(pop > 1000000),
               aes(label = name),
               size = 3.5,
               color = "black",
               nudge_x = c(1.2, -1.2, -0.8, -1.6),
               family = "Constantia") +
  scale_x_continuous(limits = c(-79, -66.5)) +
  labs(title = "Distribución poblacional de Colombia\n",
       subtitle = "Ciudades con más de <span style='color:#C8102E;'>un millón de habitantes</span>",
       caption = "Fuente: Global Human Settlement | Elaboración: @GEsteban_Gomez",
       x = NULL,
       y = NULL)+
  theme(text = element_text("Constantia"),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_markdown(hjust = 0, size = 11),
        plot.caption = element_text(hjust = 0, size =10, color = "black"),
        plot.background = element_rect(fill = "#f6f15a"),
        panel.background = element_rect(fill = "#f6f15a",
                                        color = "#f6f15a"),
        panel.grid.major = element_line(linetype = "dashed",
                                        color = "#cbc99a"),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

pob_map

ggsave(filename = "mapaRasterColombia.png",
       plot = pob_map,
       dpi = 300,
       width = 5,
       height = 7.2,
       )

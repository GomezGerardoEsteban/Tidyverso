#################################################################################
# Titulo: Visualización hidrográfica de Luisiana, Estados Unidos
# Autor: @GEsteban_Gomez
# Fecha: 21/09/2024
# Descripción: Es un script netamente geografico el cual esta inspirado en la novela
#              "Un millón de ruiseñores" de Susan Straight
#
#################################################################################



# Limpieza del ambiente ---------------------------------------------------

rm(list = ls())

# Paquetería --------------------------------------------------------------

library(tidyverse)
library(sf)
library(ggtext)

# Shapefile de Estados Unidos para obtener limites geográficos de Luisiana

lo <- st_read(dsn = "s_05mr24.shp", quiet = F)

lo$NAME

lo <- lo %>% 
  filter(NAME == "Louisiana")

# Obtencion de datos de rios
#------------------------------

# "https://www.hydrosheds.org/products/hydrobasins"  # Pagina para descargar los datos

hydroBaseAmerica <- sf::st_read(dsn = "hydro/hybas_na_lev03_v1c.shp")

# Homologación de coordenadas

sf_use_s2(F)

st_crs(hydroBaseAmerica)
st_crs(lo)

# Pegado de datos geograficos

lo <- st_set_crs(lo, st_crs(hydroBaseAmerica))

lo_rios_base <- hydroBaseAmerica %>% 
  sf::st_intersection(lo) %>% 
  dplyr::select(HYBAS_ID)

hydroRiosAmerica <- st_read(dsn = "hydro/HydroRIVERS_v10_na.shp")

lo_rios <- hydroRiosAmerica %>% 
  dplyr::select(ORD_FLOW) %>% 
  st_intersection(lo)

loHydro <- st_intersection(
  lo_rios,
  lo_rios_base
)

# Reasignación de valores en función de longitud de los rios

unique(loHydro$ORD_FLOW)

lo_width <- loHydro %>% 
  mutate(width = as.numeric(ORD_FLOW),
         width = case_when(
           # width == 1 ~ .8,
           width == 2 ~ .9,
           width == 3 ~ .7,
           width == 4 ~ .6,
           width == 5 ~ .5,
           width == 6 ~ .4,
           width == 7 ~ .3,
           # width == 8 ~ .15,
           # width == 9 ~  .1,
           TRUE ~ 0,
         )) %>% 
  st_as_sf()

lo_width %>% glimpse()


# carga de nombres ciudades -----------------------------------------------

filename <- "geonames-population.csv"

load_geonames_data <- function(){
  places_df <- read.csv(
    paste(getwd(), "/", filename, sep = ""),
    sep = ";"
  )
  return(places_df)
}

places_df <- load_geonames_data()

places_df %>% glimpse()

places_df <- places_df %>% 
  filter(country.code == "US") %>% 
  filter(admin1.code == "LA")


places_modified_df <- places_df[, c(2, 7, 13, 18)]
names(places_modified_df) <- c(
  "name", "country_code", "pop", "coords")

places_modified_df[c("lat", "long")] <-
  stringr::str_split_fixed(
    places_modified_df$coords, ",", 2
  )

places_modified_df$name

places_clean_sf <- places_modified_df %>% 
  dplyr::filter(name %in% c("Opelousas", "New Orleans")) %>% 
  dplyr::select(
    -coords,
    -country_code,
  ) %>% 
  sf::st_as_sf(
    coords = c(
      "long",
      "lat"
    ),
    crs = 4326
  )


# Visualización gráfica ---------------------------------------------------

library(extrafont)
loadfonts(device = "win") # Para ver las alternativas de tipo de letra

mapa <- lo %>% 
  ggplot() +
  geom_sf(fill = "darkblue",
          color = "darkblue",
          linewidth = 0.5) +
  geom_sf(data = lo_width,
          mapping = aes(size = width,
                        alpha = width),
          color = "white",
          show.legend = F) +
  geom_sf(data = places_clean_sf,
          shape = 18,
          color = "darkred",
          size = 2.5,
          alpha = 1) +
  geom_sf_text(data = places_clean_sf,
               aes(label = name),
               nudge_x = c(0.55, -0.50),
               nudge_y = c(0.08, 0),
               color = "white",
               size = 4,
               family = "Berlin Sans FB") +
  annotate(geom = "curve",
           xend = -91.4,
           yend = 30.8,
           x = -90.5,
           y = 31.5,
           curvature = 0.5,
           arrow = arrow(length = unit(0.5, "cm")),
           color = "white") +
  annotate(geom = "text",
           x = -90.3,
           y = 31.5,
           label = "Río\nMisisipí",
           color = "white",
           family = "Berlin Sans FB",
           size = 4) +
  labs(title = "Luisiana, Estados Unidos\n",
       subtitle = "Nueva Orleans, Opelousas y el Río Misisipí, protagonistas en la novela<br>\"Un millón de ruiseñores\" de Susan Straight",
       caption = "Fuente: U.S. National Weather Service y HydroSHEDS\n              @GEsteban_Gomez - 21/09/2024") +
  theme(text = element_text(family = "Berlin Sans FB"),
        plot.title = element_text(color = "black", size = 15),
        plot.caption = element_text(color = "black", size = 9, hjust = 0),
        plot.background =  element_rect(fill = "white"),
        plot.subtitle = element_markdown(color = "black", size = 10),
        panel.background = element_rect(fill = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

# Almacenamiento del gráfico

ggsave(filename = "lousianaMap.png",
       plot = mapa,
       dpi = 500,
       width = 9.02,
       height = 6.4
         )


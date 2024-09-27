
# limpieza de ambiente ----------------------------------------------------

rm(list = ls())

# activación de paquetes --------------------------------------------------

library(tidyverse)
library(sf)
library(ggtext)

# Shapefile de guasca -----------------------------------------------------


de <- st_read(dsn = "MGN_DPTO_POLITICO.shp", quiet = T)
de %>% glimpse()

gu <- de %>% 
  filter(DPTO_CCDGO == "25")

gu %>% 
  ggplot() +
  geom_sf()



# shapefile rivers and basins ---------------------------------------------

hydroBaseAmerica <- sf::st_read(dsn = "hydro/hybas_sa_lev03_v1c.shp")

# Homologación de coordenadas

sf_use_s2(F)

st_crs(hydroBaseAmerica)
st_crs(gu)

gu <- st_set_crs(gu, st_crs(hydroBaseAmerica))

gu_rios_base <- hydroBaseAmerica %>% 
  sf::st_intersection(gu) %>% 
  dplyr::select(HYBAS_ID)


hydroRiosAmerica <- st_read(dsn = "hydro/HydroRIVERS_v10_sa.shp")

gu_rios <- hydroRiosAmerica %>% 
  dplyr::select(ORD_FLOW) %>% 
  st_intersection(gu)

guHydro <- st_intersection(
  gu_rios,
  gu_rios_base
)

# Reasignación de valores en función de longitud de los rios

unique(guHydro$ORD_FLOW)

gu_width <- guHydro %>% 
  mutate(width = as.numeric(ORD_FLOW),
         width = case_when(
           width == 3 ~ 1,
           width == 4 ~ .9,
           width == 5 ~ .8,
           width == 6 ~ .7,
           width == 7 ~ .6,
           TRUE ~ 0,
         )) %>% 
  st_as_sf()

gu_width %>% glimpse()

guasca_coords <- tribble(~"name", ~"lat", ~"long",
                         "Guasca", 4.865833, -73.877222,
                         "Bogotá", 4.60971, -74.08175)

guasca_coords <-  guasca_coords %>% 
  sf::st_as_sf(
  coords = c(
    "long",
    "lat"
  ),
  crs = 4326
)


library(extrafont)

gu %>% 
  ggplot() +
  geom_sf(fill = "#90ed70",
          color = "#047204",
          linewidth = 0.5) +
  geom_sf(data = gu_width,
          mapping = aes(size = width,
                        alpha = width),
          color = "#0500ae",
          show.legend = F) +
  geom_sf(data = guasca_coords,
          shape = 18,
          color = "darkred",
          size = 2.5,
          alpha = 1) +
  annotate(geom = "text",
           x = c(-73.6,-73.2),
           y = c(4.1, 5.12),
           label = c("Bogotá", "Guasca"),
           size = 3,
           family = "Berlin Sans FB") +
  annotate(geom = "segment",
           xend = -73.85,   
           yend = 4.866,    
           x = -73.3,       
           y = 5.09,        
           arrow = arrow(length = unit(0.3, "cm")),
           color = "darkblue") + 
  annotate(geom = "segment",  
           xend = -74.05,    
           yend = 4.599,     
           x = -73.6,       
           y = 4.12,        
           arrow = arrow(length = unit(0.3, "cm")), 
           color = "darkblue") +
  labs(title = "Guasca, Colombia",
       subtitle = "gua 'sierra' y shuca 'falda': 'cercado de Cerros'",
       caption = "Fuente: DANE y HydroSHEDS\n           @GEsteban_Gomez - 27/09/2024") +
  theme(text = element_text(family = "Berlin Sans FB"),
        plot.title = element_text(color = "black", size = 15),
        plot.caption = element_text(color = "black", size = 7, hjust = 0),
        plot.background =  element_rect(fill = "white"),
        plot.subtitle = element_markdown(color = "black", size = 11),
        panel.background = element_rect(fill = "#717fb8"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())


ggsave(filename = "guascaRivers.png",
       dpi = 500,
       width = 4,
       height = 5)



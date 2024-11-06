
rm(list=ls())

library(tidyverse)
library(giscoR)
library(patchwork)
library(gt)
library(flextable)

co <- sf::st_read(dsn = "MGN_DPTO_POLITICO.shp", quiet = T)

# unique(co$DPTO_CNMBR)

co <- co %>% 
  filter(DPTO_CNMBR != "ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA")

# limites <- giscoR::gisco_get_countries(country = c("BR", "VE", "EC", "PE", "PA"))

PIB <- readxl::read_excel(path = "datosPibPerCapita.xlsx")

PIB$indice <- 1:nrow(PIB)

# flextable(data = PIB %>% filter(codigo != "88",
#                                 indice < 17) %>% select(depto, ProporcionInternet)) %>% 
#   width(width = c(2,1.3)) %>% 
#   set_header_labels(depto = "Departamento",
#                     ProporcionInternet = "Proporción Internet") %>% 
#   fontsize(size = 8) %>% 
#   fontsize(size = 9, part = "header") %>% 
#   height(height = 1)
# 
# flextable(data = PIB %>% filter(codigo != "88",
#                                 indice >= 17) %>% select(depto, ProporcionInternet)) %>% 
#   width(width = c(2,1.5)) %>% 
#   set_header_labels(depto = "Departamento",
#                     ProporcionInternet = "Proporción Internet") %>% 
#   bg(j = 2, bg = colourer)

co <- co %>% 
  left_join(y = PIB, by = c("DPTO_CCDGO" = "codigo"))

library(wesanderson)

colores <- wes_palette(name = "BottleRocket2", n = 5,
            type = c("continuous"))

cuantiles <- quantile(x = PIB$ProporcionInternet, probs = seq(0,1,0.2))

co <- co %>% 
  mutate(categoria = case_when(ProporcionInternet <= 30 ~ "Menor al 30%" ,
                               ProporcionInternet > 30 & ProporcionInternet <= 45 ~ "Del 30% al 45%",
                               ProporcionInternet > 45 & ProporcionInternet <= 60 ~ "Del 45% al 60%",
                               ProporcionInternet > 60 & ProporcionInternet <= 75 ~ "Del 60% al 75%",
                               ProporcionInternet > 75 ~ "Mayor al 75%")
                               )

mapa <- co %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = categoria),
          color = "white")+
  scale_fill_manual(values = colores)









mx <- sf::st_read(dsn = "marcogeoestatal2015_gw.shp")

mx %>% glimpse()

mx$color <- rnorm(n = 32, mean = 8, sd = 6)
quantiles <- quantile(x = mx$color, probs = seq(0,1,0.25), na.rm = T)

mx$categorica <- case_when(mx$color <= quantiles[2] ~ "cat1",
                           mx$color > quantiles[2] & mx$color <= quantiles[3] ~ "cat2",
                           mx$color > quantiles[3] & mx$color <= quantiles[4] ~ "cat3",
                           mx$color > quantiles[4]  ~ "cat4") 


mx %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = color)) +
  




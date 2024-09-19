################################################################################
# Titulo: Análisis de Componentes Principales para comparar el acceso a energía de
#         los hogares en los departamentos de Colombia
# Autor: Gerardo Esteban Gómez-Santiago
# Fecha: 19/09/2024
################################################################################

# Limpieza del ambiente ---------------------------------------------------

rm(list = ls())

# Paqueteria --------------------------------------------------------------

library(tidyverse)
library(sf)
library(ggtext)
library(FactoMineR)
library(cluster)
library(GGally)
library(patchwork)

# carga de la base de datos -----------------------------------------------

load("IPEM.RData")

construcBase <- construcBase %>% 
  select(-c("cluster","ind","dimension1","dimension2"))


# seleccion de variables numericas para realizar el indice ----------------

construcBaseNum <- construcBase[,c(2:15)] %>% 
  scale() %>% # para que el indice no este afectado por las unidades de medida, es necesario estandarizar las variables
  as_tibble()

# Renombramiento de las variables

nombresVar <- c("agua caliente", "cocina", "combustible cocina",
                "computador", "conexion formal", "electricidad", "gas",
                "internet", "lavadora", "nevera", "televisión",
                "ingreso", "gasto energéticos", "proporción gasto")

colnames(construcBaseNum) <- nombresVar

# Aplicación del PCA ------------------------------------------------------

pca <- PCA(construcBaseNum, ncp = 14)

summary(pca)

# Generación del indice a partir de los resultados del PCA
# La utilidad del PCA para este tipo de análisis es ponderar la incidencia de cada
# variable en función del porcentaje de variabilidad que explican de los datos.

s <- tibble(one = 1:length(construcBaseNum$`agua caliente`))

for(i in 1:length(construcBaseNum)){
  
  s[,i] <- pca$ind$coord[,i]*pca$eig[i,2]
  
}

s <- s %>% mutate(indicador = rowSums(.,1:14)/100,
                  indicador_rescale = rescale01(indicador)*10)

construcBase$indicador <- s$indicador
construcBase$indicador_r <- s$indicador_rescale
# Clasificación de los departamentos con análisis de cluster a partir del método
# de k-vecinos mas cercanos

kmedias1.3 <- kmeans(x = construcBaseNum, centers = 3, nstart = 625)
clusters1.3 <- kmedias1.3$cluster

construcBase$cluster <- kmedias1.3$cluster

# Base de IDH subnational -------------------------------------------------

idh <- read.csv(file = "GDL-Subnational-HDI-data.csv")

idh <- idh %>% 
  mutate(codigo = str_sub(Region, start = 1, end = 5),
         codigo = ifelse(codigo == "San A", "San_A",
                         ifelse(codigo == "Guaji", "La_Gu",
                                ifelse(codigo == "Vaupi", "Vaupe", codigo))))


# Join de las bases -------------------------------------------------------

construcBase <- construcBase %>% 
  mutate(codigo = str_sub(Depto, start = 1, end = 5))

construcBase <- construcBase %>% 
  left_join(y = idh %>% select(codigo, X2021), by = c("codigo"))


# Levantamos el shapefile de Colombia -------------------------------------

shape <- st_read(dsn = "MGN_DPTO_POLITICO.shp", quiet = T)

shape <- shape %>% 
  left_join(y = construcBase %>% 
              select(P1_DEPARTAMENTO, codigo, indicador, indicador_r, cluster, X2021), by = c("DPTO_CCDGO" = "P1_DEPARTAMENTO"))


shape$cluster

shape <- shape %>% 
  mutate(IPEM = ifelse(cluster == 1, "Alto",
                       ifelse(cluster == 2, "Medio", "Bajo")),
         IPEM = factor(IPEM, levels = c("Alto", "Medio", "Bajo")))

library(wesanderson)
library(extrafont)
library(ggrepel)
loadfonts(device = "win") # Para ver las alternativas de tipo de letra

shape$IPEM

g1 <- shape %>% 
  ggplot() +
  geom_point(mapping = aes(x = indicador_r, 
                           y = X2021, 
                           color = IPEM,
                           shape = IPEM),
             size = 2.5,
             show.legend = F) +
  geom_smooth(mapping = aes(x = indicador_r, y = X2021),
              method = "lm",
              se = F,
              color = "#0c1357") +
  geom_text_repel(mapping = aes(x = indicador_r, 
                                y = X2021,
                                label = codigo),
                  size = 2.8,
                  family = "Cambria") +
  geom_point(data = tibble(x = rep(8.5,3),
                           y = c(0.64,0.65,0.66),
                           label = factor(c("Bajo", "Medio", "Alto"),
                                          levels = c("Alto", "Medio", "Bajo"))),
            mapping = aes(x = x,
                          y = y,
                          color = label,
                          shape = label),
            size = 3.,
            show.legend = F) +
  geom_text(data = tibble(x = rep(8.5,3),
                          y = c(0.64,0.65,0.66),
                          label = factor(c("Bajo", "Medio", "Alto"),
                                         levels = c("Alto", "Medio", "Bajo"))),
            mapping = aes(x = x + 0.7,
                          y = y,
                          label = label),
            size = 3.5,
            family = "Cambria") +
  scale_color_manual(values = c("#FFC300", "#FF5733", "#C70039")) +
  labs(title = "Índice de acceso a la energía en departamentos de Colombia\n",
       subtitle = "Relación entre el Índice de Acceso a la Energía e IDH",
       x = "Índice de acceso a la energía",
       y = "Índice de Desarrollo Humano (IDH)",
       caption = "Fuente: DANE\n            @GEsteban_Gomez - 19/09/2024") +
       # caption = "Fuente: DANE\n            @GEsteban_Gomez - 19/09/2024") +
  theme(plot.title = element_text(hjust = 1, size = 14, color = "white"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, color = "white"),
        plot.caption = element_text(hjust = 0, size = 8, color = "white"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        # plot.caption = element_text(hjust = 0, size = 8),
        text = element_text(family = "Cambria"),
        plot.background = element_rect(fill = "#581845"),
        panel.background = element_rect(fill = "#c89bbe"),
        panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank()
        )


g1
library(ggspatial)

g2 <- shape %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = IPEM),
          show.legend = F) +
  scale_fill_manual(values = c("#FFC300", "#FF5733", "#C70039")) +
  labs(
       subtitle = "Distribución espacial por departamentos") +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         # pad_x = unit(0.65, "in"), pad_y = unit(0.4, "in"),
                         height = unit(1.2, 'cm'), width = unit(0.9, 'cm'),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.subtitle = element_text(hjust = 0.5, color = "white"),
        text = element_text(family = "Cambria"),
        plot.background = element_rect(fill = "#581845"),
        panel.background = element_rect(fill = "#c89bbe"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  )


grafico <- g1 + g2

ggsave(filename = "mapaPCA.png",
       plot = grafico,
       dpi = 500,
       width = 9.49,
       height = 5.15
         )

g2 + g1 +
  plot_annotation(title = "Índice de acceso a la energía en departamentos de Colombia\n",
                  caption = "Fuente: DANE\n            @GEsteban_Gomez - 19/09/2024")



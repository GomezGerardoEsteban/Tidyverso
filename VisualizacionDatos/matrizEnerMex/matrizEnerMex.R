
#################################################################################################################################################
# Descripción: Este codigo ordena la base de energía de OWiD para poder generar varios de los gráficos que se encuentran en la tesis.
#              Incorpora información de ClimateWatch para ver las emisiones de GEI por tipo de sector según el IPCC y variables del Banco mundial
#              (World Development Indicators) sobre población, Indice de Desarrollo Humano (IDH), industria manufacturera, PIB, PIB per cápita e 
#              industria total.
#              Hace los joins de las distintas bases por paises y años y acomoda los datos en formato largo.
# 
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        #
# Fecha: 3 de Julio de 2024                                                                                                                     #
# Bases de datos: Este código utiliza un total de 8 bases de datos, la de OWiD, 5 bases con indicadores de Banco Mundial, la base de IDH y la base
#                 de climateWacth. En el script **descargaBaseEnergia.R** se muestra la manera de obtener cada una de estas bases, ejecutado ese 
#                 script, solo debe asegurarse que las bases se enuentren en el directorio de trabajo.
# Productos: los productos que se obtienen de este script a partir del cual se generan graficos en otros documentos son:
#            - base (base de datos con la información de energia a la cual se le añaden las bases de banco mundial e IDH)
#            - baseCo2 (base de datos con la información de emisiones por sector según la categorización de IPCC)
# Detalles adicionales: Los productos de este scripts se utilizan en los documentos que tengan al inicio de la ejecución el comando 
#                       'source("baseEnergia.R")'
#################################################################################################################################################

rm(list = ls())

library(tidyverse)
library(readxl)
library(patchwork)
library(ggtext)
library(glue)
library(ggrepel)
library(stargazer)
library(latex2exp)
library(igraph)
library(ioanalysis)
library(writexl)
library(extrafont)


# Base de energia OWiD ----------------------------------------------------

# options(timeout = "1000")
# 
# # Descarga via API de la base de energía de OWiD --------------------------
# 
# url_owid <- "https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv"
# filename_owid <- basename(url_owid)
# 
# download.file(url = url_owid,
#               destfile = filename_owid, # Puedes crear una carpeta en tu directorio que conserve estas bases con 'str_c("nombreCarpetaNueva/", filename_owid, sep = "")' en lugar de solamente 'filename_owid'
#               mode = "wb")

# Descarga via API de la base de energía de OWiD sobre CO2 --------------------------

# url_owid <- "https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv"
# filename_owid <- basename(url_owid)
# 
# download.file(url = url_owid,
#               destfile = filename_owid, # Puedes crear una carpeta en tu directorio que conserve estas bases con 'str_c("nombreCarpetaNueva/", filename_owid, sep = "")' en lugar de solamente 'filename_owid'
#               mode = "wb")


base <- read.csv("owid-energy-data.csv")

# unique(base$year)

base_co2 <- read.csv("owid-co2-data.csv")

# base_co2 %>% glimpse()

base <- base %>% 
  mutate(iso_code = ifelse(country == "World", "WOR", iso_code))

base <- base %>% 
  filter(iso_code != "") # Nos quedamos solamente con datos de los paises


# Grafico de participación de fuentes

Ener <- base %>% filter(country == "Mexico" | country == "World") %>% 
  select(country, year, iso_code, ends_with("electricity"))

Ener <- Ener %>% 
  gather(key = tipo, value = valor, 4:length(Ener))

source_elec <- c("biofuel_electricity",
                 "hydro_electricity",
                 "coal_electricity",
                 "gas_electricity",
                 "oil_electricity",
                 "wind_electricity",
                 "solar_electricity",
                 "other_renewable_exc_biofuel_electricity")

map1 <- source_elec %>% 
  map(.f = ~{
    
    Ener %>% 
      filter(tipo == .x,
             year > 1989)
  })


map1 <- map1 %>% bind_rows()

map1 <- map1 %>% 
  mutate(renovable = case_when(
    tipo == "biofuel_electricity" ~ 1,
    tipo == "hydro_electricity" ~ 2, 
    tipo == "solar_electricity" ~ 1, 
    tipo == "wind_electricity" ~ 1,
    tipo == "other_renewable_exc_biofuel_electricity" ~ 1,
    tipo == "coal_electricity" ~ 0,
    tipo == "gas_electricity" ~ 0,
    tipo == "oil_electricity" ~ 0),
    nombre = case_when(
      tipo == "biofuel_electricity" ~ "Bioenergía",
      tipo == "hydro_electricity" ~ "Hidroeléct.", 
      tipo == "solar_electricity" ~ "Solar", 
      tipo == "wind_electricity" ~ "Eólica", 
      tipo == "coal_electricity" ~ "Carbón",
      tipo == "gas_electricity" ~ "Gas",
      tipo == "oil_electricity" ~ "Oil",
      tipo == "other_renewable_exc_biofuel_electricity" ~ "Otras Renov."))

map1 <- map1 %>% 
  mutate(nombre = factor(nombre, levels = c("Carbón", "Oil", "Gas", 
                                            "Bioenergía", "Solar", "Eólica", "Otras Renov.",
                                            "Hidroeléct.")),
         renovable = factor(renovable))

participacionGas2000 <- map1 %>% filter(year == 2000) %>% 
  group_by(country) %>% 
  mutate(prop = valor/sum(valor, na.rm = T)*100)

participacionGas2000 <- participacionGas2000$prop[participacionGas2000$country == "Mexico" & participacionGas2000$tipo == "gas_electricity"]

participacionGas2023 <- map1 %>% filter(year == 2023) %>% 
  group_by(country) %>% 
  mutate(prop = valor/sum(valor, na.rm = T)*100)

participacionGas2023 <- participacionGas2023$prop[participacionGas2023$country == "Mexico" & participacionGas2023$tipo == "gas_electricity"]

loadfonts(device = "win")


alpha_max <- 1
alpha_min <- 0.6
alpha_vals <- c(
  seq(alpha_max, alpha_min, length.out = 3), 
  seq(alpha_max, alpha_min, length.out = 3),
  alpha_max)

graph1 <- map1 %>% 
  filter(year > 1999 & nombre != "Otras Renov." & country == "Mexico") %>%  # Filtro para tener los valores que queremos, elimino otras renovables porque no tienen participación en Colombia
  ggplot(mapping = aes(x = year, 
                       y = valor, 
                       fill = renovable, # Atención al relleno (fill), este va a estar en función de las tres grandes categorias (renovable, no renovable e hidroeléctrica)
                       alpha = nombre    # el alpha da la tonaldad, la cual depende del nombre
  )
  ) + 
  geom_area(col = "white") +
  scale_alpha_manual(values = alpha_vals) + # elijo el nivel de claridad según alpha_vals
  scale_fill_manual(values = c("#000000", "#249206", "#3667A6")) + # Colores en HTML
  scale_x_continuous(breaks = 2000:2023) + # que salgan todos los años en el eje x
  scale_y_continuous(n.breaks = 10) +     # Numero de quiebres en el eje Y   
  labs(title = "Evolución de la generación de electricidad por fuente primaria en México",
       x = NULL,
       y = "Teravatios-Hora",
       caption = "\nFuente: Our World in Data (OWD)\n             @GEsteban_Gomez - 03/10/2024") +
  theme(text = element_text(family = "Rockwell"),
        plot.title = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(size=10, hjust=0.0, color="grey27"),
        axis.text.x = element_text(size = 9, angle = 45, vjust = 0.5, hjust = 0.5, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title.y = element_text(size = 11),
        panel.grid.major = element_line(linetype = "dashed", colour = "lightgrey"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#c9aed1"),
        panel.background = element_rect(fill = "#c9aed1"),
        legend.position = "none")

graph1


map2 <- map1 %>% 
  filter(year == 2023) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  mutate(prop = round(valor/sum(valor, na.rm = T)*100,1)) %>% 
  ungroup() %>% 
  mutate(etiqueta = paste(nombre, prop, "%"),
         etiquetas = ifelse(nombre == "Otras Renov." | nombre == "Bioenergía", NA, etiqueta))


alpha_max <- 1
alpha_min <- 0.6
alpha_vals <- c(
  seq(alpha_max, alpha_min, length.out = 3), 
  seq(alpha_max, alpha_min, length.out = 4),
  alpha_max)


B <- map2 %>% 
  filter(country %in% c("Mexico","World")) %>% 
  ggplot(mapping = aes(x = as.factor(country), y = prop, fill = renovable, alpha = nombre)) +
  geom_col(col = "white", width = 1) +
  scale_alpha_manual(values = alpha_vals) +
  scale_fill_manual(values = c("#000000", "#249206", "#3667A6")) +
  guides(
    fill = guide_none(),
    alpha = guide_legend(override.aes = list(fill = c(rep("#283227", 3),
                                                      rep("#249206", 4),
                                                      "#3667A6")))
  ) +
  scale_y_continuous(position = "right") +
  geom_text(
    data = map2,
    aes(label = etiquetas),
    position = position_stack(vjust = 0.70),
    col = 'white',
    size = 3,
    fontface = 'bold',
    family = "Rockwell"
  ) +
  labs(
    # title = "",
    title = "Participación porcentual por fuente\n2023",
    x = NULL,
    y = "Porcentaje") +
  theme(text = element_text(family = "Rockwell"),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 11, color = "black", hjust = 0.5),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title.y = element_text(size = 11),
        panel.grid.major = element_line(linetype = "dashed", colour = "lightgrey"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#c9aed1"),
        panel.background = element_rect(fill = "#c9aed1"),
        legend.position = "none")

B

graphFinal <- wrap_plots(graph1, B, ncol = 2, widths = c(0.9, 0.5))

ggsave(filename = "matrizEnerMex.png",
      plot = graphFinal,
      dpi = 500,
      # width = ,
      # height = 
        )













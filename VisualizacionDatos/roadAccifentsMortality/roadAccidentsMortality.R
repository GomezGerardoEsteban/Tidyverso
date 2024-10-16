###################################################
# Mortalidad vial
# Author: Gerardo Esteban Gomez Santiago
# Date: 16/10/2024
# 
# Descripción: Este es el código para replicar la visualización de tasa de mortalidad
# por 100 mil habitantes en accidentes de transito, la comparación es con 12 paises, de los cuales
# 6 son de LATAM y en general LATAM muestra tasas mas altas que el resto de la región.
# La base es de la OMS.
#
####################################################



rm(list = ls())

library(tidyverse)
library(ggtext)
library(patchwork)
library(gghighlight)
library(extrafont)
library(wesanderson)
extrafont::loadfonts()

base <- read.csv(file = "RS_196,RS_198.csv")

base %>% glimpse()

# road.traffic.deaths
# road.traffic.death.rate.per.100.000.population

names(base) <- c("country", "year", 
                 "bothDeaths", "maleDeaths", "femaleDeaths",
                 "bothRate", "maleRate", "femaleRate")

base <- base[-1, ]

base2 <- base %>% 
  select(-starts_with(c("male","female")), -bothDeaths)

base2 <- base2 %>% 
  separate(col = bothRate, into = c("rate", "interval"), sep = " ")

base2$rate <- as.numeric(base2$rate)
base2$year <- as.numeric(base2$year)

unique(base2$country)

countries <- c("Argentina",
               "Brazil",
               "Chile",
               "Colombia",
               "Mexico",
               "Venezuela (Bolivarian Republic of)",
               "Japan",
               "Republic of Korea",
               "China",
               "United States of America",
               "Sweden",
               "Germany")

base3 <- base2 %>% 
  filter(country %in% countries)

unique(base3$country)

base3$country <- case_when(base3$country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                           base3$country == "Republic of Korea" ~ "Corea",
                           base3$country == "Sweden" ~ "Suecia",
                           base3$country == "Germany" ~ "Alemania",
                           base3$country == "United States of America" ~ "Estados Unidos",
                           base3$country == "Brazil" ~ "Brasil",
                           base3$country == "Japan" ~ "Japón",
                           base3$country == "Mexico" ~ "México",
                           base3$country == "Colombia" ~ "Colombia",
                           base3$country == "Argentina" ~ "Argentina",
                           base3$country == "Chile" ~ "Chile",
                           base3$country == "China" ~ "China")


caption_text  <- str_glue("**Elaboración:** @GEsteban_Gomez - 16/10/2024<br>","**Fuente:** World Health Organization")

colors <- c("#6CACE4", "#DA291C", "#009739", 
  "#003087", "#006847", "#5e2129",
  "#BC002D", "#0F64CD", "#ee1c25",
  "#006AA7", "#000000", "#0A3161")


base3$country <- factor(x = base3$country,
                        levels = c("Argentina",
                                   "Chile",
                                   "Brasil",
                                   "Colombia",
                                   "México",
                                   "Venezuela",
                                   "Japón",
                                   "Corea",
                                   "China",
                                   "Suecia",
                                   "Alemania",
                                   "Estados Unidos"))

unique(base3$country)

p1 <- base3 %>% 
  ggplot() +
  geom_hline(yintercept = 10,linetype="dashed", linewidth=.01, alpha = 0.6) +
  geom_point(data=base3 %>% 
               group_by(country) %>% 
               slice_max(year),
             aes(x=year, y=rate, color=country),
             shape=16,
             show.legend = F) +
  geom_line(aes(x=year, y=rate, color=country),
            show.legend = F) +
  gghighlight(use_direct_label = FALSE,
              unhighlighted_params = list(colour = alpha("grey80", 1))) +
  geom_text(data=base3 %>% 
              group_by(country) %>% 
              slice_max(year),
            aes(x=year+1.2, y=rate, color=country, label = round(rate)), 
            vjust = .5, 
            size=3, 
            family="Consolas",
            fontface="bold",
            show.legend = F) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = c(2000, 2010, 2019)) +
  scale_y_continuous(n.breaks = 5) +
  facet_wrap(~ country, nrow = 4, ncol = 3) +
  coord_cartesian(clip = "off") +
  labs(y = "Muertes en accidentes de transito por 100 mil habitantes") +
  theme(
    text = element_text(family = "Consolas"),
    axis.title.y = element_text(size = 12, 
                                face = "bold",
                                margin = margin(t = 0, r =15, b = 0, l = 0)),
    axis.title.x = element_blank(),
    axis.text = element_text(color="black", size=9),
    axis.text.x = element_text(color="black", size=9, angle = 45, hjust = 0.5, vjust = 0.5),
    strip.text.x = element_text(face="bold"),
    strip.background = element_rect(fill = "#c2c26f", linetype = "solid",
                                    color = "#c2c26f", linewidth = 1),
    plot.title = element_markdown(hjust=.5,
                                  size=34, 
                                  color="black",
                                  lineheight=.8, 
                                  face="bold", 
                                  margin=margin(20,0,30,0)),
    plot.subtitle = element_markdown(hjust=.5,
                                     size=18, 
                                     color="black",
                                     lineheight = 1, 
                                     margin=margin(10,0,30,0)),
    plot.caption = element_markdown(hjust=.5, 
                                    margin=margin(60,0,0,0), 
                                    size=9, 
                                    color="black", 
                                    lineheight = 1),
    plot.caption.position = "plot",
    plot.background = element_rect(color="#f9f994", fill="#f9f994"),
    panel.background = element_rect(fill = "#ffffb4"),
    panel.grid = element_blank(),
    plot.margin = margin(10,10,10,10),
    legend.title = element_text(face="bold")
  )


text <- tibble(
  x = 0, y = 0,
  label = "Los accidentes de tránsito son la principal causa de muerte en adultos jóvenes. Según la OMS, el 92% de las muertes ocurren en países de ingresos medios y bajos, teniendo solamente el 60% de los vehículos. En el gráfico se observa la diferencia de algunos países de LATAM respecto a otras regiones. Mejorar la infraestructura vial, incrementar la vigilancia y abordar con campañas de concientización a la población, son medidas necesarias para que la tendencia sea a la baja.<br>"
)

sub <- ggplot(text, aes(x = x, y = y)) +
  geom_textbox(
    aes(label = label),
    box.color = "#f9f994", fill= "#ffffb4", width = unit(15, "lines"),
    family="Consolas", size = 3, lineheight = 1
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(color="#f9f994", fill="#f9f994"))


# TITLE
text2 <- tibble(
  x = 0, y = 0,
  label = "**MORTALIDAD<br>EN LAS VÍAS<br>**"
)

title <- ggplot(text2, aes(x = x, y = y)) +
  geom_textbox(
    aes(label = label),
    box.color = "#f9f994", fill="#f9f994", width = unit(15, "lines"),
    family="Consolas", size = 12, lineheight = 0.8
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(color="#f9f994", fill="#f9f994"))

finalPlot <- (title+sub)/p1 +
  plot_layout(heights = c(0.7, 2)) +
  plot_annotation(
    caption = caption_text,
    theme=theme(plot.caption = element_markdown(hjust=0, family = "Consolas", margin=margin(15,0,0,0), size=9, color="black", lineheight = 1.2),
                plot.margin = margin(10,10,10,10)))


ggsave(filename = "roadSpaguetti.png",
       plot = finalPlot,
       width = 7,
       height = 9,
       dpi = 500)



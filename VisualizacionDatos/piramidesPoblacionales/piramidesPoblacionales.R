################################################################################
# Titulo: Piramides poblacionales de Kenia, Colombia y Japón
# Autor: Gerardo Esteban Gómez-Santiago
# Fecha: 16-09-2024
# Fuente: U.S. Census Bureau
################################################################################


# limpiamos el ambiente ---------------------------------------------------

rm(list = ls())

# activación de paquetes --------------------------------------------------

library(tidyverse)
library(ggtext)

# levantamos las bases de datos -------------------------------------------

archivos <- list.files(pattern = "population*")

bases <- list()
for(i in 1:length(archivos)){
  
  bases[[i]] <- read.csv(file = archivos[i])
  
}

bases <- bases %>% bind_rows()

bases %>% glimpse()


# Procesamiento de los datos ----------------------------------------------

bases$Male.Population <- bases$Male.Population*-1

bases <- bases %>% 
  mutate(across(.cols = c("Population", 
                          "Male.Population", 
                          "Female.Population"),
                .fns = ~{.x/1000}))

bases <- bases %>% 
  select(c(1:5, 7, 9)) %>% 
  gather(key = variable, value = valor, 5:7)

bases <- bases %>% 
  rename(Country = Country.Area.Name)

bases$Country <- factor(bases$Country,
                     levels = c("Kenya", "Colombia", "Japan"),
                     labels = c("Kenia:\nPirámide progresiva",
                                "Colombia:\nPirámde estable",
                                "Japón:\nPirámide regresiva"))

bases <- bases %>% 
  filter(variable != "Population" & GROUP != "TOTAL") %>%
  mutate(GROUP = as.numeric(ifelse(GROUP == "100+", "100", GROUP)),
         variable = factor(ifelse(nchar(variable) == 15, 
                                  str_sub(variable,
                                          start = 1,
                                          end = 4),
                                  str_sub(variable,
                                          start = 1,
                                          end = 6)),
                           levels = c("Male", "Female")))

popag <- bases %>%
  mutate(Agegr = factor(ifelse(GROUP %in% 0:14, "young",
                               ifelse(GROUP %in% 15:64, "adult", "old")))) %>%
  group_by(GENC, variable, Agegr) %>%
  summarise(Population = sum(valor)) %>%
  mutate(Population = ifelse(Population < 0, Population*-1, Population)) %>% 
  ungroup() %>% 
  group_by(GENC) %>% 
  mutate(Population_T = sum(Population)) %>% 
  ungroup() %>% 
  group_by(GENC, variable) %>% 
  mutate(PopulationSex_T = sum(Population)) %>% 
  ungroup() %>% 
  mutate(PopulationPercent = round(Population/Population_T*100, 1))


# seleccion de colores ----------------------------------------------------

library(wesanderson)
library(extrafont)
# loadfonts(device = "win")

colores <- wes_palette(name = "Darjeeling1", n = 2, type = c("discrete"))


# Grafico conjunto de piramides poblacionales -----------------------------

graficoPob <- bases %>%  
  ggplot(mapping = aes(y = valor,
                       x = GROUP,
                       fill = variable)) +
  geom_col(colour = "black",
           show.legend = F,
           width = 1.2) +
  facet_wrap(~ Country, 
             scales = "free_x") +
  coord_flip() +
  scale_y_continuous(breaks = seq(-1000,1000,200),
                     labels = c(rev(seq(0,1000,200)),
                                seq(200,1000,200))) +
  scale_x_continuous(n.breaks = 15) +
  scale_fill_manual(values = colores) +
  labs(title = "Tipos de Pirámide Poblacional\n",
       subtitle = "Composición por edad y sexo de **Kenia**, **Colombia** y **Japón** al 2024 - <span style='color:#FF0000;'>**Hombres**</span> y <span style='color:#00A08A;'>**Mujeres**</span>",
       x = "Edad",
       y = "Miles de habitantes",
       caption = "Fuente: U.S. Census Bureau\n             @GEsteban_Gomez - 16/09/2024") +
  theme(text = element_text(family = "Book Antiqua"),
        plot.title = element_text(hjust = 0.5,
                                  size = 20,
                                  face = "bold"),
        plot.subtitle = element_markdown(hjust = 0.5,
                                         size = 15),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(angle = 60,
                                   vjust = 0.5,
                                   hjust = 0.3,
                                   face = "bold"),
        plot.background = element_rect(fill = "grey95"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey27", linetype = "dotted"),
        panel.grid.minor = element_line(colour = "grey87", size = 0.3),
        strip.text = element_text(face = "bold", color = "black", hjust = 0.5, size = 12),
        strip.background = element_rect(fill = "white", linetype = "solid",
                                        color = "black", linewidth = 0.5))


# Piramide poblacional de Colombia ----------------------------------------

graficoPobCol <- bases %>%
  filter(GENC == "CO") %>%
  ggplot(mapping = aes(y = valor,
                       x = GROUP,
                       fill = variable)) +
  geom_col(colour = "black",
           show.legend = F,
           width = 1.2) +
  coord_flip() +
  annotate("segment", x = 14, xend = 14, y = -600, yend = 600,colour = "grey30", 
           linewidth = 1.,
           linetype = "dashed")+
  annotate("segment", x = 64, xend = 64, y = -600, yend = 600,colour = "grey30", 
           linewidth = 1.,
           linetype = "dashed") +
  annotate("text", 
           x = 100, 
           y = -300,
           label = paste("Hombres", 
                         format(popag$PopulationSex_T[popag$GENC == "CO" & popag$variable == "Male"][1]*1000, big.mark = ",", scientific = F), sep = "\n "), 
           size = 5,
           family = "Book Antiqua",
           fontface = "bold.italic",
           color = colores[1]) +
  annotate("text", 
           x = 100, 
           y = 300,
           label = paste("Mujeres",
                         format(popag$PopulationSex_T[popag$GENC == "CO" & popag$variable == "Female"][1]*1000, big.mark = ",", scientific = F), sep = "\n "),
           size = 5,
           family = "Book Antiqua",
           fontface = "bold.italic",
           color = colores[2])+ 
  annotate("text", 
           x = 5, 
           y = -500,
           label = paste("Hombres jovenes y\nniños (0-14)", 
                         paste(format(popag$PopulationPercent[popag$GENC == "CO" & popag$variable == "Male" & popag$Agegr == "young"], big.mark = ",", scientific = F), " %", sep = ""),
                         sep = "\n "), 
           size = 3.5,
           family = "Book Antiqua") +
  annotate("text", 
           x = 5, 
           y = 500,
           label = paste("Mujeres jovenes y\nniñas (0-14)", 
                         paste(format(popag$PopulationPercent[popag$GENC == "CO" & popag$variable == "Female" & popag$Agegr == "young"], big.mark = ",", scientific = F), " %", sep = ""),
                         sep = "\n "), 
           size = 3.5,
           family = "Book Antiqua") +
  annotate("text", 
           x = 50, 
           y = -500,
           label = paste("Hombres adultos (15-64)", 
                         paste(format(popag$PopulationPercent[popag$GENC == "CO" & popag$variable == "Male" & popag$Agegr == "adult"], big.mark = ",", scientific = F), " %", sep = ""),
                         sep = "\n "), 
           size = 3.5,
           family = "Book Antiqua") +
  annotate("text", 
           x = 50, 
           y = 500,
           label = paste("Mujeres adultas (15-64)", 
                         paste(format(popag$PopulationPercent[popag$GENC == "CO" & popag$variable == "Female" & popag$Agegr == "adult"], big.mark = ",", scientific = F), " %", sep = ""),
                         sep = "\n "), 
           size = 3.5,
           family = "Book Antiqua") +
  annotate("text", 
           x = 75, 
           y = -500,
           label = paste("Hombres mayores (65 o más)", 
                         paste(format(popag$PopulationPercent[popag$GENC == "CO" & popag$variable == "Male" & popag$Agegr == "old"], big.mark = ",", scientific = F), " %", sep = ""),
                         sep = "\n "), 
           size = 3.5,
           family = "Book Antiqua") +
  annotate("text", 
           x = 75, 
           y = 500,
           label = paste("Mujeres mayores (65 o más)", 
                         paste(format(popag$PopulationPercent[popag$GENC == "CO" & popag$variable == "Female" & popag$Agegr == "old"], big.mark = ",", scientific = F), " %", sep = ""),
                         sep = "\n "), 
           size = 3.5,
           family = "Book Antiqua") +
  scale_y_continuous(breaks = seq(-400,400,100),
                     labels = c(rev(seq(0,400,100)),
                                seq(100,400,100))) +
  scale_x_continuous(n.breaks = 15,
                     limits = c(0, 110)) +
  scale_fill_manual(values = colores) +
  labs(title = "Pirámide poblacional de Colombia - 2024",
       x = "Edad",
       y = "Miles de habitantes",
       caption = "Fuente: U.S. Census Bureau\n             @GEsteban_Gomez - 16/09/2024") +
  theme(text = element_text(family = "Book Antiqua"),
        plot.title = element_text(hjust = 0.5,
                                  size = 20,
                                  face = "bold"),
        plot.subtitle = element_markdown(hjust = 0.5,
                                         size = 15),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(angle = 0,
                                   vjust = 0.5,
                                   hjust = 0.3,
                                   face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.background = element_rect(fill = "grey95"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "grey87", 
                                        size = 0.15),
        strip.text = element_text(face = "bold", color = "black", hjust = 0.5, size = 12),
        strip.background = element_rect(fill = "white", linetype = "solid",
                                        color = "black", linewidth = 0.5))



# Piramide poblacional de Kenia -------------------------------------------

graficoPobKen <- bases %>%
  filter(GENC == "KE") %>%
  ggplot(mapping = aes(y = valor,
                       x = GROUP,
                       fill = variable)) +
  geom_col(colour = "black",
           show.legend = F,
           width = 1.2) +
  coord_flip() +
  annotate("segment", x = 14, xend = 14, y = -1000, yend = 1000,colour = "grey30", 
           linewidth = 1.,
           linetype = "dashed")+
  annotate("segment", x = 64, xend = 64, y = -1000, yend = 1000,colour = "grey30", 
           linewidth = 1.,
           linetype = "dashed") +
  annotate("text", 
           x = 100, 
           y = -300,
           label = paste("Hombres", 
                         format(popag$PopulationSex_T[popag$GENC == "KE" & popag$variable == "Male"][1]*1000, big.mark = ",", scientific = F), sep = "\n "), 
           size = 5,
           family = "Book Antiqua",
           fontface = "bold.italic",
           color = colores[1]) +
  annotate("text", 
           x = 100, 
           y = 300,
           label = paste("Mujeres",
                         format(popag$PopulationSex_T[popag$GENC == "KE" & popag$variable == "Female"][1]*1000, big.mark = ",", scientific = F), sep = "\n "),
           size = 5,
           family = "Book Antiqua",
           fontface = "bold.italic",
           color = colores[2])+ 
  annotate("text", 
           x = 5, 
           y = -880,
           label = paste("Hombres jovenes y\nniños (0-14)", 
                         paste(format(popag$PopulationPercent[popag$GENC == "KE" & popag$variable == "Male" & popag$Agegr == "young"], big.mark = ",", scientific = F), " %", sep = ""),
                         sep = "\n "), 
           size = 3.5,
           family = "Book Antiqua") +
  annotate("text", 
           x = 5, 
           y = 880,
           label = paste("Mujeres jovenes y\nniñas (0-14)", 
                         paste(format(popag$PopulationPercent[popag$GENC == "KE" & popag$variable == "Female" & popag$Agegr == "young"], big.mark = ",", scientific = F), " %", sep = ""),
                         sep = "\n "), 
           size = 3.5,
           family = "Book Antiqua") +
  annotate("text", 
           x = 50, 
           y = -500,
           label = paste("Hombres adultos (15-64)", 
                         paste(format(popag$PopulationPercent[popag$GENC == "KE" & popag$variable == "Male" & popag$Agegr == "adult"], big.mark = ",", scientific = F), " %", sep = ""),
                         sep = "\n "), 
           size = 3.5,
           family = "Book Antiqua") +
  annotate("text", 
           x = 50, 
           y = 500,
           label = paste("Mujeres adultas (15-64)", 
                         paste(format(popag$PopulationPercent[popag$GENC == "KE" & popag$variable == "Female" & popag$Agegr == "adult"], big.mark = ",", scientific = F), " %", sep = ""),
                         sep = "\n "), 
           size = 3.5,
           family = "Book Antiqua") +
  annotate("text", 
           x = 75, 
           y = -500,
           label = paste("Hombres mayores (65 o más)", 
                         paste(format(popag$PopulationPercent[popag$GENC == "KE" & popag$variable == "Male" & popag$Agegr == "old"], big.mark = ",", scientific = F), " %", sep = ""),
                         sep = "\n "), 
           size = 3.5,
           family = "Book Antiqua") +
  annotate("text", 
           x = 75, 
           y = 500,
           label = paste("Mujeres mayores (65 o más)", 
                         paste(format(popag$PopulationPercent[popag$GENC == "KE" & popag$variable == "Female" & popag$Agegr == "old"], big.mark = ",", scientific = F), " %", sep = ""),
                         sep = "\n "), 
           size = 3.5,
           family = "Book Antiqua") +
  scale_y_continuous(breaks = seq(-700,700,100),
                     labels = c(rev(seq(0,700,100)),
                                seq(100,700,100))) +
  scale_x_continuous(n.breaks = 15,
                     limits = c(0, 110)) +
  scale_fill_manual(values = colores) +
  labs(title = "Pirámide poblacional de Kenia - 2024",
       x = "Edad",
       y = "Miles de habitantes",
       caption = "Fuente: U.S. Census Bureau\n             @GEsteban_Gomez - 16/09/2024") +
  theme(text = element_text(family = "Book Antiqua"),
        plot.title = element_text(hjust = 0.5,
                                  size = 20,
                                  face = "bold"),
        plot.subtitle = element_markdown(hjust = 0.5,
                                         size = 15),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(angle = 0,
                                   vjust = 0.5,
                                   hjust = 0.3,
                                   face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.background = element_rect(fill = "grey95"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "grey87", 
                                        size = 0.15),
        strip.text = element_text(face = "bold", color = "black", hjust = 0.5, size = 12),
        strip.background = element_rect(fill = "white", linetype = "solid",
                                        color = "black", linewidth = 0.5))


# Piramide poblacional de Japon -------------------------------------------

graficoPobJap <- bases %>%
  filter(GENC == "JP") %>%
  ggplot(mapping = aes(y = valor,
                       x = GROUP,
                       fill = variable)) +
  geom_col(colour = "black",
           show.legend = F,
           width = 1.2) +
  coord_flip() +
  annotate("segment", x = 14, xend = 14, y = -1200, yend = 1200,colour = "grey30", 
           linewidth = 1.,
           linetype = "dashed")+
  annotate("segment", x = 64, xend = 64, y = -1200, yend = 1200,colour = "grey30", 
           linewidth = 1.,
           linetype = "dashed") +
  annotate("text", 
           x = 105, 
           y = -400,
           label = paste("Hombres", 
                         format(popag$PopulationSex_T[popag$GENC == "JP" & popag$variable == "Male"][1]*1000, big.mark = ",", scientific = F), sep = "\n "), 
           size = 5,
           family = "Book Antiqua",
           fontface = "bold.italic",
           color = colores[1]) +
  annotate("text", 
           x = 105, 
           y = 400,
           label = paste("Mujeres",
                         format(popag$PopulationSex_T[popag$GENC == "JP" & popag$variable == "Female"][1]*1000, big.mark = ",", scientific = F), sep = "\n "),
           size = 5,
           family = "Book Antiqua",
           fontface = "bold.italic",
           color = colores[2])+ 
  annotate("text", 
           x = 5, 
           y = -800,
           label = paste("Hombres jovenes y\nniños (0-14)", 
                         paste(format(popag$PopulationPercent[popag$GENC == "JP" & popag$variable == "Male" & popag$Agegr == "young"], big.mark = ",", scientific = F), " %", sep = ""),
                         sep = "\n "), 
           size = 3.5,
           family = "Book Antiqua") +
  annotate("text", 
           x = 5, 
           y = 800,
           label = paste("Mujeres jovenes y\nniñas (0-14)", 
                         paste(format(popag$PopulationPercent[popag$GENC == "JP" & popag$variable == "Female" & popag$Agegr == "young"], big.mark = ",", scientific = F), " %", sep = ""),
                         sep = "\n "), 
           size = 3.5,
           family = "Book Antiqua") +
  annotate("text", 
           x = 30, 
           y = -1000,
           label = paste("Hombres adultos (15-64)", 
                         paste(format(popag$PopulationPercent[popag$GENC == "JP" & popag$variable == "Male" & popag$Agegr == "adult"], big.mark = ",", scientific = F), " %", sep = ""),
                         sep = "\n "), 
           size = 3.5,
           family = "Book Antiqua") +
  annotate("text", 
           x = 30, 
           y = 1000,
           label = paste("Mujeres adultas (15-64)", 
                         paste(format(popag$PopulationPercent[popag$GENC == "JP" & popag$variable == "Female" & popag$Agegr == "adult"], big.mark = ",", scientific = F), " %", sep = ""),
                         sep = "\n "), 
           size = 3.5,
           family = "Book Antiqua") +
  annotate("text", 
           x = 85, 
           y = -1000,
           label = paste("Hombres mayores (65 o más)", 
                         paste(format(popag$PopulationPercent[popag$GENC == "JP" & popag$variable == "Male" & popag$Agegr == "old"], big.mark = ",", scientific = F), " %", sep = ""),
                         sep = "\n "), 
           size = 3.5,
           family = "Book Antiqua") +
  annotate("text", 
           x = 85, 
           y = 1000,
           label = paste("Mujeres mayores (65 o más)", 
                         paste(format(popag$PopulationPercent[popag$GENC == "JP" & popag$variable == "Female" & popag$Agegr == "old"], big.mark = ",", scientific = F), " %", sep = ""),
                         sep = "\n "), 
           size = 3.5,
           family = "Book Antiqua") +
  scale_y_continuous(breaks = seq(-1000,1000,200),
                     labels = c(rev(seq(0,1000,200)),
                                seq(200,1000,200))) +
  scale_x_continuous(n.breaks = 15,
                     limits = c(0, 110)) +
  scale_fill_manual(values = colores) +
  labs(title = "Pirámide poblacional de Japón - 2024",
       x = "Edad",
       y = "Miles de habitantes",
       caption = "Fuente: U.S. Census Bureau\n             @GEsteban_Gomez - 16/09/2024") +
  theme(text = element_text(family = "Book Antiqua"),
        plot.title = element_text(hjust = 0.5,
                                  size = 20,
                                  face = "bold"),
        plot.subtitle = element_markdown(hjust = 0.5,
                                         size = 15),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(angle = 0,
                                   vjust = 0.5,
                                   hjust = 0.3,
                                   face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.background = element_rect(fill = "grey95"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "grey87", 
                                        size = 0.15),
        strip.text = element_text(face = "bold", color = "black", hjust = 0.5, size = 12),
        strip.background = element_rect(fill = "white", linetype = "solid",
                                        color = "black", linewidth = 0.5))



# Almacenamiento de los graficos para no perder resolución ----------------

ggsave(filename = "resultados/grafico_pob.png",
       plot = graficoPob,
       width = 9.39,
       height = 5.81,
       dpi = 500)

ggsave(filename = "resultados/grafico_pob_col.png",
       plot = graficoPobCol,
       width = 8.36,
       height = 5.81,
       dpi = 500)

ggsave(filename = "resultados/grafico_pob_ken.png",
       plot = graficoPobKen,
       width = 8.36,
       height = 5.81,
       dpi = 500)

ggsave(filename = "resultados/grafico_pob_jap.png",
       plot = graficoPobJap,
       width = 8.36,
       height = 5.81,
       dpi = 500)


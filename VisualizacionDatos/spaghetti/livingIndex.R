

rm(list = ls())

library(tidyverse)
library(ggtext)
library(ggrepel)
library(patchwork)
library(gghighlight)
library(extrafont)


base <- read.csv(file = "global-living-planet-index.csv")


base %>% glimpse()


LPI_original <- base %>% 
  filter(Entity !=  "Freshwater") %>% 
  mutate(Code = case_when(Entity == names[1] ~ "Africa",
                          Entity == names[2] ~ "Asia",
                          Entity == names[3] ~ "Europa",
                          Entity == names[5] ~ "LATAM",
                          Entity == names[6] ~ "North America",
                          Entity == names[7] ~ "World")) %>% 
  ggplot(mapping = aes(x = Year, y = Living.Planet.Index, color = Code)) +
  geom_line() +
  labs(title = "Índice Planeta Vivo por regiones\n(1970 - 2020)",
       y = "Índice Planeta Vivo (1970 = 100)",
       x = NULL)+
  # caption = "Datos: Our World in Data | Elaboración: @GEsteban_Gomez | 25/10/2024") +
  scale_color_manual(values = c("darkred",
                                "royalblue2",
                                "saddlebrown",
                                "darkgreen",
                                "darkblue",
                                "black"))


ggsave(filename = "LPI_original.png",
       plot = LPI_original,
       width = 8.51,
       height =  4.9,
       dpi = 500)


LPI_1 <- base %>% 
  filter(Entity != "Freshwater") %>% 
  ggplot() +
  geom_point(data = base %>% 
               filter(Entity != "Freshwater") %>%
               group_by(Entity) %>% 
               slice_max(Year), 
             mapping = aes(x = Year, 
                           y = Living.Planet.Index,
                           color = Entity),
             show.legend = F) +
  geom_point(data = base %>% 
               filter(Entity != "Freshwater") %>%
               group_by(Entity) %>% 
               slice_min(Year), 
             mapping = aes(x = Year, 
                           y = Living.Planet.Index,
                           color = Entity),
             show.legend = F) +
  geom_line(mapping = aes(x = Year, 
                          y = Living.Planet.Index, 
                          color = Entity),
            show.legend = F) +
  geom_hline(mapping = aes(yintercept = 100),
             linetype = "dashed") +
  scale_color_manual(values = c("darkred",
                                "royalblue2",
                                "saddlebrown",
                                "darkgreen",
                                "darkblue",
                                "black")) +
  gghighlight(use_direct_label = FALSE,
              unhighlighted_params = list(colour = alpha("grey70", 1))) +
  geom_text(data = base %>% 
              filter(Entity != "Freshwater") %>%
              group_by(Entity) %>% 
              slice_max(Year),
            aes(x = Year + 3,
                y = Living.Planet.Index,
                label = round(Living.Planet.Index, 1),
                color = Entity),
            size = 3,
            show.legend = F) +
  facet_wrap(~ Entity, nrow = 3, ncol = 2) +
  labs(title = "Índice Planeta Vivo por regiones\n(1970 - 2020)",
       y = "Índice Planeta Vivo (1970 = 100)",
       x = NULL,
       caption = "Datos: Our World in Data | Elaboración: @GEsteban_Gomez | 25/10/2024") +
  theme(text = element_text(family = "Rockwell"),
        plot.title = element_text(hjust = 0.5,
                                  size = 14),
        plot.background = element_rect(fill = "olivedrab1"),
        panel.background = element_rect(fill = "olivedrab1"),
        panel.grid.major = element_line(linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "black"))


ggsave(filename = "LPI_highligth.png",
       plot = LPI_1,
       width = 6.5, 
       height = 7,
       dpi = 500) 

names <- unique(base$Entity)

labels <- base %>% 
  filter(Entity != "Freshwater") %>%
  group_by(Entity) %>% 
  slice_max(Year) %>% 
  mutate(var = (Living.Planet.Index - 100),
         Code = case_when(Entity == names[1] ~ "Africa",
                          Entity == names[2] ~ "Asia",
                          Entity == names[3] ~ "Europa",
                          Entity == names[5] ~ "LATAM",
                          Entity == names[6] ~ "North America",
                          Entity == names[7] ~ "World"),
         labels = str_c(Code, " ", round(Living.Planet.Index, 1), " (", round(var), "%)", sep = ""),
         Year = ifelse(nchar(labels) > 18, Year+3, Year+2))
  
LPI_2 <- base %>% 
  filter(Year %in% c(min(Year), max(Year)) & Entity != "Freshwater") %>% 
  select(Entity, Year, Living.Planet.Index) %>% 
  spread(key = Year, value = Living.Planet.Index) %>% 
  ggplot() +
  geom_point(mapping = aes(x = 1970, y = `1970`)) +
  geom_point(mapping = aes(x = 2020, 
                           y = `2020`,
                           color = Entity),
             show.legend = F) +
  geom_segment(mapping = aes(x = 1970, xend = 2020,
                             y = `1970`, yend = `2020`,
                             color = Entity),
               show.legend = F) +
  geom_label_repel(data = labels,
            mapping = aes(x = Year, 
                          y = Living.Planet.Index,
                          label = labels,
                          color = Entity),
            show.legend = F,
            size = 2.5) +
  scale_color_manual(values =  c("darkred",
                                 "royalblue2",
                                 "saddlebrown",
                                 "darkgreen",
                                 "darkblue",
                                 "black")) +
  geom_hline(mapping = aes(yintercept = 100), 
             linetype = "dashed") +
  labs(title = "Índice Planeta Vivo por regiones\n(1970 - 2020)",
       y = "Índice Planeta Vivo (1970 = 100)",
       x = NULL,
       caption = "Datos: Our World in Data | Elaboración: @GEsteban_Gomez | 25/10/2024") +
  theme(text = element_text(family = "Rockwell"),
        plot.title = element_text(hjust = 0.5,
                                  size = 14),
        plot.background = element_rect(fill = "olivedrab1"),
        panel.background = element_rect(fill = "olivedrab1"),
        panel.grid.major = element_line(linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "black"))


ggsave(filename = "LPI_2.png",
       plot = LPI_2,
       width = 8.51,
       height =  4.9,
       dpi = 500) 















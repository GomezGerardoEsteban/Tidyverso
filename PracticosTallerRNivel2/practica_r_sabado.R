
# Repaso de R para iniciar el nivel 2


# limpieza del ambiente de trabajo ----------------------------------------

rm(list = ls())


# librerias ---------------------------------------------------------------

library(tidyverse)

# cargar los datos --------------------------------------------------------

base <- read.csv("https://raw.githubusercontent.com/owid/energy-data/refs/heads/master/owid-energy-data.csv")

base %>% glimpse()

# ¿Que paises tenemos?

unique(base$country)

# ¿QUe años tenemos?

base %>% 
  select(year) %>% 
  distinct()

# Analicemos los datos de algunos países en concreto (Colombia, México, España)

vec_paises <- c("COL", "MEX", "ESP")

base_paises <- base %>% 
  filter(iso_code %in% vec_paises) %>% 
  select(country, iso_code, year, population, 
         per_capita_electricity, greenhouse_gas_emissions)

# Corroboremos que hicimos las cosas bien

unique(base_paises$country)


# Media y el desvio del consumo per capita de electricidad por país

base_paises %>% 
  group_by(country) %>% 
  summarise(media_elec = mean(per_capita_electricity, na.rm = T),
            desvio_elec = sd(per_capita_electricity, na.rm = T))

# Visualicemos los datos

base_paises %>% 
  ggplot(mapping = aes(x = country, y = per_capita_electricity)) +
  geom_boxplot()

# Tunear grafico

base_paises %>% 
  ggplot(mapping = aes(x = country,
                       y = per_capita_electricity,
                       fill = country)) +
  geom_boxplot(show.legend = F) +
  labs(title = "Comparación en el consumo de electricidad",
       x = NULL,
       y = "Kilovatios-hora") +
  scale_fill_manual(values = c("yellow",
                               "darkgreen",
                               "darkred")) +
  theme_minimal()

# Crear una variable

base <- base %>% 
  mutate(geh_per_capita = greenhouse_gas_emissions/population*1000000)

mean(base$geh_per_capita, na.rm = T)

# Visualicemos la nueva variable

base %>% 
  filter(iso_code %in% vec_paises) %>% 
  ggplot(mapping = aes(x = geh_per_capita)) +
  geom_histogram(bins = 15,
                 color = "black") +
  facet_wrap(~ country)


# Tasa de crecimiento equivalente anual - calcularla entre 2000 - 2024
# para los tres países que estamos analizando
# (Valor_Final/Valor_Inicial)^(1/numero de anios) - 1

# Opcion 1

base %>% 
  filter(iso_code %in% vec_paises,
         year %in% c(2000, 2024)) %>% 
  select(iso_code, country, year, geh_per_capita) %>% 
  pivot_wider(names_from = year,
              values_from = geh_per_capita,
              names_prefix = "year_") %>% 
  mutate(tasa = (year_2024/year_2000)^(1/24) - 1)

base %>% 
  filter(iso_code %in% vec_paises,
         year %in% c(2000, 2024)) %>% 
  select(iso_code, country, year, per_capita_electricity) %>% 
  pivot_wider(names_from = year,
              values_from = "per_capita_electricity",
              names_prefix = "year_") %>% 
  mutate(tasa = (year_2024/year_2000)^(1/24) - 1)

# Opcion 2: crear función

# nombre_func <- function("argumentos"){
#   
#   # proceso
#   
# }

tasa_func <- function(paises, variable) {
  
  resultado <- base %>% 
    filter(iso_code %in% paises,
           year %in% c(2000, 2024)) %>% 
    select(iso_code, country, year, all_of(variable)) %>% 
    pivot_wider(names_from = year,
                values_from = .data[[variable]],
                names_prefix = "year_") %>% 
    mutate(tasa = (year_2024/year_2000)^(1/24) - 1)
  
  return(resultado)
  
}


tasa_func(paises = vec_paises, variable = "geh_per_capita")
tasa_func(paises = vec_paises, variable = "per_capita_electricity")
tasa_func(paises = c(c("USA", "CAN", "JPN"), vec_paises), 
          variable = "geh_per_capita")
tasa_func(paises = c("USA", "CAN", "JPN"), variable = "population")


vars <- names(base)
vars <- vars[endsWith(x = vars, suffix = "electricity")]
vars <- vars[c(1,2,4,5,7,8,13,14)]
vars

for (var in 1:length(vars)) {
  
  print(vars[var])
  
  tabla <- tasa_func(paises = c(c("USA", "CAN", "JPN"), vec_paises),
                     variable = vars[var])
  
  print(tabla)
  
}

lista_result <- vector(mode = "list", length = length(vars))

for (var in 1:length(vars)) {
  
  tabla <- tasa_func(paises = vec_paises,
                     variable = vars[var]) %>% 
    mutate(variable = vars[var])
  
  lista_result[[var]] <- tabla
  
}

base_tasas <- lista_result %>% bind_rows()

base_tasas <- base_tasas %>% 
  separate(col = variable, into = c("name","exc"), sep = "_")

base_tasas <- base_tasas %>% 
  mutate(name = str_to_title(name))

base_tasas

base_tasas %>% 
  ggplot(mapping = aes(x = tasa*100, y = name)) +
  geom_col(mapping = aes(fill = country),
           color = "black",
           show.legend = F) +
  facet_wrap(~ country)


# Visualizacion tuneada ---------------------------------------------------

library(extrafont)

graph <- base_tasas %>% 
  mutate(etiq = str_c(round(tasa*100,1),"%"),
         ubic = ifelse(test = tasa > 0,
                       1,
                       -1)) %>% 
  ggplot(mapping = aes(x = tasa*100, y = name)) +
  geom_col(mapping = aes(fill = country),
           color = "black",
           show.legend = F,
           width = 0.7) +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "darkred") +
  geom_text(mapping = aes(label = etiq, x = tasa*100 + (5.5*ubic)),
            size = 2.7,
            family = "Rockwell") +
  scale_fill_manual(values = c("yellow3", "palegreen3", "firebrick3")) +
  facet_wrap(~ country) +
  scale_x_continuous(limits = c(-20, 50),
                     breaks = c(-10,0,10,20,30,40),
                     labels = str_c(c(-10,0,10,20,30,40), "%")) +
  labs(title = "Crecimiento de la generación eléctrica por tipo de fuente\n2000 - 2024",
       y = NULL,
       x = "Tasa de crecimiento equivalente anual",
       caption = "Fuente: OWiD Energy | Elaboración: @GEsteban_Gomez") +
  theme(text = element_text(family = "Rockwell"),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.caption = element_text(hjust = 0,
                                    margin = margin(t = 5),
                                    color = "gray30"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text.y = element_text(size = 10),
        strip.background = element_rect(fill = "honeydew",
                                        color = "honeydew4"),
        strip.text = element_text(size = 11),
        plot.background = element_rect(fill = "azure1"),
        panel.background = element_rect(fill = "azure",
                                        color = "honeydew4"),
        panel.grid = element_line(linetype = "dashed",
                                  color = "gray80"))


ggsave(filename = "graph.png",
       plot = graph,
       dpi = 500,
       width = 10,
       height = 6)
  


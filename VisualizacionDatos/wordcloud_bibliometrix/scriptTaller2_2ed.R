

install.packages("bibliometrix")
install.packages("wordcloud")


# limpieza del ambiente de trabajo ----------------------------------------

rm(list = ls())


# librerias ---------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(bibliometrix)
library(wordcloud)


# datos -------------------------------------------------------------------

load(file = "biblio.RData")

base_biblio <- convert2df(file = "works-2025-11-21T17-18-06.csv",
                          dbsource = "openalex",
                          format = "csv")

1151963908

base_biblio %>% glimpse()

resultados <- biblioAnalysis(M = base_biblio, sep = ";")

resumen <- summary(object = resultados, k = 20, pause = F)

resumen$AnnualProduction


biblio <- base_biblio %>% 
  select(display_name, PY, authorships.raw_author_name, LA, AB, AB_raw)

biblio <- biblio %>% 
  filter(LA == "EN" & !is.na(AB) & AB != "")

texto <- biblio %>% 
  select(AB_raw) %>% 
  pull()

texto[10]

set.seed(123)
articulos_aleatorios <- sample(x = 1:nrow(biblio), size = 500, replace = F)

biblio2 <- biblio[articulos_aleatorios, ]

resumen_token <- biblio2 %>% 
  mutate(numero_articulo = 1:nrow(biblio2)) %>% 
  unnest_tokens(input = AB_raw,
                output = word)

resumen_token %>% glimpse()

n_por_articulo <- resumen_token %>% 
  group_by(numero_articulo) %>% 
  tally(sort = T)

n_por_articulo[1:30, ]

mas_999 <- n_por_articulo %>% 
  filter(n >= 1000) %>% 
  select(numero_articulo) %>% 
  pull()

mas_999

resumen_token <- resumen_token %>% 
  filter(!numero_articulo %in% mas_999)


n_por_articulo2 <- resumen_token %>% 
  group_by(numero_articulo) %>% 
  tally(sort = T)

mean(n_por_articulo2$n)


resumen_token %>% 
  group_by(word) %>% 
  tally(sort = T)

stop_words

library(stopwords)

resumen_token_filtrado <- resumen_token %>% 
  anti_join(y = stop_words, by = "word")

resumen_token_filtrado %>% 
  group_by(word) %>% 
  tally(sort = T) %>% 
  mutate(id = 1:length(n)) %>% 
  filter(id <= 10) %>% 
  ggplot(mapping = aes(x = n, y = word)) +
  geom_col()


conteo_palabras <- resumen_token_filtrado %>% 
  group_by(word) %>% 
  tally(sort = T)

hcl.pals()



png("wordcloud.png", width=3.5, height=3.5, units="in", res=300)
wordcloud(words = conteo_palabras$word,
          freq = conteo_palabras$n,
          min.freq = 100,
          colors = rev(hcl.colors(n = 10, palette = "Temps")),
          scale = c(2 , 0.5))
dev.off()










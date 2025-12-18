## Bibliometría y Minería de Texto

En este repositorio encontraras el código y el material necesario para replicar en análisis las publicaciones sobre **'Green Industrial Policy'** extraidas de [OpenAlex.org](https://openalex.org/works?page=1&filter=title_and_abstract.search:green+industrial+policy,publication_year:2023+-+2025) entre 2023 y 2025.

El análisis se divide en dos partes:

- Bibliometría utilizando `bibliometrix`
- Minería de texto utilizando `tidytext` y `wordcloud`

La base de datos `"works-2025-11-21T17-18-06.csv"` es la información cruda sin procesar de OpenAlex.org, mientras que `"biblio.RData"` es la misma base pero con las variables pertinentes para la minería de texto.

#########################################################################
# Calendario with {calendR}
# Author: Gerardo Esteban Gómez-Santiago
# Date: 02/01/2024
# Description: This document has a few code to develope an anual calendar
#              and two-months calendar
#########################################################################

# Clean Environment
rm(list = ls())

# Libraries
library(tidyverse)
library(calendR)


# Calendario curso --------------------------------------------------------

miercoles <- c(8,15,22,29,36)
sabados <- c(11,18,25,32,39)

eventos <- rep(NA,365)

eventos[miercoles] <- "Clases Miércoles"
eventos[sabados] <- "Clases Sábado"

eventos

calendario <- calendR(start = "M",
                      from = "2025-01-01",
                      to = "2025-02-28",
        special.days = eventos[1:59],
        special.col = c("olivedrab1", "powderblue"),
        legend.pos = "bottom",
        lty = 1,
        day.size = 5,
        font.family = "mono",
        months.col = "black",
        mbg.col = "grey95",
        bg.col = "honeydew2",
        col = "black",
        months.size = 16,
        title.col = "black",
        title = "Calendario - Taller introductorio al\nanálisis estadístico en R"
        )

calendario

ggsave(filename = "calendario.png",
       plot = calendario,
       dpi = 500)



# Calendario Anual --------------------------------------------------------

start_date <- as.Date("2024-12-31")

hbd_girls <- c(as.Date("2025-12-30") - start_date, # Carolina
               as.Date("2025-07-23") - start_date, # Mama
               as.Date("2025-10-13") - start_date, # Suegra
               as.Date("2025-11-10") - start_date, # Geo
               as.Date("2025-08-28") - start_date) # Martha

hbd_boys <- c(as.Date("2025-01-10") - start_date, # Papa
              as.Date("2025-04-11") - start_date, # Juancho
              as.Date("2025-06-04") - start_date, # Migue
              as.Date("2025-08-06") - start_date, # Yo
              as.Date("2025-02-02") - start_date, # Suegro
              as.Date("2025-02-22") - start_date, # Cuñ Andres
              as.Date("2025-03-13") - start_date, # Cuñ Migue
              as.Date("2025-08-14") - start_date, # Dieguito
              as.Date("2025-08-27") - start_date, # Diego E.
              as.Date("2025-06-27") - start_date) # Brayan

tallerR <- c(8,15,22,29,36, 11,18,25,32,39)
tallerFlacso <- c(as.Date("2025-02-14") - start_date,
                  as.Date("2025-02-21") - start_date,
                  as.Date("2025-02-28") - start_date)

eventos <- rep(NA,365)

library(extrafont)

eventos[hbd_girls] <- "HBD"
eventos[hbd_boys] <- "HBD"
eventos[tallerR] <- "Taller R"
eventos[tallerFlacso] <- "Taller Flacso"

CalAnual <- calendR(start = "M",
        orientation = "p",
        special.days = eventos,
        special.col = c("olivedrab1", 
                        "powderblue",
                        "yellow2"),
        legend.pos = "bottom",
        lty = 2,
        day.size = 3,
        font.family = "mono",
        months.col = "black",
        mbg.col = "grey95",
        col = "grey",
        months.size = 7,
        title.col = "black",
        title = "Calendario 2025",
        subtitle = "Cada mañana en el África ...\n",
        bg.img = "bg_img.png",
        weeknames.size = 3,
        papersize = "A4") +
  annotate(geom = "text",
           family = "Cambria",
           x = 0.15,
           y = 0.005,
           size = 3.5,
           label = "@GEsteban_Gomez | {calendR} | 02/01/2025")

CalAnual

ggsave(filename = "calAnual.png",
       plot = CalAnual,
       dpi = 500,
       width = 9.5,
       height = 8.5
      )

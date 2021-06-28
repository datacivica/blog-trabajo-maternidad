#
# Author: OE
# Maintainer(s): SW, OE, AF
# License: (c) Data Cívica 2020, GPL v2 or newer
# --------------------------------------------------
# trabajo-no-remunerado/descriptives/src/theme-mcv.R
#

if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, ggrepel, scales, patchwork, extrafont,
               tidyquant, ggnewscale, ggalt, ggmosaic, treemapify, magick, here,
               add2ggplot)

extrafont::loadfonts(quiet=T)

Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen = 9999)

tema <-  theme_minimal() +
  theme(text = element_text(family = "Barlow Condensed", color = "grey35"),
        plot.title = element_text(size = 20, face = "bold", color = "black", hjust = 0.5),
        plot.subtitle = element_text(size = 16, face = "bold", color = "#666666", hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 10, face = "italic"),
        panel.grid = element_line(linetype = 2), 
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        strip.background = element_rect(fill="#525252"),
        strip.text = element_text(size=12, face = "bold", color = "#FAF9F6")
  )

pal_base <-  c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#ffffbf", 
               "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2", "#16F4D0")

pal_5 <-  c("#d53e4f", "#fc8d59", "#fee08b", "#99d594", "#3288bd")

pal_2 <-  c("#d53e4f", "#5e4fa2")

devices <- c("jpg", "svg")

cap <- paste0("Fuente: Elaboración propia con datos de la Encuesta Nacional de Ocupación y Empleo (ENOE), 2° trimestre 2019 - 1° trimestre 2020 y\n",
                  "la Encuesta Nacional de Ocupación y Empleo Nueva Edición (ENOE^N), 3° trimestre 2020 - 1° trimestre 2021\n")

logo <- here("import/output/dc-logo.jpg")

add_dclogo <- function(graf, escala){
  graf_con_logo <- add_logo(
    plot_path = graf,
    logo_path = logo,
    logo_position = "bottom right",
    logo_scale = escala)
  
  magick::image_write(graf_con_logo, graf)
}

# done.

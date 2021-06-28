#
# Author: Sierra Wells
# Maintainer(s): SW, AF, GJ, OE
# License: (c) Data Cívica 2020, GPL v2 or newer
# -------------------------------------------------------
# blog-maternidad/descriptives/src/descr-tnrh.R
#

if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, here, matrixStats, grid, gridExtra, stringr, shadowtext)

# ==== Files ==== #
files <- list(t1_18 = here("clean-data/output/t118.rds"),
              t2_18 = here("clean-data/output/t218.rds"),
              t3_18 = here("clean-data/output/t318.rds"),
              t4_18 = here("clean-data/output/t418.rds"),
              t1_19 = here("clean-data/output/t118.rds"),
              t2_19 = here("clean-data/output/t219.rds"),
              t3_19 = here("clean-data/output/t319.rds"),
              t4_19 = here("clean-data/output/t419.rds"),
              t1_20 = here("clean-data/output/t120.rds"),
              t3_20 = here("clean-data/output/t320.rds"),
              t4_20 = here("clean-data/output/t420.rds"),
              t1_21 = here("clean-data/output/t121.rds"),
              theme_setup = here("descriptives/src/theme.R"))

source(files$theme_setup)

#========================= Load data ===============================
enoe <- readRDS(files$t1_18) %>% 
  bind_rows(readRDS(files$t2_18),
            readRDS(files$t3_18),
            readRDS(files$t4_18),
            readRDS(files$t1_19),
            readRDS(files$t2_19),
            readRDS(files$t3_19), 
            readRDS(files$t4_19), 
            readRDS(files$t1_20), 
            readRDS(files$t3_20), 
            readRDS(files$t4_20),
            readRDS(files$t1_21)) %>%
  filter(!is.na(sex), r_def == "entrevista completa", c_res == 1 | c_res == 3) %>% 
  mutate(tnrh = rowSums(.[,cuidado_2:quehacer_7], na.rm = TRUE) 
         * NA ^ (rowSums(!is.na(.[,cuidado_2:quehacer_7])) == 0),
         hrsocup = as.numeric(hrsocup),
         n_hij = case_when(n_hij == 0 ~ "Sin hijes",
                           n_hij >= 1 ~ "Con hijes",
                           T ~ NA_character_),
         grupo_edad = case_when(eda %in% 15:24 ~ "15 a 24 años",
                                eda %in% 25:34 ~ "25 a 34 años",
                                eda %in% 35:44 ~ "35 a 44 años",
                                eda %in% 45:54 ~ "45 a 54 años",
                                eda %in% 55:64 ~ "55 a 64 años",
                                eda >= 65 ~ "65 años en adelante"),
         grupo_edad = factor(grupo_edad, levels = c("15 a 24 años",
                                                    "25 a 34 años",
                                                    "35 a 44 años",
                                                    "45 a 54 años",
                                                    "55 a 64 años",
                                                    "65 años en adelante")),
         period = factor(period, levels = c("1T 2018", "2T 2018", "3T 2018", "4T 2018",
                                            "1T 2019", "2T 2019", "3T 2019", "4T 2019", 
                                            "1T 2020", "2T 2020", "3T 2020", "4T 2020", 
                                            "1T 2021")))

enoe <- enoe %>% mutate(fac_tri = as.numeric(fac_tri),
                        econ_group = case_when(e_con == "casado o unión libre" ~ "Casada o vive con su pareja",
                                               e_con %in% c("separado o divorciado", "viudo", "soltero") ~ "Soltera"),
                        econ_group = factor(econ_group, levels = c("Soltera", "Casada o vive con su pareja", "Alguna vez casada")),
                        clase1 = case_when(clase1 == "PEA" ~ "Participan en mercado laboral",
                                           clase1 == "PNEA" ~ "No participan en mercado laboral"),
                        clase1 = factor(clase1, levels = c("No participan en mercado laboral", "Participan en mercado laboral")))


#========================= Gráficas ===============================

#### Gráfica 1: Mosaic de distribución de horas de trabajo por sexo y maternidad
tempo <- enoe %>% 
  group_by(sex, n_hij) %>% 
  summarize(tnrh = sum(tnrh, na.rm = T),
            hrsocup = sum(hrsocup, na.rm = T)) %>%
  pivot_longer(cols = tnrh:hrsocup, names_to = "trabajo", values_to = "horas") %>%
  left_join(enoe %>% group_by(sex, n_hij) %>% 
              summarize(tnrh = 100 * sum(tnrh, na.rm = T) / (sum(tnrh, na.rm = T) + sum(hrsocup, na.rm = T)),
                        hrsocup = 100 * sum(hrsocup, na.rm = T) / (sum(tnrh, na.rm = T) + sum(hrsocup, na.rm = T))) %>%
              pivot_longer(cols = tnrh:hrsocup, names_to = "trabajo", values_to = "percent")) %>% 
  filter(!(sex == "mujer" & is.na(n_hij))) %>% 
  mutate(group = case_when(sex == "hombre" ~ "Hombres",
                           sex == "mujer" & n_hij == "Con hijes" ~ "Mujeres con hijes",
                           sex == "mujer" & n_hij == "Sin hijes" ~ "Mujeres sin hijes"),
         trabajo = case_when(trabajo == "tnrh" ~ "Trabajo del hogar",
                             trabajo == "hrsocup" ~ "Trabajo remunerado")) 

porc_hombres <- sum(subset(tempo, sex == "hombre")$horas)/sum(tempo$horas)
porc_mujeres <- sum(subset(tempo, sex == "mujer")$horas)/sum(tempo$horas)
porc_madres <- sum(subset(tempo, group == "Mujeres con hijes")$horas)/sum(tempo$horas)
porc_muj_sin_hij <- sum(subset(tempo, group == "Mujeres sin hijes")$horas)/sum(tempo$horas) 

ggplot(data = tempo) +
  geom_mosaic(aes(weight = horas, x = product(group), fill = trabajo)) +
  geom_rect(xmin = 0, xmax = porc_hombres - 0.009, 
            ymin = 0, ymax = 1, fill = NA, color = "black", size = 1) +
  geom_rect(xmin = 1 - porc_mujeres, xmax = 1, 
            ymin = 0, ymax = 1, fill = NA, color = "black", size = 1) + 
  geom_text(aes(x = (1 - porc_mujeres/2), y = 1.05, family = "Barlow Condensed", fontface = "bold",
                label = paste0("Mujeres (", round(100* porc_mujeres,1), "%)")))+
  geom_text(aes(x = porc_hombres/2, y = 1.05, family = "Barlow Condensed", fontface = "bold",
                label = paste0("Hombres (", round(100*porc_hombres, 1), "%)")))+
  geom_text(aes(x = 1- porc_muj_sin_hij/2, y = -0.05, family = "Barlow Condensed",
                label = paste0("Sin hijes\n (", round(100*porc_muj_sin_hij,1), "%)")), size = 3.5)+
  geom_text(aes(x = porc_hombres + porc_madres/2, y = -0.05, family = "Barlow Condensed",
                label = paste0("Con hijes\n (", round(100*porc_madres,1), "%)")), size = 3.5)+
  geom_text(aes(x = porc_hombres/2, y = -0.05, family = "Barlow Condensed",
                label = paste0("Hombres\n(", round(100*porc_hombres, 1), "%)")), size = 3.5)+
  labs(title = "¿Cómo se distribuyen las horas de trabajo en México?", fill = "", x = "", y = "",
       caption = cap) +
  tema + 
  scale_fill_manual(values = pal_2, guide=guide_legend(reverse=T)) + 
  theme(legend.position = "right",
        axis.text = element_blank())

walk(devices, ~ ggsave(filename = paste0(here("descriptives/output/1-distrib-total."), .x),
                       device = .x, width = 15, height = 10))
walk(devices, ~ add_dclogo(graf = paste0(here("descriptives/output/1-distrib-total."), .x),
                           escala = 7))

#### Gráficas 2: butterflies de doble carga, desagregado por:
# PEA vs. PNEA

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

tempo <- enoe %>% 
  group_by(n_hij, clase1) %>% 
  summarize(tnrh = weighted.mean(tnrh, fac_tri, na.rm = T),
            hrsocup = weighted.mean(hrsocup, fac_tri, na.rm = T),
            total = tnrh + hrsocup) %>% 
  filter(!is.na(n_hij) & !is.na(clase1)) %>%
  pivot_longer(cols = tnrh:total, values_to = "horas", names_to = "tipo") %>% 
  mutate(tipo = case_when(tipo == "hrsocup" ~ "Trabajo remunerado",
                          tipo == "tnrh" ~ "Trabajo del hogar",
                          tipo == "total" ~ "Total"),
         tipo = factor(tipo, levels = c("Trabajo remunerado", "Trabajo del hogar", "Total")),
         across(.cols = clase1, fct_rev))

# Sin hijos (izquierda) 
g1 <- ggplot(tempo %>% filter(n_hij == "Sin hijes", tipo != "Total"), 
             aes(x = horas, y = clase1)) +
  geom_col(aes(fill = tipo)) +
  geom_text(data = tempo %>% filter(horas != 0, n_hij == "Sin hijes", tipo != "Total"), 
            aes(label = round(horas, 1), group = tipo), 
            position=position_stack(vjust = 0.5), stat="identity", color = "white", fontface = "bold", alpha = 0.75) + 
  geom_text(data = tempo %>% filter(n_hij == "Sin hijes", tipo == "Total"),
            aes(label = round(horas, 1)), 
            color = "black", fontface = "bold", hjust = 1.3) + 
  scale_x_reverse(limits = c(65,0)) +
  labs(caption = cap,
       y = "", x = "Horas semanales", subtitle = "Sin hijes", fill = "") + 
  tema + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        plot.margin = unit(c(0,0,0,0), "mm")) + 
  scale_fill_manual(values = pal_2, guide=guide_legend(reverse=T))

# Con hijos (derecha)
g2 <- ggplot(tempo %>% filter(n_hij == "Con hijes", tipo != "Total"), 
             aes(x = horas, y = clase1)) +
  geom_col(aes(fill = tipo)) +
  geom_text(data = tempo %>% filter(horas != 0, n_hij == "Con hijes", tipo != "Total"), 
            aes(label = round(horas, 1), group = tipo), 
            position=position_stack(vjust = 0.5), stat="identity", color = "white", fontface = "bold", alpha = 0.75) + 
  geom_text(data = tempo %>% filter(n_hij == "Con hijes", tipo == "Total"),
            aes(label = round(horas, 1)), 
            color = "black", fontface = "bold", hjust = -0.3) + 
  xlim(c(0,66)) +
  labs(x = "Horas semanales", y = "", caption = " \n ", subtitle = "Con hijes") +
  tema + 
  theme(axis.text.x = element_blank(), legend.position = "none", axis.text.y = element_blank(),
        plot.margin = unit(c(0,0,0,0), "mm")) + 
  scale_fill_manual(values = pal_2, guide=guide_legend(reverse=T))

#Etiquetas
labels <- ggplot(tempo %>% filter(n_hij == "Con hijes"), aes(x = 0, y = clase1)) +
  geom_col(fill = alpha('white',0)) +
  geom_text(aes(label = str_wrap(clase1, 12)), family = "Barlow Condensed", 
            color = "grey35", size = 4.5, fontface = "bold") +
  tema +
  theme(panel.grid = element_blank(), axis.text = element_blank()) +
  labs(caption = "     ",  y = "", x = "")

# Leyenda
legend <- g_legend(g1)

# Título
titulo <- ggplot()+
  labs(title = "Tiempo promedio dedicado semanalmente por las mujeres al trabajo total",
       subtitle = paste0("Desagregado por maternidad y participación en el mercado laboral"))+
  tema

# Juntos
g3 <- grid.arrange(titulo, legend, arrangeGrob(g1 + theme(legend.position="none"),labels, g2,nrow=1,widths=c(10,3,10)),
                   nrow=3,heights=c(1, 0.5, 10))

walk(devices, ~ ggsave(filename = paste0(here("descriptives/output/3-total-clase1."), .x),
                         plot = g3, device = .x, width = 15, height = 10))
walk(devices, ~ add_dclogo(graf = paste0(here("descriptives/output/3-total-clase1."), .x),
                           escala = 7))


#### Gráfica 3: heat map de trabajo (TNRH, remunerado y total) por edad y estado conyugal

enoe %>% 
  group_by(n_hij, grupo_edad, econ_group) %>% 
  summarize(tnrh = weighted.mean(tnrh, fac_tri, na.rm = T),
            hrsocup = weighted.mean(hrsocup, fac_tri, na.rm = T),
            total = tnrh + hrsocup) %>% 
  filter(!is.na(n_hij) & !is.na(econ_group) & !is.na(grupo_edad)) %>%
  pivot_longer(cols = tnrh:total, values_to = "horas", names_to = "tipo") %>% 
  mutate(tipo = case_when(tipo == "hrsocup" ~ "Trabajo remunerado",
                          tipo == "tnrh" ~ "Trabajo del hogar",
                          tipo == "total" ~ "Total"),
         tipo = factor(tipo, levels = c("Trabajo del hogar", "Trabajo remunerado", "Total")),
         grupo_edad = case_when(grupo_edad == "15 a 24 años" ~ "15 - 24",
                                grupo_edad == "25 a 34 años" ~ "25 - 34",
                                grupo_edad == "35 a 44 años" ~ "35 - 44",
                                grupo_edad == "45 a 54 años" ~ "45 - 54",
                                grupo_edad == "55 a 64 años" ~ "55 - 64",
                                grupo_edad == "65 años en adelante" ~ "65+")) %>% 
  {
    ggplot(data = ., aes(x = n_hij, y = fct_rev(grupo_edad)))+
      geom_tile(aes(fill = horas)) +
      geom_text(aes(label = round(horas, 0)), color = "white", fontface = "bold")+
      facet_grid(tipo~econ_group)+
      scale_fill_continuous(low = "#C1BBDD", high = "#38305F")+ 
      labs(title = "Promedio de horas dedicadas semanalmente por las mujeres al trabajo",
           subtitle = "Desagregado por maternidad, estado conyugal y grupo de edad",
           caption = cap,
           y = "Grupo de edad (en años)", x = "")+
      tema +
      theme(legend.position = "none")
  }

walk(devices, ~ ggsave(filename = paste0(here("descriptives/output/4-heat-trabajo-edad-econ."), .x),
                       device = .x, width = 10, height = 10))
walk(devices, ~ add_dclogo(graf = paste0(here("descriptives/output/4-heat-trabajo-edad-econ."), .x),
                           escala = 7))

# done

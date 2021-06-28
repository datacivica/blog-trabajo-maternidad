#
# Author: Alicia Franco
# Maintainer(s): AF, SW, GJ
# License: (c) Data Cívica 2020, GPL v2 or newer
# -----------------------------------------------------------
# blog-maternidad/clean-data/src/merc-lab-descriptives.R

#### Paquetes ####

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here)

source(paths$tema)

#### Archivero ####

paths <- list(input = here("clean-data/output/"),
              tema = here("descriptives/src/theme.R"),
              plot_1 = here("descriptives/output/a-ingreso-boxplot."),
              plot_2 = here("descriptives/output/b-pnea-lollip."),
              plot_a = here("descriptives/output/rel-estcony-edades/pnea."),
              plot_b = here("descriptives/output/rel-estcony-edades/ingreso."),
              plot_c = here("descriptives/output/rel-estcony-edades/subocupacion."))

files <- dir(paths$input)

#### Crear datos ####

# toplot <- data.frame()
# 
# pb <- txtProgressBar(min = 0, max = length(files), style = 3)
# 
# for(i in seq_along(files)){
#   tempo <- readRDS(paste0(paths$input,files[i])) %>%
#     filter(r_def == "entrevista completa", c_res == 1 | c_res == 3) %>%  # Entrevistas que cumplen
#     mutate(fac_tri  = as.numeric(fac_tri),
#            sex = ifelse(sex == "hombre", "Hombres", "Mujeres"),
#            clase2 = case_when(clase2 == "formal" ~ "Empleo formal",
#                               clase2 == "informal" ~ "Empleo informal",
#                               clase2 == "desocupado"~ "Desempleo",
#                               T ~ clase2),
#            n_hij = case_when(n_hij == 0 ~ "Mujeres sin hijes",
#                              n_hij >= 1 ~ "Mujeres con hijes",
#                              T ~ NA_character_),
#            grupo_edad = case_when(eda %in% 15:24 ~ "15-24 años",
#                                   eda %in% 25:34 ~ "25-34 años",
#                                   eda %in% 35:44 ~ "35-44 años",
#                                   eda %in% 45:54 ~ "45-54 años",
#                                   eda %in% 55:64 ~ "55-64 años",
#                                   eda >= 65 ~ "65 años o más"),
#            grupo_edad = factor(grupo_edad, levels = c("15-24 años",
#                                                       "25-34 años",
#                                                       "35-44 años",
#                                                       "45-54 años",
#                                                       "55-64 años",
#                                                       "65 años o más")),
#            period = factor(period, levels = c("1T 2018", "2T 2018", "3T 2018", "4T 2018",
#                                               "1T 2019", "2T 2019", "3T 2019", "4T 2019",
#                                               "1T 2020", "2T 2020", "3T 2020", "4T 2020",
#                                               "1T 2021")),
#            ingreso = ifelse(str_detect(clase2, "ocupado") == F | (is.na(p6b2) & (p6_9 == 9 | p6a3==3)), 0, p6b2),
#            ingreso = case_when(is.na(p6b2) & org_p6c == 1 ~ .5 * salario,
#                                is.na(p6b2) & org_p6c == 2 ~ 1 * salario,
#                                is.na(p6b2) & org_p6c == 3 ~ 1.5 * salario,
#                                is.na(p6b2) & org_p6c == 4 ~ 2.5 * salario,
#                                is.na(p6b2) & org_p6c == 5 ~ 4 * salario,
#                                is.na(p6b2) & org_p6c == 6 ~ 7.5 * salario,
#                                is.na(p6b2) & org_p6c == 7 ~ 10 * salario,
#                                TRUE ~ ingreso),
#            econ_group = case_when(e_con == "casado o unión libre" ~ " casada o vive con su pareja",
#                               e_con %in% c("separado o divorciado", "viudo", "soltero") ~ " soltera",
#                               T ~ NA_character_),
#            clasif = ifelse(!is.na(econ_group) &!is.na(n_hij), paste(n_hij, econ_group, sep = ","), NA_character_),
#            clasif = ifelse(sex == "Hombres", sex, clasif),
#            n_hij = ifelse(is.na(n_hij) & sex == "Hombres", sex, n_hij)) %>%
#     group_by(sex, grupo_edad, e_con, econ_group, n_hij, ingreso, clase2, fac_tri, cs_p13_1,
#            medica5c, org_p6c, p6c, sub_o, s_clasifi, dur_est, t_tra, clasif, period) %>%
#     summarise(fac_tri = sum(fac_tri)) %>%
#     ungroup()
# 
#   toplot <- rbind(toplot, tempo)
# 
#   rm(tempo)
# 
#   setTxtProgressBar(pb, i)
# }
# 
# close(pb)
# rm(pb,i)
# 
# saveRDS(object = toplot, file = here("descriptives/output/toplot.rds"))

#### Abrir datos ####

toplot <- readRDS(here("descriptives/output/toplot.rds"))%>% 
  mutate(econ_group = str_to_sentence(econ_group),
         econ_group = str_trim(econ_group, side = "left"))


#### Objetos ####

filtro_edad <- c("25-34 años", "35-44 años", "45-54 años")

#### Gráficas ####


# I) Distribución ingreso por cada categoría (boxplot)

# Box plot con imputación
ggplot(data = toplot %>% 
         filter(clase2 == "ocupado", org_p6c %in% (1:9), !(is.na(clasif)), !(is.na(econ_group)),
                grupo_edad %in% filtro_edad) %>% 
         mutate(n_hij = factor(n_hij, levels = c("Mujeres con hijes", "Mujeres sin hijes", "Hombres"))), 
       aes(y = ingreso, x = econ_group, weigth = fac_tri, color = n_hij, fill = n_hij)) +
  geom_boxplot(alpha = 0.7, coef = 1, fatten = 2, outlier.shape = NA, 
               outlier.size = NA) +
  coord_flip(ylim = c(0, 15000)) +
  scale_fill_manual(values = c(pal_5[1:2], pal_5[4])) +
  scale_color_manual(values = c(pal_5[1:2], pal_5[4])) +
  labs(title = "¿Cómo afecta el ingreso mensual* ser mujer, con hijes, casada? **",
       subtitle = "Personas ocupadas entre 25 y 54 años",
       caption = paste0(cap, "\n* Ingreso imputado \n ** Se excluyen las personas que no reportan su salario"), 
       y = "Ingreso mensual",
       x = "") +
  stat_summary(fun = mean, geom = "point", color = pal_5[5],
               na.rm = T, show.legend = F,
               position = position_dodge(0.75)) +
  theme_linedraw() +
  tema +
  theme(legend.title = element_blank()) #Checar error de los non-finite values

walk(devices, ~ ggsave(filename = paste0(paths$plot_1, .x),
                       device = .x, width = 10, height = 8))
walk(devices, ~ add_dclogo(graf = paste0(paths$plot_1, .x),
                           escala = 7))


# II) Participación en el mercado laboral lollipops

tempo <- toplot %>% 
  filter(!(is.na(econ_group)),!(is.na(n_hij)), grupo_edad %in% filtro_edad) %>% 
  group_by(econ_group, n_hij) %>% 
  mutate(tot = sum(fac_tri)) %>% 
  ungroup() %>% 
  mutate(clase2 = ifelse(str_detect(clase2, "disponible"), "PNEA", clase2)) %>% 
  group_by(econ_group, n_hij, clase2) %>% 
  summarise(porc = round(1 - (sum(fac_tri)/tot),3) * 100) %>% 
  ungroup() %>% 
  distinct() %>% 
  mutate(n_hij = factor(n_hij, levels = c("Mujeres con hijes", "Mujeres sin hijes", "Hombres"))) %>% 
  distinct() %>% 
  filter(clase2 == "PNEA")

ggplot(tempo %>% filter(clase2 == "PNEA"), aes(y = porc, fill = n_hij, color = n_hij)) +
  geom_linerange(aes(x = econ_group, ymin = 0, ymax = porc), position = position_dodge(width = 0.6)) +
  geom_point(aes(x = econ_group,y = porc), size = 9, alpha = 0.7, shape = 21, stroke = 2, position = position_dodge(width = 0.6)) +
  coord_flip() +
  scale_fill_manual(values = c(pal_5[1:2], pal_5[4])) +
  scale_color_manual(values = c(pal_5[1:2], pal_5[4])) +
  labs(y = "Porcentaje",
       x = "",
       caption = cap,
       title = "Participación en el mercado laboral",
       subtitle = "Personas entre 25 y 54 años") +
  geom_text(aes(x = econ_group, label = paste0(porc,"%"), y = porc), vjust = 0.4, color = "white", size = 3, 
            fontface = "bold", family = "Barlow Condensed", position = position_dodge(width = 0.6)) +
  tema +
  theme(legend.title = element_blank())
  

walk(devices, ~ ggsave(filename = paste0(paths$plot_2, .x),
                       device = .x, width = 10, height = 8))
walk(devices, ~ add_dclogo(graf = paste0(paths$plot_2, .x),
                           escala = 7))


# DONE


 

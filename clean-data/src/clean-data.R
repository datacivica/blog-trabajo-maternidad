#
# Author: SW
# Maintainer(s): SW, AF, GJ
# License: (c) Data Cívica 2020, GPL v2 or newer
# -----------------------------------------------------------
# blog-maternidad/clean-data/src/clean-data.R

if(!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, stringr, here, purrr, janitor, readxl)

#### Archivero ####

paths <- list(input = here("import/output"),
              output = here("clean-data/output"))

#### Definir archivos COE2 ####

coe2 <- dir(paths$input, pattern = "coe2")

#### Definir función para juntar y limpiar tablas ####
clean <- function(x){
  juntas <- readRDS(paste0(paths$input, "/", x)) %>% 
    left_join(readRDS(paste0(paths$input, "/sdem", 
                             str_sub(x, 5, 8), ".rds")) %>% 
                filter(eda >= 15 &
                         eda != 99)) %>% 
    mutate(across(everything(), as.character),
           period = case_when(period == "t118" ~ "1T 2018",
                              period == "t218" ~ "2T 2018",
                              period == "t318" ~ "3T 2018",
                              period == "t418" ~ "4T 2018",
                              period == "t119" ~ "1T 2019",
                              period == "t219" ~ "2T 2019",
                              period == "t319" ~ "3T 2019",
                              period == "t419" ~ "4T 2019",
                              period == "t120" ~ "1T 2020",
                              period == "t320" ~ "3T 2020",
                              period == "t420" ~ "4T 2020",
                              period == "t121" ~ "1T 2021"
                              ),
           cuidado_2 = NA_real_,
           compras_3 = NA_real_,
           llevar_4 = NA_real_,
           construir_5 = NA_real_,
           reparar_6 = NA_real_,
           quehacer_7 = NA_real_,
           cuidado_2_m = NA_real_,
           compras_3_m = NA_real_,
           llevar_4_m = NA_real_,
           construir_5_m = NA_real_,
           reparar_6_m = NA_real_,
           quehacer_7_m = NA_real_)
 
  if("p9_h2" %in% colnames(juntas)){
    if(as.integer(str_sub(juntas[1, "period"],-4,-1)) >= 2016){
    juntas <- juntas %>% 
      mutate(cuidado_2 = as.numeric(p9_h2),
             compras_3 = as.numeric(p9_h3),
             llevar_4 = as.numeric(p9_h4),
             construir_5 = as.numeric(p9_h5),
             reparar_6 = as.numeric(p9_h6),
             quehacer_7 = as.numeric(p9_h7))
    }
    if(as.integer(str_sub(juntas[1, "period"],-4,-1)) %in% 2007:2012){
      juntas <- juntas %>% 
        mutate(cuidado_2 = as.numeric(p9_h2),
               construir_5 = as.numeric(p9_h3),
               reparar_6 = as.numeric(p9_h4),
               quehacer_7 = as.numeric(p9_h5))
    }
  } 
    if("p11_h2" %in% colnames(juntas)){
      if(as.integer(str_sub(juntas[1, "period"],-4,-1)) >= 2016){
      juntas <- juntas %>% 
        mutate(cuidado_2 = as.numeric(p11_h2),
               compras_3 = as.numeric(p11_h3),
               llevar_4 = as.numeric(p11_h4),
               construir_5 = as.numeric(p11_h5),
               reparar_6 = as.numeric(p11_h6),
               quehacer_7 = as.numeric(p11_h7))}
      if(as.integer(str_sub(juntas[1, "period"],-4,-1)) %in% 2007:2009){
        juntas <- juntas %>% 
          mutate(cuidado_2 = as.numeric(p11_h2),
                 construir_5 = as.numeric(p11_h3),
                 reparar_6 = as.numeric(p11_h4),
                 quehacer_7 = as.numeric(p11_h5))
      }
    } 
  
  juntas <- juntas %>% 
    select(-c(starts_with("p11_h"), starts_with("p9_h")))
  
  if("p9_m2" %in% colnames(juntas)){
    if(as.integer(str_sub(juntas[1, "period"],-4,-1)) >= 2016){
      juntas <- juntas %>% 
        mutate(cuidado_2_m = as.numeric(p9_m2)/60,
               compras_3_m = as.numeric(p9_m3)/60,
               llevar_4_m = as.numeric(p9_m4)/60,
               construir_5_m = as.numeric(p9_m5)/60,
               reparar_6_m = as.numeric(p9_m6)/60,
               quehacer_7_m = as.numeric(p9_m7)/60)
    }
    if(as.integer(str_sub(juntas[1, "period"],-4,-1)) %in% 2007:2012){
      juntas <- juntas %>% 
        mutate(cuidado_2_m = as.numeric(p9_m2)/60,
               construir_5_m = as.numeric(p9_m3)/60,
               reparar_6_m = as.numeric(p9_m4)/60,
               quehacer_7_m = as.numeric(p9_m5)/60)
    }
  } 
    if("p11_m2" %in% colnames(juntas)){
      if(as.integer(str_sub(juntas[1, "period"],-4,-1)) >= 2016){
        juntas <- juntas %>% 
          mutate(cuidado_2_m = as.numeric(p11_m2)/60,
                 compras_3_m = as.numeric(p11_m3)/60,
                 llevar_4_m = as.numeric(p11_m4)/60,
                 construir_5_m = as.numeric(p11_m5)/60,
                 reparar_6_m = as.numeric(p11_m6)/60,
                 quehacer_7_m = as.numeric(p11_m7)/60)}
      if(as.integer(str_sub(juntas[1, "period"],-4,-1)) %in% 2007:2009){
        juntas <- juntas %>% 
          mutate(cuidado_2_m = as.numeric(p11_m2)/60,
                 construir_5_m = as.numeric(p11_m3)/60,
                 reparar_6_m = as.numeric(p11_m4)/60,
                 quehacer_7_m = as.numeric(p11_m5)/60)
      }
    }
  
  juntas <- juntas %>% 
    select(-c(starts_with("p11_m"), starts_with("p9_m"))) %>% 
    mutate(cuidado_2 = case_when(cuidado_2 %in% 0:97 ~ cuidado_2,
                                 T ~ NA_real_),
           compras_3 = case_when(compras_3 %in% 0:97 ~ compras_3,
                                 T ~ NA_real_),
           llevar_4 = case_when(llevar_4 %in% 0:97 ~ llevar_4,
                                T ~ NA_real_),
           construir_5 = case_when(construir_5 %in% 0:97 ~ construir_5,
                                   T ~ NA_real_),
           reparar_6 = case_when(reparar_6 %in% 0:97 ~ reparar_6,
                                 T ~ NA_real_),
           quehacer_7 = case_when(quehacer_7 %in% 0:97 ~ quehacer_7,
                                  T ~ NA_real_),
           cuidado_2 = case_when(!is.na(cuidado_2) & !is.na(cuidado_2_m) ~ cuidado_2 + cuidado_2_m,
                                 !is.na(cuidado_2) & is.na(cuidado_2_m) ~ cuidado_2,
                                 is.na(cuidado_2) & !is.na(cuidado_2_m) ~ cuidado_2_m),
           compras_3 = case_when(!is.na(compras_3) & !is.na(compras_3_m) ~ compras_3 + compras_3_m,
                                 !is.na(compras_3) & is.na(compras_3_m) ~ compras_3,
                                 is.na(compras_3) & !is.na(compras_3_m) ~ compras_3_m),
           llevar_4 = case_when(!is.na(llevar_4) & !is.na(llevar_4_m) ~ llevar_4 + llevar_4_m,
                                !is.na(llevar_4) & is.na(llevar_4_m) ~ llevar_4,
                                is.na(llevar_4) & !is.na(llevar_4_m) ~ llevar_4_m),
           construir_5 = case_when(!is.na(construir_5) & !is.na(construir_5_m) ~ construir_5 + construir_5_m,
                                   !is.na(construir_5) & is.na(construir_5_m) ~ construir_5,
                                   is.na(construir_5) & !is.na(construir_5_m) ~ construir_5_m),
           reparar_6 = case_when(!is.na(reparar_6) & !is.na(reparar_6_m) ~ reparar_6 + reparar_6_m,
                                 !is.na(reparar_6) & is.na(reparar_6_m) ~ reparar_6,
                                 is.na(reparar_6) & !is.na(reparar_6_m) ~ reparar_6_m),
           quehacer_7 = case_when(!is.na(quehacer_7) & !is.na(quehacer_7_m) ~ quehacer_7 + quehacer_7_m,
                                  !is.na(quehacer_7) & is.na(quehacer_7_m) ~ quehacer_7,
                                  is.na(quehacer_7) & !is.na(quehacer_7_m) ~ quehacer_7_m)) %>% 
    select(-c(ends_with("_m"))) %>% 
    mutate(eda = as.integer(eda),
           eda = case_when(eda != 98 ~ eda,
                           T ~ NA_integer_),
           sex = case_when(sex == "1" ~ "hombre",
                           sex == "2" ~ "mujer",
                           T ~ NA_character_),
           e_con = case_when(e_con %in% c("1", "5") ~ "casado o unión libre",
                             e_con %in% c("2", "3")~ "separado o divorciado",
                             e_con == "4" ~ "viudo",
                             e_con == "6" ~ "soltero",
                             T ~ NA_character_),
           n_hij = as.integer(n_hij),
           n_hij = case_when(n_hij %in% 0:25 ~ n_hij,
                             T ~ NA_integer_),
           emp_ppal = case_when(emp_ppal == "1" ~ "informal",
                                emp_ppal == "2" ~ "formal",
                                T ~ NA_character_),
           ing7c = case_when(ing7c == 1 ~ "hasta un s.m",
                             ing7c == 2 ~ "1 - 2 s.m.",
                             ing7c == 3 ~ "2 - 3 s.m.",
                             ing7c == 4 ~ "3 - 5 s.m.",
                             ing7c == 5 ~ "más de 5 s.m.",
                             ing7c == 6 ~ "no recibe ingresos",
                             ing7c == 7 ~ "no especificado",
                             T ~ NA_character_),
           salario = as.numeric(salario),
           clase1 = case_when(clase1 == "1" ~ "PEA",
                              clase1 == "2" ~ "PNEA",
                              T ~ NA_character_),
           clase2 = case_when(clase2 == "1" ~ "ocupado",
                              clase2 == "2" ~ "desocupado",
                              clase2 == "3" ~ "disponible",
                              clase2 == "4" ~ "no disponible",
                              T ~ NA_character_),
           rama = case_when(rama == 6 ~ "agropecuario",
                            rama == 1 ~ "construcción",
                            rama == 2 ~ "industria manufacturera",
                            rama == 3 ~ "comercio",
                            rama == 4 ~ "servicio",
                            rama == 5 ~ "otros",
                            rama == 7 ~ "no especificado",
                            T ~ NA_character_),
           org_p6c = p6c,
           p6c = case_when(p6c == "1" | p6c == "2" ~ "menor o igual a un s.m.",
                           p6c == "3" ~ "1 - 2 s.m.",
                           p6c == "4" ~ "2 - 3 s.m.",
                           p6c == "5" ~ "3 - 5 s.m.",
                           p6c == "6" ~ "5 - 10 s.m.",
                           p6c == "7" ~ "más de 10 s.m.",
                           T ~ NA_character_),
           p6b1 = case_when(p6b1 == "1" ~ "cada mes",
                            p6b1 == "2" ~ "cada 15 días",
                            p6b1 == "3" ~ "cada semana",
                            p6b1 == "4" ~ "diario",
                            p6b1 == "6" ~ "por pieza o servicio",
                            p6b1 == "5" | p6b1 == "7" | p6b1 == "8"
                            ~ "otro",
                            T ~ NA_character_),
           p6b2 = as.integer(p6b2),
           p6b2 = case_when(p6b2 %in% 1:999998 ~ p6b2,
                            T ~ NA_integer_),
           r_def = case_when(as.integer(r_def) == 0 ~ "entrevista completa",
                             as.integer(r_def) != 0 & !is.na(r_def) ~ "entrevista no completa",
                             T ~ NA_character_),
           ent = as.numeric(ent),
           cs_p13_1 = case_when(as.integer(cs_p13_1) == 1 ~ "preescolar",
                                as.integer(cs_p13_1) == 2 ~ "primaria",
                                as.integer(cs_p13_1) == 3 ~ "secundaria",
                                as.integer(cs_p13_1) == 4 ~ "preparatoria",
                                as.integer(cs_p13_1) == 5 | cs_p13_1 == 6 ~ "carrera técnica",
                                as.integer(cs_p13_1) == 7 | cs_p13_1 == 8 | cs_p13_1 == 9
                                ~ "licenciatura o más",
                                T ~ NA_character_),
           cs_p13_1 = factor(cs_p13_1, levels = c("preescolar", "primaria", "secundaria", "preparatoria",
                                                  "carrera técnica", "licenciatura o más")),
           medica5c = case_when(as.numeric(medica5c) == 1 ~ "Sin prestaciones",
                                as.numeric(medica5c) == 2 ~ "Solo acceso a inst. salud",
                                as.numeric(medica5c) == 3 ~ "Acceso a inst. salud y otras",
                                as.numeric(medica5c) == 4 ~ "Sin acceso a inst. salud pero sí a otras",
                                as.numeric(medica5c) == 5 ~ "No especificado",
                                T ~ NA_character_), 
           s_clasifi = case_when(as.numeric(s_clasifi) == 1 ~ "Pers. c/ nexo labo. p/ afec. p/ paro técnico",
                                 as.numeric(s_clasifi) == 2 ~ "Pers. afecta. p/ una caída
en el ritmo de su activi.",
                                 as.numeric(s_clasifi) == 3 ~ "Pers. q/laboran - de 35 h.
p/ razones de mercados",
                                 as.numeric(s_clasifi) == 4 ~ "Pers. q/laboran + de 35 h.
p/ - de lo habitual p/razones de mercado",
                                 as.numeric(s_clasifi) == 5 ~ "Pers. q/ buscan aumen. su
jor. lab. p/ obtener + ing.",
                                 as.numeric(s_clasifi) == 6 ~ "Ausentes con retorno en la semana de la entrevista",
                                 T ~ NA_character_), 
           dur_est = case_when(as.numeric(dur_est) == 1 ~ "Ausentes temporales con vínculo laboral",
                               as.numeric(dur_est) == 2 ~ "Menos de 15 hrs.",
                               as.numeric(dur_est) == 3 ~ "15-34 hrs.",
                               as.numeric(dur_est) == 4 ~ "35-48 hrs",
                               as.numeric(dur_est) == 5 ~ "Más de 48 horas",
                               as.numeric(dur_est) == 6 ~ "No especificado",
                               T ~ NA_character_), 
           period = factor(period, levels = c("1T 2018", "2T 2018", "3T 2018", "1T 2019", 
                                              "2T 2019", "3T 2019", "4T 2019", 
                                              "1T 2020", "2T 2020", "3T 2020", "4T 2020", "1T 2021")))
  
  if ("t_loc" %in% colnames(juntas)){
    juntas <- juntas %>% 
      mutate(t_loc = case_when(t_loc == "1" ~ "más de 100 mil habitantes",
                               t_loc == "2" ~ "15 - 99 mil habitantes",
                               t_loc == "3" ~ "2,500 - 14,999 habitantes",
                               t_loc == "4" ~ "menos de 2,500 habitantes",
                               T ~ NA_character_))
  }  
  
  if ("t_loc_tri" %in% colnames(juntas)){
    juntas <- juntas %>% 
      mutate(t_loc_tri = case_when(t_loc_tri == "1" ~ "más de 100 mil habitantes",
                                   t_loc_tri == "2" ~ "15 - 99 mil habitantes",
                                   t_loc_tri == "3" ~ "2,500 - 14,999 habitantes",
                                   t_loc_tri == "4" ~ "menos de 2,500 habitantes",
                                   T ~ NA_character_))
  } 
  
  if ("t_loc_men" %in% colnames(juntas)){
    juntas <- juntas %>% 
      mutate(t_loc_men = case_when(t_loc_men == "1" ~ "más de 100 mil habitantes",
                                   t_loc_men == "2" ~ "15 - 99 mil habitantes",
                                   t_loc_men == "3" ~ "2,500 - 14,999 habitantes",
                                   t_loc_men == "4" ~ "menos de 2,500 habitantes",
                                   T ~ NA_character_))
  } 
  
  if("fac" %in% colnames(juntas)){
    juntas <- juntas %>% 
      rename(fac_tri = fac)
  }
  
  saveRDS(juntas, paste0(paths$output, "/", str_sub(x, 5, 8), ".rds"))
  
  rm(juntas)
}

#### Walk ####
walk(coe2, clean)

#done

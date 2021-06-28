#
# Author: SW
# Maintainer(s): SW, AF, GJ
# License:  Data Cívica 2021 © GPL V2 or newer
# ---------------------------------------------
# blog-maternidad/import/src/import.R 

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, R.utils, janitor, data.table, foreign, here)

paths <- list(input = here("import/input"),
              output = here("import/output")
)

# ---- ENOE ---- #
keep_vars_sdem <- c("r_def", "c_res", "est", "est_d", "t_loc", "cd_a", "ent", 
                    "con", "upm", "v_sel", "n_hog", "h_mud", "n_ren", "n_ent",
                    "tipo", "mes_cal", "ca", 
                    "sex", "eda", "e_con", "n_hij", "emp_ppal", 
                    "ingocup", "ing7c", "salario", "rama", "hrsocup", 
                    "clase1", "clase2", "fac", "fac_tri", "fac_men", 
                    "t_loc_tri", "t_loc_men", "cs_p13_1",
                    "medica5c", "sub_o", "s_clasifi", "dur_est", "t_tra") # nuevas

keep_vars_coe2 <- c("r_def", "c_res", "est", "est_d", "t_loc", "cd_a", "ent", 
                    "con", "upm", "v_sel", "n_hog", "h_mud", "n_ren", "n_ent",
                    "tipo", "mes_cal", "ca",
                    "p9_h2", "p9_h3", "p9_h4", "p9_h5", "p9_h6", "p9_h7",
                    "p11_h2", "p11_h3", "p11_h4", "p11_h5", "p11_h6", "p11_h7",
                    "p6c", "p6b1", "p6b2", "p6_9","p6a3","fac", "fac_tri", "fac_men", 
                    "p9_m2", "p9_m3", "p9_m4", "p9_m5", "p9_m6", "p9_m7",
                    "p11_m2", "p11_m3", "p11_m4", "p11_m5", "p11_m6", "p11_m7")

## Sociodemográficos
sdem_files <- dir(paths$input, pattern = "sdem")

import_sdem <- function(x){
  sdem <- fread(paste0(paths$input, "/", x)) %>% 
    clean_names() %>% 
    select(one_of(keep_vars_sdem)) %>% 
    mutate(period = str_sub(x, 5, 8)) %>% 
    saveRDS(paste0(paths$output, "/", str_replace(x, str_sub(x, 9, nchar(x)), ".rds")))
}

walk(sdem_files, import_sdem)

## COE2
coe2_files <- dir(paths$input, pattern = "coe2")

import_coe2 <- function(x){
  sdem <- fread(paste0(paths$input, "/", x)) %>% 
    clean_names() %>% 
    select(one_of(keep_vars_coe2)) %>% 
    mutate(period = str_sub(x, 5, 8)) %>% 
    saveRDS(paste0(paths$output, "/", str_replace(x, str_sub(x, 9, nchar(x)), ".rds")))
}

walk(coe2_files, import_coe2)

# done.

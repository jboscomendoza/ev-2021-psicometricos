library(tidyverse)
library(haven)
library(feather)

dir.create("data_grados")

alumnos <- 
  read_sav("raw_data/ED2021_BDP_Fus_Alu_20220401.sav") %>% 
  select(-c("cct", "nom_esc")) %>% 
  mutate_at("grado", as.integer) %>% 
  mutate_if(is.character, as_factor) %>% 
  mutate_at(vars(matches("(M|L)\\d{2}")), as.integer) %>% 
  mutate_at(c("id", "id_turno"), as.integer) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  filter(!is.na(fuente))

write_feather(alumnos, "alumnos.feather")

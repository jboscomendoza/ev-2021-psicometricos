library(tidyverse)
library(readxl)
library(feather)
library(openxlsx)

archivo_excel <- "ed_resultados_integrado.xlsx"
archivo_feather <- "psicometricos.feather"

columnas_excel <- c("grado", "nombre", "rc", "p", "delta", "rc_p", "rc_rpbis",
                    "p_baja", "rpbis_baja")

psicom_feather <- 
  read_feather(archivo_feather) %>% 
  mutate(asignatura = ifelse(asignatura == "M", "mat", "lec")) %>% 
  unite(col = "grado", c("asignatura", "grado"), sep = "_") %>% 
  rename("nombre" = "item") %>% 
  mutate(nombre = str_replace(nombre, "^\\w", "R"))

psicom_excel <- 
  excel_sheets(archivo_excel) %>% 
  { .[str_detect(., pattern = "_\\d$")] } %>% 
  map_df(~read_excel(archivo_excel, .)) %>% 
  select(columnas_excel)

psicom_df <- inner_join(psicom_feather, psicom_excel, by = c("grado", "nombre"))

psicom_df %>% 
  group_by(grado) %>% 
  mutate(orden = dense_rank(beta)) %>% 
  select(grado, orden, beta, delta) %>% 
  pivot_longer(c("beta", "delta"), names_to = "tipo", values_to = "puntaje") %>% 
  ggplot() +
  aes(orden, puntaje, color = tipo) +
  geom_line() +
  labs(x = "Orden de reactivos", y = "Score") +
  facet_wrap("grado") + 
  theme_bw()


psicom_df %>% 
  ggplot() +
  aes(beta, delta) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap("grado") + 
  theme_bw()

psicom_feather %>% 
  filter(grado %in% c("mat_4", "mat_8")) %>% 
  mutate(
    score_irt = (beta * 100) + 500, 
    p_baja = ifelse(itemMean < .30, "Baja", "")
  ) %>% 
  select(
    "grado", "nombre", "dificultad_irt" = "beta", "p_correcta" = "itemMean",
    "rpbis" = "pBis", "p_baja", "rpbis_baja" = "disc_baja", "score_irt"
    ) %>% 
  split(.$grado) %>% 
  write.xlsx("psicometricos_4p_2s.xlsx")

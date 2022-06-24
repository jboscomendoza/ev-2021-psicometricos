library(tidyverse)

psicom <- read_feather("psicometricos.feather")

psicom %>% 
  group_by(asignatura, grado) %>% 
  mutate(orden = dplyr::dense_rank(beta)) %>% 
  ggplot(aes(orden, beta, color = grado)) +
  geom_line(aes(group = grado), alpha = .45) +
  geom_text(aes(label = grado), size = 4, alpha = .45) +
  facet_wrap(asignatura~.) +
  theme_bw() +
    theme(legend.position = "none")

psicom %>% 
  select(c("asignatura", "grado", "alphaCronbach")) %>% 
  tidyr::unite(col = "prueba", c("asignatura", "grado"), sep = "") %>% 
  distinct() %>% 
  ggplot(aes(prueba, alphaCronbach)) +
  geom_label(aes(label = round(alphaCronbach, 3))) +
  theme_bw()

psicom %>% 
  rename("Beta" = "beta") %>% 
  group_by(grado, asignatura) %>% 
  summarise("Beta" = mean(Beta)) %>% 
  ungroup() %>% 
  ggplot(aes(grado, Beta, color = asignatura)) +
  geom_line(aes(group = asignatura)) +
  geom_label(aes(label = round(Beta, 3))) +
  theme_bw()

psicom %>% 
  rename("Beta" = "beta") %>% 
  ggplot() +
  aes(asignatura, Beta, color = grado) +
  geom_boxplot() +
  geom_point(position = position_dodge(.75), alpha =.3) +
  theme_bw()


create_boxplot <- function(tabla, eje_y) {
  tabla %>% 
    ggplot() +
    aes(asignatura, {{ eje_y }}, color = grado) +
    geom_boxplot() +
    geom_point(position = position_dodge(.75), alpha =.3) +
    theme_bw()
}-2.5

create_boxplot(psicom, beta)

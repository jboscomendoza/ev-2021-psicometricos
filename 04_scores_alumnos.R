library(tidyverse)
library(TAM)
library(CTT)
library(feather)


get_reactivos <- function(tabla, n_grado, asignatura) {
  asig_regex <- paste0(asignatura, "\\d{2}")
  
  tabla %>% 
    filter(str_detect(grado, n_grado)) %>% 
    select(matches(asig_regex)) %>% 
    {
      na_cols <- .[as.logical(colSums(., na.rm = TRUE))]
      na_rows <- na_cols[rowSums(is.na(na_cols)) != ncol(na_cols), ]
      na_rows
    }
}


get_person <- function(tabla, n_grado, asignatura) {
  salida <- list()
  
  reactivos <- get_reactivos(tabla, n_grado, asignatura)
  
  if(NROW(reactivos) == 0) return(NULL)
  
  modelo   <- tam.mml(reactivos, verbose = FALSE)
  personas <- tam.wle(modelo)
  personas <- as_tibble(personas)
  
  tibble("asignatura" = asignatura, "grado" = n_grado) %>% 
    bind_cols(., personas)
}


grid_pruebas <- expand_grid("grado" = as.character(2:9), "asignatura" = c("M", "L"))


alumnos <- read_feather("alumnos.feather")

scores_alumnos <- 
  pmap_df(grid_pruebas, function(grado, asignatura) {
    get_person(alumnos, grado, asignatura)
  })

write_feather(scores_alumnos, "scores_alumnos.feather")

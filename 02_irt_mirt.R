library(tidyverse)
library(mirt)
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


get_mirt <- function(tabla, n_grado, asignatura, parametros=1) {
  salida <- list()
  
  reactivos <- get_reactivos(tabla, n_grado, asignatura)
  
  if(NROW(reactivos) == 0) return(NULL)
  
  if(parametros == 1) {
    salida$modelo <- mirt(reactivos, itemtype = "Rasch")
  } else {
    salida$modelo <- mirt(reactivos, itemtype = "2PL")
  }
  salida$parametros <- coef(salida$modelo, 
                            IRTpars = TRUE, 
                            simplify = TRUE)$items
  
  salida$parametros <- 
    salida$parametros %>% 
    as.data.frame() %>% 
    mutate("prueba" = paste0(asignatura, n_grado)) %>% 
    rownames_to_column("item") %>% 
    select(prueba, item, "dificultad_beta" = b, "discriminacion_alfa" = a)
  
  salida
}


# Datos
alumnos <- read_feather("alumnos.feather")

grid_pruebas <- expand_grid("grado" = as.character(2:9), 
                            "asignatura" = c("M", "L"))

# Proceso
psicometricos_2pl_mirt <-
  pmap(grid_pruebas, function(grado, asignatura) {
    get_mirt(alumnos, grado, asignatura, parametros = 2)
  })

psicometricos_2pl_df <- 
  map_df(psicometricos_2pl_mirt, ~.$parametros) %>% 
  tibble() %>% 
  mutate(
    rango_dificultad = ifelse(between(dificultad_beta, -3, 3), "", "Extremo"),
    rango_discriminacion = ifelse(discriminacion_alfa >= 0.5, "", "Baja")
  )

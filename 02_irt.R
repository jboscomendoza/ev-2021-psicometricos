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


get_psych <- function(tabla, n_grado, asignatura, parametros=1) {
  salida <- list()
  
  reactivos <- get_reactivos(tabla, n_grado, asignatura)
  
  if(NROW(reactivos) == 0) return(NULL)
  
  if(parametros == 1) {
    salida$irt <- tam.mml(reactivos, verbose = FALSE)
  } else {
    salida$irt <- tam.mml.2pl(reactivos, verbose = FALSE)
  }
  salida$ctt <- itemAnalysis(as.data.frame(reactivos))
  
  resultados <- 
    bind_cols(salida$irt$item_irt, salida$ctt$itemReport) %>%
    select(-"itemName") %>%
    mutate(alphaCronbach = salida$ctt$alpha)
  
  tibble("asignatura" = asignatura, "grado" = n_grado) %>% 
    bind_cols(., resultados) %>% 
    mutate(
      disc_baja = ifelse(pBis < 0.20, "Baja", ""),
      alpha_bajo = ifelse(alphaCronbach < alphaIfDeleted, "Baja", "")
    )
}


grid_pruebas <- expand_grid("grado" = as.character(2:9), 
                            "asignatura" = c("M", "L"))


alumnos <- read_feather("alumnos.feather")

# Un parametro
psicometricos <- 
  pmap_df(grid_pruebas, function(grado, asignatura) {
    get_psych(alumnos, grado, asignatura)
  })


write_feather(psicometricos, "psicometricos.feather")
write_csv(psicometricos, "psicometricos.csv")


# Dos parametros
psicometricos_2pl <- 
  pmap_df(grid_pruebas, function(grado, asignatura) {
    get_psych(alumnos, grado, asignatura, parametros = 2)
  })

write_feather(psicometricos_2pl, "psicometricos_2pl.feather")
write_csv(psicometricos_2pl, "psicometricos_2pl.csv")

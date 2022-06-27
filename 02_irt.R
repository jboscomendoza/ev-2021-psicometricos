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


get_psych <- function(tabla, n_grado, asignatura) {
  salida <- list()
  
  reactivos <- get_reactivos(tabla, n_grado, asignatura)
    
  if(NROW(reactivos) == 0) return(NULL)
   
   salida$irt <- tam.mml(reactivos, verbose = FALSE)
   salida$ctt <- itemAnalysis(as.data.frame(reactivos))
   
   resultados <- 
     bind_cols(salida$irt$item_irt, salida$ctt$itemReport) %>%
     select(-"itemName") %>%
     mutate(alphaCronbach = salida$ctt$alpha)
   
   tibble("asignatura" = asignatura, "grado" = n_grado) %>% 
     bind_cols(., resultados)
}


grid_pruebas <- expand_grid("grado" = as.character(2:9), 
                            "asignatura" = c("M", "L"))


alumnos <- read_feather("alumnos.feather")

psicometricos <- 
  pmap_df(grid_pruebas, function(grado, asignatura) {
    get_psych(alumnos, grado, asignatura)
  })


psicometricos_salida <- 
  psicometricos %>% 
  mutate(
    disc_baja = ifelse(pBis < 0.20, "Baja", ""),
    alpha_bajo = ifelse(alphaCronbach < alphaIfDeleted, "Baja", "")
  )
  

write_feather(psicometricos_salida , "psicometricos.feather")
write_csv(psicometricos_salida , "psicometricos.csv")

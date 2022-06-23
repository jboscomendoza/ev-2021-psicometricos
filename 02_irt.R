library(tidyverse)
library(TAM)
library(CTT)
library(feather)


get_psych <- function(tabla, n_grado, asignatura) {
  salida <- list()
  asig_regex <- paste0(asignatura, "\\d{2}")
  
  reactivos <- 
    tabla %>% 
    filter(str_detect(grado, n_grado)) %>% 
    select(matches(asig_regex)) %>% 
    {
      .[rowSums(., na.rm = TRUE) > 0, as.logical(colSums(., na.rm = TRUE))]
    }
    
  
  # if(NROW(reactivos) == 0) {
  #   return(NULL)
  # }
  # 
  # salida$irt <- tam.mml(reactivos, verbose = FALSE)
  # salida$ctt <- itemAnalysis(as.data.frame(reactivos))
  # 
  # resultados <- 
  #   bind_cols(salida$irt$item_irt, salida$ctt$itemReport) %>%
  #   tibble() %>%
  #   select(-"itemName") %>%
  #   mutate(alphaCronbach = salida$ctt$alpha)
  # 
  # tibble("asignatura" = asignatura, "grado" = n_grado) %>% 
  #   bind_cols(., resultados)
}


pmap(grid_pruebas, function(grado, asignatura) {
  get_psych(alumnos, grado, asignatura)
})


grid_pruebas <- expand_grid("grado" = as.character(2:9), "asignatura" = c("M", "L"))


psicometricos <- 
  pmap_df(grid_pruebas, function(grado, asignatura) {
    get_psych(alumnos, grado, asignatura)
  })


write_feather(psicometricos, "psicometricos.feather")
write_csv(psicometricos, "psicometricos.csv")

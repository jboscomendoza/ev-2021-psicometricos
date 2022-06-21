library(tidyverse)
library(feather)
library(CTT)

alumnos <- read_feather("alumnos.feather")

out_ctt_lec <- 
  split(alumnos, list(alumnos$grado, alumnos$fuente)) %>% 
  map(select, matches("L\\d{2}")) %>%
  map(function(x){
    seleccion <- as.logical(colSums(x, na.rm = TRUE))
    tabla <- x[seleccion]
    tabla
    if(ncol(tabla) > 1) {
       itemAnalysis(as.data.frame(tabla))
    }
  })
  

get_ctt <- function(datos, asig) {
  patron <- paste0(asig, "\\d{2}")
  
  split(datos, list(datos$grado, datos$fuente)) %>% 
    map(select, matches(patron)) %>%
    map(function(x){
      seleccion <- as.logical(colSums(x, na.rm = TRUE))
      tabla <- x[seleccion]
      tabla
      if(ncol(tabla) > 1) {
        itemAnalysis(as.data.frame(tabla))
      }
    }) %>% 
    compact()
}


consolidar <- function(lista) {
  map_df(names(lista), function(x) {
    lista[[x]][["itemReport"]] %>% 
      as_tibble() %>% 
      mutate(Grupo = x) %>% 
      separate(Grupo, into = c("grado", "formato"), sep = "\\.")
  })
}


ctt_lec <- get_ctt(alumnos, "L")
ctt_mat <- get_ctt(alumnos, "M")

ctt_consolidado <- 
  bind_rows(
    consolidar(ctt_lec),
    consolidar(ctt_mat)
  ) %>% 
  mutate(asignatura = ifelse(grepl(itemName, pattern = "L"), 
                             "Lectura", "Matemáticas" ))

write.csv(ctt_consolidado, file = "ctt_consolidad.csv", 
          fileEncoding = "latin1", row.names = FALSE)



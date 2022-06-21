library(tidyverse)
library(readxl)
library(janitor)
library(openxlsx)


archivo_psico <- "psicometricos.xlsx"
grados <- excel_sheets(archivo_psico)


resultados <- 
  map(grados, function(hoja) {
    read_excel(archivo_psico, hoja) %>% 
      clean_names()
  }) %>% 
  `names<-`(grados)


resultados_rc <- 
  map(resultados, function(tabla){
    col_union <- "nombre"  
    
    valores <- 
      map(c("p", "rpbis"), function(est) {
        col_regex <- paste0(est, "\\d")
        col_nom <- paste0("rc_", est)
        
        tabla %>% 
          select(col_union, "rc", matches(col_regex)) %>% 
          mutate(rc = paste0(est, rc)) %>% 
          pivot_longer(cols = matches(col_regex)) %>% 
          filter(rc == name)  %>% 
          select(nombre,  value) %>% 
          `names<-`(c(col_union, col_nom))
      }) 
    
    frecuentes <- 
      tabla %>% 
      select(nombre, rc, matches("p\\d")) %>% 
      mutate(rc = paste0("p", rc)) %>% 
      pivot_longer(cols = matches("p\\d")) %>% 
      group_by(nombre) %>% 
      filter(value == max(value)) %>% 
      transmute(nombre, 
                frec_p = value,
                frec_opcion = name, 
                frec_correcta = ifelse(rc == name, "", "No"))
    
    
    reduce(valores, inner_join, by = col_union) %>% 
      inner_join(tabla, ., by = col_union) %>% 
      inner_join(., frecuentes, by = col_union) 
    
  })

resultados_baja <- 
  map(resultados_rc, function(tabla){
    tabla %>% 
      mutate(
        p_baja = ifelse(rc_p < .30, "Baja", ""),
        rpbis_baja = ifelse(rc_rpbis < .30, "Baja", ""),
        dificultad = (delta * 100) + 500
        )
  })

resultados_grado <- 
  map(names(resultados_baja), function(x) {
    tabla <- resultados_baja[[x]]
    datos_renglon <- rep(x, NROW(tabla)) 
    
    grado <- tibble("grado" = datos_renglon)
    
    bind_cols(grado, tabla)
  }) %>% 
  `names<-`(grados)


write.xlsx(resultados_grado, "resultados_integrado.xlsx")

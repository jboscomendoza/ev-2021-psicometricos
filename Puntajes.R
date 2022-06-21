library(tidyverse)
library(readxl)
library(janitor)
library(openxlsx)


puntajes_archivo <- "puntajes.xlsx"
puntajes_hojas <- excel_sheets(puntajes_archivo)


puntajes <- 
  map(puntajes_hojas, function(hoja) {
  tabla <- 
    read_excel(puntajes_archivo, hoja) %>% 
    clean_names() %>% 
    mutate_at("puntaje", as.numeric) %>% 
    mutate(p_aciertos = (puntaje / max(puntaje)) * 100)
  
  grado_df <- tibble("grado" = rep(hoja, NROW(tabla)))
  
  bind_cols(grado_df, tabla) %>% 
    select(
      grado, puntaje, p_aciertos, frecuencia,	porcentaje,	porcentaje_acumulado
      )
}) %>% 
  `names<-`(puntajes_hojas)




cuantilizador <- function(x, prefijo="q") {
  cuantiles <- 
    quantile(x) %>% 
    as.matrix() %>% 
    t() %>% 
    as.tibble()
  
  nombres <- 
    paste(prefijo, names(cuantiles), sep = "_") %>% 
    gsub(pattern = "%", replacement = "")
  
  names(cuantiles) <- nombres
  
  cuantiles
}


resumen_puntaje <- 
  map_df(puntajes, function(hoja) {
    p_casos <- rep(hoja[["p_aciertos"]], hoja[["frecuencia"]])
    a_casos <- rep(hoja[["puntaje"]], hoja[["frecuencia"]])
    
    grado <- hoja[["grado"]][[1]]
    
    descriptores <- 
      tibble(
        "grado" = grado, 
        "pct_media" = mean(p_casos), "pct_sd" = sd(p_casos),
        cuantilizador(p_casos, "pct"),
        "act_media" = mean(a_casos), "act_sd" = sd(a_casos),
        cuantilizador(a_casos, "act")
      )
  })




puntajes$resumen <- resumen_puntaje

write.xlsx(puntajes, "resumen_puntajes.xlsx")

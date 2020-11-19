### Funcion para obtener largo, ancho y indice de areola (forma)

## Librearias necesarias
library(ggplot2)
library(dplyr)
library(cluster)

form_are_esp_rad <- function(df, x, y){
  
  ## Graficar los puntos de inicio de las espinas y obtener una elipse ajustada 
  p_esp_R <- df %>%
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    stat_ellipse(level = 0.65)  
  
  ## Obtener los datos de la elipse ajustada
  pb <- ggplot_build(p_esp_R)
  
  ## Asignar las corsenadas de los puntos de la elipse ajustada
  el <- pb$data[[2]][c("x","y")]
  
  ## Convertir a matriz
  el_mat <- as.matrix(el)
  
  ## Calcular los elemntos de la elipse ajustada 
  ## Obtener todos los puntos de x y y de la elipse
  exy <- predict(ellipsoidhull(el_mat))
  
  ## Cambiar el nombre de columna 
  me <- colMeans((exy))
  
  ## Obtener el centro de la elipse
  dist2center <- sqrt(rowSums((t(t(exy)-me))^2))
  
  ## Calcular la distancia del eje mayor y menor de la elipse
  max_dis <- data.frame(max_dis = max(dist2center), min_dis = min(dist2center))
  
  ## Obtener las longitud, ancho y el indice de la areola a partir de el eje mayor y menor
  are_inf <- max_dis %>%
    mutate(largo = (max_dis * 2), ancho = (min_dis * 2)) %>%
    mutate(ind_form = largo/ancho) %>%
    select(-max_dis, -min_dis)
  
  return(are_inf)
}
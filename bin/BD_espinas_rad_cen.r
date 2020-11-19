## Este scrpt es para subir todos los archivos .csv de espinas centrales y radiales

## funcion para leer archivos .csv de espinas centrales
source("read_esp_cen.R")

## funcion para leer archivos .csv de espinas centrales
source("read_esp_rad.R")

## enlista en la variable "local" lo que hay en la carpeta Mesures
local <- list.files("../data/Mesures/")

#### Espinas centrales #####
## se crea el data frame vacio para que no se sobreescriba 
df_esp_cen <- data.frame()

## se utiliza el vector "local"  y asigna cada valor a la variable "i"
for (i in local) {
  ## enlista los archivos de la carpeta "CC020" en la variable "planta"
  planta <- list.files(paste0("../data/Mesures/", i))
  ## se utiliza el vector "planta"  y asigna cada valor a la variable "j"
  for (j in planta) {
    ## se enlistan todos los archivos csv de espinas centrales
    lista_archivos <- list.files(paste0("../data/Mesures/", i, "/", j, "/centrales"))
    ## se utiliza el vector "lista_archivos"  y asigna cada valor a la variable "w"
    for (w in lista_archivos) {
      ## se leen todos los archivos que est?n en el vector "lista_archivos"
      df_1 <- read_esp_cen(paste0("../data/Mesures/", i, "/", j, "/centrales/", w))
      ## crea una columna llamada #pat"  para reconocer el patr?n del nombre de los archivos
      pat <- data.frame(id = rep(stringr::str_extract(w, pattern = "CC0[0-9]+_P0[0-9]+_A[0-9]+"), times = nrow(df_1)))
      ## une la columna "pat" con df_1
      df_1 <- cbind(pat, df_1)
      ## une los archivos por filas
      df_esp_cen <- rbind(df_esp_cen, df_1)
      
    }
    
  }
  
}

## separa la columna id en "localidad, "Planta" y "Areola
df_esp_cen <- separate(df_esp_cen, id, c("Localidad", "Planta", "Areola"), sep = "_")

## Guardar la base de datos en formato .cvs
write.csv(df_esp_cen, file = "../out/files/Espinas_centrales.csv", row.names = FALSE)


#### Espinas radiales #####
## se crea el data frame vacio para que no se sobreescriba 
df_esp_rad <- data.frame()

## se utiliza el vector "local"  y asigna cada valor a la variable "i"
for (i in local) {
  ## enlista los archivos de la carpeta "CC020" en la variable "planta"
  planta <- list.files(paste0("../data/Mesures/", i))
  ## se utiliza el vector "planta"  y asigna cada valor a la variable "j"
  for (j in planta) {
    ## se enlistan todos los archivos csv de espinas centrales
    lista_archivos <- list.files(paste0("../data/Mesures/", i, "/", j, "/radiales"))
    ## se utiliza el vector "lista_archivos"  y asigna cada valor a la variable "w"
    for (w in lista_archivos) {
      ## se leen todos los archivos que est?n en el vector "lista_archivos"
      df_1 <- read_esp_rad(paste0("../data/Mesures/", i, "/", j, "/radiales/", w))
      ## crea una columna llamada #pat"  para reconocer el patr?n del nombre de los archivos
      pat <- data.frame(id = rep(stringr::str_extract(w, pattern = "CC0[0-9]+_P0[0-9]+_A[0-9]+"), times = nrow(df_1)))
      ## une la columna "pat" con df_1
      df_1 <- cbind(pat, df_1)
      ## une los archivos por filas
      df_esp_rad <- rbind(df_esp_rad, df_1)
      
    }
    
  }
  
}

## separa la columna id en "localidad, "Planta" y "Areola
df_esp_rad <- separate(df_esp_rad, id, c("Localidad", "Planta", "Areola"), sep = "_")

## Guardar la base de datos en formato .cvs
write.csv(df_esp_rad, file = "../out/files/Espinas_radiales.csv", row.names = FALSE)

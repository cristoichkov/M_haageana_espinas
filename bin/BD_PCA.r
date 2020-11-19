#### Transformar y ordenar las bases de datos de espinas radiales y centrales para realizar un PCA###
library(dplyr)
library(tidyr)

## Funciones
source("form_are_esp_rad.R")

## Cargar la base de datos de espinas radiales
esp_rad <- read.csv("../out/files/Espinas_radiales.csv")

## Obtener el numero de espinas por localidad, planta y areola
num_esp_rad <- esp_rad %>%
  select(Localidad, Planta, Areola, Num_esp) %>%
  group_by(Localidad, Planta, Areola) %>%
  summarise(num_esp_rad = max(Num_esp)) %>%
  unite(col = "ID", Localidad, Planta, Areola)

## Pasar la posicion de espinas radiales de formato largo a ancho
df_esp_rad <- esp_rad %>%
  unite(col = "ID", Localidad, Planta, Areola) %>%
  select(ID, posicion, long_esp) %>%
  group_by(ID, posicion) %>%
  summarise(long_esp = mean(long_esp)) %>%
  spread(posicion, long_esp) 



## Obtener las medidas de la forma de las espinas radiales 
df_esp_form <- data.frame()

for (i in levels(esp_rad$Localidad)) {
  
  ## Filtrar y ajustar los factores de la variable Planta
  df_are_2 <- esp_rad %>%
    filter(Localidad == i) %>%
    mutate(Planta = as.character(Planta)) %>%
    mutate(Planta = factor(Planta))
  
  for (w in levels(df_are_2$Planta)) {
    
    ## Filtrar y ajustar los factores de la variable Planta
    df_are_1 <- df_are_2 %>%
      filter(Planta == w) %>%
      mutate(Areola = as.character(Areola)) %>%
      mutate(Areola = factor(Areola))
    
    for (h in levels(df_are_1$Areola)) {
      
      ## Filtrar y ajustar los factores de la variable Planta
      df_are <- df_are_1 %>%
        filter(Areola == h)
      
      ## Obtener largo, ancho y indice de areola con la funcion areola
      df_are_med <- form_are_esp_rad(df_are, x = df_are$X_2, y = df_are$Y_2)

      ## Cambiar el nombre de las variable
      colnames(df_are_med) <- c("larg_esp_rad", "anch_esp_rad", "ind_esp_rad")
      
      ## Agregar un columna ID
      ID <- data.frame(ID = paste0(i, "_", w, "_", h))
      
      df_are_med <- cbind(ID, df_are_med)
      
      df_esp_form <- rbind(df_esp_form, df_are_med)
      
    }
  }
}


## Obtener las medidas de la areola con el data frame
df_areola <- data.frame()

for (i in levels(esp_rad$Localidad)) {
  
  ## Filtrar y ajustar los factores de la variable Planta
  df_are_2 <- esp_rad %>%
    filter(Localidad == i) %>%
    mutate(Planta = as.character(Planta)) %>%
    mutate(Planta = factor(Planta))
  
  for (w in levels(df_are_2$Planta)) {
    
    ## Filtrar y ajustar los factores de la variable Planta
    df_are_1 <- df_are_2 %>%
      filter(Planta == w) %>%
      mutate(Areola = as.character(Areola)) %>%
      mutate(Areola = factor(Areola))
    
    for (h in levels(df_are_1$Areola)) {
      
      ## Filtrar y ajustar los factores de la variable Planta
      df_are <- df_are_1 %>%
        filter(Areola == h)
      
      ## Obtener largo, ancho y indice de areola con la funcion areola
      df_are_med <- form_are_esp_rad(df_are, x = df_are$X_1, y = df_are$Y_1)
      
      ## Cambiar el nombre de las variable
      colnames(df_are_med) <- c("larg_are", "anch_are", "ind_are")
      
      ## Agregar un columna ID
      ID <- data.frame(ID = paste0(i, "_", w, "_", h))
      
      df_are_med <- cbind(ID, df_are_med)
      
      df_areola <- rbind(df_areola, df_are_med)
      
    }
  }
}


## Cargar la base de datos de espinas centrales
esp_cen <- read.csv("../out/files/Espinas_centrales.csv")

## Pasar la posicion de espinas centrales de formato largo a ancho
df_esp_cen <- esp_cen %>%
  unite(col = "ID", Localidad, Planta, Areola) %>%
  select(ID, posicion, long_esp) %>%
  spread(posicion, long_esp) %>%
  mutate(prop_inf_sup = sup/inf) 
    
## Cargar la base de datos de numero de espinas centrales
num_esp_cen <- read.csv("../out/files/num_esp_cen.csv")

num_esp_cen <- num_esp_cen %>%
  select(Id, n_cen_spin) %>%
  rename(ID = Id, num_esp_cen = n_cen_spin) %>%
  separate(ID, c("Localidad", "Planta", "Areola"))

## Corregir el id de la areola agregando un 0 de la A1 a la A9
num_esp_cen$Areola <- ifelse(num_esp_cen$Areola == "A1", "A01",
                               ifelse(num_esp_cen$Areola == "A2", "A02",
                                      ifelse(num_esp_cen$Areola == "A3", "A03",
                                             ifelse(num_esp_cen$Areola == "A4", "A04",
                                                    ifelse(num_esp_cen$Areola == "A5", "A05",
                                                           ifelse(num_esp_cen$Areola == "A6", "A06",
                                                                  ifelse(num_esp_cen$Areola == "A7", "A07", 
                                                                         ifelse(num_esp_cen$Areola == "A8", "A08",
                                                                                ifelse(num_esp_cen$Areola == "A9", "A09", "A10")))))))))

num_esp_cen <- num_esp_cen %>%
  unite(col = "ID", Localidad, Planta, Areola)

## Cargar base de datos de flores

flores <- read.csv("../out/files/Flores_form_color.csv") %>%
  unite(col = "ID", Localidad, Planta, Areola) 


## Unir las cuatro base de datos num_esp_rad, df_esp_rad, df_esp_cen y num_esp_cen

PCA_df <- left_join(df_esp_rad, num_esp_rad, by = "ID") %>%
  left_join(., df_esp_form, by = "ID") %>%
  left_join(., df_esp_cen, by = "ID") %>%
  left_join(., num_esp_cen, by = "ID") %>%
  left_join(., df_areola, by = "ID") %>%
  left_join(., flores, by = "ID") %>%
  separate(ID, c("Localidad", "Planta", "Areola"))


M_haageana <- c("CC020", "CC021", "CC022")

M_oaxacana <- c("CC038", "CC039", "CC034", "CC033", "CC040", "CC035")

PCA_df$group_gen <- ifelse(PCA_df$Localidad %in% M_haageana, "M_haageana",
                           ifelse(PCA_df$Localidad %in% M_oaxacana, "M_oaxacana",
                                  ifelse(PCA_df$Localidad == "CC029", "M_acultzingensis",
                                         ifelse(PCA_df$Localidad == "CC046", "Sto_Domingo",
                                                ifelse(PCA_df$Localidad == "CC036", "Huauclilla",
                                                       ifelse(PCA_df$Localidad == "CC047", "M_albilanata",
                                                              ifelse(PCA_df$Localidad %in% c("CC043", "CC044"), "Tehuantepec", "Balsas")))))))


write.csv(PCA_df, file = "../out/files/PCA_espinas_areola_flores.csv", row.names = FALSE)

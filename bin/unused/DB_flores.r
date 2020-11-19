library(tibble)
library(dplyr)
library(tidyr)

flores <- read.csv("../data/Datos_raw.csv")

df_flores <- flores %>% 
  filter(Localidad != "CC024" & Localidad != "CC026" & Localidad != "CC037" & Localidad != "CC041" & Localidad != "san_ang") %>%
  mutate(Localidad = as.character(Localidad)) %>%
  mutate(Localidad = as.factor(Localidad)) %>%
  separate(Imagen, c("Localidad_2", "Planta", "Fot", "Num", "SEF", "pm")) %>%
  select(-Localidad_2, -Flor, -Fot, -Num, -SEF, -pm) 


df_flores_fin <- data.frame()

for (i in levels(df_flores$Localidad)) {
  
  df_1 <- df_flores %>%
    filter(Localidad == i) %>%
    mutate(Planta = as.character(Planta)) %>%
    mutate(Planta = factor(Planta))
  
  for (w in levels(df_1$Planta)) {
    
    df_2 <- df_1 %>%
      filter(Planta == w) %>%
      rownames_to_column() %>%
      mutate(Areola = paste0("A0", rowname)) %>%
      select(-rowname) %>%
      select(Localidad, Planta, Areola, PC1S:PC10A)
      
    df_flores_fin <- rbind(df_flores_fin, df_2)
    
  }
}

write.csv(df_flores_fin, file = "../out/files/Flores_form_color.csv", row.names = FALSE)

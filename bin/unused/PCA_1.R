source("export_formattable.R")

PCA_df <- read_csv("../out/files/PCA_espinas_areola_flores.csv") 

PCA_espinas <- na.omit(PCA_df[, c(47, 7, 16:19, 21:26)])

######### Calcular componentes principales ###########

bios_val <- PCA_espinas[-1]

## Generamos la matriz de datos centrados y estandarizados de nuestras variables usando el comando scale: 

CentrEst <- scale(bios_val)

## Calculamos la matríz de correlación entre todas las variables mediante el método de spearman:
MatCorr <- cor(CentrEst, method="spearman") 


## calculamos los eigen vectorers y eigen values 

Eigen <- eigen(MatCorr) 

eigvec <- Eigen$vectors 

eigval <- Eigen$values 

## calculamos el porcentaje de participación de cada componente: 
Porcentaje <- (Eigen$values)/(sum(Eigen$values))  ## muestra el % individual de cada componente

pca.porcen <- data.frame(Porcentaje)

pca.porcen$index <- as.factor(1:nrow(pca.porcen))

pca.porcen$index.cont <- 1:nrow(pca.porcen)

percent_p <- pca.porcen %>%
  ggplot(aes(x = index, y = Porcentaje, label = round(Porcentaje*100, 2))) +
  geom_bar(stat = "identity") +
  geom_path(aes(x = index.cont), size = 1, colour = "Gray50") +
  geom_point(size = 3) +
  geom_label_repel(nudge_y = 0.01, direction = "x")

ggsave(percent_p, file="../out/estadisticas/Porcentaje_componentes_espinas.png", device="png", dpi = 300, width = 24, height = 14)


PorcentajeAcum <- cumsum(Porcentaje)  ## muestra el % acumulado del número de componente

## calculamos los componentes principales: 
Componentes <- CentrEst %*% Eigen$vector

## calculamos la correlacion de variables
CorrelaVariables <- cor(cbind(CentrEst,Componentes))[1 : ncol(CentrEst), 
                                                     (ncol(CentrEst)+1):(ncol(CentrEst)+ncol(Componentes))] 

## calculamos la comunalidad (pareticipacion de cada cada vartiable dentro del compnete)
Comunali <- CorrelaVariables^2

cp_1 <- data.frame(Comunali) %>% 
  rownames_to_column() %>%
  as.tibble() %>%
  select(rowname, V1) %>%
  rename(Variable_C1 = rowname, Componente_1 = V1) %>%
  arrange(desc(Componente_1)) %>%
  mutate(Componente_1 = round(Componente_1, 3))

cp_2 <- data.frame(Comunali) %>% 
  rownames_to_column() %>%
  as.tibble() %>%
  select(rowname, V2) %>%
  rename(Variable_C2 = rowname, Componente_2 = V2) %>%
  arrange(desc(Componente_2)) %>%
  mutate(Componente_2 = round(Componente_2, 3))

cp_3 <- data.frame(Comunali) %>% 
  rownames_to_column() %>%
  as.tibble() %>%
  select(rowname, V3) %>%
  rename(Variable_C3 = rowname, Componente_3 = V3) %>%
  arrange(desc(Componente_3)) %>%
  mutate(Componente_3 = round(Componente_3, 3))


cp_4 <- data.frame(Comunali) %>% 
  rownames_to_column() %>%
  as.tibble() %>%
  select(rowname, V4) %>%
  rename(Variable_C4 = rowname, Componente_4 = V4) %>%
  arrange(desc(Componente_4)) %>%
  mutate(Componente_4 = round(Componente_4, 3))

df_stat_comun <- cbind(cp_1, cp_2, cp_3, cp_4)

tab_stat_comun <- formattable(df_stat_comun, align = c("l", "c", "l", "c", "l", "c", "l", "c"))

## Guardar tabla en formato .png
export_formattable(tab_stat_comun, file = "../out/estadisticas/Contribucion_var_comp_espinas.png")


## imprimimos la gráfica de componente uno contra dos:]
df_componentes <- data.frame(Componentes)

df_com_esp <- cbind(PCA_espinas, df_componentes)


morf_all <- df_com_esp %>%
  ggplot(aes(x = X1, y = X2, color = group_gen, shape = group_gen)) +
  geom_point(size = 4) +
  scale_shape_manual(values = 11:18) +
  theme(legend.title = element_text(size = 17), legend.text = element_text(size=17), legend.position = "right", 
        legend.text.align = 0, text = element_text(size=18))
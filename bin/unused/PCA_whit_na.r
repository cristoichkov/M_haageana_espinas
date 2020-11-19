library(dplyr)
library(readr)
library(missMDA)
library(VIM)
library(naniar)
library(FactoMineR)

### Script para realizar PCA con missing data ###

PCA_df <- read_csv("../out/files/PCA_espinas_areola_flores.csv")

grupo_gen <- PCA_df$group_gen

PCA_df_with_na <- PCA_df[, c(7, 16:19, 21:26, 37:46)]

## Generamos la matriz de datos centrados y estandarizados de nuestras variables usando el comando scale: 

PCA_df_with_na <- data.frame(scale(PCA_df_with_na))


nb <- estim_ncpPCA(PCA_df_with_na, ncp.min = 0, ncp.max = 5, method.cv = "Kfold", nbsim = 100, pNA = 0.05) # estimate the number of components from incomplete data
#(available methods include GCV to approximate CV)
nb$ncp #2

plot(0:5, nb$criterion, xlab = "nb dim", ylab = "MSEP")

res.comp <- imputePCA(PCA_df_with_na, ncp = nb$ncp)



imp <- cbind.data.frame(res.comp$completeObs, grupo_gen)

res.pca <- PCA(imp, quanti.sup = 1, quali.sup = 22, ncp = nb$ncp, graph=FALSE)

plot(res.pca, hab=22, lab="quali")

plot(res.pca, choix="var")

res.pca$var$cor

PCA_coords <- cbind.data.frame(res.pca$ind$coord, grupo_gen) 

PCA_coords %>%
  ggplot(aes(x = Dim.1, y = Dim.3, color = grupo_gen, shape = grupo_gen)) +
  geom_point(size = 4) +
  scale_shape_manual(values = 11:18) +
  theme(legend.title = element_text(size = 17), legend.text = element_text(size=17), legend.position = "right", 
        legend.text.align = 0, text = element_text(size=18))

PCA_coords %>%
  ggplot(aes(x = grupo_gen, y = Dim.4, color = grupo_gen)) +
  geom_boxplot()


PCA_coords %>%
  filter(grupo_gen != "Balsas", grupo_gen != "Sto_Domingo", grupo_gen != "M_albilanata") %>%
  ggplot(aes(x = Dim.1, y = Dim.2, color = grupo_gen, shape = grupo_gen)) +
  geom_point(size = 4) +
  scale_shape_manual(values = 11:18) +
  stat_ellipse(aes(fill = grupo_gen), geom="polygon",level=0.95,alpha=0.2) +
  theme(legend.title = element_text(size = 17), legend.text = element_text(size=17), legend.position = "right", 
        legend.text.align = 0, text = element_text(size=18))

PCA_coords %>%
  filter(grupo_gen != "M_acultzingensis", grupo_gen != "M_haageana", grupo_gen != "M_oaxacana") %>%
  ggplot(aes(x = Dim.1, y = Dim.2, color = grupo_gen, shape = grupo_gen)) +
  geom_point(size = 4) +
  scale_shape_manual(values = 11:18) +
  stat_ellipse(aes(fill = grupo_gen), geom="polygon",level=0.95,alpha=0.2)

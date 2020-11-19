library(ggplot2)
library(tidyr)
library(tibble)
library(dplyr)
library(cluster)

bd_esp_rad <- read.csv("../out/files/Espinas_radiales.csv")

esp_rad_are <- bd_esp_rad %>%
  filter(Localidad == "CC020", Planta == "P01", Areola == "A01") 

p_esp_R <- esp_rad_are %>%
  ggplot(aes(x = X_1, y = Y_1)) +
  geom_point() +
  stat_ellipse(level = 0.65)  

pb <- ggplot_build(p_esp_R)

el <- pb$data[[2]][c("x","y")]

el_mat <- as.matrix(el)

exy <- predict(ellipsoidhull(el_mat))

me <- colMeans((exy))

dist2center <- sqrt(rowSums((t(t(exy)-me))^2))

max_dis <- max(dist2center)     ## major axis

min_dis <- min(dist2center)   ## minor axis  

exy <- data.frame(cbind(exy, dist2center))

colnames(exy)[1:2] <- c("x", "y")

exy_X_1 <- exy %>%
  filter(dist2center == max_dis) %>%
  select(x, y) %>%
  distinct() %>%
  rownames_to_column() %>%
  filter(rowname == 1) %>%
  select(-rowname) %>%
  rename(X_1 = x, Y_1 = y)

exy_X_2 <- exy %>%
  filter(dist2center == max_dis) %>%
  select(x, y) %>%
  distinct() %>%
  rownames_to_column() %>%
  filter(rowname == 2) %>%
  select(-rowname) %>%
  rename(X_2 = x, Y_2 = y)

X_line <- cbind(exy_X_1, exy_X_2)  

exy_Y_1 <- exy %>%
  filter(dist2center == min_dis) %>%
  select(x, y) %>%
  distinct() %>%
  rownames_to_column() %>%
  filter(rowname == 1) %>%
  select(-rowname) %>%
  rename(X_1 = x, Y_1 = y)

exy_Y_2 <- exy %>%
  filter(dist2center == min_dis) %>%
  select(x, y) %>%
  distinct() %>%
  rownames_to_column() %>%
  filter(rowname == 2) %>%
  select(-rowname) %>%
  rename(X_2 = x, Y_2 = y)

Y_line <- cbind(exy_Y_1, exy_Y_2) 

exy %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_segment(X_line, mapping=aes(x=X_1, y=Y_1, xend=X_2, yend=Y_2)) +
  geom_segment(Y_line, mapping=aes(x=X_1, y=Y_1, xend=X_2, yend=Y_2)) +
  xlim(-0.7, 0.7) + ylim(-0.7, 0.7) +
  coord_fixed(ratio = 1) 


esp_rad_are %>%
  ggplot(aes(x = X_1, y = Y_1)) +
  geom_point() +
  geom_segment(X_line, mapping=aes(x=X_1, y=Y_1, xend=X_2, yend=Y_2)) +
  geom_segment(Y_line, mapping=aes(x=X_1, y=Y_1, xend=X_2, yend=Y_2)) +
  xlim(-0.7, 0.7) + ylim(-0.7, 0.7) +
  coord_fixed(ratio = 1) 

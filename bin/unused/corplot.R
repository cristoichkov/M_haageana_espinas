library(tidyverse)
library(corrplot)


cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat) 
  p.mat
}

PCA_df <- read.csv("../out/files/PCA_espinas_areola_flores.csv") 

cor_flore <- PCA_df[,27:46] %>%
  na.omit() %>%
  cor()

.mat <- cor.mtest(cor_flore )

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor_flore, method="color", col=col(200),  
         diag=FALSE, # tl.pos="d", 
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         # hide correlation coefficient on the principal diagonal
         mar=c(0,0,1,0) 
)


cor_espinas <- PCA_df[,4:26] %>%
  na.omit() %>%
  cor()

.mat <- cor.mtest(cor_espinas)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor_espinas, method="color", col=col(200),  
         diag=FALSE, # tl.pos="d", 
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         # hide correlation coefficient on the principal diagonal
         mar=c(0,0,1,0) 
)

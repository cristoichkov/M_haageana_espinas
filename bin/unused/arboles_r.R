library(dplyr)
library(readr)
library(caret)
library(rpart)
library(rpart.plot)
library(ROCR)


PCA_df_no_na  <- read_csv("../out/files/PCA_espinas_areola_flores.csv") 

PCA_df_no_na <- na.omit(PCA_df_no_na[, c(47, 7, 16:19, 21:26, 37:46)])


set.seed(2000)

training.ids <- createDataPartition(PCA_df_no_na$group_gen, p = 0.7, list = FALSE)

mod <- rpart(group_gen ~ ., data = PCA_df_no_na[training.ids, ], method = "class", 
             control = rpart.control(minsplit = 20, cp = 0.01))


prp(mod, type = 2, extra = 104, nn = TRUE, fallen.leaves = TRUE, faclen = 4, 
    varlen = 8, shadow.col = "grey")


data.frame(mod$cptable) %>%
  mutate(error = 0.3899083+xstd)

mod.pruned <- prune(mod, mod$cptable[6, "CP"])

prp(mod.pruned, type = 2, extra = 104, nn = TRUE, fallen.leaves = TRUE, faclen = 4, 
                 varlen = 8, shadow.col = "grey")
)
  
colnames(PCA_df_no_na)
    
pre.pruned <- predict(mod.pruned, PCA_df_no_na[-training.ids, ], type = "class")    

table(PCA_df_no_na[-training.ids, ]$group_gen, pre.pruned, dnn = c("Actual", "Predicho"))

PCA_df_no_na$M_albilanata <- ifelse(PCA_df_no_na$group_gen == "M_albilanata", 1, 0)

pre.pruned2 <- predict(mod.pruned, PCA_df_no_na[-training.ids, ], type = "prob")

pred <- prediction(pre.pruned2[ ,4], PCA_df_no_na[-training.ids, "M_albilanata"])

perf <- performance(pred, "tpr", "fpr")

plot(perf)

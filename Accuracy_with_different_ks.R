library(tidyverse)

library(caret)

library(dslabs)

data("tissue_gene_expression")

dat <- tissue_gene_expression

ks = seq(1,11,2)

acc <- map_dbl(ks, function(x) {
  
  set.seed(1)

  train_ind <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)

  trainx <- dat$x[train_ind,]

  testx <- dat$x[-train_ind,]
  
  trainy <- dat$y[train_ind]
  
  testy <- dat$y[-train_ind]

  
  fit <- knn3(trainx, trainy, k = x)
  
  y_hat_knn <- predict(fit, testx, type = "class")
  
   confusionMatrix(y_hat_knn, testy)$overall["Accuracy"]
  
})

acc

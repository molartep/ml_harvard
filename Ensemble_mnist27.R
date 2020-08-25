models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")

library(caret)
library(dslabs)
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

y_hat_matrix <- sapply(fits, predict, mnist_27$test)

dim(y_hat_matrix)

c <- c(1:23)
confusionvector <- sapply(c, function(c){
  confusionMatrix(factor(y_hat_matrix[,c]), mnist_27$test$y)$overall["Accuracy"]
  })

mean(confusionvector)

r <- c(1:200)
new_mat <- y_hat_matrix[r,] == 2
ensemble_ind <- rowMeans(new_mat)
ensemble <- ifelse(ensemble_ind > 0.5, ensemble_ind <- 2,  ensemble_ind <- 7)
  
ensemble_acc <- confusionMatrix(factor(ensemble), mnist_27$test$y)$overall["Accuracy"]

confusionvector > ensemble_acc

accs <- sapply(c, function(c){
  fits[[c]][4]$results$Accuracy
})

mean(as.numeric(as.character(unlist(accs))))
fits[[23]][4]$results

#New ensemble

ind <- confusionvector >= 0.8
sum(ind)
models_new <- models[ind]
  

fits_new <- lapply(models_new, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits_new) <- models_new

y_hat_matrix_new <- sapply(fits_new, predict, mnist_27$test)

cols <- dim(y_hat_matrix_new)[2]

r <- c(1:200)
new_matrix <- y_hat_matrix_new[r,] == 2
ensemble_index <- rowMeans(new_matrix)
ensemble_new <- ifelse(ensemble_index > 0.5, ensemble_index <- 2,  ensemble_index <- 7)

ensemble_accuracy <- confusionMatrix(factor(ensemble_new), mnist_27$test$y)$overall["Accuracy"]
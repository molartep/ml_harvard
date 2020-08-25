library(dslabs)

data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x

set.seed(1991)

fit <- train(x = x, y = y, method = "rpart", control = rpart.control(minsplit = 0), tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))

plot(fit)

fit$bestTune

confusionMatrix(fit)

plot(fit$finalModel)
text(fit$finalModel)

imp <- varImp(fit)
tree_terms <- as.character(unique(fit$finalModel$frame$var[!(fit$finalModel$frame$var == "<leaf>")]))
tree_terms
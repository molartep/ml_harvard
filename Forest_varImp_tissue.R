library(dslabs)

data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x

set.seed(1991)

fit <- train(x = x, y = y, method = "rf", nodesize = 1, tuneGrid = data.frame(mtry = seq(50, 200, 25)))

plot(fit)

fit$bestTune

imp <- varImp(fit)

sortImp(fit)

confusionMatrix(fit)

plot(fit$finalModel)
text(fit$finalModel)

modelLookup("rf")

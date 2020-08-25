library(dslabs)
data("tissue_gene_expression")

dat <- tissue_gene_expression

x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

fit <- train(x, y, method = "knn", tuneGrid = data.frame(k = seq(1, 300, 25)))
ggplot(fit)
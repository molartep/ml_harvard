if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("genefilter", version = "3.8")

library(genefilter)
tt <- colttests(x, y)

tt$p.value

ind <- which(tt$p.value <= 0.01)

length(ind)

x_subset <- x[,ind]

k = seq(101, 301, 25)

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)
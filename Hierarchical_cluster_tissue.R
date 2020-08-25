library(dslabs)
library(RColorBrewer)
library(matrixStats)
data("tissue_gene_expression")

d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x)) 

plot(hclust(d), labels = tissue_gene_expression$y, cex = 0.25)

k <- kmeans(tissue_gene_expression$x, centers = 7)
table(k$cluster, tissue_gene_expression$y)


sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row")

rev(colors)

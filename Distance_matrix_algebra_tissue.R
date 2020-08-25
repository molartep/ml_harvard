library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

mat <- tissue_gene_expression$x

d <- dist(tissue_gene_expression$x)

image(as.matrix(d))


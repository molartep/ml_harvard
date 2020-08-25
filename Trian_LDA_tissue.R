library(ggrepel)

set.seed(1993)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

train_lda <- train(x = x, y = y, method = "lda", preProcess = "center")

means <- data.frame(t(train_lda$finalModel$means)) 
means <- means %>% mutate(gene = as.factor(rownames(means)))

means %>% ggplot(aes(x = cerebellum, y = hippocampus, colour = gene, label = gene)) +
  ggtitle("LDA Means - Cerebellum vs Hippocampus") +
  geom_point() +
  geom_text_repel(aes(label=gene)) +
  theme(legend.position="none")  
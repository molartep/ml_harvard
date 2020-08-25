data("tissue_gene_expression")
dim(tissue_gene_expression$x)

x <- tissue_gene_expression$x
pca <- prcomp(x)

summary(pca)

frame <- rbind(data.frame(pca$x[1:38,1:2], Tissue = "cerebellum"), data.frame(pca$x[39:72,1:2], Tissue = "colon"), data.frame(pca$x[73:87,1:2], Tissue = "endometrium"),
               data.frame(pca$x[88:118,1:2], Tissue = "hippocampus"), data.frame(pca$x[119:157,1:2], Tissue = "kidney"), data.frame(pca$x[158:183,1:2], Tissue = "liver"),
               data.frame(pca$x[184:189,1:2], Tissue = "placenta"))

frame %>% 
  ggplot(aes(PC1,PC2, fill = Tissue)) +
  geom_point(cex = 3, pch = 21) +
  coord_fixed(ratio = 1)

data.frame(pca$x[,1:2], Tissue = tissue_gene_expression$y) %>% 
  ggplot(aes(PC1,PC2, fill = Tissue)) +
  geom_point(cex = 3, pch = 21) +
  coord_fixed(ratio = 1)

data.frame(pca$x[,1:2], avg = rowMeans(x), Tissue = tissue_gene_expression$y)%>% 
  ggplot(aes(avg, PC1, fill = Tissue)) +
  geom_point(cex = 3, pch = 21)

dat <- data.frame(pca$x[,1:2], avg = rowMeans(x), Tissue = tissue_gene_expression$y)

dat <- dat %>% select(PC1, avg) 
cor(dat)







x <- sweep(x, 1, rowMeans(tissue_gene_expression$x))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()







data.frame(pca$x[,1:7], Tissue = tissue_gene_expression$y)%>% 
  ggplot(aes(y = PC7, x = Tissue)) + geom_boxplot()





importance_df <- data.frame(summary(pca)$importance)
importance_df <- importance_df[2,] %>% 
  gather(key = pc, value = importance)
importance_df <- importance_df %>% mutate(pc_index = as.integer(str_remove(importance_df$pc, "PC")))
importance_df$pc <- factor(importance_df$pc, levels = importance_df$pc[order(importance_df$pc_index)])
importance_df <- importance_df %>% mutate(cum_sum = cumsum(importance))

importance_df %>% 
  filter(pc_index < 20) %>% 
  arrange(pc_index, cum_sum) %>% 
  ggplot(aes(x = pc, y = cum_sum, fill=pc)) +
  geom_col() +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  theme_grey()



library(broom)
library(dslabs)

mnist <- read_mnist()

dat <- mnist$train$images

y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)

qplot(as.factor(mnist$train$labels), y, geom = "boxplot")

mean(y)

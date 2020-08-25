library(caret)
library(ggplot2)
library(lattice)
library(dplyr)
library(purrr)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

test %>% group_by(Species) %>% ggplot(aes(x=Sepal.Length, y=Sepal.Width, color = Species)) + geom_point()
test %>% group_by(Species) %>% ggplot(aes(x=Petal.Length, y=Petal.Width, color = Species)) + geom_point()

y <- test$Species
cut <- seq(3,7,0.1)
cutw <- seq(1,2.5,0.1)


accuracy <- function(cutoff, cutoffw){
  y_hat <- ifelse(train$Petal.Width > cutoffw & train$Petal.Length > cutoff, "virginica", "versicolor") %>% factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
}

res <- sapply(cut, function(x) mapply(accuracy,x,cutw))

max(res)

y_hatn <- ifelse(test$Petal.Length>4.7 & test$Petal.Width>1.5 ,"virginica","versicolor") %>% factor(levels = levels(test$Species))
mean(y_hatn==test$Species)


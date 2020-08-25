library(caret)
library(dslabs)

params <- mnist_27$train %>% group_by(y) %>% summarize(avg_1 = mean(x_1), avg_2 = mean(x_2), sd_1 = sd(x_1), sd_2 = sd(x_2), corr = cor(x_1, x_2))

train_qda <- train(y ~ ., method = "qda", data = mnist_27$train)

y_hat <- predict(train_qda, mnist_27$test)

confusionMatrix(y_hat, mnist_27$test$y)$overall["Accuracy"]

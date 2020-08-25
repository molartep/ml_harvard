library(broom)
library(dslabs)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
qplot(x_2, y, data = mnist_27$train)

fit <- loess(formula = as.numeric(y) ~ x_2, data = mnist_27$train)

mnist_27$train %>% mutate(smooth = fit$fitted) %>% ggplot(aes(x = x_2, y = y)) + geom_point() + geom_line(aes(x_2, smooth), color = "red")



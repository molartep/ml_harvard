library(randomForest)
library(caret)
set.seed(1)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

y_hat <- train(y ~ x, method = "Rborist", data = dat, tuneGrid = data.frame(predFixed = 1, minNode = seq(25, 100, 25)))

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)
  
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)

plot(fit)

  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)
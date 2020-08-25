library(caret)
library(dplyr)

set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)

  test_index <- createDataPartition(y = dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat[-test_index,]
  test_set <- dat[test_index,]
  fit <- lm(y ~ x_1, train_set)
  fit1 <- lm(y ~ x_2, train_set)
  fit2 <- lm(y ~ x_1 + x_2, train_set)
  y_hat <- predict(fit, test_set)
  y_hat1 <- predict(fit1, test_set)
  y_hat2 <- predict(fit2, test_set)
  sqrt(mean((y_hat - test_set$y)^2 ))
  sqrt(mean((y_hat1 - test_set$y)^2 ))
  sqrt(mean((y_hat2 - test_set$y)^2 ))
  

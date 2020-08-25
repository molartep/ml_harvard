library(caret)
library(dplyr)

rmse <- function(n){
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

errors <- replicate(100, {
  test_index <- createDataPartition(y = dat$y, times = 1, p = 0.5, list = FALSE)
  test <- dat %>% slice(test_index)
  train <- dat %>% slice(-test_index)
  
  fit <- lm(formula = y ~ x, data = train)
  y_hat <- predict(fit, test)
  
  sqrt((mean((y_hat-test$y)^2)))
  
})

print(mean(errors))
print(sd(errors))
}

vec <- c(100, 500, 1000, 5000, 10000)
set.seed(1)
sapply(vec, rmse)

library(caret)
set.seed(2)
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}

delta <- seq(0, 3, len=25)

func <- function(z){
  
  dat <- make_data(mu_1 = z)
  fit <- glm(y ~ x, dat$train, family = "binomial")
  p_hat <- predict(fit, dat$test, type = "response")
  y_hat <- factor(ifelse(p_hat > 0.5, 1, 0))

  confusionMatrix(y_hat, dat$test$y)$overall[[1]]
}

accuracy <- sapply(delta, func)

df <- data.frame(delta, accuracy)
df %>% ggplot() + geom_point(aes(x=delta, y=accuracy))

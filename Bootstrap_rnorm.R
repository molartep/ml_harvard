set.seed(1)
y <- rnorm(100, 0, 1)

set.seed(1)

  indexes <- createResample(y, 10000, list = FALSE)

  list <- list(lapply(1:10000, function(x){y[indexes[,x]]}))
  
  qs <- lapply(list[[1]], quantile, 0.75)
  
  mean(as.numeric(qs))
  sd(as.numeric(qs))

qnorm(0.75)

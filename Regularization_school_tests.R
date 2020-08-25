library(dplyr)
library(ggplot2)
library(MLmetrics)

set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))

#Now let's assign a true quality for each school that is completely independent from size.
#This is the parameter we want to estimate in our analysis. The true quality can be assigned using the following code:

set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:100),
size = n,
quality = mu,
rank = rank(-mu))

#We can see the top 10 schools using this code: 

schools %>% top_n(10, quality) %>% arrange(desc(quality))

#Now let's have the students in the school take a test. 
#There is random variability in test taking,
#so we will simulate the test scores as normally distributed with the average determined by the school quality with a standard deviation of 30 percentage points.
#This code will simulate the test scores:
  
  set.seed(1)
scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

top_10 <- schools %>% top_n(10, score) %>% arrange(desc(score))
worst_10 <- schools %>% top_n(-10, score) %>% arrange(desc(score))

median(schools$size)
median(top_10$size)
median(worst_10$size)


plot <- schools %>% select(score, size, quality) %>% ggplot(aes(x = size, y = score)) + geom_point()

topq <- schools %>% top_n(10, quality)
plot + geom_point(data = topq, color = "red")

overall <- mean(sapply(scores, mean))

reg_schools <- schools %>% mutate(reg = overall + ((score - overall) * size / (size + 25))) %>% arrange(desc(reg))
reg_schools %>% top_n(10, reg)

alphas <- seq(10, 250)

rmses <- sapply(alphas, function(a){
  reg_scores <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+a))
  mean((reg_scores - schools$quality)^2)
  })


dat <- data.frame(rmse = rmses, alpha = alphas) 

dat %>% ggplot(aes(x = alpha, y = rmse)) + geom_point()

alphas[which.min(dat$rmse)]

opt_reg_schools <- schools %>% mutate(reg = overall + ((score - overall) * size / (size + 128))) %>% arrange(desc(reg))
opt_reg_schools %>% top_n(10, reg)


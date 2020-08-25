library(dslabs)
library(dplyr)
library(ggplot2)
library(lubridate)
data("movielens")

dat <- movielens

p <- dat %>% count(year)

plot <- p %>% ggplot(aes(x = year, y = sqrt(n))) + geom_point()

p$year[which.max(p$n)]

dat %>% filter(movieId == 318, !is.na(rating)) %>% .$rating %>% mean()

count <- dat %>% filter(title == "Forrest Gump") %>% count(year)

plotc <- count %>% ggplot(aes(x = year, y = sqrt(n))) + geom_point()


tbl <- movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years)

tbl %>%
  top_n(25, rate) %>%
  arrange(desc(rate)) 

tbl %>% ggplot(aes(x = rating, y = rate)) + geom_point()


movielens <- mutate(movielens, date = as_datetime(timestamp))

movielens$date <- round_date(movielens$date, "week")

movielens %>% group_by(date) %>% summarize(rating = mean(rating)) %>% ggplot(aes(x = date, y = rating)) + geom_point() + geom_smooth()




plot <- movielens %>%
  group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n)) %>%
  filter(n >= 1000) %>%
  arrange(desc(avg)) %>%
  ggplot (aes(x=genres, y=avg)) +
  geom_point() +
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=0.4, colour="red", alpha=0.8, size=1.3) +
  geom_bar(stat="identity", fill="skyblue", alpha=0.7) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


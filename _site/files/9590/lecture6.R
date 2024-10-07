###################################
## Code for POLSCI 9590 Week 5   ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

library(ggplot2)
library(dplyr)
library(rio)
library(tidyr)
library(uwo4419)

set.seed(2543)
pop <- runif(10000, 0, 1)


ggplot() + 
  geom_histogram(aes(x=pop), 
                 bins=25, 
                 col="white") + 
  theme_bw() + 
  labs(x="Population Distribution of X")


mean(pop)


sqrt(sum((pop - mean(pop))^2)/length(pop))

samp <- sample(pop, 100, replace=TRUE)
# sample mean
mean(samp)
# sample sd
sd(samp)


mean(samp) - mean(pop)


means <- replicate(1000, 
       mean(sample(pop, 100, replace=TRUE)))
samp.errors <- means - mean(pop)
ggplot() + 
  geom_histogram(aes(x=samp.errors), 
                 bins=25, 
                 col="white") + 
  theme_bw() + 
  geom_vline(xintercept=0, col="red") + 
  labs(x="Distribution of Sampling Errors")


ggplot() + 
  geom_histogram(aes(x=means), 
                 bins=25, 
                 col="white") + 
  theme_bw() + 
  geom_vline(xintercept=mean(pop), 
             col="red") + 
  labs(x="Distribution of Sample Means")





true.se <- sqrt(sum((pop - mean(pop))^2)/length(pop))/sqrt(100)
ggplot() +
  stat_function(data = data.frame(x = c(.35, .65)), 
                mapping = aes(x), 
                fun = dnorm, 
                n = 101, 
                args = list(mean = mean(pop), 
                            sd = true.se)) + 
  geom_point(aes(x=.45, y=0), col="red") + 
  ylab("") + 
  theme_bw()


pnorm(.45, mean(pop), true.se)


s <- seq(.35, .45, length=50)
ggplot() +
  stat_function(data = data.frame(x = c(.35, .65)), 
                mapping = aes(x), 
                fun = dnorm, 
                n = 101, 
                args = list(mean = mean(pop), 
                            sd = true.se)) + 
  geom_point(aes(x=.45, y=0), col="red") + 
  geom_polygon(aes(x=c(s, rev(s), s[1]), 
                   y=c(rep(0, length(s)), 
                       dnorm(rev(s), mean(pop), true.se), 
                       0), fill=rgb(1,0,0,.25)), show.legend = FALSE) + 
  ylab("") + 
  theme_bw()


confidenceInterval(samp, distr = "norm")
confidenceInterval(samp, distr = "t")


mean_cl_normal(samp)

ces <- import("ces19.dta")
confidenceInterval(ces$market)
x <- ces %>% 
  filter(!is.na(vote) & !is.na(market)) %>%
  mutate(vote = factorize(vote)) %>% 
  group_by(vote) %>% 
  summarise(ci = list(mean_cl_normal(market))) %>% 
  unnest(ci)
x


ggplot(x, aes(x=reorder(vote, -y, mean), y=y, 
              ymin=ymin, 
              ymax=ymax)) + 
  geom_pointrange() + 
  theme_classic() + 
  labs(x="Vote Choice in 2019 Federal Election", 
       y="Market Liberalism Scale")






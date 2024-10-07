###################################
## Code for POLSCI 9590 Week 6   ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

library(ggplot2)
library(dplyr)
library(rio)
library(DAMisc)
library(patchwork)

ggplot() +
  stat_function(data = data.frame(x = c(-2, 2)), 
                mapping = aes(x), 
                fun = dnorm, 
                n = 101, 
                args = list(mean = 0, 
                            sd = .75)) + 
  geom_segment(aes(x=-.95, xend=.95, y=dnorm(-1, sd=.75), yend=dnorm(1, sd=.75)), 
               arrow = arrow(angle=90, length=unit(.1, "inches"), ends="both")) + 
  annotate("text", x=.4, y=.25, label=expression(frac(sigma,sqrt(n)))) + 
  labs(x=expression(bar(x)), y="") + 
  theme_bw() + 
  scale_x_continuous(breaks=0, labels=expression(mu))




ggplot() +
  stat_function(data = data.frame(x = c(-2, 2)), 
                mapping = aes(x), 
                fun = dnorm, 
                n = 101, 
                args = list(mean = 0, 
                            sd = .75)) + 
  geom_segment(aes(x=-.95, xend=.95, y=dnorm(-1, sd=.75), yend=dnorm(1, sd=.75)), 
               arrow = arrow(angle=90, length=unit(.1, "inches"), ends="both")) + 
  annotate("text", x=.4, y=.25, label=expression(frac(plain(s)[x],sqrt(n)))) + 
  labs(x=expression(bar(x)), y="") + 
  theme_bw() + 
  scale_x_continuous(breaks=0, labels=expression(mu))




ggplot() +
  stat_function(data = data.frame(x = c(-2, 2)), 
                mapping = aes(x), 
                fun = dnorm, 
                n = 101, 
                args = list(mean = 0, 
                            sd = .75)) + 
  geom_segment(aes(x=-.95, xend=.95, y=dnorm(-1, sd=.75), yend=dnorm(1, sd=.75)), 
               arrow = arrow(angle=90, length=unit(.1, "inches"), ends="both")) + 
  annotate("text", x=.4, y=.25, label=expression(frac(plain(s)[x],sqrt(n)))) + 
  labs(x=expression(bar(x)), y="") + 
  theme_bw() + 
  scale_x_continuous(breaks=0, labels=expression(mu[0]))


dxy <- tibble(x=seq(-1, 1.25, length=100), 
              y=dt(x/.3, 99))

df0 <- tibble(x = seq(0.5, 1.25, length=100), 
                 y = dt(x/.3, df=99))
otg <- ggplot() +
  geom_line(data=dxy, aes(x=x, y=y)) + 
  geom_polygon(mapping=aes(x=c(df0$x, rev(df0$x), df0$x[1]), 
                            y=c(rep(0, 100), rev(df0$y), 0)), 
               fill="red", 
               alpha=.25) + 
  annotate("text", x=1, y=.05, label=expression(p(T>t)==0.049)) + 
  theme_bw() + 
  theme(aspect.ratio=1) + 
  labs(x= expression(bar(x)), y="") + 
  ggtitle(expression(plain("One tailed:")~plain("H:")[0]~mu>0)) 

df <- tibble(x = seq(-1,.5, length=100),
             y = dt(x/.3, df=99))
otl <- ggplot() +
  geom_line(data=dxy, aes(x=x, y=y)) + 
  geom_polygon(mapping=aes(x=c(df$x, rev(df$x), df$x[1]),
                           y=c(rep(0, 100), rev(df$y), 0)),
                fill="blue",
                alpha=.25) +
  annotate("text", x=1, y=.05, label=expression(p(T<t)==0.951)) +
  theme_bw() +
  theme(aspect.ratio=1) + 
  labs(x= expression(bar(x)), y="") +
  ggtitle(expression(plain("One tailed:")~plain("H:")[0]~mu<0)) 



df1 <- tibble(x = seq(-1,-.5, length=100), 
                 y = dt(x/.3, df=99))
df2 <- tibble(x = seq(.5, 1.25, length=100), 
                 y = dt(x/.3, df=99))
ott <- ggplot() +
  geom_line(data=dxy, aes(x=x, y=y)) + 
  geom_polygon(mapping=aes(x=c(df1$x, rev(df1$x), df1$x[1]), 
                            y=c(rep(0, 100), rev(df1$y), 0)), 
               fill="green", 
               alpha=.25) + 
  geom_polygon(mapping=aes(x=c(df2$x, rev(df2$x), df2$x[1]), 
                            y=c(rep(0, 100), rev(df2$y), 0)), 
               fill="green", 
               alpha=.25) + 
  annotate("text", x=0, y=.05, label=expression(p(symbol("|")~T~symbol("|")<symbol("|")~t~symbol("|"))==0.099)) + 
  theme_bw() + 
  theme(aspect.ratio=1) + 
  labs(x= expression(bar(x)), y="") + 
  ggtitle(expression(plain("Two tailed:")~plain("H:")[0]~mu!=0))

gridExtra::grid.arrange(otl, ott, otg, nrow=1)


set.seed(519)
x <- scale(rnorm(100, 0, 1))
x <- x*3 + .5
t.test(x, mu = 0, alternative="two")

t.test(x, mu = 0, alternative="less")


t.test(x, mu = 0, alternative="greater")


prop.test(x=110, n=250, p=.5)

ces <- import("ces19.dta")
ces <- ces %>% mutate(
  vote_con = case_when(vote == 2 ~ 1, 
                       vote %in% c(1,3,4) ~ 0, 
                       TRUE ~ NA_real_))


tTest("vote_con", "market", data=ces, var.equal=FALSE)


ces <- ces %>% mutate(
  coll_grad = case_when(educ == 3 ~ 1, 
                        educ %in% 1:2 ~ 0, 
                        TRUE ~ NA_real_))
s <- ces %>% 
  # group by the independent variable
  group_by(coll_grad) %>% 
  filter(!is.na(coll_grad) & !is.na(vote_con)) %>% 
  # summarise the dependent variable
  summarise(n_con = sum(vote_con, na.rm=TRUE), 
            n = n()) 


s
prop.test(s$n_con, s$n)


ces %>% 
  mutate(vc = factor(vote_con, 
                     levels=c(0,1), 
                     labels=c("No", "Yes"))) %>% 
  filter(!is.na(vc)) %>% 
ggplot(aes(x=vc, y=market)) + 
  geom_boxplot(width=.25) + 
  theme_classic() + 
  labs(x="Vote for Conservative", 
       y="Market Liberalism Scale")





ces %>% select(coll_grad, vote_con) %>% 
  na.omit %>% 
  group_by(coll_grad, vote_con) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(coll_grad) %>% 
  mutate(prop = n/sum(n)) %>% 
ggplot(aes(x=factor(coll_grad, labels=c("No", "Yes")), 
           y=prop, 
           fill=factor(vote_con, labels=c("No", "Yes")))) +
  geom_bar(stat="identity", position="stack") + 
  theme_bw() + 
  labs(x="College Graduate", 
       y="Proportion", 
       fill="Vote Conservative?") + 
  scale_y_continuous(labels=scales::label_percent())





d <- ces %>% select(coll_grad, vote_con) %>% 
  na.omit %>% 
  group_by(coll_grad, vote_con) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(coll_grad) %>% 
  mutate(prop = n/sum(n)) 
p1 <- ggplot(d, aes(x=factor(coll_grad, labels=c("No", "Yes")), 
           y=prop, 
           fill=factor(vote_con, labels=c("No", "Yes")))) +
  geom_bar(stat="identity", position="stack") + 
  theme_bw() + 
  theme(legend.position="bottom") + 
  facet_wrap(~"A") + 
  labs(x="College Graduate", 
       y="Proportion", 
       fill="Vote Conservative?") + 
  scale_y_continuous(labels=scales::label_percent())

p2 <- ggplot(d %>% filter(vote_con == 1), 
             aes(x=factor(coll_grad, labels=c("No", "Yes")), 
                 y=prop)) +
  geom_bar(stat="identity") + 
  theme_bw() + 
  facet_wrap(~"B") + 
  labs(x="College Graduate", 
       y="Proportion\nVoting Conservative") + 
  scale_y_continuous(labels=scales::label_percent())


p3 <- ggplot(d %>% filter(vote_con == 1), 
             aes(x=factor(coll_grad, labels=c("No", "Yes")), 
                 y=prop)) +
  geom_point() + 
  geom_segment(aes(xend=factor(coll_grad, labels=c("No", "Yes")), yend=0)) + 
  theme_bw() + 
  facet_wrap(~"C") + 
  labs(x="College Graduate", 
       y="Proportion\nVoting Conservative") + 
  scale_y_continuous(labels=scales::label_percent())

p1 + p2 + p3



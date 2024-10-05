###################################
## Code for POLSCI 9590 Week 9   ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

library(tidyverse)
library(rio)
library(plotly)
library(marginaleffects)

dat = rio::import("example1.dta")
ggplot(dat, 
       mapping=aes(x=x, y=y)) + 
  geom_point(pch=1) +  
  theme_bw() + 
  theme(aspect.ratio=1) 

x <- dat$x
y <- dat$y
a <- seq(1.75, 2.25, length=100)
b <- seq(2.5, 3.5, length=100)
eg <- expand.grid(a=a, b=b)
yhats <- sapply(1:nrow(eg), function(i)eg[i,1] + eg[i,2]*x)
rss <- apply(yhats, 2, function(x)sum((x-y)^2))
eg$rss <- rss
fitmat <- matrix(eg$rss, 
                 nrow=length(a), 
                 ncol=length(b))
plot_ly(x = ~a, y=~b, z=~fitmat) %>% 
  add_surface(colorbar=list(title="RSS"))

x <- dat$x
y <- dat$y
mod <- lm(y ~ x)
g <- ggplot(eg, aes(x=a, y=b, z=rss)) + 
  geom_contour(breaks = quantile(eg$rss, c(.0001, .001, .01, .05, .1,.2,.3,.4,.5,.6)), col="black") + 
  geom_hline(yintercept=mod$coef[2], col="red") +
  geom_vline(xintercept=mod$coef[1], col="red") +
  theme_bw()
gp1 <- ggplotly(g)
gp1

mod <- lm(y ~ x)
summary(mod)

ggplot(dat, aes(x=x, y=y)) + 
  geom_point(shape=1) + 
  geom_smooth(method="lm", 
              col="black") + 
  theme_classic() + 
  labs(x="x", y="y")

demo <- rio::import("demo.dta")
demo <- rio::factorize(demo)
dmod1 <- lm(demodays ~ corrupt, data=demo)
summary(dmod1)

s <- sd(demo$corrupt, na.rm=TRUE)
s*0.32


.79/sd(demo$demodays, na.rm=TRUE)

plot_predictions(dmod1,
                 condition="corrupt", 
                 points=1) + 
  theme_classic()

df <- import("cat_example.dta")
df <- factorize(df)
df %>% group_by(x) %>% 
  summarise(m = mean(y), 
            s = sd(y), 
            n = n())


xmod <- lm(y ~ x, data=df)
summary(xmod)

avg_predictions(xmod, variables="x")
avg_predictions(xmod, variables="x", hypothesis = "pairwise")

plot_predictions(xmod, condition="x") + 
  geom_point(data=df, aes(x=x, y=y), position=position_jitter(width=.15), color="gray50", alpha=.25, inherit.aes = FALSE) + 
  theme_classic()

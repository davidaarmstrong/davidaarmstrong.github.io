###################################
## Code for POLSCI 9592 Week 1   ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

## ----setup, include=FALSE---------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(maxLik)

## ----binom_ex, echo=FALSE, fig.height=5, fig.width=10, out.width="80%",fig.align="center"-----------------------
x <- data.frame(
    prob = c(dbinom(0:4, 4, .22), dbinom(0:4, 4, .8)), 
    vals = c(0:4,0:4), 
    p = rep(c(.22, .8), each=5)
)
library(ggplot2)
ggplot(x, aes(x=vals, y=prob)) + geom_bar(stat="identity") + facet_wrap(~p) + theme_bw()


## ----binom_ex2, echo=FALSE, fig.height=6, fig.width=6, out.width="45%", fig.align="center"----------------------
s <- seq(0.01, .99, length=250)
p <- dbinom(1, 4, s)
x2 <- data.frame(
    s=s, p=p)
ggplot(x2, aes(x=s, y=p)) + geom_line() + theme_bw() + labs(x="p", y="Likelihood")


## ----binom_ex3, echo=FALSE, fig.height=6, fig.width=6, out.width="100%", fig.align="center"---------------------
s <- seq(0.01, .99, length=250)
p1 <- dbinom(1, 4, s)
p2 <- dbinom(2, 4, s)
x2 <- data.frame(
    s=s, p=c(p1, p2), point = factor(rep(c(1,2), each=250)))
ggplot(x2, aes(x=s, y=p, colour=point)) + geom_line() + theme_bw() + labs(x="p", y="Likelihood")



## ----binom_ex4, echo=FALSE, fig.height=6, fig.width=6, out.width="100%", fig.align="center"---------------------
s <- seq(0.01, .99, length=250)
p1 <- dbinom(1, 4, s)
p2 <- dbinom(10, 40, s)
p3 <- dbinom(100, 400, s)
x2 <- data.frame(
    s=s, p=c(p1, p2,p3), point = factor(rep(c(1,2,3), each=250), labels=c("n=4", "n=40", "n=400")))
ggplot(x2, aes(x=s, y=p, colour=point)) + geom_line() + theme_bw() + labs(x="p", y="Likelihood")


## ----normal_pdf, echo=FALSE, fig.width=6, fig.height=6, fig.align="center"--------------------------------------
s <- seq(-3, 3, length=100)
p <- dnorm(s, 0, 1) 
df <- data.frame(s = s, p = p)
ggplot(df, aes(x=s, y=p)) + geom_line() + theme_bw() + labs(x="x", y="Density")  + ggtitle("Normal PDF")


## ----normal_cdf, echo=FALSE, fig.width=6, fig.height=6, fig.align="center"--------------------------------------
s <- seq(-3, 3, length=100)
p <- pnorm(s, 0, 1) 
df <- data.frame(s = s, p = p)
ggplot(df, aes(x=s, y=p)) + geom_line() + theme_bw() + labs(x="x", y="Probability < x")  + ggtitle("Normal CDF")


## ----binomial_pmf, echo=FALSE, fig.width=6, fig.height=6, out.width="80%", fig.align="center"-------------------
s <- 0:5
p <- dbinom(s, 5, .3)
df <- data.frame(s = s, p = p)
ggplot(df, aes(x=s, y=p, xend = s, yend=0)) + geom_point() + geom_segment() + theme_bw() + labs(x="x", y="Density")  + ggtitle("Binomial PMF")


## ----binomial_cdf, echo=FALSE, fig.width=6, fig.height=6, out.width="80%", fig.align="center"-------------------
s <- 0:5
p <- pbinom(s, 5, .3)
df <- data.frame(s = s, p = p)
ggplot(df, aes(x=s, y=p, xend = s, yend=0)) + geom_point() + geom_segment() + theme_bw() + labs(x="x", y="Density") + ggtitle("Binomial CDF")


## ----binom_ex3a, echo=FALSE, fig.height=6, fig.width=6, out.width="100%", fig.align="center"--------------------
s <- seq(0.01, .99, length=250)
p1 <- dbinom(1, 4, s)
p2 <- dbinom(2, 4, s)
x2 <- data.frame(
    s=s, p=p1*p2)
ggplot(x2, aes(x=s, y=p)) + geom_line() + theme_bw() + labs(x = "p", y="Likelihood")


## ----binom_ex3b, echo=FALSE, fig.height=6, fig.width=6, out.width="100%", fig.align="center"--------------------
s <- seq(0.01, .99, length=250)
p1 <- dbinom(1, 4, s)
p2 <- dbinom(2, 4, s)
x2 <- data.frame(
    s=s, p=log(p1) + log(p2))
ggplot(x2, aes(x=s, y=p)) + geom_line() + theme_bw() + labs(x = "p", y = "Log-likelihood")


## ---------------------------------------------------------------------------------------------------------------
library(maxLik)
llfun <- function(par, x){
    p <- dbinom(x, 4, par[1])
    sum(log(p))
}
out <- maxLik(llfun, start=.5, x=c(1,2))
summary(out)



## ----binom_ex4a, echo=FALSE, fig.height=6, fig.width=6, fig.align="center"--------------------------------------
s <- seq(0.01, .99, length=250)
p1 <- dbinom(1, 4, s)
p2 <- dbinom(2, 4, s)
x2 <- data.frame(
    s=s, p=p1*p2)
ggplot(x2, aes(x=s, y=p)) + geom_line() + theme_bw() + geom_vline(xintercept=0.375) + labs(x = "p", y="Likelihood")


## ----binom_ex4b, echo=FALSE, fig.height=6, fig.width=6, fig.align="center"--------------------------------------
s <- seq(0.01, .99, length=250)
p1 <- dbinom(1, 4, s)
p2 <- dbinom(2, 4, s)
x2 <- data.frame(
    s=s, p=log(p1) + log(p2))
ggplot(x2, aes(x=s, y=p)) + geom_line() + theme_bw() + geom_vline(xintercept=0.375) + labs(x = "p", y = "Log-likelihood")


## ---------------------------------------------------------------------------------------------------------------
library(maxLik)
llfun <- function(par, x){
    p <- dbinom(x, 4, par[1])
    sum(log(p))
}
out <- maxLik(llfun, start=.5, x=c(1,2,1,2,1,2,1,2,1,2))
summary(out)


## ----lln, echo=FALSE, fig.height=6, fig.width=6, out.width="100%", fig.align="center"---------------------------
s <- seq(0.01, .99, length=250)
p1 <- dbinom(1, 4, s)
p2 <- dbinom(2, 4, s)

ll2 <- log(p1) + log(p2)
ll10 <- log(p1)*5 + log(p2)*5
df <- data.frame(p=s, ll = c(ll2, ll10), n = factor(rep(1:2, each=250), labels=c("n=2", "n=10")))
ggplot(df, aes(x=p, y=ll, colour=n)) + geom_line() + theme_bw() + labs(x = "p", y = "Log-likelihood")


## ----echo=TRUE, eval=FALSE--------------------------------------------------------------------------------------
## library(googlesheets4)
## library(tidyr)
## g <- read_sheet("https://docs.google.com/spreadsheets/d/1GkCSXFtdw9NSwNe9kZJqWO_F4yaMtQZSS43v1GwxSeI/edit?gid=0#gid=0")
## g.long <- g %>% pivot_longer(-Trial, names_to = "person", values_to = "evens")
## out1 <- maxLik(llfun, start=.25, x=g.long$evens)
## summary(out1)


## ----echo=T, eval=FALSE-----------------------------------------------------------------------------------------
## llfun <- function(par, x, group){
##     p <- dbinom(x, 4, par[group])
##     sum(log(p))
## }
## g <- as.numeric(as.factor(g.long$person))
## 
## out <- maxLik(llfun, start=rep(.5, 10), x=g.long$evens, group=g)
## summary(out)


## ----echo=TRUE, eval=FALSE--------------------------------------------------------------------------------------
## lr <- -2*(logLik(out1) - logLik(out))
## pchisq(lr, 9, lower.tail=FALSE)


## ---------------------------------------------------------------------------------------------------------------
data(Prestige, package="carData")
Prestige <- na.omit(Prestige)
summary(lm(prestige ~ education, data=Prestige))


## ---------------------------------------------------------------------------------------------------------------
X <- cbind(1, Prestige$education)
y <- matrix(Prestige$prestige, ncol=1)
llfun <- function(par, X, y, ...){
   n <- nrow(X)
   b <- par[1:2]
   yhat <- X %*% b
   sig2 <- par[3]
   sum(dnorm(y-yhat, 0, sqrt(exp(sig2)), log=TRUE))
}
lm_mle <- maxLik(llfun, X=X, y=y, start=c(0,0,1), tol=1E-15)
summary(lm_mle)


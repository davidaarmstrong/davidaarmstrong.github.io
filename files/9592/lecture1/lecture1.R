# Lecture 1: Maximum Likelihood Estimation
# R code accompanying the London mayoral election / binomial MLE example

library(car)
library(tibble)
library(ggplot2)
library(xtable)
library(dplyr)
library(tidyr)
library(maxLik)

## ---------------------------------------------------------------------------------------------
## P(N^M >= 52 | N=100, p=.5): probability of a result this favorable to Morgan (or more),
## if the race were truly split 50/50 -- "at least as extreme as the one we found"
pbinom(51, 100, .5, lower.tail=FALSE)


## ----echo=FALSE, fig.height=4, fig.width=10, out.width="75%", fig.align="center"--------------
## Compare two candidate guesses for p (.48 vs .6): which one makes the observed
## count of 52 more probable? (highlighted bar shows x = 52 for both candidates)
tibble(x=rep(45:60, 2),
       p = rep(c(.48, .6), each=16),
       f = dbinom(x, 100, p)) |>
  mutate(bright = ifelse(x == 52, "Yes", "No"),
         x = as.factor(x)) |>
  ggplot(aes(x=x, y=f, fill=as.factor(p), alpha=bright)) +
  geom_bar(stat="identity", position="dodge") +
  theme_minimal() +
  scale_alpha_manual(values=c(.25, 1)) +
  guides(alpha="none") +
  labs(x="Number of Votes for Morgan", y="Binomial PMF",
       fill = "Binomial Probability Guess") +
  theme(legend.position = "top")



## ----echo=FALSE, include=TRUE, out.width="90%", fig.align="center"----------------------------
## Now sweep over ALL possible values of p: this curve -- the binomial PMF evaluated
## at the observed count (52) as a function of p -- is the likelihood function.
## Its peak (dashed line) sits at p = .52, the sample proportion.
tibble(p = seq(0,1, by=.001),
       f = dbinom(52, 100, p)) |>
  ggplot(aes(x=p, y=f)) +
  geom_line() +
  theme_minimal() +
  geom_vline(xintercept=.52, color="gray50", linetype="dashed") +
  labs(x="Binomial Probability", y="f(52 | N=100, p)")



## ----echo=FALSE, include=TRUE, out.width="90%", fig.align="center"----------------------------
## Same plot, shown again on the slide after revealing "we just did maximum likelihood estimation!"
tibble(p = seq(0,1, by=.001),
       f = dbinom(52, 100, p)) |>
  ggplot(aes(x=p, y=f)) +
  geom_line() +
  theme_minimal() +
  geom_vline(xintercept=.52, color="gray50", linetype="dashed") +
  labs(x="Binomial Probability", y="f(52 | N=100, p)")



## ----normal_pdf, echo=FALSE, fig.width=6, fig.height=6, fig.align="center"--------------------
## Standard normal PDF
s <- seq(-3, 3, length=100)
p <- dnorm(s, 0, 1)
df <- data.frame(s = s, p = p)
ggplot(df, aes(x=s, y=p)) + geom_line() + theme_bw() + labs(x="x", y="Density")  + ggtitle("Normal PDF")


## ----normal_cdf, echo=FALSE, fig.width=6, fig.height=6, fig.align="center"--------------------
## Standard normal CDF
s <- seq(-3, 3, length=100)
p <- pnorm(s, 0, 1)
df <- data.frame(s = s, p = p)
ggplot(df, aes(x=s, y=p)) + geom_line() + theme_bw() + labs(x="x", y="Probability < x")  + ggtitle("Normal CDF")


## ----binomial_pmf, echo=FALSE, fig.width=6, fig.height=6, out.width="80%", fig.align="center"----
## Binomial PMF, n=5, p=.3 -- illustrates a discrete distribution
s <- 0:5
p <- dbinom(s, 5, .3)
df <- data.frame(s = s, p = p)
ggplot(df, aes(x=s, y=p, xend = s, yend=0)) + geom_point() + geom_segment() + theme_bw() + labs(x="x", y="Density")  + ggtitle("Binomial PMF")


## ----binomial_cdf, echo=FALSE, fig.width=6, fig.height=6, out.width="80%", fig.align="center"----
## Binomial CDF, n=5, p=.3
s <- 0:5
p <- pbinom(s, 5, .3)
df <- data.frame(s = s, p = p)
ggplot(df, aes(x=s, y=p, xend = s, yend=0)) + geom_point() + geom_segment() + theme_bw() + labs(x="x", y="Density") + ggtitle("Binomial CDF")


## ----echo=FALSE, include=TRUE, fig.width=8, fig.height=4, out.width="90%", fig.align="center"----
## Log-likelihood (l) and score (S) functions of p, given 52 successes out of 100.
## The score crosses zero exactly where the log-likelihood peaks (p = .52).
tibble(p = seq(0,1, by=.01),
      l = dbinom(52, 100, p, log=TRUE),
      S = 52/p - (100-52)/(1-p)) |>
  pivot_longer(l:S, names_to = "fn", values_to = "val") |>
  ggplot(aes(x=p, y=val)) +
  geom_line() +
  facet_wrap(~fn, ncol=2, scales="free_y") +
  theme_bw() +
  labs(x="Binomial Probability", y="Value")


## ---------------------------------------------------------------------------------------------
## Log-likelihood function for a single binomial probability p, given data x out of n trials
llfun <- function(par, x, n=100){
    p <- dbinom(x, n, par[1], log=TRUE)
    sum(p)
}
## Numerically find the MLE of p for x=52, n=100 (should match k/n = .52 below)
out <- maxLik(llfun, start=.5, x=52)
summary(out)



## ---------------------------------------------------------------------------------------------
# k/n: closed-form MLE
52/100

# sqrt([p(1-p)]/n): closed-form standard error, using p-hat in place of the unknown p

sqrt((.52*(1-.52))/100)


## ---------------------------------------------------------------------------------------------
## Same proportion (520/1000 = .52) but 10x the sample size --
## same point estimate, smaller standard error (see the SE calc above vs. the summary here)
out <- maxLik(llfun, start=.5, x=520, n=1000)
summary(out)


## ----lln, echo=FALSE, fig.height=6, fig.width=6, out.width="100%", fig.align="center"---------
## Compare the log-likelihood curves for n=100 vs n=1000: same peak location (p=.52),
## but the n=1000 curve is much more peaked -- more information, lower sampling variance
s <- seq(0.01, .99, length=250)
p1 <- dbinom(52, 100, s, log=TRUE)
p2 <- dbinom(520, 1000, s, log=TRUE)

df <- data.frame(p=c(s, s), ll = c(p1, p2), n = factor(rep(c(100, 1000), each=250), labels=c("n=100", "n=1000")))
ggplot(df, aes(x=p, y=ll, colour=n)) + geom_line() + theme_bw() + labs(x = "p", y = "Log-likelihood", colour="") + theme(legend.position = "top")


## ---------------------------------------------------------------------------------------------
## What if renters and owners have different probabilities of voting for Morgan?
## Log-likelihood allowing a separate p for each group (par is a vector: one p per group)
llfun2 <- function(par, x, n){
  l <- dbinom(x, n, par, log=TRUE)
  sum(l)
}
## Fit the unrestricted (two-parameter) model: renter and owner estimated separately
out2 <- maxLik(llfun2,
               start=c(renter=.5, owner=.5),
               x=c(20, 32), n=c(30, 70))

summary(out2)


## ----echo=TRUE----------------------------------------------------------------------------------
## Restricted model: renters and owners share a single common probability (par[1] used for both)
llfun1 <- function(par, x, n){
   l <- dbinom(x, n, c(par[1],par[1]), log=TRUE)
   sum(l)
}
out1 <- maxLik(llfun1, x=c(20,32), start=c(both = .5), n = c(30,70))

## Likelihood ratio test: does letting renters and owners differ improve the fit
## enough to justify the extra parameter?
lmtest::lrtest(out1, out2)

## POLSCI 9592, Lecture 7: Count Data Models
## Code for the slides in lecture7.Rmd

library(rio)
library(dplyr)
library(tidyr)
library(ggplot2)
library(marginaleffects)
library(DAMisc)
library(pscl)

# make sure select() is dplyr's, even if another package masks it
select <- function(...)dplyr::select(...)


## ---- Example ----

# number of voluntary organizations people belong to;
# factorize() turns labelled values into factors
dat <- import("data/count.dta")
dat <- dat %>%
  mutate(across(c("unemployed", "religimp"), factorize))

mod <- glm(volorgs ~ age + educ + unemployed + hhincome_num +
             leftright + numkids + religimp,
           data=dat,
           family=poisson)
summary(mod)


## ---- Illustration ----

# a hypothetical respondent with 11, 12, 16 and 17 years of education
tmpdf <- data.frame(
    age = 45,
    educ = c(11,12,16,17),
    unemployed = factor(0, levels=c(0,1), labels=levels(dat$unemployed)),
    hhincome_num = 15,
    leftright = 5,
    numkids = 0,
    religimp = factor(1, levels=0:1, labels=levels(dat$religimp)))
preds <- predict(mod, newdata=tmpdf, type="response")
preds

# one more year of education multiplies the expected count by
# exp(b_educ), whatever the starting point
preds[2]/preds[1]
preds[4]/preds[3]


## ---- Interpretation II ----

# the same effect as a percentage change: 100*(exp(b_educ) - 1)
100*(preds[2] - preds[1])/preds[1]
100*(preds[4] - preds[3])/preds[3]


## ---- Discrete Changes (MERs) ----

# changes in the expected count with everything else held at
# its median (mode for factors)
comparisons(mod,
            newdata = "median",
            variables=list(
              age = "2sd",
              educ = c(12,16),
              unemployed= "minmax",
              hhincome_num = "2sd",
              leftright = c(2,8),
              numkids =c(0,2),
              religimp = "minmax")) %>%
  select(1:7)


## ---- Discrete Changes (AMEs) ----

# the same changes, averaged over everyone in the sample
avg_comparisons(mod,
            variables=list(
              age = "2sd",
              educ = c(12,16),
              unemployed= "minmax",
              hhincome_num = "2sd",
              leftright = c(2,8),
              numkids =c(0,2),
              religimp = "minmax")) %>%
  select(1:6)


## ---- Effects Plot ----

# expected count across education, other variables at their medians
preds <- predictions(mod,
            newdata="median",
            variables = list(educ = 1:17))
ggplot(preds,
       aes(x=educ, y=estimate,
           ymin=conf.low,
           ymax=conf.high)) +
  geom_ribbon(fill="gray75") +
  geom_line() +
  theme_classic() +
  labs(x="Years of Education",
       y="Expected Number of Voluntary Organizations")


## ---- Effects Plot (AME) ----

# expected count across education, averaged over the sample
apreds <- avg_predictions(mod,
            variables = list(educ = 1:17))
ggplot(apreds,
       aes(x=educ, y=estimate,
           ymin=conf.low,
           ymax=conf.high)) +
  geom_ribbon(fill="gray75") +
  geom_line() +
  theme_classic() +
  labs(x="Years of Education",
       y="Expected Number of Voluntary Organizations")


## ---- Model Fit ----

poisfit(mod)


## ---- Predicted vs. Actual (Poisson) ----

# observed share of each count against the average predicted
# probability of that count, (1/n) sum_i Pr(y = k | x_i)
yhat <- predict(mod, type="response")
y <- model.response(model.frame(mod))
k <- 0:max(y)
fitdat <- tibble::tibble(
  val = k,
  observed = sapply(k, function(j)mean(y == j)),
  predicted = sapply(k, function(j)mean(dpois(j, yhat)))
) %>%
  pivot_longer(-val, names_to="type", values_to="prop")
ggplot(fitdat,
       aes(x=val, y=prop, fill=type)) +
  geom_col(position="dodge") +
  theme_classic() +
  scale_fill_brewer(palette="Set1") +
  labs(x="Voluntary Organizations", y="Proportion", fill="") +
  theme(legend.position = "top")


## ---- NBRM Example ----

# Theta is the inverse of the overdispersion: var(y) = mu + mu^2/theta
mod2 <- MASS::glm.nb(volorgs ~ age + educ + unemployed +
                 hhincome_num + leftright + numkids +
                 religimp,
               data=dat)
summary(mod2)


## ---- Predicted vs. Actual (NB) ----

yhat <- predict(mod2, type="response")
y <- model.response(model.frame(mod2))
k <- 0:max(y)
fitdat <- tibble::tibble(
  val = k,
  observed = sapply(k, function(j)mean(y == j)),
  predicted = sapply(k, function(j)
    mean(dnbinom(j, mu=yhat, size=mod2$theta)))
) %>%
  pivot_longer(-val, names_to="type", values_to="prop")
ggplot(fitdat,
       aes(x=val, y=prop, fill=type)) +
  geom_col(position="dodge") +
  theme_classic() +
  scale_fill_brewer(palette="Set1") +
  labs(x="Voluntary Organizations", y="Proportion", fill="") +
  theme(legend.position = "top")


## ---- Hurdle Model: Estimation ----

# a logit for zero vs. positive, then a zero-truncated NB for the
# positive counts; without "|" in the formula both parts use
# the same variables
mod <- hurdle(volorgs ~ age + educ + unemployed + hhincome_num +
             leftright + numkids + religimp,
           data=dat,
           dist="negbin",
           zero.dist = "binomial")
summary(mod)


## ---- Hurdle Model: Effect of Unemployment ----

# "zero": Pr(y > 0) from the hurdle relative to Pr(y > 0) from the
# count model (1 = the hurdle adds nothing)
avg_predictions(mod, variables="unemployed",
                type="zero") %>%
  as_tibble() %>%
  select(unemployed, estimate, conf.low, conf.high)

# "count": mean of the untruncated count distribution
avg_predictions(mod, variables="unemployed",
                type="count") %>%
  as_tibble() %>%
  select(unemployed, estimate, conf.low, conf.high)

# "prob": probability of each count; the first two rows are Pr(y = 0)
avg_predictions(mod, variables="unemployed",
                type="prob") %>%
  head(n=2) %>%
  as_tibble() %>%
  select(unemployed, group, estimate, conf.low, conf.high)

# "response": E(y), Pr(y > 0) times the truncated count mean
avg_predictions(mod, variables="unemployed",
                type="response") %>%
  as_tibble() %>%
  select(unemployed, estimate, conf.low, conf.high)


## ---- Zero-Inflated Models ----

# zeros come from an always-zero group (logit) or from the NB itself
mod <- zeroinfl(volorgs ~ age + educ + unemployed + hhincome_num +
             leftright + numkids + religimp,
           data=dat,
           dist="negbin",
           link = "logit")
summary(mod)


## ---- Zero-Inflated Models: Effect of Unemployment ----

# "zero": probability of being in the always-zero group
avg_predictions(mod, variables="unemployed",
                type="zero") %>%
  as_tibble() %>%
  select(unemployed, estimate, conf.low, conf.high)

# "count": mean of the NB part
avg_predictions(mod, variables="unemployed",
                type="count") %>%
  as_tibble() %>%
  select(unemployed, estimate, conf.low, conf.high)

# "prob": the first two rows are Pr(y = 0)
avg_predictions(mod, variables="unemployed",
                type="prob") %>%
  head(n=2) %>%
  as_tibble() %>%
  select(unemployed, group, estimate, conf.low, conf.high)

# "response": E(y) = (1 - Pr(always zero)) * count mean
avg_predictions(mod, variables="unemployed",
                type="response") %>%
  as_tibble() %>%
  select(unemployed, estimate, conf.low, conf.high)


## ---- Example: Modeling Manifestos ----

# Comparative Manifestos Project: per201 is the percentage of sentences
# on freedom and human rights; convert it to a count
man <- import("data/man2014.dta")
man$num201 <- floor((man$per201/100)*man$total)


## ---- Histogram of Statements ----

ggplot(man, aes(x=num201)) + geom_histogram() + theme_classic() +
  labs(x="Human Rights Statements")


## ---- Poisson Model ----

# the same complete cases for every manifesto model
tmp <- na.omit(man[,c("num201", "total", "rile")])

# without exposure
mod <- glm(num201 ~ rile,
           data=tmp,
           family=poisson)

# with exposure: offset(log(total)) models the rate per sentence
mode0 <- glm(num201 ~ 1 + offset(log(total)),
             data=tmp,
             family=poisson)
mode <- glm(num201 ~ 1 + rile + offset(log(total)),
             data=tmp,
             family=poisson)
AIC(mod, mode0, mode)

# average predicted probability of each count from 0 to 74
obs <- model.response(model.frame(mod))
k <- 0:74
avgp <- function(mu)sapply(k, function(j)mean(dpois(j, mu)))
fitdat <- tibble::tibble(
  model = rep(c("Poisson", "Poisson Null (Exposure)", "Poisson Exposure"),
              each = length(k)),
  val = rep(k, 3),
  observed = rep(sapply(k, function(j)mean(obs == j)), 3),
  predicted = c(avgp(predict(mod, type="response")),
                avgp(predict(mode0, type="response")),
                avgp(predict(mode, type="response")))) %>%
  pivot_longer(observed:predicted, names_to="type", values_to="prop")
ggplot(fitdat,
       aes(x=val, y=prop, fill=type)) +
  geom_col(position="dodge") +
  facet_wrap(~model, ncol=2) +
  theme_bw() +
  scale_fill_brewer(palette="Set1") +
  labs(x="Number of Statements", y="Proportion", fill="") +
  theme(legend.position = c(.825, .33),
        panel.grid = element_blank())


## ---- Negative Binomial Model ----

# without exposure
mod2 <- MASS::glm.nb(num201 ~ rile, data=tmp)

# with exposure
mod2e0 <- MASS::glm.nb(num201 ~ 1 +
                 offset(log(total)),
               data=tmp)
mod2e <- MASS::glm.nb(num201 ~ rile +
                 offset(log(total)),
               data=tmp)
AIC(mod2, mod2e0, mod2e)

obs <- model.response(model.frame(mod2))
k <- 0:74
avgp <- function(m)sapply(k, function(j)
  mean(dnbinom(j, mu=predict(m, type="response"), size=m$theta)))
fitdat <- tibble::tibble(
  model = rep(c("NB", "NB Null (Exposure)", "NB Exposure"),
              each = length(k)),
  val = rep(k, 3),
  observed = rep(sapply(k, function(j)mean(obs == j)), 3),
  predicted = c(avgp(mod2), avgp(mod2e0), avgp(mod2e))) %>%
  pivot_longer(observed:predicted, names_to="type", values_to="prop")
ggplot(fitdat,
       aes(x=val, y=prop, fill=type)) +
  geom_col(position="dodge") +
  facet_wrap(~model, ncol=2) +
  theme_bw() +
  scale_fill_brewer(palette="Set1") +
  labs(x="Number of Statements", y="Proportion", fill="") +
  theme(legend.position = c(.825, .33),
        panel.grid = element_blank())


## ---- Binomial Model Example ----

# successes (num201) and failures (other) out of n sentences;
# total is not always a whole number, so n is built from the two counts
tmp$other <- floor(tmp$total - tmp$num201)
tmp$n <- tmp$num201 + tmp$other
mod3 <- glm(cbind(num201, other) ~ rile, data=tmp,
            family=binomial)

obs <- tmp$num201
p <- predict(mod3, type="response")
k <- 0:74
fitdat <- tibble::tibble(
  val = k,
  Observed = sapply(k, function(j)mean(obs == j)),
  Predicted = sapply(k, function(j)mean(dbinom(j, tmp$n, p)))
) %>%
  pivot_longer(-val,
               names_to = "type",
               values_to = "prop")
ggplot(fitdat,
       aes(x=val, y=prop, fill=type)) +
  geom_col(position="dodge") +
  theme_classic() +
  scale_fill_brewer(palette="Set1") +
  labs(x="Number of Statements", y="Proportion", fill="") +
  theme(legend.position = "top")


## ---- Beta-Binomial Model Example ----

# a binomial whose probability varies across manifestos according to
# a Beta distribution - the binomial's version of the NB
mod3b <- glmmTMB::glmmTMB(
  cbind(num201, other) ~ rile,
  data=tmp,
  family=glmmTMB::betabinomial)
AIC(mod3, mod2e, mod3b)
cbind(binomial = coef(mod3),
      betabin = glmmTMB::fixef(mod3b)$cond)

# beta-binomial probabilities: p*phi and (1-p)*phi are the two
# Beta parameters, with phi = sigma() in glmmTMB
obs <- tmp$num201
phi <- glmmTMB::sigma(mod3b)
p <- predict(mod3b, type="response")
a <- p*phi
b <- (1-p)*phi
dbetabin <- function(j){
  ifelse(j <= tmp$n,
         exp(lchoose(tmp$n, j) + lbeta(j + a, pmax(tmp$n - j, 0) + b) - lbeta(a, b)),
         0)
}
k <- 0:74
fitdat <- tibble::tibble(
  val = k,
  Observed = sapply(k, function(j)mean(obs == j)),
  Predicted = sapply(k, function(j)mean(dbetabin(j)))
) %>%
  pivot_longer(-val,
               names_to = "type",
               values_to = "prop")
ggplot(fitdat,
       aes(x=val, y=prop, fill=type)) +
  geom_col(position="dodge") +
  theme_classic() +
  scale_fill_brewer(palette="Set1") +
  labs(x="Number of Statements", y="Proportion", fill="") +
  theme(legend.position = "top")


## ---- Binomial vs. Beta-Binomial: Effect of rile ----

# predicted probability of a freedom/human rights sentence, plotted over
# each manifesto's observed proportion (circles sized by length)
s <- seq(-75, 90, length=100)
bbdat <- bind_rows(
  avg_predictions(mod3, variables=list(rile=s)) %>%
    as_tibble() %>% mutate(model="Binomial"),
  avg_predictions(mod3b, variables=list(rile=s)) %>%
    as_tibble() %>% mutate(model="Beta-Binomial"))
ggplot(bbdat, aes(x=rile, y=estimate)) +
  geom_point(data=tmp, aes(x=rile, y=num201/n, size=n),
             inherit.aes=FALSE, alpha=.15, shape=1) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=model),
              alpha=.3) +
  geom_line(aes(color=model)) +
  scale_fill_brewer(palette="Set1") +
  scale_color_brewer(palette="Set1") +
  scale_size_area(max_size=8) +
  coord_cartesian(ylim=c(0, .1)) +
  theme_classic() +
  labs(x="Left-Right (rile)", y="Pr(Freedom/Human Rights Statement)",
       color="", fill="", size="Statements") +
  theme(legend.position="top")


## ---- OLS and AIC ----

ols <- lm(num201 ~ rile + total, data=tmp)
mu <- fitted(ols)
sig <- sqrt(mean(resid(ols)^2)) # ML estimate
y <- tmp$num201

# log Pr(y = k) = log(Pr(Y > k - .5) - Pr(Y > k + .5)), computed from
# log upper tails so very large counts don't underflow to log(0);
# zeros get all the probability below .5
la <- pnorm(ifelse(y == 0, -Inf, y - .5), mu, sig,
            lower.tail=FALSE, log.p=TRUE)
lb <- pnorm(y + .5, mu, sig,
            lower.tail=FALSE, log.p=TRUE)
ll <- sum(la + log1p(-exp(lb - la)))

# AIC from the normal density vs. from the count probabilities
c(density = AIC(ols),
  discrete = -2*ll + 2*attr(logLik(ols), "df"))
AIC(mode, mod3, mod2e, mod3b)


## ---- Effects ----

# expected number of statements across rile for every model
ap1 <- avg_predictions(mode, variables=list(rile = s))
ap2 <- avg_predictions(mod2e, variables=list(rile = s))
ap3 <- avg_predictions(mod3, variables=list(rile = s))
ap3b <- avg_predictions(mod3b, variables=list(rile = s))
ap4 <- avg_predictions(ols, variables=list(rile = s))

# the binomial models predict a probability; multiplying by the mean
# length puts them on the same scale as the offset models
plot.dat <- ap1 %>%
  as_tibble() %>%
  mutate(method = "Poisson (E)") %>%
  bind_rows(ap2 %>% as_tibble() %>% mutate(method="NB (E)"),
            ap3 %>% as_tibble() %>%
              mutate(method="Binom",
                     across(c(estimate, conf.low, conf.high),
                            ~.x * mean(tmp$total))),
            ap3b %>% as_tibble() %>%
              mutate(method="Beta-Binom",
                     across(c(estimate, conf.low, conf.high),
                            ~.x * mean(tmp$total))),
            ap4 %>% as_tibble() %>% mutate(method="OLS"))

ggplot(plot.dat, aes(x=rile,y=estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax=conf.high, fill
                  =method),
              alpha=.25) +
  geom_line(aes(color = method)) +
  theme_classic()


## ---- Exercise ----

sci <- import("data/science.dta")
sci <- sci %>%
  mutate(across(age_group:income, factorize))

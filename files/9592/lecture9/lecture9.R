## POLSCI 9592, Lecture 9: Dependent Data
## Code for the slides in lecture9.Rmd

library(rio)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(marginaleffects)
# bet() and win(): remotes::install_github("davidaarmstrong/bwreg")
library(bwreg)

# make sure select() is dplyr's, even if another package masks it
select <- function(...)dplyr::select(...)


## ---- Snijders and Bosker's example ----

# five groups of two observations each
sb <- data.frame(
  g = rep(1:5, each=2),
  x = c(1,3,2,4,3,5,4,6,5,7),
  y = c(5,7,4,6,3,5,2,4,1,3))

# one row per group: the group means of x and y
sb_means <- sb %>% group_by(g) %>% summarise(x = mean(x), y = mean(y))

# (1) total regression, (2) regression between group means,
# (3/4) between-within regression: bet() is the group mean of x,
# win() the deviation from it
reg1 <- lm(y ~ x, data=sb)
reg2 <- lm(y ~ x, data=sb_means)
reg3 <- lm(y ~ bet(x, g) + win(x, g), data=sb)
coef(reg1)
coef(reg2)
coef(reg3)

# the data with the total (solid), between (dashed) and within-group
# (dotted) regression lines
plot(c(0,8), c(0,8), type="n", xlab="x", ylab="y")
for(j in 1:5){
  abline(lm(y ~ x, data=sb[sb$g == j, ]), lty=3)
  points(sb[sb$g == j, c("x", "y")], pch=j, cex=2)
}
abline(coef(reg2), lty=2, lwd=2)
abline(coef(reg1), lty=1, lwd=2)
legend(8,8, c("Total Regression", "Regression Between Groups",
    "Regressions Within Groups"), lty=c(1,2,3), xjust=1)


## ---- What does OLS estimate? ----

# within and between sums of squares of x
sb_xbar <- ave(sb$x, sb$g)
sb_W <- sum((sb$x - sb_xbar)^2)
sb_B <- sum((sb_xbar - mean(sb$x))^2)

# OLS slope = SS_W/(SS_W + SS_B) * within slope + SS_B/(SS_W + SS_B) * between slope;
# here the within slope is 1 and the between slope is -1
(sb_W*1 + sb_B*(-1))/(sb_W + sb_B)


## ---- OLS, RE and FE: one family ----

# British Election Study 1997: voters (LRSCALE, INCOME) in constituencies (PANO)
bes <- import("data/context.dta")

# within and between sums of squares, and the within and between slopes
x <- bes$INCOME; y <- bes$LRSCALE; g <- bes$PANO
xbar <- ave(x, g); ybar <- ave(y, g)
SS_W <- sum((x - xbar)^2)
SS_B <- sum((xbar - mean(x))^2)
b_W <- sum((x - xbar)*(y - ybar))/SS_W
b_B <- sum((xbar - mean(x))*(ybar - mean(y)))/SS_B

# lambda = sigma^2/(sigma^2 + n*tau^2): how much RE discounts the between
# information (using the average group size)
f_re <- lmer(LRSCALE ~ INCOME + (1|PANO), data=bes)
fvc <- as.data.frame(VarCorr(f_re))$vcov
lambda <- fvc[2]/(fvc[2] + mean(table(g))*fvc[1])

# weight on the between slope: lambda = 1 is OLS, lambda = 0 is FE
c(OLS = SS_B/(SS_W + SS_B),
  RE = lambda*SS_B/(SS_W + lambda*SS_B),
  FE = 0)

# the three estimates of the income slope ...
c(OLS = unname(coef(lm(LRSCALE ~ INCOME, data=bes))[2]),
  RE = unname(fixef(f_re)[2]),
  FE = unname(coef(lm(LRSCALE ~ INCOME + factor(PANO), data=bes))[2]))

# ... and the weighted-average formula (approximate for RE, since the
# groups are not all the same size)
(SS_W*b_W + lambda*SS_B*b_B)/(SS_W + lambda*SS_B)


## ---- Step 1: the empty model ----

dat <- import("data/context.dta")
dat <- dat %>% mutate(SEX = rio::factorize(SEX))

# random intercept only: splits the variance of LRSCALE into between
# (tau^2) and within (sigma^2) constituencies
m0 <- lmer(LRSCALE ~ 1 + (1|PANO), data=dat)
VarCorr(m0)

# ICC = tau^2/(tau^2 + sigma^2); design effect = 1 + (nbar - 1)*ICC
vc <- as.data.frame(VarCorr(m0))
icc <- vc$vcov[1]/sum(vc$vcov)
nbar <- mean(table(dat$PANO))
deff <- 1 + (nbar-1)*icc
c(icc = icc, nbar = nbar, deff = deff)

# effective sample size for constituency-level quantities
nrow(dat)/deff


## ---- Step 2: modeling both between and within effects ----

# bet() and win() for each individual-level variable (for the factor SEX,
# bet() is the proportion of men); PROFMAN only varies by constituency
mod <- lmer(LRSCALE ~ bet(INCOME, PANO) + win(INCOME, PANO) +
              bet(AGE, PANO) + win(AGE, PANO) +
              bet(SEX, PANO) + win(SEX, PANO) +
              PROFMAN + (1|PANO), data=dat)
round(coef(summary(mod)), 3)


## ---- Within effects = fixed effects ----

# fixed effects: a dummy variable for every constituency
fe_mod <- lm(LRSCALE ~ INCOME + AGE + SEX + factor(PANO), data=dat)

# plain random effects: one coefficient per variable
re_mod <- lmer(LRSCALE ~ INCOME + AGE + SEX + PROFMAN + (1|PANO), data=dat)

# REWB within coefficients are identical to FE; plain RE is in between
# the within and between slopes
round(cbind(
  FE   = coef(fe_mod)[c("INCOME", "AGE", "SEXmen")],
  REWB = fixef(mod)[c("win(INCOME, PANO)", "win(AGE, PANO)", "win(SEX, PANO)")],
  RE   = fixef(re_mod)[c("INCOME", "AGE", "SEXmen")]), 4)


## ---- Testing the contextual effect (H2) ----

# contextual effect = between - within: the difference between two people
# with the same income in constituencies whose average income differs by 1
hypotheses(mod, "`bet(INCOME, PANO)` - `win(INCOME, PANO)` = 0")


## ---- Within and between slopes with marginaleffects ----

# within effect: changing income for people in real constituencies only
# moves the within component
w_eff <- avg_slopes(mod, variables = "INCOME", re.form = NA)

# between effect: between_grid() makes hypothetical constituencies whose
# mean income moves with income
bg <- between_grid(mod, INCOME = c(4, 6, 8))
b_eff <- avg_slopes(mod, variables = "INCOME", newdata = bg, re.form = NA)

bind_rows(Within = as.data.frame(w_eff),
          Between = as.data.frame(b_eff),
          .id = "Effect") %>%
  select(Effect, estimate, std.error, conf.low, conf.high) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

# picture: income only, to keep the lines clean
m_inc <- lmer(LRSCALE ~ bet(INCOME, PANO) + win(INCOME, PANO) + (1|PANO), data=dat)

# the between line: predictions for hypothetical constituencies
between <- predictions(m_inc, newdata = between_grid(m_inc, INCOME = seq(3, 10, by=.25)),
                       re.form = NA)

# within lines: five real constituencies spread across the range of mean
# income, with income varied +/- 2 around each one's mean
means <- tapply(dat$INCOME, dat$PANO, mean)
grps <- as.numeric(names(sort(means)))[round(seq(5, length(means)-5, length=5))]
centres <- means[as.character(grps)]
within_grid <- do.call(rbind, lapply(seq_along(grps), function(i) {
  data.frame(PANO = grps[i], INCOME = centres[i] + seq(-2, 2, length.out = 20))
}))
within <- predictions(m_inc, newdata = within_grid, re.form = NA)
at_mean <- predictions(m_inc, newdata = data.frame(PANO = grps, INCOME = centres), re.form = NA)

ggplot(between, aes(INCOME, estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
  geom_line(linewidth = 1) +
  geom_line(data = within, aes(group = PANO), colour = "#4F2683") +
  geom_point(data = at_mean, colour = "#4F2683", size = 2) +
  labs(x = "Income", y = "Predicted Left-Right Score",
       caption = "Black: between-constituency effect; purple: within five constituencies") +
  theme_classic()


## ---- Step 3: a random slope for income (H3) ----

# let the within effect of income vary across constituencies
mod_rs <- lmer(LRSCALE ~ bet(INCOME, PANO) + win(INCOME, PANO) +
              bet(AGE, PANO) + win(AGE, PANO) + bet(SEX, PANO) + win(SEX, PANO) +
              PROFMAN + (1 + win(INCOME, PANO)|PANO), data=dat)
VarCorr(mod_rs)

# the models differ only in their random effects, so the REML fits can be
# compared (refit=FALSE)
lrt <- anova(mod, mod_rs, refit=FALSE)
round(as.data.frame(lrt)[, c("npar", "AIC", "logLik", "Chisq", "Df", "Pr(>Chisq)")], 3)


## ---- Reading the random slope result ----

# a variance can't be negative, so use a 50:50 mixture of chi-square(1)
# and chi-square(2) for the slope variance plus its covariance
chi <- lrt$Chisq[2]
p1 <- pchisq(chi, 1, lower.tail=FALSE)
p2 <- pchisq(chi, 2, lower.tail=FALSE)

# p2 is the naive p-value that anova() reports; the mixture p-value
# averages the two tail areas
c(chi = chi, p_chisq1 = p1, p_chisq2 = p2, p_mixture = .5*p1 + .5*p2)


## ---- Shrinkage in the empty model ----

# estimated constituency effects (U_0j)
head(ranef(m0)$PANO)

# raw constituency means vs. shrunken (empirical Bayes) estimates
eb <- dat %>% group_by(PANO) %>%
  summarise(raw = mean(LRSCALE), n = n()) %>%
  mutate(eb = fixef(m0)[1] + ranef(m0)$PANO[as.character(PANO), 1])

# arrows go from the raw mean to the shrunken estimate; smaller samples
# are pulled further toward the overall mean (dashed line)
ggplot(eb, aes(x = n)) +
  geom_hline(yintercept = fixef(m0)[1], linetype=2) +
  geom_segment(aes(xend = n, y = raw, yend = eb),
               arrow = arrow(length = unit(.08, "in")), colour="gray50") +
  geom_point(aes(y = raw), colour = "gray50") +
  geom_point(aes(y = eb), colour = "#4F2683") +
  labs(x = "Respondents in Constituency", y = "Constituency Mean of LRSCALE") +
  theme_classic()


## ---- p-values ----

# lmerTest adds degrees of freedom and p-values; note that it masks
# lme4's lmer() from here on
library(lmerTest)
mod <- as_lmerModLmerTest(mod)

# Kenward-Roger df: about 2000 for within variables, about the number of
# constituencies for between and constituency-level variables
anova(mod, ddf="Kenward-Roger")


## ---- Cluster-robust standard errors ----

library(clubSandwich)

# OLS with the same between and within variables
ols <- lm(LRSCALE ~ bet(INCOME, PANO) + win(INCOME, PANO) + bet(AGE, PANO) + win(AGE, PANO) +
              bet(SEX, PANO) + win(SEX, PANO) + PROFMAN, data=dat)

# standard errors: naive OLS, OLS with CR2, the multilevel model, and the
# multilevel model with CR2
ses <- cbind(
  OLS       = sqrt(diag(vcov(ols))),
  OLS_CR2   = sqrt(diag(vcovCR(ols, cluster=dat$PANO, type="CR2"))),
  MLM       = sqrt(diag(vcov(mod))),
  MLM_CR2   = sqrt(diag(vcovCR(mod, type="CR2"))))
round(ses[-1, ], 4)

# CR2 tests with Satterthwaite degrees of freedom
coef_test(mod, vcov="CR2", test="Satterthwaite")


## ---- Heisig and Schaeffer example ----

# World Values Survey 2005-2009; relig: higher values = religion more
# important, grand-mean centred
wvs <- import("data/wvs2005_2009_mlm.dta") %>%
  mutate(country = rio::factorize(country), sex = rio::factorize(sex),
         relig = 5 - religimp, relig = relig - mean(relig, na.rm=TRUE)) %>%
  select(lrself, relig, age, sex, educ, income, country) %>%
  na.omit() %>% droplevels()

# cross-level interaction (individual religiosity x country religiosity)
# without and with a random slope for individual religiosity
hs1 <- lmer(lrself ~ bet(relig, country)*win(relig, country) + age + sex + educ + income +
              (1|country), data=wvs)
hs2 <- lmer(lrself ~ bet(relig, country)*win(relig, country) + age + sex + educ + income +
              (1 + win(relig, country)|country), data=wvs)

# without the random slope, the standard errors are much too small
cf <- c("win(relig, country)", "bet(relig, country):win(relig, country)")
round(cbind(b_noRS = fixef(hs1)[cf], se_noRS = sqrt(diag(vcov(hs1)))[cf],
            b_RS = fixef(hs2)[cf], se_RS = sqrt(diag(vcov(hs2)))[cf]), 3)

# how much bigger the standard errors are with the random slope
sqrt(diag(vcov(hs2)))[cf]/sqrt(diag(vcov(hs1)))[cf]


## ---- Multilevel GLMs (syntax only) ----

# binary/count models
# glmer(y ~ bet(x, g) + win(x, g) + (1|g), data=dat, family=binomial)
# ordinal models
# ordinal::clmm(y ~ bet(x, g) + win(x, g) + (1|g), data=dat)
# many other families (negative binomial, beta, zero-inflated)
# glmmTMB::glmmTMB(y ~ bet(x, g) + win(x, g) + (1|g), data=dat, family=nbinom2)


## ---- Example: religious self-identification ----

# age in decades keeps the coefficients on a sensible scale
wvs2 <- import("data/wvs2005_2009_mlm.dta") %>%
  mutate(country = rio::factorize(country), sex = rio::factorize(sex), age10 = age/10) %>%
  select(religperson, age10, sex, educ, income, country) %>%
  na.omit() %>% droplevels()

# random-intercept logit
g1 <- glmer(religperson ~ age10 + sex + educ + income + (1|country), data=wvs2, family=binomial)

# tau and the approximate attenuation of population-averaged coefficients
tau <- sqrt(VarCorr(g1)$country[1])
c(tau = tau, attenuation = 1/sqrt(1 + .346*tau^2))

# effect of age in a typical country (U = 0) vs. averaged over the
# countries in the data (the second ignores uncertainty in the U_0j)
s_c <- avg_slopes(g1, variables = "age10", re.form = NA)
s_a <- avg_slopes(g1, variables = "age10", re.form = NULL)
bind_rows(`U = 0` = as.data.frame(s_c), `Avg over countries` = as.data.frame(s_a), .id = "Type") %>%
  select(Type, estimate, std.error, conf.low, conf.high) %>%
  mutate(across(where(is.numeric), ~round(.x, 4)))


## ---- Conditional vs. population-averaged predictions ----

# 500 evenly spaced draws from the random-effect distribution, N(0, tau^2)
u <- qnorm(ppoints(500), 0, tau)

# ages from 18 to 85, other variables at typical values
pg <- datagrid(model = g1, age10 = seq(1.8, 8.5, length=50))
pg$xb <- predict(g1, newdata = pg, re.form = NA)

# conditional: a country with U = 0; population-averaged: the average
# probability over the distribution of countries
pg$Conditional <- plogis(pg$xb)
pg$PopAvg <- sapply(pg$xb, function(x) mean(plogis(x + u)))

pg %>%
  select(age10, Conditional, PopAvg) %>%
  pivot_longer(-age10, names_to = "Type", values_to = "p") %>%
  mutate(Type = factor(Type, labels = c("Conditional (U = 0)", "Population-Averaged"))) %>%
  ggplot(aes(x = age10*10, y = p, linetype = Type)) +
  geom_line(linewidth = 1) +
  labs(x = "Age", y = "Pr(Religious Person)", linetype = "") +
  theme_classic() +
  theme(legend.position = "top")


## ---- Troubleshooting the random slope model (H3) ----

# diagnose: the fit is singular, and the random-effect variance has only
# one real dimension (the second SD is 0)
isSingular(mod_rs)
rePCA(mod_rs)$PANO$sdev

# fix: a weak (gamma) prior on the random-effect SDs keeps them off the boundary
library(blme)
mod_b <- blmer(formula(mod_rs), data=dat)
isSingular(mod_b)
VarCorr(mod_b)

# check: largest difference in the income coefficients across optimizers
af <- allFit(mod_rs, verbose=FALSE)
fe <- summary(af)$fixef
round(apply(fe, 2, function(x)diff(range(x))), 6)[2:3]

# the prior barely changes the fixed effects
cf <- c("bet(INCOME, PANO)", "win(INCOME, PANO)")
round(cbind(lmer = fixef(mod_rs)[cf],
            blmer = fixef(mod_b)[cf]), 4)


## ---- Other extensions: explained variance ----

# marginal (fixed effects only) and conditional (fixed + random) R-squared
performance::r2_nakagawa(mod)


## ---- Exercise ----

dat <- import("data/wvs2005_2009_mlm.dta")

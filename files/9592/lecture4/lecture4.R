## POLSCI 9592, Lecture 4: Model Fit and Evaluation
## Code for the slides in Lecture4.Rmd

library(rio)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(RColorBrewer)
library(DAMisc)
library(separationplot)
library(psre)
library(patchwork)
library(lmtest)
library(AICcmodavg)
library(clarkeTest)
library(rsample)
library(statmod)
library(car)
library(logistf)
library(marginaleffects)

# make sure select() is dplyr's, even if another package masks it
select <- function(...)dplyr::select(...)


## ---- Scalar measures of fit (ANES 2008 turnout) ----

# turnout logit
load("data/anes_2008_binary.rda")
dat$race <- rio::factorize(dat$race)
mod <- glm(voted ~ age + educ + income + poly(leftright, 2) +
    female + race, data=dat, family=binomial(link="logit"))

# likelihood-based and pseudo-R2 measures of fit
binfit(mod)

# PRE and expected PRE, with simulated sampling distributions
pre(mod, sim=T)


## ---- Separation plot ----

# one tick per observation, sorted by the predicted probability and
# colored by the observed outcome
cols <- brewer.pal(5, "Set2")[c(2,3)]
y <- model.response(model.frame(mod))
separationplot(fitted(mod), c(y),
    col0=cols[1], col1=cols[2], file=NULL)


## ---- Empirical vs predicted probabilities (heatmap fit) ----

# remotes::install_github("davidaarmstrong/psre")
# loess of the observed outcome on the predicted probability, with a
# bootstrap band from simulating the outcome under the fitted model
gh1 <- gg_hmf(
  model.response(model.frame(mod)),
  fitted(mod),
  method="loess")

# histogram of the predictions on top of the heatmap fit plot
gh1[[1]] + gh1[[2]] +
  plot_layout(heights=c(2,8), ncol=1)


## ---- Nested models: likelihood ratio test ----

m1 <-  glm(voted ~ age + educ + income + poly(leftright, 2) +
    female + race, data=dat, family=binomial(link="logit"))
m2 <-  glm(voted ~ educ + income + poly(leftright, 2),
           data=dat, family=binomial(link="logit"))
lrtest(m1, m2)


## ---- Non-nested models: information criteria and Clarke's test ----

# AIC, AICc and BIC for each model, as differences from the smallest
# value of each (the best model gets 0)
IC_delta <- function(...){
  mods <- list(...)
  a <- AIC(...)
  a$AICc <- sapply(mods, AICc)
  a$BIC <- BIC(...)[,2]
  out <- a %>%
  mutate(across(c(AIC, AICc, BIC), ~.x-min(.x)))
  names(out)[2:4] <- paste0("D_", names(out)[2:4])
  out
}

# two non-nested turnout models
m1 <-  glm(voted ~ age + female + race, data=dat,
           family=binomial(link="logit"))
m2 <-  glm(voted ~ educ + income + poly(leftright, 2),
           data=dat, family=binomial(link="logit"))
IC_delta(m1, m2)

# paired sign test on the individual log-likelihoods
clarke_test(m1, m2)


## ---- Non-nested models: cross-validation ----

# strength of ideology: distance from the scale midpoint
dat$lrstren <- abs(dat$leftright-5)
m1 <-  glm(voted ~ age + female + race +
             lrstren, data=dat,
           family=binomial(link="logit"))
m2 <-  glm(voted ~ educ + income +
             poly(leftright, 2), data=dat,
           family=binomial(link="logit"))

# refit both models on the training part of a split and return each
# model's negative log-likelihood in the held-out part (smaller is better)
cv_logit <- function(split, m1, m2, ...){
  m1_up <- update(m1, data=analysis(split))
  m2_up <- update(m2, data=analysis(split))
  y_out <- model.response(
            model.frame(formula(m1),
                        data=assessment(split)))
  p1 <- predict(m1_up,
                newdata=assessment(split),
                type="response")
  p2 <- predict(m2_up,
                newdata=assessment(split),
                type="response")
  ll1 <- sum(-y_out*log(p1) - (1-y_out)*log(1-p1))
  ll2 <- sum(-y_out*log(p2) - (1-y_out)*log(1-p2))
  tibble(model = factor(1:2,
                        labels=c("Model1", "Model2")),
         ll = c(ll1, ll2))
}

# 10-fold cross-validation repeated 10 times; total the held-out
# negative log-likelihoods for each repeat and take the difference
# (positive values favour Model 1)
cv_out <- vfold_cv(dat,
                   v=10,
                   repeats=10) %>%
    mutate(ll = map(splits, cv_logit, m1, m2)) %>%
    unnest(ll) %>%
    group_by(id, model) %>%
    summarise(ll = sum(ll)) %>%
    pivot_wider(names_from="model", values_from="ll") %>%
  mutate(diff = Model2-Model1)
cv_out


## ---- Component + residual plots ----

# component + residual plot for one term of a glm: the term's
# contribution to the linear predictor plus the chosen residuals, with
# linear and loess fits (set plot=FALSE to get the data instead)
gg_crplot <- function(obj, term, plot=TRUE,
                      res_type = c("std_deviance", "std_pearson", "deviance", "pearson", "response", "working", "quantile"),
                      ...){
  trms <- predict(obj, type="terms")
  restyp <- match.arg(res_type)
  if(!term %in% colnames(trms)){
    stop("term must be one of: ", paste(paste('"', colnames(trms), '"', sep=""), collapse=", "))
  }
  if(restyp == "std_deviance")e <- rstandard(obj, type="deviance")
  if(restyp == "std_pearson")e <- rstandard(obj, type="pearson")
  if(restyp == "deviance")e <- residuals(obj, type="deviance")
  if(restyp == "pearson")e <- residuals(obj, type="pearson")
  if(restyp == "response")e <- residuals(obj, type="response")
  if(restyp == "working")e <- residuals(obj, type="working")
  if(restyp == "quantile")e <- statmod::qresid(obj)

  cpr <- apply(trms, 2, \(x)x+e)
  tmp <- data.frame(x = trms[,term],
                    y = cpr[,term])
  attr(tmp, "term") <- term
  attr(tmp, "res_type") <- restyp
  if(plot){
  ggplot(tmp, aes(x=x, y=y)) +
      geom_point(shape=1, col="gray50") +
      geom_smooth(aes(linetype="Linear"), method="lm", formula = y ~ x, se=FALSE, color="black") +
      geom_smooth(aes(linetype="LOESS"), method="loess", formula = y ~ x, se=FALSE, color="black") +
      scale_linetype_manual(values=c(3,1)) +
      theme_classic() +
      labs(x=term, y="Component + Residual", linetype="")

  }else{
    return(tmp)
  }
}

# back to the full turnout model
m1 <-  glm(voted ~ age + educ + income + poly(leftright, 2) +
    female + race, data=dat, family=binomial(link="logit"))

# is age linear? is the quadratic in left-right enough?
gg_crplot(m1, "age")
gg_crplot(m1, "poly(leftright, 2)")

# left-right as a factor (fully flexible) vs. the quadratic
dat$lrfac <- as.factor(dat$leftright)
mnew <- glm(voted ~ age + educ + income + as.factor(leftright) +
    female + race, data=dat, family=binomial(link="logit"))
anova(m1, mnew, test='Chisq')


## ---- Outliers and influential observations ----

# index plot of Cook's distances, labelling the 10 largest
influenceIndexPlot(mod, vars="Cook", id=list(n=10))

# effect of age on predicted probabilities as the 25 most outlying
# observations are removed cumulatively
outEff(mod,
       var='age',
       data=dat,
       nOut=25,
       cumulative=TRUE)


## ---- Separation: voting for the National Front ----

# French election data, complete cases on the model variables
sepdat <- rio::import("data/france_binary.dta")
sepdat <- sepdat %>%
  mutate(across(c("demsat", "retnat", "union"),
                rio::factorize))
sepdat <- sepdat |>
  select(votefn, demsat, age, lrself,
         hhincome, retnat, union) |>
  na.omit()

# ML logit: no "Very satisfied" respondent voted FN, so the demsat
# coefficients and their standard errors are huge
mod3 <- glm(votefn ~ demsat + age + lrself + hhincome + retnat
    + union, data=sepdat, family=binomial)
printCoefmat(summary(mod3)$coefficients)


## ---- Firth logit ----

# print a logistf model: coefficient, SE, p-value and the profile
# likelihood CI, with the columns aligned on the decimal point
pfl <- function(x, digits=4){
out <- cbind(x$coefficients, diag(x$var)^0.5, x$ci.lower,
     x$ci.upper, qchisq(1 - x$prob, 1), x$prob)
out <- apply(out, 2, function(x)sprintf(paste("%.", digits, "f", sep=""), x))
for(i in 1:ncol(out)){
    tmp <- strsplit(out[,i], split=".", fixed=T)
    nc <- sapply(tmp, function(x)nchar(x[1]))
    mnc <- max(nc)
    pads <- mnc-nc
    pads <- sapply(pads, function(x)paste(rep(" ", x), collapse=""))
    for(j in 1:length(tmp)){
        tmp[[j]][1] <- paste(pads[j], tmp[[j]][1], sep="")
    }
    res <- c(sapply(tmp, function(x)paste(x, collapse=".")))
    out[,i] <- res
}
dimnames(out) <- list(names(x$coefficients), c("coef", "se(coef)",
         paste(c("lower", "upper"), 1 - x$alpha), "Chisq", "p"))
out <- out[,c(1,2,6,3,4,5)]
noquote(out)
}

# Firth's penalized likelihood keeps the coefficients finite
mod3a <- logistf(votefn ~ demsat + age + lrself + hhincome + retnat
    + union, data=sepdat)
pfl(mod3a)


## ---- Profile vs. Wald confidence intervals ----

# profile deviance for the Somewhat satisfied coefficient (relative to
# Very satisfied, the separated category) compared with the parabola
# the Wald interval assumes
v <- "demsatSomewhat satisfied"
bhat <- coef(mod3a)[v]
se <- sqrt(diag(vcov(mod3a)))[v]
pr <- profile(mod3a, variable = v, limits = c(-2, 8.5), steps = 200, plot = FALSE)
prof <- tibble(b = pr$beta, Profile = 2 * (mod3a$loglik["full"] - pr$loglik),
               Wald = ((pr$beta - bhat) / se)^2) %>%
  tidyr::pivot_longer(-b, names_to = "type", values_to = "dev")

# the two 95% intervals, drawn below the curves
ci <- tibble(type = c("Profile", "Wald"),
             lwr = c(mod3a$ci.lower[v], bhat - 1.96 * se),
             upr = c(mod3a$ci.upper[v], bhat + 1.96 * se),
             y = c(-.7, -1.5))
cols <- c(Profile = "#2a78d6", Wald = "#eb6834")

# each interval is where its curve is below the chi-square cutoff
ggplot(prof, aes(x = b, y = dev, colour = type, linetype = type)) +
  geom_hline(yintercept = qchisq(.95, 1), colour = "gray50") +
  annotate("text", x = 12, y = qchisq(.95, 1), label = "chi[1]^2*' cutoff = 3.84'",
           parse = TRUE, hjust = 1, vjust = -.5, colour = "gray30", size = 4.5) +
  geom_vline(xintercept = bhat, colour = "gray70", linetype = 3) +
  geom_line(linewidth = 1) +
  geom_segment(data = ci, aes(x = lwr, xend = upr, y = y, yend = y),
               linewidth = 1.5, inherit.aes = FALSE, colour = cols[ci$type]) +
  geom_text(data = ci, aes(x = upr, y = y,
                           label = sprintf("%s 95%% CI [%.2f, %.2f]", type, lwr, upr)),
            hjust = -.05, size = 4.5, colour = "gray20", inherit.aes = FALSE) +
  scale_colour_manual(values = cols) +
  scale_linetype_manual(values = c(Profile = 1, Wald = 2)) +
  coord_cartesian(ylim = c(-1.8, 10), xlim = c(-2, 12)) +
  labs(x = "Coefficient: Somewhat satisfied (vs. Very satisfied)",
       y = "Deviance from the maximum", colour = NULL, linetype = NULL) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "top", panel.grid.minor = element_blank())


## ---- FLIC and FLAC: predicted probabilities ----

# FLIC: Firth slopes with the intercept re-estimated by ML
mod3_flic <- logistf(formula(mod3),
                     data=sepdat,
                     flic=TRUE)

# FLAC: ML on the Firth-augmented data with a pseudo-observation dummy
# (call flac() at the top level; it can't find data inside a function)
mod3_flac <- flac(formula(mod3),
                     data=sepdat)

# profile likelihood CIs for predicted probabilities and first
# differences: profile_pred() and profile_fd()
source("profile_fd.R")

# each demsat category, other variables at their means or modes
dg <- marginaleffects::datagrid(
  model = mod3a,
  demsat =levels(sepdat$demsat),
  grid_type="mean_or_mode")

# predicted probabilities with profile CIs from each estimator
p_lf <- profile_pred(mod3a, dg)
p_flic <- profile_pred(mod3_flic, dg)
p_flac <- profile_pred(mod3_flac, dg)

all_dat <- p_lf |> mutate(type="Firth") |>
  bind_rows(
    p_flic |> mutate(type = "FLIC"),
    p_flac |> mutate(type = "FLAC")
  ) |>
  mutate(type = factor(type, levels=c("FLAC", "FLIC", "Firth")))

ggplot(all_dat, aes(x=fit, xmin = lower, xmax = upper, y=demsat, color=type)) +
  geom_pointrange(position = position_dodge(width=.5)) +
  theme_minimal(base_size = 16) +
  theme(legend.position="top") +
  labs(x="Predicted Pr(Vote FN)", y="", color="Model")


## ---- FLIC and FLAC: first differences ----

# Very satisfied - Not at all satisfied with Wald (delta method) CIs
comp_lf <- comparisons(mod3a, newdata = datagrid(model = mod3a, grid_type = "mean_or_mode"), variables=list(demsat = c("Not at all satisfied", "Very satisfied")))

comp_flic_d <- comparisons(mod3_flic, newdata = datagrid(model = mod3a, grid_type = "mean_or_mode"), variables=list(demsat = c("Not at all satisfied", "Very satisfied")))

comp_flac_d <- comparisons(mod3_flac, newdata = datagrid(model = mod3a, grid_type = "mean_or_mode"), variables=list(demsat = c("Not at all satisfied", "Very satisfied")))

# the same difference with profile likelihood CIs (row 1 of dg is Very
# satisfied, row 4 is Not at all satisfied)
comp_lf_prof <- profile_fd(mod3a, dg[4, ], dg[1,])
comp_flic_prof <- profile_fd(mod3_flic, dg[4, ], dg[1,])
comp_flac_prof <- profile_fd(mod3_flac, dg[4, ], dg[1,])

# Wald and profile intervals from each estimator in one table
all_comps <- comp_lf |>
  as_tibble() |>
  select(estimate, lower=conf.low, upper=conf.high) |>
  mutate(type = "Wald", model = "Firth") |>
  bind_rows(
comp_flic_d |>
  as_tibble() |>
  select(estimate, lower=conf.low, upper=conf.high) |>
  mutate(type = "Wald", model = "FLIC"),
comp_flac_d |>
  as_tibble() |>
  select(estimate, lower=conf.low, upper=conf.high) |>
  mutate(type = "Wald", model = "FLAC"),
do.call(data.frame, comp_lf_prof) |>
  select(estimate, lower, upper) |>
  mutate(type ="Profile", model="Firth"),
do.call(data.frame, comp_flic_prof) |>
  select(estimate, lower, upper) |>
  mutate(type ="Profile", model="FLIC"),
do.call(data.frame, comp_flac_prof) |>
  select(estimate, lower, upper) |>
  mutate(type ="Profile", model="FLAC")
  )
all_comps

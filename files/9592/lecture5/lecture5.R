## POLSCI 9592, Lecture 5: Ordinal Data Models
## Code for the slides in lecture5.Rmd

library(rio)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ordinal)
library(marginaleffects)
library(car)
library(DAMisc)

# make sure select() is dplyr's, even if another package masks it
select <- function(...)dplyr::select(...)


## ---- The latent variable and its thresholds ----

# the unobserved continuum, y*
df1 <- data.frame(
  ystar = seq(-3,3,length=601),
  y = 1)
ggplot(df1) +
  geom_segment(aes(x=min(ystar), xend=max(ystar), y=y, yend=y),
    arrow=arrow(length=unit(.5, "cm"), ends="both")) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(y="", x="y*")

# what we observe: y* cut into four categories at three thresholds
df1 <- data.frame(
  ystar = seq(-3,3,length=601),
  y = 0)
cuts <- data.frame(
  x=c(-2,.5,2),
  y1=c(-.1,-.1,-.1),
  y2=c(.1,.1,.1)
)
ggplot(df1) +
  geom_segment(aes(x=min(ystar), xend=max(ystar),
                        y=y, yend=y),
    arrow=arrow(length=unit(.5, "cm"), ends="both")) +
  geom_segment(data=cuts, aes(x=x, xend=x, y=y1, yend=y2)) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=15)) +
  labs(y="", x="y*") +
  scale_x_continuous(breaks=c(-2,.5,2), labels=c(expression(tau[1]), expression(tau[2]), expression(tau[3]))) +
  annotate("text", x=c(-2.5, -.75, 1.25, 2.5), y=.05, label=c("1", "2", "3", "4"), size=5)


## ---- Ordered logit: state repression ----

# State Department repression score (1-5) as an ordered factor
dat <- rio::import("data/ologit_data.dta")
dat$sd_fac <- factor(dat$sd, levels=1:5)

# cumulative link (ordered logit) model; democracy enters as a quadratic
mod <- clm(sd_fac ~ cwarcow + iwarcow + logpop + logpcgnp + poly(vanadd, 2), data=dat)
summary(mod)

# likelihood ratio tests for each term (the poly() term is tested jointly)
Anova(mod)


## ---- Cumulative odds ratios ----

# exp(b): the multiplicative change in the odds of being above any
# threshold (not meaningful for the polynomial terms)
round(exp(coef(mod)[c("cwarcow", "iwarcow", "logpop", "logpcgnp")]), 2)

# check it: cumulative probabilities Pr(y <= j) without and with civil
# war, other variables at their means or modes
p <- predictions(mod,
  newdata = datagrid(cwarcow = c(0,1),
        grid_type="mean_or_mode"))

p <- p |> select(group, cwarcow, estimate) |>
  as_tibble() |>
  mutate(estimate = cumsum(estimate),
         .by=cwarcow) |>
  filter_out(group == 5) |>
  pivot_wider(names_from = "group",
              values_from = "estimate",
              names_prefix = "gp_")

p

# odds of being above each threshold, civil war vs. none; all four
# equal exp(b) for cwarcow
((1-p$gp_1[2])/p$gp_1[2])/((1-p$gp_1[1])/p$gp_1[1])
((1-p$gp_2[2])/p$gp_2[2])/((1-p$gp_2[1])/p$gp_2[1])
((1-p$gp_3[2])/p$gp_3[2])/((1-p$gp_3[1])/p$gp_3[1])
((1-p$gp_4[2])/p$gp_4[2])/((1-p$gp_4[1])/p$gp_4[1])


## ---- Discrete changes: MER (reasonable values) ----

# change in each category's probability, other variables at their medians
mer_comps <- comparisons(mod, newdata = "median",
                         # "2sd": x - 1 SD to x + 1 SD; "iqr": 25th to 75th
                         # percentile (2sd would push democracy below 0)
                         variables=list(cwarcow = c(0,1),
                                        iwarcow = c(0,1),
                                        logpop = "2sd",
                                        logpcgnp = "2sd",
                                        vanadd = "iqr"),
                         type="prob")

# one row per variable, one column per category
mer_comps %>%
  # * = 95% confidence interval excludes 0
  mutate(estimate = sprintf("%.2f%s", estimate, ifelse(sign(conf.low) == sign(conf.high), "*", ""))) %>%
  select(term, contrast, group, estimate) %>%
  pivot_wider(names_from = "group", values_from = "estimate")

# the same changes with confidence intervals
ggplot(mer_comps, aes(x=as.factor(group), y=estimate,
                     ymin=conf.low, ymax=conf.high)) +
  geom_pointrange(size=.1) +
  geom_hline(yintercept=0, linetype=3) +
  facet_wrap(~term, ncol=2) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  labs(x="", y="Change in Predicted Probability")


## ---- Discrete changes: AME (observed values) ----

# make each change for every observation, then average
ave_comps <- avg_comparisons(mod,
                         # "2sd": x - 1 SD to x + 1 SD; "iqr": 25th to 75th
                         # percentile (2sd would push democracy below 0)
                         variables=list(cwarcow = c(0,1),
                                        iwarcow = c(0,1),
                                        logpop = "2sd",
                                        logpcgnp = "2sd",
                                        vanadd = "iqr"),
                         type="prob")
ave_comps %>%
  # * = 95% confidence interval excludes 0
  mutate(estimate = sprintf("%.2f%s", estimate, ifelse(sign(conf.low) == sign(conf.high), "*", ""))) %>%
  select(term, contrast, group, estimate) %>%
  pivot_wider(names_from = "group", values_from = "estimate")

# MER and AME changes side by side
bcomps <- bind_rows(
  mer_comps %>%
    select(term, group, estimate, conf.low, conf.high) %>%
    mutate(type = "MER (medians)"),
  ave_comps %>%
    select(term, group, estimate, conf.low, conf.high) %>%
    mutate(type = "AME (observed)"))

ggplot(bcomps, aes(x=as.factor(group), y=estimate,
                     ymin=conf.low, ymax=conf.high,
                  colour=type)) +
  geom_pointrange(size=.05,
                  position=position_dodge(width=.5)) +
  geom_hline(yintercept=0, linetype=3) +
  facet_wrap(~term, ncol=2) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        legend.position="bottom") +
  labs(x="", y="Change in Predicted Probability")


## ---- Predicted probabilities: MER ----

# 100 evenly spaced values across the range of x
seq_range <- function(x,n=100){
  mn <- min(x, na.rm=TRUE)
  mx <- max(x, na.rm=TRUE)
  seq(mn, mx, length=n)
}

# probability of each category across log GNP/capita, other variables
# at their medians
pred_gnp <- predictions(mod,
  newdata = datagrid(logpcgnp = seq_range(dat$logpcgnp),
                     FUN_numeric = median))
ggplot(pred_gnp, aes(x=logpcgnp, y=estimate,
                     ymin=conf.low,
                     ymax=conf.high,
                     fill=as.factor(group),
                     colour=as.factor(group))) +
  geom_ribbon(alpha=.15, colour="transparent") +
  geom_line() +
  theme_classic() +
  theme(legend.position="bottom") +
  labs(x="log(GNP/capita)", y="Predicted Probability",
       colour = "Level of Repression",
       fill = "Level of Repression")

# the same across democracy; predictions() handles the poly() term
pred_dem <- predictions(mod,
  newdata = datagrid(vanadd = seq_range(dat$vanadd),
                     FUN_numeric = median))

ggplot(pred_dem, aes(x=vanadd, y=estimate,
                     ymin=conf.low,
                     ymax=conf.high,
                     fill=as.factor(group),
                     colour=as.factor(group))) +
  geom_ribbon(alpha=.15, colour="transparent") +
  geom_line() +
  theme_classic() +
  theme(legend.position="bottom") +
  labs(x="Democracy (Vanhanen index)",
       y="Predicted Probability",
       colour = "Level of Repression",
       fill = "Level of Repression")


## ---- Predicted probabilities: AME ----

# set everyone's GNP to each value in turn and average the predictions
# over the observed values of the other variables
pred_gnp2 <- avg_predictions(mod,
  variables=list(logpcgnp=seq_range(dat$logpcgnp)))

ggplot(pred_gnp2, aes(x=logpcgnp, y=estimate,
                     ymin=conf.low,
                     ymax=conf.high,
                     fill=as.factor(group),
                     colour=as.factor(group))) +
  geom_ribbon(alpha=.15, colour="transparent") +
  geom_line() +
  theme_classic() +
  theme(legend.position="bottom") +
  labs(x="log(GNP/capita)", y="Predicted Probability",
       colour = "Level of Repression",
       fill = "Level of Repression")


## ---- Model fit ----

# predicted probability of each observation's observed category, by
# observed category
probs <- predictions(mod, newdata=dat)
probs <- probs %>%  filter(group == sd)
ggplot(probs, aes(x=estimate)) +
  geom_histogram(aes(y=after_stat(ndensity)), bins=15) +
  facet_wrap(~sd, nrow=3) +
  theme_bw() +
  theme(panel.grid=element_blank())

# pseudo-R2 measures for ordinal models
ordfit(mod, data=dat)

# PRE and expected PRE; pre() needs the polr() version of the model
p <- MASS::polr(formula(mod), data=dat)
pre(p, data=dat)


## ---- Why the scale matters (binary logit simulation) ----

# 2,500 samples with x and z independent; the simulations were run with
# coef_rescale.R and the results saved

# logit coefficient on x without and with z: adding z makes it larger
load("data/coef_sim.rda")
coef_siml2 %>%
  filter(model == "Logit") %>%
  ggplot(aes(x=Simple, y=Multiple)) +
  geom_abline(intercept=0, slope=1, col="red") +
  geom_point(alpha=.1) +
  labs(x="Without z", y="With z") +
  facet_wrap(~"Logit coefficient on x") +
  theme_bw()

# the AME of x without and with z: it stays on the 45-degree line
load("data/me_sim.rda")
ggplot(me_sim, aes(x=Logit_Simple, y=Logit_Multiple)) +
  geom_abline(intercept=0, slope=1, col="red") +
  geom_point(alpha=.1) +
  labs(x="Without z", y="With z") +
  facet_wrap(~"AME of x (first difference)") +
  theme_bw()


## ---- Equally spaced thresholds ----

# LR test of equally spaced thresholds (the ordinal analogue of what OLS
# on the codes 1-5 assumes) against freely estimated ones
mod_eq <- update(mod, threshold = "equidistant")
eq_lr <- 2*(as.numeric(logLik(mod)) - as.numeric(logLik(mod_eq)))
c(LR = round(eq_lr, 1), df = 2, p = round(pchisq(eq_lr, 2, lower.tail = FALSE), 3))


## ---- Testing parallel regressions: nominal_test() ----

# center so the thresholds are
# evaluated inside the data
dat$logpop_c <- dat$logpop -
  mean(dat$logpop)
dat$logpcgnp_c <- dat$logpcgnp -
  mean(dat$logpcgnp)
mod_c <- clm(sd_fac ~ cwarcow + iwarcow +
  logpop_c + logpcgnp_c + poly(vanadd, 2),
  data=dat)

# for each term, LR test of letting its slopes differ across thresholds
nt <- nominal_test(mod_c)
round(as.data.frame(nt)[-1,
  c("Df", "LRT", "Pr(>Chi)")], 3)


## ---- Omnibus test ----

# free every slope at once and compare with the ordered logit
# ((J-2) x K = 3 x 6 = 18 df)
full <- clm(sd_fac ~ 1, nominal = ~ cwarcow + iwarcow + logpop_c +
  logpcgnp_c + poly(vanadd, 2), data=dat)
omni <- 2*(as.numeric(logLik(full)) - as.numeric(logLik(mod_c)))
data.frame(LR = round(omni, 1), df = 18,
           p = signif(pchisq(omni, 18, lower.tail = FALSE), 3))


## ---- Simulation: is chi-square the right reference distribution? ----

# simulate outcomes from the fitted ordered logit, where parallel
# regressions holds by construction, refit, and record each test statistic.
# This takes several minutes; the results are saved to
# data/nominal_stats.rda, so you can skip the loop and load them below.
probs <- predict(mod_c, type = "prob",
  newdata = select(dat, -sd_fac))$fit
nom_stats <- matrix(NA, 2500, 6)
# a loop, not replicate(): nominal_test()
# refits the model using the global data
for(i in 1:2500){
  dat$newy <- factor(apply(probs, 1,
    function(p)sample(1:5, 1, prob = p)),
    levels = 1:5)
  u <- update(mod_c, newy ~ .)
  nt_sim <- nominal_test(u)
  f_sim <- update(full, newy ~ .)
  omni_sim <- 2*(as.numeric(logLik(f_sim)) -
                 as.numeric(logLik(u)))
  # drop if separation stops the full model converging
  if(f_sim$convergence$code != 0) omni_sim <- NA
  nom_stats[i, ] <- c(nt_sim$LRT[-1], omni_sim)
}
colnames(nom_stats) <- c(rownames(nt_sim)[-1],
                         "Omnibus")
save(nom_stats, file = "data/nominal_stats.rda")

# simulated densities of each statistic against the chi-square
load("data/nominal_stats.rda")
ns <- as.data.frame(nom_stats) %>%
  pivot_longer(everything(), names_to = "term",
               values_to = "LR") %>%
  filter(!is.na(LR)) %>%
  mutate(df = case_when(
    term == "poly(vanadd, 2)" ~ 6,
    term == "Omnibus" ~ 18,
    TRUE ~ 3))
theo <- ns %>% distinct(term, df) %>%
  reframe(x = seq(.05, qchisq(.999, df),
                  length = 200),
          .by = c(term, df)) %>%
  mutate(dens = dchisq(x, df))
ggplot(ns, aes(x = LR)) +
  stat_density(aes(colour = "Empirical"),
               geom = "line") +
  geom_line(data = theo, aes(x = x, y = dens,
                    colour = "Theoretical")) +
  facet_wrap(~term, ncol = 3, scales = "free") +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "LR Statistic", y = "Density",
       colour = "")

# share of simulated statistics above the chi-square .05 critical value
# (should be about .05 if the chi-square reference is right)
crit <- qchisq(.95, c(3, 3, 3, 3, 6, 18))
rej <- colMeans(sweep(nom_stats, 2, crit, ">"), na.rm = TRUE)
rej

# simulated p-values (counting the observed statistic as one draw)
# next to the asymptotic ones
obs_res <- c(nt[-1, "LRT"], omni)
B <- colSums(!is.na(nom_stats))
sim_p <- (colSums(sweep(nom_stats, 2, obs_res, ">"), na.rm = TRUE) + 1)/(B + 1)
asy_p <- c(nt[-1, "Pr(>Chi)"], pchisq(omni, 18, lower.tail = FALSE))
fmt <- function(p) ifelse(p < .001, "<.001", sprintf("%.3f", p))
data.frame(simulation = fmt(sim_p), asymptotic = fmt(asy_p),
           row.names = colnames(nom_stats))


## ---- Unequal latent variances: scale_test() ----

# for each term, LR test of letting it change the latent variance
st <- scale_test(mod)
round(as.data.frame(st)[-1, c("Df", "LRT", "Pr(>Chi)")], 3)


## ---- Location-scale model ----

# the war variables in the scale part: log(sigma) = z * gamma
mod_ls <- update(mod_c,
  scale = ~ cwarcow + iwarcow)
# ratio of latent SDs
round(exp(mod_ls$zeta), 2)
# remaining parallel-regressions violations
nt <- nominal_test(mod_ls)
round(as.data.frame(nt)[-1,
  c("Df", "LRT", "Pr(>Chi)")], 3)

# could scale effects explain the rest? put every variable in the scale
# part and test parallel regressions again
mod_all <- update(mod_c, scale = ~ cwarcow + iwarcow + logpop_c + logpcgnp_c + poly(vanadd, 2))
all_lr <- 2*(as.numeric(logLik(mod_all)) - as.numeric(logLik(mod_ls)))
c(LR = round(all_lr, 1), df = 4, p = round(pchisq(all_lr, 4, lower.tail = FALSE), 3))
nt_all <- nominal_test(mod_all)
round(as.data.frame(nt_all)[-1, c("Df", "LRT", "Pr(>Chi)")], 3)


## ---- Stereotype model ----

# VGAM's formula needs democracy and its square as separate variables
dat$va <- as.numeric(scale(dat$vanadd))
dat$va2 <- dat$va^2
f <- sd_fac ~ cwarcow + iwarcow + logpop +
  logpcgnp + va + va2

# rank-1 reduced-rank multinomial logit = one-dimensional stereotype
# model, with category 1 as the baseline
st1 <- VGAM::rrvglm(f, VGAM::multinomial(refLevel=1),
                    data=dat, Rank=1)
# VGAM fixes phi_2 = 1; rescale so phi_5 = 1
phi <- c(0, VGAM::Coef(st1)@A[,1])
setNames(round(phi/phi[5], 2), 1:5)


## ---- Comparing the models ----

# adjacent-category logit with common slopes = stereotype model with
# equally spaced scores
ac  <- VGAM::vglm(f, VGAM::acat(parallel=TRUE), data=dat)      # equally spaced scores

# log-likelihoods, parameter counts, AIC and BIC
ll <- c(ologit = as.numeric(logLik(mod)), loc_scale = as.numeric(logLik(mod_ls)),
        adjacent = logLik(ac), stereotype = logLik(st1))
k  <- c(10, 12, 10, 13)
data.frame(logLik = round(ll, 1), params = k, AIC = round(-2*ll + 2*k, 1),
           BIC = round(-2*ll + log(nrow(dat))*k, 1))

# LR test of equal spacing (adjacent-category vs. stereotype, 3 df)
2*(ll[["stereotype"]] - ll[["adjacent"]])


## ---- Predicted probabilities from three models ----

# profile: no war, population and democracy at their medians, GNP varying;
# the VGAM models need the standardized democracy variables
nd <- data.frame(cwarcow = 0, iwarcow = 0, logpop = median(dat$logpop),
                 vanadd = median(dat$vanadd), logpcgnp = seq_range(dat$logpcgnp))
nd$va <- (nd$vanadd - mean(dat$vanadd))/sd(dat$vanadd)
nd$va2 <- nd$va^2

# predicted probabilities; use VGAM::predictvglm(), since plain predict()
# ignores type = "response" for vglm models when VGAM isn't attached
p_ol <- predict(mod, newdata = nd, type = "prob")$fit
p_ac <- VGAM::predictvglm(ac, newdata = nd, type = "response")
p_st <- VGAM::predictvglm(st1, newdata = nd, type = "response")

# stack into long form: one row per model, GNP value and category
preds <- bind_rows(
  as.data.frame(p_ol) %>% mutate(model = "Ordered logit"),
  as.data.frame(p_ac) %>% setNames(1:5) %>% mutate(model = "Adjacent-category"),
  as.data.frame(p_st) %>% setNames(1:5) %>% mutate(model = "Stereotype")) %>%
  mutate(logpcgnp = rep(nd$logpcgnp, 3),
         model = factor(model, levels = c("Ordered logit", "Adjacent-category", "Stereotype"))) %>%
  pivot_longer(`1`:`5`, names_to = "category", values_to = "prob") %>%
  mutate(category = paste("Repression =", category))

ggplot(preds, aes(x = logpcgnp, y = prob, colour = model, linetype = model)) +
  geom_line(linewidth = .8) +
  facet_wrap(~category, nrow = 1) +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = "bottom") +
  labs(x = "log(GNP/capita)", y = "Predicted Probability", colour = "", linetype = "")

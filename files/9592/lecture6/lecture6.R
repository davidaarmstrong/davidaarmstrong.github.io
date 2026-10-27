## POLSCI 9592, Lecture 6: Unordered Choice Models
## Code for the slides in lecture6.Rmd

library(rio)
library(dplyr)
library(tidyr)
library(ggplot2)
library(marginaleffects)
library(nnet)
library(DAMisc)
library(factorplot)
library(fastDummies)
library(dfidx)
library(mlogit)
library(brglm2)
library(uwo9592)

# make sure select() is dplyr's, even if another package masks it
select <- function(...)dplyr::select(...)


## ---- France data ----

# French election study; factorize() turns labelled values into factors
dat <- import("data/france_mnl.dta")
dat <- factorize(dat)

# variable names and their labels
labs <- sapply(1:ncol(dat), function(i)attr(dat[[i]], "label"))
data.frame(var = names(dat), description = labs)


## ---- Is there an order? One-dimensional stereotype model ----

# the same complete cases are used for every model in this section
f <- vote ~ lrself + male + retnat +
  age + union
d <- na.omit(dat[, all.vars(f)])

# rank-1 reduced-rank (stereotype) multinomial logit, PCF as the baseline
rr1 <- VGAM::rrvglm(f,
  VGAM::multinomial(refLevel=1),
  data=d, Rank=1)

# party scores, phi (0 for the baseline); VGAM fixes phi_PS = 1,
# so rescale so phi_UMP = 1
phi <- c(0, VGAM::Coef(rr1)@A[,1])

# mean perceived left-right positions of the parties
perc <- colMeans(dat[, c("rp10", "rp20",
  "rp50", "rp70", "rp73", "rp80")],
  na.rm=TRUE)

# do the estimated scores line up with where voters place the parties?
data.frame(perceived = round(perc, 2),
  phi = round(phi/phi[5], 2),
  row.names = levels(d$vote))


## ---- How many dimensions? ----

# rr_ladder() (uwo9592) fits every model from the ordered logit to the MNL
# on the same cases and reports each one's share of the MNL's fit
lad <- rr_ladder(f, data = d)


## ---- Two dimensions: France ----

# the rank-2 fit from the ladder -
# any anchoring gives the same result
rr2 <- lad$fits$R2

# whitened, principal-axes biplot; orient puts UMP* at the top of axis 1
# and PS at the top of axis 2
bp <- rr_biplot(rr2,
  orient=c("UMP*", "PS"),
  var_labels=c("lrself", "male",
    "retnat: same", "retnat: worse",
    "age", "union"), plot=FALSE)

# share of the spread between parties
round(bp$share, 3)

# party positions on each axis
round(bp$positions, 2)


## ---- Structure correlations and the biplot ----

# correlations of each predictor
# with each axis - the arrows
round(bp$structure, 2)

bp$plot


## ---- Comparing one, two and five dimensions ----

# average predicted probability of each party as everyone's left-right
# self-placement is set to 0, 1, ..., 10
lrs <- 0:10
ame_lrs <- function(fit){
  t(sapply(lrs, function(l)
    colMeans(VGAM::predictvglm(fit,
      newdata = mutate(d, lrself = l),
      type = "response"))))
}

# stack the rank-1, rank-2 and MNL predictions in long form
comp <- bind_rows(
  data.frame(lrself = lrs, ame_lrs(rr1),
    model = "R = 1", check.names = FALSE),
  data.frame(lrself = lrs, ame_lrs(rr2),
    model = "R = 2", check.names = FALSE),
  data.frame(lrself = lrs, ame_lrs(lad$fits$MNL),
    model = "MNL (R = 5)", check.names = FALSE)) %>%
  pivot_longer(PCF:FN, names_to = "party",
               values_to = "estimate") %>%
  mutate(party = factor(party, levels = levels(d$vote)),
    model = factor(model,
      levels = c("R = 1", "R = 2", "MNL (R = 5)")))

ggplot(comp, aes(x=lrself, y=estimate,
                 colour=model, linetype=model)) +
  geom_line(linewidth=.8) +
  facet_wrap(~party, ncol=2) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        legend.position="bottom") +
  labs(x="Left-right Self-placement",
       y="Predicted Pr(Vote)", colour="", linetype="")


## ---- Which distinctions need more dimensions? ----

# set variable v to val for every observation (keeping factor levels)
set_var <- function(data, v, val){
  data[[v]] <- if(is.factor(data[[v]])) factor(rep(val, nrow(data)), levels = levels(data[[v]])) else val
  data
}

# predicted probabilities and the average first difference from lo to hi
pr <- function(m, nd) VGAM::predictvglm(m, newdata = nd, type = "response")
fd <- function(m, v, lo, hi) colMeans(pr(m, set_var(d, v, hi)) - pr(m, set_var(d, v, lo)))

# low and high values: mean -/+ 1 SD for the numeric variables
lo <- list(lrself = mean(d$lrself) - sd(d$lrself), union = "no, not a union member",
           age = mean(d$age) - sd(d$age), retnat = "better")
hi <- list(lrself = mean(d$lrself) + sd(d$lrself), union = "yes, union member",
           age = mean(d$age) + sd(d$age), retnat = "worse")

# one variable-party pair per row, one model per column
cases <- list(c("lrself", "PS"), c("union", "PCF"), c("age", "Green"), c("retnat", "FN"))
mods <- lad$fits[c("R1", "R2", "R3", "R4", "MNL")]
names(mods) <- c(paste("R =", 1:4), "MNL")
tab <- sapply(mods, function(m) sapply(cases, function(x) fd(m, x[1], lo[[x[1]]], hi[[x[1]]])[[x[2]]]))
rownames(tab) <- sapply(cases, paste, collapse = " -> ")
round(tab, 3)


## ---- Multinomial logit ----

# PCF (the first level) is the baseline category
mod <- multinom(vote ~ lrself + male + retnat + age +
    union , data=dat, trace=F)

# coefficients with significance stars
noquote(t(mnlSig(mod)))


## ---- Comparing categories: factorplot ----

# all pairwise differences in the lrself coefficients across parties
plot(factorplot(mod, variable="lrself"))

# the same for retnat = worse
plot(factorplot(mod,
  variable="retnatworse"))

# both retnat dummies at once: pull out their coefficients and
# variance-covariance matrix and pass them to factorplot() directly
b <- c(t(coef(mod)))
v <- vcov(mod)
rn_inds <- grep("retnat", rownames(v))
b_rn <- b[rn_inds]
v_rn <- v[rn_inds, rn_inds]
names(b_rn) <- rownames(v_rn)
plot(factorplot(b_rn, var=v_rn, resdf=Inf),
     print.est=FALSE, print.se=FALSE,
     print.square.leg=FALSE, abbrev.char = 25)


## ---- Predicted probabilities: MER ----

# probability of each party across lrself, other variables at their
# means (numeric) or modes (factors)
pred_lrs <- predictions(mod,
            newdata=datagrid(grid_type = "mean_or_mode"),
            variables= list(lrself = seq_range(dat$lrself)))

# rowid indexes the lrself values; put them back in
pred_lrs <- as.data.frame(pred_lrs) |>
  mutate(lrself = seq_range(dat$lrself)[rowid])

ggplot(pred_lrs,
       aes(x=lrself,
           y=estimate,
           ymin = conf.low,
           ymax=conf.high)) +
  geom_ribbon(alpha=.2, colour="transparent") +
  geom_line() +
  facet_wrap(~group, ncol=2) +
  theme_bw() +
  theme(panel.grid=element_blank())


## ---- Predicted probabilities: AME ----

# set lrself for every observation, then average the predictions
ave_pred_lrs <- avg_predictions(mod,
            variables=list(lrself = seq_range(dat$lrself)))
ggplot(ave_pred_lrs,
       aes(x=lrself,
           y=estimate,
           ymin = conf.low,
           ymax=conf.high)) +
  geom_ribbon(alpha=.2, colour="transparent") +
  geom_line() +
  facet_wrap(~group, ncol=2) +
  theme_bw() +
  theme(panel.grid=element_blank())

# MER and AME curves together
all_lrs <- bind_rows(pred_lrs %>% mutate(type = "MER"),
          ave_pred_lrs %>% mutate(type = "AME"))
ggplot(all_lrs,
       aes(x=lrself,
           y=estimate,
           ymin = conf.low,
           ymax=conf.high,
           colour=type,
           fill=type)) +
  geom_ribbon(alpha=.2, colour="transparent") +
  geom_line() +
  facet_wrap(~group) +
  theme_bw() +
  theme(panel.grid=element_blank())


## ---- Discrete changes: MER and AME ----

# change in each party's probability, other variables at their medians;
# "2sd": x - 1 SD to x + 1 SD, "minmax": lowest to highest level
comps <- comparisons(mod, newdata="median",
            variables = list(lrself = "2sd",
                             male = c(0,1),
                             retnat = "minmax",
                             age = "2sd",
                             union = "reference"))

# one row per variable, one column per party; * = p < .05
comps %>%
  mutate(eff = sprintf("%.2f%s",
                       estimate,
                       ifelse(p.value < .05, "*", ""))) %>%
  select(2:4, eff) %>%
  pivot_wider(names_from="group", values_from="eff")

# the same changes made for every observation, then averaged
ave_comps <- avg_comparisons(mod,
      variables = list(lrself = "2sd",
       male = c(0,1),
       retnat = "minmax",
       age = "2sd",
       union = "reference"))
ave_comps %>%
  mutate(eff = sprintf("%.2f%s",
                       estimate,
                       ifelse(p.value < .05, "*", ""))) %>%
  select(1:3, eff) %>%
  pivot_wider(names_from="group", values_from="eff")


## ---- Model fit ----

# pseudo-R-squared measures
mnlfit(mod)

# proportional reduction in error
DAMisc::pre(mod)

# distribution of predicted probabilities by observed vote
probgroup(mod)

# observed vs. predicted (most likely) vote
y <- model.response(model.frame(mod))
yhat <- predict(mod)
table(y, yhat)


## ---- Conditional logit: data in long form ----

# rename the party-position variables rp.PCF, rp.PS, etc.
names(dat)[13:18]
names(dat)[13:18] <- paste("rp", levels(dat$vote), sep=".")
dat2 <- dat %>%
  dplyr::select(c("vote", "lrself", "urban", "union", "retnat",
           "age", "male", starts_with("rp"))) %>%
  na.omit()

# one 0/1 indicator per party, vote.PCF, vote.PS, etc.
dat2 <- dat2 %>%
  dummy_cols("vote", remove_selected_columns=TRUE)
names(dat2) <- gsub("vote_", "vote.", names(dat2))

# long form: one row per voter and party, with vote (0/1) and rp
# (that party's position)
dat2 <- dat2 %>%
  mutate(obs = row_number()) %>%
  pivot_longer(7:18,
               names_pattern = "(.*)\\.(.*)",
               names_to = c(".value", "alt"))

# mlogit needs a plain data frame, not a tibble
dat2l <- dfidx(as.data.frame(dat2), idx = c("obs", "alt"), choice = "vote")


## ---- Conditional logit model ----

# before the |: alternative-specific (distance from voter to party);
# after the |: individual-specific, one coefficient per party
mlogit2 <- mlogit(vote ~ 0 + I(abs(lrself - rp)) |  urban + union + retnat +
    age + male, data=dat2l, reflevel="PS")
printCoefmat(summary(mlogit2)$CoefTable)


## ---- Predicted probabilities: MER ----

# makeFakeData() (uwo9592): a typical voter at each lrself value, with
# the party positions (rp) varying across parties
lrs <- 0:10
fake <- makeFakeData(mlogit2,
                     dat2l,
                     change=list(lrself=lrs),
                     varying="rp")
probs <- predict(mlogit2, newdata=fake)
plot.dat <- data.frame(
  prob = c(t(probs)),
  party = factor(rep(colnames(probs),
                     length(lrs)),
                 levels=levels(dat$vote)),
  lrself = rep(lrs, each=6))
ggplot(plot.dat, aes(x=lrself, y=prob)) +
  geom_line() +
  facet_wrap(~party, ncol=2) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  labs(x="Left-right Self-placement",
       y="Predicted Pr(Vote)")

# confidence intervals by simulation: draw coefficients from their
# sampling distribution and predict with each draw
B <- MASS::mvrnorm(2500, coef(mlogit2), vcov(mlogit2))
tmp <- mlogit2
probs <- NULL
for(i in 1:2500){
  tmp$coefficients <- B[i,]
  probs <- rbind(probs,
   cbind(data.frame(lrself = unique(fake$lrself),
                    sim=i),
         predict(tmp,
                 newdata=fake)))
}

# mean and 95% interval across the draws
plot.dat <- probs %>%
  pivot_longer(PS:`UMP*`,
               names_to="party",
               values_to="prob") %>%
  mutate(party = factor(party,
                        levels=levels(dat$vote))) %>%
  group_by(lrself, party) %>%
  summarise(p = mean(prob),
            lwr= quantile(prob, .025),
            upr = quantile(prob, .975))

ggplot(plot.dat, aes(x=lrself,
                 y=p,
                 ymin = lwr,
                 ymax=upr)) +
  geom_ribbon(alpha=.25,
              colour="transparent") +
  geom_line() +
  facet_wrap(~party, ncol=2) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  labs(x="Left-right Self-placement",
       y="Predicted Pr(Vote)")


## ---- Predicted probabilities: AME ----

# simulated coefficients and a place to hold the results
B <- MASS::mvrnorm(2500, coef(mlogit2), vcov(mlogit2))
lrs <- 0:10
out <- data.frame(
  lrself = rep(lrs, each=6),
  party = rep(levels(index(dat2l)$alt), length(lrs)),
  p = NA,
  lwr = NA,
  upr = NA
)

# for each lrself value: set it for everyone, compute each voter's
# probabilities (6 rows per voter) and average them, at the estimates
# and for each simulated draw
for(j in lrs){
  tmp <- dat2l
  tmp$lrself <- j
  X <- model.matrix(
    update(mlogit2, data=tmp))

  b <- coef(mlogit2)
  XB <- X %*% t(B)
  Xb <- X %*% b
  p <- prop.table(matrix(exp(Xb), ncol=6, byrow=TRUE), 1)
  m <- colMeans(p)

  res <- NULL
  for(i in 1:ncol(XB)){
    exb <- exp(matrix(XB[,i], ncol=6, byrow=TRUE))
    p <- prop.table(exb, 1)
    res <- rbind(res, colMeans(p))
  }

  l <- apply(res, 2, quantile, .025)
  u <- apply(res, 2, quantile, .975)
  out$p[which(out$lrself == j)] <- m
  out$lwr[which(out$lrself == j)] <- l
  out$upr[which(out$lrself == j)] <- u
}
out$party <- factor(out$party, levels=levels(dat$vote))

ggplot(out, aes(x=lrself, y=p,
                ymin = lwr, ymax=upr)) +
  geom_ribbon(alpha=.25, colour="transparent") +
  geom_line() +
  facet_wrap(~party) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  labs(x="Left-right Self-placement",
       y="Predicted Pr(Vote)")


## ---- First differences: MER ----

# typical voter at lrself = 0 and 10
fake <- makeFakeData(mlogit2,
                     dat2l,
                     change=list(lrself=c(0,10)),
                     varying = "rp")
B <- MASS::mvrnorm(2500, coef(mlogit2), vcov(mlogit2))

tmp <- mlogit2
probs <- NULL
for(i in 1:2500){
  tmp$coefficients <- B[i,]
  probs <- rbind(probs,
   cbind(data.frame(lrself = unique(fake$lrself),
                    sim=i),
         predict(tmp,
                 newdata=fake)))
}

# difference (10 - 0) for each draw, then its mean, 95% interval and
# one-sided p-value
fd <- probs %>%
  group_by(sim) %>%
  summarise(across(PS:`UMP*`, ~diff(.x))) %>%
  ungroup %>%
  summarise(across(-sim, list(mean = ~mean(.x),
                              lwr = ~quantile(.x, .025),
                              upr = ~quantile(.x, .975),
                              pval = ~mean(.x < 0)))) %>%
  mutate(across(contains("pval"), ~ifelse(.x > .5, 1-.x, .x))) %>%
  pivot_longer(everything(),
               names_pattern="(.*)_(.*)",
               names_to=c("party", ".value"))

fd %>%
  mutate(across(-party,
                ~round(.x, 2)))


## ---- First differences: AME ----

# everyone at lrself = 0 and everyone at lrself = 10
fake1 <- fake0 <- dat2l
fake0$lrself <- 0
fake1$lrself <- 10

# average difference for each simulated draw
B <- MASS::mvrnorm(100, coef(mlogit2), vcov(mlogit2))
tmp <- mlogit2
res <- NULL
for(i in 1:100){
  tmp$coefficients <- B[i,]
  p_hat0 <- colMeans(predict(tmp, newdata=fake0))
  p_hat1 <- colMeans(predict(tmp, newdata=fake1))
  res <- rbind(res, p_hat1-p_hat0)
}

fd_ame <- as.data.frame(res) %>%
  summarise(across(everything(),
                   list(p = ~mean(.x),
                        lwr = ~quantile(.x, .025),
                        upr = ~quantile(.x, .975),
                        pval = ~mean(.x > 0)))) %>%
  mutate(across(contains("pval"),
                ~ifelse(.x > .5, 1-.x, .x))) %>%
  pivot_longer(everything(),
               names_pattern="(.*)_(.*)",
               names_to=c("party", ".value"))

fd_ame %>%
  mutate(across(-party,
                ~round(.x, 2)))


## ---- Conditional logit fit ----

# proportional reduction in error: most likely party vs. observed vote
yhat <- predict(mlogit2,newdata=dat2l)
yhat <- colnames(yhat)[apply(yhat, 1, which.max)]
yhat <- factor(yhat, levels=levels(dat$vote))
obs_vote <- dat2l %>%  filter(vote == 1) %>% ungroup %>% unnest(idx) %>% select(alt) %>% pull()
tab <- table(obs_vote, yhat)
pcp <- sum(obs_vote == yhat)/sum(tab)   # percent correctly predicted
pmc <- max(table(obs_vote))/sum(tab)    # percent in the modal category
(pcp-pmc)/(1-pmc)


## ---- Does proximity matter? ----

# same model without the party-position term
mlogit0 <- mlogit(vote ~ 0 | urban + union + retnat + age + male,
                  data=dat2l, reflevel="PS")

# likelihood ratio test
x2 <- 2*as.numeric(logLik(mlogit2) - logLik(mlogit0))
data.frame(LR = round(x2, 1), df = 1, p = signif(pchisq(x2, 1, lower.tail=FALSE), 2))


## ---- Separation ----

# adding satisfaction with democracy produces huge coefficients and
# standard errors: (quasi-)separation
mods <- multinom(vote ~ lrself + male + retnat + age +
    union + demsat, data=dat, trace=F)
mnlSig(mods)

# bias-reduced multinomial logit (brglm2); the slides load a saved copy
# mods_br <- brmultinom(formula(mods), data=dat)
load("data/mods_br.rda")
noquote(t(mnlSig(mods_br)))

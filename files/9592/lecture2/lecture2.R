## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------------------------------
## Packages used anywhere in the script, all loaded up front.
library(car)
library(tibble)
library(ggplot2)
library(modelsummary)
library(dplyr)
library(tidyr)
library(flextable)
library(rio)
library(marginaleffects)

## ----echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------
## Load the ANES 2008 turnout data and turn labelled variables into factors.
## Plot age against (jittered) turnout, with the observed mean turnout at
## each age overlaid in red -- this is the motivating picture for "how do
## we map age onto the mean of a binary y?"
load("anes_2008_binary.rda")
dat <- factorize(dat)
dat_m <- dat |> summarise(age = mean(age), .by=voted)
dat_m <- bind_rows(dat_m,
                   dat |> summarise(across(c(age, voted), mean)))
ggplot(dat, aes(x=age, y=voted)) +
  geom_point(alpha=.25,
             position = position_jitter(height=.05)) +
  geom_point(data=dat_m, color = "red", size=3.5) +
  theme_classic() +
  scale_y_continuous(breaks=c(0,1),
                     labels=c("No", "Yes")) +
  labs(x="Age", y="Turnout")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
## Linear Probability Model (LPM): regress the binary voted variable
## directly on age with OLS.
mod <- lm(voted ~ age,
   data=dat)
summary(mod)


## ----echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------
## Show the fitted LPM line over the (jittered) scatterplot.
ggplot(dat, aes(x=age, y=voted)) +
  geom_point(alpha=.25, position=position_jitter(height=.025)) +
  geom_smooth(method="lm", se=FALSE) +
  theme_classic() +
  scale_y_continuous(breaks=c(0,1),
                     labels=c("No", "Yes")) +
  labs(x="Age", y="Turnout")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
## A better-specified LPM: add education, income, ideological strength
## (distance from the midpoint of the left-right scale), gender, and race.
dat$ideo_strength <- abs(dat$leftright-5)
mod <- lm(voted ~ age + educ + income +
            ideo_strength + female + race,
          data=dat)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
summary(mod)


## ----echo=FALSE, out.width="90%", fig.align="center"-------------------------------------------------------------------------------------------------------
## Toy example: unwind a simple log-odds model (log-odds = -1 + 2x) back
## into predicted probabilities with plogis(), to show the resulting
## S-shaped curve.
x <- seq(-2,3,length=100)
f <- plogis(-1+2*x)
ggplot(mapping = aes(x=x, y=f)) +
  geom_line() +
  theme_classic() +
  labs(x="x", y="Predicted Pr(Y=1|x)")


## ----"age_turn1", echo=TRUE, eval=FALSE, out.width="90%", fig.align="center"-------------------------------------------------------------------------------
## Observed turnout at each individual age value, connected by a line --
## the "rawest" possible look at Pr(Voted | Age).
dat_ag <- dat %>%
  group_by(age) %>%
  summarise(turnout = mean(voted, na.rm=TRUE))
ggplot(dat_ag, aes(x=age, y=turnout)) +
  geom_line(col="black") +
  theme_classic() +
  labs(x="Age", y="Pr(Voted | Age)")


## ----"age_turn2", echo=TRUE, eval=FALSE, out.width="90%", fig.align="center"-------------------------------------------------------------------------------
## Smooth out the noisy age-by-age turnout line with a loess smoother.
ggplot() +
  geom_line(data = dat_ag,
            aes(x=age, y=turnout),
            col="black") +
  geom_smooth(data = dat,
              aes(x=age, y=voted),
              method="loess",
              se=TRUE,
              fill="red",
              color="red") +
  theme_classic() +
  labs(x="Age", y="Pr(Voted | Age)")


## ----"age_turn3", echo=TRUE, eval=FALSE, out.width="90%", fig.align="center"-------------------------------------------------------------------------------
## Overlay the loess smoother and a logistic GLM smoother, to show they
## agree closely over the observed range of age.
ggplot() +
  geom_line(data = dat_ag,
            aes(x=age, y=turnout),
            col="black") +
  geom_smooth(data = dat,
              aes(x=age, y=voted,
                  fill="LOESS",
                  color="LOESS"),
              method="loess",
              se=TRUE) +
  geom_smooth(data = dat,
              aes(x=age, y=voted,
                  fill="GLM",
                  color="GLM"),
              method="glm",
              se=TRUE,
              method.args=list(family=binomial)) +
  scale_fill_manual(values=c("blue", "red")) +
  scale_colour_manual(values=c("blue", "red")) +
  theme_classic() +
  theme(legend.position="top") +
  labs(x="Age",
       y="Pr(Voted | Age)",
       colour="Model",
       fill="Model")


## ----"age_turn4", echo=TRUE, eval=FALSE, out.width="90%", fig.align="center"-------------------------------------------------------------------------------
## Extend the fitted logit curve well beyond the observed age range
## (-200 to 200) to reveal the full S-shape; the gray box marks the
## actual 17-90 age range used to fit the model, which only covers the
## middle, near-linear part of the curve.
mod <- glm(voted ~ age, data=dat, family=binomial)
b <- mod$coef
s <- seq(-200, 200, length = 1000)
p <- plogis(b[1] + b[2] * s)
ggplot(mapping=aes(x=s, y=p)) +
  geom_line() +
  geom_polygon(mapping=aes(x=c(17,90,90,17,17),
                           y=c(0,0,1,1,0)),
               fill="gray50",
               alpha=.25) +
  theme_classic() +
  labs(x="Age", y="Pr(Voted | Age)")

## ----fullmod, echo=T---------------------------------------------------------------------------------------------------------------------------------------
## The model we'll use for the rest of the lecture: logistic regression
## of turnout on age and race.
mod <- glm(voted ~ age + race,
           data=dat,
           family=binomial(link="logit"))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
summary(mod)


## ----odds2probs, echo=F,  out.width="80%", fig.align="center", fig.height=5, fig.width=15------------------------------------------------------------------
## For each race category, compute predicted probabilities at ages 18-78
## and again 10 years older, and plot the change as a set of vertical
## segments -- shows that a constant log-odds change (a constant
## coefficient) implies a *non*-constant change in predicted probability.
eg0 <- expand.grid(
  age=seq(18,78, by=1),
  race = factor(1:3, labels=c("Other", "White", "Black"))
)
eg1 <- expand.grid(
  age=seq(18,78, by=1)+10,
  race = factor(1:3, labels=c("Other", "White", "Black"))
)
p0 <- predict(mod, newdata=eg0, type="response")
p1 <- predict(mod, newdata=eg1, type="response")
eg0$p0 <- p0
eg0$p1 <- p1

ggplot(eg0, aes(x=age,
                xend=age,
                y=p0,
                yend=p1
                )) +
  geom_segment() +
  facet_wrap(~race, ncol=3) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  labs(x="Age", y="Change in Predicted Probabiliy\nFor a 10-year Change in Age")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
## First Difference at Reasonable Values (FD at RV): a 10-year change in
## age for a 40-year-old White respondent.
comparisons(mod, newdata=datagrid(age = 40, race="White"), variables=list(age=10))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
## FD at RV for race: pairwise contrasts among race categories for a
## 45-year-old.
comparisons(mod, newdata=datagrid(age=45), variables=list(race="pairwise"))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
## Marginal Effect at Reasonable Values (ME at RV): the instantaneous
## slope (dy/dx) of age for a 40-year-old White respondent. Marginal
## effects only make sense for continuous variables, so this isn't done
## for race.
comparisons(mod, newdata=datagrid(age = 40, race="White"), variables="age", comparison = "dydx")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
## Average First Difference (AFD): compute the 10-year age FD for every
## observation in the data, then average.
avg_comparisons(mod, variables = list(age=10), comparison="difference")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
## AFD for race: pairwise contrasts, averaged over the sample.
avg_comparisons(mod, variables = list(race="pairwise"))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
## Average Marginal Effect (AME) of age.
avg_comparisons(mod, variables = "age", comparison="dydx")


## ----echo=FALSE, out.width="90%", fig.align="center"-------------------------------------------------------------------------------------------------------
## Distribution, across the sample, of the age discrete-change effect --
## this is variability *within* the sample, not sampling variability.
age_comps <- comparisons(mod, newdat=dat, variables="age")
ggplot(age_comps, mapping = aes(x=estimate)) +
  geom_histogram() +
  geom_vline(xintercept=mean(age_comps$estimate),
             col="red", linetype=2) +
  theme_classic() +
  labs(x="Effect") +
  ggtitle("Distribution of Effects in Sample")


## ----echo=FALSE, out.width="90%", fig.align="center"-------------------------------------------------------------------------------------------------------
## Contrast that with the *sampling* distribution of the average effect,
## obtained via simulation-based inference -- this is repeated-sampling
## variability, not variability across individuals.
infs <- avg_comparisons(mod, variables="age") %>%
  inferences(method="simulation") %>%
  posterior_draws()
ggplot(infs, aes(x=draw)) +
  geom_histogram() +
  geom_vline(xintercept=infs$estimate[1],
             col="red", linetype=2) +
  theme_classic() +
  labs(x="Effects") +
  ggtitle("Sampling Distribution of Average Effect")


## ----echo=FALSE, out.width="90%", fig.align="center"-------------------------------------------------------------------------------------------------------
## Same sample-level effect distribution as above, but colored by race --
## shows the pooled distribution is really a mix of three fairly distinct,
## race-specific distributions.
ggplot(age_comps, mapping = aes(x=estimate, fill=race)) +
  geom_histogram(position="identity", alpha=.25) +
  geom_vline(xintercept=mean(age_comps$estimate),
             col="red", linetype=2) +
  theme_classic() +
  theme(legend.position="top") +
  labs(x="Effect") +
  ggtitle("Distribution of Effects in Sample")



## ----echo=FALSE, out.width="90%", fig.align="center"-------------------------------------------------------------------------------------------------------
## Average age effect at each age, faceted by race, showing how the
## (non-constant) marginal effect of age itself varies by age and race.
age_comps %>%
  as.data.frame() %>%
  group_by(age, race) %>%
  summarise(eff = mean(estimate)) %>%
  ggplot(aes(x=age, y=eff)) +
  geom_line() +
  facet_wrap(~race, ncol=1) +
  theme_bw() +
  labs(x="Age", y="Effect")


## ----"age_eff", echo=TRUE, eval=FALSE, out.width="90%", fig.align="center"---------------------------------------------------------------------------------
## Effect plot, "Reasonable Values" approach: predicted probability of
## voting across the full age range, with everything else held at
## reasonable/representative values (the datagrid() default).
p_age <- predictions(mod,
          newdata=datagrid(age=18:90))

ggplot(p_age, aes(x=age, y=estimate,
                  ymax=conf.high,
                  ymin=conf.low)) +
  geom_ribbon(alpha=.25) +
  geom_line() +
  theme_classic() +
  labs(x="Age", y="Predicted Pr(Voted)")


## ----"ave_age_eff", echo=TRUE, eval=FALSE, out.width="90%", fig.align="center"-----------------------------------------------------------------------------
## Effect plot, "Average Effect" approach: predicted probability of
## voting at each age, averaged over the observed distribution of race
## in the sample, rather than holding race fixed.
ap_age <- avg_predictions(mod,
          variables = list(age=18:90))

ggplot(ap_age, aes(x=age, y=estimate,
                  ymax=conf.high,
                  ymin=conf.low)) +
  geom_ribbon(alpha=.25) +
  geom_line() +
  theme_classic() +
  labs(x="Age", y="Predicted Pr(Voted)")


## ----"both_age_eff", echo=TRUE, eval=FALSE, out.width="90%", fig.align="center"----------------------------------------------------------------------------
## Overlay both effect-plot approaches (Reasonable Values vs. Average
## Effect) to compare them directly.
age_both <- p_age %>%
  as.data.frame() %>%
  mutate(type="Average Case") %>%
  bind_rows(ap_age %>%
              as.data.frame() %>%
              mutate(type="Average Effect"))

ggplot(age_both, aes(x=age, y=estimate,
                  ymax=conf.high,
                  ymin=conf.low)) +
  geom_ribbon(aes(fill=type), alpha=.25) +
  geom_line(aes(color=type)) +
  theme_classic() +
  theme(legend.position="top") +
  labs(x="Age", y="Predicted Pr(Voted)",
       color="", fill="")


## ----"tabex", echo=TRUE, eval=FALSE, results='asis'--------------------------------------------------------------------------------------------------------
## Example write-up table: side-by-side logit coefficients and average
## first differences, formatted with modelsummary/flextable. Requires a
## small tidy.comparisons() method so modelsummary knows how to pull
## estimates out of a marginaleffects "comparisons" object.
tidy.comparisons <- function(x, ...){
  comps %>% select(term, estimate, std.error,
                   p.value, conf.low, conf.high)
}
registerS3method("tidy", "comparisons", tidy.comparisons)
comps <- avg_comparisons(mod) %>%
  mutate(term =c("age", "raceBlack", "raceWhite"))
f <- function(x) format(round(x, 3), big.mark=",")
gm <- list(
  list("raw" = "nobs", "clean" = "N", "fmt" = f),
  list("raw" = "logLik", "clean" = "LL", "fmt" = f),
  list("raw" = "aic", "clean" = "AIC", "fmt" = f),
  list("raw" = "bic", "clean" = "BIC", "fmt" = f))

modelsummary(
  list("GLM" = mod,
       "FD" = comps),
  estimate = c("{estimate}{stars}",
                  "{estimate}"),
  stars = c("*" = .05),
  coef_map = c("age" = "Age",
               "raceBlack" = "Race: Black",
               "raceOther" = "Race: Other",
               "raceWhite" = "Race: White",
               "(Intercept)" = "Constant"),
  gof_map = gm,
  notes = "* p < 0.05 (two-tailed)",
  output = "flextable"
) %>% autofit()


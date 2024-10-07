###################################
## Code for POLSCI 9592 Week 2   ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

## ----setup, include=FALSE----------------------------------------------------------------------
library(ggplot2)
library(modelsummary)
library(dplyr)
library(tidyr)

## ----echo=FALSE--------------------------------------------------------------------------------
library(car)
library(rio)
load("anes_2008_binary.rda")
dat <- factorize(dat)
ggplot(dat, aes(x=age, y=voted)) + 
  geom_point(alpha=.25, 
             position = position_jitter(height=.05)) + 
  theme_classic() + 
  scale_y_continuous(breaks=c(0,1), 
                     labels=c("No", "Yes")) + 
  labs(x="Age", y="Turnout")



## ----------------------------------------------------------------------------------------------
mod <- lm(voted ~ age, 
   data=dat)
summary(mod)


## ----echo=FALSE--------------------------------------------------------------------------------
ggplot(dat, aes(x=age, y=voted)) + 
  geom_point(alpha=.25, position=position_jitter(height=.025)) + 
  geom_smooth(method="lm", se=FALSE) + 
  theme_classic() + 
  scale_y_continuous(breaks=c(0,1), 
                     labels=c("No", "Yes")) + 
  labs(x="Age", y="Turnout")


## ----------------------------------------------------------------------------------------------
dat$ideo_strength <- abs(dat$leftright-5)
mod <- lm(voted ~ age + educ + income + 
            ideo_strength + female + race, 
          data=dat)


## ----------------------------------------------------------------------------------------------
summary(mod)


## ----echo=FALSE, out.width="90%", fig.align="center"-------------------------------------------
x <- seq(-2,3,length=100)
f <- plogis(-1+2*x)
ggplot(mapping = aes(x=x, y=f)) + 
  geom_line() + 
  theme_classic() + 
  labs(x="x", y="Predicted Pr(Y=1|x)")


## ----"age_turn1", echo=TRUE, eval=FALSE, out.width="90%", fig.align="center"-------------------
dat_ag <- dat %>%
  group_by(age) %>%
  summarise(turnout = mean(voted, na.rm=TRUE))
ggplot(dat_ag, aes(x=age, y=turnout)) +
  geom_line(col="black") +
  theme_classic() +
  labs(x="Age", y="Pr(Voted | Age)")

## ----"age_turn2", echo=TRUE, eval=FALSE, out.width="90%", fig.align="center"-------------------
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

## ----"age_turn3", echo=TRUE, eval=FALSE, out.width="90%", fig.align="center"-------------------
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


## ----"age_turn4", echo=TRUE, eval=FALSE, out.width="90%", fig.align="center"-------------------
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

## ----fullmod, echo=T---------------------------------------------------------------------------
mod <- glm(voted ~ age + race, 
           data=dat, 
           family=binomial(link="logit"))


## ----------------------------------------------------------------------------------------------
summary(mod)


## ----odds2probs, echo=F,  out.width="80%", fig.align="center", fig.height=5, fig.width=15------
eg0 <- expand.grid(
  age=seq(18,78, by=1), 
  race = factor(1:3, labels=c("White", "Black", "Other"))
)
eg1 <- expand.grid(
  age=seq(18,78, by=1)+10, 
  race = factor(1:3, labels=c("White", "Black", "Other"))
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


## ----------------------------------------------------------------------------------------------
library(marginaleffects)
comparisons(mod, newdata=datagrid(age = 40, race="White"), variables=list(age=10)) 


## ----------------------------------------------------------------------------------------------
comparisons(mod, newdata=datagrid(age=45), variables=list(race="pairwise"))


## ----------------------------------------------------------------------------------------------
comparisons(mod, newdata=datagrid(age = 40, race="White"), variables="age", comparison = "dydx") 


## ----------------------------------------------------------------------------------------------
avg_comparisons(mod, variables = list(age=10), comparison="difference")


## ----------------------------------------------------------------------------------------------
avg_comparisons(mod, variables = list(race="pairwise"))


## ----------------------------------------------------------------------------------------------
avg_comparisons(mod, variables = "age", comparison="dydx")


## ----echo=FALSE, out.width="90%", fig.align="center"-------------------------------------------
age_comps <- comparisons(mod, newdat=dat, variables="age")
ggplot(age_comps, mapping = aes(x=estimate)) + 
  geom_histogram() + 
  geom_vline(xintercept=mean(age_comps$estimate), 
             col="red", linetype=2) + 
  theme_classic() + 
  labs(x="Effect") + 
  ggtitle("Distribution of Effects in Sample")


## ----echo=FALSE, out.width="90%", fig.align="center"-------------------------------------------
infs <- avg_comparisons(mod, variables="age") %>% 
  inferences(method="simulation")
ggplot(mapping=aes(x=c(attr(infs, "posterior_draws")))) + 
  geom_histogram() + 
  geom_vline(xintercept=infs$estimate[1], 
             col="red", linetype=2) + 
  theme_classic() + 
  labs(x="Effects") + 
  ggtitle("Sampling Distribution of Average Effect")


## ----echo=FALSE, out.width="90%", fig.align="center"-------------------------------------------
ggplot(age_comps, mapping = aes(x=estimate, fill=race)) + 
  geom_histogram(position="identity", alpha=.25) + 
  geom_vline(xintercept=mean(age_comps$estimate), 
             col="red", linetype=2) + 
  theme_classic() + 
  theme(legend.position="top") + 
  labs(x="Effect") + 
  ggtitle("Distribution of Effects in Sample")



## ----echo=FALSE, out.width="90%", fig.align="center"-------------------------------------------
age_comps %>% 
  as.data.frame() %>% 
  group_by(age, race) %>% 
  summarise(eff = mean(estimate)) %>% 
  ggplot(aes(x=age, y=eff)) + 
  geom_line() + 
  facet_wrap(~race, ncol=1) + 
  theme_bw() + 
  labs(x="Age", y="Effect")


## ----"age_eff", echo=TRUE, eval=FALSE, out.width="90%", fig.align="center"---------------------
p_age <- predictions(mod,
          newdata=datagrid(age=18:90))

ggplot(p_age, aes(x=age, y=estimate,
                  ymax=conf.high,
                  ymin=conf.low)) +
  geom_ribbon(alpha=.25) +
  geom_line() +
  theme_classic() +
  labs(x="Age", y="Predicted Pr(Voted)")

## ----"ave_age_eff", echo=TRUE, eval=FALSE, out.width="90%", fig.align="center"-----------------
ap_age <- avg_predictions(mod,
          variables = list(age=18:90))

ggplot(ap_age, aes(x=age, y=estimate,
                  ymax=conf.high,
                  ymin=conf.low)) +
  geom_ribbon(alpha=.25) +
  geom_line() +
  theme_classic() +
  labs(x="Age", y="Predicted Pr(Voted)")


## ----"both_age_eff", echo=TRUE, eval=FALSE, out.width="90%", fig.align="center"----------------
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

## ----"tabex", echo=TRUE, eval=FALSE, results='asis'--------------------------------------------
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
)

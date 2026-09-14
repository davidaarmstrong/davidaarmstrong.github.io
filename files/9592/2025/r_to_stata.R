library(Statamarkdown)
library(ggplot2)
library(marginaleffects)
library(modelsummary)
library(dplyr)
library(tidyr)
library(car)
library(rio)

load("anes_2008_binary.rda")
dat <- factorize(dat)
mod <- glm(voted ~ age + race, 
           data=dat, 
           family=binomial(link="logit"))
summary(mod)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predictions(mod, newdata=datagrid(race="White"), variables=list(age= c(40, 50)), by="age")
comparisons(mod, newdata=datagrid(age = 40, race="White"), variables=list(age=10)) 


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predictions(mod, newdata=datagrid(age=45), variables="race")
comparisons(mod, newdata=datagrid(age=45), variables=list(race="pairwise"))

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
comparisons(mod, newdata=datagrid(age = 40, race="White"), variables="age", comparison = "dydx") 


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
avg_comparisons(mod, variables = list(age=10), comparison="difference")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
avg_comparisons(mod, variables = list(race="pairwise"))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
avg_comparisons(mod, variables = "age", comparison="dydx")


## ----eval=TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------
infs <- avg_comparisons(mod, variables="age") %>% 
  inferences(method="simulation")
ggplot(mapping=aes(x=c(attr(infs, "posterior_draws")))) + 
  geom_histogram() + 
  geom_vline(xintercept=infs$estimate[1], 
             col="red", linetype=2) + 
  theme_classic() + 
  labs(x="Effects") + 
  ggtitle("Sampling Distribution of Average Effect")


## ----fig.align="center", out.width="100%"-------------------------------------------------------------------------------------------------------------------------------------------
age_comps <- comparisons(mod, newdat=dat, variables="age")
ggplot(age_comps, mapping = aes(x=estimate)) + 
  geom_histogram() + 
  geom_vline(xintercept=mean(age_comps$estimate), 
             col="red", linetype=2) + 
  theme_classic() + 
  labs(x="Effect") + 
  ggtitle("Distribution of Effects in Sample")


## ----out.width="100%"---------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(age_comps, mapping = aes(x=estimate, fill=race)) + 
  geom_histogram(position="identity", alpha=.25) + 
  geom_vline(xintercept=mean(age_comps$estimate), 
             col="red", linetype=2) + 
  theme_classic() + 
  theme(legend.position="top") + 
  labs(x="Effect") + 
  ggtitle("Distribution of Effects in Sample")


## ----out.width="100%"---------------------------------------------------------------------------------------------------------------------------------------------------------------
p_age <- predictions(mod,
          newdata=datagrid(age=18:90))

ggplot(p_age, aes(x=age, y=estimate,
                  ymax=conf.high,
                  ymin=conf.low)) +
  geom_ribbon(alpha=.25) +
  geom_line() +
  theme_classic() +
  labs(x="Age", y="Predicted Pr(Voted)")


## ----out.width="100%"---------------------------------------------------------------------------------------------------------------------------------------------------------------
ap_age <- avg_predictions(mod,
          variables = list(age=18:90))

ggplot(ap_age, aes(x=age, y=estimate,
                  ymax=conf.high,
                  ymin=conf.low)) +
  geom_ribbon(alpha=.25) +
  geom_line() +
  theme_classic() +
  labs(x="Age", y="Predicted Pr(Voted)")



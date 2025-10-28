library(rio)
library(dplyr)
library(marginaleffects)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rio)
library(DAMisc)
library(mlogit)
library(brglm2)
library(nnet)
library(factorplot)
library(marginaleffects)
library(RColorBrewer)
library(formatR)
library(knitr)
library(xaringan)
library(xaringanthemer)
`-.gg` <- function(plot, layer) {
    if (missing(layer)) {
        stop("Cannot use `-.gg()` with a single argument. Did you accidentally put - on a new line?")
    }
    if (!is.ggplot(plot)) {
        stop('Need a plot on the left side')
    }
    plot$layers = c(layer, plot$layers)
    plot
}
calc_ymax <- function(plot, scale=.3) {
  gb = ggplot_build(plot)
  ymin = gb$layout$panel_params[[1]]$y.range[1]
  ymax = gb$layout$panel_params[[1]]$y.range[2]
  ymin + (ymax-ymin)*scale
}

select <- function(...)dplyr::select(...)
## ----pois_ex1, echo=T----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(rio)
dat <- import("data/count.dta")
dat <- dat %>% 
  mutate(across(c("unemployed", "religimp"), factorize))
mod <- glm(volorgs ~ age + educ + unemployed + hhincome_num + 
             leftright + numkids + religimp, 
           data=dat, 
           family=poisson)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(mod)


## ----p1, echo=T----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
preds[2]/preds[1]
preds[4]/preds[3]


## ----ill2, echo=T--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
100*(preds[2] - preds[1])/preds[1]
100*(preds[4] - preds[3])/preds[3]


## ----glmc, echo=T--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(marginaleffects)
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
              


## ----ames, echo=T--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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



## ----eff1, eval=FALSE, echo=T, fig.height=8, fig.width=8-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
  labs(x="Years of Education", y="Expected Number of Voluntary Organizations")


## ----eff2, eval=FALSE, echo=T, fig.height=8, fig.width=8-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
apreds <- avg_predictions(mod,
            variables = list(educ = 1:17))
ggplot(apreds,
       aes(x=educ, y=estimate,
           ymin=conf.low,
           ymax=conf.high)) +
  geom_ribbon(fill="gray75") +
  geom_line() +
  theme_classic() +
  labs(x="Years of Education", y="Expected Number of Voluntary Organizations")



## ----poisfit, echo=T-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
poisfit(mod)


## ----poisavp, echo=TRUE, eval=FALSE, fig.height=7, fig.width=7, fig.align="center"---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
yhat <- predict(mod, type="response")
draw <- rpois(length(yhat), yhat)
fitdat <- tibble::tibble(
  val = c(model.response(model.frame(mod)), draw),
  type = factor(rep(c("observed", "predicted"), each=length(draw)))
)
ggplot(fitdat,
       aes(x=val, fill=type)) +
  geom_bar(position="dodge") +
  theme_classic() +
  scale_fill_brewer(palette="Set1") +
  labs(x="Voluntary Organizations", fill="") +
  theme(legend.position = "top")




## ----nbregex, echo=T-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mod2 <- MASS::glm.nb(volorgs ~ age + educ + unemployed + 
                 hhincome_num + leftright + numkids + 
                 religimp, 
               data=dat)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(mod2)


## ----negbinavp, echo=TRUE, eval=FALSE, fig.height=7, fig.width=7, fig.align="center"-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
yhat <- predict(mod2, type="response")
draw <- MASS::rnegbin(length(yhat), yhat, theta=mod2$theta)
fitdat <- tibble::tibble(
  val = c(model.response(model.frame(mod)), draw),
  type = factor(rep(c("observed", "predicted"), each=length(draw)))
)
ggplot(fitdat,
       aes(x=val, fill=type)) +
  geom_bar(position="dodge") +
  theme_classic() +
  scale_fill_brewer(palette="Set1") +
  labs(x="Voluntary Organizations", fill="") +
  theme(legend.position = "top")




## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(pscl)
mod <- hurdle(volorgs ~ age + educ + unemployed + hhincome_num + 
             leftright + numkids + religimp, 
           data=dat, 
           dist="negbin", 
           zero.dist = "binomial")


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
avg_predictions(mod, variables="unemployed", type="zero") %>% 
  as_tibble() %>% 
  select(unemployed, estimate, conf.low, conf.high)
avg_predictions(mod, variables="unemployed", type="count") %>% 
  as_tibble() %>% 
  select(unemployed, estimate, conf.low, conf.high)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
avg_predictions(mod, variables="unemployed", type="prob") %>% head(n=2) %>%
  as_tibble() %>% 
  select(unemployed, estimate, conf.low, conf.high)
avg_predictions(mod, variables="unemployed", type="response")  %>% 
  as_tibble() %>% 
  select(unemployed, estimate, conf.low, conf.high)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mod <- zeroinfl(volorgs ~ age + educ + unemployed + hhincome_num + 
             leftright + numkids + religimp, 
           data=dat, 
           dist="negbin", 
           zero.dist = "binomial")


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
avg_predictions(mod, variables="unemployed", type="zero") %>% 
  as_tibble() %>% 
  select(unemployed, estimate, conf.low, conf.high)
avg_predictions(mod, variables="unemployed", type="count") %>% 
  as_tibble() %>% 
  select(unemployed, estimate, conf.low, conf.high)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
avg_predictions(mod, variables="unemployed", type="prob") %>% head(n=2) %>%
  as_tibble() %>% 
  select(unemployed, group, estimate, conf.low, conf.high)
avg_predictions(mod, variables="unemployed", type="response")  %>% 
  as_tibble() %>% 
  select(unemployed, estimate, conf.low, conf.high)


## ----makefree, echo=T----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
man <- import("data/man2014.dta")
man$num201 <- floor((man$per201/100)*man$total)


## ----histfree, echo=T, message=F, warning=F, fig.height=8, fig.width=8, out.width="40%", fig.align="center"--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(man, aes(x=num201)) + geom_histogram() + theme_classic() + labs(x="Human Rights Statements")


## ----poisfree, echo=T----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Without Exposure
tmp <- na.omit(man[,c("num201", "total", "rile")])
mod <- glm(num201 ~ rile, 
           data=tmp, 
           family=poisson)
mode0 <- glm(num201 ~ 1 + offset(log(total)), 
             data=tmp, 
             family=poisson)
mode <- glm(num201 ~ 1 + rile + offset(log(total)), 
             data=tmp, 
             family=poisson)
AIC(mod, mode0, mode)


## ----echo=FALSE, eval=TRUE, out.width="100%"-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
obs <- model.response(model.frame(mod))
fitdat <- tibble::tibble(
  model = rep(c("Poisson", "Poisson Null (Exposure)", "Poisson Exposure"), 
              each = length(obs)),
  observed = rep(obs, 3), 
  predicted = c(rpois(length(obs), predict(mod)), 
                rpois(length(obs), predict(mode0)), 
                rpois(length(obs), predict(mode)))) %>% 
  filter(observed < 75) %>% 
  pivot_longer(observed:predicted, names_to="type", values_to="val")
ggplot(fitdat, 
       aes(x=val, fill=type)) + 
  geom_bar(position="dodge") + 
  facet_wrap(~model, ncol=2) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set1") + 
  labs(x="Number of Statements", fill="") + 
  theme(legend.position = c(.825, .33), 
        panel.grid = element_blank())





## ----nbfree, echo=T------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Without exposure 
mod2 <- MASS::glm.nb(num201 ~ rile, data=tmp)
mod2e0 <- MASS::glm.nb(num201 ~ 1 + 
                 offset(log(total)),
               data=tmp)

mod2e <- MASS::glm.nb(num201 ~ rile + 
                 offset(log(total)),
               data=tmp)
AIC(mod2, mod2e0, mod2e)


## ----nbfree2, echo=FALSE, out.width="100%"-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
obs <- model.response(model.frame(mod2))
fitdat <- tibble::tibble(
  model = rep(c("NB", "NB Null (Exposure)", "NB Exposure"), 
              each = length(obs)),
  observed = rep(obs, 3), 
  predicted = c(MASS::rnegbin(length(obs), predict(mod2), mod2$theta), 
                MASS::rnegbin(length(obs), predict(mod2e0), mod2e0$theta), 
                MASS::rnegbin(length(obs), predict(mod2e), mod2e$theta))) %>% 
  filter(observed < 75) %>% 
  pivot_longer(observed:predicted, names_to="type", values_to="val")
ggplot(fitdat, 
       aes(x=val, fill=type)) + 
  geom_bar(position="dodge") + 
  facet_wrap(~model, ncol=2) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set1") + 
  labs(x="Number of Statements", fill="") + 
  theme(legend.position = c(.825, .33), 
        panel.grid = element_blank())



## ----binfree, echo=T-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tmp$other <- floor(tmp$total - tmp$num201)
mod3 <- glm(cbind(num201, other) ~ rile, data=tmp, family=binomial)


## ----echo=FALSE, eval=TRUE, out.width="100%"-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
obs <- tmp$num201
fitdat <- tibble::tibble(
  Observed = obs, 
  Predicted = rbinom(length(obs), tmp$total, predict(mod3, type="response"))
) %>% 
  pivot_longer(everything(), 
               names_to = "type", 
               values_to = "val")
ggplot(fitdat, 
       aes(x=val, fill=type)) + 
  geom_bar(position="dodge") + 
  theme_classic() + 
  scale_fill_brewer(palette="Set1") + 
  labs(x="Number of Statements", fill="") + 
  theme(legend.position = "top") 


## ----binqfree, echo=T----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mod3q <- glm(cbind(num201, other) ~ rile, data=tmp, family=quasibinomial)


## ----echo=FALSE, eval=TRUE, out.width="100%"-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
obs <- tmp$num201
fitdat <- tibble::tibble(
  Observed = obs, 
  Predicted = rbinom(length(obs), tmp$total, predict(mod3q, type="response"))
) %>% 
  pivot_longer(everything(), 
               names_to = "type", 
               values_to = "val")
ggplot(fitdat, 
       aes(x=val, fill=type)) + 
  geom_bar(position="dodge") + 
  theme_classic() + 
  scale_fill_brewer(palette="Set1") + 
  labs(x="Number of Statements", fill="") + 
  theme(legend.position = "top") 


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
s <- seq(-75, 90, length=100)
mode <- glm(num201 ~ 1 + rile + offset(log(total)), 
             data=tmp, 
             family=poisson)
mod2e <- MASS::glm.nb(num201 ~ rile + 
                 offset(log(total)),
               data=tmp)
mod3 <- glm(cbind(num201, other) ~ rile, data=tmp, family=quasibinomial)
ap1 <- avg_predictions(mode, variables=list(rile = s))
ap2 <- avg_predictions(mod2e, variables=list(rile = s))
ap3 <- avg_predictions(mod3, variables=list(rile = s))
ap3q <- avg_predictions(mod3q, variables=list(rile = s))
plot.dat <- ap1 %>% 
  as_tibble() %>% 
  mutate(method = "Poisson (E)") %>% 
  bind_rows(ap2 %>% as_tibble() %>% mutate(method="NB (E)"), 
            ap3 %>% as_tibble() %>% 
              mutate(method="Binom",
                     across(c(estimate, conf.low, conf.high), 
                            ~.x * median(tmp$total))), 
            ap3q %>% as_tibble() %>% 
              mutate(method="Quasi-Binom",
                     across(c(estimate, conf.low, conf.high), 
                            ~.x * median(tmp$total))))

ggplot(plot.dat, aes(x=rile,y=estimate)) + 
  geom_ribbon(aes(ymin = conf.low, ymax=conf.high, fill
                  =method), 
              alpha=.25) + 
  geom_line(aes(color = method)) + 
  theme_classic()


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(rio)
library(dplyr)
sci <- import("data/science.dta")
sci <- sci %>%
  mutate(across(age_group:income, factorize))


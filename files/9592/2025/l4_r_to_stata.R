###################################
## Code for POLSCI 9592 Week 4   ##
## R to Stata Translation        ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

library(ggplot2)
library(marginaleffects)
library(modelsummary)
library(dplyr)
library(tidyr)
library(car)
library(rio)
library(dplyr)
library(ggplot2)
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


library(DAMisc)
load("data/anes_2008_binary.rda")
dat <- rio::factorize(dat)
mod <- glm(voted ~ age + educ + income + poly(leftright, 2) + 
    female + race, data=dat, family=binomial(link="logit"))
binfit(mod)
pre(mod, sim=T)


library(RColorBrewer)
library(separationplot)
cols <- brewer.pal(5, "Set2")[c(2,3)]
y <- model.response(model.frame(mod))
separationplot(fitted(mod), c(y),width = 10, height=1.25, 
    col0=cols[1], col1=cols[2], file=NULL)








library(lmtest)
dat1 <- dat %>% 
  dplyr::select(voted, age, educ, income, leftright, race, female) %>% 
  na.omit()
m1 <-  glm(voted ~ age + educ + income + poly(leftright, 2) + 
    female + race, data=dat1, family=binomial(link="logit"))
m2 <-  glm(voted ~ educ + income + poly(leftright, 2), 
           data=dat1, family=binomial(link="logit"))
lrtest(m1, m2)



IC_delta <- function(...){
  require(AICcmodavg)
  require(dplyr)
  mods <- list(...)
  a <- AIC(...)
  a$AICc <- sapply(mods, AICc)
  a$BIC <- BIC(...)[,2]
  out <- a %>% 
  mutate(across(c(AIC, AICc, BIC), ~.x-min(.x)))
  names(out)[2:4] <- paste0("D_", names(out)[2:4])
  out
}

library(clarkeTest)
m1 <-  glm(voted ~ age + female + race, data=dat, 
           family=binomial(link="logit"))
m2 <-  glm(voted ~ educ + income + poly(leftright, 2), 
           data=dat, family=binomial(link="logit"))
IC_delta(m1, m2)
clarke_test(m1, m2)


library(rsample)
library(tidyr)
library(purrr)
dat$lrstren <- abs(dat$leftright-5)
m1 <-  glm(voted ~ age + female + race +
             lrstren, data=dat, 
           family=binomial(link="logit"))
m2 <-  glm(voted ~ educ + income + 
             poly(leftright, 2), data=dat, 
           family=binomial(link="logit"))

cv_logit <- function(split, m1, m2, ...){
  m1_up <- update(m1, data=analysis (split))
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
  ll1 <- sum(-y_out*log(p1) - (1-y_out)*(1-log(p1)))
  ll2 <- sum(-y_out*log(p2) - (1-y_out)*(1-log(p2)))
  tibble(model = factor(1:2, 
                        labels=c("Model1", "Model2")), 
         ll = c(ll1, ll2))
}

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
mean(cv_out$diff < 0)


m1 <-  glm(voted ~ age + educ + income + poly(leftright, 2) + 
    female + race, data=dat, family=binomial(link="logit"))

dat$lrfac <- as.factor(dat$leftright)
mnew <- glm(voted ~ age + educ + income + as.factor(leftright) + 
    female + race, data=dat, family=binomial(link="logit"))

anova(m1, mnew, test='Chisq')



DAMisc::outEff(m1,
       var='age',
       data=dat,
       nOut=25,
       cumulative=TRUE)


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


sepdat <- rio::import("data/france_binary.dta")
sepdat <- sepdat %>%
  mutate(across(c("demsat", "retnat", "union"), rio::factorize))
mod3 <- glm(votefn ~ demsat + age + lrself + hhincome + retnat 
    + union, data=sepdat, family=binomial)

library(logistf)
mod3a <- logistf(mod3, sepdat)
pfl(mod3a)



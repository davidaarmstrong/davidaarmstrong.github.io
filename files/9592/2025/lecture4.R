###################################
## Code for POLSCI 9590 Week 4   ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

## ----setup, include=FALSE---------------------------------------------------------------------------------------
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

## ----ex1, echo=T------------------------------------------------------------------------------------------------
library(DAMisc)
load("data/anes_2008_binary.rda")
mod <- glm(voted ~ age + educ + income + poly(leftright, 2) + 
    female + race, data=dat, family=binomial(link="logit"))
binfit(mod)


## ----echo=FALSE, out.width="65%", fig.align="center"------------------------------------------------------------
knitr::include_graphics("images/pre1.png")


## ----pre_ex, echo=F, results="hide"-----------------------------------------------------------------------------
p1 <- c(.6,.51,.55,.75,.25,.45,.4)
p2 <- c(.8,.51,.65,.95,.15,.25,.1)
y <- c(1,0,1,1,0,0,0)
epcp1 <- mean(y*p1 + (1-y)*(1-p1))
epcp2 <- mean(y*p2 + (1-y)*(1-p2))
epmc <- mean(y)
epre1 <- (epcp1 - epmc)/(1-epmc)
epre2 <- (epcp2 - epmc)/(1-epmc)


## ----preR, echo=T-----------------------------------------------------------------------------------------------
pre(mod, sim=T)


## ----sepplot, echo=T, eval=FALSE, fig.align="center", out.width="65%"-------------------------------------------
cols <- brewer.pal(5, "Set2")[c(2,3)]
y <- model.response(model.frame(mod))
separationplot(fitted(mod), c(y),
    col0=cols[1], col1=cols[2], file=NULL)



## ----results='hide'---------------------------------------------------------------------------------------------
# remotes::install_github("davidaarmstrong/psre")
library(psre)
gh1 <- gg_hmf(model.response(model.frame(mod)), fitted(mod), method="loess")


## ----evpplot, echo=TRUE , eval=FALSE, fig.align="center", out.width="65%"---------------------------------------
library(patchwork)
gh1[[1]] + gh1[[2]] + plot_layout(heights=c(2,8), ncol=1)



## ----lrt1, echo=T-----------------------------------------------------------------------------------------------
library(lmtest)
m1 <-  glm(voted ~ age + educ + income + poly(leftright, 2) + 
    female + race, data=dat, family=binomial(link="logit"))
m2 <-  glm(voted ~ educ + income + poly(leftright, 2), 
           data=dat, family=binomial(link="logit"))
lrtest(m1, m2)


## ----echo=FALSE-------------------------------------------------------------------------------------------------

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


## ----ct1, echo=T------------------------------------------------------------------------------------------------
library(clarkeTest)
m1 <-  glm(voted ~ age + female + race, data=dat, 
           family=binomial(link="logit"))
m2 <-  glm(voted ~ educ + income + poly(leftright, 2), 
           data=dat, family=binomial(link="logit"))
IC_delta(m1, m2)
clarke_test(m1, m2)


## ---------------------------------------------------------------------------------------------------------------
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
  m1_up <- update(m1, data=assessment(split))
  m2_up <- update(m2, data=assessment(split))
  y_out <- model.response(
            model.frame(formula(m1), 
                        data=analysis(split)))
  p1 <- predict(m1_up, 
                newdata=analysis(split), 
                type="response")
  p2 <- predict(m2_up, 
                newdata=analysis(split), 
                type="response")
  ll1 <- sum(-y_out*log(p1) - (1-y_out)*(1-log(p1)))
  ll2 <- sum(-y_out*log(p2) - (1-y_out)*(1-log(p2)))
  tibble(model = factor(1:2, 
                        labels=c("Model1", "Model2")), 
         ll = c(ll1, ll2))
}


## ---------------------------------------------------------------------------------------------------------------
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


## ----echo=FALSE-------------------------------------------------------------------------------------------------
gg_crplot <- function(obj, term, plot=TRUE, 
                      res_type = c("std_deviance", "std_pearson", "deviance", "pearson", "response", "working", "quantile"), 
                      ...){
  require(statmod)
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



## ---------------------------------------------------------------------------------------------------------------
m1 <-  glm(voted ~ age + educ + income + poly(leftright, 2) + 
    female + race, data=dat, family=binomial(link="logit"))


## ----echo=TRUE, out.width="60%", fig.align="center"-------------------------------------------------------------
gg_crplot(m1, "age")


## ----echo=TRUE, out.width="60%", fig.align="center"-------------------------------------------------------------
gg_crplot(m1, "poly(leftright, 2)")


## ---------------------------------------------------------------------------------------------------------------
dat$lrfac <- as.factor(dat$leftright)
mnew <- glm(voted ~ age + educ + income + as.factor(leftright) + 
    female + race, data=dat, family=binomial(link="logit"))
anova(m1, mnew, test='Chisq')


## ----inflind, echo=T, eval=FALSE, out.width="100%"--------------------------------------------------------------
library(car)
influenceIndexPlot(mod, vars="Cook", id.n=10)


## ----outeffplot, echo=T,eval=FALSE, out.width="100%"------------------------------------------------------------
outEff(mod,
       var='age',
       data=dat,
       nOut=25,
       cumulative=TRUE)

## ----sepex, echo=T----------------------------------------------------------------------------------------------
sepdat <- rio::import("data/france_binary.dta")
sepdat <- sepdat %>%
  mutate(across(c("demsat", "retnat", "union"), rio::factorize))
mod3 <- glm(votefn ~ demsat + age + lrself + hhincome + retnat 
    + union, data=sepdat, family=binomial)
printCoefmat(summary(mod3)$coefficients)


## ----pfl, echo=F------------------------------------------------------------------------------------------------
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


## ----firth, echo=T----------------------------------------------------------------------------------------------
library(logistf)
mod3a <- logistf(mod3, sepdat)
pfl(mod3a)


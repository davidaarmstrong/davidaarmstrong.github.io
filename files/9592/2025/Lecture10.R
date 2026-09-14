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


## ----nes_mod, echo=T, results='hide', fig.show='hide'-----------------------------------------------------------
## Load packages
library(rio)
library(mice)
library(dplyr)
## read in data
poetate <- rio::import("data/poetate.dta")
poetate <- poetate %>% 
  select(IDORIGIN, 
         YEAR, AI, POLRT, LPOP, PCGNP, LEFT, 
          MIL2, BRIT, CWARCOW, IWARCOW2) %>% 
  mutate(POLRT = as.factor(POLRT), 
         LEFT = as.factor(LEFT), 
         MIL2 = as.factor(MIL2), 
         BRIT = as.factor(BRIT), 
         CWARCOW = as.factor(CWARCOW), 
         IWARCOW2 = as.factor(IWARCOW2))

## estimate and summarize listwise deleted model
md.pattern(poetate, rotate.names=TRUE)




## ----echo=FALSE-------------------------------------------------------------------------------------------------
#' Identify if a variable is numeric
#' 
#' @param x A variable to be evaluated
#' @param ncats Number of categories under which variable will be considered categorical
#' 
#' This is different from is.numeric() becuse it recognizes the possibility that numerical 
#' variables with `< ncats` may still be categorical in nature (i.e., we may want to treat them
#' as categorical rather than numeric when imputing, for example). 
isnum <- function(x, ncats=10){
  !(length(unique(na.omit(x))) < ncats | inherits(x, "factor"))
}

#' Split a Variable into Equal Groups
#' 
#' @param x A variable to be split into groups
#' @param ncats Number of categories into which variable will be separated. 
#' @param ... Other arguments passed down to `cut()`. 
#' 
#' This function is an extension of `cut()` that makes
#' `ncats` groups of roughly equal size.  Note that this may not work 
#' for skewed variables where, for example, the 10th and 20th percentile are both zero. 
binvar <- function(x, ncats=10, ...){
  qtl <- unique(quantile(x, probs = seq(0, 1, 1/ncats), na.rm = TRUE))
  qtl[1] <- qtl[1] - .001
  qtl[length(qtl)] <- qtl[length(qtl)] + .001
  cut(x, qtl, labels = FALSE, include.lowest=TRUE, right=FALSE, ...)
}

#' Missing Data Diagnostic Plot
#' 
#' @param formula A formula with the DV on the LHS and independent variables on the RHS
#' @param data A data frame
#' @param ncats The number of unique values over which numeric variables will be broken into categories. 
#' @param stacked Logical indicating whether the bar plots should be stacked or side-by-side
#' @param nrow Number of rows in the resulting display. 
#' @param ... Other arguments passed down, not implemented. 
#' 
#' This makes a diagnostic plot that looks at the distribution of the dependent variables
#' from the formula for the missing and non-missing observations on the dependent variable. 
mi_diag_plot <- function(formula, data, ncats = 10, stacked=TRUE, nrow = 1, ...){
  require(dplyr)
  require(ggplot2)
  D <- get_all_vars(formula, data)
  resp <- deparse(attr(terms(formula), "variables")[[2]])
  dv <- D %>% select(all_of(resp))
  dv <- dv %>% mutate(!!resp := is.na(.data[[resp]]))
  ivs <- D %>% select(-all_of(resp))
  ivs <- ivs %>% 
    mutate(across(where(isnum), ~binvar(.x, ncats=ncats)), 
           across(where(is.factor), ~as.numeric(.x)))
  D <- bind_cols(dv, ivs)
  D <- D %>% 
    mutate(obs = row_number()) %>% 
    pivot_longer(-all_of(c("obs", resp)), names_to="vbl", values_to = "vals") %>% 
    mutate(vbl = as.factor(vbl), 
           vbl = reorder(vbl, vals, \(x)length(unique(x))))
  if(stacked){
  D2 <- D %>% group_by(.data[[resp]], vbl, vals) %>% tally() %>% na.omit() %>% group_by(.data[[resp]], vbl) %>% mutate(pct = n/sum(n)) 
  ggplot(D2, aes(x=AI, y=pct, fill=as.factor(vals))) + 
    geom_bar(stat="identity", position="stack") + 
    facet_wrap(~vbl, nrow=nrow) + 
    scale_fill_brewer(palette="Paired") + 
    scale_y_continuous(labels = scales::percent) + 
    theme_bw() + 
    labs(x="DV Missing", y="", fill="Group")
  }
  else{
  ggplot(D, aes(x=vals, fill=.data[[resp]])) +
    geom_bar(position="dodge") +
    facet_wrap(~vbl, nrow=nrow, scales="free_x") +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(x = "", fill="Group")
  }
}


## ----echo=TRUE, fig.align="center", fig.width=10, fig.height=4, out.width="90%"---------------------------------
mi_diag_plot(AI ~ POLRT + LPOP + PCGNP + LEFT + MIL2 + BRIT + CWARCOW + IWARCOW2, data=poetate, nrow=2)


## ---------------------------------------------------------------------------------------------------------------
pats <- md.pairs(poetate)
inbound <- pats$mr/(pats$mr+pats$mm)
round(na.omit(inbound), 3)


## ---------------------------------------------------------------------------------------------------------------
outbound <- pats$rm/(pats$rm+pats$rr)
round(outbound, 3)


## ---------------------------------------------------------------------------------------------------------------
round(flux(poetate), 3)


## ----eval=FALSE-------------------------------------------------------------------------------------------------
## pt.mice <- mice(poetate, printFlag=F, m=5, maxit=20)

## ----echo=FALSE-------------------------------------------------------------------------------------------------
load("data/pt.mice.rda")

## ---------------------------------------------------------------------------------------------------------------
summary(pt.mice)


## ----out.width="100%", fig.height=4, fig.width=16---------------------------------------------------------------
densityplot(pt.mice, layout = c(4,1))


## ---------------------------------------------------------------------------------------------------------------
poetate <- poetate %>% 
  mutate(loggnp = log(PCGNP), 
         AI = as.factor(AI))


## ---------------------------------------------------------------------------------------------------------------
pm <- make.predictorMatrix(poetate)
pm["PCGNP", ] <- pm[,"PCGNP"] <- 0


## ---------------------------------------------------------------------------------------------------------------
meth <- make.method(poetate)
meth["PCGNP"] <- "~I(exp(loggnp))"


## ----eval=FALSE-------------------------------------------------------------------------------------------------
## pt.mice2 <- mice(poetate, printFlag=F, m=5, maxit=20,
##                  meth=meth, pred=pm)


## ----echo=F-----------------------------------------------------------------------------------------------------
load("data/pt.mice2.rda")


## ----out.width="100%", fig.height=4, fig.width=16---------------------------------------------------------------
densityplot(pt.mice2, layout = c(4,1))


## ----facdens, fig.show='hide'-----------------------------------------------------------------------------------
comps <- lapply(1:5, function(x)complete(pt.mice2, x))
comps <- do.call(rbind, comps)
comps <- rbind(poetate %>% mutate(loggnp = log(PCGNP)), comps)
comps$draw <- rep(0:5, each=nrow(poetate))
obsai <- as.numeric(!is.na(poetate$AI))
obsai <- factor(obsai, levels=c(0,1), 
                labels=c("Imputed", "Observed"))
comps$obs <- rep(obsai, 6)
comps <- comps %>% 
  filter((draw == 0 & obsai == "Observed") | 
         (draw > 0 & obsai == "Imputed"))
comps <- comps %>% 
  group_by(draw, obs, AI) %>% 
  summarise(n = n()) %>% 
  ungroup %>% 
  group_by(draw, obs) %>% 
  mutate(pct = n/sum(n))

ggplot(comps, aes(x=AI, y=pct)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~draw) + 
  theme_bw() 



## ----results='hide', fig.show='hide'----------------------------------------------------------------------------
library(gamlss)
comps <- lapply(1:5, function(x)complete(pt.mice2, x))

## ----echo=T, eval=F---------------------------------------------------------------------------------------------
## mods <- lapply(comps, function(D){
##   D$comp <- ici(pt.mice2)
##   gamlss(comp ~ pb(loggnp) +
##                 pb(LPOP) + POLRT +
##                 AI + LEFT + MIL2 +
##                 IWARCOW2 + CWARCOW,
##               data = D, family=BI)})

## ----echo=F-----------------------------------------------------------------------------------------------------
load("data/gmods.rda")

## ----pctmiss, eval=TRUE-----------------------------------------------------------------------------------------
fits <- lapply(mods, predict, type="response")
for(i in 1:5)comps[[i]]$pcomp <- fits[[i]]
comps <- do.call(rbind, comps)
comps$gnpobs <- as.numeric(!is.na(poetate$loggnp))
comps$gnpobs <- factor(comps$gnpobs, 
                       labels=c("Imputed", "Observed"))
comps$draw <- rep(1:5, each=nrow(poetate))


## ----pctmiss2, eval=FALSE---------------------------------------------------------------------------------------
pal2 <- c("#4F2683","#807F83")
ggplot(comps, aes(x=pcomp, y=loggnp,
                  colour=gnpobs, fill=gnpobs,
                  alpha=gnpobs)) +
  geom_point() +
  facet_wrap(~draw) +
  theme_bw() +
  scale_colour_manual(values=pal2) +
  scale_alpha_manual(values=c(.75,.1)) +
  theme(legend.position="top") +
  labs(x="Pr(Complete Case)", y="log(GNP/capita)",
       colour="", fill="", alpha="")



## ----echo=FALSE-------------------------------------------------------------------------------------------------
plot(pt.mice2, layout=c(2,5))


## ----pt_mods, echo=T, results='hide', fig.show='hide', fig.height=6, fig.width=12-------------------------------
## Estimate the models on each of the imputed datasets 
comps <- lapply(1:5, function(x)complete(pt.mice2, x))
library(plm)
pcomps <- lapply(comps, function(x)pdata.frame(x, index=c("IDORIGIN", "YEAR")))
for(i in 1:length(pcomps)){
  pcomps[[i]]$AI <- as.numeric(pcomps[[i]]$AI)
  pcomps[[i]]$lagAI <- lag(pcomps[[i]]$AI)  
}

mice.mods <- lapply(pcomps, function(x)
    lm(AI ~ lagAI + POLRT + LPOP +  I(PCGNP/10000) +
    LEFT + MIL2 + BRIT + CWARCOW + IWARCOW2, data=as.data.frame(x)))
## load the mitools package which contains MIcombine
library(mitools)
## use Rubin's rules to pool over the different models estimaetd on the
## different imputation sets
mice.pool <- MIcombine(mice.mods)


## ---------------------------------------------------------------------------------------------------------------
## save the confidence intervals from the model summary
cis1 <- as.matrix(summary(mice.pool)[,c(1,3,4)])
cmod <- lm(AI ~ lagAI + POLRT + LPOP +  I(PCGNP/10000) +
    LEFT + MIL2 + BRIT + CWARCOW + IWARCOW2, 
    data=subset(as.data.frame(pcomps[[1]]), ici(pt.mice2)))
cis0 <- cbind(coef(cmod), confint(cmod))
## combine the CIs into a single object
x <- rbind(cis1, cis0)
x <- as.data.frame(x)
## add the parameter name to the dataset
x$parm <- rownames(cis1)
rownames(x) <- NULL
x$parm <- as.factor(x$parm)
## add a variable indicating the model (mice or CC)
x$mod <- factor(rep(c(1,2), each=nrow(cis1)), levels=c(1,2),
    labels=c("mice", "CC"))
## plot the confidence intervals for each parameter by imptuation method.
names(x)[1:3]<- c("coef", "lower", "upper")


## ----ppdfig,  eval=FALSE----------------------------------------------------------------------------------------
ggplot(x, aes(x=coef,
              y=parm,
              colour=mod)) +
  geom_point(
    position=position_dodge(width=.5)) +
  geom_errorbarh(
    aes(xmin=lower, xmax=upper),
    position = position_dodge(width=.5),
    height=0) +
  geom_vline(
    xintercept=0,
    col="black",
    lty=2) +
  scale_colour_manual(
    values=pal2) +
  theme_bw() +
  theme(
    legend.position="top") +
  labs(
    x="Coefficient (95%CI)",
    y="",
    colour="")

## ----echo=FALSE-------------------------------------------------------------------------------------------------
#' Make Posterior-predictive Data
#' 
#' @param formula The formula that gives the variables used in the model.  This could
#' be either a one-sided or two-sided formula, it is simply used to get the relevant
#' data from the data frame with `get_all_vars(formula, data)`.  
#' @param data A data frame from which variables will be retrieved. 
#' 
#' This function makes a stacked posterior predictive data frame where copies of the 
#' data are made where each variable in the formula with missing data is in turn 
#' made entirely missing.  The resulting data frame has an attribute called "idx" that 
#' can be used to extract the appropriate imputations. 
make_pp <- function(formula, data, ...){
  D <- get_all_vars(formula, data)
  wna <- which(c(unlist(lapply(D, \(x)any(is.na(x))))))
  out <- D
  k <- nrow(out)
  attrmat <- NULL
  for(i in seq_along(wna)){
    tmp <- D
    tmp[[names(wna)[i]]] <- NA
    out <- rbind(out, tmp)
    attrmat <- rbind(attrmat, data.frame(vbl = names(wna)[i], start = k+1, end=nrow(out)))
    k <- nrow(out)
  }
  attr(out, "idx") <- attrmat
  out
}

#' Extract Posterior-predicted Data From Imputations
#' 
#' @param res The result of `mice` estimated on the data produced by 
#' `make_pp()`. 
#' @param data The result of `make_pp()`.  
#' @param orig_data The original data. 
#' @param ... Other arguments passed down, not implemented. 
#' 
#' This function extracts the appropriate posterior-predicted data that can be 
#' used as a diagnostic for the utility of the imputation model.  If the imputation 
#' model is good, then the results using the posterior-predicted data should be similar 
#' to those using the imputations on the original data. 
extract_pp <- function(res, data, orig_data, ...){
  idx <- attr(data, "idx")
  ovars <- setdiff(names(poetate),names(ppdat))
  comps <- lapply(1:res$m, \(i)complete(res, i))
  D <- lapply(comps, \(x)x[1:(idx$start[1]-1), ])
  for(i in 1:nrow(idx)){
    for(j in 1:length(D)){
      D[[j]][[idx$vbl[i]]] <- comps[[j]][[idx$vbl[i]]][idx$start[i]:idx$end[i]]
    }
  }
  D <- lapply(D, \(x)cbind(orig_data[,ovars], x))
}


## ----makedata, echo=T, eval=FALSE-------------------------------------------------------------------------------
## make copies of the data and stack them on top of each other,
## in each copy, make one of the variables with missing data entirely
## missing, but don't change the others for that particular copy.
ppdat <- make_pp(AI ~ POLRT + LPOP + PCGNP + LEFT + MIL2 + BRIT + CWARCOW + IWARCOW2, data=poetate)

## estimate the imputation model on the stacked data.  Note that
## m here should in "real life" be something like 500 or 1000.  You
## need enough observations to generate a reasonable p-value.
pp.mice <- mice(ppdat, printFlag=F, m=25, maxit=5, meth=meth, pred=pm)
res <- extract_pp(pp.mice, ppdat, poetate)
orig.imp <- mice(poetate, printFlag=F, m=25, maxit=5,
                 meth=meth, pred=pm)


## ----echo=FALSE-------------------------------------------------------------------------------------------------
load("data/pp_data.rda")

## ----ppdreg, echo=T---------------------------------------------------------------------------------------------
library(plm)
pt.comp <- lapply(1:orig.imp$m, \(i)complete(orig.imp, i))
## estimate models on both the partially imputed and fully imputed data
l1 <- lapply(pt.comp, function(x){
  lm(as.numeric(AI) ~ POLRT + LPOP +  I(PCGNP/10000) +
    LEFT + MIL2 + BRIT + CWARCOW + IWARCOW2, data=x)})
l2 <- lapply(res, function(x){
    lm(as.numeric(AI) ~ POLRT + LPOP +  I(PCGNP/10000) +
      LEFT + MIL2 + BRIT + CWARCOW + IWARCOW2, data=x)})
## save the coefficients from each model
b1 <- sapply(l1, coef)
b2 <- sapply(l2, coef)
## fin pval as the proportion of times that b1 > b2
pval <- apply(b1>b2, 1, mean)
## replace one-sided p-value with two-sided p-value
pval <- 2*ifelse(pval > .5, 1-pval, pval)
pval


## ----omtest, echo=T---------------------------------------------------------------------------------------------
library(car)
## calculate the difference between the two sets of
## coefficients for all parameter estimates
coef <- rowMeans(b1-b2)
## calculate the variance of the difference in coefficients
## this would be similar to what we would get from vcov()
v <- var(t(b1-b2))
## use linear hypothesis to calculate the joint test on all
## parameters.  Since we're supplying the coefficients and
## the variance covariance, the only thing model needs to have
## in it is the residual degrees of freedom.  Setting it to NULL
## is the equivalent of doing a z-test instead of a t-test.   
linearHypothesis(model=list(df.residual=NULL),
  hypothesis.matrix=diag(length(coef)),
  vcov.=v, coef.=coef, rhs=rep(0, length(coef)))


## ----echo=FALSE-------------------------------------------------------------------------------------------------
#' Make Frequency Distribution of Categorical Imputations
#' 
#' @param x A set of imputations from `mice()`. 
#' @param ... Other arguments passed down, not implemented. 
make_df <- function(x, ...){
  require(dplyr)
  require(tidyr)
  data.frame(obs = rep(rownames(x), ncol(x)), 
             imp = rep(1:ncol(x), each = nrow(x)), 
             val = c(as.matrix(x))) %>%
    group_by(obs,val) %>% 
    tally() %>% 
    group_by(obs) %>% 
    mutate(pct = n/sum(n)) %>%
    select(-n) %>% 
    ungroup %>% 
    complete(obs, val, fill=list(pct=0))
}

#' Test imputation sensitivity to MAR assumption
#' 
#' @param variable String giving the name of the variable to be changed. 
#' @param data The data used in the imputation. 
#' @param imputations The output from `mice()`. 
#' @param type The type of variable - `"cat"` for categorical or `"num"` for numeric.  
#' This changes the way `delta` operates. 
#' @param delta The amount of change to induce in the variable.  If the variable is numeric, 
#' `delta` will be added to all imputed values.  If the variable is categorical with `m` categories, 
#' and `delta` is a scalar, the first `m/2` categories will have their probabilities decreased by `delta/.5*m` and the 
#' last `m/2` categories will have their probabilities increased by `delta/.5*m`.  If `delta` is an `m`-length 
#' vector, then `delta` will be the added to the existing probabilities and normalized to sum to 1. 
#' @param ... Other arguments passed down, not implemented. 
sens_impute <- function(variable, 
                        data, 
                        imputations, 
                        type = c("cat", "num"), 
                        delta, 
                        ...){
  if(length(delta) > 1 & type == "num")stop("If type = 'num', delta should be a scalar.")
  imps <- imputations$imp[[variable]]  
  if(type == "cat"){
      impl <- make_df(imps)
      lp <- length(unique(impl$val))  
      d <- rep(0, lp)
      ng <- ifelse(lp%%2, (lp-1)/2, lp/2)
      delta1 <- delta/ng
      d[1:ng] <- -delta1
      d[(lp-(ng-1)):lp] <- d[(lp-(ng-1)):lp] + delta1
      gdat <- impl %>% 
        group_by(obs) %>% 
        group_keys()
      new_imps <- impl %>% 
        group_by(obs) %>% 
        group_split() %>% 
        purrr::map(\(x){
          p <- x$pct
          p <- p+d
          p <- pmax(0, p)
          p <- pmin(1, p)
          p <- p/sum(p)
          sample(x$val, ncol(imputations$imp[[variable]]), prob=p, replace=TRUE)
        })
      new_imps <- do.call("rbind", new_imps) %>% 
        as.data.frame()
      rownames(new_imps) <- gdat$obs
      colnames(new_imps) <- colnames(imputations$imp[[variable]])
      ni <- new_imps[match(rownames(imputations$imp[[variable]]), rownames(new_imps)), ]  
      if(is.numeric(data[[variable]])){
        ni <- ni %>% mutate(across(everything(), as.numeric))
      }
      if(is.factor(data[[variable]])){
        levs <- levels(data[[variable]])
        ni <- ni %>% mutate(across(everything(), ~factor(.x, levels=levs)))
      }
    }else{
    ni <- imps %>% mutate(across(everything(), ~.x + delta[1]))
  }
  imputations$imp[[variable]] <- ni
  return(imputations)  
}


## ----eval=FALSE-------------------------------------------------------------------------------------------------
## pt.mice2 <- mice(poetate, printFlag=T, m=100, maxit=5,
##                  meth=meth, pred=pm)


## ----echo=FALSE-------------------------------------------------------------------------------------------------
load("data/pt.mice2_100.rda")


## ----results='hide'---------------------------------------------------------------------------------------------
sense_ai <- sens_impute("AI", poetate, pt.mice2, 
                        type="cat", delta=.1)

comps2 <- lapply(1:pt.mice2$m, \(i)complete(pt.mice2, i))
compss <- lapply(1:sense_ai$m, \(i)complete(sense_ai, i))

omod <- lapply(comps2, \(d)lm(as.numeric(AI) ~ scale(POLRT) + 
    scale(LPOP) + scale(loggnp) + LEFT + MIL2 + BRIT + 
    CWARCOW + IWARCOW2, data=d))
smod <- lapply(compss, \(d)lm(as.numeric(AI) ~ scale(POLRT) + 
    scale(LPOP) + scale(loggnp) + LEFT + MIL2 + BRIT + 
    CWARCOW + IWARCOW2, data=d))

omod_sum <- MIcombine(omod)
smod_sum <- MIcombine(smod)

plot.dat <- as_tibble(summary(omod_sum), rownames="param") %>% 
  setNames(c("param", "estimate", "se", "lwr", "upr", "mi")) %>%
  mutate(model = "Original")
plot.dat <- plot.dat %>% 
  bind_rows(as_tibble(summary(smod_sum), rownames="param") %>% 
    setNames(c("param", "estimate", "se", "lwr", "upr", "mi")) %>%
    mutate(model = "Sensitivity"))


## ----echo=FALSE, out.width="100%", fig.width=6, fig.height=7, fig.align="center"--------------------------------
ggplot(plot.dat %>% filter(param != "(Intercept)"), 
       aes(x=estimate, xmin=lwr, xmax=upr, y=model)) + 
  geom_pointrange(fatten=.2) + 
  facet_grid(param ~ .) + 
  theme_bw() + 
  theme(strip.text.y.right = element_text(angle=0)) + 
  labs(x="Estimate\n95% Confidence Interval", 
       y="")



## ----eval=FALSE-------------------------------------------------------------------------------------------------
## load("data/good_df.rda")
## 
## ## Do imputation
## 
## 
## ## Model Formulae
## form1 <- GeWom ~ FemDel_P, data = df_original
## form2 <- GeWom ~ FemDel_P + UNSCR + ImUN + ImOth + NAP
## form3 <- GeWom ~ FemDel_P + GDI + SEP_Fem + TeenPreg
## form4 <- GeWom ~ FemDel_P + PolInt_Cmb + WomParl
## form5 <- GeWom ~ FemDel_P + state_prev_avg + female_combatants_exs
## form6 <- GeWom ~ FemDel_P + GDI + WomParl + SEP_Fem + TeenPreg + NYT_p + UNSCR +
##   Press_UNSC + ImUN + ImOth + NAP + PolInt_Cmb + JobEql_Cmb + LeadPol_Cmb +
##   state_prev_avg + female_combatants_exs
## 
## ## Do diagnostics


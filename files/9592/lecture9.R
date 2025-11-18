## ----setup, include=FALSE----------------------------------------------
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

## ----sbmodels, echo=F--------------------------------------------------
dat <- matrix(c(
1,  1,  1,  2,  5,  6,
1,  2,  3,  2,  7,  6,
2,  1,  2,  3,  4,  5,
2,  2,  4,  3,  6,  5,
3,  1,  3,  4,  3,  4,
3,  2,  5,  4,  5,  4,
4,  1,  4,  5,  2,  3,
4,  2,  6,  5,  4,  3,
5,  1,  5,  6,  1,  2,
5,  2,  7,  6,  3,  2), ncol=6, byrow=T)
reg1 <- lm(dat[,5] ~ dat[,3])
reg2 <- lm(dat[,6] ~ dat[,4])
reg3 <- lm(I(dat[,5]-dat[,6]) ~ I(dat[,3] - dat[,4]))
library(lme4)
mix1 <- lmer(dat[,5] ~ dat[,4] + dat[,3] + (1|dat[,1]))
b1 <- coef(reg1)
b2 <- coef(reg2)
b3 <- coef(reg3)
bmix <- fixef(mix1)


## ----sbexfig, echo=F, results="hide", fig.height=7, fig.width=7, out.width="90%"----
plot(c(0,8), c(0,8), type="n", xlab="x", ylab="y")
abline(lm(dat[1:2, 5] ~ dat[1:2, 3]), lty=3)
abline(lm(dat[3:4, 5] ~ dat[3:4, 3]), lty=3)
abline(lm(dat[5:6, 5] ~ dat[5:6, 3]), lty=3)
abline(lm(dat[7:8, 5] ~ dat[7:8, 3]), lty=3)
abline(lm(dat[9:10, 5] ~ dat[9:10, 3]), lty=3)
points(dat[1:2, c(3,5)], pch=1, cex=2)
points(dat[3:4, c(3,5)], pch=2, cex=2)
points(dat[5:6, c(3,5)], pch=3, cex=2)
points(dat[7:8, c(3,5)], pch=4, cex=2)
points(dat[9:10, c(3,5)], pch=5, cex=2)
abline(8,-1,lty=2, lwd=2)
abline(lm(dat[,5] ~ dat[,3]), lty=1, lwd=2)
legend(8,8, c("Total Regression", "Regression Between Groups", 
    "Regressions Within Groups"), lty=c(1,2,3), xjust=1)


## ----echo=TRUE, eval=FALSE---------------------------------------------
## remotes::install_github("davidaarmstrong/uwo9592")


## ----echo=FALSE--------------------------------------------------------
make_between_data <- function(formula, data, id){
    require(dplyr)
    vars <- get_all_vars(formula, dat)
    wc <- which(complete.cases(vars))
    vars <- vars[wc, ]
    idv <- NULL
    for(i in 1:length(id)){
        idv <- cbind(idv, dat[[id[i]]])
    }
    if(!is.matrix(idv)){
        idv <- matrix(idv, ncol=length(id))
    }
    idv <- idv[wc, , drop=FALSE]
    for(i in 1:length(id)){
        X <- model.matrix(~., data=vars)[,-1]
        X <- as.data.frame(X)
        X <- cbind(X, idv[,i]) 
        names(X)[ncol(X)] <- "id"
        Xm <- X %>% group_by(id) %>% summarise_all(mean, na.rm=TRUE)
        names(Xm)[-1] <- paste0(names(Xm)[-1], "_b", i)
        vars$id <- idv[,i]
        if(i == 1){
            out <- X
        }
        out <- left_join(out, Xm, by="id")
        dv <- names(model.frame(formula, data))[1]
        transvars <- grep(paste0("\\_b",i), names(out), value=TRUE)
        transvars <- gsub(paste0("\\_b",i), "", transvars)
        transvars <- ifelse(transvars == dv, NA, transvars)
        transvars <- na.omit(transvars)
        for(j in 1:length(transvars)){
            out[[paste0(transvars[j], "_w", i)]] <- out[[transvars[j]]] - out[[paste0(transvars[j], "_b", i)]]
        }
        v <- apply(out, 2, var, na.rm=TRUE)
        v["id"] <- 1
        if(any(v == 0))out <- out[,-which(v == 0)]
        out <- out[,-which(names(out) == paste0(dv, "_b", i))]
        names(out) <- gsub("id", id[i], names(out))
    }
    return(out)
}   


## ----bweff, echo=T, include=T------------------------------------------
library(rio)
library(uwo9592)
dat <- import("data/context.dta")
dat <- dat %>% mutate(SEX = rio::factorize(SEX))
betweendat <- make_between_data(LRSCALE ~ AGE + SEX + INCOME + PROFMAN, dat, id="PANO")

library(lme4)
mod <- lmer(LRSCALE ~ INCOME_b1 + INCOME_w1 + AGE_b1 + AGE_w1 + 
    SEXmen_b1 + SEXmen_w1 + PROFMAN_b1 + (1|PANO), data=betweendat)
s <- summary(mod, corr=FALSE)
round(s$coefficients, 3)


## ----echo=T------------------------------------------------------------
head(ranef(mod)$PANO)


## ----p1, echo=T--------------------------------------------------------
library(LMERConvenienceFunctions)
pamer.fnc(mod)


## ----lmertest2, echo=T, include=T--------------------------------------
library(lmerTest)
mod <- update(mod)
car::Anova(mod)


## ----equal, echo=T-----------------------------------------------------
library(car)
linearHypothesis(mod, "INCOME_b1 = INCOME_w1")


## ----------------------------------------------------------------------
library(lmtest)
printCoefmat(summary(mod)$coefficients[,-3], digits = 3)


## ----------------------------------------------------------------------
mod1 <- lm(LRSCALE ~ INCOME_b1 + INCOME_w1 + AGE_b1 + AGE_w1 + 
    SEXmen_b1 + SEXmen_w1 + PROFMAN_b1, data=betweendat)
coeftest(mod1, vcov. = sandwich::vcovCL, df=135, cluster=~PANO)


## ----------------------------------------------------------------------
library(rio)
dat <- import("data/wvs2005_2009_mlm.dta")


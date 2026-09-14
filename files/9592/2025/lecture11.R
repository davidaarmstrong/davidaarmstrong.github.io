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


## ----srm, echo=T----------------------------------------------
library(psych)
dat <- rio::import('data/srm.dta')
alpha(dat)


## ----restplot, eval=FALSE, echo=T, out.width="100%", fig.align="center"----
## restplot is in v. 0.1.3 of uwo9592
library(uwo9592)
restplot(dat,
         span =	.7,
         family = "symmetric",
         degree = 2)


## ----demdat, echo=T, eval=FALSE, out.width="100%"-------------
library(rio)
library(psych)
dat <- import("data/dem_rep.dta")
X <- na.omit(dat[,-c(1:3)])
R <- cor(X)
scree(R, pc=FALSE)


## -------------------------------------------------------------
fn <- fa(X, nfactors=2, rotate="none", fm="pa", SMC=TRUE)
fn

## ----scores, echo=T-------------------------------------------
demfa2 <- fa(X, 2, fm="pa", rotate="promax", scores=T)
fa.scores <- scale(demfa2$scores)
srm.scores <- scale(cbind(rowMeans(X[,1:4]), 
   rowMeans(X[,5:8])))
colnames(fa.scores) <- c("dem", "rep")
colnames(srm.scores) <- c("dem", "rep")
srm.scores <- as.data.frame(srm.scores)
fa.scores <- as.data.frame(fa.scores)
m1 <- lm(rep ~ dem, data=fa.scores)
m2 <- lm(rep ~ dem, data=srm.scores)


## ----comp, echo=FALSE-----------------------------------------
library(stargazer)
stargazer(m1, m2, type="text")


## -------------------------------------------------------------
library(rio)
wom <- import("data/women_dat.dta")


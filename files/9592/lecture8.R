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

## ----echo=FALSE-------------------------------------------------------------------------------------------------
brief <- function(obj, ...){
  quiet_sum <- purrr::quietly(gamlss:::summary.gamlss)
  s <- quiet_sum(obj, save=TRUE, ...)
  class(s) <- "brief"
  return(s)
}
print.brief <- function(x, ...){
  printCoefmat(x$result$coef.table, ...)
}
.S3method("brief", "gamlss", brief)
.S3method("print", "brief", print.brief)


## ---------------------------------------------------------------------------------------------------------------
library(gamlss)
library(psre)
data(wvs)
wvs2 <- wvs %>% 
  select(resemaval, pct_high_rel_imp, pct_univ_degree) %>% na.omit()
g1 <- gamlss(resemaval ~ pct_high_rel_imp + pct_univ_degree, data=wvs2)
g2 <- gamlss(resemaval ~ pct_high_rel_imp + pct_univ_degree, 
             sigma.formula =  ~ pct_high_rel_imp + pct_univ_degree,
             data=wvs2)
VC.test(g1, g2)


## ---------------------------------------------------------------------------------------------------------------
# Constant Variance
brief(g1)


## ---------------------------------------------------------------------------------------------------------------
# Parameterized Variance
brief(g2)


## ----mean_sig, echo=TRUE, eval=FALSE, out.width="100%", fig.height=8, fig.width=6-------------------------------
library(patchwork)
p1 <- plot_predictions(g2,
                       condition="pct_high_rel_imp",
                       what="mu") +
  theme_classic() +
  ggtitle("Expected Value")
p2 <- plot_predictions(g2,
                       condition="pct_high_rel_imp",
                       what="sigma") +
  theme_classic() +
  ggtitle("Residual Variane")
p1 + p2 + plot_layout(ncol=1)


## ----simplespline, echo=F, fig.height=6, fig.width=6, out.width="100%", fig.align="center"----------------------
## set random number generator seed
set.seed(15)
## generate x as integer values from 1-100
x <- 1:100

## create the before 60 and after 60 basis functions
before <- function(x) ifelse (x<60, 60-x,0)
after <- function(x) ifelse (x<60,0, x-60)

## Use the basis functions to make the design matrix
X <- cbind(before(x), after(x))
## Create y that is f(X) + normal error
y <- 1 + 1*before(x) + 1*after(x) + rnorm(100, 0, 5)
ex_dat <- data.frame(x = x, y=y)

## plot y against x
ggplot(mapping=aes(x=x, y=y)) +
  geom_point(pch=1) + 
  theme_classic() 


## ----echo=FALSE-------------------------------------------------------------------------------------------------
tpb <- function (x, degree = 3, nknots = 3, knot_loc = NULL) 
{
#    out <- sapply(1:degree, function(d) x^(d))
    if (is.null(knot_loc)) {
        q <- seq(0, 1, length = nknots + 2)
        q <- q[-c(1, length(q))]
        s <- quantile(x, q, na.rm = TRUE)
        if (length(s) != length(unique(s))) {
            stop("The quantiles of the variable are not unique.\n")
        }
    }
    else {
        s <- knot_loc
    }
    out <- NULL
    for (i in 1:length(s)) {
        out <- cbind(out, (x - s[i])^degree * (x > s[i]))
    }
    colnames(out) <- paste0("tpb", 1:ncol(out))
    return(out)
}


## ---------------------------------------------------------------------------------------------------------------
mod <- lm(y ~ x + tpb(x, 1, 1, knot_loc=60))
summary(mod)


## ---------------------------------------------------------------------------------------------------------------
car::linearHypothesis(mod, 
                      "x + tpb(x, 1, 1, knot_loc = 60) = 0")


## ----simnl, echo=F, fig.height=6, fig.width=6, out.height="75%", fig.align="center"-----------------------------
## set random number generator seed
set.seed(1)
## set number of observations
n <- 400
## generate x in [0,1]
x <- 0:(n-1)/(n-1)
## create compled function of x
f <- 0.2*x^11*(10*(1-x))^6+10*(10*x)^3*(1-x)^10
## create y = f(x) + random noise
y <- f + rnorm(n, 0, sd = 2)
## plot x against y
ggplot(mapping=aes(x=x)) + 
  geom_point(aes(y=y), pch=1) + 
  geom_line(aes(y=f), size=1) + 
  theme_classic() + 
  labs(x="x", y="y")


## ----tb3, echo=F------------------------------------------------------------------------------------------------
## estimate the model by hand-coding
## truncated power basis functions
k <- c(.2, .4, .6, .8)
csmod <- lm(y ~ x + I(x^2) + I(x^3) + tpb(x, 3, 4, knot_loc=k))
car::S(csmod, brief=TRUE)


## ----csp1, echo=F, fig.height=6, fig.width=6, out.width="45%", fig.align="center"-------------------------------
library(ggeffects)
p1 <- ggpredict(csmod, term="x [n=50]")
df <- data.frame(x=x, y=y, f=f)
ggplot() + 
  geom_point(data=df, aes(x=x, y=y), pch=1) + 
  geom_line(data=df, aes(x=x, y=f, colour="True"), 
            size=1.5) + 
  geom_line(data=p1, aes(x=x, y=predicted, 
                         colour="Estimated")) + 
  theme_classic() + 
  theme(legend.position = "top") + 
  labs(x="x", y="y", colour="")


## ----simnl2, echo=F, results='hide'-----------------------------------------------------------------------------
## set random number generator seed
set.seed(1)
## set number of observations
n <- 400
## generate x in [0,1]
x <- 0:(n-1)/(n-1)
## create compled function of x
f <- 0.2*x^11*(10*(1-x))^6+10*(10*x)^3*(1-x)^10
## create y = f(x) + random noise
y <- f + rnorm(n, 0, sd = 2)


## ---------------------------------------------------------------------------------------------------------------
df <- data.frame(x=x, y=y, f=f)
mod <- lm(y ~ poly(x, 3, raw=TRUE) + tpb(x, 3, 20), data=df)
D <- diag(24)
D[1:4,1:4] <- 0
X <- model.matrix(mod)
y <- model.response(model.frame(mod))

lambda <- .0025
b.constr <- solve(t(X) %*% X + lambda^2*D) %*% t(X) %*% y

fit0 <- X %*% mod$coef
fit1 <- X %*% b.constr


## ----echo=FALSE, fig.height=6, fig.width=6, fig.align="center", out.width="90%"---------------------------------
df.fit <- data.frame(
  fit = c(fit0, fit1, f), 
  x = c(x,x, x), 
  model = factor(rep(1:3, each=nrow(X)), 
                 labels=c("OLS", "Penalized", "Truth"))
)
ggplot() + 
  geom_point(data=df, aes(x=x, y=y), pch=1, alpha=.5) + 
  geom_line(data=df.fit, aes(x=x, y=fit, colour=model)) + 
  theme_classic() + 
  theme(legend.position="top") + 
  labs(x="x", y="y")


## ---------------------------------------------------------------------------------------------------------------
library(gamlss)
dframe <- data.frame(x=x, y=y, f=f)
mod <- gamlss(y ~ pb(x, control=pb.control(inter=50)), data=dframe)
brief(mod)


## ---------------------------------------------------------------------------------------------------------------
mod$mu.coefSmo[[1]]


## ---------------------------------------------------------------------------------------------------------------
# coefficients
c(mod$mu.coefSmo[[1]]$coef)


## ----gl1, eval=FALSE--------------------------------------------------------------------------------------------
term.plot(mod)


## ----results='hide'---------------------------------------------------------------------------------------------
library(foreign)
dat <- import("data/linear_ex.dta")
dat$polity_dem_fac <- as.factor(dat$polity_dem)
unrestricted.mod1 <- gamlss(rep1 ~ polity_dem_fac + iwar +
    cwar + logpop + gdppc,data=dat)
mono.mod1 <- gamlss(rep1 ~ pbm(polity_dem, mono="down") + 
    iwar + cwar + logpop + gdppc,data=dat)
nonmono.mod1 <- gamlss(rep1 ~ pb(polity_dem) + iwar +
    cwar + logpop + gdppc,data=dat)
mod.2p <- gamlss(rep1 ~ polity_dem + 
    I((polity_dem - 9)*(polity_dem >= 9)) + iwar +
    cwar + logpop + gdppc,data=dat)


## ---------------------------------------------------------------------------------------------------------------
VC.test(unrestricted.mod1, mono.mod1)
VC.test(mod.2p, nonmono.mod1)
VC.test(mono.mod1, nonmono.mod1)
VC.test(mono.mod1, mod.2p)


## ---------------------------------------------------------------------------------------------------------------
## read in ANSS data
anes <- import("data/anes1992.dta")
## make PID a factor and store in pidfac
anes$pidfac <- as.factor(anes$pid)
## estimate the linear (restricted) and
## dummy variable (unrestricted) models
unrestricted.mod2 <- gamlss(votedem ~ retnat + pidfac + age + male +
	 educ + black + south, data=anes, family=BI)
mono.mod2 <- gamlss(votedem ~ retnat + pbm(pid, mono="down") + 
  age + male + educ + black + south, data=anes, family=BI)
## test the difference in two models
VC.test(unrestricted.mod2, mono.mod2)


## ----echo=FALSE, fig.height=6, fig.width=6, out.width="90%", fig.align="center"---------------------------------
tp <- termplot(unrestricted.mod1, terms = "polity_dem_fac", 
               se=TRUE, plot=FALSE)
tp2 <- termplot(mono.mod1, terms = 'pbm(polity_dem, mono = "down")', 
         se=TRUE, plot=FALSE)
tp[[1]]$x <- tp2[[1]]$x
tp_pol <- bind_rows(tp[[1]], tp2[[1]])
tp_pol$model <- factor(rep(1:2, each=nrow(tp[[1]])), 
                      labels=c("Unconstrained", "Monotonic"))
ggplot(tp_pol, aes(x=x, y=y, colour=model, fill=model, 
                   ymin = y-1.96*se, ymax=y+1.96*se)) + 
  geom_ribbon(alpha=.2, col="transparent") + 
  geom_line() + 
  theme_xaringan() + 
  theme(legend.position = "top") + 
  labs(x="Polity Democracy", y="Predicted Repression")


## ----echo=FALSE, fig.height=6, fig.width=6, out.width="90%", fig.align="center"---------------------------------
tp <- termplot(unrestricted.mod2, terms = "pidfac", 
               se=TRUE, plot=FALSE)
tp2 <- termplot(mono.mod2, terms = 'pbm(pid, mono = "down")', 
         se=TRUE, plot=FALSE)
tp[[1]]$x <- tp2[[1]]$x
tp_pid <- bind_rows(tp[[1]], tp2[[1]])
tp_pid$model <- factor(rep(1:2, each=nrow(tp[[1]])), 
                      labels=c("Unconstrained", "Monotonic"))
ggplot(tp_pid, aes(x=x, y=plogis(y), colour=model, fill=model, 
                   ymin = plogis(y-1.96*se), 
                   ymax = plogis(y+1.96*se))) + 
  geom_ribbon(alpha=.2, col="transparent") + 
  geom_line() + 
  theme_xaringan() + 
  theme(legend.position = "top") + 
  labs(x="Party ID", y="Predicted Pr(Dem Vote)")



## ---------------------------------------------------------------------------------------------------------------
library(gamlss.ggplots)
moddat <- get_all_vars(mono.mod1, dat)
cen_moddat <- lapply(moddat, \(x)DAMisc::central(x))
cen_moddat$polity_dem <- 0:10
cen_moddat <- do.call(data.frame, cen_moddat)

registerDoParallel(cores = 10)
B1 <- BayesianBoot(mono.mod1, B=100, newdata=cen_moddat)
stopImplicitCluster()
post_sum <- t(apply(B1$par$mu, 1, \(x)c(mean(x), sd(x), quantile(x, c(.025,.975)))))
colnames(post_sum) <- c("fit", "sd", "lwr", "upr")

cen_moddat <- cbind(cen_moddat, post_sum)

## ----echo=TRUE, eval=FALSE--------------------------------------------------------------------------------------
ggplot(cen_moddat,
       aes(x=polity_dem, y=fit,
           ymin=lwr, ymax=upr)) +
  geom_ribbon(fill="gray75", alpha=.25) +
  geom_line(color="black") +
  theme_bw() +
  labs(x="Fitted Values", y="Polity")


## ----echo=FALSE, out.width="100%", fig.align="center"-----------------------------------------------------------
ggplot(cen_moddat, 
       aes(x=polity_dem, y=fit, 
           ymin=lwr, ymax=upr)) + 
  geom_ribbon(fill="gray75", alpha=.25) + 
  geom_line(color="black") + 
  theme_bw() + 
  labs(x="Fitted Values", y="Polity")


## ---------------------------------------------------------------------------------------------------------------
moddat <- get_all_vars(mono.mod1, dat)
cen_moddat <- lapply(moddat, \(x)DAMisc::central(x))
cen_moddat$polity_dem <- c(0,10)

cen_moddat <- do.call(data.frame, cen_moddat)

registerDoParallel(cores = 10)
B1 <- BayesianBoot(mono.mod1, B=100, newdata=cen_moddat)
stopImplicitCluster()
post <- B1$par$mu

psum <- t(apply(post, 1, \(x)c(mean(x), quantile(x, c(.025, .975)))))
fd <- apply(post, 2, diff)
fd <- c(mean(fd), quantile(fd, c(.025, .975)))
out <- rbind(psum, fd) %>% 
  as_tibble() %>% 
  setNames(c("fit", "lwr", "upr")) %>% 
  mutate(condition = c("Polity = 0", "Polity = 10", "Difference"), .before="fit")
out


## ----inter1, eval=FALSE, echo=T---------------------------------------------------------------------------------
## devtools::install_github('xuyiqing/interflex')
library(interflex)
## make some data
set.seed(43901)
X1 <- rnorm(200, 3, 1)
X2 <- runif(200, -3, 3)
e <- rnorm(200, 0, 4)
D1 <- rbinom(200, 1, .5)
Y1 <- 5-4*X1 -9*D1 + 3*D1*X1 + e
Y2 <- 2.5- X2^2 -5*D1 + 2*D1*X2^2  + e
dat <- as.data.frame(cbind(Y1,Y2,D1,X1, X2))
dat$D10 <- 1-dat$D1
i1 <- interflex(estimator="raw",
               dat,
               "Y1",
               "D1",
               "X1",
               treat.type="discrete")

plot(i1)

## ----inter2, eval=FALSE, echo=T---------------------------------------------------------------------------------
## make some data
i2 <- interflex(estimator="raw",
               dat,
               "Y2",
               "D1",
               "X2",
               treat.type="discrete")

plot(i2)


## ----ref.label="inter2", echo=FALSE, eval=TRUE, fig.align="center", out.width="80%"-----------------------------


## ----binest1, echo=T, eval=FALSE--------------------------------------------------------------------------------
## use a binning estimator to estimate the interaction effect
b1 <- interflex(estimator="binning",
                dat,
                "Y1",
                "D1",
                "X1",
                treat.type="discrete")
plot(b1)


## ----binest2, echo=T, eval=FALSE--------------------------------------------------------------------------------
## use a binning estimator to estimate the interaction effect
b2 <- interflex(estimator="binning",
                dat,
                "Y2",
                "D1",
                "X2",
                treat.type="discrete")
plot(b2)


## ----praw, echo=TRUE, fig.height=4, fig.width=12, fig.align="center", out.width="100%"--------------------------
data(Prestige, package="carData")
interflex("raw", Prestige, "prestige", "income", "education", ncols=3)


## ----pbin, echo=T, fig.show='hide'------------------------------------------------------------------------------
pres.b <- interflex("binning", 
                    Prestige, 
                    "prestige", 
                    "income", 
                    "education",
                    Z = c("type", 
                          "women"), 
                    na.rm=T, 
                    figure=FALSE)


## ----echo=TRUE--------------------------------------------------------------------------------------------------
pres.b$tests


## ----echo=FALSE, fig.height=6, fig.width=6, fig.align="center", out.width="75%"---------------------------------
plot(pres.b)


## ----gamfake1, echo=T-------------------------------------------------------------------------------------------
library(gamlss.add)
mod1 <- gamlss(Y1 ~ D1 + 
                 pb(X1) + 
                 pb(I(X1*(D1 == 1))),  
               data=dat)
mod2 <- gamlss(Y1 ~ X1*D1, data=dat)
VC.test(mod1, mod2)


## ----gamfake2, echo=T-------------------------------------------------------------------------------------------
library(gamlss.add)
mod1 <- gamlss(Y2 ~ ga(~ D1 + 
                 s(X2, by=D1, bs="ts")+ 
                 s(X2, by=D10, bs="ts")),
               data=dat)
mod2 <- gamlss(Y2 ~ X2*D1, data=dat)
VC.test(mod1, mod2)


## ---------------------------------------------------------------------------------------------------------------
fake.dat <- expand.grid(
  X2 = seq(min(dat$X2), max(dat$X2), length=100), 
  D1 = c(0,1)
)
fake.dat$D10 <- 1-fake.dat$D1
lbx2 <- max(min(dat$X2[which(dat$D1 == 0)]), 
            min(dat$X2[which(dat$D1 == 1)]))
ubx2 <- min(max(dat$X2[which(dat$D1 == 0)]), 
            max(dat$X2[which(dat$D1 == 1)]))
fake.dat <- fake.dat %>% 
  filter(X2 > lbx2 & X2 < ubx2)
X <- model.matrix(mod1$mu.coefSmo[[1]], newdata=fake.dat)
fit <- X %*% mod1$mu.coefSmo[[1]]$coefficients
b <- MASS::mvrnorm(1500, 
                   coef(mod1$mu.coefSmo[[1]]), 
                   vcov(mod1$mu.coefSmo[[1]]))
Xb <- X %*% t(b)
diff <- Xb[99:196, ] - Xb[1:98, ]
mean.diff <- rowMeans(diff)
diff.ci <- apply(diff, 1, quantile, c(.025,.975))
fake.dat$diff <- fit[99:196]-fit[1:98]
fake.dat$lower <- diff.ci[1,]
fake.dat$upper <- diff.ci[2,] 
fake.dat <- fake.dat[which(fake.dat$D1 == 0), ]


## ----treat1, eval=FALSE-----------------------------------------------------------------------------------------
ggplot(fake.dat, aes(x=X2, y=diff,
                     ymin=lower, ymax=upper)) +
  geom_ribbon(alpha=.2, col="transparent") +
  geom_line() +
  theme_xaringan() +
  labs(x="X2", y="Predicted Treatment Effect")



## ----gamintertest, echo=T---------------------------------------------------------------------------------------
library(mgcv)
## estimate the GAM with no smooths (essentially just a glm)
## that has an interaction between the log of income and 
## education to capture the presumed parametric trend.
Prestige <- na.omit(Prestige)
mod1 <- gamlss(prestige ~ log(income)*education + 
    women + type, data=Prestige)
## estimate the second model with a smooth on income and education
mod2 <- gamlss(prestige ~ 
    ga(~ ti(income) + ti(education) + ti(income, education)) + 
    women + type, data=Prestige)
## test the difference between the two models,.
VC.test(mod1, mod2)


## ----eval=FALSE-------------------------------------------------------------------------------------------------
s_inc <- with(Prestige, seq(min(income), max(income), length=25))
s_ed <- with(Prestige, seq(min(education), max(education), length=25))
s_linc <- log(s_inc)

p1 <- Vectorize(function(x, y, ...){
  d <- data.frame(type = factor(1, levels=1:3, labels=c("bc", "prof", "wc")),
                  women = 29,
                  income = x,
                  education = y)
  predict(mod1, newdata=d)
})
p2 <- Vectorize(function(x, y, ...){
  d <- data.frame(type = factor(1, levels=1:3,
                    labels=c("bc", "prof", "wc")),
                  women = 29,
                  income = x,
                  education = y)
  predict(mod2, newdata=d)
})

o1 <- outer(s_inc, s_ed, p1)
o2 <- outer(s_inc, s_ed, p2)

plot_ly() %>%
  add_trace(x=~s_inc, y=~s_ed, z=~t(o1), type="surface",
    colorscale=list(c(0,1),
      RColorBrewer::brewer.pal(9, "Blues")[c(1,9)])) %>%
  add_trace(x=~s_inc, y=~s_ed, z=~t(o2), type="surface",
    colorscale=list(c(0,1),
      RColorBrewer::brewer.pal(9, "Reds")[c(1,9)]))


## ----eval=FALSE-------------------------------------------------------------------------------------------------
fldat <- rio::import("fl_repdata.dta")
fldat$onset <- ifelse(fldat$onset > 1, 1, fldat$onset)
bmod <- glm(onset ~ warl + gdpenl + lpopl1 +
      lmtnest + ncontig + Oil + nwstate + instab +
      polity2l + ethfrac + relfrac,
    data=fldat, family=binomial)


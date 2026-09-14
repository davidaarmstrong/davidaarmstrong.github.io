library(rio)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
select <- function(...)dplyr::select(...)

## ----fig.height=2, fig.width=10, out.width="\\textwidth", out.height=".2\\textwidth", fig.align="center", echo=FALSE--------------------------------------
library(ggplot2)
df1 <- data.frame(
  ystar = seq(-3,3,length=601), 
  y = 1)
ggplot(df1) + 
  geom_segment(aes(x=min(ystar), xend=max(ystar), y=y, yend=y),
    arrow=arrow(length=unit(.5, "cm"), ends="both")) + 
  theme_bw() + 
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank()) + 
  labs(y="", x="y*") 


## ----fig.height=2, fig.width=10, out.width="\\textwidth", out.height=".2\\textwidth", fig.align="center", echo=FALSE--------------------------------------
library(ggplot2)
df1 <- data.frame(
  ystar = seq(-3,3,length=601), 
  y = 0)
cuts <- data.frame(
  x=c(-2,.5,2), 
  y1=c(-.1,-.1,-.1),
  y2=c(.1,.1,.1)
)
ggplot(df1) + 
  geom_segment(aes(x=min(ystar), xend=max(ystar), 
                        y=y, yend=y),
    arrow=arrow(length=unit(.5, "cm"), ends="both")) + 
  geom_segment(data=cuts, aes(x=x, xend=x, y=y1, yend=y2)) + 
  theme_bw() + 
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text.x = element_text(size=15)) + 
  labs(y="", x="y*") + 
  scale_x_continuous(breaks=c(-2,.5,2), labels=c(expression(tau[1]), expression(tau[2]), expression(tau[3]))) + 
  annotate("text", x=c(-2.5, -.75, 1.25, 2.5), y=.05, label=c("1", "2", "3", "4"), size=5)


## ----olex, echo=T-----------------------------------------------------------------------------------------------------------------------------------------
library(ordinal)
dat <- rio::import("data/ologit_data.dta")
dat$sd_fac <- factor(dat$sd, levels=1:5)
mod <- clm(sd_fac ~ cwarcow + iwarcow + logpop + logpcgnp + poly(vanadd, 2), data=dat)
summary(mod)


## ----lrtests, echo=T--------------------------------------------------------------------------------------------------------------------------------------
library(car)
Anova(mod)


## ----prgraph2, echo=TRUE, eval=FALSE----------------------------------------------------------------------------------------------------------------------
library(marginaleffects)
probs <- predictions(mod, newdata=dat)
probs <- probs %>%  filter(group == sd)
ggplot(probs, aes(x=estimate)) +
  geom_histogram(aes(y=after_stat(ndensity)), bins=15) +
  facet_wrap(~sd, nrow=3) +
  theme_bw() +
  theme(panel.grid=element_blank())


## ----oc1, echo=T------------------------------------------------------------------------------------------------------------------------------------------
library(tidyr)
mer_comps <- comparisons(mod, newdata = datagrid("median"), 
                         variables=list(cwarcow = c(0,1), 
                                        iwarcow = c(0,1), 
                                        logpop = "2sd", 
                                        logpcgnp = "2sd", 
                                        vanadd = "2sd"), 
                         type="prob") 
mer_comps %>% 
  mutate(estimate = sprintf("%.2f%s", estimate, ifelse(sign(conf.low) == sign(conf.high), "*", ""))) %>%
  select(term, contrast, group, estimate) %>%
  pivot_wider(names_from = "group", values_from = "estimate")


## ----oc2p, echo=T, eval=FALSE-----------------------------------------------------------------------------------------------------------------------------
ggplot(mer_comps, aes(x=as.factor(group), y=estimate,
                     ymin=conf.low, ymax=conf.high)) +
  geom_pointrange(size=.1) +
  geom_hline(yintercept=0, linetype=3) +
  facet_wrap(~term, ncol=2) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  labs(x="", y="Change in Predicted Probability")

## ----oc2, echo=T------------------------------------------------------------------------------------------------------------------------------------------
ave_comps <- avg_comparisons(mod, 
                         variables=list(cwarcow = c(0,1), 
                                        iwarcow = c(0,1), 
                                        logpop = "2sd", 
                                        logpcgnp = "2sd", 
                                        vanadd = "2sd"), 
                         type="prob") 
ave_comps %>% 
  mutate(estimate = sprintf("%.2f%s", estimate, ifelse(sign(conf.low) == sign(conf.high), "*", ""))) %>%
  select(term, contrast, group, estimate) %>%
  pivot_wider(names_from = "group", values_from = "estimate")



## ----oc2p1, echo=T, eval=FALSE, fig.height=7, fig.width=6, out.width="100%"-------------------------------------------------------------------------------
bcomps <- bind_rows(
  mer_comps %>%
    select(term, group, estimate, conf.low, conf.high) %>%
    mutate(type = "Reasonable Values"),
  ave_comps %>%
    select(term, group, estimate, conf.low, conf.high) %>%
    mutate(type = "Observed Values"))


ggplot(bcomps, aes(x=as.factor(group), y=estimate,
                     ymin=conf.low, ymax=conf.high,
                  colour=type)) +
  geom_pointrange(size=.05, position=position_dodge(width=.5)) +
  geom_hline(yintercept=0, linetype=3) +
  facet_wrap(~term, ncol=2) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        legend.position="bottom") +
  labs(x="", y="Change in Predicted Probability")

## ----merplot, echo=T, eval=FALSE--------------------------------------------------------------------------------------------------------------------------
pred_gnp <- plot_predictions(mod,
                             newdata="median",
                             condition="logpcgnp",
                             draw=FALSE)
ggplot(pred_gnp, aes(x=logpcgnp, y=estimate,
                     ymin=conf.low,
                     ymax=conf.high,
                     fill=as.factor(group),
                     colour=as.factor(group))) +
  geom_ribbon(alpha=.15, colour="transparent") +
  geom_line() +
  theme_classic() +
  theme(legend.position="bottom") +
  labs(x="GNP/capita", y="Predicted Probability",
       colour = "Level of Repression",
       fill = "Level of Repression")

## ----merplot2, echo=T, eval=FALSE-------------------------------------------------------------------------------------------------------------------------
pred_dem <- plot_predictions(mod,
                        condition="vanadd",
                        newdata = "median",
                        draw=FALSE)

ggplot(pred_dem, aes(x=vanadd, y=estimate,
                     ymin=conf.low,
                     ymax=conf.high,
                     fill=as.factor(group),
                     colour=as.factor(group))) +
  geom_ribbon(alpha=.15, colour="transparent") +
  geom_line() +
  theme_classic() +
  theme(legend.position="bottom") +
  labs(x="Polyarchy", y="Predicted Probability",
       colour = "Level of Repression",
       fill = "Level of Repression")

## ----ameplot, echo=T, eval=FALSE--------------------------------------------------------------------------------------------------------------------------
seq_range <- function(x,n=100){
  mn <- min(x, na.rm=TRUE)
  mx <- max(x, na.rm=TRUE)
  seq(mn, mx, length=n)
}
pred_gnp2 <- avg_predictions(mod,
                        variables=list(logpcgnp=seq_range(dat$logpcgnp, n=100)))

ggplot(pred_gnp2, aes(x=logpcgnp, y=estimate,
                     ymin=conf.low,
                     ymax=conf.high,
                     fill=as.factor(group),
                     colour=as.factor(group))) +
  geom_ribbon(alpha=.15, colour="transparent") +
  geom_line() +
  theme_classic() +
  theme(legend.position="bottom") +
  labs(x="GNP/capita", y="Predicted Probability",
       colour = "Level of Repression",
       fill = "Level of Repression")


## ----ordfit, echo=T---------------------------------------------------------------------------------------------------------------------------------------
library(DAMisc)
ordfit(mod, data=dat)


## ----ordfit2, echo=T--------------------------------------------------------------------------------------------------------------------------------------
p <- MASS::polr(formula(mod), data=dat)
pre(p, data=dat)


## ----brant, echo=T----------------------------------------------------------------------------------------------------------------------------------------
library(brant)
dat$vascale <- scale(dat$vanadd)
bmod  <- MASS::polr(sd_fac ~ cwarcow + iwarcow + logpop +
        logpcgnp + vascale + I(vascale^2), data=dat)


## ----warning=TRUE, message=TRUE---------------------------------------------------------------------------------------------------------------------------
library(brant)
brant(bmod)


## ----eval=FALSE-------------------------------------------------------------------------------------------------------------------------------------------
X <- model.matrix(bmod)[,-1]
b <- coef(bmod)

xb <- X %*% b
tau <- c(-Inf, bmod$zeta, Inf)
q <- sapply(tau, function(t)plogis(t-xb))
D <- matrix(0, nrow=ncol(q), ncol = ncol(q)-1)
D[c(1,2), 1] <- c(-1,1)
D[c(2,3), 2] <- c(-1,1)
D[c(3,4), 3] <- c(-1,1)
D[c(4,5), 4] <- c(-1,1)
D[c(5,6), 5] <- c(-1,1)

probs <- q %*% D

brant_stats <- NULL
sink(tempfile())
for(i in 1:2500){
  newy <- as.factor(apply(probs, 1, function(x)
            which.max(rmultinom(1, 1, x))))
  u <- update(bmod, newy ~ .)
  brant_stats <- c(brant_stats, brant(u)[1])
}
sink()

## ----eval=TRUE, echo=TRUE, fig.height=6, fig.width=8, out.width="100%", fig.align="center"----------------------------------------------------------------
# load("data/brant_stats.rda")
ggplot() + 
  stat_density(aes(x=brant_stats, colour="Empirical"), geom="line") + 
  stat_function(fun = function(x)dchisq(x, 18), aes(colour="Theoretical"), geom="line") + 
  theme_classic() + 
  theme(legend.position="inside", 
        legend.position.inside = c(.85, .85)) + 
  labs(x="Brant Statistic", y = "Density", colour="")


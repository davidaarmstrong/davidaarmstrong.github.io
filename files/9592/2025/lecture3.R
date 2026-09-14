## ----setup, include=FALSE---------------------------------------------------------------------------------------
library(rio)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(formatR)
library(knitr)
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


## ----label=inccat, echo=T---------------------------------------------------------------------------------------
## show model with interaction of two
## categorical variables
library(DAMisc)
library(car)
data(Duncan)
## make a 3-category income variable for the
## purposes of this exercise.  We probably
## wouldn't do this to a quantitative variable
## in 'real life'.
Duncan <- Duncan %>% 
  mutate(inc.cat = cut(Duncan$income, 3), 
         inc.cat = factor(as.numeric(inc.cat), 
                          labels=c("Low", "Middle", "High")))

mod <- lm(prestige~ inc.cat * type + education,
  data=Duncan)


## ----label=summod-----------------------------------------------------------------------------------------------
S(mod, brief=TRUE)


## ----incF1, echo=T----------------------------------------------------------------------------------------------
## Anova (big-A) shows whether all product regressors
## are jointly significantly different from zero.
Anova(mod)


## ----label=eff2cata, eval=FALSE, echo=T-------------------------------------------------------------------------
## load marginal effects package
library(marginaleffects)
## generate predictions for the interaction
p1 <- predictions(mod, newdata = "mean", variables=list(inc.cat = unique, type = unique))
## generate plot the effect with error bars
ggplot(p1) +
  geom_pointrange(aes(x=inc.cat, y=estimate,
                 ymin=conf.low,
                 ymax=conf.high)) +
  facet_wrap(~type, ncol=2) +
  theme_bw() +
  labs(x="Income Category",
       y="Predicted Prestige")

## ---------------------------------------------------------------------------------------------------------------
p1 %>%
  mutate(param = paste0("b", row_number())) %>% 
  select(param, inc.cat, type, estimate) %>% 
  as.data.frame()


## ---------------------------------------------------------------------------------------------------------------
hypotheses(p1, "b3-b6 = b2 -b5", 
           df = mod$df.residual)


## ----catcont, echo=T--------------------------------------------------------------------------------------------
## Divide income by 1000 to rescale
## income coefficient
data(Prestige, package="carData")
Prestige$income <- Prestige$income/1000
## estimate model
mod <- lm(prestige ~ income*type + education,
        data=Prestige)
S(mod, brief=TRUE)


## ----label=anova, echo=T----------------------------------------------------------------------------------------
## look at income:type line to figure out
## whether all income:type regressors are
## jointly different from zero
Anova(mod)


## ----label=condinc, echo=T--------------------------------------------------------------------------------------
## calculate slopes of income for each
## value of type
(s <- slopes(mod, variables="income", by = "type"))
## calculate pairwise comparisons of slopes
hypotheses(s, hypothesis = ~pairwise)


## ----condinc2, echo=T, eval=FALSE-------------------------------------------------------------------------------
preds <- predictions(mod, newdata="mean",
            variables = list(income = unique,
                             type=unique))
library(patchwork)
g1 <- ggplot(preds,
             aes(x=income,
                 y=estimate,
                 ymin=conf.low,
                 ymax=conf.high,
                 fill=type,
                 color=type)) +
  geom_ribbon(alpha=.15,
              color="transparent") +
  geom_line()  +
  theme_classic() +
  theme(legend.position="bottom")

g2 <- ggplot(Prestige %>%
               filter(!is.na(type)),
             aes(x=income,
                 fill=type)) +
  geom_histogram(position="identity",
                 alpha=.15,
                 show.legend = FALSE) +
  theme_void()

g2 +
  g1 +
  plot_layout(nrow = 2,
              heights = c(1, 4))

## ----label=condtype, echo=T, eval=TRUE--------------------------------------------------------------------------
num3 <- function(x, mult=1){
  s <- sd(x, na.rm=TRUE)*mult
  m <- mean(x, na.rm=TRUE)
  c("Mean - SD" = m-s, "Mean" = m, "Mean + SD" = m+s)
}
p2 <- purrr::map(num3(Prestige$income), \(x)
             avg_predictions(mod, newdata= datagrid(income=x), variables=list(type=unique)))
             
bind_rows(lapply(p2, \(x)hypotheses(x, hypothesis=~pairwise)), .id="income") %>% 
  as_tibble() %>% 
  select(income, hypothesis, estimate, std.error, conf.low, conf.high)


## ----2contex, echo=T--------------------------------------------------------------------------------------------
## estimate model with income and
## education interaction
mod <- lm(prestige ~ income*education + type, data=Prestige)
S(mod, brief=TRUE)


## ----2cont1, echo=T, fig.height=10, fig.width=6, out.height="50%", fig.align="center", eval=FALSE---------------
library(ggplotify)
## marginal effect plot as suggested by
## Berry, Golder, Milton (2012) with
## density histogram, print to two PDFs
## saved in R's working directory.
## to print the file to the screen, use plot.type="screen"
h1 <- ggplot(Prestige, aes(x=education)) +
  geom_histogram() +
  theme_void()
m1 <- plot_slopes(mod, variable="income", condition="education")  +
  geom_hline(yintercept=0, linetype=3) +
  theme_classic() +
  labs(x="Education", y="Conditional Effect of Income")
m1 <- m1 - annotation_custom(as.grob(h1), ymax=calc_ymax(m1))

m2 <- plot_slopes(mod, variable="education", condition="income") +
  geom_hline(yintercept=0, linetype=3) +
  theme_classic() +
  labs(x="Income", y="Conditional Effect of Education")
h2 <- ggplot(Prestige, aes(x=income)) +
  geom_histogram() +
  theme_void()
m2 <- m2 - annotation_custom(as.grob(h2), ymax=calc_ymax(m2))

m1 + m2 + plot_layout(nrow=2)

## ----changesig, echo=T------------------------------------------------------------------------------------------
## calculate changes in significance
changeSig(mod, c("income", "education"))


## ---------------------------------------------------------------------------------------------------------------
library(DAMisc)
BGMtest(mod, c("income", "education"))

## ----2dex, echo=T-----------------------------------------------------------------------------------------------
dat <- import("data/anes2012.dta")
mod <- glm(votedem ~ black + evprot + incgroup_num *lrself + 
             econ_retnat, data=dat, family=binomial)
summary(mod)


## ----secdiff1, echo=T-------------------------------------------------------------------------------------------
avg_predictions(mod, variables = list(incgroup_num = c(9,22), lrself=c(3,8)), 
                hypothesis="(b3-b1)-(b4-b2)=0")


## ---------------------------------------------------------------------------------------------------------------
avg_comparisons(mod, 
                newdata = datagrid(lrself=c(3,8), grid_type = "counterfactual"), 
                variables = list(incgroup_num = c(9,22)), by="lrself", 
                hypothesis = ~ pairwise)


## ---------------------------------------------------------------------------------------------------------------
avg_comparisons(mod, 
                newdata = datagrid(lrself=c(3,8), grid_type = "counterfactual"), 
                variables = list(incgroup_num = c(9,22)), by="lrself", 
                hypothesis = ~ revpairwise)


## ----eff, echo=T, eval=FALSE, out.width="100%"------------------------------------------------------------------
pv <- avg_predictions(mod,
                      variables=list(incgroup_num=unique,
                                     lrself=num3)) %>%
  mutate(cond = as.factor(lrself))
levels(pv$cond) <- c("Mean - SD", "Mean", "Mean + SD")

ggplot(pv, aes(x=incgroup_num,
               y=estimate,
               ymin=conf.low,
               ymax=conf.high,
               fill=cond)) +
  geom_ribbon(alpha=.15, color="transparent") +
  geom_line(aes(color=cond)) +
  theme_classic() +
  theme(legend.position="top") +
  labs(x="Income Group",
       y="Pr(Vote Obama)",
       colour="L-R Self", fill="L-R Self")


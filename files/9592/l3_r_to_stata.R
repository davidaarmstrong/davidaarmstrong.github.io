## ----setup, echo=FALSE, include=FALSE----------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, size="small")
options(width = 400)
library(Statamarkdown)
library(ggplot2)
library(marginaleffects)
library(modelsummary)
library(dplyr)
library(tidyr)
library(car)
library(rio)
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


pre {
  font-size: 5px
}

## ----------------------------------------------------------------------------------------------------------------------------------------
## show model with interaction of two
## categorical variables
library(DAMisc)
## I made a 3-category income variable for the
## purposes of this exercise.  We probably
## wouldn't do this to a quantitative variable
## in 'real life'.
Duncan <- import("Duncan_cat.dta")
Duncan <- factorize(Duncan)
mod <- lm(prestige~ inc_cat * type + education,
  data=Duncan)
summary(mod)


use Duncan_cat.dta, clear
reg prestige i.inc_cat##i.type education 

## ----------------------------------------------------------------------------------------------------------------------------------------
## Anova (big-A) shows whether all product regressors
## are jointly significantly different from zero.
Anova(mod, type=3)


* Test inc_cat terms
 test (2.inc_cat = 0) (3.inc_cat = 0)

* Test type terms
test (2.type=0) (3.type=0)

* Test interaction terms
test (2.inc_cat#2.type = 0) (2.inc_cat#3.type = 0) (3.inc_cat#2.type = 0) (3.inc_cat#3.type = 0)

* Test education
test education

## ----------------------------------------------------------------------------------------------------------------------------------------
## load marginal effects package
library(marginaleffects)
## generate predictions for the interaction
p1 <- predictions(mod, newdata = "mean", variables=list(inc_cat = unique, type = unique))

p1 %>%
  mutate(param = paste0("b", row_number())) %>% 
  select(param, inc_cat, type, estimate, std.error, conf.low, conf.high) %>% 
  as.data.frame()


margins inc_cat#type

## ----------------------------------------------------------------------------------------------------------------------------------------
hypotheses(p1, "b3-b6 = b2 -b5", 
           df = mod$df.residual)


margins inc_cat#type, post
test 3.inc_cat#1.type - 2.inc_cat#1.type - 3.inc_cat#3.type + 2.inc_cat#3.type = 0

## ----fig.align="center", out.width="75%"-------------------------------------------------------------------------------------------------
## generate plot the effect with error bars
ggplot(p1) +
  geom_pointrange(aes(x=inc_cat, y=estimate,
                 ymin=conf.low,
                 ymax=conf.high)) +
  facet_wrap(~type, ncol=2) +
  theme_bw() +
  labs(x="Income Category",
       y="Predicted Prestige")


marginsplot, by(type) recast(scatter) plotopts(msymbol(circle))
graph export "margins_2cat.png", as(png) replace

## ----echo=FALSE, fig.align="center", out.width="75%"-------------------------------------------------------------------------------------
knitr::include_graphics("margins_2cat.png")


## ----eval=TRUE---------------------------------------------------------------------------------------------------------------------------
data(Prestige, package="carData")
Prestige$income <- Prestige$income/1000
mod <- lm(prestige ~ income*type + education,
        data=Prestige)
summary(mod)


use Prestige.dta, clear
replace income = income/1000
reg prestige c.income##i.type education

## ----------------------------------------------------------------------------------------------------------------------------------------
Anova(mod, type=3)


test 2.type 3.type
test 2.type#c.income 3.type#c.income

## ----------------------------------------------------------------------------------------------------------------------------------------
s <- slopes(mod, variables="income", by = "type")
s
hypotheses(s, hypothesis = "pairwise")


margins, dydx(income) at(type = (1 2 3)) 
margins, dydx(income) at(type = (1 2 3)) pwcompare

## ----fig.align="center", out.width="75%"-------------------------------------------------------------------------------------------------
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



quietly margins, at(income=(.5(1.25)26.75) type=(1 2 3))
marginsplot, recast(line) recastci(rarea) ///
  ci1opts(color("red%20")) plot1opts(lcolor("red")) ///
  ci2opts(color("green%20")) plot2opts(lcolor("green") lpattern("solid")) ///
  ci3opts(color("blue%20")) plot3opts(lcolor("blue") lpattern("solid")) ///
  xlabel(.5 5.75 11 16.25 21.5 26.75) saving(m1, replace)

twoway (hist income if type == 1, color("red%20") freq width(1.25) start(.5)) ///
  (hist income if type == 2, color("green%20") freq width(1.25) start(.5)) ///
  (hist income if type == 3, color("blue%20") freq  width(1.25) start(.5)), ///
  legend(label(1 "BC") label(2 "Prof") label(3 "WC")) xlabel(.5 5.75 11 16.25 21.5 26.75) saving(h1, replace) fysize(25)

gr combine h1.gph m1.gph, cols(1) 
graph export "cat_con_eff_hist.png", replace

## ----echo=FALSE, fig.align="center", out.width = "75%"-----------------------------------------------------------------------------------
knitr::include_graphics("cat_con_eff_hist.png")


## ----eval=FALSE--------------------------------------------------------------------------------------------------------------------------
p2 <- purrr::map(c(4.6, 6.7, 8.8), \(x)
             avg_predictions(mod, newdata= datagrid(income=x), variables=list(type=unique)))
             
bind_rows(lapply(p2, \(x)hypotheses(x, hypothesis="pairwise")), .id="income") %>% 
  as_tibble() %>% 
  select(income, term, estimate, std.error, conf.low, conf.high)


foreach n of numlist 4.1 6.7 8.8 {
	margins type, at(income=`n') pwcompare
}

## ----------------------------------------------------------------------------------------------------------------------------------------
mod <- lm(prestige ~ income*education + type, data=Prestige)
summary(mod)


use Prestige.dta, clear
replace income = income/1000
reg prestige c.income##c.education i.type

## ----fig.align="center", out.width="75%"-------------------------------------------------------------------------------------------------
library(ggplotify)
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


margins, dydx(income) at(education = (6(1)16)) 
marginsplot, recast(line) recastci(rarea) ciopts(color("%15")) saving(inc_marg, replace) title("")
twoway hist education, start(6) width(.5) saving(ed_hist, replace) freq fysize(25) xtitle("")

margins, dydx(education) at(income = (.5(2)26.5)) 
marginsplot, recast(line) recastci(rarea) ciopts(color("%15")) saving(ed_marg, replace) title("")
twoway hist income, start(.5) width(2) saving(inc_hist, replace) freq fysize(25) xtitle("")

gr combine ed_hist.gph inc_marg.gph, cols(1) xcommon saving(inc_eff, replace)
gr combine inc_hist.gph  ed_marg.gph, cols(1) xcommon saving(ed_eff, replace)

gr combine inc_eff.gph ed_eff.gph, cols(2)
graph export "cond_eff_inc_ed.png", replace

## ----echo=FALSE, fig.align="center", out.width="100%"------------------------------------------------------------------------------------
knitr::include_graphics("cond_eff_inc_ed.png")


## ----------------------------------------------------------------------------------------------------------------------------------------
changeSig(mod, c("income", "education"))


reg prestige c.income##c.education i.type
mat list e(b)

do "changeSig.do"
changeSig, xind(1) zind(2) multind(3)

## ----------------------------------------------------------------------------------------------------------------------------------------
library(DAMisc)
BGMtest(mod, c("income", "education"))


sum educ
margins, dydx(income) at(education = (`r(min)' `r(max)'))
sum income 
margins, dydx(education) at(income = (`r(min)' `r(max)'))

## ----------------------------------------------------------------------------------------------------------------------------------------
dat <- import("data/anes2012.dta")
dat <- rio::factorize(dat)
mod <- glm(votedem ~ black + evprot + incgroup_num *lrself + 
             econ_retnat, data=dat, family=binomial)
summary(mod)


use data/anes2012.dta, clear
logit votedem black evprot c.incgroup_num##c.lrself i.econ_retnat

## ----------------------------------------------------------------------------------------------------------------------------------------
avg_slopes(mod, 
  newdata = datagrid(lrself=0:10, grid_type = "counterfactual"), 
  variables="incgroup_num", 
  by="lrself")


margins, dydx(incgroup_num) at(lrself = (0(1)10))

## ----------------------------------------------------------------------------------------------------------------------------------------
avg_slopes(mod, 
  newdata = datagrid(lrself=c(0,9), grid_type = "counterfactual"), 
  variables="incgroup_num", 
  by="lrself", 
  hypothesis = "b2-b1=0")


margins, dydx(incgroup_num) at(lrself= (0 9)) pwcompare

## ----------------------------------------------------------------------------------------------------------------------------------------
avg_comparisons(mod, 
  newdata = datagrid(lrself=c(0,9), grid_type = "counterfactual"), 
  variables="incgroup_num", 
  by="lrself", 
  hypothesis = "b2-b1=0")


margins, at(lrself=(0 9)) at(lrself=(0 9) incgroup_num=generate(incgroup_num+1)) post
lincom (4._at - 3._at) - (2._at - 1._at)

## ----fig.align="center", out.width="75%"-------------------------------------------------------------------------------------------------
pv <- avg_predictions(mod,
        newdata = datagrid(lrself=c(0,5,10), grid_type="counterfactual"), 
        variables=list(incgroup_num=unique), 
        by=c("lrself", "incgroup_num")) %>% 
  mutate(cond = as.factor(lrself))
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


quietly logit votedem black evprot c.incgroup_num##c.lrself i.econ_retnat
quietly margins, at(incgroup_num = (1(1)28) lrself = (0 5 10))
marginsplot, recast(line) recastci(rarea) ///
  plot1opts(lcolor("red")) ///
  plot2opts(lcolor("green")) ///
  plot3opts(lcolor("blue") lpattern("solid")) ///
  ci1opts(color("red%15")) ///
  ci2opts(color("green%15")) ///
  ci3opts(color("blue%15")) ///
  xtitle("Income Group") ///
  ytitle("Predicted Pr(Vote Obama)")
graph export "inc_ed_eff_stata.png", replace

## ----fig.align="center", out.width="75%"-------------------------------------------------------------------------------------------------
knitr::include_graphics("inc_ed_eff_stata.png")


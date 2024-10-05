###################################
## Code for POLSCI 9590 Week 10  ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

library(tidyverse)
library(srvyr)
library(rio)
library(DAMisc)
library(survey)

corfun <- function(df, var1, var2, level=.95, digits=3){
  require(survey)
  form <- glue::glue("scale({var1}) ~ scale({var2})-1")
  m = svyglm(form, design = df)
  r = coef(m)[1]
  p <- summary(m)$coef[1,4]
  r <- sprintf(glue::glue("%.{digits}f"), r)
  r <- glue::glue("{r}{ifelse(p < 1-level, '*', '')}")
  cat(glue::glue("r({var1},{var2}) = {r}\n"))
}

set.seed(4532)
mu <- c(-5, 0, 10)
probs=c(.6, .3, .1)
pop <- data.frame(group = sample(1:3, 
                                 100000, 
                                 replace=TRUE, 
                                 prob = probs))
pop$y <- rnorm(100000, mu[pop$group], 2)
mean(pop$y)
samp <- pop %>% 
  group_by(group) %>% 
  mutate(n_pop = n()) %>%
  sample_n(500) %>% 
  mutate(weight = n_pop/n()) %>% 
  ungroup

samp %>% summarise(mean = mean(y))

samp %>% as_survey_design(weights=weight) %>% 
  summarise(mean = survey_mean(y))

ces19w <- import("ces19w.dta")
ces19w <- factorize(ces19w)
cesw <- ces19w %>% 
  as_survey_design(weights=weight)
sumStats(ces19w, var="market", byvar="agegrp")
sumStats(cesw, var="market", byvar="agegrp")


 cesw %>% 
  group_by(agegrp) %>% 
  summarise(m = survey_mean(market, na.rm=TRUE)) %>% 
  na.omit() %>% 
  mutate(lwr = m-1.96*m_se, 
         upr = m+1.96*m_se) %>% 
  ggplot(aes(x=agegrp, y=m, ymin=lwr, ymax=upr)) + 
    geom_pointrange() + 
    theme_bw() + 
    labs(x = "Age Group", 
         y="Average Market Liberalism (95% CI)")

xt(cesw, var="vote", byvar="agegrp")

corfun(cesw, "market", "leader_con")

w_mod <- svyglm(market ~ educ , design=cesw)
summary(w_mod)



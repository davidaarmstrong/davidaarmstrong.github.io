###################################
## Code for POLSCI 9590 Week 7   ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

library(ggplot2)
library(DAMisc)
library(dplyr)
library(rio)
library(ggmosaic)

ces <- import("ces19.dta")
ces <- factorize(ces)
levels(ces$relig) <- c("Atheist", "Protestant", "Catholic", "Other")
tab <- xt(ces, "vote", "relig")

tab$tab
tab$stats

tab <- xt(ces, "educ", "agegrp", ordinal=TRUE)
tab$tab

tab$stats

ces <- ces %>% 
  mutate(relig = factor(as.character(relig), 
    levels=c("Protestant", "Atheist", 
             "Other", "Catholic")))
d <- ces %>% 
  group_by(vote, relig) %>% 
  tally() %>% 
  na.omit() %>% 
  group_by(relig) %>% 
  mutate(pct = n/sum(n))

ggplot(d, aes(x=vote, y=pct, fill=relig)) + 
  geom_bar(stat="identity", position="dodge") + 
  theme_classic() + 
  theme(legend.position = c(.9, .8)) + 
  labs(x="Vote", fill="Religion",
       y="Percentage (by Religion)") + 
  scale_y_continuous(labels = scales::label_percent())

ggplot(ces %>% filter(!is.na(relig) & !is.na(vote))) +
  geom_mosaic(aes(x = product(relig), fill=vote), 
              show.legend = FALSE) +
  theme_mosaic() +
  scale_fill_manual(values = c("#d71920", "#003F72", "#F58220", "gray50"))

tab <- table(ces$vote, ces$relig)
tab %>% as.data.frame() %>%
  rename(vote = Var1, relig = Var2) %>% 
  mutate(stdres = c(chisq.test(tab)$stdres), 
    stdres2 = case_when(stdres < -3 ~ "e < -3",
      stdres >= -3 & stdres < -2 ~ "-3 <= e < 2", 
      stdres >= -2 & stdres < -1 ~ "-2 <= e < -1", 
      stdres >= -1 & stdres < 0 ~ "-1 <= e < 0", 
      stdres >= 0 & stdres < 1 ~ "0 <= e < 1", 
      stdres >= 1 & stdres < 2 ~ "1 <= e < 2", 
      stdres >= 2 & stdres <= 3 ~ "2 <= e < 3", 
      stdres > 3 ~ "3 < e"), 
   stdres2 = factor(stdres2, 
     levels=c("e < -3", "-3 <= e < 2", "-2 <= e < -1",  
        "-1 <= e < 0", "0 <= e < 1", "1 <= e < 2", "2 <= e < 3", "3 < e")), 
   vote = factor(as.character(vote), 
     levels=c("NDP", "Liberal", "Conservative", "Other")), 
   relig = factor(as.character(relig), 
     levels=c("Catholic", "Protestant", "Other", "Atheist"))) %>% 
  ggplot(aes(x=relig, y=vote, fill=stdres2)) + 
    geom_tile(col="white") + 
    theme_bw() + 
    scale_fill_brewer(palette="RdBu") + 
    labs(y="Vote", x="Religious Affiliation", 
         fill="Std. Resid.")






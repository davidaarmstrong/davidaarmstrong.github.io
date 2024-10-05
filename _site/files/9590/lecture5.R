###################################
## Code for POLSCI 9590 Week 4   ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

library(rio)
library(DAMisc)
library(uwo4419)
library(ggplot2)
library(dplyr)
library(tidyr)


ces <- import("ces19.dta")
ces$educ <- factorize(ces$educ)
sumStats(ces, "leader_lib")
sumStats(ces, "leader_lib", byvar="educ")


ces %>% filter(!is.na(educ)) %>% 
ggplot(aes(x=educ, y=leader_lib)) + 
  stat_summary(geom="point", fun=mean) + 
  theme_bw() + 
  labs(x="Highest Level of Educational Attainment", 
       y = "Liberal Leader Feeling Thermometer (0-100)") + 
  ylim(0,100) #<<
  





ces %>% filter(!is.na(educ)) %>% 
ggplot(aes(x=educ, y=leader_lib)) + 
  stat_summary(aes(shape="Mean", colour="Mean"), #<<
               geom="point", #<<
               fun=mean) + #<<
  stat_summary(aes(shape="Median", colour="Median"), #<<
               geom="point", #<<
               fun=median) + #<<
  theme_bw() + 
  theme(legend.position="top") + 
  labs(x="Highest Level of Educational Attainment", 
       y = "Liberal Leader Feeling Thermometer (0-100)", 
       colour="Measure", shape="Measure") + #<<
  ylim(0,100) 
  





ces %>% filter(!is.na(educ)) %>% 
ggplot(aes(x=educ, y=leader_lib)) + 
  stat_summary(aes(shape="Mean", colour="Mean"), 
               geom="point", 
               fun=mean, 
               position = position_nudge(x=-.1)) + #<<
  stat_summary(aes(shape="Median", colour="Median"), 
               geom="point",
               fun=median, 
               position = position_nudge(x=.1)) + #<<
  theme_bw() + 
  theme(legend.position="top") + 
  labs(x="Highest Level of Educational Attainment", 
       y = "Liberal Leader Feeling Thermometer (0-100)", 
       colour="Measure", shape="Measure") + 
  ylim(0,100) 








x <- tibble::tibble(
  country = c("A", "B", "C"), 
  `1999` = 1:3, 
  `2000` = 4:6)
xl <- pivot_longer(x, cols=`1999`:`2000`, 
             names_to="year", 
             values_to="cases")
x
xl





xw <- xl %>% pivot_wider(names_from="year", 
                   values_from="cases")
xl
xw


x <- ces %>% 
  filter(!is.na(educ)) %>% 
  group_by(educ) %>% 
  summarise(across(starts_with("leader"), 
                   list(Mean = ~mean(.x, na.rm=TRUE), 
                        Median = ~median(.x, na.rm=TRUE)))) %>% 
  pivot_longer(-educ, 
               names_pattern="leader_(.*)_(.*)", 
               names_to = c("party", "measure"), 
               values_to="val") %>% 
  mutate(party = factor(party, 
                        levels=c("ndp", "lib", "con"), 
                        labels=c("NDP", "Liberal", "Conservative")))

ggplot(x, aes(x=educ, y=val, 
              colour=party)) + 
  geom_point(position=position_dodge(width=.25)) + 
  theme_bw() + 
  facet_wrap(~measure, ncol=2) + 
  scale_colour_manual(values=c( "#F58220", "#d71920", "#003F72")) + 
  theme(legend.position="top") + 
  labs(x="Highest Level of Educational Attainment", 
       y = "Liberal Leader Feeling Thermometer (0-100)") + 
  ylim(0,100)





ggplot(x, aes(x=as.numeric(educ), y=val, 
              colour=party)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  facet_wrap(~measure, ncol=2) + 
  scale_colour_manual(values=c( "#F58220", "#d71920", "#003F72")) + 
  scale_x_continuous(breaks = 1:3, labels=c("<HS", "HS/college", "College\nGrad")) + 
  theme(legend.position="top", 
        panel.spacing=unit(1.5, "lines")) + 
  labs(x="Highest Level of Educational Attainment", 
       y = "Liberal Leader Feeling Thermometer (0-100)") + 
  ylim(0,100)






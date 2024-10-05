###################################
## Code for POLSCI 9590 Week 2   ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

library(ggplot2)
library(dplyr)

install.packages("remotes")
remotes::install_github("davidaarmstrong/damisc")
remotes::install_github("davidaarmstrong/uwo4419")


library(ggplot2)
library(dplyr)
library(rio)
library(scales)
library(uwo4419)
library(DAMisc)


ces19 <- import("ces19.dta")



freqDist(ces19$educ)
ces19$educ <- factorize(ces19$educ)
freqDist(ces19$educ)



sumStats(ces19, "leader_lib")


sumStats(ces19, "leader_lib", byvar="educ")



quantile(ces19$leader_lib, .62, na.rm=TRUE)


quantile(ces19$leader_lib, c(.38, .62), na.rm=TRUE)


crime <- import("crime.dta")
ggplot(crime, 
       aes(x=year, 
           y=incidents, 
           colour=prov)) +
  geom_line()





ggplot(crime, 
       aes(x=year, 
           y=incidents, 
           colour=prov)) +
  geom_line() + 
  theme_classic()#<<


ggplot(crime, 
       aes(x=year, 
           y=incidents, 
           colour=prov)) +
  geom_line() + 
  theme_classic() + 
  scale_color_brewer(palette="Paired") #<<


ggplot(crime, 
       aes(x=year, 
           y=incidents, 
           colour=prov)) +
  geom_line() + 
  theme_classic() + 
  scale_color_brewer(palette="Paired") + 
  labs(x="Year", y="Incidents of Crime", #<<
       colour="Province")#<<


ggplot(crime, 
       aes(x=year, 
           y=incidents, 
           colour=prov)) +
  geom_line() + 
  theme_classic() + 
  scale_colour_brewer(palette="Paired") + 
  labs(x="Year", y="Incidents of Crime", 
       colour="Province") + 
  scale_y_continuous( label = comma) #<<


ggplot(crime, 
       aes(x=year, 
           y=rate, #<<
           colour=prov)) +
  geom_line() + 
  theme_classic() + 
  scale_colour_brewer(palette="Paired") + 
  labs(x="Year", y="Incidents of Crime", 
       colour="Province") + 
  scale_y_continuous( label = comma) 


freqDist(ces19$educ)


ggplot(ces19, aes(x=educ)) + 
  geom_bar() 





ces19 %>% filter(!is.na(educ)) %>% #<<
ggplot(aes(x=educ)) + 
  geom_bar() 


ces19 %>% filter(!is.na(educ)) %>%
  ggplot(aes(x=educ, 
             y=after_stat(count/sum(count)))) + #<<
    geom_bar() + 
    scale_y_continuous(label=percent) #<<


ces19 %>% filter(!is.na(educ)) %>%
  ggplot(aes(x=educ, 
             y=after_stat(count/sum(count)))) + 
    geom_bar() + 
    scale_y_continuous(label=percent) +
    labs(x="Highest Level of Education", #<<
       y="Percentage") + #<<
    theme_classic() #<<



g <- ces19 %>% filter(!is.na(educ)) %>%
  ggplot(aes(x=educ, 
             y=..count../sum(..count..))) + 
    geom_bar() + 
    scale_y_continuous(label=
            label_percent(accuracy=2)) + 
  labs(x="Highest Level of Education", 
       y="Percentage") 


g + theme_minimal() + ggtitle("theme_minimal()")


g + theme_bw() + ggtitle("theme_bw()")


ggplot(ces19, aes(x=leader_lib)) + 
  geom_histogram() + 
  theme_classic() + 
  labs(
    x="Liberal Leader Feeling Thermometer", 
    y="# Observations")





ggplot(ces19, aes(x=leader_lib)) + 
  geom_histogram(bins=10) + #<<
  theme_classic() + 
  labs(
    x="Liberal Leader Feeling Thermometer", 
    y="# Observations")





ces19$gender <- factor(ces19$gender, 
                     levels=c(1,5), 
                     labels=c("Male","Female"))


ces19 %>% 
  filter(!is.na(educ)) %>%
  group_by(gender, educ) %>% 
  summarise(n = n()) %>% 
  ungroup %>% 
  group_by(gender) %>% 
  mutate(prop = n/sum(n)) %>%
  ggplot(aes(x=educ, 
             y=prop, 
             fill=gender)) + 
    geom_bar(position = position_dodge(), 
             stat="identity") + 
    scale_y_continuous(label=
            label_percent(accuracy=2)) + 
    theme_classic() + 
    labs(x="Highest Level of Education", 
         y="Percentage") 






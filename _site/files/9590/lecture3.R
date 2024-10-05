# ngrok http -subdomain=quantoid http://192.168.1.2:49010
options(htmltools.dir.version = FALSE)
knitr::opts_chunk$set(message = FALSE, warning = FALSE, dev="png", tidy=TRUE,  tidy.opts = list(only.comment=TRUE,  width.cutoff=80))
knitr::opts_chunk$set(fig.retina=2)
knitr::opts_hooks$set(fig.callout = function(options) {
  if (options$fig.callout) {
    options$echo <- FALSE
    options$out.height <- "99%"
    # options$fig.width <- 16
    # options$fig.height <- 8
  }
  options
})
library(tibble)
library(ggplot2)
library(formatR)
library(knitr)
library(pander)
library(xtable)
library(dplyr)
library(xaringanthemer)
style_mono_accent(base_color = "#4F2683", code_font_size=".65rem", text_font_size = "1.25rem")

xaringanExtra::use_scribble(pen_color="#3252a8", pen_size=5, 
    palette=c("#e41a1c", "#4daf4a", "#ff7f00", "#4F2683", "#3252a8") 
)
xaringanExtra::use_clipboard()
xaringanExtra::use_panelset()
xaringanExtra::use_tile_view()
# xaringanExtra::use_editable()


mytheme <- function(){
    theme_xaringan() +
    theme(axis.title=element_text(size=15), 
          axis.text=element_text(size=12),
          text=element_text(size=12), 
          title=element_text(size=15))
}


install.packages("remotes")
remotes::install_github("davidaarmstrong/damisc")
remotes::install_github("davidaarmstrong/uwo4419")


library(ggplot2)
library(dplyr)
library(rio)
library(scales)
library(uwo4419)
library(DAMisc)


exec(open('python_functions.py').read())


ces19 <- import("ces19.dta")


reticulate::use_python("/Users/david/.pyenv/shims/python")


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






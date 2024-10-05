###################################
## Code for POLSCI 9590 Week 8   ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

library(ggplot2)
library(dplyr)
library(rio)
library(GGally)
library(ggcorrplot)

ces <- import("ces19.dta")
cor(ces$leader_con, ces$leader_lib)
cor(ces$leader_con, ces$leader_lib, use="pairwise.complete")

therms = ces %>% select(starts_with("leader")) %>% na.omit()
DAMisc::pwCorrMat(~., 
                  data=therms, 
                  method="sim")

custom_smooth <- function(data, mapping, 
  ..., span=.35, pt.alpha=.25, jitter=TRUE) {
  if(jitter){
    pos <- position_jitter(width=2, height=2)
  }else{
    pos <- position_identity()
  }
  ggplot(data, mapping, ...) + 
    geom_point(shape=1, col="gray", 
               position=pos, alpha=pt.alpha) + 
    geom_smooth(method="loess", span=span,
                family="symmetric",
                se=FALSE, col="red") + 
    geom_smooth(method="lm", col="black", se=FALSE) 
}
ggpairs(therms,
  lower = list(continuous = wrap(custom_smooth, 
               span=.5, 
               pt.alpha=.15,
               jitter=TRUE))) + 
theme(legend.position = "bottom")  

r <- cor(therms)
colnames(r) <- rownames(r) <- c("Liberal", "NDP", 
                                "Conservative")
ggcorrplot(r, 
           ggtheme = theme_classic, 
           lab=TRUE, 
           type="upper", 
           show.diag=FALSE) + 
  theme(legend.position = "inside", 
        legend.position.inside=c(.75, .25), 
        legend.background=element_rect(fill="transparent"), 
        legend.title = element_blank()) + 
  ggtitle("Correlation Matrix of Leader Feeling Thermometers")






###################################
## Code for POLSCI 9590 Week 1   ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

library(ggplot2)
library(dplyr)

library(rio)
ces <- import("ces19.dta")

str(ces$agegrp)
str(ces$leader_lib)

data("mtcars", package="datasets")



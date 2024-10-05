
library(ggplot2)
library(dplyr)

library(rio)
ces <- import("ces19.dta")

str(ces$agegrp)
str(ces$leader_lib)

data("mtcars", package="datasets")



###################################
## Code for POLSCI 9590 Week 3   ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

library(ggplot2)
library(dplyr)
library(rio)
library(scales)

marbles <- function(draws, 
                    colors=c("red" = 10, "blue" = 10, "yellow" = 10, "green" = 10), 
                    replace=FALSE){
  # draws should be a character vector of required draws
  # colors defines the contents of the marble bag
  # replace indicates whether a drawn marble should be replaced in the bag
  ## make the bag 
  inbag <- rep(names(colors), colors)
  ## initialize the draws
  out_draw <- NULL
  ## loop over the values in draws
  for(i in 1:length(draws)){
    ## sample 1 observation from the bag
    tmp_s <- sample(inbag, 1)
    ## record the sampled item in the output object
    out_draw <- c(out_draw, tmp_s)
    if(!replace){
      ## if no replacement, then remove the drawn value
      ## from the bag
      w <- min(which(inbag == tmp_s))
      inbag <- inbag[-w]
    }
  }
  ## return a vector indicating whether the 
  ## draw matched all the conditions
  all(out_draw == draws)
}


reps1 <- replicate(250000, 
                   marbles(c("red", "green"), 
                      replace=TRUE)
                  )
reps2 <- replicate(250000, 
                   marbles(c("red", "green"), 
                           replace=FALSE)
                   )
tmp <- tibble(
  reps = c(reps1, reps2), 
  it = rep(1:250000, 2), 
  replace=rep(c("Yes", "No"), each=250000))
tmp <- tmp %>%
  group_by(replace) %>% 
  mutate(cm = cummean(reps)) %>% 
  filter(it %% 10 == 0)


ggplot(tmp, aes(x=it, 
                y=cm, 
                colour=replace))+ 
  geom_line() + 
  theme_bw() + 
  labs(x="Iteration", 
       y="Cumulative Probability") + 
  theme(legend.position="top")

# case_when(x, 
#   condition1 ~ value1, 
#   condition2 ~ value2, 
#   TRUE ~ value3)


dat = data.frame(x = c(1,2,3,4))
dat <- dat%>% 
  mutate(y = case_when(x <=3 ~ "yes", 
                       TRUE ~ "no"))
dat


dat <- dat %>% 
  mutate(y = case_when(x %in% c(1,2,3) ~ "yes", 
                       TRUE ~ "no"))
dat


dat <- data.frame(x = c(1,2,3,4, NA))
dat <- dat %>% 
  mutate(y = case_when(x <=3 ~ "yes", 
               TRUE ~ "no"))
dat


dat <- dat %>% 
  mutate(y = case_when(x <=3 ~ "yes", 
               is.na(x) ~ NA_character_, 
               TRUE ~ "no"))
dat


dat <- data.frame(x=1:4)
dat <- dat %>% 
  mutate(y = case_when(x <= 2 ~ "l", 
                       x >= 2 ~ "g"), 
         z = case_when(x >= 2 ~ "g",
               x <= 2 ~ "l"))
dat


ces <- import("ces19.dta")
q40 <- quantile(ces$market, .4, na.rm=TRUE)
ces <- ces %>% 
  mutate(market_01 = case_when(
    market < q40 ~ 0, 
    is.na(market) ~ NA_real_, 
    TRUE ~ 1
  ))
table(ces$market_01, useNA="ifany")


ces <- ces %>% 
  mutate(heart_ndp = case_when(
    leader_lib < leader_ndp & leader_con < leader_ndp ~ "yes", 
    is.na(leader_lib) | is.na(leader_con) | is.na(leader_ndp) ~ NA_character_, 
    TRUE ~ "no"))
table(ces$heart_ndp, useNA="ifany")


ces %>% 
  mutate(vote = factorize(vote)) %>% 
  group_by(vote, heart_ndp) %>% 
  summarise(n = n()) %>% 
  na.omit() %>% 
  group_by(heart_ndp) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x=vote, y=pct, fill=heart_ndp)) + 
    geom_bar(stat="identity", 
             position=position_dodge()) + 
    theme_classic() + 
  scale_y_continuous(label=percent) + 
  labs(x="Vote in 2019 Federal Election", 
       y="Percentage Voting For", 
       fill="Like NDP\nLeader Most")


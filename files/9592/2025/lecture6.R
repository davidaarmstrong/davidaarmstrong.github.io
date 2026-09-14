library(rio)
library(dplyr)
library(marginaleffects)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rio)
library(DAMisc)
library(mlogit)
library(brglm2)
library(nnet)
library(factorplot)
library(marginaleffects)
library(RColorBrewer)
library(formatR)
library(knitr)
library(xaringan)
library(xaringanthemer)
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


## ----france, echo=T----------------------------------------------------------------------------
dat <- import("data/france_mnl.dta")
dat <- factorize(dat)
labs <- sapply(1:ncol(dat), function(i)attr(dat[[i]], "label"))
data.frame(var = names(dat), description = labs)


## ----mod1, echo=T------------------------------------------------------------------------------
mod <- multinom(vote ~ lrself + male + retnat + age +
    union , data=dat, trace=F)
noquote(t(mnlSig(mod)))

dat$vote2 <- relevel(dat$vote, "PS")
mod2 <- multinom(vote2 ~ lrself + male + retnat + age +
                  union , data=dat, trace=F)

## ----fp, echo=T, eval=FALSE, fig.height=8, fig.width=8, out.height="100%", fig.align="center"----
plot(factorplot(mod, variable="lrself"))

## ----fp2, echo=T, eval=FALSE, fig.height=8, fig.width=8, out.height="100%", fig.align="center"----
plot(factorplot(mod,
  variable="retnatworse"))

## ----fp2b, echo=T, eval=FALSE, fig.height=8, fig.width=8, out.height="100%", fig.align="center"----
b <- c(t(coef(mod)))
v <- vcov(mod)
rn_inds <- grep("retnat", rownames(v))
b_rn <- b[rn_inds]
v_rn <- v[rn_inds, rn_inds]
names(b_rn) <- rownames(v_rn)
plot(factorplot(b_rn, var=v_rn, resdf=Inf),
     print.est=FALSE, print.se=FALSE,
     print.square.leg=FALSE, abbrev.char = 25)


## ----eff1, echo=T, eval=FALSE, fig.height=9, fig.width=6, out.width="100%", fig.align="center"----
seq_range <- function(x, n=25, ...){x <- na.omit(x); seq(min(x), max(x), length=n)}
pred_lrs <- predictions(mod,
            newdata="median",
            variables=list(lrself = seq_range(dat$lrself)))

ggplot(pred_lrs,
       aes(x=lrself,
           y=estimate,
           ymin = conf.low,
           ymax=conf.high)) +
  geom_ribbon(alpha=.2, colour="transparent") +
  geom_line() +
  facet_wrap(~group, ncol=2) +
  theme_bw() +
  theme(panel.grid=element_blank())


## ----eff1a, echo=T, eval=FALSE, fig.height=9, fig.width=6, out.width="100%", fig.align="center"----
ave_pred_lrs <- avg_predictions(mod,
            variables=list(lrself = seq_range(dat$lrself)))
ggplot(ave_pred_lrs,
       aes(x=lrself,
           y=estimate,
           ymin = conf.low,
           ymax=conf.high)) +
  geom_ribbon(alpha=.2, colour="transparent") +
  geom_line() +
  facet_wrap(~group, ncol=2) +
  theme_bw() +
  theme(panel.grid=element_blank())

## ----compeff, echo=TRUE, eval=FALSE, fig.height=9, fig.width=6, out.width="100%", fig.align="center"----
all_lrs <- bind_rows(pred_lrs %>% mutate(type = "MER"),
          ave_pred_lrs %>% mutate(type = "AME"))
ggplot(all_lrs,
       aes(x=lrself,
           y=estimate,
           ymin = conf.low,
           ymax=conf.high,
           colour=type,
           fill=type)) +
  geom_ribbon(alpha=.2, colour="transparent") +
  geom_line() +
  facet_wrap(~group) +
  theme_bw() +
  theme(panel.grid=element_blank())

## ----d1, echo=T--------------------------------------------------------------------------------
comps <- comparisons(mod, newdata="median", 
            variables = list(lrself = "2sd", 
                             male = c(0,1), 
                             retnat = "minmax", 
                             age = "2sd", 
                             union = "reference"))

comps %>% 
  mutate(eff = sprintf("%.2f%s", 
                       estimate, 
                       ifelse(p.value < .05, "*", ""))) %>% 
  select(2:4, eff) %>% 
  pivot_wider(names_from="group", values_from="eff")




## ----d1a, echo=T-------------------------------------------------------------------------------
ave_comps <- avg_comparisons(mod, 
      variables = list(lrself = "2sd", 
       male = c(0,1), 
       retnat = "minmax", 
       age = "2sd", 
       union = "reference"))
ave_comps %>% 
  mutate(eff = sprintf("%.2f%s", 
                       estimate, 
                       ifelse(p.value < .05, "*", ""))) %>% 
  select(1:3, eff) %>% 
  pivot_wider(names_from="group", values_from="eff")



## ----fit, echo=T-------------------------------------------------------------------------------
mnlfit(mod)


## ----pre, echo=T-------------------------------------------------------------------------------
pre(mod)


## ----prgraph3, echo=T, results='hide', fig.height=5, fig.width=15, out.width="\\textwidth"-----
probgroup(mod)


## ----------------------------------------------------------------------------------------------
y <- model.response(model.frame(mod))
yhat <- predict(mod)
table(y, yhat)


## ----addvars, echo=T, include=T----------------------------------------------------------------
names(dat)[13:18]
names(dat)[13:18] <- paste("rp", levels(dat$vote), sep="_")
dat2 <- dat %>% 
  dplyr::select(c("vote", "lrself", "urban", "union", "retnat", 
           "age", "male", starts_with("rp"))) %>% 
  na.omit()
library(fastDummies)
library(dfidx)
dat2 <- dat2 %>% 
  dummy_cols("vote", remove_selected_columns=TRUE) 
# don't need this if change sep="_" above
# names(dat2) <- gsub("vote_", "vote.", names(dat2))
dat2 <- dat2 %>% 
  mutate(obs = row_number()) %>% 
  pivot_longer(7:18, 
               names_pattern = "(.*)\\_(.*)", 
               names_to = c(".value", "alt"))
dat2l <- dfidx(dat2, idx = c("obs", "alt"), choice = "vote")


## ----cl, echo=T, include=T---------------------------------------------------------------------
mlogit2 <- mlogit(vote ~ 0 + I(abs(lrself - rp)) |  urban + union + retnat + 
    age + male, data=dat2l, reflevel="PS")
printCoefmat(summary(mlogit2)$CoefTable)


## ----makeFake, echo=F, include=F---------------------------------------------------------------
makeFakeData <- function(obj, data, change, varying = NULL){
    if(inherits(data, "dfidx")){
      idvar <- names(index(data))[1]
      altvar <- names(index(data))[2]
      data <- data %>% unnest(idx)      
    }else{
      stop("Data should inherit class dfidx\n")
    }
    tmpdata <- data[which(data[[idvar]] == 1), ]
    y <- model.response(model.frame(obj))
    vars <- all.vars(formula(obj))[-1]
    vars <- vars[-which(vars %in% names(change))]
    if(!is.null(varying)){
        vars <- vars[-which(vars %in% varying)]
    }
    if (any(!(vars %in% names(data)))) {
        vars <- vars[-which(!vars %in% names(data))]
    }
    var.classes <- sapply(vars, function(x) 
      case_when(inherits(data[[x]], "factor") ~ "factor", 
                inherits(data[[x]], "numeric") ~ "numeric", 
                TRUE ~ NA_character_))
    meds <- lapply(vars, function(x) NA)
    names(meds) <- vars
    levs <- list()
    for(i in 1:length(var.classes)){
        if(var.classes[i] == "factor"){
            levs[[vars[i]]] <- levels(data[[vars[i]]])
        }
    }
    if (length(levs) > 0) {
        for (i in 1:length(levs)) {
            tmp.tab <- table(data[[names(levs)[i]]])
            tmpdata[[names(levs)[i]]] <- factor(names(tmp.tab)[which.max(tmp.tab)], 
                levels = levs[[i]])
            vars <- vars[-which(vars == names(levs)[i])]
        }
    }
    for (i in 1:length(vars)) {
        tmpdata[[vars[i]]] <- median(data[[vars[i]]], na.rm = T)
    }
    alt <- data[[altvar]]
    if(!is.null(varying)){
        for(i in 1:length(varying)){
            ag <- aggregate(data[[varying[i]]], list(alt), mean)
            tmpdata[[varying[i]]] <- ag[match(tmpdata[[altvar]], ag[,1]), 2]
        }
    }
    tmpdata2 <- data[which(data[[idvar]] %in% 1:length(change[[1]])), ]
    ch <- tmpdata2[[idvar]]
    for(i in 1:length(change[[1]])){
        tmpdata2[which(tmpdata2[[idvar]] == i), ] <- tmpdata
    }
    tmpdata2[[idvar]] <- ch
    for(i in 1:length(change[[1]])){
        tmpdata2[which(tmpdata2[[idvar]] == i), names(change)[1]] <- change[[1]][i]
    }
    attr(tmpdata2, "index") <- attr(tmpdata2, "index")[1:nrow(tmpdata2), ]
    tmpdata2
}




## ----p1, echo=T, eval=FALSE, fig.height=9, fig.width=6, out.width="100%", fig.align="center"----
lrs <- 0:10
fake <- makeFakeData(mlogit2,
                     dat2l,
                     change=list(lrself=lrs),
                     varying="rp")
probs <- predict(mlogit2, newdata=fake)
plot.dat <- data.frame(
  prob = c(t(probs)),
  party = factor(rep(colnames(probs),
                     length(lrs)),
                 levels=levels(dat$vote)),
  lrself = rep(lrs, each=6))
ggplot(plot.dat, aes(x=lrself, y=prob)) +
  geom_line() +
  facet_wrap(~party, ncol=2) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  labs(x="Left-right Self-placement",
       y="Predict Pr(Vote)")

## ----ci2---------------------------------------------------------------------------------------
B <- MASS::mvrnorm(2500, coef(mlogit2), vcov(mlogit2))
tmp <- mlogit2
probs <- NULL
for(i in 1:2500){
  tmp$coefficients <- B[i,]
  probs <- rbind(probs, 
   cbind(data.frame(lrself = unique(fake$lrself), 
                    sim=i), 
         predict(tmp, 
                 newdata=fake)))
}
plot.dat <- probs %>% 
  pivot_longer(PS:`UMP*`, 
               names_to="party", 
               values_to="prob") %>% 
  mutate(party = factor(party, 
                        levels=levels(dat$vote))) %>% 
  group_by(lrself, party) %>% 
  summarise(p = mean(prob), 
            lwr= quantile(prob, .025), 
            upr = quantile(prob, .975))


## ----p1x, echo=T, eval=FALSE, fig.height=9, fig.width=6, out.width="100%", fig.align="center"----
ggplot(plot.dat, aes(x=lrself,
                 y=p,
                 ymin = lwr,
                 ymax=upr)) +
  geom_ribbon(alpha=.25,
              colour="transparent") +
  geom_line() +
  facet_wrap(~party, ncol=2) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  labs(x="Left-right Self-placement",
       y="Predict Pr(Vote)")

## ----------------------------------------------------------------------------------------------
B <- MASS::mvrnorm(2500, coef(mlogit2), vcov(mlogit2))
lrs <- 0:10
out <- data.frame(
  lrself = rep(lrs, each=6),
  party = 1:6, 
  p = NA, 
  lwr = NA, 
  upr = NA
)


## ----------------------------------------------------------------------------------------------
for(j in lrs){
  tmp <- dat2l
  tmp$lrself <- j
  X <- model.matrix(
    update(mlogit2, data=tmp))
  
  b <- coef(mlogit2)
  XB <- X %*% t(B)
  Xb <- X %*% b
  p <- prop.table(matrix(exp(Xb), ncol=6, byrow=TRUE), 1)
  m <- colMeans(p)
  
  res <- NULL
  for(i in 1:ncol(XB)){
    exb <- exp(matrix(XB[,i], ncol=6, byrow=TRUE))
    p <- prop.table(exb, 1)
    res <- rbind(res, colMeans(p))
  }
  
  l <- apply(res, 2, quantile, .025)
  u <- apply(res, 2, quantile, .975)
  out$p[which(out$lrself == j)] <- m
  out$lwr[which(out$lrself == j)] <- l
  out$upr[which(out$lrself == j)] <- u
}
out$party <- factor(out$party, labels=c("FN", "Green", "PCF", "PS", "UDF", "UMP"))


## ----ame_fig, echo=TRUE, eval=FALSE, fig.height=6, fig.width=9, out.height="100%", fig.align="center"----
ggplot(out, aes(x=lrself, y=p,
                ymin = lwr, ymax=upr)) +
  geom_ribbon(alpha=.25, colour="transparent") +
  geom_line() +
  facet_wrap(~party) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  labs(x="Left-right Self-placement",
       y="Predicted Pr(Vote)")


## ----------------------------------------------------------------------------------------------
fake <- makeFakeData(mlogit2, 
                     dat2l, 
                     change=list(lrself=c(0,10)), 
                     varying = "rp")
B <- MASS::mvrnorm(2500, coef(mlogit2), vcov(mlogit2))

tmp <- mlogit2
probs <- NULL
for(i in 1:2500){
  tmp$coefficients <- B[i,]
  probs <- rbind(probs, 
   cbind(data.frame(lrself = unique(fake$lrself), 
                    sim=i), 
         predict(tmp, 
                 newdata=fake)))
}

fd <- probs %>% 
  group_by(sim) %>% 
  summarise(across(PS:`UMP*`, ~diff(.x))) %>% 
  ungroup %>% 
  summarise(across(-sim, list(mean = ~mean(.x), 
                              lwr = ~quantile(.x, .025), 
                              upr = ~quantile(.x, .975), 
                              pval = ~mean(.x < 0)))) %>% 
  mutate(across(contains("pval"), ~ifelse(.x > .5, 1-.x, .x))) %>% 
  pivot_longer(everything(),
               names_pattern="(.*)_(.*)", 
               names_to=c("party", ".value"))


## ----------------------------------------------------------------------------------------------
fd %>% 
  mutate(across(-party, 
                ~round(.x, 2)))


## ----------------------------------------------------------------------------------------------
fake <- makeFakeData(mlogit2, 
                     dat2l, 
                     change=list(lrself=c(0,10)), 
                     varying="rp")


fake1 <- fake0 <- dat2l
fake0$lrself <- 0
fake1$lrself <- 10

B <- MASS::mvrnorm(100, coef(mlogit2), vcov(mlogit2))
tmp <- mlogit2
res <- NULL
for(i in 1:100){
  tmp$coefficients <- B[i,] 
  p_hat0 <- colMeans(predict(tmp, newdata=fake0))
  p_hat1 <- colMeans(predict(tmp, newdata=fake1))
  res <- rbind(res, p_hat1-p_hat0)
}

fd_ame <- as.data.frame(res) %>% 
  summarise(across(everything(), 
                   list(p = ~mean(.x), 
                        lwr = ~quantile(.x, .025), 
                        upr = ~quantile(.x, .975), 
                        pval = ~mean(.x > 0)))) %>% 
  mutate(across(contains("pval"), 
                ~ifelse(.x > 0, 1-.x, .x))) %>% 
  pivot_longer(everything(), 
               names_pattern="(.*)_(.*)", 
               names_to=c("party", ".value"))



## ----------------------------------------------------------------------------------------------
fd_ame %>%
  mutate(across(-party, 
                ~round(.x, 2)))


## ----------------------------------------------------------------------------------------------
yhat <- predict(mlogit2,newdata=dat2l)
yhat <- colnames(yhat)[apply(yhat, 1, which.max)]
yhat <- factor(yhat, levels=levels(dat$vote))
obs_vote <- dat2l %>%  filter(vote == 1) %>% ungroup %>% unnest(idx) %>% select(alt) %>% pull()
tab <- table(obs_vote, yhat)
pcp <- sum(obs_vote == yhat)/sum(tab)
pmc <- max(table(dat$vote))/sum(tab)
(pcp-pmc)/(1-pmc)


## ----------------------------------------------------------------------------------------------
ll1 <- logLik(mod)
ll2 <- logLik(mlogit2)
x2 <- -2*(ll1-ll2)
pchisq(x2, 9, lower.tail=FALSE)


## ----------------------------------------------------------------------------------------------
mods <- multinom(vote ~ lrself + male + retnat + age +
    union + demsat, data=dat, trace=F)
mnlSig(mods)



## ----eval=FALSE--------------------------------------------------------------------------------
## mods_br <- brmultinom(formula(mods), data=dat)

## ----eval=TRUE, echo=FALSE, results='hide'-----------------------------------------------------
load("data/mods_br.rda")

## ----------------------------------------------------------------------------------------------
noquote(t(mnlSig(mods_br)))


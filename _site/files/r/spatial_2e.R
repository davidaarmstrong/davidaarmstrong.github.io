############################################################
## Analyzing Spatial Models of Choice and Judgment with R ##
## 2nd edition, CRC Press, 2021                           ##
## Dave Armstrong, Ryan Bakker, Royce Carroll,            ##   
##     Chris Hare, Keith Poole and Howard Rosenthal       ##
##                                                        ##     
## Code for all figures and output                        ##
## Questions/Bugs: darmst46@uwo.ca                        ##
############################################################

###################################################
### code chunk number 1: opts
###################################################
options(width=60,
	str=strOptions(strict.width="wrap"))
library(anominate) 
library(apsrtable) 
library(asmcjr)    
library(basicspace)
library(effects)   
library(ellipse)   
library(ggplot2)   
library(gridExtra) 
library(MASS)      
library(MCMCpack)  
library(oc)        
library(ooc)       
library(perm)      
library(pscl)      
library(rgenoud)   
library(runjags)   
library(smacof)    
library(wnominate) 
library(xtable)    
library(ggeffects)
library(dplyr)
library(stringr)


###################################################
### code chunk number 2: plotthreeutilityfunctions1
###################################################
norm.util <- function(x){ return(15*exp(-0.25*(0-x)^2))}
quad.util <- function(x){ return(15+15*(-0.25*(0-x)^2))}
lin.util <- function(x){ return(5*-abs(x)+15)}
plot(c(-3,3),c(0,20),type="n",bty="n",xlab="Policy Location",
     ylab="Utility",cex.lab=1.2)
lines(seq(-3,3,0.01), norm.util(seq(-3,3,0.01)),lwd=3,lty=1)
lines(seq(-3,3,0.01), quad.util(seq(-3,3,0.01)),lwd=3,lty=2)
lines(seq(-3,3,0.01), lin.util(seq(-3,3,0.01)),lwd=3,lty=3)


###################################################
### code chunk number 3: chapter2.Rnw:136-137
###################################################
options(prompt=" ",continue=" ")


###################################################
### code chunk number 4: aldmck1
###################################################
library(asmcjr)
library(ggplot2)
data(franceEES2009)
head(franceEES2009)


###################################################
### code chunk number 5: aldmck2
###################################################
#Loading the 'basicspace' package
library(basicspace)
#Running Bayesian Aldrich-Mckelvey scaling on France EES
result.france <- aldmck(franceEES2009, respondent=1, polarity=2,
    missing=c(77,88,89), verbose=FALSE)


###################################################
### code chunk number 6: aldmck3
###################################################
#Output of the results of Aldrich-Mckelvey scaling
summary(result.france)


###################################################
### code chunk number 7: AMfrance_votersonly
###################################################
# plot density of ideal points
plot_resphist(result.france, xlab="Left-Right")


###################################################
### code chunk number 8: AMfrance
###################################################
# plot stimuli locations in addition to ideal point density
plot_resphist(result.france, addStim=TRUE, xlab = "Left-Right") +
  theme(legend.position="bottom", aspect.ratio=1) +
  guides(shape = guide_legend(override.aes = list(size = 4),
                              nrow=3)) +
  labs(shape="Party", colour="Party")


###################################################
### code chunk number 9: AMfrance_positive
###################################################
# Isolate positive weights
plot_resphist(result.france, addStim=TRUE, weights="positive",
    xlab = "Left-Right")  +
  theme(legend.position="bottom", aspect.ratio=1) +
  guides(shape = guide_legend(override.aes = list(size = 4),
                              nrow=3)) +
  labs(shape="Party", colour="Party")


###################################################
### code chunk number 10: AMfrance_negative
###################################################
# Isolating negative weights
plot_resphist(result.france, addStim=TRUE, weights="negative",
    xlab = "Left-Right")  +
  theme(legend.position="bottom", aspect.ratio=1) +
  guides(shape = guide_legend(override.aes = list(size = 4),
                              nrow=3)) +
  labs(shape="Party", colour="Party")


###################################################
### code chunk number 11: aldmckNES1
###################################################
# Loading 'urbanunrest' data
data(nes1968_urbanunrest)
# Creating object with US president left-right dimensions
urban <- as.matrix(nes1968_urbanunrest[,-1])
#Running Bayesian Aldrich-Mckelvey scaling on President positions
result.urb <- aldmck(urban, polarity=2, respondent=5,
    missing=c(8,9), verbose=FALSE)
summary(result.urb)


###################################################
### code chunk number 12: AMurbanunrest_voters
###################################################
# Extracting vote.choice column
# recode so that only Humphrey, Nixon and Wallace are present
vote <- car:::recode(nes1968_urbanunrest[,1],
    "3='Humphrey'; 5 = 'Nixon'; 6 = 'Wallace'; else=NA",
    as.factor=FALSE)
# Convert vote to factor with appropriate levels
vote <- factor(vote, levels=c("Humphrey", "Nixon", "Wallace"))
# Plot population distribution by vote choice
plot_resphist(result.urb, groupVar=vote, addStim=TRUE,
  xlab="Liberal-Conservative")  +
  theme(legend.position="bottom", aspect.ratio=1)  +
  guides(shape = guide_legend(override.aes =
      list(size = 4, color=c("gray25", "gray50", "gray75"))),
    colour = "none") +
  xlim(c(-2,2)) +
  labs(shape="Candidate")


###################################################
### code chunk number 13: aldmckNES2
###################################################
data(nes1968_vietnam)
vietnam <- as.matrix(nes1968_vietnam[,-1])
#Aldrich-Mckelvey function for vietnam dataset
result.viet <- aldmck(vietnam, polarity=2, respondent=5,
    missing=c(8,9), verbose=FALSE)
summary(result.viet)


###################################################
### code chunk number 14: AMVietnam
###################################################
library(ggplot2)
plot_resphist(result.viet, addStim=TRUE,
          xlab="Liberal-Conservative") +
  theme(legend.position="bottom", aspect.ratio=1) +
  guides(shape = guide_legend(override.aes = list(size = 4))) +
  labs(colour="Politician", shape="Politician") +
  xlim(-1.5, 1.5)


###################################################
### code chunk number 15: aldmck_boot
###################################################
boot.france  <- boot.aldmck(franceEES2009,
        polarity=2, respondent=1, missing=c(77,88,89),
        verbose=FALSE, boot.args = list(R=100))


###################################################
### code chunk number 16: AMfrance_bootstrap
###################################################
library(ggplot2)
plot(boot.france$sumstats) +
  ylab(NULL) +
  xlab("Left-Right")


###################################################
### code chunk number 17: BSconvention1
###################################################
library(basicspace)
data(CDS2000)
head(CDS2000[,5:8])


###################################################
### code chunk number 18: BSconvention2
###################################################
issues <- as.matrix(CDS2000[,5:14])
#Blackbox syntax of Republican-Democrat left-right scale
result.repdem <- blackbox(issues,
            missing=99, dims=3, minscale=5, verbose=TRUE)


###################################################
### code chunk number 19: BSconvention3
###################################################
result.repdem$fits


###################################################
### code chunk number 20: BSconvention4
###################################################
result.repdem$stimuli


###################################################
### code chunk number 21: BSdelegates
###################################################
# Party: Democrats = 1; Republicans = 2
party <- car:::recode(CDS2000[,1],
    "1='Democrat'; 2='Republican'; else=NA",
    as.factor=TRUE)
# Make the plot
plot_blackbox(result.repdem, dims=c(1,2), groupVar=party,
                xlab= "First Dimension\n(Left-Right)", 
                ylab="Second Dimension") +
  theme(legend.position="bottom", aspect.ratio=1) +
  guides(shape=guide_legend(override.aes=list(size=4))) + 
  labs(colour="Party")


###################################################
### code chunk number 22: BSdelegatesnormvec1
###################################################
plot_blackbox(result.repdem, dims=c(1,2), groupVar=party,
    issueVector=c(2,10), data=issues,
    nudgeX= c(0,.125), nudgeY=c(-.05,0)) +
  theme(legend.position="bottom", aspect.ratio=1) +
  labs(colour="Party") +
  guides(shape = guide_legend(override.aes = list(size = 4)))


###################################################
### code chunk number 23: BSdelegates_histogram
###################################################
#Weight Rep/Dem data by number of delegates
plot_resphist(result.repdem, groupVar=party,
  addStim=FALSE, xlab = "Liberal-Conservative", dim=1, whichRes=2) +
  theme(legend.position="bottom", aspect.ratio=1) +
  labs(colour="Party") +
  guides(colour = guide_legend(override.aes = list(size = 2)))



###################################################
### code chunk number 24: BS_Sweden
###################################################
library(basicspace)
data(Sweden2010)
head(Sweden2010[,1:8])
#Extract issues scales and convert to numeric
issues.sweden <- as.matrix(Sweden2010[,7:56])
mode(issues.sweden) <- "numeric"
#Blacbox syntax for Sweden issue scale
result.sweden <- blackbox(issues.sweden, missing=8,
                          dims=3, minscale=5, verbose=FALSE)
# change polarity of scores
if(result.sweden$individuals[[1]][13,1] < 0)
result.sweden$individuals[[1]][,1] <-
    result.sweden$individuals[[1]][,1] * -1
result.sweden$fits


###################################################
### code chunk number 25: BS_Sweden2
###################################################
result.sweden$stimuli[[1]][16:25,]


###################################################
### code chunk number 26: BSsweden_histogram
###################################################
elected <- as.numeric(Sweden2010[,2])
party.name.sweden <- as.factor(Sweden2010[,3])
plot_resphist(result.sweden, groupVar=party.name.sweden, dim=1,
  scaleDensity=FALSE) +
  facet_wrap(~stimulus, ncol=2) +
  theme(legend.position="none") +
  scale_color_manual(values=rep("black", 10))


###################################################
### code chunk number 27: BSsweden_histogram2
###################################################
#Density plot syntax and comparison of defeated/elected candidates
# Keep only the parties of elected candidates, set others to NA
party.name.sweden[which(elected == 0)] <- NA
plot_resphist(result.sweden, groupVar=party.name.sweden, dim=1,
   scaleDensity=FALSE) + facet_wrap(~stimulus, ncol=2) +
  theme(legend.position="none") +
  scale_color_manual(values=rep("black", 10))


###################################################
### code chunk number 28: BS_Sweden_bootstrap (eval = FALSE)
###################################################
## #Candidate point estimates blackbox syntax
## outbb <- boot.blackbox(issues.sweden, missing=8, dims=3, minscale=5,
##     verbose=FALSE, posStimulus=13)


###################################################
### code chunk number 29: chapter2.Rnw:737-738
###################################################
load("bs_sweden_bootstrap.rda")


###################################################
### code chunk number 30: BS_Sweden_bootstrap_2
###################################################
#Matrix creation for Swedish candidates
first.dim <- data.frame(
  point = result.sweden$individuals[[3]][,1],
  se = apply(outbb[,1,], 1, sd)
)
first.dim$lower <- with(first.dim, point - 1.96*se)
first.dim$upper <- with(first.dim, point + 1.96*se)
first.dim$elected <- factor(elected, levels=c(0,1),
    labels=c("Not Elected", "Elected"))
head(first.dim)


###################################################
### code chunk number 31: BSsweden_bootstrap_histogram
###################################################
#Plot for the distribution of first dimensions bootstrapped SE
ggplot(first.dim, aes(x=se, group=elected)) +
  stat_density(geom="line", bw=.005) +
  facet_wrap(~elected) +
  theme(aspect.ratio=1) +
  xlab("Standard Error") +
  xlim(c(0,.2)) +
  theme_bw()


###################################################
### code chunk number 32: BS_Sweden_bootstrap2 (eval = FALSE)
###################################################
## #Variance test syntax
## library(perm)
## levels(first.dim$elected) <- c("No", "Yes")
## permTS(se ~ elected, data=first.dim,
##        alternative="greater", method="exact.mc",
##        control=permControl(nmc=10^4-1))


###################################################
### code chunk number 33: BST_Mexico
###################################################
library(basicspace)
data(mexicoCSES2000)
data(mexicoCSES2006)
head(mexicoCSES2000)


###################################################
### code chunk number 34: BST_Mexico_2 (eval = FALSE)
###################################################
## #Blackbox syntax for two datasets, with data cleaning arguments
## result_2000 <- blackbox_transpose(mexicoCSES2000, missing=99,
##     dims=3, minscale=5, verbose=TRUE)
## result_2006 <- blackbox_transpose(mexicoCSES2006, missing=99,
##     dims=3, minscale=5, verbose=TRUE)


###################################################
### code chunk number 35: chapter2.Rnw:853-854
###################################################
load("bst_mexico_2.rda")


###################################################
### code chunk number 36: BST_Mexico_3
###################################################
print(result_2000$stimuli[[2]])


###################################################
### code chunk number 37: BST_Mexicoflip
###################################################
#Multiplying here to avoid negative scores
first.dim.2000 <- -1 * result_2000$stimuli[[2]][,2]
second.dim.2000 <-  result_2000$stimuli[[2]][,3]
first.dim.2006 <- -1 * result_2006$stimuli[[2]][,2]
second.dim.2006 <- -1*result_2006$stimuli[[2]][,3]
plot.df <- data.frame(
  dim1 = c(first.dim.2000, first.dim.2006),
  dim2 = c(second.dim.2000, second.dim.2006),
  year = rep(c(2000, 2006), c(length(first.dim.2000),
                              length(first.dim.2006))),
  party = factor(c(rownames(result_2000$stimuli[[2]]),
                   rownames(result_2006$stimuli[[2]]))))
plot.df$nudge_x <- c(0, 0,  0, 0, 0, -.125,
                     0, 0, 0, 0.13,   0, 0, -.225, 0)
plot.df$nudge_y <- c(-.05, -.05,.05, -.05, -.05,    0,
                     .05, .05, .05, -.03, -.05, .05,  .025, -.05)
head(plot.df)



###################################################
### code chunk number 38: BST_Mexico2000plot
###################################################
#Year 2000 plot syntax
ggplot(plot.df, aes(x=dim1, y=dim2, group=year)) +
  geom_point() +
  geom_text(aes(label=party), nudge_y=plot.df$nudge_y, size=3,
            nudge_x=plot.df$nudge_x, group=plot.df$year) +
  facet_wrap(~year) +
  xlim(-.55,1) +
  ylim(-.55,1) +
  theme_bw() +
  labs(x="First Dimension", y="Second Dimension")


###################################################
### code chunk number 39: BST_BSE_France (eval = FALSE)
###################################################
## rankings <- as.matrix(franceEES2009[,2:9])
## mode(rankings) <- "numeric"
## original <- blackbox_transpose(rankings,
##                     missing=c(77,88,89),
##     dims=3, minscale=5, verbose=FALSE)
## #Reverse check
## if(original$stimuli[[1]][1,2] > 0)
##     original$stimuli[[1]][,2] <- -1 *
##   original$stimuli[[1]][,2]
## original$fits


###################################################
### code chunk number 40: BST_BSE_France2 (eval = FALSE)
###################################################
## outbbt <- boot.blackbox_transpose(rankings, missing=c(77,88,89),
##                 dims=3, minscale=5, verbose=FALSE, R=5)


###################################################
### code chunk number 41: chapter2.Rnw:940-941
###################################################
load("bst_france_12.rda")


###################################################
### code chunk number 42: BST_BSE_France3
###################################################
france.boot.bbt <- data.frame(
  party = colnames(rankings),
  point = original$stimuli[[1]][,2],
  se = apply(outbbt[,1,], 1, sd, na.rm=TRUE)
)
france.boot.bbt$lower <- with (france.boot.bbt, point-1.96*se)
france.boot.bbt$upper <- with (france.boot.bbt, point+1.96*se)
france.boot.bbt


###################################################
### code chunk number 43: ooc1
###################################################
#install.packages("devtools")
#devtools::install_github('tzuliu/ooc')
library(ooc)
data("ANES2004_OOC")
issuescales <- ANES2004[,1:14]
head(issuescales)


###################################################
### code chunk number 44: ooc2 (eval = FALSE)
###################################################
## ooc.result <- ooc(issuescales, dims=2, minvotes=10, lop=0.001,
##   polarity=c(1,1), iter=25, nv.method="svm.reg", cost=1)


###################################################
### code chunk number 45: chapter2.Rnw:1068-1069
###################################################
load("ooc_results.rda")


###################################################
### code chunk number 46: ooc3
###################################################
issue.result <- ooc.result$issues.unique
rownames(issue.result) <- colnames(issuescales)
print(issue.result[,c("normVectorAngle2D","wrongScale",
                      "correctScale", "errorsNull","PREScale")])


###################################################
### code chunk number 47: ooc4
###################################################
plot.df <- data.frame(
  presvote = ANES2004$presvote,
  x = ooc.result$respondents[,"coord1D"],
  y = ooc.result$respondents[,"coord2D"])
plot.df <- na.omit(plot.df)
ggplot(plot.df, aes(x=x, y=y, group=presvote, col=presvote,
        shape=presvote)) +
  geom_point(size=3) +
  theme_bw() +
  xlab("First Dimension") +
  ylab("Second Dimension") +
  scale_color_manual(values=gray.colors(2, end=.7),
                     name="Presidential\nVote") +
  scale_shape_manual(values=c("B", "K"),
                     name="Presidential\nVote") +
  theme(legend.position="bottom", aspect.ratio=1)



###################################################
### code chunk number 48: av1
###################################################
data("ches_eu")
# calculate party means and standard deviations across all CHES experts
means <- colMeans(sub.europe, na.rm=TRUE)
sds2 <- apply(sub.europe, 2, sd, na.rm=TRUE)


###################################################
### code chunk number 49: av2
###################################################
#Convert to matrix to numeric
sub.europe <- as.matrix(sub.europe)
mode(sub.europe) <- "numeric"
#Call the blackbox_transpose routine
result <- blackbox_transpose(sub.europe,dims=3,
                             minscale=5,verbose=TRUE)


###################################################
### code chunk number 50: av2a
###################################################
europe.dat <- data.frame(
  x = -result$stimuli[[2]][,2],
  y = result$stimuli[[2]][,3],
  means = means,
  party = colnames(sub.europe),
  type = car:::recode(means,
        "lo:3 = 'Left'; 3:7='Moderate'; 7:hi = 'Right'",
        as.factor=TRUE)
    )
parties.dat <- europe.dat[-(1:3), ]
vignette.dat <- europe.dat[(1:3), ]
onedim <- result$fits[1,3]
twodim <- result$fits[2,3]


###################################################
### code chunk number 51: av3
###################################################
ggplot(parties.dat, aes(x=x, y=y)) +
  geom_point(aes(shape=type, color=type), size=3) +
  scale_color_manual(values=gray.palette(3)) +
  theme_bw() +
  geom_text(data=vignette.dat, label=c("A", "B", "C"),
    show.legend=FALSE, size=10, color="black") +
  xlab(paste0("First Dimension (fit = ", round(onedim,1), "%)")) +
  ylab(paste0("Second Dimension (fit = ", round(twodim,1), "%)")) +
  theme( legend.position="bottom", aspect.ratio=1) +
  labs(colour="Party Group", shape="Party Group")



###################################################
### code chunk number 52: av4
###################################################
ggplot(parties.dat, aes(x=x, y=means)) +
  geom_smooth(method="loess", color="black", lwd=.5, se=FALSE) +
  geom_point(aes(shape=type, color=type), size=3) +
  scale_color_manual(values=gray.palette(3)) +
  theme_bw() +
  xlab("First Dimension Coordinates") +
  ylab("Mean Party Placement") +
  theme(legend.position="bottom", aspect.ratio=1) +
  labs(shape="Party Group", colour="Party Group")


###################################################
### code chunk number 53: nations1
###################################################
library(asmcjr)
data(nations)
print(nations)


###################################################
### code chunk number 54: double_center_1
###################################################
d <- (9-nations)^2


###################################################
### code chunk number 55: double_center_2
###################################################
D <- doubleCenter(d)
ev <- eigen(D)


###################################################
### code chunk number 56: double_center_3
###################################################
torgerson.soln <- sqrt(max((abs(ev$vec[,1]))^2 +
                             (abs(ev$vec[,2]))^2))
torgerson1 <- ev$vec[,1]*(1/torgerson.soln)*sqrt(ev$val[1])
torgerson2 <- ev$vec[,2]*(1/torgerson.soln)*sqrt(ev$val[2])


###################################################
### code chunk number 57: double_center_nations
###################################################
plot.df <- data.frame(
  dim1 = torgerson1,
  dim2 = torgerson2,
  country = rownames(nations)
)
ggplot(plot.df, aes(x=dim1, y=dim2)) +
  geom_point() +
  geom_text(aes(label=country), nudge_y=-.25) +
  xlab("") +
  ylab("") +
  geom_abline(slope=c(-1,1), intercept=c(0,0), lty=2) +
  xlim(-6.5,6.5) +
  ylim(-6.5,6.5) +
  theme_bw() +
  theme(aspect.ratio=1)


###################################################
### code chunk number 58: optim_SSE
###################################################
fr <- function(par, obs, ndim){
	d.x <- dist(matrix(par, ncol=ndim, byrow=2))
	diff <- d.x - sqrt(obs[lower.tri(obs)])
	sse <- c(diff %*% diff)
	sse
}


###################################################
### code chunk number 59: optim_1
###################################################
ndim <- 2
Z <- c(t(ev$vector[,1:ndim]))


###################################################
### code chunk number 60: optim_2
###################################################
model_sann <- optim(par=Z, fn=fr, method="SANN",
	control=list(maxit=50000), hessian=TRUE, obs=d, ndim=2)
model_bfgs <- optim(par=model_sann$par, fn=fr, method="BFGS",
	control=list(maxit=50000), hessian=TRUE, obs=d, ndim=2)
model_nm <- optim(par=model_bfgs$par, fn=fr, method="Nelder-Mead",
	control=list(maxit=50000), hessian=TRUE, obs=d, ndim=2)


###################################################
### code chunk number 61: optim_3
###################################################
SSE <- rbind(model_sann$value, model_bfgs$value, model_nm$value)
rownames(SSE) <- cbind("Stage 1 (Simulated Annealing)",
                       "Stage 2 (BFGS)","Stage 3 (Nelder-Mead)")
colnames(SSE) <- c("Sum of Squared Errors (SSE)")


###################################################
### code chunk number 62: optim_4
###################################################
print(SSE)


###################################################
### code chunk number 63: optim_5
###################################################
model_sann2 <- optim(par=model_nm$par, fn=fr, method="SANN",
	control=list(maxit=50000), hessian=TRUE, obs=d, ndim=2)
model_bfgs2 <- optim(par=model_sann2$par, fn=fr, method="BFGS",
	control=list(maxit=50000), hessian=TRUE, obs=d, ndim=2)
model_nm2 <- optim(par=model_bfgs2$par, fn=fr, method="Nelder-Mead",
	control=list(maxit=50000), hessian=TRUE, obs=d, ndim=2)


###################################################
### code chunk number 64: optim_6
###################################################
SSE <- rbind(model_sann2$value, model_bfgs2$value, model_nm2$value)
rownames(SSE) <- cbind("Stage 4 (Simulated Annealing)",
                       "Stage 5 (BFGS)","Stage 6 (Nelder-Mead)")
colnames(SSE) <- c("Sum of Squared Errors (SSE)")


###################################################
### code chunk number 65: optim_7
###################################################
print(SSE)


###################################################
### code chunk number 66: optim_8
###################################################
library(rgenoud)
set.seed(18134)
model_genoud <- genoud(fn=fr, nvars=length(Z), pop.size=3000,
    starting.values=model_nm2$par, obs=d, ndim=2)


###################################################
### code chunk number 67: optim_9
###################################################
print(model_genoud$value)


###################################################
### code chunk number 68: optim_nations
###################################################
xmetric <- data.frame(
  dim1 = scale(model_genoud$par[seq(1,23,by=2)], scale=FALSE),
  dim2 = scale(model_genoud$par[seq(2,24,by=2)], scale=FALSE),
  country = rownames(nations))
if (xmetric$dim1[8]<0) {xmetric$dim1 <- -xmetric$dim1}
if (xmetric$dim2[9]<0) {xmetric$dim2 <- -xmetric$dim2}
ggplot(xmetric, aes(x=dim1, y=dim2)) +
  geom_point() +
  geom_text(aes(label=country), nudge_y=-.25) +
  xlab("") +
  ylab("") +
  geom_abline(slope=c(-3/5,5/3), intercept=c(0,0), lty=2) +
  xlim(-4,4) +
  ylim(-4,4) +
  theme_bw() +
  theme(aspect.ratio=1)


###################################################
### code chunk number 69: smacof_metric_1
###################################################
data(nations)
d <- (9-nations)^2


###################################################
### code chunk number 70: smacof_metric_2
###################################################
#install.packages('smacof')
library(smacof)
smacof_metric_result <- smacofSym(delta=d, ndim=2, itmax = 1000,
                        type = "interval", eps=0.000001)


###################################################
### code chunk number 71: smacof_metric_3
###################################################
conf <- smacof_metric_result$conf
print(smacof_metric_result$niter)
print(smacof_metric_result$stress)


###################################################
### code chunk number 72: smacof_metric_nations
###################################################
smacof.dat <- data.frame(
  dim1 = conf[,1],
  dim2 = conf[,2],
  country=rownames(nations)
)
ggplot(smacof.dat, aes(x=dim1, y=dim2)) +
  geom_point() +
  geom_text(aes(label=country), nudge_y=-.05) +
  xlab("") +
  ylab("") +
  geom_abline(slope=c(-3/5,5/3), intercept=c(0,0), lty=2) +
  xlim(-1,1) +
  ylim(-1,1) +
  theme_bw() +
  theme(aspect.ratio=1)


###################################################
### code chunk number 73: smacof_metric_4
###################################################
library(smacof)
ndim <- 5
result <- vector("list", ndim)
for (i in 1:ndim){
result[[i]] <- smacofSym(delta=d, ndim=i, type = "interval")
}
stress <- sapply(result, function(x)x$stress)


###################################################
### code chunk number 74: smacof_metric_nations_stress
###################################################
stress.df <- data.frame(
  stress=stress,
  dimension = 1:length(stress))
ggplot(stress.df, aes(x=dimension, y=stress)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x="Dimension", y="Stress")


###################################################
### code chunk number 75: smacof_missing_1
###################################################
data(nations)
d <- (9-nations)^2


###################################################
### code chunk number 76: smacof_missing_2
###################################################
d[1,2] <- d[2,1] <- NA


###################################################
### code chunk number 77: smacof_missing_3
###################################################
weightmat <- d
weightmat[!is.na(d)] <- 1
weightmat[is.na(d)] <- 0


###################################################
### code chunk number 78: smacof_missing_4
###################################################
d[is.na(d)] <- mean(d, na.rm=TRUE)


###################################################
### code chunk number 79: smacof_missing_5
###################################################
missing_result <- smacofSym(delta=d, ndim=2, weightmat=weightmat,
     itmax = 1000, eps=0.000001)


###################################################
### code chunk number 80: smacof_missing_metric_nations
###################################################
smacof.m.dat <- data.frame(
  dim1 = missing_result$conf[,1],
  dim2 = missing_result$conf[,2],
  country = rownames(nations)
)
ggplot(smacof.m.dat, aes(x=dim1, y=dim2)) +
  geom_point() +
  geom_text(aes(label=country), nudge_y=-.05) +
  xlab("") +
  ylab("") +
  geom_abline(slope=c(-3/5,5/3), intercept=c(0,0), lty=2) +
  xlim(-1,1) +
  ylim(-1,1) +
  theme_bw() +
  theme(aspect.ratio=1)


###################################################
### code chunk number 81: metric_nonmetric_stress_1
###################################################
library(MASS)
data(nations)
d <- (9-nations)^2
ndim <- 5
metric.result <- vector("list", ndim)
for (i in 1:ndim){
metric.result[[i]] <- smacofSym(delta=d, ndim=i, type = "interval")
}
nonmetric.result <- vector("list", ndim)
for (i in 1:ndim){
nonmetric.result[[i]] <- smacofSym(delta=d, ndim=i, type = "ordinal")
}
metric.stress <- sapply(metric.result, function(x)x$stress)
nonmetric.stress <- sapply(nonmetric.result, function(x)x$stress)


###################################################
### code chunk number 82: metric_nonmetric_stress_plot
###################################################
nmstress <- data.frame(
  stress=c(metric.stress, nonmetric.stress),
  dimension = rep(1:ndim, 2),
  model = factor(rep(1:2, each=ndim), labels=c("Metric", "Nonmetric")))
ggplot(nmstress, aes(x=dimension, y=stress, color=model)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values=gray.palette(2), name="Model") +
  theme_bw() +
  xlab("Dimension") +
  ylab("Stress") +
  theme(aspect.ratio=1, legend.position="bottom")


###################################################
### code chunk number 83: smacof_nonmetric_1
###################################################
data(nations)
d <- (9-nations)^2


###################################################
### code chunk number 84: smacof_nonmetric_2
###################################################
smacof_nonmetric_result <- smacofSym(delta=d, ndim=2,
                          type = "ordinal")
nm.conf <- smacof_nonmetric_result$conf


###################################################
### code chunk number 85: smacof_nonmetric_3
###################################################
print(smacof_nonmetric_result$niter)
print(smacof_nonmetric_result$stress)


###################################################
### code chunk number 86: smacof_metric_nations2
###################################################
both.mods <- data.frame(
  dim1 = c(smacof_metric_result$conf[,1], smacof_nonmetric_result$conf[,1]),
  dim2 = c(smacof_metric_result$conf[,2], smacof_nonmetric_result$conf[,2]),
  model = factor(rep(1:2, each=12), label = c("Metric (SMACOF)", "Nonmetric (SMACOF)")),
  country = rep(rownames(nations), 2)
)
nudge <- rep(-.05, 24)
nudge[24] <- .05
ggplot(both.mods, aes(x=dim1, y=dim2)) +
  geom_point() +
  geom_text(aes(label=country), nudge_y=nudge) +
  geom_abline(slope=c(-3/5,5/3), intercept=c(0,0), lty=2) +
  xlab("First Dimension") +
  ylab("Second Dimension") +
  theme(aspect.ratio=1) +
  xlim(-1,1) +
  ylim(-1,1) +
  theme_bw() +
  facet_wrap(~model)+
  theme(aspect.ratio=1)


###################################################
### code chunk number 87: metricMDSnationsrotate1
###################################################
A <- matrix(c(cos(-30), -sin(-30), sin(-30), cos(-30)), nrow=2, ncol=2,
    byrow=TRUE)
rot.mds <- smacof_metric_result$conf %*% A


###################################################
### code chunk number 88: smacof_metric_nations_rotated_1
###################################################
rot.dat <- data.frame(
  dim1 = c(smacof_metric_result$conf[,1], rot.mds[,1]),
  dim2 = c(smacof_metric_result$conf[,2], rot.mds[,2]),
  model = factor(rep(1:2, each=12), label = c("Original", "Rotated")),
  country = rep(rownames(nations), 2)
)
ggplot(rot.dat, aes(x=dim1, y=dim2)) +
  geom_point() +
  geom_text(aes(label=country), nudge_y=nudge) +
  geom_abline(slope=c(-3/5,5/3), intercept=c(0,0), lty=2) +
  xlab("First Dimension") +
  ylab("Second Dimension") +
  theme(aspect.ratio=1) +
  xlim(-1,1) +
  ylim(-1,1) +
  theme_bw() +
  facet_wrap(~model)+
  theme(aspect.ratio=1)


###################################################
### code chunk number 89: procrustes_rotation1
###################################################
library(MCMCpack)
metric.conf <- rot.mds
nonmetric.conf <- smacof_nonmetric_result$conf
proc <- procrustes(X=nonmetric.conf, Xstar=metric.conf,
    translation=FALSE, dilation=FALSE)
nonmetric.conf.rotated <- proc$X.new


###################################################
### code chunk number 90: smacof_metric_nations_procrustes_rotation_1
###################################################
proc.dat <- data.frame(
  dim1 = c(rot.mds[,1], nonmetric.conf.rotated[,1]),
  dim2 = c(rot.mds[,2], nonmetric.conf.rotated[,2]),
  model = factor(rep(1:2, each=12),
      labels=c("Rotated Metric Solution",
               "Procrustes Rotate NM Solution")),
  country = rownames(nations)
)
ggplot(proc.dat, aes(x=dim1, y=dim2)) +
  geom_point() +
  geom_text(aes(label=country), nudge_y=nudge) +
  xlab("First Dimension") +
  ylab("Second Dimension") +
  theme(aspect.ratio=1) +
  xlim(-1,1) +
  ylim(-1,1) +
  theme_bw() +
  facet_wrap(~model)+
  theme(aspect.ratio=1)


###################################################
### code chunk number 91: detach_1
###################################################
# From time to time, detach!
rm(list=ls(all=TRUE))


###################################################
### code chunk number 92: smacof_metric_nonmetric_90th_senate_1
###################################################
data(senate.90)
print(senate.90[1:6,7:12])


###################################################
### code chunk number 93: smacof_metric_nonmetric_90th_senate_2
###################################################
d <- (1-senate.90[,8:109])^2
metric.result <- smacofSym(d, ndim=2, type = "interval")
metric.conf <- metric.result$conf
metric.stress <- metric.result$stress
nonmetric.result <- smacofSym(d, ndim=2, type = "ordinal")
nonmetric.conf <- nonmetric.result$conf
nonmetric.stress <- nonmetric.result$stress


###################################################
### code chunk number 94: smacof_metric_nonmetric_90th_senate_2
###################################################
if (metric.conf[1,1] > 0) metric.conf[,1] <- -metric.conf[,1]
if (metric.conf[1,2] > 0) metric.conf[,2] <- -metric.conf[,2]
if (nonmetric.conf[1,1] > 0) nonmetric.conf[,1] <- -nonmetric.conf[,1]
if (nonmetric.conf[1,2] > 0) nonmetric.conf[,2] <- -nonmetric.conf[,2]


###################################################
### code chunk number 95: smacof_metric_90th_senate_plot
###################################################
senate.df <- data.frame(
  dim1 = c(metric.conf[,1], nonmetric.conf[,1]),
  dim2 = c(metric.conf[,2], nonmetric.conf[,2]),
  party = rep(senate.90$party, 2),
  statecode = rep(senate.90$statecode, 2),
  model = factor(rep(1:2, each=nrow(metric.conf)),
                labels=c("Metric", "Nonmetric")))

senate.df$label <- NA
senate.df$label[which(senate.df$party == 100 &
        senate.df$statecode %in% c(40:49,51,53,56))] <- "S"
senate.df$label[which(senate.df$party == 100 &
        !(senate.df$statecode %in% c(99,40:49,51,53,56)))] <- "D"
senate.df$label[which(senate.df$party == 200)] <- "R"
senate.df$label[which(senate.df$statecode == 99)] <- "LBJ"
senate.df$label <- factor(senate.df$label,
                          levels=c("D", "S", "R", "LBJ"))
ggplot() +
  geom_text(data=subset(senate.df, label!="LBJ"),
            aes(x=dim1, y=dim2, color=label,
                      label=label), show.legend=FALSE) +
  geom_point(data=subset(senate.df, label!="LBJ"),
            aes(x=dim1, y=dim2, color=label), size=-1) +
  theme_bw() +
  facet_wrap(~model) +
  scale_color_manual(values=rev(gray.palette(4)), name="Party Group",
    labels=c("Democrat", "Southern Dem", "Republican", "LBJ")) +
  guides(colour = guide_legend(override.aes =
                      list(size = 4, pch=c("D", "S", "R")))) +
  geom_text(data=subset(senate.df, label=="LBJ"),
            aes(x=dim1, y=dim2, label=label),
            show.legend=FALSE, size=6) +
  xlab("Liberal-Conservative") +
  xlim(-1.25, 1.25) +
  ylab("Region/Civil Rights") +
  ylim(-1.25, 1.25) +
  theme(aspect.ratio=1, legend.position="bottom")


###################################################
### code chunk number 96: smacof_metric_nonmetric_90th_senate_shepard
###################################################
metric.obsdiss <- metric.result$dhat
metric.dhat <- metric.result$confdist
nonmetric.obsdiss <- nonmetric.result$dhat
nonmetric.dhat <- nonmetric.result$confdist


###################################################
### code chunk number 97: smacof_metric_90th_senate_shepard_plot
###################################################
shep.dat <- data.frame(
  obsdiss = c(c(metric.obsdiss), c(nonmetric.obsdiss)),
  repdiss = c(c(metric.dhat), c(nonmetric.dhat)),
  model = factor(rep(1:2, each=length(c(metric.result$dhat))),
                 labels=c("Metric", "Nonmetric")))
ggplot(shep.dat, aes(x=obsdiss, y=repdiss)) +
  geom_point(shape=1, col="gray65") +
  facet_wrap(~model, scales="free") +
  geom_smooth(method="loess", span=.85,
              method.args=list(family="symmetric"),
              se=FALSE, col="black", lwd=2, fullrange=FALSE) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  labs(x="Observed Distances", y="Reproduced Distances")


###################################################
### code chunk number 98: interpoint_correlations1
###################################################
interpoint.d.metric <- dist(metric.conf)
interpoint.d.nonmetric <- dist(nonmetric.conf)
cor(interpoint.d.metric, interpoint.d.nonmetric)


###################################################
### code chunk number 99: indscal_3
###################################################
data(french.parties.individuals)
french.parties.individuals <- na.omit(french.parties.individuals)


###################################################
### code chunk number 100: indscal_4
###################################################
p <- as.matrix(french.parties.individuals)
parties <- lapply(1:nrow(french.parties.individuals),
       function(x)outer(p[x,], p[x,], "-")^2+.001)
for(i in 1:length(parties)){diag(parties[[i]]) <- 0}


###################################################
### code chunk number 101: indscal_5
###################################################
rownames(parties[[1]]) <- attr(french.parties.individuals,
                               "var.labels")
colnames(parties[[1]]) <- 1:ncol(parties[[1]])
print(parties[[1]])


###################################################
### code chunk number 102: indscal_6 (eval = FALSE)
###################################################
## indscal.result.2dim <- smacofIndDiff(delta=parties,
## ndim=2, type="ratio",constraint="indscal",eps=0.00001)


###################################################
### code chunk number 103: chapter3.Rnw:1214-1215
###################################################
load("france_indscal_ch3.rda")


###################################################
### code chunk number 104: indscal_7
###################################################
summary(indscal.result.2dim)


###################################################
### code chunk number 105: indscal_nations_group_plot
###################################################
parts <- rownames(indscal.result.2dim$gspace)
ind.df <- data.frame(
  x = indscal.result.2dim$gspace[,1],
  y = indscal.result.2dim$gspace[,2])
ind.df$party <- factor(parts, levels=parts[order(ind.df$x)])
ggplot(ind.df, aes(x=x, y=y, shape=party)) +
  geom_point(size=3) +
  scale_shape_manual(values=1:8, name="Party") +
  xlim(-1.15,1.15) +
  ylim(-1.15,1.15) +
  theme_bw() +
  xlab("First Dimension") +
  ylab("Second Dimension")+
  theme(legend.position="bottom", aspect.ratio=1) +
  guides(shape = guide_legend(nrow=3))


###################################################
### code chunk number 106: indscal_nations_respondents_plot_1
###################################################
confs <- indscal.result.2dim$conf
weights <- rbind(diag(indscal.result.2dim$cweights[[176]]),
                 diag(indscal.result.2dim$cweights[[65]]))
iw.df <- data.frame(
    x = c(confs[[176]][,1], confs[[65]][,1]),
    y = c(confs[[176]][,2], confs[[65]][,2]),
    party = ind.df$party,
    respondent = factor(rep(c(1,2), each=nrow(confs[[1]])),
                        labels = c("# 176", "# 65"))
    )
ggplot(iw.df, aes(x=x, y=y, shape=party)) +
  geom_point(size=3) +
  scale_shape_manual(values=1:8, name="Party") +
  theme_bw() +
  xlab("First Dimension") +
  ylab("Second Dimension")+
  facet_wrap(~respondent) +
  xlim(-1.15,1.15) +
  ylim(-1.15,1.15) +
  theme(legend.position="bottom", aspect.ratio=1)


###################################################
### code chunk number 107: MLSMU6interest1
###################################################
library(asmcjr)
data("interest1981")


###################################################
### code chunk number 108: MLSMU6interest2 (eval = FALSE)
###################################################
## input <- as.matrix(interest1981[,9:38])
## cutoff <- 5
## input <- input[rowSums(!is.na(input))>=cutoff,]
## input <- (100-input)/50
## input2 <- input*input
## input2[is.na(input)] <- (mean(input,na.rm=TRUE))^2


###################################################
### code chunk number 109: MLSMU6interest3 (eval = FALSE)
###################################################
## inputDC <- doubleCenterRect(input2)


###################################################
### code chunk number 110: MLSMU6interest4 (eval = FALSE)
###################################################
## xsvd <- svd(inputDC)
## ndim <- 2
## stims <- xsvd$v[,1:ndim]
## inds <- xsvd$u[, 1:ndim]# matrix(0, nrow=n, ncol=ndim)
## for (i in 1:ndim){
## stims[,i] <- stims[,i] * sqrt(xsvd$d[i])
## inds[,i] <- inds[,i] * sqrt(xsvd$d[i])
## }


###################################################
### code chunk number 111: MLSMU6interest5
###################################################
out <- mlsmu6(input = interest1981[,9:38], ndim=2, cutoff=5,
    id=factor(interest1981$party, labels=c("D", "R")))
tail(out$iter)


###################################################
### code chunk number 112: plotMLSMU6interest1
###################################################
library(ggplot2)
inc.stims <- c("ada", "cope", "ccus", "cc", "ll", "nfu", "ntu",
     "aft", "nfib", "ccause", "aclu", "ripon")
plot(out, selected.stims=inc.stims) +
    labs(x="First Dimension\nD=Democrat, R=Republican", y="Dimension 2")


###################################################
### code chunk number 113: SMACOFrect1
###################################################
library(smacof)
data(denmarkEES2009)
input.den <- as.matrix(denmarkEES2009[,c("q39_p1","q39_p2","q39_p3",
    "q39_p4","q39_p5","q39_p6","q39_p7","q39_p8")])
colnames(input.den) <- c("Social Democrats",
    "Danish Social Liberal Party", "Conservative Peoples Party",
    "Socialist Peoples Party", "Danish Peoples Party",
    "Liberal Party", "Liberal Alliance", "June Movement")
input.den[input.den == 77 | input.den == 88 |
    input.den == 89] <- NA
input.den <- (10-input.den)/5
cutoff <- 5
input.den <- input.den[rowSums(!is.na(input.den))>=cutoff,]


###################################################
### code chunk number 114: SMACOFrect2
###################################################
weightmat <- input.den
weightmat[!is.na(input.den)] <- 1
weightmat[is.na(input.den)] <- 0
input.den[is.na(input.den)] <- mean(input.den,na.rm=TRUE)


###################################################
### code chunk number 115: SMACOFrect3
###################################################
result <- smacofRect(delta=input.den, ndim=2, itmax=1000,
    weightmat=weightmat, init=NULL, verbose=FALSE)


###################################################
### code chunk number 116: SMACOFrect5
###################################################
voters <- result$conf.row
parties <- result$conf.col
if (parties[1,1] > 0){
parties[,1] <- -1 * parties[,1]
voters[,1] <- -1 * voters[,1]
}


###################################################
### code chunk number 117: SMACOFrectParties1
###################################################
voters.dat <- data.frame(
  dim1 = voters[,1],
  dim2 = voters[,2]
)
parties.dat <- data.frame(
  dim1 = parties[,1],
  dim2 = parties[,2],
  party = factor(1:8, labels=rownames(parties))
)

g <- ggplot() +
  geom_point(data = voters.dat, aes(x=dim1, y=dim2), pch=1,
             color="gray65") +
  geom_point(data=parties.dat, aes(x=dim1, y=dim2), size=2.5) +
  geom_text(data=parties.dat, aes(x=dim1, y=dim2, label = party),
            nudge_y=.1, size=5) +
  theme_bw() +
  labs(x="First Dimension", y="Second Dimension") +
  theme(aspect.ratio=1)
g


###################################################
### code chunk number 118: OLScalcnormvec1
###################################################
library(basicspace)
LR <- denmarkEES2009[,c("q46","q47_p1","q47_p2","q47_p3",
    "q47_p4","q47_p5","q47_p6","q47_p7","q47_p8")]
LR <- as.matrix(LR)
colnames(LR) <- c("Self","Social Democrats",
  "Danish Social Liberal Party","Conservative Peoples Party",
    "Socialist Peoples Party","Danish Peoples Party","Liberal Party",
    "Liberal Alliance","June Movement")
mode(LR) <- "numeric"
AM.result <- aldmck(LR,polarity=2,respondent=1,
                    missing=c(77,88,89,99),verbose=TRUE)


###################################################
### code chunk number 119: OLScalcnormvec2
###################################################
ols <- lm(AM.result$stimuli ~ parties[,1] + parties[,2])
printCoefmat(summary(ols)$coefficients)


###################################################
### code chunk number 120: OLScalcnormvec3
###################################################
N1 <- ols$coefficients[2] /
    (sqrt((ols$coefficients[2]^2) + (ols$coefficients[3]^2)))
N2 <- ols$coefficients[3] /
    (sqrt((ols$coefficients[2]^2) + (ols$coefficients[3]^2)))


###################################################
### code chunk number 121: SMACOFrectParties2
###################################################
exp.factor <- 1.1
g +
  geom_segment(aes(x=exp.factor*-N1, y=exp.factor*-N2,
    xend=exp.factor*N1, yend=exp.factor*N2), lty=2, size=1)



###################################################
### code chunk number 122: normvec_cuttingline
###################################################
#
#install.packages('MASS')
library(MASS)
library(asmcjr)
#,
cut.dat <- c(88,4418,41,100,-0.900,-0.300,6,88,8764,41,100,-0.500,-0.400,6,88,486,81,100,-0.250,0.050,6,88,3658,61,200,0.400,-0.600,6,88,3864,81,100,-0.500,0.750,6,88,4227,61,100,0.500,-0.200,6,88,486,81,100,-0.050,0.400,6,88,3388,42,100,0.000,0.700,1,88,3388,42,100,0.600,0.100,1,88,5372,71,200,0.900,0.000,1,88,6151,42,100,0.750,0.350,1,88,5372,71,200,0.800,0.400,1)

X <- matrix(cut.dat,ncol=7,byrow=TRUE)

#
#
#  Cutting Line
#
#  88  409  74822  74  27 101   6 6 1 -0.341 -0.801 -0.599
#
#midpoint <-  0.341
midpoint <-  0.375
norm.vec.1 <-  0.600
norm.vec.2 <-  0.800
#
plot(X[,5],X[,6],type="n",asp=1,main="",
       xlab="",
       ylab="",
       xlim=c(-1.0,1.0),ylim=c(-1.0,1.0))
#
#  Yea
points(X[X[,7]==1,5],X[X[,7]==1,6],pch='Y',col="gray25")
#  Nay
points(X[X[,7]==6,5],X[X[,7]==6,6],pch='N',col="gray50")
#
#  Plot Cutting Line
#
#  This computes the point on the Normal Vector
#    Through which the Cutting Line passes
#
xpoint <- midpoint*norm.vec.1
ypoint <- midpoint*norm.vec.2
#
#  Add cutting Line
arrows(xpoint,ypoint,xpoint+norm.vec.2,ypoint-norm.vec.1,length=0.0,lwd=2,col="black")
arrows(xpoint,ypoint,xpoint-norm.vec.2,ypoint+norm.vec.1,length=0.0,lwd=2,col="black")
#
#  Add the Normal Vector
#arrows(0.0,0.0,norm.vec.1,norm.vec.2,length=0.1,lwd=2,col="black")
#
#  Indicate the outcome points
#
xktp <- .2
xktp2 <- .3
arrows(xpoint-xktp*norm.vec.2,ypoint+xktp*norm.vec.1,xpoint-xktp*norm.vec.2+xktp2*norm.vec.1,ypoint+xktp*norm.vec.1+xktp2*norm.vec.2,length=0.0,lty=2,lwd=3,col="black")
arrows(xpoint-xktp*norm.vec.2,ypoint+xktp*norm.vec.1,xpoint-xktp*norm.vec.2-xktp2*norm.vec.1,ypoint+xktp*norm.vec.1-xktp2*norm.vec.2,length=0.0,lty=2,lwd=3,col="black")
#
#  Labels For Outcome Points
#
text(xpoint-xktp*norm.vec.2+xktp2*norm.vec.1,ypoint+xktp*norm.vec.1+xktp2*norm.vec.2," Oy",col="black",adj=0)
text(xpoint-xktp*norm.vec.2-xktp2*norm.vec.1,ypoint+xktp*norm.vec.1-xktp2*norm.vec.2,"On ",col="black",adj=1)
#
text( 1.0,-.45,"Cutting\nPlane")
text( 0.825, .8,"Normal\nVector")
#
points(0,0,pch=16,cex=1.1,col="black")
text(0,-0.1,"Origin (0,0)")


###################################################
### code chunk number 123: cleanup_chp6
###################################################
rm(list=ls(all=TRUE))


###################################################
### code chunk number 124: dwnomdensityplot1
###################################################
data("rcx", package="asmcjr")


###################################################
### code chunk number 125: dwnomdensityplot2
###################################################
ncong <- c(90, 100, 110)
sub <- with(rcx, which(cong %in% ncong & dist != 0))
polarization <- cbind(rcx[sub, c("dwnom1", "party")],
    congress = factor(paste("House", rcx$cong[sub]),
    levels=c("House 90", "House 100", "House 110")))
polarization$party <- factor(polarization$party,
    levels=c(100, 200), labels=c("D", "R"))


###################################################
### code chunk number 126: plotdwnomdensity
###################################################
library(ggplot2)
ggplot(polarization, aes(x=dwnom1, group=party,
    colour=party, fill=party, colour=party)) +
  geom_density(adjust=2.5, alpha=.2) +
  facet_wrap(~congress, ncol=2) +
  xlab("DW-Nominate\n(First Dimension)") +
  theme_bw() +
  scale_colour_manual(values=c("gray25", "gray75"), name="Party") +
  scale_fill_manual(values=c("gray25", "gray75"), name="Party") +
  theme(aspect.ratio=1, legend.position="bottom")


###################################################
### code chunk number 127: wnominate1
###################################################
data(hr108, package="asmcjr")
class(hr108)


###################################################
### code chunk number 128: wnominate2 (eval = FALSE)
###################################################
## #install.packages("wnominate")
## library(wnominate)
## result <- wnominate(hr108, ubeta=15, uweights=0.5, dims=2,
##   minvotes=20, lop=0.025, trials=1, polarity=c(1,5), verbose=FALSE)


###################################################
### code chunk number 129: chapter5_nb.Rnw:381-382
###################################################
load("wnominate2_hr108.rda")


###################################################
### code chunk number 130: wnominate3
###################################################
summary(result)


###################################################
### code chunk number 131: legislatorestimatesplot
###################################################
library(ggplot2)
group <- rep(NA, nrow(result$legislators))
group[with(result$legislators, which(partyCode == 100
    & icpsrState %in% c(40:51, 53, 54)))] <- 1
group[with(result$legislators, which(partyCode == 100
    & icpsrState %in% c(1:39, 52, 55:82)))] <- 2
group[with(result$legislators, which(partyCode == 200))] <- 3
group[with(result$legislators, which(partyCode == 328))] <- 4
group <- factor(group, levels=1:4,
    labels=c("Southern Dems", "Northern Dems",
             "Republicans", "Independents"))

plot_wnom_coords(result, shapeVar=group, dropNV=FALSE) +
  scale_color_manual(values=gray.colors(4, end=.75), name="Party Group",
  labels=c("Southern Dems", "Northern Dems",
           "Republicans", "Independents")) +
  scale_shape_manual(values=c("S", "N", "R", "I"), name="Party Group",
  labels=c("Southern Dems", "Northern Dems",
           "Republicans", "Independents")) +
  theme_bw() +
  theme(aspect.ratio=1, legend.position="bottom") +
  xlab("First Dimension") +
  ylab("Second Dimension") +
  guides(colour=guide_legend(nrow=2))



###################################################
### code chunk number 132: rollcallplot
###################################################
weight <-  result$weights[2]/result$weights[1]
wnom.dat <- data.frame(
    coord1D = result$legislators$coord1D,
    coord2D = result$legislators$coord2D*weight,
    group=group)
pdf("spatial-rollcallplot.pdf", height=6.25, width=5)
plot_rollcall(result, hr108, wnom.dat, 528,
              wnom.dat$group, dropNV=TRUE) +
  theme_bw() +
  scale_shape_manual(values=c("S", "D", "R", "I"),
                     name="Party Group") +
  xlab("First Dimension") +
  ylab("Second Dimension") +
  theme(aspect.ratio=1, legend.position="bottom") +
  guides(shape = guide_legend(nrow=2))
dev.off()


###################################################
### code chunk number 133: plotrollcalls11 (eval = FALSE)
###################################################
## rc.errors(result, hr108, 528)[c("tot.errors", "PRE")]


###################################################
### code chunk number 134: rollcallploterrors
###################################################
plot_rollcall(result, hr108, wnom.dat, 528,
              shapeVar=wnom.dat$group, onlyErrors=TRUE) +
  scale_shape_manual(values=c("S", "N", "R"), name="Party Group") +
  theme_bw() +
  theme(aspect.ratio=1, legend.position="bottom") +
  xlab("First Dimension") +
  ylab("Second Dimension") +
  guides(shape = guide_legend(nrow=2))


###################################################
### code chunk number 135: coombsmeshplot
###################################################
plot.cutlines(result, lines=100, main="")


###################################################
### code chunk number 136: cuttinglineangles1
###################################################
angles <- makeCutlineAngles(result)
head(angles)


###################################################
### code chunk number 137: cuttinglineangles3
###################################################
print(angles[528,])


###################################################
### code chunk number 138: cuttinglineangles4
###################################################
mean(abs(angles$angle), na.rm=T)
mean(abs(angles$angle) > 60, na.rm=T)


###################################################
### code chunk number 139: wnominatecuttinglineangles
###################################################
ggplot(angles, aes(x=angle)) +
    geom_histogram() +
    theme_bw() +
    theme(aspect.ratio=1) +
    xlab("Cutline Angles")


###################################################
### code chunk number 140: wnomparbootstrap1
###################################################
library(wnominate)
data("rc_ep", package="asmcjr")


###################################################
### code chunk number 141: wnomparbootstrap2 (eval = FALSE)
###################################################
## result <- wnominate(rc_ep, ubeta=15, uweights=0.5, dims=2, minvotes=20,
##     lop=0.025, trials=101, polarity=c(25,25), verbose=FALSE)


###################################################
### code chunk number 142: loadwnomparboot2
###################################################
load("rc_ep_wnom.rda")


###################################################
### code chunk number 143: wnombootstrapplot1
###################################################
pb <- result$legislators$EPG
pb[which(!pb %in% c("C", "M", "G", "S", "L"))] <- NA
plot_wnom_coords(result, pb, dropNV=TRUE, ptSize=3) +
  theme_bw() +
  ylim(-1,1) +
  xlim(-1,1) +
  coord_fixed() +
  scale_shape_manual(values=c("C", "G", "L", "M", "S"),
      labels= c("Conservative", "Gaullist", "Liberal",
                "Radical Left", "Socialist")) +
  scale_color_manual(values=gray.colors(5, end=.75),
      labels= c("Conservative", "Gaullist", "Liberal",
                "Radical Left", "Socialist")) +
  labs(x="First Dimension", y="Second Dimension",
       shape= "Party", colour="Party") +
  theme(aspect.ratio = 1, legend.position="bottom") +
  guides(shape=guide_legend(nrow=2))


###################################################
### code chunk number 144: wnombootstrapplot2
###################################################
pb2 <- pb
pb2[which(!(pb2 %in% c("C", "G") &
              abs(result$legislators$corr.1) > .3))] <- NA
plot_wnom_coords(result, shapeVar=pb2, ptSize=3,
                 dropNV=TRUE, ci=TRUE) +
  theme_bw() + ylim(-1.35,1.35) + xlim(-1.35,1.35) +
  theme(aspect.ratio=1, legend.position="bottom") +
  scale_shape_manual(values=c("C", "G"),
      labels=c("Conservatives", "Gaullists"), name="Party") +
  scale_color_manual(values=c("black", "black"),
      labels = c("Conservatives", "Gaullists"), name="Party") +
  xlab("First Dimension (Left - Right)") +
  ylab("Second Dimension (European Integration)") +
  labs(shape="Party", colour="Party")



###################################################
### code chunk number 145: ocfrance1
###################################################
data("france4", package="asmcjr")
rc <- rollcall(data=france4[,6:ncol(france4)],
    yea=1,
    nay=6,
    missing=7,
    notInLegis=c(8,9),
    legis.names=paste(france4$NAME,france4$CASEID,sep=""),
    vote.names=colnames(france4[6:ncol(france4)]),
    legis.data=france4[,2:5],
    vote.data=NULL,
    desc="National Assembly of the French Fourth Republic")


###################################################
### code chunk number 146: ocfrance2 (eval = FALSE)
###################################################
## #install.packages('oc')
## library(oc)
## result2 <- oc(rc, dims=2, minvotes=20, lop=0.025,
##     polarity=c(2,2), verbose=FALSE)


###################################################
### code chunk number 147: chapter5_nb.Rnw:787-788
###################################################
load("france_oc.rda")


###################################################
### code chunk number 148: ocfrance3
###################################################
summary(result2)


###################################################
### code chunk number 149: ocfrance4 (eval = FALSE)
###################################################
## result1 <- oc(rc, dims=1, minvotes=20, lop=0.025,
##     polarity=2, verbose=FALSE)


###################################################
### code chunk number 150: ocfrance5
###################################################
fits <- cbind(result1$fits, result2$fits)
colnames(fits) <- c("1 Dim", "2 Dim")
rownames(fits) <- c("% Correct", "APRE")
fits


###################################################
### code chunk number 151: ocfrance6
###################################################
# (THIS CODE CHUNK NOT SEEN SEEN) This stores the cached results from
# "result2" as "result", so that we're working with identical
# results in the roll call section
result <- result2


###################################################
### code chunk number 152: plotocfrance1
###################################################
pb <- rc$legis.data$PAR
pb <- car::recode(pb, '1="Communitst"; 2="Socialists";
     5="Christian Dems"; 7 = "Poujadists"; else=NA',
     as.factor=TRUE)
plot_oc_coords(result, pb, dropNV=TRUE, ptSize=3) +
    theme_bw() +
    theme(aspect.ratio=1, legend.position="bottom") +
    scale_shape_manual(values=c("C", "S", "D", "P"),
        labels= c("Communist", "Socialist",
                  "Christian Dem", "Poujadists"),
        name = "Party") +
    scale_color_manual(values=gray.colors(5),
        labels= c("Communist", "Socialist",
                  "Christian Dem", "Poujadists"),
        name = "Party")+
  labs(x="First Dimension", y="Second Dimension")


###################################################
### code chunk number 153: ocfrance5
###################################################
deg2rad <- function(x)x*pi/180
rad45 <- deg2rad(45)
A <- matrix(c(cos(rad45), -sin(rad45),
              sin(rad45), cos(rad45)),
            nrow=2, ncol=2, byrow=TRUE)


###################################################
### code chunk number 154: plotocfrance2
###################################################
pb <- rc$legis.data$PAR
pb <- car::recode(pb, '1="Communitsts"; 2="Socialists";
     5="Christian Dems"; 7 = "Poujadists"; else=NA',
     as.factor=TRUE)
plot_oc_coords(result, pb, dropNV=TRUE, ptSize=3, rotMat=A) +
    theme_bw() +
    coord_fixed() +
    scale_shape_manual(values=c("C", "S", "D", "P"),
        labels= c("Communist", "Socialist",
                  "Christian Dem", "Poujadists"),
        name = "Party") +
    scale_color_manual(values=gray.colors(5),
        labels= c("Communist", "Socialist",
                  "Christian Dem", "Poujadists"),
        name = "Party")+
  labs(x="First Dimension", y="Second Dimension") +
  theme(aspect.ratio=1, legend.position="bottom")



###################################################
### code chunk number 155: ocfrancerollcall1
###################################################
which(colnames(rc$votes)=="V3090")


###################################################
### code chunk number 156: plotocfrancerollcall1
###################################################
vote <- rc$votes[, "V3090"]
plot_oc_rollcall(result, rc, shapeVar=pb, 807, dropNV=TRUE,
                 ptSize=3, onlyErrors=FALSE)  +
  theme_bw() +
  theme(aspect.ratio=1, legend.position="bottom") +
  xlim(-1.1,1.1) + ylim(-1.1,1.1) +
  scale_shape_manual(values=c("D", "C", "P", "S"),
      labels=c("Christian Dems", "Communists",
               "Poujadists", "Socialists"),
      name = "Party") +
  xlab("First Dimension (Left-Right)") +
  ylab("Second Dimension (Pro / Anti-Regime") +
  guides(shape=guide_legend(nrow=2)) +
  annotate("text", label=paste0("Yea = ",
    sum(vote %in% rc$codes$yea)), colour="gray33", x=-.9, y=1) +
  annotate("text", label=paste0("Nay = ",
    sum(vote %in% rc$codes$nay)), colour="gray67", x=-.9, y=.9) +
  annotate("text", label="Predicted\nYea",
    colour="gray33", x=.1, y=.2 ) +
  annotate("text", label="Predicted\nNay",
    colour="gray67", x=-.6, y=-.5 )


###################################################
### code chunk number 157: ocfrancerollcall6
###################################################
rc.errors(result, rc, 807)[c("tot.errors", "PRE")]


###################################################
### code chunk number 158: plotocfrancerollcallerrors
###################################################
vote <- rc$votes[, "V3090"]
plot_oc_rollcall(result, rc, shapeVar=pb, 807, dropNV=TRUE,
                 ptSize=3, onlyErrors=TRUE)  +
  theme_bw() +
  theme(aspect.ratio=1, legend.position="bottom") +
  xlim(-1.1,1.1) + ylim(-1.1,1.1) +
  scale_shape_manual(values=c("D", "C", "P", "S"),
      labels=c("Christian Dems", "Communists",
               "Poujadists", "Socialists"),
      name = "Party") +
  xlab("First Dimension (Left-Right)") +
  ylab("Second Dimension (Pro / Anti-Regime") +
  annotate("text", label=paste0("Yea = ",
    sum(vote %in% rc$codes$yea)), colour="gray33", x=-.9, y=1) +
  annotate("text", label=paste0("Nay = ",
    sum(vote %in% rc$codes$nay)), colour="gray67", x=-.9, y=.9) +
  annotate("text", label="Predicted\nYea",
           colour="gray33", x=.1, y=.2 ) +
  annotate("text", label="Predicted\nNay",
           colour="gray67", x=-.6, y=-.5 ) +
  annotate("text", label="Errors = 76",
           colour="black", x=.9, y=1) +
  annotate("text", label="PRE = 0.66",
           colour="black", x=.9, y=.9) +
  guides(shape=guide_legend(nrow=2))


###################################################
### code chunk number 159: octhermometers1
###################################################
data("candidatetherms2008", package="asmcjr")
print(candidatetherms2008[1:5,1:5])


###################################################
### code chunk number 160: octhermometers3
###################################################
X <- binary.comparisons(candidatetherms2008)
print(X[1:5,1:4])


###################################################
### code chunk number 161: octhermometers4
###################################################
library(oc)


###################################################
### code chunk number 162: octhermometers5
###################################################
ANES08 <- rollcall(data=X, yea=1, nay=6, missing=9, notInLegis=0,
    legis.names=NULL, vote.names=colnames(T), legis.data=NULL,
    vote.data=NULL, desc="2008 American National Election Study")


###################################################
### code chunk number 163: octhermometers6 (eval = FALSE)
###################################################
## result1 <- oc(ANES08, dims=1, minvotes=20, lop=0.005,
##     polarity=3, verbose=FALSE)
## result2 <- oc(ANES08, dims=2, minvotes=20, lop=0.005,
##     polarity=c(3,3), verbose=FALSE)


###################################################
### code chunk number 164: chapter5_nb.Rnw:1091-1092
###################################################
load("octherm6_nes.rda")


###################################################
### code chunk number 165: octhermometers7
###################################################
fits <- cbind(result1$fits, result2$fits)
colnames(fits) <- c("1 Dim", "2 Dim")
rownames(fits) <- c("% Correct", "APRE")
fits


###################################################
### code chunk number 166: octhermometers8
###################################################
angles <- makeCutlineAngles(result2)
angles$comparison <- colnames(X)
print(angles[18, c("comparison", "angle")])


###################################################
### code chunk number 167: plotocthermometers1
###################################################
plot_OCcutlines2(result2, lines=1:8, main.title = "")
text("M", x=.45, y=-.25, cex=2.5)


###################################################
### code chunk number 168: octhermometers12
###################################################
data("presvote2008", package="asmcjr")
pts <- data.frame(
    oc1 = result2$legislators[,"coord1D"],
    oc2 = result2$legislators[,"coord2D"],
    vote = presvote2008[,1])
pts <- pts[which(pts$vote == 0), ]


###################################################
### code chunk number 169: plotocthermometers2
###################################################
plot_OCcutlines2(result2, lines=1:8, main.title="")
points(pts$oc1, pts$oc2, pch="M", cex=.75, col="gray70")
text("M", x=.45, y=-.25, cex=2.5)


###################################################
### code chunk number 170: ocwnomcomparisonfrance1
###################################################
library(pscl)
data("france4", package="asmcjr")
rc <- rollcall(data=france4[,6:ncol(france4)],
    yea=1,
    nay=6,
    missing=7,
    notInLegis=c(8,9),
    legis.names=paste(france4$NAME,france4$CASEID,sep=""),
    vote.names=colnames(france4[6:ncol(france4)]),
    legis.data=france4[,2:5],
    vote.data=NULL,
    desc="National Assembly of the French Fourth Republic")

#


###################################################
### code chunk number 171: chapter5_nb.Rnw:1193-1198 (eval = FALSE)
###################################################
## result.oc <- oc(rc, dims=2, minvotes=20, lop=0.025,
##                 polarity=c(2,2), verbose=FALSE)
## result.wnom <- wnominate(rc, ubeta=15, uweights=0.5,
##                          dims=2, minvotes=20, lop=0.025,
##                          trials=3, polarity=c(2,2), verbose=FALSE)


###################################################
### code chunk number 172: chapter5_nb.Rnw:1201-1204
###################################################
load("france_oc.rda")
result.oc <- result2
load("france_wnom_result.rda")


###################################################
### code chunk number 173: ocwnomcomparisonfrance1a
###################################################
A <- matrix(c(cos(rad45), -sin(rad45), sin(rad45),
    cos(rad45)), nrow=2, ncol=2, byrow=TRUE)
#
rot.oc <- cbind(result.oc$legislators$coord1D,
                result.oc$legislators$coord2D)
for (i in 1:nrow(rot.oc)){
rot.oc[i,] <- rot.oc[i,] %*% A
}
oc1 <- rot.oc[,1]
oc2 <- rot.oc[,2]
#
wnom1 <- result.wnom$legislators$coord1D
wnom2 <- result.wnom$legislators$coord2D


###################################################
### code chunk number 174: ocwnomcomparisonfrance2
###################################################
library(MCMCpack)
oc.comp <- na.omit(cbind(oc1, oc2))
wnom.comp <- na.omit(cbind(wnom1, wnom2))
proc <- procrustes(X=wnom.comp, Xstar=oc.comp, translation=FALSE,
    dilation=FALSE)


###################################################
### code chunk number 175: ocwnomcomparisonfrance3
###################################################
wnom1.new <- proc$X.new[,1]
wnom2.new <- proc$X.new[,2]
party <- rc$legis.data$PAR[!is.na(wnom1)]
plot.dat <- data.frame(
    coord1D = c(wnom1.new, oc.comp[,1]),
    coord2D = c(wnom2.new, oc.comp[,2]),
    party = rep(party, 2),
    model = factor(rep(c(1,2), each=length(wnom1.new)),
        labels=c("W-NOMINATE", "OC"))
)
plot.dat$party <- factor(plot.dat$party,
    levels=c(1,2,5,7), labels=c(
        "Communist", "Socialist", "Christian Dem", "Poujadist"))
plot.dat <- na.omit(plot.dat)


###################################################
### code chunk number 176: plotocwnomcomparisonfrance1
###################################################
ggplot(plot.dat, aes(x=coord1D, y=coord2D,
                     colour=party, shape=party)) +
    geom_point(size=4) +
    facet_wrap(~model) +
    scale_shape_manual(values=c("C", "S", "D", "P"),
        labels= c("Communist", "Socialist",
                  "Christian Dem", "Poujadists"),
        name = "Party") +
    scale_color_manual(values=gray.colors(5),
        labels= c("Communist", "Socialist",
                  "Christian Dem", "Poujadists"),
        name = "Party") +
    theme_bw() +
    theme(aspect.ratio=1, legend.position="bottom") +
    coord_fixed() +
    labs(x="First Dimension", y="Second Dimension")


###################################################
### code chunk number 177: compareidealpt1
###################################################
rm(list=ls(all=TRUE))
#
data("hr111")


###################################################
### code chunk number 178: compareidealpt2
###################################################
party <- hr111$legis.data$partyCode


###################################################
### code chunk number 179: compareidealpt4
###################################################
wnom.result <- wnominate(hr111, ubeta=15, uweights=0.5, dims=2,
    minvotes=20, lop=0.025, trials=1, polarity=c(2,2),
    verbose=FALSE)
oc.result <- oc(hr111, dims=2, minvotes=20, lop=0.025,
    polarity=c(2,2), verbose=FALSE)
weight <-  wnom.result$weights[2]/wnom.result$weights[1]

plot.dat <- data.frame(
    wnom = c(wnom.result$legislators$coord1D,
             wnom.result$legislators$coord2D*weight),
    oc = c(oc.result$legislators[,"coord1D"],
           oc.result$legislators[,"coord2D"]),
    dimension = factor(rep(c(1,2), each =
        nrow(oc.result$legislators)),
        labels=c("First Dimension", "Second Dimension")),
    party = party)
plot.dat$party <- factor(plot.dat$party, levels=c(100, 200),
    labels=c("Democrats", "Republicans"))
plot.dat <- na.omit(plot.dat)


###################################################
### code chunk number 180: plotcomparewnoc
###################################################
ggplot(plot.dat, aes(x=wnom, y=oc)) +
      geom_point(pch=1) +
    facet_grid(party ~ dimension) +
    theme_bw() +
    theme(aspect.ratio=1) +
    xlab("W-NOMINATE Score") +
    ylab("OC Score")


###################################################
### code chunk number 181: comparewnoc
###################################################
by(plot.dat, list(plot.dat$party, plot.dat$dimension),
   function(d)cor(d$oc, d$wnom))


###################################################
### code chunk number 182: bam_file (eval = FALSE)
###################################################
## system.file("templates/BAM_JAGScode.bug", package="asmcjr")


###################################################
### code chunk number 183: libs
###################################################
library(basicspace)


###################################################
### code chunk number 184: BAM1
###################################################
data(franceEES2009)
MLE_result.france <- aldmck(franceEES2009, polarity=2, respondent=1,
     missing=c(77,88,89), verbose=FALSE)
bamdata <- bamPrep(franceEES2009,
    missing=c(77,88,89), self=1)


###################################################
### code chunk number 185: BAM2
###################################################
#Aldrich-Mckelvey scaling, with cutoff of 5 or more
bamdata <- bamPrep(franceEES2009,
    missing=c(77,88,89), self=1, nmin=5)


###################################################
### code chunk number 186: BAM3 (eval = FALSE)
###################################################
## bam.france <- BAM(bamdata, polarity=2, n.adapt=2500, n.sample=5000,
##       zhat=TRUE, ab=TRUE, resp.idealpts=TRUE)


###################################################
### code chunk number 187: BAM3load
###################################################
load("bam.france.rda")


###################################################
### code chunk number 188: BAM4
###################################################
bam.france$zhat.ci


###################################################
### code chunk number 189: BAM6
###################################################
diffStims(bam.france$zhat, stims=c(3,4))


###################################################
### code chunk number 190: BAM_Socialist_Green_Parties_facade
###################################################
soc_samples <- do.call("c", bam.france$zhat[,3])
greens_samples <- do.call("c", bam.france$zhat[,4])
df <- data.frame(x = c(soc_samples, greens_samples),
      stimuli = factor(rep(c(1,2), each=length(soc_samples)),
                labels=c("Socialists", "Greens")))
ggplot(df, aes(x=x, group=stimuli, color=stimuli)) +
  geom_line(stat="density") + xlim(range(df$x)) +
  scale_color_manual(values=gray.palette(2)) +
  xlab("Left-Right") + theme_bw() +
  theme(legend.text=element_text(size=12),
        legend.position="bottom", aspect.ratio=1) +
  guides(shape = guide_legend(override.aes = list(size = 4)))


###################################################
### code chunk number 191: BAM_Socialist_Green_Parties
###################################################
soc_samples <- do.call("c", bam.france$zhat[,3])
greens_samples <- do.call("c", bam.france$zhat[,4])
df <- data.frame(x = c(soc_samples, greens_samples),
      stimuli = factor(rep(c(1,2), each=length(soc_samples)),
                labels=c("Socialists", "Greens")))
ggplot(df, aes(x=x, group=stimuli, color=stimuli)) +
  geom_line(stat="density") + xlim(range(df$x)) +
  scale_color_manual(values=gray.palette(2)) +
  xlab("Left-Right") + theme_bw() +
  theme(legend.text=element_text(size=12),
        legend.position="bottom", aspect.ratio=1) +
  guides(shape = guide_legend(override.aes = list(size = 4)))



###################################################
### code chunk number 192: AM_BAM_point_comparison
###################################################
#Comparison of estimated stimuli locations
df2 <- data.frame(MLE = MLE_result.france$stimuli,
    BAM = bam.france$zhat.ci$idealpt,
    stimname = names(MLE_result.france$stimuli))
ggplot(df2, aes(x=MLE, y=BAM)) +
  geom_point(pch=16, cex=2)  +
  theme_bw() +
  geom_text(aes(label=stimname),
     nudge_x=c(-0.204, 0.180, -0.144, 0.120,
               -0.204, 0.228, -0.228, -0.156)) +
  geom_smooth(method="lm", se=FALSE, fullrange=TRUE,
              color="black", lwd=.5) + xlim(-.8,1)


###################################################
### code chunk number 193: boot.france.aldmck2
###################################################
boot.france  <- boot.aldmck(franceEES2009,
        polarity=2, respondent=1, missing=c(77,88,89),
        verbose=FALSE, boot.args = list(R=100))
result.france <- aldmck(franceEES2009, respondent=1, polarity=2,
    missing=c(77,88,89), verbose=FALSE)


###################################################
### code chunk number 194: BAM8
###################################################
zhat.samples <- do.call("rbind", bam.france$zhat)
samples.france <- zhat.samples[,
    match(names(MLE_result.france$stimuli), colnames(zhat.samples))]
samples.scale.france <- apply(zhat.samples, 1, scale)
bayes.se.france <- apply(samples.scale.france, 1, sd)
boot.scale.france <- t(apply(boot.france$b$t, 1, scale))
# Extracting standard errors
# Since we're using normal theory confidence intervals,
# we first scale the bootstrapped stimuli and then
# get the square root of the variances.
boot.se.france <- sqrt(diag(var(boot.scale.france)))
df3 <- data.frame(stimname = names(MLE_result.france$stimuli),
                  BAM = bayes.se.france, Boot = boot.se.france
      )


###################################################
### code chunk number 195: AM_BAM_SE_comparison
###################################################
#Bootstrapped standard errors plot syntax
ggplot(df3, aes(x=Boot, y=BAM)) +
  geom_point() +
  theme_bw() +
  xlim(.01, .023) +
  ylim(.01,.023) +
  geom_abline(slope=1, intercept=0) +
  geom_text(aes(label=stimname),
      nudge_x=c(.0015,-.00125, -0.001,-.001, .0012,.001,0,.0012),
      nudge_y=c(.0003, -.0003, 0,0,0,-.0003,-.0003,0))


###################################################
### code chunk number 196: BAM9
###################################################
df3$ASE <- aldmckSE(result.france, franceEES2009)
#Standard error comparisons
print(df3)


###################################################
### code chunk number 197: getJAGSfile (eval = FALSE)
###################################################
## system.file("templates/BMDS_JAGS_model.bug", package="asmcjr")


###################################################
### code chunk number 198: jags_nations_1
###################################################
data(nations)


###################################################
### code chunk number 199: jags_z
###################################################
z <- matrix(NA, nrow=nrow(nations), ncol=2)
z[10, ] <- 0
z[11,2] <- 0


###################################################
### code chunk number 200: jags_nations_2
###################################################
nations.bmds <- BMDS(nations, posStims=c(7,2), negStims=c(9,8), z=z,
        fname="nations_jags.txt", n.sample=10000, n.adapt=10000)


###################################################
### code chunk number 201: jags_nations_plot_1
###################################################
jags.df <- data.frame(
  x = nations.bmds$zhat.ci$idealpt[1:12],
  y = nations.bmds$zhat.ci$idealpt[13:24],
  country = rownames(nations)
  )
ggplot(jags.df, aes(x=x, y=y, group=country)) +
  geom_point() +
  geom_text(aes(label=country),
            nudge_y=c(rep(-.15,11), .175))+
  theme_bw() +
  xlab("First Dimension") +
  ylab("Second Dimension") +
  theme(aspect.ratio=1) +
  xlim(-3,6) +
  ylim(-3,6)


###################################################
### code chunk number 202: jags_nations_plot_2
###################################################
samples <- do.call(rbind, nations.bmds$zhat)
dens.df <- data.frame(
  dim1 = c(samples[,2:3]),
  country = rep(c("Congo", "Cuba"), each=nrow(samples)))
ggplot(dens.df, aes(x=dim1, group=country)) +
  geom_line(aes(color=country, lty=country), stat="density") +
  scale_color_manual(values=gray.palette(2)) +
  theme_bw() +
  xlab("First Dimension") +
  theme(aspect.ratio=1, legend.position="bottom") +
  guides(shape = guide_legend(override.aes = list(size = 4)))


###################################################
### code chunk number 203: BayesianMU2
###################################################
data("ANES1968", package="asmcjr")
anes.input <- ANES1968[,1:12]
anes.input <- as.matrix(anes.input)
vote.turnout <- ANES1968[,13]
print(anes.input[1:5,1:6])


###################################################
### code chunk number 204: bunfold (eval = FALSE)
###################################################
## result <- bayesunfold(anes.input, dims=2,
##     burnin=1000, nsamp=2000)


###################################################
### code chunk number 205: bunfold_load
###################################################
load("bayes_unfold_result_102619.rda")


###################################################
### code chunk number 206: Bayesian_unfolding_SMACOF_NES1968
###################################################
row.dat <- as.data.frame(result$smacof.result$conf.row)
col.dat <- as.data.frame(result$smacof.result$conf.col)
col.dat$candidate <- factor(1:nrow(col.dat), labels=rownames(col.dat))
col.dat <- col.dat %>% filter(candidate %in%
  c("Humphrey", "Wallace", "Nixon"))
row.dat$vote <- NA
row.dat$vote  <- factor(ANES1968[,14], levels=1:3,
    labels=c("Humphrey", "Nixon", "Wallace"))[result$retained.obs]
ggplot(na.omit(row.dat), aes(x=D1, y=D2)) +
    geom_point(aes(shape=vote, colour=vote), size=3) +
    scale_shape_manual(values=c("H", "N", "W"),
        labels=c("Humphrey", "Nixon", "Wallace"),
        name="Vote") +
    scale_colour_manual(values=c("gray50", "gray65", "gray80"),
        labels=c("Humphrey", "Nixon", "Wallace"),
        name = "Vote") +
    geom_text(data = col.dat, aes(label=candidate),
        size=5, color="black") +
    theme_bw() +
    theme(aspect.ratio=1, legend.position="bottom") +
    xlab("First Dimension") +
    ylab("Second Dimension")


###################################################
### code chunk number 207: BayesianMUdolbfgs1
###################################################
ind.dat <- as.data.frame(result$lbfgs.result$individuals)
names(ind.dat) <- c("D1", "D2")
ind.dat$vote <- NA
ind.dat$vote <- factor(ANES1968[,14], levels=1:3,
    labels=c("Humphrey", "Nixon", "Wallace"))[result$retained.obs]
stim.dat <- as.data.frame(result$lbfgs.result$stimuli)
names(stim.dat) <- c("D1", "D2")
stim.dat$candidate <- factor(1:nrow(stim.dat),
    labels=rownames(result$unrotated$stimuli$mean))
ind.dat <- na.omit(ind.dat)
stim.dat <- stim.dat %>% filter(candidate %in%
  c("Humphrey", "Nixon", "Wallace"))
ggplot(ind.dat, aes(x=D1, y=D2)) +
    geom_point(aes(shape=vote, colour=vote), size=2) +
    scale_shape_manual(values=c("H", "N", "W"),
        labels=c("Humphrey", "Nixon", "Wallace"),
        name="Vote") +
    scale_color_manual(values=c("gray50", "gray65", "gray80"),
        labels=c("Humphrey", "Nixon", "Wallace"),
        name="Vote") +
    geom_text(data = stim.dat, aes(label=candidate),
        size=6, color="black") +
    theme_bw() +
    theme(aspect.ratio=1, legend.position="bottom") +
    xlab("First Dimension") +
    ylab("Second Dimension")


###################################################
### code chunk number 208: Bayesian_unfolding_BayesianSlice_NES1968
###################################################
ind.dat <- as.data.frame(result$rotated$individuals$mean)
names(ind.dat) <- c("D1", "D2")
ind.dat$vote <- NA
ind.dat$vote <- factor(ANES1968[,14], levels=1:3,
    labels=c("Humphrey", "Nixon", "Wallace"))[result$retained.obs]
stim.dat <- as.data.frame(result$rotated$stimuli$mean)
names(stim.dat) <- c("D1", "D2")
stim.dat$candidate <- factor(1:nrow(stim.dat),
    labels=colnames(anes.input))
stim.dat <- stim.dat %>% filter(candidate %in%
  c("Humphrey", "Wallace", "Nixon"))
ind.dat <- na.omit(ind.dat)
ggplot(ind.dat, aes(x=D1, y=D2)) +
    geom_point(aes(shape=vote, colour=vote), size=2) +
    scale_shape_manual(values=c("H", "N", "W"),
        labels=c("Humphrey", "Nixon", "Wallace"),
        name="Vote") +
    scale_color_manual(values=c("gray50", "gray65", "gray80"),
        labels=c("Humphrey", "Nixon", "Wallace"),
        name="Vote") +
    geom_text(data = stim.dat, aes(label=candidate),
        size=6, color="black") +
    theme_bw() +
    theme(aspect.ratio=1, legend.position="bottom") +
    xlab("First Dimension") +
    ylab("Second Dimension")


###################################################
### code chunk number 209: bayes_chapter.Rnw:699-711 (eval = FALSE)
###################################################
## ## Difference between Main Candidates
## stim.mat <- as.matrix(result$rotated$stim.samples)
## d <- stim.mat[,3] -  #Nixon
##      stim.mat[,2] #Humphrey
## md1 <- mean(d > 0)
## d <- stim.mat[,1] -  #Wallace
##      stim.mat[,2] #Humphrey
## md2 <- mean(d > 0)
## d <- stim.mat[,1] -  #Wallace
##      stim.mat[,3] #Nixon
## md3 <- mean(d > 0)
## c(md1, md2, md3)


###################################################
### code chunk number 210: BayesianMU10
###################################################
h.df <- as.data.frame(
  as.matrix(result$rotated$stim.samples)[,c(2,14)])
n.df <- as.data.frame(
  as.matrix(result$rotated$stim.samples)[,c(3,15)])
w.df <- as.data.frame(
  as.matrix(result$rotated$stim.samples)[,c(1,13)])
names(h.df) <- names(w.df) <- names(n.df) <- c("D1", "D2")
h.df$candidate = factor(1, levels=1:3,
                        labels=c("Humphrey", "Nixon", "Wallace"))
n.df$candidate = factor(2, levels=1:3,
                        labels=c("Humphrey", "Nixon", "Wallace"))
w.df$candidate = factor(3, levels=1:3,
                        labels=c("Humphrey", "Nixon", "Wallace"))
plot.df <- rbind(h.df, n.df, w.df)
z.df <- data.frame(
    x=result$rotated$stimuli$mean[,1],
    xl=result$rotated$stimuli$lower[,1],
    xu=result$rotated$stimuli$upper[,1],
    y=result$rotated$stimuli$mean[,2],
    yl=result$rotated$stimuli$lower[,2],
    yu=result$rotated$stimuli$upper[,2])
z.df$candidate <- factor(1:12, labels=colnames(anes.input))
z.df$short <- substr(as.character(z.df$candidate), 1, 3)
z.df <- z.df %>% filter(candidate %in%
  c("Humphrey", "Nixon", "Wallace"))
ggplot(plot.df, aes(group=candidate)) +
  geom_density_2d(aes(x = D1, y = D2), bins=4, color="gray75") +
  geom_segment(data=z.df, aes(x=xl, xend=xu, y=y, yend=y)) +
  geom_segment(data=z.df, aes(x=x, xend=x, y=yl, yend=yu)) +
  geom_text(data=z.df, aes(x=c(-.08, .4, 1), y=c(-0.1, -0.52, 0.3)),
            label=c("Humphrey", "Nixon", "Wallace")) +
  theme_bw() +
  theme(aspect.ratio=1) +
  labs(x="First Dimension",
       y="Second Dimension")



###################################################
### code chunk number 211: Bayesian_unfolding_BayesianSlice_NES1968_uncertainty
###################################################
ci.df <- with(result$rotated, data.frame(
  diffs = c(stimuli$upper[,1] - stimuli$lower[,1],
         individuals$upper[,1] - individuals$lower[,1],
         stimuli$upper[,2] - stimuli$lower[,2],
         individuals$upper[,2] - individuals$lower[,2]),
  dimension = factor(rep(c(1,2), each=nrow(stimuli$mean) + nrow
         (individuals$mean)), labels=c("D1", "D2")),
  object = factor(rep(rep(c(1,2),
                  c(nrow(stimuli$mean), nrow(individuals$mean))), 2),
              labels=c("Stimuli", "Individuals"))
  )
)

ggplot(ci.df, aes(x=diffs, y=..density..)) +
    stat_density(geom="line") +
    facet_grid(object~dimension, scale="free_y") +
    theme_bw() +
    labs(x="Width of 90% Credible Intervals", y="Density")


###################################################
### code chunk number 212: BayesianMU11
###################################################
anes.input <- anes.input[which(result$retained.obs), ]
print(anes.input[6,])


###################################################
### code chunk number 213: BayesianMU3dplotRespondent6
###################################################
r6.df <- as.data.frame(as.matrix(result$rotated$indiv.samples)[,
    c(6, sum(result$retained.obs)+6)])
names(r6.df) <- c("D1", "D2")
r6.df$observation <- factor(1, levels=c(1,2),
  labels=c("Respondent 6", "Respondent 730"))
r730.df <- as.data.frame(as.matrix(result$rotated$indiv.samples)[,
    c(730, sum(result$retained.obs)+730)])
names(r730.df) <- c("D1", "D2")
r730.df$observation <- factor(2, levels=c(1,2),
  labels=c("Respondent 6", "Respondent 730"))
r6.df <- rbind(r6.df, r730.df)
ggplot(r6.df, aes(x = D1, y = D2)) +
  geom_density_2d(color="gray25") +
  facet_wrap(~observation) +
  geom_text(data = stim.dat, aes(label=candidate),
      size=4, color="black") +
  xlim(-.5, 1.5) +
  theme_bw() +
  theme(aspect.ratio=1) +
  labs(x="First Dimension",
       y="Second Dimension")


###################################################
### code chunk number 214: unidimensionalIRT1
###################################################
library(MCMCpack)
data(SupremeCourt)
SupCourt2000 <- t(SupremeCourt)
print(SupCourt2000[1:6,1:6])


###################################################
### code chunk number 215: unidimensionalIRT2 (eval = FALSE)
###################################################
## posterior1d.unid <- MCMCirt1d(SupCourt2000,
##     theta.constraints=list(Scalia="+"),
##     mcmc=55000, burnin=5000, thin=10,
##     theta.start=NA, alpha.start=NA, beta.start=NA,
##     t0=0, T0=1, a0=0, A0=0.25, b0=0, B0=0.25,
##     seed=NA, verbose=0, store.item=TRUE,
##     store.ability=TRUE, drop.constant.items=TRUE)


###################################################
### code chunk number 216: load_sc_uni_irt
###################################################
load("sc_uni_irt.rda")


###################################################
### code chunk number 217: post_std1
###################################################
justices <- posterior1d.unid[,
  grep("^theta", colnames(posterior1d.unid))]
justicesZ <- apply(justices, 1, scale)
justices_sd <- apply(justices, 1, sd)
justices_mean <- apply(justices, 1, mean)


###################################################
### code chunk number 218: unidimensionalIRT4a
###################################################
alphas <- posterior1d.unid[,
  grep("^alpha", colnames(posterior1d.unid))]
betas <- posterior1d.unid[,
  grep("^beta", colnames(posterior1d.unid))]
disc.parameters <- apply(betas, 2, function(x)x*justices_sd)
diff.parameters <- apply(betas, 2, function(x)x*justices_mean) +
  alphas


###################################################
### code chunk number 219: unidimensionalIRT4
###################################################
idealpt.means <- colMeans(justices)
diff.means <- colMeans(diff.parameters)
disc.means <- colMeans(disc.parameters)


###################################################
### code chunk number 220: plotIRTjustices1
###################################################
sc.df <- data.frame(
    ideal.pt = c(t(justicesZ)),
    justice = factor(rep(1:9, each=nrow(justices)),
                     labels=colnames(SupremeCourt))
)
sc.df <- sc.df[which(sc.df$justice %in%
  c("Stevens", "O'Connor", "Rehnquist", "Scalia")), ]
dens <- by(sc.df$ideal.pt, list(sc.df$justice), density)
x <- c(unlist(sapply(dens, function(z)z$x)))
y <- c(unlist(sapply(dens, function(z)z$y)))
dens.df <- data.frame(x=x, y=y, justice = factor(rep(1:4, each=512),
    labels=colnames(SupremeCourt)[1:4])
)
ggplot(dens.df) +
  geom_line(aes(x=x,y=y, linetype=justice), size=.75) +
  theme_bw() +
  theme(aspect.ratio=1, legend.position="bottom") +
  xlim(-2,3) +
  labs(x="Ideology (Liberal - Conservative)",
       y = "Posterior Density", linetype="Justice") +
  guides(linetype=guide_legend(nrow=2))


###################################################
### code chunk number 221: plotIRTjustices2
###################################################
cl.df <- lapply(1:4, function(x)data.frame(cutline = diff.means/disc.means))
for(i in 1:4){
  cl.df[[i]]$justice <- factor(i, levels=1:9,
                               labels=levels(sc.df$justice))
}
cl.df <- do.call(rbind, cl.df)
cl.df <- cl.df %>% filter(cutline > -4)
ggplot(sc.df) +
  geom_vline(data=cl.df, aes(xintercept=cutline), size=.035,
             color="black") +
  geom_density(aes(x=ideal.pt), fill="gray50", linetype=0, alpha=.5) +
  facet_wrap(~justice, scales="free_y") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank()) +
  labs(x="Ideology (Liberal - Conservative)",
       y = "Posterior Density") +
  theme(aspect.ratio=1)


###################################################
### code chunk number 222: plotIRTjustices1b
###################################################
rg <- range(c(justicesZ))
s <- seq(rg[1], rg[2], length=100)
probs <- pnorm(cbind(1, s) %*% rbind(diff.means, disc.means))
icc.df <- data.frame(
  probs = c(probs),
  s = rep(s, 43),
  indicator = as.factor(rep(1:43, each=100))
)
ggplot(icc.df, aes(x=s, y=probs, group=indicator)) +
  geom_line() +
  theme_bw() + theme(aspect.ratio=1) +
  labs(x="Ideology (Liberal - Conservative)",
       y = "Posterior Probability of Vote")


###################################################
### code chunk number 223: idealSupCourt1
###################################################
library(pscl)
SC.rc <- rollcall(SupCourt2000, legis.names=rownames(SupCourt2000))
ideal.SupCourt <- ideal(SC.rc,
    dropList=list(codes="notinLegis",lop=0,legisMin=20),
    d=1, maxiter=10000, thin=100, burnin=500,
    impute=FALSE, normalize=TRUE, priors = NULL, startvals="eigen",
    store.item=TRUE, file=NULL, verbose=FALSE)


###################################################
### code chunk number 224: idealSupCourt2
###################################################
unidentified.result <- ideal(SC.rc, d=1, maxiter=10000,
    thin=100, burnin=500, normalize=FALSE)
identified.result <- postProcess(unidentified.result,
    constraints=list(Stevens=-1,Scalia=1))


###################################################
### code chunk number 225: plotIDEALjustices1
###################################################
plot(ideal.SupCourt, conf.int=0.95)


###################################################
### code chunk number 226: IRTmultchains1
###################################################
theta <- runif(9, min=-2, max=2)
theta[4] <- abs(theta[4])


###################################################
### code chunk number 227: IRTmultchains2 (eval = FALSE)
###################################################
## posterior1d.unid.2 <- MCMCirt1d(SupCourt2000,
##     theta.constraints=list(Scalia="+"),
##     mcmc=55000, burnin=5000, thin=10,
##     theta.start=theta, alpha.start=rnorm(1), beta.start=rnorm(1),
##     t0=0, T0=1, a0=0, A0=0.25, b0=0, B0=0.25,
##     seed=23456, verbose=0, store.item=TRUE,
##     store.ability=TRUE, drop.constant.items=TRUE)
## posterior1d.2 <- t(apply(posterior1d.unid.2, 1, scale))


###################################################
### code chunk number 228: load_sc_uni_irt_2
###################################################
load("sc_uni_irt2.rda")


###################################################
### code chunk number 229: IRTmultchains3
###################################################
posterior1d <- as.mcmc(posterior1d)
colnames(posterior1d) <- colnames(posterior1d.unid)
posterior1d.2 <- as.mcmc(posterior1d.2)
colnames(posterior1d.2) <- colnames(posterior1d.unid.2)
chains <- mcmc.list(posterior1d,posterior1d.2)


###################################################
### code chunk number 230: IRTmultchains4
###################################################
full.posterior1d <- rbind(chains[[1]],chains[[2]])
nlegislators <- nrow(SupCourt2000)
legislators <- full.posterior1d[,1:nlegislators]
rollcalls <- full.posterior1d[,
  (nlegislators+1):ncol(full.posterior1d)]


###################################################
### code chunk number 231: IDEALmultchains5
###################################################
ideal.SupCourt1 <- ideal(SC.rc, d=1, normalize=TRUE,
    store.item=TRUE, startvals="eigen")
ideal.SupCourt2 <- ideal(SC.rc, d=1, normalize=TRUE,
    store.item=TRUE, startvals="random")


###################################################
### code chunk number 232: IDEALmultchains6
###################################################
ideal.chains.legislators <- mcmc.list(
  as.mcmc(ideal.SupCourt1$x[,,1]),
  as.mcmc(ideal.SupCourt2$x[,,1]))
ideal.chains.rollcalls <- mcmc.list(
  as.mcmc(ideal.SupCourt1$beta[,,1]),
  as.mcmc(ideal.SupCourt2$beta[,,1]))


###################################################
### code chunk number 233: unidimensionalIRT21
###################################################
rm(list=ls(all=TRUE))
library(MCMCpack)


###################################################
### code chunk number 234: unidimensionalIRT22
###################################################
library(pscl)
hr <- readKH("sen100kh.ord",
       dtl=NULL,
       yea=c(1,2,3),
       nay=c(4,5,6),
       missing=c(7,8,9),
       notInLegis=0,
       desc="100th Senate Roll Call Data",
       debug=FALSE)


###################################################
### code chunk number 235: unidimensionalIRT23
###################################################
dat <- hr$votes
dat[dat==7 | dat==8 | dat==9 | dat==0] <- NA
dat[dat==1 | dat==2 | dat==3] <- 1
dat[dat==4 | dat==5 | dat==6] <- 0
colnames(dat) <- gsub("\\s","",colnames(dat))
rownames(dat) <- gsub("\\s","",rownames(dat))
rownames(dat) <- gsub("\\(","",rownames(dat))
rownames(dat) <- gsub("\\)","",rownames(dat))
rownames(dat) <- gsub("\\-","",rownames(dat))


###################################################
### code chunk number 236: unidimensionalIRT24 (eval = FALSE)
###################################################
## posterior1d.unid <- MCMCirt1d(dat,
##     theta.constraints=list(HELMSRNC="+",KENNEDYDMA="-"),
##     mcmc=55000, burnin=5000, thin=10,
##     theta.start=NA, alpha.start=NA, beta.start=NA,
##     t0=0, T0=1, a0=0, A0=0.25, b0=0, B0=0.25,
##     seed=NA, verbose=0, store.item=TRUE,
##     store.ability=TRUE, drop.constant.items=TRUE)
## posterior1d <- t(apply(posterior1d.unid, 1, scale))
## colnames(posterior1d) <- colnames(posterior1d.unid)


###################################################
### code chunk number 237: load_sen100_irt
###################################################
load("sen100_irt1d.rda")


###################################################
### code chunk number 238: unidimensionalIRT25
###################################################
nlegislators <- nrow(dat)
legislators <- posterior1d[,1:nlegislators]
rollcalls <- posterior1d[,(nlegislators+1):ncol(posterior1d)]


###################################################
### code chunk number 239: unidimensionalIRT26
###################################################
nrollcall <- 348
diff.mean <- mean(rollcalls[,paste("alpha.Vote",nrollcall,sep="")])
disc.mean <- mean(rollcalls[,paste("beta.Vote",nrollcall,sep="")])


###################################################
### code chunk number 240: unidimensionalIRT27
###################################################
idealpt.mean <- colMeans(legislators)
party <- hr$legis.data$partyCode
state <- hr$legis.data$icpsrState
vote <- as.integer(dat[,nrollcall])


###################################################
### code chunk number 241: unidimensionalIRT28
###################################################
kpyea <- sum(vote==1, na.rm=T)
kpnay <- sum(vote==0, na.rm=T)
errors1 <- !is.na(vote) & vote==1 &
  (idealpt.mean < (diff.mean/disc.mean))
errors2 <- !is.na(vote) & vote==0 &
  (idealpt.mean > (diff.mean/disc.mean))
errors3 <- !is.na(vote) & vote==1 &
  (idealpt.mean > (diff.mean/disc.mean))
errors4 <- !is.na(vote) & vote==0 &
  (idealpt.mean < (diff.mean/disc.mean))
errors12 <- sum(errors1==1) + sum(errors2==1)
errors34 <- sum(errors3==1) + sum(errors4==1)
nerrors <- min(errors12, errors34)
PRE <- (min(kpyea,kpnay) - nerrors)/min(kpyea,kpnay)


###################################################
### code chunk number 242: unidimensionalIRT210
###################################################
x <- seq(-3, 3, by=0.01)
y <- pnorm((disc.mean * x) - diff.mean)


###################################################
### code chunk number 243: plotIRTbork1
###################################################
bork.df <- data.frame(x=x, y=y)
vote.df <- data.frame(
  vote=factor(vote, levels=c(0,1), labels=c("Nay", "Yea")),
  vote_num=vote,
  partyid=factor(party, levels=c(100,200), labels=c("D", "R")),
  ip=idealpt.mean)

ggplot(bork.df) + geom_line(aes(x=x,y=y)) +
  geom_point(data=vote.df, aes(x=ip, y=vote_num, shape=partyid,
             colour=partyid), size=3,
             position=position_jitter(height=.05, width=0)) +
  scale_shape_manual(values=c("D", "R"), name="Party",
                     labels=c("Democrat", "Republican")) +
  scale_colour_manual(values=c("gray25", "gray75"), name="Party",
                      labels=c("Democrat", "Republican")) +
  geom_vline(xintercept=diff.mean/disc.mean, col="gray50",
             lty=2, size=.75) +
  theme_bw() + theme(aspect.ratio=1, legend.position="bottom") +
  labs(x="Ideology (Liberal - Conservative)",
       y="Vote Choice/Pr(Vote Choice)") +
  xlim(c(-1.1, 2.1))


###################################################
### code chunk number 244: unidimensionalIRT212
###################################################
disc.parameters <- rollcalls[,seq(2,ncol(rollcalls), by=2)]
print(sort(colMeans(abs(disc.parameters)))[1:4])
print(sort(colMeans(abs(disc.parameters)))[
  (ncol(disc.parameters)-3):ncol(disc.parameters)])


###################################################
### code chunk number 245: IRTconvergenceplot1
###################################################
reagan <- as.mcmc(legislators[,"theta.REAGANRUSA"])
plot(reagan)


###################################################
### code chunk number 246: geweke_diag_stat
###################################################
gew <- abs(geweke.diag(legislators)$z)


###################################################
### code chunk number 247: IRTconvergence1
###################################################
sort(abs(geweke.diag(legislators)$z), decreasing=T)[1:2]


###################################################
### code chunk number 248: IRTconvergenceplot2
###################################################
inouye <- as.mcmc(legislators[,"theta.INOUYEDHI"])
geweke.ggplot(inouye) +
  theme_bw()  +
  guides(colour=FALSE)



###################################################
### code chunk number 249: IRTgelmanrubin1 (eval = FALSE)
###################################################
## # This runs in the background and is necessary because we previously cleared everything.
## library(MCMCpack)
## data(SupremeCourt)
## SupCourt2000 <- t(SupremeCourt)
## posterior1d.unid <- MCMCirt1d(SupCourt2000,
##     theta.constraints=list(Scalia="+"),
##     mcmc=25000, burnin=200000, thin=25,
##     theta.start=NA, alpha.start=NA, beta.start=NA,
##     t0=0, T0=1, a0=0, A0=0.25, b0=0, B0=0.25,
##     seed=NA, verbose=0, store.item=TRUE,
##     store.ability=TRUE, drop.constant.items=TRUE)
## theta <- rnorm(9,0,1)
## theta[4] <- abs(theta[4])
## posterior1d.unid.2 <- MCMCirt1d(SupCourt2000,
##     theta.constraints=list(Scalia="+"),
##     mcmc=25000, burnin=200000, thin=25,
##     theta.start=theta, alpha.start=0, beta.start=0,
##     t0=0, T0=1, a0=0, A0=0.25, b0=0, B0=0.25,
##     seed=67890, verbose=0, store.item=TRUE,
##     store.ability=TRUE, drop.constant.items=TRUE)


###################################################
### code chunk number 250: IRTgrchains
###################################################
load("sschains.rda")
posterior1d.unid <- chains[[1]]
posterior1d.unid.2 <- chains[[2]]


###################################################
### code chunk number 251: IRTgrchainsb
###################################################
chains <- mcmc.list(posterior1d.unid, posterior1d.unid.2)
justices <- lapply(chains, function(x)x[,grep("^theta", colnames(x))])
justicesZ <- lapply(justices, function(x)apply(x, 1, scale))
rownames(justicesZ[[1]]) <- rownames(justicesZ[[2]]) <-
  grep("^theta", colnames(posterior1d.unid), value=TRUE)
justices_sd <- lapply(justices, function(x)apply(x, 1, sd))
justices_mean <- lapply(justices, function(x)apply(x, 1, mean))

alphas <- lapply(chains, function(x)x[,grep("^alpha", colnames(x))])
betas <- lapply(chains, function(x)x[,grep("^beta", colnames(x))])
disc.parameters <- lapply(1:2, function(i)apply(betas[[i]], 2,
  function(x)x*justices_sd[[i]]))
diff.parameters <- lapply(1:2, function(i)apply(betas[[i]], 2,
  function(x)x*justices_mean[[i]]) + alphas[[i]])


###################################################
### code chunk number 252: IRTgelmanrubin2 (eval = FALSE)
###################################################
## chain1 <- cbind(t(justicesZ[[1]]), alphas[[1]], betas[[1]])
## chain2 <- cbind(t(justicesZ[[2]]), alphas[[2]], betas[[2]])
## chains <- mcmc.list(as.mcmc(chain1),as.mcmc(chain2))


###################################################
### code chunk number 253: IRTgelmanrubin3
###################################################
gr <- gelman.diag(chains, confidence=0.95, transform = FALSE,
    autoburnin = TRUE, multivariate=FALSE)
print(gr$psrf[1:2,])
which.max(gr$psrf[,2])
gr$psrf[which.max(gr$psrf[,2]),]


###################################################
### code chunk number 254: multidimensionalIRT1
###################################################
rm(list=ls(all=TRUE))
library(MCMCpack)
library(pscl)
hr <- readKH("sen89kh.ord",
       dtl=NULL,
       yea=c(1,2,3),
       nay=c(4,5,6),
       missing=c(7,8,9),
       notInLegis=0,
       desc="89th Senate Roll Call Data",
       debug=FALSE)
dat <- hr$votes
dat[dat==7 | dat==8 | dat==9 | dat==0] <- NA
dat[dat==1 | dat==2 | dat==3] <- 1
dat[dat==4 | dat==5 | dat==6] <- 0
colnames(dat) <- gsub("\\s","",colnames(dat))
rownames(dat) <- gsub("\\s","",rownames(dat))
rownames(dat) <- gsub("\\(","",rownames(dat))
rownames(dat) <- gsub("\\)","",rownames(dat))
rownames(dat) <- gsub("\\-","",rownames(dat))


###################################################
### code chunk number 255: multidimensionalIRT2 (eval = FALSE)
###################################################
## posterior2d <- MCMCirtKd(dat, dimensions=2,
##     item.constraints=list(Vote151=list(2,"-"), Vote151=c(3,0),
##     Vote78=list(3,"-")), mcmc=15000, burnin=5000, thin=10,
##     theta.start=NA, alpha.start=NA, beta.start=NA,
##     t0=0, T0=1, a0=0, A0=0.25, b0=0, B0=0.25,
##     seed=NA, verbose=0, store.item=FALSE,
##     store.ability=TRUE, drop.constant.items=TRUE)


###################################################
### code chunk number 256: load_sen89_irt2d
###################################################
load("sen89_irt2d.rda")


###################################################
### code chunk number 257: multidimensionalIRT3
###################################################
party <- hr$legis.data$partyCode
code <- hr$legis.data$icpsrLegis
state <- hr$legis.data$icpsrState
idealpt1 <- colMeans(posterior2d[,seq(1, ncol(posterior2d), by=2)])
idealpt2 <- colMeans(posterior2d[,seq(2, ncol(posterior2d), by=2)])


###################################################
### code chunk number 258: multidimensionalIRT4
###################################################
nlegislators <- nrow(dat)
idealpt1.10 <- idealpt1.90 <- idealpt2.10 <- idealpt2.90 <-
    rep(NA, nlegislators)
for (i in seq(1,ncol(posterior2d),2)){
idealpt1.10[ceiling(i/2)] <- HPDinterval(posterior2d[,i],
                                         prob=0.9)[1]
idealpt1.90[ceiling(i/2)] <- HPDinterval(posterior2d[,i],
                                         prob=0.9)[2]
idealpt2.10[ceiling(i/2)] <- HPDinterval(posterior2d[,i+1],
                                         prob=0.9)[1]
idealpt2.90[ceiling(i/2)] <- HPDinterval(posterior2d[,i+1],
                                         prob=0.9)[2]
}


###################################################
### code chunk number 259: s89df
###################################################
s89.df <- data.frame(i1=idealpt1, i2=idealpt2,
    i1.10 = idealpt1.10, i1.90 = idealpt1.90,
    i2.10 = idealpt2.10, i2.90 = idealpt2.90,
    party=party, code=code, state=state, group=NA)
s89.df$group[which(s89.df$party == 100 & s89.df$state >= 40 &
                   s89.df$state <= 51 & s89.df$code!=5009)] <- "S"
s89.df$group[which(s89.df$party == 100 & s89.df$state == 53 &
                   s89.df$code!=5009)] <- "S"
s89.df$group[which(s89.df$party == 100 & s89.df$state == 54 &
                   s89.df$code!=5009)] <- "S"
s89.df$group[which(s89.df$party == 100 & (s89.df$state <40 |
                   s89.df$state > 54))] <- "D"
s89.df$group[which(s89.df$party == 100 & s89.df$state == 52)] <- "D"
s89.df$group[which(s89.df$party == 200)] <- "R"
s89.df$group[which(is.na(s89.df$group))] <- "Johnson"


###################################################
### code chunk number 260: plotmultiIRTSenate
###################################################
ggplot(s89.df, aes(colour=group)) +
  geom_segment(aes(x=i1.10, xend=i1.90, y=i2, yend=i2)) +
  geom_segment(aes(x=i1, xend=i1, y=i2.10, yend=i2.90)) +
  annotate(geom="text", x=-.6, y=1.1, label="Johnson") +
  scale_colour_manual(
    values=c("gray80", "gray60", "gray40", "black"),
    name="Party Group",
    labels=c("Democrat", "Johnson", "Republican", "Southern Dem")) +
  theme_bw() +
  theme(aspect.ratio = 1, legend.position="bottom") +
  labs(x="Liberal - Conservative", y="Region/Race") +
  guides(colour=guide_legend(nrow=2))


###################################################
### code chunk number 261: IRTlegislatorthreshold
###################################################
cutoff <- 20
dat <- dat[rowSums(!is.na(dat)) >= 20,]


###################################################
### code chunk number 262: IRTrollcallthreshold
###################################################
lop <- function(x,y){
    lop.votes <- rep(0,ncol(x))
    for (i in 1:ncol(x)){
        if ((length(x[,i][!is.na(x[,i]) & x[,i]==1]) /
            length(x[,i][!is.na(x[,i])])) < y) lop.votes[i] <- 1
        if ((length(x[,i][!is.na(x[,i]) & x[,i]==0]) /
            length(x[,i][!is.na(x[,i])])) < y) lop.votes[i] <- 1
        }
    return(lop.votes)
    }
dat <- dat[,lop(dat,0.025)==0]


###################################################
### code chunk number 263: multidimensionalIRT6
###################################################
RFKennedy <- posterior2d[,"theta.KENNEDYDNY.1"]
TKennedy <- posterior2d[,"theta.KENNEDYDMA.1"]
nsims <- 50000
x1 <- RFKennedy[sample(1:length(RFKennedy), nsims, replace=T)]
x2 <- TKennedy[sample(1:length(TKennedy), nsims, replace=T)]
correct.order <- NULL
for (i in 1:nsims){
correct.order[i] <- x1[i] < x2[i]
}
p.correct.order <- mean(correct.order)


###################################################
### code chunk number 264: multidimensionalIRT6 (eval = FALSE)
###################################################
## RFKennedy <- posterior2d[,"theta.KENNEDYDNY.1"]
## TKennedy <- posterior2d[,"theta.KENNEDYDMA.1"]
## nsims <- 50000
## x1 <- RFKennedy[sample(1:length(RFKennedy), nsims, replace=T)]
## x2 <- TKennedy[sample(1:length(TKennedy), nsims, replace=T)]
## correct.order <- NULL
## for (i in 1:nsims){
## correct.order[i] <- x1[i] < x2[i]
## }
## p.correct.order <- mean(correct.order)


###################################################
### code chunk number 265: multidimensionalIRT7
###################################################
print(p.correct.order)


###################################################
### code chunk number 266: idealSenate1 (eval = FALSE)
###################################################
## ideal.unidentified <- ideal(hr, d=2, maxiter=55000,
##     thin=50, burnin=5000, normalize=FALSE,
##     store.item = TRUE)
## ideal.identified <- postProcess(ideal.unidentified,
##     constraints=list(MONDALE=c(-0.774,-0.188),
##     TALMADGE=c(0.266,0.794), SIMPSON=c(0.971,-0.238)))


###################################################
### code chunk number 267: load_sen89_ideal1
###################################################
load("sen89_ideal1.rda")


###################################################
### code chunk number 268: idealSenate2
###################################################
ideal1 <- ideal.identified$xbar[,1]
ideal2 <- ideal.identified$xbar[,2]


###################################################
### code chunk number 269: idealSenate3 (eval = FALSE)
###################################################
## # Not shown but does the work.
## code <- hr$legis.data$icpsrLegis
## party <- hr$legis.data$partyCode
## state <- hr$legis.data$icpsrState
## library(wnominate)
## wnom.result <- wnominate(hr, dims=2, polarity=c(2,2))
## wnom1 <- wnom.result$legislators$coord1D
## wnom2 <- wnom.result$legislators$coord2D


###################################################
### code chunk number 270: load_sen89_wnom
###################################################
load("sen89_wnom1.rda")


###################################################
### code chunk number 271: plotmultiIRTSenateIDEAL1
###################################################
s89.df <- data.frame(
  D1=c(ideal1, wnom1),
  D2 = c(ideal2, wnom2),
  party =rep(party, 2),
  code=rep(code, 2),
  state=rep(state, 2),
  method = factor(rep(c(1,2), c(length(ideal1), length(wnom1))),
                  labels=c("IDEAL", "W-NOMINATE")),
  group=NA)
s89.df$group[which(s89.df$party == 100 & s89.df$state >= 40 &
                   s89.df$state <= 51 & s89.df$code!=5009)] <- "S"
s89.df$group[which(s89.df$party == 100 & s89.df$state == 53 &
                   s89.df$code!=5009)] <- "S"
s89.df$group[which(s89.df$party == 100 & s89.df$state == 54 &
                   s89.df$code!=5009)] <- "S"
s89.df$group[which(s89.df$party == 100 & (s89.df$state <40 |
                   s89.df$state > 54))] <- "D"
s89.df$group[which(s89.df$party == 100 & s89.df$state == 52)] <- "D"
s89.df$group[which(s89.df$party == 200)] <- "R"
s89.df <- na.omit(s89.df)

ggplot(s89.df, aes(x=D1, y=D2, shape=group, colour=group)) +
  geom_point(size=3) +
  facet_wrap(~method) +
  theme_bw() +
  scale_shape_manual(values=c("D", "R", "S"), name="Party Group",
               labels=c("Democrat", "Republican", "Southern Dem")) +
  scale_colour_manual(values=c("gray75", "gray50", "gray25"),
               name="Party Group",
               labels=c("Democrat", "Republican", "Southern Dem")) +
  labs(x="Liberal - Conservative", y="Region/Race") +
  theme(legend.position="bottom", aspect.ratio=1)



###################################################
### code chunk number 272: idealSenate4
###################################################
# Voting Rights Act of 1965
print(ideal.identified$betabar["Vote 78",])
# Housing and Urban Development Act of 1965
print(ideal.identified$betabar["Vote 162",])
# Social Security Amendments of 1965
print(ideal.identified$betabar["Vote 174",])
# Immigration and Nationality Act of 1965
print(ideal.identified$betabar["Vote 232",])


###################################################
### code chunk number 273: plottwoutilityfunctions1
###################################################
norm.util <- function(x){ return(15*exp(-0.25*(0-x)^2))}
quad.util <- function(x){ return(15+15*(-0.25*(0-x)^2))}
plot(c(-3,3),c(0,20), xlab="Policy Location", ylab="Utility",
     cex.lab=1.2, bty="n", type="n")
lines(seq(-3,3,0.01), norm.util(seq(-3,3,0.01)), lwd=3, lty=1)
lines(seq(-3,3,0.01), quad.util(seq(-3,3,0.01)), lwd=3, lty=2)


###################################################
### code chunk number 274: anominate1 (eval = FALSE)
###################################################
## library(anominate)
## data(sen111)
## result <- anominate(sen111, dims=1, nsamp=2000, thin=1, burnin=1000,
##     minvotes=20, lop=0.025, polarity=2, random.starts=FALSE,
##     verbose=FALSE)


###################################################
### code chunk number 275: load_anominate
###################################################
library(anominate)
data(sen111)
load("anominate_result.rda")


###################################################
### code chunk number 276: anominate3
###################################################
summary(result)


###################################################
### code chunk number 277: anominateplot1
###################################################
par(mfrow=c(1,2))
traceplot(as.mcmc(result$alpha), ylim=c(.9,1), main="Trace of alpha")
densplot(as.mcmc(result$alpha), main="Density of alpha")


###################################################
### code chunk number 278: raw
###################################################
r <- cor(na.omit(result$wnom.result$legislators$coord1D),
    summary(result$legislators[[1]])[[1]][,1])


###################################################
### code chunk number 279: anominateplot2 (eval = FALSE)
###################################################
## plot(result, main="")


###################################################
### code chunk number 280: ordinalIRT1
###################################################
data(ANES2004)
issues <- as.matrix(ANES2004[,1:16])
presvote <- ANES2004$presvote
partyid <- ANES2004$partyid


###################################################
### code chunk number 281: ordinalIRT2 (eval = FALSE)
###################################################
## library(MCMCpack)
## result <- MCMCordfactanal(issues, factors=2,
##     lambda.constraints=list(healthinsurance=list(2,"+"),
##     healthinsurance=list(3,0), partialbirthabortion=list(3,"-")),
##     burnin=25000, mcmc=25000, thin=25,
##     l0=0, L0=0.1, store.lambda=TRUE, store.scores=TRUE)


###################################################
### code chunk number 282: load_anes_ofa
###################################################
library(MCMCpack)
load("anes_ofa.rda")


###################################################
### code chunk number 283: ordinalIRT3
###################################################
means.sds <- summary(result)[[1]][,1:2]
ideal.points <- means.sds[grepl("phi",rownames(means.sds)),]
item.params <- means.sds[grepl("Lambda",rownames(means.sds)),]


###################################################
### code chunk number 284: ordinalIRT4
###################################################
print(item.params[grep("libcon|diplomacy|govtspend|govtjobs|
    |partialbirthabortion|gunregulations|gaymarriage",
    rownames(item.params)),])


###################################################
### code chunk number 285: ordinalIRT5
###################################################
irt1.means <- ideal.points[seq(1,nrow(ideal.points), by=2),1]
irt2.means <- ideal.points[seq(2,nrow(ideal.points), by=2),1]


###################################################
### code chunk number 286: ordinalIRTplot1
###################################################
oirt.df <- data.frame(
    D1=irt1.means, D2=irt2.means,
    presvote = presvote
)
oirt.df <- na.omit(oirt.df)
scatter <- ggplot(oirt.df,
    aes(x=D1, y=D2, colour=presvote, shape=presvote)) +
  geom_point(size=3) +
  scale_colour_manual(values=c("gray70", "gray30"),
    name="Vote", labels=c("Bush", "Kerry")) +
  scale_shape_manual(values=c("B", "K"), name="Vote",
    labels=c("Bush", "Kerry")) + theme_bw() +
    theme(legend.position="bottom") +
    labs(x="Liberal - Conservative", y="Second Dimension")
empty <- ggplot()+geom_point(aes(1,1), colour="white")+
theme(axis.ticks=element_blank(),
      panel.background=element_blank(),
      axis.text.x=element_blank(), axis.text.y=element_blank(),
      axis.title.x=element_blank(), axis.title.y=element_blank())
h1 <- ggplot(oirt.df, aes(x=D1)) +
    geom_histogram(data=subset(oirt.df, presvote == "Bush"),
      fill="gray70", alpha=.8) +
    geom_histogram(data=subset(oirt.df, presvote == "Kerry"),
      fill="gray30", alpha=.5) + theme_bw() + labs(x="", y="")
h2 <- ggplot(oirt.df, aes(x=D2)) +
    geom_histogram(data=subset(oirt.df, presvote == "Bush"),
      fill="gray70", alpha=.8) +
    geom_histogram(data=subset(oirt.df, presvote == "Kerry"),
      fill="gray30", alpha=.5) + theme_bw() + labs(x="", y="") +
    coord_flip()
library(gridExtra)
grid.arrange( h1, empty, scatter, h2,  ncol=2, nrow=2,
  widths=c(4,1), heights=c(1, 4))


###################################################
### code chunk number 287: ordinalIRT6
###################################################
#install.packages('apsrtable')
library(apsrtable)
mod1 <- glm(presvote ~ partyid, family=binomial(link="probit"))
mod2 <- glm(presvote ~ irt1.means + irt2.means,
    family=binomial(link="probit"))
mod3 <- glm(presvote ~ irt1.means + irt2.means + partyid,
    family=binomial(link="probit"))
apsrtable(mod1, mod2, mod3, Sweave=T)


###################################################
### code chunk number 288: ordinalIRTplot2 (eval = FALSE)
###################################################
## library(ggeffects)
## party <- NA
## party[partyid<=3] <- "Democratic"
## party[partyid==4] <- "Independent"
## party[partyid>=5] <- "Republican"
## party <- as.factor(party)
## mod <- glm(presvote ~ irt1.means*party,
##            family=binomial(link="probit"))
## ggp <- ggpredict(mod, c("irt1.means [n=25]", "party"))
## ggp %>%
##   ggplot(aes(x=x, y=predicted, ymin = conf.low, ymax=conf.high)) +
##   geom_line() +
##   geom_ribbon(alpha=.25) +
##   facet_wrap(~group) +
##   labs(x="First Dimension IRT Score (Liberal - Conservative)",
##     y="Probability of Kerry Vote") +
##   theme(aspect.ratio=1) +
##   theme_bw()


###################################################
### code chunk number 289: man_in
###################################################
data(uk)
libs <- list()
cons <- list()
libs[[1]] <- "per105"
cons[[1]] <- "per104"
libs[[2]] <- "per107"
cons[[2]] <- "per109"
libs[[3]] <- "per204"
cons[[3]] <- "per203"
libs[[4]] <- "per302"
cons[[4]] <- "per301"
libs[[5]] <- "per406"
cons[[5]] <- "per407"
libs[[6]] <- "per504"
cons[[6]] <- "per505"
libs[[7]] <- "per506"
cons[[7]] <- "per507"
libs[[8]] <- "per602"
cons[[8]] <- "per601"
libs[[9]] <- "per604"
cons[[9]] <- "per603"
libs[[10]] <- "per607"
cons[[10]] <- "per608"
libs[[11]] <- "per701"
cons[[11]] <- "per702"
libs[[12]] <- c("per403", "per404", "per412", "per413")
cons[[12]] <- c("per401", "per402", "per414")

n <- sapply(1:12, function(i)round(
  (rowSums(uk[, c(libs[[i]], cons[[i]])])/100)*uk$total))
X <- sapply(1:12, function(i)round(
  rowSums(as.matrix(uk[,cons[[i]]]))))

X[which(n ==0, arr.ind=T)] <- NA
n[which(n == 0, arr.ind=T)] <- 10


###################################################
### code chunk number 290: run_uk_rwp (eval = FALSE)
###################################################
## mod <- "model{
## for(i in 1:npe){ #loop through party-elections
##     for(j in 1:ncolx){ #loop through issues
##         X[i,j] ~ dbin(p[i,j], n[i,j])
##             logit(p[i,j]) <- alpha[j] + beta[j] * Z[party[i], elec[i]]
##             }
##         }
## beta[1] <- 1
## alpha[1] ~ dnorm(0,1)
## for(i in 2:ncolx){
##     beta[i] ~ dnorm(0, 1)T(0, ) # Here, betas are truncated to be
##     alpha[i] ~ dnorm(0,1)   # positive, theoretically defensible
## }
## for(j in 1:nparty){
## Z[j, 1] ~ dnorm(0, tau.Z[1])
## for(i in 2:nelec){
## Z[j, i] ~ dnorm(Z[j, i-1], tau.Z[2])
## }
## }
## tau.Z[1] ~ dgamma(1, .1)
## tau.Z[2] ~ dgamma(1, .1)
## }"
## 
## jags.data <- list(
##     nelec = max(as.numeric(as.factor(uk$date))),
##     nparty = max(as.numeric(as.factor(uk$partyname))),
##     npe = nrow(uk),
##     ncolx = ncol(X),
##     X=as.matrix(X), n=as.matrix(n),
##     party = as.numeric(as.factor(uk$partyname)),
##     elec = as.numeric(as.factor(uk$date))
## )
## library(runjags)
## out <- run.jags(mod, data=jags.data, monitor=c("beta", "alpha",
##     "tau.Z", "Z"), adapt=5000, burnin=20000, sample=5000, thin=5,
##     summarise=F)


###################################################
### code chunk number 291: load_ukirt
###################################################
library(runjags)
load("uk_irt.rda")


###################################################
### code chunk number 292: inform_priors (eval = FALSE)
###################################################
## probs <- jags.data$X/jags.data$n
## probsm <- colMeans(probs, na.rm=TRUE)
## alpha.init <- qlogis(probsm)
## probsd <- apply(probs, 2, sd, na.rm=TRUE)
## beta.init <- probsd/probsd[1]
## beta.init[1] <- NA
## 
## inform.inits <- list(
##     Z = matrix(c(1,-1,0, rep(NA, 17*3)), nrow=3),
##     alpha=alpha.init,
##     beta=beta.init,
##     tau.Z = c(.1,.1))
## 
## out1 <- run.jags(mod, data=jags.data,
##                  monitor=c("beta", "alpha", "tau.Z", "Z"),
##     inits = list(inform.inits, inform.inits), adapt=5000,
##     burnin=20000, sample=5000, thin=5, summarise=F)
## 
## om <- out$mcmc
## om1 <- out1$mcmc
## 
## p <- om[,c(grep("alpha", colnames(om[[1]])),
##            grep("beta", colnames(om[[1]])))]
## p1 <- om1[,c(grep("alpha", colnames(om1[[1]])),
##              grep("beta", colnames(om1[[1]])))]
## sp <- summary(p)$statistics
## sp1 <- summary(p1)$statistics
## cor(sp[,1], sp1[,1])
## mean(sp[,2]/sp1[,2], na.rm=TRUE)


###################################################
### code chunk number 293: uk_icc
###################################################
coefs <- out$mcmc[,-grep("^Z\\[",
                         colnames(out$mcmc[[1]]), fixed=F)]
coefs <- combine.mcmc(coefs)

coef.list <- lapply(1:12, function(x)
  cbind(coefs[,x], coefs[, (x+12)]))

lats <- out$mcmc[,grep("^Z\\[", colnames(out$mcmc[[1]]), fixed=F)]
lats <- combine.mcmc(lats)

s <- seq(min(colMeans(lats)), max(colMeans(lats)), length=250)

preds <- lapply(coef.list, function(x)plogis(x %*% t(cbind(s,1))))
preds.ci <- lapply(preds, function(x)
  t(apply(x, 2, quantile,c(.5,.025,.975))))

preds.ci <- do.call(rbind, preds.ci)
preds.ci <- as.data.frame(preds.ci)
names(preds.ci) <- c("p", "lower", "upper")
preds.ci$var <- rep(1:12, each=250)
preds.ci$s <- rep(s, 12)

issues <- c("Military", "Internationalism" ," Constitutionalism",
  "Centralization", "Protectionism", "Welfare", "Education",
  "National Way of Life", "Traditional Morality", "Multiculturalism",
  "Labour", "Economy")
preds.ci$var <- factor(preds.ci$var, levels=1:12, labels=issues)

ggplot(preds.ci, aes(x=s)) +
  geom_ribbon(aes(ymin = lower, ymax=upper), col="gray75", alpha=.25) +
  geom_line(aes(y=p)) +
  facet_wrap(~var) +
  theme_bw() +
  labs(x="Left-Right Scale", y="Pr(Right Statements)")


###################################################
### code chunk number 294: makepplace
###################################################
uk$partynum <- as.numeric(uk$partyname)
uk$elecnum <- as.numeric(as.factor(uk$date))
uk$pelec <- apply(uk[,c("partynum", "elecnum")], 1,
                  paste, collapse=":")
plot.dat <- data.frame(
    z = colMeans(lats),
    lower = apply(lats, 2, quantile, .025),
    upper = apply(lats, 2, quantile, .975),
    party = factor(rep(1:3, 18),
                   labels=c("Conservative", "Labour", "Liberal")),
    elec = factor(rep(1:18, each=3),
                  labels=as.character(unique(uk$edate))))

ggplot(plot.dat, aes(y=elec)) +
  geom_point(aes(x=z), pch=16) +
  geom_segment(aes(x=lower, xend=upper, y=elec, yend=elec)) +
  facet_wrap(~party) +
  theme_bw() +
  labs(y="", x="Left-Right Scale")


###################################################
### code chunk number 295: dmat1
###################################################
## identify only conservative party latent variable scores
conspart <- lats[,grep("^Z\\[1", colnames(lats))]

## any changes (i.e., from any time to any other time)
combs <- combn(18,2)
d <- matrix(0, ncol=ncol(combs), nrow=ncol(conspart))
d[cbind(combs[1,], 1:ncol(combs))] <- -1
d[cbind(combs[2,], 1:ncol(combs))] <- 1
diff <- conspart %*%d
p <- apply(diff, 2, function(x)mean(x > 0))
p <- ifelse(p > .5, 1-p, p)

## only year-over-year changes
dy <- d[, which(combs[2,] - combs[1,] == 1)]
diff2 <- conspart %*% dy
p2 <- apply(diff2, 2, function(x)mean(x >0))
p2 <- ifelse(p2 > .5, 1-p2, p2)
pmat <- p
p2mat <- p2


###################################################
### code chunk number 296: dmat2
###################################################
labpart <- lats[,grep("^Z\\[2", colnames(lats))]

## any changes (i.e., from any time to any other time)
combs <- combn(18,2)
d <- matrix(0, ncol=ncol(combs), nrow=ncol(labpart))
d[cbind(combs[1,], 1:ncol(combs))] <- -1
d[cbind(combs[2,], 1:ncol(combs))] <- 1
diff <- labpart %*%d
p <- apply(diff, 2, function(x)mean(x > 0))
p <- ifelse(p > .5, 1-p, p)

## only year-over-year changes
dy <- d[, which(combs[2,] - combs[1,] == 1)]
diff2 <- labpart %*% dy
p2 <- apply(diff2, 2, function(x)mean(x >0))
p2 <- ifelse(p2 > .5, 1-p2, p2)

pmat <- cbind(pmat, p)
p2mat <- cbind(p2mat, p2)

libpart <- lats[,grep("^Z\\[3", colnames(lats))]

## any changes (i.e., from any time to any other time)
combs <- combn(ncol(libpart),2)
d <- matrix(0, ncol=ncol(combs), nrow=ncol(libpart))
d[cbind(combs[1,], 1:ncol(combs))] <- -1
d[cbind(combs[2,], 1:ncol(combs))] <- 1
diff <- libpart %*%d
p <- apply(diff, 2, function(x)mean(x > 0))
p <- ifelse(p > .5, 1-p, p)

## only year-over-year changes
dy <- d[, which(combs[2,] - combs[1,] == 1)]
diff2 <- libpart %*% dy
p2 <- apply(diff2, 2, function(x)mean(x >0))
p2 <- ifelse(p2 > .5, 1-p2, p2)

pmat <- cbind(pmat, p)
p2mat <- cbind(p2mat, p2)


###################################################
### code chunk number 297: makep2tab
###################################################
rn <- c("1950-1945","1951-1950","1955-1951","1959-1955","1964-1959",
        "1966-1964","1970-1966","1974.1-1970", "1974.2-1974",
        "1979-1974.2", "1983-1979","1987-1983","1992-1987",
        "1997-1992", "2001-1997","2005-2001","2010-2005")
rownames(p2mat) <- rn
colnames(p2mat) <- c("Conservative", "Labour", "Liberal")
library(xtable)
print(xtable(p2mat), digits=2, only.contents=TRUE)


###################################################
### code chunk number 298: pagg
###################################################
colnames(pmat) <- colnames(p2mat) <- levels(uk$partynum)

unelec <- unique(as.character(uk$date))
undate <- as.Date(paste(substr(unelec, 1, 4),
                        substr(unelec, 5, 6),
                        "01", sep="-"),
                  format="%Y-%m-%d")

pyear <- substr(as.character(undate), 1, 4)
pyear[9:10] <- c("1974.1", "1974.2")
pyearmat <- matrix(pyear[combs], nrow=2)
pyearmat <- pyearmat[c(2,1), ]
rownames(pmat) <- apply(pyearmat, 2, paste, collapse="-")

yearmat <- matrix(pyear[combs[, which(combs[2,] - combs[1,] == 1)]],
                  nrow=2)
yearmat <- yearmat[c(2,1), ]
rownames(p2mat) <- apply(yearmat, 2, paste, collapse="-")

sigdiffs <- rbind(apply(pmat, 2, function(x)mean(x < .05)),
                  apply(p2mat, 2, function(x)mean(x < .05)))
rownames(sigdiffs) <- c("All Differences", "Election-over-Election")
colnames(sigdiffs) <- c("Conservative", "Labour", "Liberal")
sigdiffs <- sigdiffs[c(2,1), ]


###################################################
### code chunk number 299: printdifftab
###################################################
print(xtable(sigdiffs), only.contents=TRUE)


###################################################
### code chunk number 300: loadSOTU
###################################################
data(SOTUcorpus, package="asmcjr")


###################################################
### code chunk number 301: load_emirt
###################################################
library(emIRT)
library(tm)
library(quanteda)


###################################################
### code chunk number 302: loadSOTU
###################################################
data(SOTUcorpus, package="asmcjr")
head(summary(SOTUcorpus))


###################################################
### code chunk number 303: tailSOTU
###################################################
tail(kwic(SOTUcorpus, "homeland", 3), 10)


###################################################
### code chunk number 304: preprocSOTU
###################################################
SOTUcorpus.subset <- corpus_subset(SOTUcorpus,
                                   Date > as.Date("1985-01-01"))
party <- docvars(SOTUcorpus.subset, "party")
dfmat <- dfm(SOTUcorpus.subset,
    stem = TRUE,
    remove_punct = TRUE,
    remove = stopwords("english"))
dfm.trimmed <- dfm_trim(dfmat, min_termfreq = 3, min_docfreq = 1)


###################################################
### code chunk number 305: dimtrim
###################################################
dim(dfm.trimmed)
dat <- t(as.matrix(dfm.trimmed))
J <- nrow(dat)
K <- ncol(dat)


###################################################
### code chunk number 306: poisIRT1
###################################################
starts <- list(alpha = matrix(runif(K, -1, 1)),
    x = matrix(as.numeric(
              party=="Republican") - 0.5),
    psi = matrix(runif(J, 0, 1)),
    beta = matrix(runif(J, 0, 1)))
priors <- list(alpha = list(mu = 0, sigma2 = 100),
    x = list(mu = 0, sigma2 = 100),
    psi = list(mu = 0, sigma2 = 100),
    beta = list(mu = 0, sigma2 = 100))


###################################################
### code chunk number 307: poisIRT2 (eval = FALSE)
###################################################
## set.seed(12943021)
## lout <- poisIRT(.rc = dat,
##     .starts = starts,
##     .priors = priors,
##     i = 0:(K-1),
##     NI = K,
##     .control = {list(
##         threads = 1,
##         thresh = 1e-6,
##         maxit=1000)}
##     )


###################################################
### code chunk number 308: emirtPlot (eval = FALSE)
###################################################
## df.plot <- data.frame(
##   idealpoint=lout$means$x,
##   sd = sqrt(c(lout$vars$x)),
##   name = colnames(dat))
## df.plot <- df.plot %>%
##               mutate(
##                 lower = idealpoint - 1.96*sd,
##                 upper = idealpoint + 1.96*sd,
##                 year = str_extract(
##                   as.character(df.plot$name), "\\d{4}")) %>%
##               mutate(year = as.numeric(year))
## 
## df.eras <- data.frame(
##   x=c(1984.5, 1988.5, 1988.5, 1984.5, 1984.5,
##       1992.5, 2000.5, 2000.5, 1992.5, 1992.5,
##       2008.5, 2016.5, 2016.5, 2008.5, 2008.5),
##   y=c(-.1, -.1, .1,.1,-.1,
##       -.1, -.1, .1,.1,-.1,
##       -.1, -.1, .1,.1,-.1),
##   group=as.factor(rep(1:3, each=5)))
## ggplot(df.plot) +
##     geom_point(aes(x=year, y=idealpoint), size=2, alpha=0.7) +
##   geom_segment(aes(x=year, y=lower, xend=year, yend=upper)) +
##   geom_polygon(data=df.eras, aes(x=x, y=y, group=group),
##                fill="gray75", alpha=.25) +
##     xlab("\nIdeal Point") +
##     ylab("") +
##   coord_cartesian(ylim=c(-.08, .08)) +
##   scale_x_continuous(sec.axis=sec_axis(trans=~.,
##          breaks=c(1986.5, 1990.5, 1996.5, 2004.5, 2012.5, 2018),
##          labels=c("Reagan", " GHW Bush", "Clinton",
##                   "GW Bush", "Obama","Trump"))) +
##   theme_bw()
## 



***********************************
** Code for POLSCI 9590 Week 10  **
** Dave Armstrong                **
** University of Western Ontario **
** dave.armstrong@uwo.ca         **
** 2024                          **
***********************************

use "wt_samp.dta", clear
mean y

mean y [pw=weight]

use "ces19w.dta", clear

table agegrp [pw=weight], stat(mean market)

svyset [pw=weight]
svy: regress market i.agegrp
margins agegrp
marginsplot, recast(scatter) plotopts(msymbol(o))


svy: tab vote agegrp, col

egen z_market = std(market)
egen z_leader_con = std(leader_con)
egen z_market = std(market)

svy: regress z_market z_leader_con

svy: regress market i.educ

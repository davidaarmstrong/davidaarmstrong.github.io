***********************************
** Code for POLSCI 9590 Week 5   **
** Dave Armstrong                **
** University of Western Ontario **
** dave.armstrong@uwo.ca         **
** 2024                          **
***********************************


use samp.dta, clear
ci means samp

use ces19.dta, clear
mean market, over(vote)


recode vote (2=1 "Conservative") (4=2 "Other") (1=3 "Liberal") (3=4 "NDP"), gen(voteo)
mean market, over(voteo)
marginsplot, recast(scatter) plotopts(msym(circle)) xtitle("Vote") ytitle("Average Market Liberalism")



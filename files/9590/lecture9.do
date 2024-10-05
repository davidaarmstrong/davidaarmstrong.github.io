***********************************
** Code for POLSCI 9590 Week 1   **
** Dave Armstrong                **
** University of Western Ontario **
** dave.armstrong@uwo.ca         **
** 2024                          **
***********************************

use ces19.dta, clear
cor leader*

pwcorr leader*, sig list star(.05)

graph matrix leader_con leader_lib leader_ndp, jitter(1) mcolor(gray%15)

net install heatplot.pkg
pwcorr leader_con leader_lib leader_ndp
mat R = r(C)
heatplot R, values(format(%9.3f)) color(hcl, diverging intensity(.6)) ///
  aspectratio(1) lower nodiag cuts(-1(.1)1)

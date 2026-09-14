***********************************
** Code for POLSCI 9590 Week 4   **
** R to Stata Translation        **
** Dave Armstrong                **
** University of Western Ontario **
** dave.armstrong@uwo.ca         **
** 2024                          **
***********************************


** Install Packages
* net install fitstat.pkg
* net install epcp, from("http://www.cnlawrence.com/data/")
* net install uwo_9592, from("https://raw.githubusercontent.com/davidaarmstrong/uwo_9592/refs/heads/main/")
* net from https://www.stata.com
use "data/anes_2008_binary.dta", clear
logit voted age educ income c.leftright##c.leftright female i.race
fitstat
epcp 


preserve
predict pred_prob
drop if voted == . 
sort pred_prob
gen y = 1
gen index=_n
twoway (spike y index, sort colorvar(voted) lwidth(1)) ///
  (lowess voted index, lcolor(red)), ///
  clegend(off) yscale(off) legend(off)
graph export "images/sepplot_stata.png", replace
restore


quietly logit voted age educ income c.leftright##c.leftright female i.race
est store full
quietly logit voted educ income c.leftright##c.leftright if e(sample)
lrtest . full


quietly logit voted age female i.race 
est store m1
quietly logit voted educ income c.leftright##c.leftright 
est store m2

IC_delta m1 m2 


clarke_binary m1 m2


gen lrstren = abs(leftright-5)
quietly logit voted age female i.race lrstren
est store m3
quietly logit voted age female i.race c.leftright##c.leftright
est store m4
compare_cv m3 m4


quietly logit voted age educ income c.leftright##c.leftright female i.race
est store m5
quietly logit voted age educ income i.leftright female i.race
est store m6
lrtest m5 m6


quietly glm voted age educ income c.leftright##c.leftright female i.race, family(binomial)
gen obs = _n
preserve
gen eff = .
capture drop cookd
predict cookd, cooksd
replace cookd = -cookd
foreach i of numlist 1(1)25 {
	sort cookd
	quietly replace voted = . in 1
	sort obs
	quietly glm voted age educ income c.leftright##c.leftright female i.race, family(binomial)
	quietly margins, at((asobserved) _all) at(age=generate(age+10)) contrast(atcontrast(r))
	mat tab = r(table)
	local eff = tab[1,1]
	quietly replace eff = `eff' in `i'
	capture drop cookd
	quietly predict cookd, cooksd
	quietly replace cookd = -cookd
}

sum eff
restore


use "data/france_binary.dta", clear
* net install firthlogit.pkg
firthlogit votefn i.demsat age lrself hhincome i.retnat union



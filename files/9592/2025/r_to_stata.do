use Duncan_cat.dta, clear
reg prestige i.inc_cat##i.type education 


* Test inc_cat terms
 test (2.inc_cat = 0) (3.inc_cat = 0)

* Test type terms
test (2.type=0) (3.type=0)

* Test interaction terms
test (2.inc_cat#2.type = 0) (2.inc_cat#3.type = 0) (3.inc_cat#2.type = 0) (3.inc_cat#3.type = 0)

* Test education
test education


margins inc_cat#type


margins inc_cat#type, post
test 3.inc_cat#1.type - 2.inc_cat#1.type - 3.inc_cat#3.type + 2.inc_cat#3.type = 0


marginsplot, by(type) recast(scatter) plotopts(msymbol(circle))
graph export "margins_2cat.png", as(png) replace


use Prestige.dta, clear
replace income = income/1000
reg prestige c.income##i.type education


test 2.type 3.type
test 2.type#c.income 3.type#c.income


margins, dydx(income) at(type = (1 2 3)) 
margins, dydx(income) at(type = (1 2 3)) pwcompare


quietly margins, at(income=(.5(1.25)26.75) type=(1 2 3))
marginsplot, recast(line) recastci(rarea) ///
  ci1opts(color("red%20")) plot1opts(lcolor("red")) ///
  ci2opts(color("green%20")) plot2opts(lcolor("green") lpattern("solid")) ///
  ci3opts(color("blue%20")) plot3opts(lcolor("blue") lpattern("solid")) ///
  xlabel(.5 5.75 11 16.25 21.5 26.75) saving(m1, replace)

twoway (hist income if type == 1, color("red%20") freq width(1.25) start(.5)) ///
  (hist income if type == 2, color("green%20") freq width(1.25) start(.5)) ///
  (hist income if type == 3, color("blue%20") freq  width(1.25) start(.5)), ///
  legend(label(1 "BC") label(2 "Prof") label(3 "WC")) xlabel(.5 5.75 11 16.25 21.5 26.75) saving(h1, replace) fysize(25)

gr combine h1.gph m1.gph, cols(1) 
graph export "cat_con_eff_hist.png", replace


foreach n of numlist 4.1 6.7 8.8 {
	margins type, at(income=`n') pwcompare
}


use Prestige.dta, clear
replace income = income/1000
reg prestige c.income##c.education i.type


margins, dydx(income) at(education = (6(1)16)) 
marginsplot, recast(line) recastci(rarea) ciopts(color("%15")) saving(inc_marg, replace) title("")
twoway hist education, start(6) width(.5) saving(ed_hist, replace) freq fysize(25) xtitle("")

margins, dydx(education) at(income = (.5(2)26.5)) 
marginsplot, recast(line) recastci(rarea) ciopts(color("%15")) saving(ed_marg, replace) title("")
twoway hist income, start(.5) width(2) saving(inc_hist, replace) freq fysize(25) xtitle("")

gr combine ed_hist.gph inc_marg.gph, cols(1) xcommon saving(inc_eff, replace)
gr combine inc_hist.gph  ed_marg.gph, cols(1) xcommon saving(ed_eff, replace)

gr combine inc_eff.gph ed_eff.gph, cols(2)
graph export "cond_eff_inc_ed.png", replace


reg prestige c.income##c.education i.type
mat list e(b)


do "changeSig.do"


changeSig, xind(1) zind(2) multind(3)


sum educ
margins, dydx(income) at(education = (`r(min)' `r(max)'))
sum income 
margins, dydx(education) at(income = (`r(min)' `r(max)'))


use data/anes2012.dta, clear
logit votedem black evprot c.incgroup_num##c.lrself i.econ_retnat


margins, dydx(incgroup_num) at(lrself = (0(1)10))


margins, dydx(incgroup_num) at(lrself= (0 9)) pwcompare


margins, at(lrself=(0 9)) at(lrself=(0 9) incgroup_num=generate(incgroup_num+1)) post
lincom (4._at - 3._at) - (2._at - 1._at)


quietly logit votedem black evprot c.incgroup_num##c.lrself i.econ_retnat
quietly margins, at(incgroup_num = (1(1)28) lrself = (0 5 10))
marginsplot, recast(line) recastci(rarea) ///
  plot1opts(lcolor("red")) ///
  plot2opts(lcolor("green")) ///
  plot3opts(lcolor("blue") lpattern("solid")) ///
  ci1opts(color("red%15")) ///
  ci2opts(color("green%15")) ///
  ci3opts(color("blue%15")) ///
  xtitle("Income Group") ///
  ytitle("Predicted Pr(Vote Obama)")
graph export "inc_ed_eff_stata.png", replace


use Duncan_cat.dta, clear
reg prestige i.inc_cat##i.type education 


* Test inc_cat terms
 test (2.inc_cat = 0) (3.inc_cat = 0)

* Test type terms
test (2.type=0) (3.type=0)

* Test interaction terms
test (2.inc_cat#2.type = 0) (2.inc_cat#3.type = 0) (3.inc_cat#2.type = 0) (3.inc_cat#3.type = 0)

* Test education
test education


margins inc_cat#type


margins inc_cat#type, post
test 3.inc_cat#1.type - 2.inc_cat#1.type - 3.inc_cat#3.type + 2.inc_cat#3.type = 0


marginsplot, by(type) recast(scatter) plotopts(msymbol(circle))
graph export "margins_2cat.png", as(png) replace


use Prestige.dta, clear
replace income = income/1000
reg prestige c.income##i.type education


test 2.type 3.type
test 2.type#c.income 3.type#c.income


margins, dydx(income) at(type = (1 2 3)) 
margins, dydx(income) at(type = (1 2 3)) pwcompare


quietly margins, at(income=(.5(1.25)26.75) type=(1 2 3))
marginsplot, recast(line) recastci(rarea) ///
  ci1opts(color("red%20")) plot1opts(lcolor("red")) ///
  ci2opts(color("green%20")) plot2opts(lcolor("green") lpattern("solid")) ///
  ci3opts(color("blue%20")) plot3opts(lcolor("blue") lpattern("solid")) ///
  xlabel(.5 5.75 11 16.25 21.5 26.75) saving(m1, replace)

twoway (hist income if type == 1, color("red%20") freq width(1.25) start(.5)) ///
  (hist income if type == 2, color("green%20") freq width(1.25) start(.5)) ///
  (hist income if type == 3, color("blue%20") freq  width(1.25) start(.5)), ///
  legend(label(1 "BC") label(2 "Prof") label(3 "WC")) xlabel(.5 5.75 11 16.25 21.5 26.75) saving(h1, replace) fysize(25)

gr combine h1.gph m1.gph, cols(1) 
graph export "cat_con_eff_hist.png", replace


foreach n of numlist 4.1 6.7 8.8 {
	margins type, at(income=`n') pwcompare
}


use Prestige.dta, clear
replace income = income/1000
reg prestige c.income##c.education i.type


margins, dydx(income) at(education = (6(1)16)) 
marginsplot, recast(line) recastci(rarea) ciopts(color("%15")) saving(inc_marg, replace) title("")
twoway hist education, start(6) width(.5) saving(ed_hist, replace) freq fysize(25) xtitle("")

margins, dydx(education) at(income = (.5(2)26.5)) 
marginsplot, recast(line) recastci(rarea) ciopts(color("%15")) saving(ed_marg, replace) title("")
twoway hist income, start(.5) width(2) saving(inc_hist, replace) freq fysize(25) xtitle("")

gr combine ed_hist.gph inc_marg.gph, cols(1) xcommon saving(inc_eff, replace)
gr combine inc_hist.gph  ed_marg.gph, cols(1) xcommon saving(ed_eff, replace)

gr combine inc_eff.gph ed_eff.gph, cols(2)
graph export "cond_eff_inc_ed.png", replace


reg prestige c.income##c.education i.type
mat list e(b)


do "changeSig.do"


changeSig, xind(1) zind(2) multind(3)


sum educ
margins, dydx(income) at(education = (`r(min)' `r(max)'))
sum income 
margins, dydx(education) at(income = (`r(min)' `r(max)'))


use data/anes2012.dta, clear
logit votedem black evprot c.incgroup_num##c.lrself i.econ_retnat


margins, dydx(incgroup_num) at(lrself = (0(1)10))


margins, dydx(incgroup_num) at(lrself= (0 9)) pwcompare


margins, at(lrself=(0 9)) at(lrself=(0 9) incgroup_num=generate(incgroup_num+1)) post
lincom (4._at - 3._at) - (2._at - 1._at)


quietly logit votedem black evprot c.incgroup_num##c.lrself i.econ_retnat
quietly margins, at(incgroup_num = (1(1)28) lrself = (0 5 10))
marginsplot, recast(line) recastci(rarea) ///
  plot1opts(lcolor("red")) ///
  plot2opts(lcolor("green")) ///
  plot3opts(lcolor("blue") lpattern("solid")) ///
  ci1opts(color("red%15")) ///
  ci2opts(color("green%15")) ///
  ci3opts(color("blue%15")) ///
  xtitle("Income Group") ///
  ytitle("Predicted Pr(Vote Obama)")
graph export "inc_ed_eff_stata.png", replace


use Duncan_cat.dta, clear
reg prestige i.inc_cat##i.type education 


* Test inc_cat terms
 test (2.inc_cat = 0) (3.inc_cat = 0)

* Test type terms
test (2.type=0) (3.type=0)

* Test interaction terms
test (2.inc_cat#2.type = 0) (2.inc_cat#3.type = 0) (3.inc_cat#2.type = 0) (3.inc_cat#3.type = 0)

* Test education
test education


margins inc_cat#type


margins inc_cat#type, post
test 3.inc_cat#1.type - 2.inc_cat#1.type - 3.inc_cat#3.type + 2.inc_cat#3.type = 0


marginsplot, by(type) recast(scatter) plotopts(msymbol(circle))
graph export "margins_2cat.png", as(png) replace


use Prestige.dta, clear
replace income = income/1000
reg prestige c.income##i.type education


test 2.type 3.type
test 2.type#c.income 3.type#c.income


margins, dydx(income) at(type = (1 2 3)) 
margins, dydx(income) at(type = (1 2 3)) pwcompare


quietly margins, at(income=(.5(1.25)26.75) type=(1 2 3))
marginsplot, recast(line) recastci(rarea) ///
  ci1opts(color("red%20")) plot1opts(lcolor("red")) ///
  ci2opts(color("green%20")) plot2opts(lcolor("green") lpattern("solid")) ///
  ci3opts(color("blue%20")) plot3opts(lcolor("blue") lpattern("solid")) ///
  xlabel(.5 5.75 11 16.25 21.5 26.75) saving(m1, replace)

twoway (hist income if type == 1, color("red%20") freq width(1.25) start(.5)) ///
  (hist income if type == 2, color("green%20") freq width(1.25) start(.5)) ///
  (hist income if type == 3, color("blue%20") freq  width(1.25) start(.5)), ///
  legend(label(1 "BC") label(2 "Prof") label(3 "WC")) xlabel(.5 5.75 11 16.25 21.5 26.75) saving(h1, replace) fysize(25)

gr combine h1.gph m1.gph, cols(1) 
graph export "cat_con_eff_hist.png", replace


foreach n of numlist 4.1 6.7 8.8 {
	margins type, at(income=`n') pwcompare
}


use Prestige.dta, clear
replace income = income/1000
reg prestige c.income##c.education i.type


margins, dydx(income) at(education = (6(1)16)) 
marginsplot, recast(line) recastci(rarea) ciopts(color("%15")) saving(inc_marg, replace) title("")
twoway hist education, start(6) width(.5) saving(ed_hist, replace) freq fysize(25) xtitle("")

margins, dydx(education) at(income = (.5(2)26.5)) 
marginsplot, recast(line) recastci(rarea) ciopts(color("%15")) saving(ed_marg, replace) title("")
twoway hist income, start(.5) width(2) saving(inc_hist, replace) freq fysize(25) xtitle("")

gr combine ed_hist.gph inc_marg.gph, cols(1) xcommon saving(inc_eff, replace)
gr combine inc_hist.gph  ed_marg.gph, cols(1) xcommon saving(ed_eff, replace)

gr combine inc_eff.gph ed_eff.gph, cols(2)
graph export "cond_eff_inc_ed.png", replace


reg prestige c.income##c.education i.type
mat list e(b)


do "changeSig.do"


changeSig, xind(1) zind(2) multind(3)


sum educ
margins, dydx(income) at(education = (`r(min)' `r(max)'))
sum income 
margins, dydx(education) at(income = (`r(min)' `r(max)'))


use data/anes2012.dta, clear
logit votedem black evprot c.incgroup_num##c.lrself i.econ_retnat


margins, dydx(incgroup_num) at(lrself = (0(1)10))


margins, dydx(incgroup_num) at(lrself= (0 9)) pwcompare


margins, at(lrself=(0 9)) at(lrself=(0 9) incgroup_num=generate(incgroup_num+1)) post
lincom (4._at - 3._at) - (2._at - 1._at)


quietly logit votedem black evprot c.incgroup_num##c.lrself i.econ_retnat
quietly margins, at(incgroup_num = (1(1)28) lrself = (0 5 10))
marginsplot, recast(line) recastci(rarea) ///
  plot1opts(lcolor("red")) ///
  plot2opts(lcolor("green")) ///
  plot3opts(lcolor("blue") lpattern("solid")) ///
  ci1opts(color("red%15")) ///
  ci2opts(color("green%15")) ///
  ci3opts(color("blue%15")) ///
  xtitle("Income Group") ///
  ytitle("Predicted Pr(Vote Obama)")
graph export "inc_ed_eff_stata.png", replace


use Duncan_cat.dta, clear
reg prestige i.inc_cat##i.type education 


* Test inc_cat terms
 test (2.inc_cat = 0) (3.inc_cat = 0)

* Test type terms
test (2.type=0) (3.type=0)

* Test interaction terms
test (2.inc_cat#2.type = 0) (2.inc_cat#3.type = 0) (3.inc_cat#2.type = 0) (3.inc_cat#3.type = 0)

* Test education
test education


margins inc_cat#type


margins inc_cat#type, post
test 3.inc_cat#1.type - 2.inc_cat#1.type - 3.inc_cat#3.type + 2.inc_cat#3.type = 0


marginsplot, by(type) recast(scatter) plotopts(msymbol(circle))
graph export "margins_2cat.png", as(png) replace


use Prestige.dta, clear
replace income = income/1000
reg prestige c.income##i.type education


test 2.type 3.type
test 2.type#c.income 3.type#c.income


margins, dydx(income) at(type = (1 2 3)) 
margins, dydx(income) at(type = (1 2 3)) pwcompare


quietly margins, at(income=(.5(1.25)26.75) type=(1 2 3))
marginsplot, recast(line) recastci(rarea) ///
  ci1opts(color("red%20")) plot1opts(lcolor("red")) ///
  ci2opts(color("green%20")) plot2opts(lcolor("green") lpattern("solid")) ///
  ci3opts(color("blue%20")) plot3opts(lcolor("blue") lpattern("solid")) ///
  xlabel(.5 5.75 11 16.25 21.5 26.75) saving(m1, replace)

twoway (hist income if type == 1, color("red%20") freq width(1.25) start(.5)) ///
  (hist income if type == 2, color("green%20") freq width(1.25) start(.5)) ///
  (hist income if type == 3, color("blue%20") freq  width(1.25) start(.5)), ///
  legend(label(1 "BC") label(2 "Prof") label(3 "WC")) xlabel(.5 5.75 11 16.25 21.5 26.75) saving(h1, replace) fysize(25)

gr combine h1.gph m1.gph, cols(1) 
graph export "cat_con_eff_hist.png", replace


foreach n of numlist 4.1 6.7 8.8 {
	margins type, at(income=`n') pwcompare
}


use Prestige.dta, clear
replace income = income/1000
reg prestige c.income##c.education i.type


margins, dydx(income) at(education = (6(1)16)) 
marginsplot, recast(line) recastci(rarea) ciopts(color("%15")) saving(inc_marg, replace) title("")
twoway hist education, start(6) width(.5) saving(ed_hist, replace) freq fysize(25) xtitle("")

margins, dydx(education) at(income = (.5(2)26.5)) 
marginsplot, recast(line) recastci(rarea) ciopts(color("%15")) saving(ed_marg, replace) title("")
twoway hist income, start(.5) width(2) saving(inc_hist, replace) freq fysize(25) xtitle("")

gr combine ed_hist.gph inc_marg.gph, cols(1) xcommon saving(inc_eff, replace)
gr combine inc_hist.gph  ed_marg.gph, cols(1) xcommon saving(ed_eff, replace)

gr combine inc_eff.gph ed_eff.gph, cols(2)
graph export "cond_eff_inc_ed.png", replace


reg prestige c.income##c.education i.type
mat list e(b)


do "changeSig.do"


changeSig, xind(1) zind(2) multind(3)


sum educ
margins, dydx(income) at(education = (`r(min)' `r(max)'))
sum income 
margins, dydx(education) at(income = (`r(min)' `r(max)'))


use data/anes2012.dta, clear
logit votedem black evprot c.incgroup_num##c.lrself i.econ_retnat


margins, dydx(incgroup_num) at(lrself = (0(1)10))


margins, dydx(incgroup_num) at(lrself= (0 9)) pwcompare


margins, at(lrself=(0 9)) at(lrself=(0 9) incgroup_num=generate(incgroup_num+1)) post
lincom (4._at - 3._at) - (2._at - 1._at)


quietly logit votedem black evprot c.incgroup_num##c.lrself i.econ_retnat
quietly margins, at(incgroup_num = (1(1)28) lrself = (0 5 10))
marginsplot, recast(line) recastci(rarea) ///
  plot1opts(lcolor("red")) ///
  plot2opts(lcolor("green")) ///
  plot3opts(lcolor("blue") lpattern("solid")) ///
  ci1opts(color("red%15")) ///
  ci2opts(color("green%15")) ///
  ci3opts(color("blue%15")) ///
  xtitle("Income Group") ///
  ytitle("Predicted Pr(Vote Obama)")
graph export "inc_ed_eff_stata.png", replace



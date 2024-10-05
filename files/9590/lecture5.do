***********************************
** Code for POLSCI 9590 Week 4   **
** Dave Armstrong                **
** University of Western Ontario **
** dave.armstrong@uwo.ca         **
** 2024                          **
***********************************

use ces19.dta, clear
tabstat leader_lib,  statistics(mean sd min p25 p50 p75 max n )

tabstat leader_lib, by(educ) statistics(mean sd min p25 p50 p75 max n )

preserve
collapse (mean) mean = leader_lib, by(educ)
drop if educ == .

twoway scatter mean educ, xlabel(1 "<HS" 2 "HS/College" 3 "College Grad") \\\
yscale(range(0 100)) ylabel(0(10)100) \\\
ytitle("Lib Leader Feeling Thermometer (0-100)") \\\
xtitle("Highest Level of Educational Attainment")

restore

preserve
collapse (mean) mean = leader_lib (median) med = leader_lib, by(educ)
drop if educ == .

twoway (scatter mean educ, mcolor("black") msymbol(circle)) \\\
  (scatter med educ, mcolor("gray") msymbol(square)), \\\
  xlabel(1 "<HS" 2 "HS/College" 3 "College Grad") \\\
  yscale(range(0 100)) ylabel(0(10)100) \\\
  ytitle("Lib Leader Feeling Thermometer (0-100)") \\\
  xtitle("Highest Level of Educational Attainment") \\\
  legend(label(1 "Mean") label(2 "Median"))

restore


preserve
collapse (mean) mean = leader_lib (median) med = leader_lib, by(educ)
drop if educ == .

gen edlow = educ - .125
gen edhigh = educ + .125

twoway (scatter mean edlow, mcolor("black") msymbol(circle)) \\\
  (scatter med edhigh, mcolor("gray") msymbol(square)), \\\
  xlabel(1 "<HS" 2 "HS/College" 3 "College Grad") \\\
  yscale(range(0 100)) ylabel(0(10)100) \\\
  ytitle("Lib Leader Feeling Thermometer (0-100)") \\\
  xtitle("Highest Level of Educational Attainment") \\\
  legend(label(1 "Mean") label(2 "Median"))

restore


clear
set obs 3
gen country = "A"
replace country = "B" in 2
replace country = "C" in 3
gen yr1999 = _n
gen yr2000 = _n+3

reshape long yr, i(country) j(year)
rename yr value

clear
set obs 3
gen country = "A"
replace country = "B" in 2
replace country = "C" in 3
gen yr1999 = _n
gen yr2000 = _n+3
reshape long yr, i(country) j(year)
rename yr value
list

reshape wide value, i(country) j(year)
list

use ces19.dta, clear
collapse (mean) mean_ndp = leader_ndp mean_con = leader_con mean_lib = leader_lib (median) median_ndp = leader_ndp median_con = leader_con median_lib = leader_lib, by(educ)

reshape long mean_ median_, i(educ) j(party) string

rename (mean_ median_) (stat_mean stat_median)

reshape long stat_, i(educ party) j(value) string
drop if educ == .
rename (stat_ value) (value stat)
encode party, into(party_num)

twoway (scatter value educ if party == "ndp", mcol("245 130 32") msymbol(circle)) (scatter value educ if party == "lib", mcol("215 25 32") msymbol(square)) (scatter value educ if party == "con", mcol("0 63 114") msymbol(triangle)), by(stat, imargin(medlarge)) xlabel(1 "<HS" 2 "HS/College" 3 "College Grad") xtitle("Highest Level of Educational Attainment") ytitle("Average Leader Thermometer Score") legend(label(1 "NDP") label(2 "Liberal") label(3 "Conservative") rows(1))


twoway (scatter value educ if party == "ndp", c(l) lcol("245 130 32") mcol("245 130 32") msymbol(circle)) (scatter value educ if party == "lib", c(l) lcol("215 25 32") mcol("215 25 32") msymbol(square)) (scatter value educ if party == "con", c(l) lpattern(solid) lcol("0 63 114") mcol("0 63 114") msymbol(triangle)), by(stat, imargin(medlarge)) xlabel(1 "<HS" 2 "HS/College" 3 "College Grad") xtitle("Highest Level of Educational Attainment") ytitle("Average Leader Thermometer Score") legend(label(1 "NDP") label(2 "Liberal") label(3 "Conservative") rows(1))



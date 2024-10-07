***********************************
** Code for POLSCI 9590 Week 3   **
** Dave Armstrong                **
** University of Western Ontario **
** dave.armstrong@uwo.ca         **
** 2024                          **
***********************************

gen y = value3
replace y = value1 if condition1
replace y = value2 if condition2


foreach x of numlist 1 3 4{
  replace y=0 if x == `x'
}


set obs 4
gen x = _n
gen y = "no"
replace y = "yes" if x <= 3
list

capture drop y
gen y = "no"
foreach i of numlist 1 2 3{
  replace y = "yes" in `i'
}

clear
set obs 5
gen x = _n
replace x = . in 5
gen y = "no"
replace y = "yes" if x <= 3

replace y = . if x == .


set obs 4
gen x = _n
gen y = .
gen z = .
replace y = 1 if x <= 2
replace y = 2 if x >= 2
replace z = 2 if x >= 2
replace z = 1 if x <= 2

set obs 4
gen x = _n
recode x (1/2 = 1) (2/4 = 2), into(y)
recode x (2/4=2) (1/2 = 1), into(z)
list

use ces19.dta, clear
gen market_01 = 1
centile market, centile(40)
replace market_01 = 0 if market < `r(c_1)'
replace market_01 = . if market == .
tab market_01, missing

gen heart_ndp = 0
replace heart_ndp = 1 if leader_ndp > leader_lib & leader_ndp > leader_con
replace heart_ndp = . if leader_ndp ==. | leader_lib == . | leader_con == .
tab heart_ndp, missing


label def heart_ndp 0 "No" 1 "Yes"
label val heart_ndp heart_ndp
catplot heart_ndp vote, percent(heart_ndp) vertical asyvars legend(title("Likes NDP" "Leader Most"))



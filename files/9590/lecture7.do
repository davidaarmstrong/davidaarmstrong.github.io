***********************************
** Code for POLSCI 9590 Week 6   **
** Dave Armstrong                **
** University of Western Ontario **
** dave.armstrong@uwo.ca         **
** 2024                          **
***********************************

use dat_1samp.dta
ttest x == 0

use dat_1samp.dta
ttest x == 0

bitesti 250 110 .5

use ces19.dta, clear
recode vote (2=1) (1 3 4 = 0) (else = .), into(vote_con)
ttest market, by(vote_con) unequal

recode educ (3=1) (1 2=0) (else = .), into(coll_grad)
prtest vote_con, by(coll_grad)

label def vc 1 "Yes" 0 "No"
label val vote_con vc
label var vote_con "Vote Conservative"
graph box market, over(vote_con, axis(fextend)) alsize(0) ytitle("Market Liberalism") title("Vote Conservative")


preserve 
gen ones = 1
collapse (count) ones, by(coll_grad vote_con)
drop if coll_grad ==. | vote_con == .
egen n = sum(ones), by(coll_grad)
gen pct = ones/n
drop ones n
reshape wide pct, i(coll_grad) j(vote_con)
label def yn 0 "No" 1 "Yes"
label val coll_grad yn
graph bar pct1 pct0, over(coll_grad) stack legend(order(2 1) label(2 "No") label(1 "Yes") title("Vote" "Conservative?")) b1title("College Graduate")
restore



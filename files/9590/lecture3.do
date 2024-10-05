***********************************
** Code for POLSCI 9590 Week 2   **
** Dave Armstrong                **
** University of Western Ontario **
** dave.armstrong@uwo.ca         **
** 2024                          **
***********************************

net install catplot.pkg
use "ces19.dta", clear
tab educ

sum leader_lib, det

tabstat leader_lib, by(educ) statistics(mean sd min p25 p50 p75 max n )

centile leader_lib, centile(62)

centile leader_lib, centile(38 62)

twoway (line incidents year if prov == "Alb", lpattern(solid)) 
  (line incidents year if prov == "BC", lpattern(solid)) 
  (line incidents year if prov == "NB", lpattern(solid)) 
  (line incidents year if prov == "NL", lpattern(solid)) 
  (line incidents year if prov == "NS", lpattern(solid)) 
  (line incidents year if prov == "Ont", lpattern(solid)) 
  (line incidents year if prov == "PEI", lpattern(solid)) 
  (line incidents year if prov == "Que", lpattern(solid)) 
  (line incidents year if prov == "Sask", lpattern(solid)), 
legend(label(1 "AB") label(2 "BC") label(3 "NB") 
       label(4 "NS") label(5 "NL") label(6 "ON") 
       label(7 "PEI") label(8 "QC") label(9 "SK")) 

graph bar, over(educ)

hist leader_lib, freq

hist leader_lib, bins(10) color(gray) frequency

label def gender 1 "Male" 5 "Female"
label val gender gender
catplot gender educ, percent(gender) vertical asyvars



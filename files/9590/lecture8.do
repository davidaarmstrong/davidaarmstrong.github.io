***********************************
** Code for POLSCI 9590 Week 7   **
** Dave Armstrong                **
** University of Western Ontario **
** dave.armstrong@uwo.ca         **
** 2024                          **
***********************************

net install gr0031_1.pkg
use ces19.dta, clear

tab vote relig, col all

tab educ agegrp, col all

recode relig (2=1 "Protestant") (1=2 "Atheist") (4=3 "Other") (3=4 "Catholic"), gen(relig2)
catplot relig2 vote, percent(relig2) vertical asyvars


recode relig (2=1 "Protestant") (1=2 "Atheist") (4=3 "Other") (3=4 "Catholic"), into(relig2)
label var relig2 "Religion"
label var vote "Vote"
spineplot vote relig2



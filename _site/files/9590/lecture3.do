net install catplot.pkg


use "ces19.dta", clear


tab educ

  Respondent |
     highest |
    level of |
   education |      Freq.     Percent        Cum.
-------------+-----------------------------------
         <HS |        491       17.59       17.59
  HS/College |      1,029       36.86       54.44
College Grad |      1,272       45.56      100.00
-------------+-----------------------------------
       Total |      2,792      100.00


sum leader_lib, det

          Liberal party leader feeling thermometer
-------------------------------------------------------------
      Percentiles      Smallest
 1%            0              0
 5%            0              0
10%            0              0       Obs               2,792
25%            9              0       Sum of wgt.       2,792

50%           50                      Mean           43.39327
                        Largest       Std. dev.      30.83938
75%           70            100
90%           80            100       Variance       951.0671
95%           90            100       Skewness      -.0663978
99%          100            100       Kurtosis       1.694905




 tabstat leader_lib, by(educ) statistics(mean sd min p25 p50 p75 max n )

Summary for variables: leader_lib
Group variable: educ (Respondent highest level of education)

        educ |      Mean        SD       Min       p25       p50       p75       Max         N
-------------+--------------------------------------------------------------------------------
         <HS |  35.70408  32.62209         0         0        29        60       100       490
  HS/College |  38.94737  30.33554         0         6        39        65       100      1026
College Grad |   50.0528  29.15665         0        29        55        75       100      1269
-------------+--------------------------------------------------------------------------------
       Total |  43.43698  30.83552         0         9        50        70       100      2785
----------------------------------------------------------------------------------------------




 centile leader_lib, centile(62)
                                                         Binom. interp.   
    Variable |       Obs  Percentile    Centile        [95% conf. interval]
-------------+-------------------------------------------------------------
  leader_lib |     2,792         62          60              60          60



centile leader_lib, centile(38 62)
                                                          Binom. interp.   
    Variable |       Obs  Percentile    Centile        [95% conf. interval]
-------------+-------------------------------------------------------------
  leader_lib |     2,792         38          29              29    37.84239
             |                   62          60              60          60


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


net install catplot.pkg
label def gender 1 "Male" 5 "Female"
label val gender gender
catplot gender educ, percent(gender) vertical asyvars



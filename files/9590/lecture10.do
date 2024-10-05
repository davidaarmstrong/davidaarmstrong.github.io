***********************************
** Code for POLSCI 9590 Week 9   **
** Dave Armstrong                **
** University of Western Ontario **
** dave.armstrong@uwo.ca         **
** 2024                          **
***********************************

use example1.dta, clear
reg y x

twoway (lfit y x) || (scatter y x, msymbol(oh)), legend(off)

use demo.dta, clear
reg demodays corrupt

twoway (lfit demodays corrupt) || (scatter demodays corrupt, msymbol(oh)), legend(off)


use "cat_example.dta", clear
reg y i.x

margins x

margins x, pwcompare

marginsplot, addplot(scatter y x, mcol("black") ///
  msymbol(oh) jitter(15)) legend(off) recast(scatter) plotopts(msymbol(o))

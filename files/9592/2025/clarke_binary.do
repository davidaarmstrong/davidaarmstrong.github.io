capture program drop clarke_binary
program define clarke_binary
	syntax namelist(min=2 max=2) 
*	preserve
	capture drop pp_1 pp_2 d
	local k = 1
	local N = `e(N)'
	foreach mod of local namelist{
		quietly est restore `mod'
		quietly predict pp_`k', pr
		quietly replace pp_`k' = 1-pp_`k' if `e(depvar)' == 0
		quietly replace pp_`k' = log(pp_`k')
		if `k' == 1{
			local p = `e(rank)'
			local N1 = `e(N)'
		}
		else{
			local q = `e(rank)'
			local N2 = `e(N)'
		}
		local k = `k' + 1
	}
	if `N1' != `N2' {
		dis "Two models must have same sample size"
		exit
	}
	dis "Q = `q', P = `p', N = `N1'"
	local correction = (`p' - `q')*(log(`N1')/(2*`N1'))
	quietly gen d = pp_1 - pp_2 
	quietly count if d > `correction'
	local B = `r(N)'
	local bprob = binomial(`N1', `B', .5)
	local lwb = "Neither model is preferred."
	if(`bprob' < .025){
		local lwb = "Model 2 is preferred."
	}
	if(`bprob' > .975){
		local lwb = "Model 1 is preferred."
	}
	local pval = 2*`bprob'
	if `bprob' > .5 {
		local pval = 2*(1-`bprob')
	}
	local pval = round(`pval', .001)
	dis "`lwb' (N = `N1', k=`B', p-value = `pval')"
*	restore
end





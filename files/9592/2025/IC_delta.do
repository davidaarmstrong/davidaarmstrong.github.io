capture program drop IC_delta
program define IC_delta 
	syntax namelist
	local i = 1
		foreach mod of local namelist{
			quietly est restore `mod'
			quietly fitstat 
			local K = `r(n_parm)'
			local n = `r(N)'
			local aic = -2*`r(ll)' + 2*`K' 
			local aicc = -2*`r(ll)' + 2*`K' + (2*`K'*(`K'-1))/(`n' - `K' - 1)
			local bic = -2*`r(ll)' + `K'*log(`n')
			if `i' == 1{
				mat crit = (`aic', `aicc', `bic')
			}
			else {
				mat crit = crit \ (`aic', `aicc', `bic')
			}
			local i = `i' + 1
		}
	mata: crit = st_matrix("crit")
	mata: crit = crit :- colmin(crit)
	mata: st_matrix("delta", crit)
	mat list delta
end

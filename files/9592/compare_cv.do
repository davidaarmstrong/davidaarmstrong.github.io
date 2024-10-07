capture program drop compare_cv
program define compare_cv
		syntax namelist(min=2 max=2), [k(integer 10) r(integer 10) seed1(string) seed2(string)]
	local i = 1
	foreach mod of local namelist { 
		if `i' == 1 { 
			quietly est restore `mod'
			dis "Model 1 CV Result"
			cv_kfold, k(`k') reps(`r') seed(`seed1')
			mat cverr = r(msqr)
			local i = `i' + 1
		}
		else{
			quietly est restore `mod'
			dis ""
			dis "Model 2 CV Result"
			cv_kfold, k(`k') reps(`r') seed(`seed2')
			mat cverr = (cverr,  r(msqr))
		}
	}
	mata: cverr = st_matrix("cverr")
	mata: p_2_better = mean(cverr[,1] :< cverr[,2])
	mata: st_numscalar("p_2_better", p_2_better)
	local p2b = p_2_better
	dis ""
	dis "Pr(Model 2 > Model 1) = `p2b'"
end	

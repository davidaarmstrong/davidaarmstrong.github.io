capture program drop changeSig
program define changeSig
    syntax , Xind(integer) Zind(integer) Multind(integer) [lox(real -1000000) highx(real 1000000) loz(real -1000000) highz(real 1000000) tol(real .001) iter(integer 1000) clev(real .95)]
	if ("`e(cmd)'" != "regress") {
		display as error "Last estimate is not a linear regression"
		exit 198
	}
	local cl = 1-((1-`clev')/2)
	mata: changeSig(`xind', `zind', `multind', `lox', `highx', `loz', `highz', `tol', `iter', `cl')
end



mata: mata clear
mata 

function lwr(x, b_m, b_i, v_m, v_i, v_mi, dfres, clev){
	est = b_m :+ b_i:*x
	se = sqrt(v_m :+ x:^2:*v_i :+ 2:*x:*v_mi)
	return(est :- invttail(dfres, clev):*se)
}
function upr(x, b_m, b_i, v_m, v_i, v_mi, dfres, clev){
	est = b_m :+ b_i:*x
	se = sqrt(v_m :+ x:^2:*v_i :+ 2:*x:*v_mi)
	return(est :+ invttail(dfres, clev):*se)
}
function changeSig(x_ind, z_ind, mult_ind, lo_x, high_x, lo_z, high_z, tol, iter, clev){
	stata("matrix b = e(b)")
	stata("matrix v = e(V)")
	stata("local dfres = `e(df_r)'")
	dfr = st_local("dfres")
	dfr = strtoreal(dfr)
	
	b = st_matrix("b")
	v = st_matrix("v")
	b_cx = b[1,x_ind]
	b_i = b[1,mult_ind]
	b_cz = b[1,z_ind]
	v_cx = v[x_ind, x_ind]
	v_cz = v[z_ind, z_ind]
	v_i = v[mult_ind, mult_ind]
	v_ix = v[x_ind, mult_ind]
	v_iz = v[z_ind, mult_ind]
	cap1 = mm_root(x=., &lwr(), lo_z, high_z, tol, iter, b_cx, b_i, v_cx, v_i, v_ix, dfr, clev)
	if (cap1 != 0){
		_error(3300, "Try making the value of loz smaller")
	}	
	xl = x
	cap2 = mm_root(x=., &upr(), lo_z, high_z, tol, iter, b_cx, b_i, v_cx, v_i, v_ix, dfr, clev)
	if (cap2 != 0){
		_error(3300, "Try making the value of higz larger")
	}	
	xu = x
	cap3 = mm_root(x=., &lwr(), lo_x, high_x, tol, iter, b_cz, b_i, v_cz, v_i, v_iz, dfr, clev)
		if (cap3 != 0){
		_error(3300, "Try making the value of lox smaller")
	}	
zl = x
	cap4 = mm_root(x=., &upr(), lo_x, high_x, tol, iter, b_cz, b_i, v_cz, v_i, v_iz, dfr, clev)
	if (cap4 != 0){
		_error(3300, "ry making the value of higx larger")
	}	
	zu = x
	printf("%s: %f\n", "The lower bound of the conditional effect of X crosses zero when Z = ", round(xl, .001))
	printf("%s: %f\n", "The upper bound of the conditional effect of X crosses zero when Z = ", round(xu, .001))
	printf("%s: %f\n", "The lower bound of the conditional effect of Z crosses zero when X = ", round(zl, .001))
	printf("%s: %f\n", "The upper bound of the conditional effect of Z crosses zero when X = ", round(zu, .001))
}
end

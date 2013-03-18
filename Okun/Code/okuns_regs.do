cd k:\lynn\okuns
cap log c
log using okuns_regressions.log, replace

use GDP_spd_extrapolated, clear
drop if pc_rgdp >30 & pc_rgdp!=.
estimates clear
gen pc_rgdp2=pc_rgdp^2

/**** by union rates****/
forval u=1/2 {
	

	reg due pc_rgdp if mu==`u', cluster(fips)
	est store allunion`u'
	
	xi: reg due pc_rgdp i.state  if mu==`u', cluster(fips)
	est store allunionsfe`u'

	forval y=1/2 {
	
		reg due pc_rgdp if mu==`u' & period==`y', cluster(fips)
		est store p`y'union`u'
		
		xi: reg due pc_rgdp i.state  if mu==`u' & period==`y', cluster(fips)
		est store p`y'unionsfe`u'
	}
	
}

/*xi: reg due pc_rgdp pc_rgdp2 i.state , cluster(fips)
est store allp2_stfe*/

/* Regressions without unionizations rates*/
reg due pc_rgdp, cluster(fips)
est store nu_all

xi: reg due pc_rgdp i.state , cluster(fips)
est store nu_all_stfe

forval y=1/2 {

	reg due pc_rgdp if period==`y', cluster(fips) 
	estimates store p`y'

	xi: reg due pc_rgdp i.fips if period==`y' , cluster(fips)
	estimates store p`y'_sfe

	/*xi: reg due pc_rgdp pc_rgdp2 i.fips if period==`y', cluster(fips)
	estimates store p2`y'_yrfe*/
}

forval d=1/3 {
	
	reg due pc_rgdp if decade==`d', cluster(fips) 
	estimates store decade`d'

	xi: reg due pc_rgdp i.fips if decade==`d' , cluster(fips)
	estimates store decade`d'_sfe
}


#delimit ;
esttab all* using allperiods.csv, replace cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) constant
starlevels(* 0.10 ** 0.05 *** 0.01) stats(N r2, fmt(%9.0g) label("Observations")) style(tex) label;

#delimit ;
esttab nu_all* using nu_allperiods.csv, replace cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) constant
starlevels(* 0.10 ** 0.05 *** 0.01) stats(N r2, fmt(%9.0g) label("Observations")) style(tex) label;

#delimit ;
esttab  p1 p1_sfe p2 p2_sfe  using byperiods.csv, replace cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) constant
starlevels(* 0.10 ** 0.05 *** 0.01) stats(N r2, fmt(%9.0g) label("Observations")) style(tex) label;

#delimit ;
esttab  p1union* p2union* using unionsbyperiods.csv, replace cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) constant
starlevels(* 0.10 ** 0.05 *** 0.01) stats(N r2, fmt(%9.0g) label("Observations")) style(tex) label;


#delimit ;
esttab  d* using bydecades.csv, replace cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) constant
starlevels(* 0.10 ** 0.05 *** 0.01) stats(N r2, fmt(%9.0g) label("Observations")) style(tex) label;


graph tw lfit due pc_rgdp ||  scatter due pc_rgdp ,  msize(vtiny) by(period) ytitle("Change in U.E.") xtitle("GDP Growth Rate")
graph tw lfit due pc_rgdp ||  scatter due pc_rgdp if decade==1 ,  msymbol(p) ytitle("Change in U.E.") xtitle("GDP Growth Rate") title("1964 to 1979")
graph tw lfit due pc_rgdp ||  scatter due pc_rgdp if decade==2 ,  msize(tiny) msymbol(p)  ytitle("Change in U.E.") xtitle("GDP Growth Rate") title("1980 to 1989")
graph tw lfit due pc_rgdp ||  scatter due pc_rgdp if decade==3 ,  msize(tiny) msymbol(p) ytitle("Change in U.E.") xtitle("GDP Growth Rate") title("1990 to 2010")


graph tw lfit due pc_rgdp ||  scatter due pc_rgdp if mu==1 ,  msize(vtiny) msymbol(p)  ytitle("Change in U.E.") xtitle("GDP Growth Rate") title("Higher Unionization Decline")
graph tw lfit due pc_rgdp ||  scatter due pc_rgdp if mu==2 ,  msize(vtiny) msymbol(p)  ytitle("Change in U.E.") xtitle("GDP Growth Rate") title("Lower Unionization Decline")

label define lblmu 1 "Higher Unionization Decline" 2 "Lower Unionization Decline"
label values mu lblmu
graph tw lfit due pc_rgdp ||  scatter due pc_rgdp  ,  by(mu) msize(vtiny) msymbol(p)  ytitle("Change in U.E.") xtitle("GDP Growth Rate") 






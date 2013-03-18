cd k:\lynn\okuns
estimates clear
cap log c
log using okuns_regs2stages.log, replace

use GDP_spd_extrapolated, clear
drop if pc_rgdp >30 & pc_rgdp!=. /*getting rid of outliers*/

gen beta_pre=0
gen beta_post=0
gen cons_pre=0
gen cons_post=0

levelsof fips, local(fips)
foreach y of local fips { 
 
/****Pre Period *****/	
display "State `y' Period 1"
	reg due pc_rgdp if fips==`y' & period==1
	matrix b_pre`y' = e(b)
	svmat double b_pre`y', names(matcol)
	egen tempbeta=max(b_pre`y'pc_rgdp) 
	egen tempcons=max(b_pre`y'_cons)

	replace beta_pre= tempbeta if fips==`y'
	replace cons_pre=tempcons if fips==`y'

	drop tempbeta tempcons
display "State `y' Period 2"
	reg due pc_rgdp if fips==`y' & period==2
	matrix b_post`y'=e(b)
	svmat double b_post`y', names(matcol)

	egen tempbeta=max(b_post`y'pc_rgdp) 
	egen tempcons=max(b_post`y'_cons)

	replace beta_post= tempbeta if fips==`y'
	replace cons_post=tempcons if fips==`y'
	
	drop tempbeta tempcons	
}

 sum mem ue dunion due pc_rgdp beta* cons* if fips==38

keep if year==2009
drop if fips==56|fips==38|fips==22 /* these states had insignificant coefficient estimates*/

keep fips  beta* cons* /*year is arbitrary since values are constant for each state*/

gen trend_pre= -(cons_pre/beta_pre) 
gen trend_post= - (cons_post/beta_post)
gen dtrend= trend_post - trend_pre
label var dtrend "Change in Trend Pre-Post"

gen cycle_pre = beta_pre
gen cycle_post= beta_post
gen dcycle= beta_post - beta_pre
label var dcycle "Change in Cycle Pre-Post"
sort fips

merge fips using change_and_postlevels
label var dunion "Change in Unionization Rate 1964 to 2010"
 sum cons* beta* trend* cycle* dtrend dcycle dunion dsman l_union l_manemp 
        
tab _m
drop _m
/*keep if trend_post <8 */

foreach y in trend cycle {

	reg d`y' dunion 
	estimates store d`y'_dunion

	reg d`y' l_union 
	estimates store d`y'_l_union

	reg d`y' dunion l_union 
	estimates store d`y'_dl_union


	reg d`y' dunion dsman
	estimates store d`y'_dboth

	reg d`y' l_union l_manemp
	estimates store d`y'_lboth

	reg d`y' dunion l_union dsman l_manemp
	estimates store d`y'_dlboth
}

#delimit ;
esttab  dtrend* dcycle* using dtrend_dcycle_regs.csv, replace cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) constant
starlevels(* 0.10 ** 0.05 *** 0.01) stats(N r2, fmt(%9.0g) label("Observations")) style(tex) label;
#delimit cr
graph matrix dcycle  dunionrate dsmanemp l_union l_manemp 
graph matrix dtrend  dunionrate dsmanemp l_union l_manemp if trend_post <8

log c

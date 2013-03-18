set more off
cap log c
log using datasetup.log, replace

cd k:\lynn\okuns
/**********************READ IN RAW DATA****************************************/

/*********************UNEMPLOYMENT DATA**********************************/
clear
insheet using "UE data by state 1964-2010.csv"
drop v50
sort fips area
rename area state
reshape long ue, i(fips) j(year)

save unemp1964_2010, replace

/*********************REAL GDP DATA************************/
clear
insheet using ratio_RGDP2005to1997dollars.csv
sort fips 
rename area state
save ratioRGDP2005to1997, replace

clear

insheet using RGDP_BEA_1987_2010.csv
d
local y=1964
forval v=3/49 {

	rename v`v' rgdp`y' 
	local y=`y'+1
}

d
reshape long rgdp, i(fips) j(year)
label var rgdp "Real Gross Domestic Product 2005$"
rename area state

save RGDP_1987_2010, replace

/****************************CURRENT GDP**********************************/

clear
insheet using CurrentGDP.csv, names
d
drop v50 v51
local y=1964
forval v=3/49 {

	rename v`v' ngdp`y' 
	local y=`y'+1
}
d
reshape long ngdp, i(fips) j(year)
label var ngdp "Nominal Gross Domestic Product"
rename area state
save current_gdp, replace

/*************QUANTITY INDICES*************************************************/

clear
insheet using QI_1977_2010.csv, names
d
local y=1977
forval v=3/36 {

	rename v`v' qi`y' 
	local y=`y'+1
}
reshape long qi, i(fips) j(year)
rename area state
label var qi "State Quantity Index" 
save QI_1977_2010, replace

/**************************************************************/

clear
insheet using QI_77_97based97.csv, names
d
local y=1977
forval v=3/23 {

	rename v`v' qi97_`y' 
	local y=`y'+1
}
reshape long qi97_, i(fips) j(year)
label var qi "State Quantity Index Based 1997"
rename area state 
save QI_77_97based97, replace


/*****************************NATIONAL PRICE DEFLATORS*********************************/
clear
insheet using npd_1964_2010.csv, names
d

label var npd "National Price Deflator"
sort year

save NPD_1964_2010, replace

clear
/*****************UNIONIZATION RATES***************************************/
insheet using unions.csv
d

gen dunionrate= mem10-mem64 /*change in unionization rate over period in each state.*/
sort fips 
forval y=0/9 {

	rename mem0`y' mem200`y'
}
rename mem10 mem2010

forval y=64/99 {

	rename mem`y' mem19`y'
}

reshape long mem, i(fips) j(year)
rename area state
save unions, replace

/******MANUFACTURING AND TOTAL EMPLOYMENT**********/
clear

insheet using total_state_employment1964_2010.csv
d
drop v50
reshape long annual, i(fips) j(year)
rename annual totemp
label var totemp "Total State Employment"
rename area state
save totalemployment, replace

clear
insheet using State_manufacturing_employment1964_2010.csv
d

reshape  long  annual, i(fips) j(year)
rename annual manemp
label var manemp "State Manufacturing Employment"
rename area state

save manufacturingemployment, replace

/********Merging DATA SETS*******/

use current_gdp, clear
sort year

merge year using NPD_1964_2010
tab _m
tab _m  year if _m!=3
drop _m
sort fips year

merge  fips year using QI_1977_2010
tab _m
tab _m year if _m!=3
drop _m
d
sort fips year

merge  fips year using QI_77_97based97
tab _m
tab year _m if _m!=3
drop _m
d
sort fips year


merge fips year using RGDP_1987_2010
tab _m
tab _m  state if _m!=3
drop _m

sort fips year
merge fips  using ratioRGDP2005to1997
tab _m
tab _m  state if _m!=3
drop _m

sort fips year
merge fips year using unemp1964_2010
tab _m
tab _m  state if _m!=3
drop _m

sort fips year
merge fips year using unions
tab _m
tab _m  state if _m!=3
drop _m


sort fips year
merge fips year using totalemployment
tab _m
tab _m state if _m!=3
drop if _m==2
drop _m

sort fips year
merge fips year using manufacturingemployment
tab state _m if _m!=3
drop _m


/*converting Real GDP in 97$ to real GDP in 2005$ for 1987 to 1997*/
/*Note: the ratio used is the ratio of GDP in 1997 in 2005$ to 1997$ by state*/

gen rgdp2 = rgdp *ratio if year <= 1997
replace rgdp=rgdp2 if year <= 1997

tab year, sum(rgdp)


save GDPdata1964_2010, replace

drop if fips==2|fips==11|fips==0

/*** Estimate of Real GDP for the years 1964 to 1986*/

gen temp_gdp87 = rgdp[_n] if year == 1987 /*real GDP in 1987 for each state*/
egen rgdp87 = max(temp_gdp87), by(state)  /*creates a constant of real GDP in 1987 for each state*/
drop temp_gdp

gen temp_qi87=qi[_n] if year==1987  
egen qi87=max(temp_qi87), by(state)
drop temp_qi

gen e_rgdp= (qi/qi87)*rgdp87 if year < 1987
replace rgdp=e_rgdp if year >= 1977 & year <1987 


/***Calculating STATE LEVEL PRICE DEFLATORS*****/

gen spd= ngdp/rgdp  
label var spd "State Price Deflator"

replace npd =npd/100

/********************Extrapolations******************/

drop if fips==11 /*DC*/
gen spd2=spd
gen pd_yr=.
foreach y in 1	2	4	5	6	8	9	10	12	13	15	16	17	18	19	20	21	22	23	24	25	26	27	28	29	30	31	32	33	34	35	36	37	38	39	40	41	42	44	45	46	47	48	0	49	50	51	53	54	55	56 {

	ipolate spd year if fips==`y' & year <= 2005, gen(pd`y') epolate
	ipolate spd npd if fips==`y' & year <= 2005, gen(spd_npd`y') epolate
	
	replace spd2 = spd_npd`y' if fips==`y' & spd==.
	replace pd_yr=pd`y' if fips==`y' &  spd==.
	drop spd_npd`y' pd`y'

		
}
	label var spd2 "Estimate of State PD using National PD"
	label var pd_yr "Estimate of State State PD using year"

replace rgdp = ngdp/spd2 if year < 1977 

/*Estimate of annual percent change in QI and percent change in RGDP*/
sort fips year
gen pc_qi= ((qi[_n]/qi[_n-1]) - 1) *100 if year!=1964
gen pc_rgdp= ((rgdp[_n]/rgdp[_n-1]) - 1)*100 if year!=1964
label var pc_rgdp "Annual Percent Change in Real GDP$2005"

sort fips year

gen due = ue[_n]-ue[_n-1] if year!=1964 
label var due "Percentage Point Change in UE"

recode year (1964/1985=1 "1964-1985")  (1986/2010=2 "1986-2010") , gen(period)
recode year (1964/1979=1 "1964-1979")  (1980/1989=2 "1980-1989") (1990/2010=3), gen(decade)

gen lnue=log(ue)
gen lnrgdp=log(rgdp)
gen lndue =log(due)


/*****change in unionization rates: divide sample by 50th percentile*/
xtile mu= dunionrate, nq(2)
gen u1 = dunion <= -15
gen u2 = dunion >-15 & dunion < 0
gen u3 = dunion >=0


save GDP_spd_extrapolated, replace

/********Create Variables for Merging to Saved REgression Coefficients in okuns_regs2stages.do ***********/

gen s_manemp = manemp/totemp
sum s_manemp

egen l_manemp=mean(s_manemp), by(period fips)
label var l_manemp "Mean Manufacturing Share of Employment Post" /*This ends up being only the post value because later I keep only the 2010 values*/

egen l_union=mean(mem), by(period fips)
label var l_union "Mean Unionization Rate Post"   /*See previous comment*/

gen temp2010=s_manemp if year==2010
egen s_man2010=max(temp2010), by(fips)
drop temp2010

gen temp1964=s_manemp if year==1964
egen s_man1964=max(temp1964), by(fips)
drop temp1964

gen dsmanemp=s_man2010 - s_man1964
label var dsmanemp "Change in Share of Manufacturing Employment"

keep if year==2010
keep dunion dsmanemp state fips l_union l_manemp 
sort fips
save change_and_postlevels, replace
log c


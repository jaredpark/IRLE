cd k:\gsr\lynn\okuns
/****	NOTE: ALL OF THIS CODE IS NOW IN ONE DO FILE CALLED DATASETUP.DO NO REASON TO RUN SEPARATELY****/

use GDPdata1964_2010, clear
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

sort fips year

gen due = ue[_n]-ue[_n-1] if year!=1964 
label var due "Percentage Point Change in UE"

recode year (1964/1985=1 "1964-1985")  (1986/2010=2 "1986-2010") , gen(period)
recode year (1964/1979=1 "1964-1979")  (1980/1989=2 "1980-1989") (1990/2010=3), gen(decade)

gen lnue=log(ue)
gen lnrgdp=log(rgdp)
gen lndue =log(due)


/*****unionization rates: divide sample by 50th percentile*/
xtile mu= dunionrate, nq(2)

save GDP_spd_extrapolated, replace

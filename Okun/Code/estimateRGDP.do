cap log c
log using datasetup.log, replace
/****	NOTE: ALL OF THIS CODE IS NOW IN ONE DO FILE CALLED DATASETUP.DO NO REASON TO RUN SEPARATELY****/
cd k:\gsr\lynn\okuns

use GDPdata1964_2010, clear

/*** Estimate of Real GDP for the years 1964 to 1986*/

gen temp_gdp87 = rgdp[_n] if year == 1987 /*real GDP in 1987 for each state*/
egen rgdp87 = max(temp_gdp87), by(state)  /*creates a constant of real GDP in 1987 for each state*/
drop temp_gdp

gen temp_qi87=qi[_n] if year==1987  
egen qi87=max(temp_qi87), by(state)
drop temp_qi

gen e_rgdp= (qi/qi87)*rgdp87 if year < 1987
replace rgdp=e_rgdp if year >= 1977 & year <1987 

/*Estimate of annual percent change in QI and percent change in RGDP*/

gen pcg_qi= qi[_n]/qi[_n-1] - 1
gen pcg_rgdp= rgdp[_n]/rgdp[_n-1] - 1

/***Calculating STATE LEVEL PRICE DEFLATORS*****/
gen spd= ngdp/rgdp  
label var spd "State Price Deflator"

replace npd =npd/100

save GDP_price_deflators1964_2010, replace


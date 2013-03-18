use C:\Users\scholl\AppData\Local\Temp\okunwunions.dta , clear
format employment %15.0g
gen unionmembers = (unionization * employment)/100
collapse (sum) employment unionmember, by(year)
format unionmember %15.0g
outsheet employm unionm year using unionmembers, c replace

cd k:\lynn\okuns
cap log c
log using okuns_regressions.log, replace

use GDP_spd_extrapolated, clear
drop if pc_rgdp >30 & pc_rgdp!=.
estimates clear
gen pc_rgdp2=pc_rgdp^2


sum  rgdp  pc_rgdp dunionrate ue due mem

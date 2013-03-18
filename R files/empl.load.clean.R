setwd('~/Projects/Reich/Okun/productivity')
empl = read.table('employment76to10.txt', header = F, sep = "")
rawearlyempl = read.table('earlyempldata.csv', header = F, sep = ",")
rawearlyempl = 1000*earlyempl

setwd('~/Projects/Reich/R files')
load('.Rdata')

empl = empl[,-1]
row.names(empl) = state.name
names(empl) = 1976:2010

for (i in 1:ncol(empl)){
  empl[,i] = as.numeric(gsub('\\(.+', '', empl[,i]))
}

row.names(rawearlyempl) = state.name
names(rawearlyempl) = 1964:1975

tot.empl = cbind(earlyempl, empl)

empl.column = c()
included = which(is.element(row.names(tot.empl), unique(dat$state)))
for (i in 1:length(included)){
  statevec = as.numeric(tot.empl[included[i], ])
  empl.column = c(empl.column, statevec)
}

dat$empl = empl.column/1000

# this raw data needs to be scaled up to account for lack of farm data:

ngdp = new$ngdp
dat$ngdp = ngdp
setwd('~/Projects/Reich/Okun/productivity')

farm.dat = read.table('farm.ngdp.64to97.txt', header = T, sep = ",")
setwd('~/Projects/Reich/R files')
rownames(farm.dat) = as.character(farm.dat[,2])
farm.dat = farm.dat[,-c(1,2)]
names(farm.dat) = 1964:1997
farm.ngdp = farm.dat
f.ngdp = c()
included = which(is.element(row.names(farm.ngdp), unique(dat$state)))
for (i in 1:length(included)){
  statevec = c(as.numeric(farm.ngdp[included[i], ]), rep(NA, 13))
  f.ngdp = c(f.ngdp, statevec)
}
dat$f.ngdp = f.ngdp

farm.pct = f.ngdp/ngdp #/.75
dat$f.pct = farm.pct

# farm.pct is the percent of state/year ngdp produced by farms. I scale the empl column
# for all years 1964:1975 to reflect the data lacking farm employment totals:

# emp*farm.pct = farm.emp; emp - farm.emp = curr.empl; so emp = curr.empl/(1-farm.pct)

scaled.empl = dat$empl/(1-farm.pct)
scrubbed.empl = ifelse(dat$year < 1976, scaled.empl, dat$empl)
s.prod = dat$rgdp/scrubbed.empl
dat$s.prod = s.prod

productivity = ifelse(!is.na(dat$empl), dat$rgdp/dat$empl, NA)
dat$productivity = productivity
#rgdp is in millions, employment in thousands; productivity is in thousands per employed
hist(na.exclude(productivity))
hist(log(na.exclude(productivity))) #normallizes the data
hist(log(dat$rgdp)) #same result

d.productivity = c(dat$productivity, NA) - c(NA, dat$productivity)
d.productivity[1+47*(0:45)] = NA
d.productivity = d.productivity[-length(d.productivity)]
dat$d.prod = d.productivity

pct.d.productivity = (c(dat$productivity, NA) - c(NA, dat$productivity)) / c(dat$productivity, NA)
# pct.d.productivity = (c(dat$productivity, NA) - c(NA, dat$productivity)) / c(NA, dat$productivity)
# because 76 is the first year; was '1+' which references the year 1964 - now 13+
pct.d.productivity[13+47*(0:45)] = NA
pct.d.productivity = pct.d.productivity[-length(pct.d.productivity)]
dat$pct.d.prod = pct.d.productivity*100



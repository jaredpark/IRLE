dat.backup.July10 = dat

load('.Rdata')

setwd('~/Projects/Reich/R Files')

data_3_30 <- read.delim("~/Projects/Reich/wages vs ue/data_3_30.txt")

dat = data_3_30
dat = dat[-which(is.na(dat$totemp)),]
dat = dat[, c(2, 3, 8, 10, 12, 15)]
dat$yr = rep(1:47, 49)

setwd('~/Projects/Reich/Okun/productivity')

LAUS.empl = read.table('employment76to10.txt', header = F, sep = "")
CPS.empl = read.table('nonfarmCESempl.csv', header = F, sep = ",")
setwd('~/Projects/Reich/R files')
LAUS.empl = LAUS.empl[,-1]
row.names(LAUS.empl) = state.name
row.names(CPS.empl) = state.name
names(LAUS.empl) = 1976:2010
names(CPS.empl) = 1964:2010
for (i in 1:ncol(LAUS.empl)){
  LAUS.empl[,i] = as.numeric(gsub('\\(.+', '', LAUS.empl[,i]))
}
for (i in 1:ncol(CPS.empl)){
  CPS.empl[,i] = as.numeric(gsub('\\(.+', '', CPS.empl[,i]))
}
LAUS.column = c()
included = which(is.element(row.names(LAUS.empl), as.character(unique(dat$state))))
for (i in 1:length(included)){
  statevec = c(rep(NA, 12), as.numeric(LAUS.empl[included[i], ]))
  LAUS.column = c(LAUS.column, statevec)
}
CPS.column = c()
included = which(is.element(row.names(CPS.empl), unique(dat$state)))
for (i in 1:length(included)){
  statevec = as.numeric(CPS.empl[included[i], ])
  CPS.column = c(CPS.column, statevec)
}
CPS.column = CPS.column*1000
dat$LAUS.empl = LAUS.column
dat$CPS.empl = CPS.column


setwd('~/Projects/Reich/Okun/productivity')

bea.empl.1 = read.table('SA25.empl.69to01.txt', header = F, sep = ",")
bea.empl.1 = bea.empl.1[is.element(bea.empl.1[,2], unique(dat$state)),]
rownames(bea.empl.1) = bea.empl.1[,2]
bea.empl.1 = bea.empl.1[,-c(1,2)]
names(bea.empl.1) = 1969:2001
bea.empl.2 = read.table('SA25.empl.90to10.txt', header = F, sep = ",")
bea.empl.2 = bea.empl.2[is.element(bea.empl.2[,2], unique(dat$state)),]
rownames(bea.empl.2) = bea.empl.2[,2]
bea.empl.2 = bea.empl.2[,-c(1,2)]
names(bea.empl.2) = 1990:2010
setwd('~/Projects/Reich/R files')

sum(bea.empl.1[,22:33] == bea.empl.2[,1:12])/length(bea.empl.1[,22:33] == bea.empl.2[,1:12])

bea.empl = cbind(bea.empl.1, bea.empl.2[,13:21])

bea.empl.col = c()
for (i in 1:nrow(bea.empl)){
  state.vec = c(rep(NA, 5), as.numeric(bea.empl[i,]))
  bea.empl.col = c(bea.empl.col, state.vec)
}
dat$bea.empl = bea.empl.col
dat$productivity = 1000*dat$rgdp/dat$bea.empl

#checking for outliers in productivity:

par(mfrow = c(3,1), mar = c(2.5,4,2.5,2))

for (i in 1:49){
  plot(1969:2010, dat$productivity[is.element(dat$year, 1969:2010) & dat$state == unique(dat$state)[i]],
       ylab = 'productivity', xlab = '', main =  unique(dat$state)[i],
       ylim = c(30,110))
  abline(h = min(dat$productivity[is.element(dat$year, 1969:2010) & dat$state == unique(dat$state)[i]]), col = 'red', lty = 2)
  abline(h = max(dat$productivity[is.element(dat$year, 1969:2010) & dat$state == unique(dat$state)[i]]), col = 'red', lty = 2)
}

par(mfrow = c(3,1), mar = c(3,4,2,2))
prev.outliers = c('Louisiana', 'North Dakota', 'Wyoming')
for (i in 1:3){
  plot(1969:2010, dat$productivity[is.element(dat$year, 1969:2010) & dat$state == prev.outliers[i]],
       ylab = 'productivity', xlab = '', main = prev.outliers[i],
       ylim = c(30,100))
  abline(h = min(dat$productivity[is.element(dat$year, 1969:2010) & dat$state == prev.outliers[i]]), col = 'red', lty = 2)
  abline(h = max(dat$productivity[is.element(dat$year, 1969:2010) & dat$state == prev.outliers[i]]), col = 'red', lty = 2)
}


k.wk.beg.and.end.avgs()
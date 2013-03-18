setwd('~/Projects/Reich/Okun/productivity')
rtw = read.csv('RtWdata.csv', header = F)
rtw = rtw[,-3]
setwd('~/R files/Okun')
load('.Rdata')

rtw = rtw[is.element(rtw[,1], unique(dat$state)),]
newcol = as.character(unique(dat$state)[!is.element(unique(dat$state), rtw[,1])])
newcol2 = rep(2012, 26)
newdat = data.frame('V1' = newcol, 'V2' = newcol2)
newrtw = rbind(rtw, newdat)
row.names(newrtw) = 1:46
sum(newrtw[order(as.character(newrtw[,1])), 1] ==as.character(unique(dat$state)))
newrtw = newrtw[order(as.character(newrtw[,1])), ]

vec = c()
rtw.column = c()
for (i in 1:nrow(newrtw)){
  statevec = ifelse(1964:2010<newrtw[i,2], 0, 1)
  rtw.column = c(rtw.column, statevec)
}

dat$rtw = rtw.column
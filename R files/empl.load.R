setwd('~/Projects/Reich/Okun/productivity')
LAUS.empl = read.table('employment76to10.txt', header = F, sep = "")
CPS.empl = read.table('nonfarmCESempl.csv', header = F, sep = ",")
setwd('~/Projects/Reich/R files')
LAUS.empl = LAUS.empl[,-1]
row.names(LAUS.empl) = state.name
row.names(CPS.empl) = state.name
names(LAUS.empl) = 1976:2010
names(CPS.empl) = 1964:2010
load('.Rdata')
for (i in 1:ncol(LAUS.empl)){
  LAUS.empl[,i] = as.numeric(gsub('\\(.+', '', LAUS.empl[,i]))
}
for (i in 1:ncol(CPS.empl)){
  CPS.empl[,i] = as.numeric(gsub('\\(.+', '', CPS.empl[,i]))
}
LAUS.column = c()
included = which(is.element(row.names(LAUS.empl), unique(dat$state)))
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
dat$productivity = 1000*dat$rgdp/dat$LAUS.empl
simple.scale = mean(na.exclude(LAUS.column/CPS.column))
dat$est.LAUS.empl = dat$CPS.empl*simple.scale
dat$est.prod = dat$rgdp/dat$est.LAUS.empl*1000 # to make prod in $1000/employee

########################
####### new empl #######
########################
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

sum(bea.empl.1[,22:33] == bea.empl.2[,1:12])

bea.empl = cbind(bea.empl.1, bea.empl.2[,13:21])

bea.empl.col = c()
for (i in 1:nrow(bea.empl)){
  state.vec = c(rep(NA, 5), as.numeric(bea.empl[i,]))
  bea.empl.col = c(bea.empl.col, state.vec)
}
dat$bea.empl = bea.empl.col
dat$productivity = 1000*dat$rgdp/dat$bea.empl


par(mfrow = c(2,1), mar = c(4, 6, 2, 4))
k = 5

prod.dat = k.wk.beg.and.end.avgs(dat, 'productivity', k, 1969, 2010)
d.prod = 100*(prod.dat[,2]-prod.dat[,1])/prod.dat[,1]

mem.dat = k.wk.beg.and.end.avgs(dat, 'mem', k, 1969, 2010)
d.mem = -(mem.dat[,2]-mem.dat[,1])

xy.fit.line(d.mem, d.prod, text = T,
            maintext = '1969 to 2010, 5 yr avgs', xtext = 'Pct point d.union share',
            ytext = '% d.prod (RGDP/employed)')
text(22, 59, '    slope = -1.537 \n p value = 0.0066', cex = .7)

k = 3

prod.dat = k.wk.beg.and.end.avgs(dat, 'productivity', k, 1969, 2010)
d.prod = 100*(prod.dat[,2]-prod.dat[,1])/prod.dat[,1]

mem.dat = k.wk.beg.and.end.avgs(dat, 'mem', k, 1969, 2010)
d.mem = -(mem.dat[,2]-mem.dat[,1])

xy.fit.line(d.mem, d.prod, text = T,
            maintext = '1969 to 2010, 3 yr avgs', xtext = 'Pct point d.union share',
            ytext = '% d.prod (RGDP/employed)')
text(23, 63, '    slope = -1.457 \n p value = 0.0104', cex = .7)

end.early = 1984
begin.late = end.early+1
k = 3
early.prod = k.wk.beg.and.end.avgs(dat, 'productivity', k, 1969, end.early)
late.prod = k.wk.beg.and.end.avgs(dat, 'productivity', k, begin.late, 2010)
early.d.prod = 100*(early.prod[,2]-early.prod[,1])/early.prod[,1]
late.d.prod = 100*(late.prod[,2]-late.prod[,1])/late.prod[,1]

early.mem = k.wk.beg.and.end.avgs(dat, 'mem', k, 1969, end.early)
late.mem = k.wk.beg.and.end.avgs(dat, 'mem', k, begin.late, 2010)
early.d.mem = -(early.mem[,2]-early.mem[,1])
late.d.mem = -(late.mem[,2]-late.mem[,1])

par(mfrow = c(2,1), mar = c(4, 6, 2, 4))
xy.fit.line(early.d.mem, early.d.prod, text = T,
            maintext = '1969 to 1984, 3 yr avgs', xtext = 'Pct point d.union share',
            ytext = '% d.prod (RGDP/employed)')
text(-1.2, 5, '    slope = -0.829 \n p value = 0.0153', cex = .7)
xy.fit.line(late.d.mem, late.d.prod, text = T,
            maintext = '1985 to 2010, 3 yr avgs', xtext = 'Pct point d.union share',
            ytext = '% d.prod (RGDP/employed)')
text(1.2, 60, '    slope = -2.472 \n p value = 0.0019', cex = .7)

end.early = 1980
begin.late = end.early+1
k = 3
early.prod = k.wk.beg.and.end.avgs(dat, 'productivity', k, 1969, end.early)
late.prod = k.wk.beg.and.end.avgs(dat, 'productivity', k, begin.late, 2010)
early.d.prod = 100*(early.prod[,2]-early.prod[,1])/early.prod[,1]
late.d.prod = 100*(late.prod[,2]-late.prod[,1])/late.prod[,1]

early.mem = k.wk.beg.and.end.avgs(dat, 'mem', k, 1969, end.early)
late.mem = k.wk.beg.and.end.avgs(dat, 'mem', k, begin.late, 2010)
early.d.mem = -(early.mem[,2]-early.mem[,1])
late.d.mem = -(late.mem[,2]-late.mem[,1])

par(mfrow = c(2,1), mar = c(4, 6, 2, 4))
xy.fit.line(early.d.mem, early.d.prod, text = T,
            maintext = '1969 to 1980, 3 yr avgs', xtext = 'Pct point d.union share',
            ytext = '% d.prod (RGDP/employed)')
text(8.4, -1, '    slope = -0.919 \n p value = 0.0107', cex = .7)
xy.fit.line(late.d.mem, late.d.prod, text = T,
            maintext = '1981 to 2010, 3 yr avgs', xtext = 'Pct point d.union share',
            ytext = '% d.prod (RGDP/employed)')
text(16, 19, '    slope = -1.863 \n p value = 0.0054', cex = .7)

end.early = 1989
begin.late = end.early+1
k = 3
early.prod = k.wk.beg.and.end.avgs(dat, 'productivity', k, 1969, end.early)
late.prod = k.wk.beg.and.end.avgs(dat, 'productivity', k, begin.late, 2010)
early.d.prod = 100*(early.prod[,2]-early.prod[,1])/early.prod[,1]
late.d.prod = 100*(late.prod[,2]-late.prod[,1])/late.prod[,1]

early.mem = k.wk.beg.and.end.avgs(dat, 'mem', k, 1969, end.early)
late.mem = k.wk.beg.and.end.avgs(dat, 'mem', k, begin.late, 2010)
early.d.mem = -(early.mem[,2]-early.mem[,1])
late.d.mem = -(late.mem[,2]-late.mem[,1])

par(mfrow = c(2,1), mar = c(4, 6, 2, 4))
xy.fit.line(early.d.mem, early.d.prod, text = T,
            maintext = '1969 to 1989, 3 yr avgs', xtext = 'Pct point d.union share',
            ytext = '% d.prod (RGDP/employed)')
text(.5, 19, '    slope = -1.360 \n p value = 0.0041', cex = .7)
xy.fit.line(late.d.mem, late.d.prod, text = T,
            maintext = '1990 to 2010, 3 yr avgs', xtext = 'Pct point d.union share',
            ytext = '% d.prod (RGDP/employed)')
text(1.4, 58, '    slope = -2.325 \n p value = 0.0151', cex = .7)

end.early = 1984
begin.middle = end.early+1
end.middle = 1997
begin.late = end.middle+1
k = 3
# 83, 97, k = 4; 85, 98, k = 3

early.prod = k.wk.beg.and.end.avgs(dat, 'productivity', k, 1969, end.early)
mid.prod = k.wk.beg.and.end.avgs(dat, 'productivity', k, begin.middle, end.middle)
late.prod = k.wk.beg.and.end.avgs(dat, 'productivity', k, begin.late, 2010)
early.d.prod = 100*(early.prod[,2]-early.prod[,1])/early.prod[,1]
mid.d.prod = 100*(mid.prod[,2]-mid.prod[,1])/mid.prod[,1]
late.d.prod = 100*(late.prod[,2]-late.prod[,1])/late.prod[,1]

early.mem = k.wk.beg.and.end.avgs(dat, 'mem', k, 1969, end.early)
mid.mem = k.wk.beg.and.end.avgs(dat, 'mem', k, begin.middle, end.middle)
late.mem = k.wk.beg.and.end.avgs(dat, 'mem', k, begin.late, 2010)
early.d.mem = -(early.mem[,2]-early.mem[,1])
mid.d.mem = -(mid.mem[,2]-mid.mem[,1])
late.d.mem = -(late.mem[,2]-late.mem[,1])

par(mfrow = c(3,1), mar = c(4, 6, 1.8, 4))
xy.fit.line(early.d.mem, early.d.prod, text = T,
            maintext = '1969 to 1984, 3 yr avgs', xtext = 'Pct point d.union share',
            ytext = '% d.prod (RGDP/employed)')
text(-1.5, 10, '    slope = -0.829 \n p value = 0.0153', cex = .7)

xy.fit.line(mid.d.mem, mid.d.prod, text = T,
            maintext = '1985 to 1997, 3 yr avgs', xtext = 'Pct point d.union share',
            ytext = '% d.prod (RGDP/employed)')
text(.2, 11, '    slope = -1.524 \n p value = 0.0073', cex = .7)

xy.fit.line(late.d.mem, late.d.prod, text = T,
            maintext = '1998 to 2010, 3 yr avgs', xtext = 'Pct point d.union share',
            ytext = '% d.prod (RGDP/employed)')
text(-1.1, 9, '  slope = -1.389 \n p value = 0.0231', cex = .7)
dev.off()

end.early = 1979
begin.middle = end.early+1
end.middle = 1989
begin.middle.2 = end.middle+1
end.middle.2 = 1999
begin.late = end.middle.2+1
k = 2
par(mfrow = c(2,1))
  early.prod = k.wk.beg.and.end.avgs(dat, 'productivity', k, 1969, end.early)
  mid.prod = k.wk.beg.and.end.avgs(dat, 'productivity', k, begin.middle, end.middle)
  mid.prod.2 = k.wk.beg.and.end.avgs(dat, 'productivity', k, begin.middle.2, end.middle.2)
  late.prod = k.wk.beg.and.end.avgs(dat, 'productivity', k, begin.late, 2010)
  early.d.prod = 100*(early.prod[,2]-early.prod[,1])/early.prod[,1]
  mid.d.prod = 100*(mid.prod[,2]-mid.prod[,1])/mid.prod[,1]
  mid.d.prod.2 = 100*(mid.prod.2[,2]-mid.prod.2[,1])/mid.prod.2[,1]
  late.d.prod = 100*(late.prod[,2]-late.prod[,1])/late.prod[,1]
  
  early.mem = k.wk.beg.and.end.avgs(dat, 'mem', k, 1969, end.early)
  mid.mem = k.wk.beg.and.end.avgs(dat, 'mem', k, begin.middle, end.middle)
  mid.mem.2 = k.wk.beg.and.end.avgs(dat, 'mem', k, begin.middle.2, end.middle.2)
  late.mem = k.wk.beg.and.end.avgs(dat, 'mem', k, begin.late, 2010)
  early.d.mem = -(early.mem[,2]-early.mem[,1])
  mid.d.mem = -(mid.mem[,2]-mid.mem[,1])
  mid.d.mem.2 = -(mid.mem.2[,2]-mid.mem.2[,1])
  late.d.mem = -(late.mem[,2]-late.mem[,1])

par(mfrow = c(2,1))
xy.fit.line(early.d.mem, early.d.prod, text = T,
            maintext = '1969 to 1979, 3 yr avgs', xtext = 'Pct point d.union share',
            ytext = '% d.prod (RGDP/employed)')
text(8.5, -3, '    slope = -0.778 \n p value = 0.0328', cex = .7)

xy.fit.line(mid.d.mem, mid.d.prod, text = T,
            maintext = '1980 to 1989, 3 yr avgs', xtext = 'Pct point d.union share',
            ytext = '% d.prod (RGDP/employed)')
text(8.5, -3, '    slope = -0.538 \n p value = 0.161', cex = .7)

xy.fit.line(mid.d.mem.2, mid.d.prod.2, text = T,
            maintext = '1990 to 1999, 3 yr avgs', xtext = 'Pct point d.union share',
            ytext = '% d.prod (RGDP/employed)')
text(8.5, -3, '    slope = -1.859 \n p value = 0.0058', cex = .7)

xy.fit.line(late.d.mem, late.d.prod, text = T,
            maintext = '2000 to 2010, 3 yr avgs', xtext = 'Pct point d.union share',
            ytext = '% d.prod (RGDP/employed)')
text(8.5, -3, '    slope = -1.859 \n p value = 0.0058', cex = .7)

rtw.states = unique(dat$state)[which(rtw.column[dat$year == 2010] == 1)]
rtw.index = is.element(unique(dat$state), rtw.states)
rtw.col = rgb(.7, .1, .1)
n.rtw.col = rgb(.1, .1, .7)
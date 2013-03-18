# 76-78 ETRP to be used to scale from payroll data to LAUS data

etrp.dat = read.csv('~/Projects/Reich/Okun/productivity/ETRP 76-78.csv', header = T)
etrp.dat = etrp.dat[is.element(unique(etrp.dat[,1]), unique(dat$state)),]
rownames(etrp.dat) = etrp.dat[,1]
etrp.dat = etrp.dat[,-1]
names(etrp.dat) = 1976:1978

ratio.num = data.frame(dat$emp[dat$year == 1976],
                       dat$emp[dat$year == 1977],
                       dat$emp[dat$year == 1978])
names(ratio.num) = names(etrp.dat)
rownames(ratio.num) = unique(dat$state)

ratio.dat = ratio.num/etrp.dat
int.slope.pvalue = matrix(rep(NA), nrow = nrow(ratio.dat), ncol = 3)
for (i in 1:nrow(ratio.dat)){
  lf = lm(as.numeric(ratio.dat[i,])~c(1,2,3))
  int.slope.pvalue[i, 1:2] = as.numeric(lf$coef)
  int.slope.pvalue[i, 3] = summary(lf)$coef[2,4]
}
ratio.dat$int = int.slope.pvalue[,1]
ratio.dat$slope = int.slope.pvalue[,2]
ratio.dat$pvalue = int.slope.pvalue[,3]

write.table(ratio.dat, file = 'conversion.data.txt')


#############
#   6/23 approach; using BLS CES nonfarm empl:
#  1. Have 76-2010 total employment (LAUS)
#  2. Have 64-75 NONFARM employment (CPS)
#  3. Want 64-75 total employment:
#     a) Strategy is to find a scaling factor for each state such that:
#         total empl_s/nonfarm empl_s = R_s
#         and so nonfarm empl_s*R_s = total empl_s with _s marking a state level statistic
#  4. I will test the method by:
#     a) Determine each R_s with 81-85 data
#     b) predict total empl_s for each state for 76-80
#     c) inspect est total empl_s vs total empl_s for possible bias of the method
#     d) present summary statistics of estimated total employment
#     e) if results are acceptable, apply the method to the unknown 64-75 data, determining R_s with 76-80 data.
#############


#function to find average(s) of ratio between two columns in a df, return
#a vector with R_s for each state based on years X to Y

mean.ratio.over.range = function(dat, numerator.col, denominator.col, begin, end){
  l = length(unique(dat$state))
  year.range = begin:end
  R_s.vector = rep(NA, l)
  for (state in 1:l){
    index = dat$state == unique(dat$state)[state] & is.element(dat$year, year.range)
    R_s.vector[state] = mean(dat[index, numerator.col]/dat[index, denominator.col])
  }
  return(R_s.vector)
}
R_s.81to85 = mean.ratio.over.range(dat, 'LAUS.empl', 'CPS.empl', 1981, 1985)
a.76to80 = dat$LAUS.empl[is.element(dat$year, 1976:1980)]
nonfarmempl.76to80 = dat$CPS.empl[is.element(dat$year, 1976:1980)]
expanded.R_s.81to85 = rep(R_s.81to85, each = 5)
p.76to80 = nonfarmempl.76to80*expanded.R_s.81to85
pct.err.76to80 = 100*(p.76to80-a.76to80)/a.76to80
ln.ratio.76to80 = 100*log(p.76to80/a.76to80)
raw.pct.matrix = matrix(pct.err.76to80, nrow = length(unique(dat$state)), byrow = T)
ln.matrix = matrix(ln.ratio.76to80, nrow = length(unique(dat$state)), byrow = T)
pct.matrix = cbind(raw.pct.matrix, apply(raw.pct.matrix, 1, mean), apply(raw.pct.matrix,1,sd))
pct.matrix = rbind(pct.matrix, apply(pct.matrix, 2, mean), apply(pct.matrix, 2, sd))
pct.matrix[47, 7] = NA; pct.matrix[48, 6:7] = NA
rownames(pct.matrix) = c(unique(dat$state), 'mean', 'sd')
colnames(pct.matrix) = c(1976:1980, 'mean', 'sd')
# need to write a function to return this pct.matrix starting with making R_s...

tot.empl.extrapolate = function(dat, num.col, denom.col, learn.begin, learn.end, new.begin, new.end){
  R_s = mean.ratio.over.range(dat, num.col, denom.col, begin = learn.begin, end = learn.end)
  new.range = new.begin:new.end
  new.index = is.element(dat$year, new.range)
  actual.empl = dat$LAUS.empl[new.index]
  predicted.empl = dat$CPS.empl[new.index]*rep(R_s, each = length(new.range))
  newdat = cbind(actual.empl, predicted.empl)
  rownames(newdat) = rep(unique(dat$state), each = length(new.range))
  newdat = cbind(rep(new.range, length(unique(dat$state))), newdat)
  newdat[,3] = round(newdat[,3], 0)
  newdat = cbind(newdat, newdat[,3]-newdat[,2])
  newdat = cbind(newdat, round(100*newdat[,4]/newdat[,2], 3))
  newdat = cbind(newdat, round(1000*dat$rgdp[new.index]/newdat[,2], 3), round(1000*dat$rgdp[new.index]/newdat[,3], 3))
  newdat = cbind(newdat, round(100*(newdat[,7]-newdat[,6])/newdat[,6], 2))
  colnames(newdat) = c('year', 'empl.act', 'empl.pred', 'empl.diff', 'pct.empl.diff', 'prod.act', 'prod.pred', 'pct.prod.diff')
  return(newdat)
}

test.extrap = tot.empl.extrapolate(dat, 'LAUS.empl', 'CPS.empl', 1981, 1985, 1976, 1980)
test.extrap

# now I can look at the pct.d.prod vs d.mem plot with my predicted data:
# first to add the predicted productivity, after checking that the actual prod column is correct:
#sum(test.extrap[,6]==round(dat$productivity[is.element(dat$year, 1976:1980)], 3)) # > 230  ... =46*5, good.

# adding the extrap data to the dat df
empl.extrap.column = c()
for (i in 1:length(unique(rownames(test.extrap)))){
  index = rownames(test.extrap)==unique(dat$state)[i]
  statevec = c(rep(NA, 12), test.extrap[index,'prod.pred'], rep(NA, 30))
  empl.extrap.column = c(empl.extrap.column, statevec)
}
dat$extrap.prod = as.numeric(empl.extrap.column)
extrap.index = !is.na(dat$extrap.prod)
extrap.prod.err = dat$extrap.prod[extrap.index] - dat$productivity[extrap.index]
sum(extrap.prod.err^2)/length(extrap.prod.err)

extrap.d.prod = cbind(k.wk.beg.and.end.avgs(dat, 'extrap.prod', 3, 1976, 1980)[,1], 
                      k.wk.beg.and.end.avgs(dat, 'productivity', 3, 1976, 2010))
extrap.data = cbind(100*(extrap.d.prod[,3]-extrap.d.prod[,1])/extrap.d.prod[,1],
                    100*(extrap.d.prod[,3]-extrap.d.prod[,2])/extrap.d.prod[,2])
colnames(extrap.data) = c('pred.pct.d.prod', 'act.prct.d.prod')
mem.avgs = k.wk.beg.and.end.avgs(dat, 'mem', 3, 1976, 2010)
mem.data = -(mem.avgs[,2] - mem.avgs[,1])
par(mfrow = c(2,1))
xy.fit.line(mem.data, extrap.data[,1], text = T,
            xtext = 'Decrease in union share %',
            ytext = '', 
            maintext = '1976 - 2010; Predicted LAUS data')
text(3,60,'slope = -1.615 \n p = 0.034', cex = .55)
xy.fit.line(mem.data, extrap.data[,2], text = T,
            xtext = 'Decrease in union share %',
            ytext = '', 
            maintext = '1976 - 2010; Actual LAUS data')
text(3,50,'slope = -2.02 \n p = 0.009', cex = .55)
extrap.data = cbind(extrap.data, extrap.data[,1] - extrap.data[,2])
extrap.data = cbind(extrap.data, extrap.data[,3]^2)
extrap.data = cbind(extrap.data, 100*extrap.data[,3]/extrap.data[,2])
extrap.data = rbind(extrap.data, apply(extrap.data, 2, mean))
rownames(extrap.data) = c(rownames(extrap.data)[-47], 'mean')
colnames(extrap.data) = c(colnames(extrap.data)[1:2], 'diff', 'pct.diff')
extrap.data = round(extrap.data, 2)

sum(extrap.prod.err^2)/length(extrap.prod.err)/mean(dat$productivity[extrap.index])

# I see that I overpredict the pct change in productivity by about 0.14 on average.
# I will subtract this amount and rerun the regression
xy.fit.line(mem.data, y = extrap.data[-47,1]-.14, text = T,
            xtext = 'Decrease in union share %',
            ytext = '', 
            maintext = '1976 - 2010; Predicted LAUS data')

extrap.empl.64to75 = tot.empl.extrapolate(dat, 'LAUS.empl', 'CPS.empl', 1976, 1978, 1964, 1975)
empl.extrap.column = c()
for (i in 1:length(unique(rownames(extrap.empl.64to75)))){
  index = rownames(extrap.empl.64to75)==unique(dat$state)[i]
  statevec = c(extrap.empl.64to75[index,'prod.pred'], rep(NA, 35))
  empl.extrap.column = c(empl.extrap.column, statevec)
}
dat$extrap.prod = as.numeric(empl.extrap.column)
index = !is.na(dat$extrap.prod)
new.prod.column = ifelse(index, dat$extrap.prod, dat$productivity)
dat$new.prod = new.prod.column

new.prod.avgs = k.wk.beg.and.end.avgs(dat, 'new.prod', 3, 1964, 1985)
prod.data = 100*(new.prod.avgs[,2] - new.prod.avgs[,1])/new.prod.avgs[,1]
mem.avgs = k.wk.beg.and.end.avgs(dat, 'mem', 3, 1964, 1985)
mem.data = -(mem.avgs[,2] - mem.avgs[,1])
hist(prod.data, breaks = 10, xlab = '% change in productivity', main = '1964-1985, Using Estimated Total Employment')
xy.fit.line(mem.data, y = prod.data, text = T,
            xtext = 'Decrease in union share %',
            ytext = '', 
            maintext = '1964 - 1985; Predicted LAUS data')
new.prod.avgs = k.wk.beg.and.end.avgs(dat, 'new.prod', 3, 1964, 2010)
prod.data = 100*(new.prod.avgs[,2] - new.prod.avgs[,1])/new.prod.avgs[,1]
mem.avgs = k.wk.beg.and.end.avgs(dat, 'mem', 3, 1964, 2010)
mem.data = -(mem.avgs[,2] - mem.avgs[,1])
xy.fit.line(mem.data, y = prod.data, text = T,
            xtext = 'Decrease in union share %',
            ytext = '', 
            maintext = '1964 - 2010; Predicted LAUS data')
new.prod.avgs = k.wk.beg.and.end.avgs(dat, 'new.prod', 3, 1985, 2010)
prod.data = 100*(new.prod.avgs[,2] - new.prod.avgs[,1])/new.prod.avgs[,1]
mem.avgs = k.wk.beg.and.end.avgs(dat, 'mem', 3, 1985, 2010)
mem.data = -(mem.avgs[,2] - mem.avgs[,1])
xy.fit.line(mem.data, y = prod.data, text = T,
            xtext = 'Decrease in union share %',
            ytext = '', 
            maintext = '1985 - 2010; Predicted LAUS data')
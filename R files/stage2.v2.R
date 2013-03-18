##################
#
#  Stage 2
#
##################

# 1. Determine trend estimate for each state in the two time periods
# 2. d.trend = trend.post - trend.pre
#    d.cycle = cycle.post - cycle.pre
#    d.union = slopes
#    d.manuf = manufacturing employment share change for each state
#    union.post = ?
#    manuf.post = ?
# 7. ...

##################
#
#  Loading the data
#
##################

setwd('~/R Files/Okun')
load('.Rdata')
View(dat)

state.trends = c()
state.cycle = c()
state.constant = c()
for (i in 1:length(unique(dat$state))){
  index = dat$state == unique(dat$state)[i]
  lmfit = lm(d.ue~pct.d.rgdp, data = dat[index, ])
  state.trends[i] = as.numeric(-lmfit$coef[1]/lmfit$coef[2])
  state.cycle[i] = as.numeric(lmfit$coef[2])
  state.constant[i] = as.numeric(lmfit$coef[1])
}

state.trends.pre = c()
state.cycle.pre = c()
state.constant.pre = c()
for (i in 1:length(unique(dat$state))){
  index = dat$state == unique(dat$state)[i] & dat$year < 1986
  lmfit = lm(d.ue~pct.d.rgdp, data = dat[index, ])
  state.trends.pre[i] = as.numeric(-lmfit$coef[1]/lmfit$coef[2])
  state.cycle.pre[i] = as.numeric(lmfit$coef[2])
  state.constant.pre[i] = as.numeric(lmfit$coef[1])
}

state.trends.post = c()
state.cycle.post = c()
state.constant.post = c()
for (i in 1:length(unique(dat$state))){
  index = dat$state == unique(dat$state)[i] & dat$year >= 1986
  lmfit = lm(d.ue~pct.d.rgdp, data = dat[index, ])
  state.trends.post[i] = as.numeric(-lmfit$coef[1]/lmfit$coef[2])
  state.cycle.post[i] = as.numeric(lmfit$coef[2])
  state.constant.post[i] = as.numeric(lmfit$coef[1])
}

d.state.trends = state.trends.post - state.trends.pre
d.state.cycle = state.cycle.post - state.cycle.pre

d.trend = c()
d.cycle = c()
for (i in 1:length(unique(dat$state))){
  d.trend = c( d.trend, rep(d.state.trends[i], length(unique(dat$year))))
  d.cycle = c( d.cycle, rep(d.state.cycle[i], length(unique(dat$year))))
}
dat$d.trend = d.trend
dat$d.cycle = d.cycle

d.union = c()
for (i in 1:length(unique(dat$state))){
  fit = lm(mem ~ yr, data = dat[dat$state == unique(dat$state)[i], ] )
  d.union[i] = fit$coef[2]
}

union.diff = dat$mem[dat$yr == 47] - dat$mem[dat$yr == 1]
pct.union.diff = union.diff/dat$mem[dat$yr == 1]
pct.d.union = d.union/dat$mem[dat$yr == 1]
union.diff.2 =  dat$mem[dat$yr == 47] - dat$mem[dat$yr == 23]
pct.union.diff.2 = union.diff.2/dat$mem[dat$yr == 23]

d.manuf = c()
for (i in 1:length(unique(dat$state))){
  fit = lm(manuf.share ~ yr, data = dat[dat$state == unique(dat$state)[i], ] )
  d.manuf[i] = fit$coef[2]*100
}

manuf.diff = dat$manuf.share[dat$yr == 47]-dat$manuf.share[dat$yr == 1]
pct.manuf.diff = manuf.diff/dat$manuf.share[dat$yr == 1]
pct.d.manuf = d.manuf/dat$manuf.share[dat$yr == 1]

#have: d.trend, d.cycle, d.union, d.manuf
#need: manuf.post, union.post
# Mean of the last half of the years for these numbers

manuf.post = c()
for (i in 1:length(unique(dat$state))){
  manuf.post[i] = mean(dat$manuf.share[dat$year > 1985 & dat$state == unique(dat$state)[i]])
}

union.post = c()
for (i in 1:length(unique(dat$state))){
  union.post[i] = mean(dat$mem[dat$year > 1985 & dat$state == unique(dat$state)[i]])/100
}

#have all my needed responses and predictors:

##################
#
#  Regressions
#
##################

# WV and IA have change in state trends greater than magnitude 4, outliers
outliers = which(unique(dat$state)=='West Virginia' | unique(dat$state)=='Iowa')

pct.d.trend = d.state.trends/state.trends.pre
pct.d.cycle = d.state.cycle/state.cycle.pre


fits = list()
fits[[1]] = lm(pct.d.trend~pct.union.diff+pct.d.manuf)
fits[[2]] = lm(pct.d.trend~union.post+manuf.post)
fits[[3]] = lm(pct.d.trend~pct.union.diff+pct.d.manuf+union.post+manuf.post)
fits[[4]] = lm(pct.d.cycle~pct.union.diff+pct.d.manuf)
fits[[5]] = lm(pct.d.cycle~union.post+manuf.post)
fits[[6]] = lm(pct.d.cycle~pct.union.diff+pct.d.manuf+union.post+manuf.post)

exclude = c(10,47)
fits[[1]] = lm(pct.d.trend[-exclude]~pct.union.diff[-exclude]+pct.d.manuf[-exclude])
fits[[2]] = lm(pct.d.trend[-exclude]~union.post[-exclude]+manuf.post[-exclude])
fits[[3]] = lm(pct.d.trend[-exclude]~pct.union.diff[-exclude]+pct.d.manuf[-exclude]+union.post[-exclude]+manuf.post[-exclude])
fits[[4]] = lm(pct.d.cycle[-exclude]~pct.union.diff[-exclude]+pct.d.manuf[-exclude])
fits[[5]] = lm(pct.d.cycle[-exclude]~union.post[-exclude]+manuf.post[-exclude])
fits[[6]] = lm(pct.d.cycle[-exclude]~pct.union.diff[-exclude]+pct.d.manuf[-exclude]+union.post[-exclude]+manuf.post[-exclude])

table3out(fits)
write.csv(table3out(fits), file = 't3output_5_15.v1.csv')
table3out = function(fits){

c1 = summary(fits[[1]])$coef
c2 = summary(fits[[2]])$coef
c3 = summary(fits[[3]])$coef
c4 = summary(fits[[4]])$coef
c5 = summary(fits[[5]])$coef
c6 = summary(fits[[6]])$coef

t3output = matrix(rep(NA, length(fits)*11), ncol = length(fits))
t3output[,1] = c(c1[2, 1], c1[2, 2], c1[3, 1], c1[3, 2],
                 NA, NA, NA, NA,
                 c1[1, 1], c1[1, 2], summary(fits[[1]])$r.squared)
t3output[,2] = c(NA, NA, NA, NA,
                 c2[2, 1], c2[2, 2], c2[3, 1], c2[3, 2],
                 c2[1, 1], c2[1, 2], summary(fits[[1]])$r.squared)
t3output[,3] = c(c3[2, 1], c3[2, 2], c3[3, 1], c3[3, 2],
                 c3[4, 1], c3[4, 2], c3[5, 1], c3[5, 2],
                 c3[1, 1], c3[1, 2], summary(fits[[1]])$r.squared)
t3output[,4] = c(c4[2, 1], c4[2, 2], c4[3, 1], c4[3, 2],
                 NA, NA, NA, NA,
                 c4[1, 1], c4[1, 2], summary(fits[[1]])$r.squared)
t3output[,5] = c(NA, NA, NA, NA,
                 c5[2, 1], c5[2, 2], c5[3, 1], c5[3, 2],
                 c5[1, 1], c5[1, 2], summary(fits[[1]])$r.squared)
t3output[,6] = c(c6[2, 1], c6[2, 2], c6[3, 1], c6[3, 2],
                 c6[4, 1], c6[4, 2], c6[5, 1], c6[5, 2],
                 c6[1, 1], c6[1, 2], summary(fits[[1]])$r.squared)
rownames(t3output) = c('d.union', 'se', 'd.manuf', 'se',
                       'union.post', 'se', 'manuf.post', 'se',
                       'constant', 'se', 'r sqrd')
colnames(t3output) = 1:6
return(t3output)
}
write.csv(t3output, 't3output_5_14_v5.csv')

###################
#
# Table 4
#
###################

largeorsmall = c(); for ( i in 1:46){ largeorsmall[i] = dat$union.decr.2[i*47] }

#same as above, but with old independent variables

fits = list()
fits[[1]] = lm(d.state.trends~union.diff+manuf.diff)
fits[[2]] = lm(d.state.trends~union.post+manuf.post)
fits[[3]] = lm(d.state.trends~union.diff+manuf.diff+union.post+manuf.post)
fits[[4]] = lm(d.state.cycle~union.diff+manuf.diff)
fits[[5]] = lm(d.state.cycle~union.post+manuf.post)
fits[[6]] = lm(d.state.cycle~union.diff+manuf.diff+union.post+manuf.post)

c1 = summary(fits[[1]])$coef
c2 = summary(fits[[2]])$coef
c3 = summary(fits[[3]])$coef
c4 = summary(fits[[4]])$coef
c5 = summary(fits[[5]])$coef
c6 = summary(fits[[6]])$coef

t3output = matrix(rep(NA, 6*11), ncol = 6)
t3output[,1] = c(c1[2, 1], c1[2, 2], c1[3, 1], c1[3, 2],
                 NA, NA, NA, NA,
                 c1[1, 1], c1[1, 2], summary(fits[[1]])$r.squared)
t3output[,2] = c(NA, NA, NA, NA,
                 c2[2, 1], c2[2, 2], c2[3, 1], c2[3, 2],
                 c2[1, 1], c2[1, 2], summary(fits[[1]])$r.squared)
t3output[,3] = c(c3[2, 1], c3[2, 2], c3[3, 1], c3[3, 2],
                 c3[4, 1], c3[4, 2], c3[5, 1], c3[5, 2],
                 c3[1, 1], c3[1, 2], summary(fits[[1]])$r.squared)
t3output[,4] = c(c4[2, 1], c4[2, 2], c4[3, 1], c4[3, 2],
                 NA, NA, NA, NA,
                 c4[1, 1], c4[1, 2], summary(fits[[1]])$r.squared)
t3output[,5] = c(NA, NA, NA, NA,
                 c5[2, 1], c5[2, 2], c5[3, 1], c5[3, 2],
                 c5[1, 1], c5[1, 2], summary(fits[[1]])$r.squared)
t3output[,6] = c(c6[2, 1], c6[2, 2], c6[3, 1], c6[3, 2],
                 c6[4, 1], c6[4, 2], c6[5, 1], c6[5, 2],
                 c6[1, 1], c6[1, 2], summary(fits[[1]])$r.squared)
rownames(t3output) = c('d.union', 'se', 'd.manuf', 'se',
                       'union.post', 'se', 'manuf.post', 'se',
                       'constant', 'se', 'r sqrd')
colnames(t3output) = 1:6

write.csv(t3output, 't3output_5_14_v4.csv')

###################
#
# Visuals
#
###################

# plot(d.union, d.state.trends.post, xlab = 'd.union', ylab = 'd.trend', pch = 20, col = rgb(.3, .6, .9, alpha = .6), main = 'Change in Trend vs. Change in union membership %')
quick = lm(d.state.trends~d.union)
abline(a = quick$coef[1], b = quick$coef[2], col = rgb(.9, .4, .4))
text(-0.6, c(-3, -3.7), paste(names(coef(quick)), ' = ', round(as.numeric(coef(quick)), 2), sep = ''), cex = .7)

plot(1:47, dat$mem[dat$state==unique(dat$state)[20]], pch = 20, col = rgb(.3, .6, .9, alpha = .6),
     axes = F, ylab = 'Union density', xlab = 'Year', main = 'Example State d.union = -0.59')
axis(1, at = (2+5*(0:9)), labels = c('1965', '1970', '1975', '1980', '1985', '1990', '1995', '2000', '2005', '2010'))
axis(2, at = c(20, 30, 40), labels = c('20%', '30%', '40%'))

dat$d.union = dat$yr #to make my paste code show d.union
quicker = lm(mem~d.union, data = dat[dat$state==unique(dat$state)[20],])
abline(a = quicker$coef[1], b = quicker$coef[2], col = rgb(.9, .4, .4))
text(10, c(25, 20), paste(names(coef(quicker)), ' = ', round(as.numeric(coef(quicker)), 2), sep = ''), cex = .7)

plot(d.union,state.trends.post, pch = 20, col = rgb(.3, .6, .9, alpha = .6),
     main = 'Post trend estimate vs d.union')
quickest = lm(state.trends.post~d.union)
abline(a = quickest$coef[1], b = quickest$coef[2], col = rgb(.9, .4, .4))
text(-.65, c(1.5, 1.1), paste(names(coef(quickest)), ' = ', round(as.numeric(coef(quickest)), 2), sep = ''), cex = .7)
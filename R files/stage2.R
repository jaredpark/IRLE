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

d.manuf = c()
for (i in 1:length(unique(dat$state))){
  fit = lm(manuf.share ~ yr, data = dat[dat$state == unique(dat$state)[i], ] )
  d.manuf[i] = fit$coef[2]*100
}

lynn.d.manuf = dat$manuf.share[dat$yr == 47]-dat$manuf.share[dat$yr == 1]
cor(d.manuf, lynn.d.manuf) #0.99452!

#have: d.trend, d.cycle, d.union, d.manuf
#need: manuf.post, union.post
# Mean of the last half of the years for these numbers

mean.manuf.post = c()
for (i in 1:length(unique(dat$state))){
  mean.manuf.post[i] = mean(dat$manuf.share[dat$year > 1985 & dat$state == unique(dat$state)[i]])
}

mean.union.post = c()
for (i in 1:length(unique(dat$state))){
  mean.union.post[i] = mean(dat$mem[dat$year > 1985 & dat$state == unique(dat$state)[i]])/100
}

#have all my needed responses and predictors:

##################
#
#  Regressions
#
##################

s.2.fit.1 = lm(d.state.trends~d.union+d.manuf)
s.2.fit.2 = lm(d.state.trends~mean.union.post+mean.manuf.post)
s.2.fit.3 = lm(d.state.trends~d.union+d.manuf+mean.union.post+mean.manuf.post)
s.2.fit.4 = lm(d.state.cycle~d.union+d.manuf)
s.2.fit.5 = lm(d.state.cycle~mean.union.post+mean.manuf.post)
s.2.fit.6 = lm(d.state.cycle~d.union+d.manuf+mean.union.post+mean.manuf.post)

s.2.1 = c(as.numeric(summary(s.2.fit.1)$coef[, c(1:2)]), 
          summary(s.2.fit.1)$r.squared)
s.2.2 = c(as.numeric(summary(s.2.fit.2)$coef[, c(1:2)]), 
          summary(s.2.fit.2)$r.squared)
s.2.3 = c(as.numeric(summary(s.2.fit.3)$coef[, c(1:2)]), summary(s.2.fit.3)$r.squared)
s.2.4 = c(as.numeric(summary(s.2.fit.4)$coef[, c(1:2)]), summary(s.2.fit.4)$r.squared)
s.2.5 = c(as.numeric(summary(s.2.fit.5)$coef[, c(1:2)]), summary(s.2.fit.5)$r.squared)
s.2.6 = c(as.numeric(summary(s.2.fit.6)$coef[, c(1:2)]), summary(s.2.fit.6)$r.squared)

stage2.output = data.frame(c(s.2.1, rep(NA, 4)), c(s.2.2, rep(NA, 4)), s.2.3, c(s.2.4, rep(NA, 4)), c(s.2.5, rep(NA, 4)), s.2.6)
names(stage2.output) = 1:6

write.csv(stage2.output, '~/stage2_4_26_2.csv', row.names = F)

###################
#
# Table 4
#
###################

#same as above, but with trend.post and cycle.post as the response variable

s.2.fit.1 = lm(state.trends.post~d.union+d.manuf)
s.2.fit.2 = lm(state.trends.post~mean.union.post+mean.manuf.post)
s.2.fit.3 = lm(state.trends.post~d.union+d.manuf+mean.union.post+mean.manuf.post)
s.2.fit.4 = lm(state.cycle.post~d.union+d.manuf)
s.2.fit.5 = lm(state.cycle.post~mean.union.post+mean.manuf.post)
s.2.fit.6 = lm(state.cycle.post~d.union+d.manuf+mean.union.post+mean.manuf.post)

s.2.1 = c(as.numeric(summary(s.2.fit.1)$coef[, c(1:2)]), 
          summary(s.2.fit.1)$r.squared)
s.2.2 = c(as.numeric(summary(s.2.fit.2)$coef[, c(1:2)]), 
          summary(s.2.fit.2)$r.squared)
s.2.3 = c(as.numeric(summary(s.2.fit.3)$coef[, c(1:2)]), summary(s.2.fit.3)$r.squared)
s.2.4 = c(as.numeric(summary(s.2.fit.4)$coef[, c(1:2)]), summary(s.2.fit.4)$r.squared)
s.2.5 = c(as.numeric(summary(s.2.fit.5)$coef[, c(1:2)]), summary(s.2.fit.5)$r.squared)
s.2.6 = c(as.numeric(summary(s.2.fit.6)$coef[, c(1:2)]), summary(s.2.fit.6)$r.squared)

table4.output = data.frame(c(s.2.1, rep(NA, 4)), c(s.2.2, rep(NA, 4)), s.2.3, c(s.2.4, rep(NA, 4)), c(s.2.5, rep(NA, 4)), s.2.6)
names(table4.output) = 1:6

write.csv(table4.output, '~/table4_4_26_2.csv', row.names = F)


###################
#
# Visuals
#
###################

plot(d.union, d.state.trends, xlab = 'd.union', ylab = 'd.trend', pch = 20, col = rgb(.3, .6, .9, alpha = .6), main = 'Change in Trend vs. Change in union membership %')
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
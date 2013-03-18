#################
#
#    Data Set-up
#
#################

setwd('~/R Files/Okun')
load('.RData')

data_3_30 <- read.delim("~/Projects/Reich/wages vs ue/data_3_30.txt")
dat = data_3_30
dat = dat[!dat$state == "" & !dat$state == "North Dakota" & !dat$state == "Louisiana" & !dat$state == "Wyoming",]
dat = dat[, c(2, 3, 8, 10, 12, 14, 15, 24)]

d.rgdp = (c(dat$rgdp, NA) - c(NA, dat$rgdp))/c(NA, dat$rgdp) #year 2 - year 1 divided by year 1...
d.rgdp[1+47*(0:45)] = NA
d.rgdp = d.rgdp[-length(d.rgdp)]
dat$pct.d.rgdp = d.rgdp*100

d.ue = c(dat$ue, NA) - c(NA, dat$ue)
d.ue[1+47*(0:45)] = NA
d.ue = d.ue[-length(d.ue)]
dat$d.ue = d.ue

pct.d.ue = (c(dat$ue, NA) - c(NA, dat$ue))/c(NA, dat$mem)*100
pct.d.ue[1+47*(0:45)] = NA
pct.d.ue = pct.d.ue[-length(pct.d.ue)]
dat$pct.d.ue = pct.d.ue

d.mem = c(dat$mem, NA) - c(NA, dat$mem)
d.mem[1+47*(0:45)] = NA
d.mem = d.mem[-length(d.mem)]
dat$d.mem = d.mem

pct.d.mem = (c(dat$mem, NA) - c(NA, dat$mem))/c(NA, dat$mem)*100
pct.d.mem[1+47*(0:45)] = NA
pct.d.mem = pct.d.mem[-length(pct.d.mem)]
dat$pct.d.mem = pct.d.mem

dat$manuf.share = dat$manemp/dat$totemp

d.manuf.share = c(dat$manuf.share, NA) - c(NA, dat$manuf.share)
d.manuf.share[1+47*(0:45)] = NA
d.manuf.share = d.manuf.share[-length(d.manuf.share)]
dat$d.manuf.share = d.manuf.share

union.net.change = c()
for (i in 1:length(unique(dat$state))){
  fit = lm(mem ~ yr, data = dat[dat$state == unique(dat$state)[i], ] )
  union.net.change[i] = fit$coef[2]*47
}
union.rank = unique(dat$state)[order(union.net.change)]
large.union.decr = union.rank[1:23]
small.union.decr = union.rank[24:26]
dat$union.decr = ifelse(is.element(dat$state, large.union.decr), 'large', 'small')
dat$union.decr.slope[47*(1:46)] = union.net.change

manufshare.net.change = c()
for (i in 1:length(unique(dat$state))){
  fit = lm(manuf.share ~ yr, data = dat[dat$state == unique(dat$state)[i], ] )
  manufshare.net.change[i] = fit$coef[2]*47
}
manufshare.rank = unique(dat$state)[order(manufshare.net.change)]
large.manufshare.decr = manufshare.rank[1:23]
small.manufshare.decr = manufshare.rank[24:26]
dat$manufshare.decr = ifelse(is.element(dat$state, large.manufshare.decr), 'large', 'small')
dat$manufshare.slope[47*(1:46)] = manufshare.net.change

dat$yr = rep(1:47, 46)



#################
#
#    Functions
#
#################


boot.trend = function(data){
  index = sample(1:nrow(data), size = nrow(data), replace = T)
  fit = lm(d.ue~pct.d.rgdp + state, data = data[index, ])
  int = coef(fit)[1]
  slope = coef(fit)[2]
  offsets = coef(fit)[3:length(coef(fit))]
  offsets = c(0, offsets)
  S = length(unique(data$state[index]))
  boot.trend.est = -(sum(offsets+int))/(S*slope)
#   boot.trend.est = 1
  return(boot.trend.est)
#   return(length(unique(dat$state[index]))==46)
}

stage1 = function(data, B = 50){

  lm1 = lm(d.ue~pct.d.rgdp + state, data = data)
  int = coef(lm1)[1]
  slope = coef(lm1)[2]
  offsets = coef(lm1)[3:length(coef(lm1))]
  offsets = c(0, offsets)

  S = length(unique(data$state))
  trend.est = -(sum(offsets+int))/(S*slope)

  trend.samples = replicate(B, boot.trend(data = data))

  values = list('fit' = lm1, 'trend1' = trend.est, 'trend.var' = var(trend.samples))
  
  return(values)
}

#################
#
#  Setting up data indexes
#
#################

i1 = 1:nrow(dat)
i2 = which(dat$year < 1986)
i3 = which(dat$year >= 1986)
i4 = which(dat$union.decr == 'large')
i5 = which(dat$union.decr == 'small')
i6 = which(dat$union.decr == 'large' & dat$year < 1986)
i7 = which(dat$union.decr == 'small' & dat$year < 1986)
i8 = which(dat$union.decr == 'large' & dat$year >= 1986)
i9 = which(dat$union.decr == 'small' & dat$year >= 1986)

indexes = list(i1, i2, i3, i4, i5, i6, i7, i8, i9)

##################
#
#  Now regressions
#
##################

results = list()
for (i in 1:length(indexes)){
  results[[i]] = stage1(data = dat[indexes[[i]], ])
}

# Now editing code for no state fixed effects:

boot.trend = function(data){
  index = sample(1:nrow(data), size = nrow(data), replace = T)
  fit = lm(d.ue~pct.d.rgdp, data = data[index, ])
  int = coef(fit)[1]
  slope = coef(fit)[2]
#  offsets = coef(fit)[3:length(coef(fit))]
#  offsets = c('stateAlabama' = 0, offsets)
#  S = length(unique(dat$state[index]))
#  boot.trend.est = -(sum(offsets+int))/(S*slope)
   boot.trend.est = -int/slope
   return(boot.trend.est)
#   return(length(unique(dat$state[index]))==46)
}

stage1 = function(data, B = 50){

  lm1 = lm(d.ue~pct.d.rgdp, data = data)
  int = coef(lm1)[1]
  slope = coef(lm1)[2]
#  offsets = coef(lm1)[3:length(coef(lm1))]
#  offsets = c(0, offsets)

#  S = length(unique(dat$state))
#  trend.est = -(sum(offsets+int))/(S*slope)
  trend.est = -int/slope
  trend.samples = replicate(B, boot.trend(data = data))

  values = list('fit' = lm1, 'trend1' = trend.est, 'trend.var' = var(trend.samples))
  
  return(values)
}

#and stage1 should be fine:

i1.n = 1:nrow(dat)
i2.n = which(dat$year < 1986)
i3.n = which(dat$year >= 1986)

indexes = list(i1.n, i2.n, i3.n)

for (i in 1:length(indexes)){
  results[[9+i]] = stage1(data = dat[indexes[[i]], ])
}

stage1regressions = list(results[[10]], results[[1]], results[[11]], results[[2]], 
                         results[[12]], results[[3]], results[[4]], results[[5]],
                         results[[6]], results[[7]], results[[8]],results[[9]])

c(length(i1.n), length(i1), length(i2.n), length(i2), length(i3.n), length(i3),
  length(i4), length(i5), length(i6), length(i7), length(i8), length(i9))

GDP.coefs = list()
for (i in 1:12) GDP.coefs[[i]] = summary(stage1regressions[[i]]$fit)$coefficients[2,]
int.coefs = list()
for (i in 1:12) int.coefs[[i]] = summary(stage1regressions[[i]]$fit)$coefficients[1,]
trend.ests = c()
for (i in 1:12) trend.ests[[i]] = as.numeric(stage1regressions[[i]][2])
trend.ses = c()
for (i in 1:12) trend.ses[[i]] = sqrt(as.numeric(stage1regressions[[i]][3]))
fit.r2 = c()
for (i in 1:12) fit.r2[i] = summary(stage1regressions[[i]]$fit)$r.squared

m = matrix(rep(NA, 11*12), nrow = 12)
for (i in 1:12){
  m[i, ] = c(as.numeric(GDP.coefs[[i]]), as.numeric(int.coefs[[i]]), trend.ests[i], trend.ses[i], fit.r2[i])
}

okunstage1 = as.data.frame(m)
names(okunstage1) = c('GDP', 'se', 't', 'p', 'Int', 'se', 't', 'p', 'trend', 'se', 'r^2')
# View(okunstage10)
# write.csv(okunstage1, '~/okunstage1_4_25_2.csv', row.names = F)

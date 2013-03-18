#################
#
#    Data Set-up
#
#################

setwd('~/R Files/Okun')
load('stage1.RData')

data_3_30 <- read.delim("~/Projects/Reich/wages vs ue/data_3_30.txt")
dat = data_3_30
dat = dat[!dat$state == "" & !dat$state == "North Dakota" & !dat$state == "Louisiana" & !dat$state == "Wyoming",]
dat = dat[, c(2, 3, 8, 10, 12, 14, 15, 24)]
# View(dat)

# library('lattice')
# print(xyplot(y ~ year | state, 
#              data=Fatality, 
#              panel=panel.lines,
#              ylab="Vehicle Fatality Rate (annual, per 10,000 people)",
#              xlab="Year"))
# ?xyplot
# xyplot(ue ~ year | state, data = dat, panel = panel.lines, par = .7)
# xyplot(ue ~ rgdp | state, data = dat, 
#         panel=function(x,y,...){
#           panel.xyplot(x,y,...)
#           panel.lmline(x,y,...)
#           })
# summary(dat$rgdp)
# # par(mfrow = c(5,5), mar = c(.5,.5,.5,5))
# 
# M = matrix(rep(NA, 3*49), ncol = 3)
# for (i in 1:length(unique(dat$state))){
#   M[i, 1] = as.character(unique(dat$state)[i])
#   M[i, 2:3] = range(dat$rgdp[dat$state == unique(dat$state)[i]])
# }
# View(M)
# par(mfrow = c(1,1))

d.rgdp = (c(dat$rgdp, NA) - c(NA, dat$rgdp))/c(NA, dat$rgdp) #year 2 - year 1 divided by year 1...
# head(d.rgdp); d.rgdp[47:49]
d.rgdp[1+47*(0:45)] = NA
d.rgdp = d.rgdp[-length(d.rgdp)]
# View(d.rgdp)
dat$pct.d.rgdp = d.rgdp*100

d.ue = c(dat$ue, NA) - c(NA, dat$ue)
# head(d.ue)
d.ue[1+47*(0:45)] = NA
d.ue = d.ue[-length(d.ue)]
# View(d.ue)
dat$d.ue = d.ue

# pct.d.rgdp.mean = ave(dat[!is.na(dat[,10]), 10], dat$state[!is.na(dat[,10])], FUN = mean)
# ctr.pct.d.rgdp = dat$pct.d.rgdp[!is.na(dat$pct.d.rgdp)] - pct.d.rgdp.mean
# ctr.pct.d.rgdp.mat = matrix(ctr.pct.d.rgdp, nrow = 49, byrow = T)
# ctr.pct.d.rgdp.2 = c()
# for (i in 1:49){
#   ctr.pct.d.rgdp.2 = c(ctr.pct.d.rgdp.2, NA, ctr.pct.d.rgdp.mat[i,])
# }
# # View(ctr.pct.d.rgdp.2)
# dat$ctr.pct.d.rgdp = ctr.pct.d.rgdp.2
# 
# d.ue.mean = ave(dat[!is.na(dat[,9]), 9], dat$state[!is.na(dat[,9])], FUN = mean)
# ctr.d.ue = dat$d.ue[!is.na(dat$d.ue)] - d.ue.mean
# ctr.d.ue.mat = matrix(ctr.d.ue, nrow = 49, byrow = T)
# ctr.d.ue.2 = c()
# for (i in 1:49){
#   ctr.d.ue.2 = c(ctr.d.ue.2, NA, ctr.d.ue.mat[i,])
# }
# # View(ctr.d.ue.2)
# dat$ctr.d.ue = ctr.d.ue.2

# View(dat)

# fm = lm(ctr.d.ue ~ ctr.pct.d.rgdp)
# summary(fm)


#################
#
#    Regressions on entire data set
#
#################


lm1 = lm(d.ue~pct.d.rgdp + state, data = dat)
# summary(lm1)
int = coef(lm1)[1]
slope = coef(lm1)[2]
offsets = coef(lm1)[3:length(coef(lm1))]
offsets = c('stateAlabama' = 0, offsets)
hist(int+offsets); hist(dat$pct.d.rgdp); hist(dat$d.ue)

S = length(unique(dat$state))
trend.est = -(sum(offsets+int))/(S*slope)

vcov1 = vcov(lm1)

#################
#
#    Bootstrapping for trend.est CI
#
#################

boot.trend = function(){
  index = sample(1:nrow(dat), size = nrow(dat), replace = T)
  fit = lm(d.ue~pct.d.rgdp + state, data = dat[index, ])
  int = coef(fit)[1]
  slope = coef(fit)[2]
  offsets = coef(fit)[3:length(coef(fit))]
  offsets = c('stateAlabama' = 0, offsets)
  S = length(unique(dat$state[index]))
  boot.trend.est = -(sum(offsets+int))/(S*slope)
#   boot.trend.est = 1
  return(boot.trend.est)
#   return(length(unique(dat$state[index]))==46)
}

B = 2500
trend.samples = replicate(B, boot.trend())
hist(trend.samples, breaks = 25)
lines(seq(min(trend.samples), max(trend.samples), length.out = 100))
var(trend.samples); mean(trend.samples)
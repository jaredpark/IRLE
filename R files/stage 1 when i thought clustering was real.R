# run a function here called cl() written by Mahmood Ara in Stockholm University - the backup can be found here and here.

setwd('~/R Files/Okun')



cl <- function(dat,fm, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL)
}
cl(dat, lm2, dat$state)

library('Ecdat')
data(Fatality)
LSDV <- lm(mrall ~ beertax + factor(year) + factor(state), data=Fatality)
LSDV
nrow(Fatality)

gcenter <- function(df1,group) {
  variables <- paste(rep("C", ncol(df1)), colnames(df1), sep=".")
  copydf <- df1
  for (i in 1:ncol(df1)) {
    copydf[,i] <- df1[,i] - ave(df1[,i], group,FUN=mean)
  }
  colnames(copydf) <- variables
  return(cbind(df1,copydf))
 }



centerFatality = gcenter(Fatality[, 3:4], Fatality$state)
unique(ave(Fatality[,3], Fatality$state,FUN=mean))
unique(Fatality$state)

ctrFatality = gcenter(Fatality[1:4], Fatality$state)


data_3_30 <- read.delim("~/Projects/Reich/wages vs ue/data_3_30.txt")
dat = data_3_30
dat = dat[!dat$state == "",]
dat = dat[, c(2, 3, 8, 10, 12, 14, 15, 24)]
View(dat)
ctr.dat = gcenter(dat[, 3:5], dat$state)
View(ctr.dat)
ctr.dat = data.frame('year' = dat[,1], 'state' = dat[,2], ctr.dat, row.names = NULL)

Fatality$y <- Fatality$mrall*10000
library('lattice')
print(xyplot(y ~ year | state, 
             data=Fatality, 
             panel=panel.lines,
             ylab="Vehicle Fatality Rate (annual, per 10,000 people)",
             xlab="Year"))
?xyplot
xyplot(ue ~ year | state, data = dat, panel = panel.lines, par = .7)
xyplot(ue ~ rgdp | state, data = dat, 
        panel=function(x,y,...){
          panel.xyplot(x,y,...)
          panel.lmline(x,y,...)
          })
summary(dat$rgdp)
# par(mfrow = c(5,5), mar = c(.5,.5,.5,5))

M = matrix(rep(NA, 3*49), ncol = 3)
for (i in 1:length(unique(dat$state))){
  M[i, 1] = as.character(unique(dat$state)[i])
  M[i, 2:3] = range(dat$rgdp[dat$state == unique(dat$state)[i]])
}
View(M)
# par(mfrow = c(1,1))

d.rgdp = (c(dat$rgdp, NA) - c(NA, dat$rgdp))/c(NA, dat$rgdp)
head(d.rgdp)
d.rgdp[1+47*(0:48)] = NA
d.rgdp = d.rgdp[-length(d.rgdp)]
# View(d.rgdp)
dat$pct.d.rgdp = d.rgdp

d.ue = c(dat$ue, NA) - c(NA, dat$ue)
head(d.ue)
d.ue[1+47*(0:48)] = NA
d.ue = d.ue[-length(d.ue)]
# View(d.ue)
dat$d.ue = d.ue/100

pct.d.rgdp.mean = ave(dat[!is.na(dat[,10]), 10], dat$state[!is.na(dat[,10])], FUN = mean)
ctr.pct.d.rgdp = dat$pct.d.rgdp[!is.na(dat$pct.d.rgdp)] - pct.d.rgdp.mean
ctr.pct.d.rgdp.mat = matrix(ctr.pct.d.rgdp, nrow = 49, byrow = T)
ctr.pct.d.rgdp.2 = c()
for (i in 1:49){
  ctr.pct.d.rgdp.2 = c(ctr.pct.d.rgdp.2, NA, ctr.pct.d.rgdp.mat[i,])
}
# View(ctr.pct.d.rgdp.2)
dat$ctr.pct.d.rgdp = ctr.pct.d.rgdp.2

d.ue.mean = ave(dat[!is.na(dat[,9]), 9], dat$state[!is.na(dat[,9])], FUN = mean)
ctr.d.ue = dat$d.ue[!is.na(dat$d.ue)] - d.ue.mean
ctr.d.ue.mat = matrix(ctr.d.ue, nrow = 49, byrow = T)
ctr.d.ue.2 = c()
for (i in 1:49){
  ctr.d.ue.2 = c(ctr.d.ue.2, NA, ctr.d.ue.mat[i,])
}
# View(ctr.d.ue.2)
dat$ctr.d.ue = ctr.d.ue.2

# View(dat)

fm = lm(ctr.d.ue ~ ctr.pct.d.rgdp)
summary(fm)

lm1 = lm(pct.d.rgdp~d.ue + state, data = dat)
summary(lm1)
int = coef(lm1)[1]
slope = coef(lm1)[2]
offsets = coef(lm1)[3:length(coef(lm1))]
offsets = c(0, offsets)
trend.est = -slope*mean(1/(int+offsets))
mean(int+offsets)

lm2 = lm(d.ue~pct.d.rgdp + state, data = dat)
int = coef(lm2)[1]
slope = coef(lm2)[2]
offsets = coef(lm2)[3:length(coef(lm2))]
offsets = c(0, offsets)
trend.est = -slope*mean(1/(int+offsets))
-slope/(mean(offsets)+int)

library('plm')

plm.dat = plm.data(dat, indexes = c('state', 'year'))
View(plm.dat)
plmfm = plm(d.ue ~ pct.d.rgdp, data = plm.dat, method = 'within')
summary(plmfm)
setwd('~/R files/Okun')
load('.Rdata')
library('mFilter')

# example hp filter:

ex.actual = c(seq(0,5, by = .4), seq(5.2,7.6, by = .10))
ex.series = ex.actual + rnorm(length(ex.actual), 0, 0.4)
ex.decomp = hpfilter(ex.series, type = 'lambda', freq = 6.25)
l = length(ex.series)
plot(1:l, ex.series, type = 'l')
lines(1:l, ex.actual, type = 'l', col = 'red')
lines(1:l, ex.decomp$trend, type = 'l', col = 'blue')
actual.cycle = ex.series - ex.actual
est.cycle = ex.decomp$cycle
over.est = est.cycle - actual.cycle 
hist(over.est)
sum(over.est) # a biased estimator
sd(over.est) # captures actual se w/in .0138
plot(1:length(over.est), over.est); abline(h = 0, lty = 2)

#finding distribution parameters of rgdp
hist(dat$ln.rgdp[dat$state == 'Alabama'])

hp.list = list()
for (i in 1:length(unique(dat$state))){
  hp.list[[paste(unique(dat$state)[i])]] = hpfilter(dat$ln.rgdp[dat$state == unique(dat$state)[i]],
                                                    freq = 6.25,
                                                    type = 'lambda')
}

hp.list[[1]]
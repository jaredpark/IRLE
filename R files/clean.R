#################
#
#    Data Set-up
#
#################

setwd('~/Projects/Reich/R Files')

data_3_30 <- read.delim("~/Projects/Reich/wages vs ue/data_3_30.txt")
new = data_3_30[!data_3_30$state == "" & !data_3_30$state == "North Dakota" & !data_3_30$state == "Louisiana" & !data_3_30$state == "Wyoming",]
dat = data_3_30
dat = dat[!dat$state == "" & !dat$state == "North Dakota" & !dat$state == "Louisiana" & !dat$state == "Wyoming",]
dat = dat[, c(2, 3, 8, 10, 12, 14, 15, 24)]

d.rgdp = (c(dat$rgdp, NA) - c(NA, dat$rgdp))/c(NA, dat$rgdp) #year 2 - year 1 divided by year 1...
d.rgdp[1+47*(0:45)] = NA
d.rgdp = d.rgdp[-length(d.rgdp)]
dat$pct.d.rgdp = d.rgdp*100

SW.d.rgdp = 100*log(c(dat$rgdp, NA)/c(NA, dat$rgdp))
SW.d.rgdp[1+47*(0:45)] = NA
SW.d.rgdp = SW.d.rgdp[-length(SW.d.rgdp)]

ln.rgdp = log(dat$rgdp*1000000)
dat$ln.rgdp = ln.rgdp

d.ln.rgdp = 

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

union.diff = dat$mem[dat$yr == 47] - dat$mem[dat$yr == 1]
pct.union.diff = union.diff/dat$mem[dat$yr == 1]*100

union.rank = unique(dat$state)[order(pct.union.diff)]
abbreviated = state.abb[match(unique(dat$state), state.name)]

large.union.decr = union.rank[1:15]
med.union.decr = union.rank[16:31]
small.union.decr = union.rank[32:46]

dat$union.decr = ifelse(is.element(dat$state, large.union.decr),
                        yes = 'large',
                        no = ifelse(is.element(dat$state, med.union.decr),
                                    yes = 'med',
                                    no = 'small'))
large.union.decr.2 = union.rank[1:23]
small.union.decr.2 = union.rank[24:46]

dat$union.decr.2 = ifelse(is.element(unique(dat$state), large.union.decr.2),
                          yes = 'large', no = 'small')

dat$manuf.share = dat$manemp/dat$totemp
d.manuf.share = c(dat$manuf.share, NA) - c(NA, dat$manuf.share)
d.manuf.share[1+47*(0:45)] = NA
d.manuf.share = d.manuf.share[-length(d.manuf.share)]
dat$d.manuf.share = d.manuf.share

manuf.diff = dat$manuf.share[dat$yr == 47] - dat$manuf.share[dat$yr == 1]
pct.manuf.diff = manuf.diff/dat$manuf.share[dat$yr == 1]*100

manuf.rank = unique(dat$state)[order(pct.manuf.diff)]
large.manuf.decr = manuf.rank[1:17]
med.manuf.decr = manuf.rank[18:29]
small.manuf.decr = manuf.rank[30:46]

dat$yr = rep(1:47, 46)

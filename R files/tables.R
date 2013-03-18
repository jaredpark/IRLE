setwd('~/R Files/Okun')
load('.RData')

############
#
# Table A1
#
############

rgdp.summary = c(as.numeric(summary(.001*dat$rgdp)[4]), sd(.001*dat$rgdp), as.numeric(summary(.001*dat$rgdp)[c(1,6)]))

pct.d.rgdp.summary = c(as.numeric(summary(dat$pct.d.rgdp[!is.na(dat$pct.d.rgdp)])[4]), 
                       sd(dat$pct.d.rgdp[!is.na(dat$pct.d.rgdp)]), 
                       as.numeric(summary(dat$pct.d.rgdp[!is.na(dat$pct.d.rgdp)])[c(1,6)]))

union.rate.summary = c(as.numeric(summary(dat$mem[!is.na(dat$mem)])[4]), 
                       sd(dat$mem[!is.na(dat$mem)]), 
                       as.numeric(summary(dat$mem[!is.na(dat$mem)])[c(1,6)]))

pct.d.union.rate.summary = c(as.numeric(summary(dat$pct.d.mem[!is.na(dat$pct.d.mem)])[4]), 
                             sd(dat$pct.d.mem[!is.na(dat$pct.d.mem)]), 
                             as.numeric(summary(dat$pct.d.mem[!is.na(dat$pct.d.mem)])[c(1,6)]))

ue.rate.summary = c(as.numeric(summary(dat$ue[!is.na(dat$ue)])[4]), 
                    sd(dat$ue[!is.na(dat$ue)]), 
                    as.numeric(summary(dat$ue[!is.na(dat$ue)])[c(1,6)]))

pct.d.ue.rate.summary = c(as.numeric(summary(dat$pct.d.ue[!is.na(dat$pct.d.ue)])[4]), 
                          sd(dat$pct.d.ue[!is.na(dat$pct.d.ue)]), 
                          as.numeric(summary(dat$pct.d.ue[!is.na(dat$pct.d.ue)])[c(1,6)]))

d.union.rate.summary = c(as.numeric(summary(dat$d.mem[!is.na(dat$d.mem)])[4]), 
                         sd(dat$d.mem[!is.na(dat$d.mem)]), 
                         as.numeric(summary(dat$d.mem[!is.na(dat$d.mem)])[c(1,6)]))

d.ue.rate.summary = c(as.numeric(summary(dat$d.ue[!is.na(dat$d.ue)])[4]), 
                      sd(dat$d.ue[!is.na(dat$d.ue)]), 
                      as.numeric(summary(dat$d.ue[!is.na(dat$d.ue)])[c(1,6)]))

okun.tableA1 = matrix(c(rgdp.summary, pct.d.rgdp.summary, union.rate.summary, pct.d.union.rate.summary,
                      ue.rate.summary, pct.d.ue.rate.summary, d.union.rate.summary, d.ue.rate.summary),
                      ncol = 4, byrow = T)

row.names(okun.tableA1) = c('rgdp.summary', 'pct.d.rgdp.summary', 'union.rate.summary', 'pct.d.union.rate.summary',
                      'ue.rate.summary', 'pct.d.ue.rate.summary', 'd.union.rate.summary', 'd.ue.rate.summary')

write.csv(okun.tableA1, "okuntableA1_4_26.csv")

############
#
# Table A2
#
############

# first run stage2 code to get variables, or just load .Rdata
# cycle_pre, cycle_post, d.cycle, trend_pre, trend_post, d.trend,
# d.union.rate (my variable called slopes), d.manuf.share,
# mean(unionization rate post), mean(manuf share post) **provide both 23 and 10 year avgs for 'post'

# cycle.pre.summary = c(as.numeric(summary(dat$state.cycle.pre[!is.na(dat$state.cycle.pre)])[4]), 
#                       sd(dat$state.cycle.pre[!is.na(dat$d.ue)]), 
#                       as.numeric(summary(dat$d.ue[!is.na(dat$d.ue)])[c(1,6)]))
#enough is enough, making a function:

okun.summary = function(x){
  if (!mode(x)== 'numeric' | !is.vector(x)) warning('x must be a numeric vector')
  foo = c(as.numeric(summary(x[!is.na(x)])[4]),
          sd(x[!is.na(x)]), 
          as.numeric(summary(x[!is.na(x)])[c(1,6)]),
          length(x))
  return(foo)
}

needed = list(state.constant.pre, state.constant.post,
              state.cycle.pre, state.cycle.post, d.state.cycle,
              state.trends.pre, state.trends.post, d.state.trends,
              union.net.change, manufshare.net.change,
              mean.union.post, mean.manuf.post)

okuntableA2 = matrix(rep(NA, 5*length(needed)), ncol = 5)

for(i in 1:length(needed)){
  okuntableA2[i, ] = okun.summary(needed[[i]])
}

rownames(okuntableA2) = c('constant pre', 'constant post', 
                          'cycle pre', ' cycle post', 'd.cycle',
                          'trend pre', ' trend post', 'd.trend',
                          'net union change', 'net manuf change',
                          'mean union post', 'mean manuf post')

write.csv(okuntableA2, 'okuntableA2_4_26.csv')

############
#
# Table A3
#
############

okuntableA3 = matrix(c(as.character(unique(dat$state)), union.net.change), ncol = 2)
write.csv(okuntableA3, 'okuntableA3_4_26.csv')

union.diff = dat$mem[dat$yr == 47] - dat$mem[dat$yr == 1]
union.net = round(as.numeric(okuntableA3[,2]), 1)
plot(1:46, union.diff, pch = 20)
lines(1:46, union.net, type = 'p', pch = 20, col = 'blue')

diff.rank = unique(dat$state)[order(union.diff)]
net.rank = unique(dat$state)[order(union.net)]

#maine, vermont, south dakota, alabama*, new hampshire*, kansas*, colorado*, maryland*, nebraska* 

high.diff.rank = diff.rank[1:23]
low.diff.rank = diff.rank[24:46]
high.net.rank = net.rank[1:23]
low.net.rank = net.rank[24:46]

high.net.rank[!is.element(high.net.rank, high.diff.rank)] #show me the high.net.rank that are not is high.diff.rank
 # these states are 'high union decline' in my metric that are low decline with diff method
low.net.rank[!is.element(low.net.rank, low.diff.rank)]
 # these states are 'low decline' in mine, high in diff method
okuntableA3 = matrix(c(as.character(unique(dat$state)), union.net.change, dat$mem[dat$yr == 47] - dat$mem[dat$yr == 1]), ncol = 3)
write.csv(okuntableA3, 'okuntableA3_4_26.csv')
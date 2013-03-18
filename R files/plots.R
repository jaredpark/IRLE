# d.ue vs pct.d.productivity; the pct point change in ue vs the % change in productivity



states = unique(dat$state)
par(mfrow=c(3,1))

# 6/13 plots
peak.to.peak.pct.d.prod = 100*(dat$productivity[which(dat$year == 2007)] - dat$productivity[which(dat$year == 1979)])/dat$productivity[which(dat$year == 1979)]
peak.to.peak.pct.d.mem = 100*-(dat$mem[which(dat$year == 2007)] - dat$mem[which(dat$year == 1979)])/dat$mem[which(dat$year == 1979)]

hist(peak.to.peak.pct.d.mem, main = 'Percent decrease in union share; -(final - initial)/initial', breaks = 15, xlab = '')
hist(peak.to.peak.pct.d.prod, main = 'Percent increase in productivity; (final - initial)/initial', breaks = 15, xlab = '')
xy.fit.line(peak.to.peak.pct.d.mem, peak.to.peak.pct.d.prod, text = T,
            xtext = 'Percent decrease in union share, 1979 - 2007',
            ytext = 'Percent increase in productivity, 1979 - 2007', 
            maintext = 'Productivity measured as RGDP/#employed')

par(mfrow = c(1,1))
boxplot(peak.to.peak.pct.d.prod~dat$rtw[44+47*(0:45)], 
        names = c(paste('Not RTW; n = ', sum(dat$rtw[44+47*(0:45)]==0), sep = '')
                  , paste('RTW; n =', sum(dat$rtw[44+47*(0:45)]==1), sep = '')), 
        ylab = 'Percent increase in productivity', 
        main = 'Productivity; 1979 - 2007; (final - initial)/initial')
t.test(peak.to.peak.pct.d.prod~dat$rtw[44+47*(0:45)])

# 6/14 plots

prod.3.yr.avgs = k.wk.beg.and.end.avgs(dat = dat, feature.name = 'productivity', k = 3, begin = 1976, end = 2010)
mem.3.yr.avgs = k.wk.beg.and.end.avgs(dat = dat, feature.name = 'mem', k = 3, begin = 1976, end = 2010)
prod.mem.3.yr.avg.pct.change = 100*cbind((prod.3.yr.avgs[,2]-prod.3.yr.avgs[,1])/prod.3.yr.avgs[,1], -(mem.3.yr.avgs[,2]-mem.3.yr.avgs[,1])/mem.3.yr.avgs[,1])

par(mfrow = c(3,1))
hist(prod.mem.3.yr.avg.pct.change[,2], main = 'Percent decrease in union share; 3 year avg method', breaks = 15, xlab = '')
hist(prod.mem.3.yr.avg.pct.change[,1], main = 'Percent increase in productivity; 3 year avg method', breaks = 10, xlab = '')
xy.fit.line(prod.mem.3.yr.avg.pct.change[,2], prod.mem.3.yr.avg.pct.change[,1], text = T,
            xtext = 'Percent decrease in union share',
            ytext = 'Percent increase in productivity (RGDP/#employed)', 
            maintext = '1976 - 2010; 3 year avg of intitial and final')

prod.5.yr.avgs = k.wk.beg.and.end.avgs(dat = dat, feature.name = 'productivity', k = 5, begin = 1976, end = 2010)
mem.5.yr.avgs = k.wk.beg.and.end.avgs(dat = dat, feature.name = 'mem', k = 5, begin = 1976, end = 2010)
prod.mem.5.yr.avg.pct.change = 100*cbind((prod.5.yr.avgs[,2]-prod.5.yr.avgs[,1])/prod.5.yr.avgs[,1], -(mem.5.yr.avgs[,2]-mem.5.yr.avgs[,1])/mem.5.yr.avgs[,1])

hist(prod.mem.5.yr.avg.pct.change[,2], main = 'Percent decrease in union share; 5 year avg method', breaks = 15, xlab = '')
hist(prod.mem.5.yr.avg.pct.change[,1], main = 'Percent increase in productivity; 5 year avg method', breaks = 10, xlab = '')
xy.fit.line(prod.mem.5.yr.avg.pct.change[,2], prod.mem.5.yr.avg.pct.change[,1], text = T,
            xtext = 'Percent decrease in union share',
            ytext = 'Percent increase in productivity (RGDP/#employed)', 
            maintext = '1976 - 2010; 5 year avg of intitial and final')

prod.3.yr.avgs = k.wk.beg.and.end.avgs(dat = dat, feature.name = 'productivity', k = 3, begin = 1976, end = 2010)
mem.3.yr.avgs = k.wk.beg.and.end.avgs(dat = dat, feature.name = 'mem', k = 3, begin = 1976, end = 2010)
# changed to have union decrease, not pct decrease
prod.mem.3.yr.avg.pct.change = cbind(100*(prod.3.yr.avgs[,2]-prod.3.yr.avgs[,1])/prod.3.yr.avgs[,1], -(mem.3.yr.avgs[,2]-mem.3.yr.avgs[,1]))

par(mfrow = c(3,1))
hist(prod.mem.3.yr.avg.pct.change[,2], main = 'Union share decrease; 3 year avg method', breaks = 15, xlab = '')
hist(prod.mem.3.yr.avg.pct.change[,1], main = 'Percent increase in productivity; 3 year avg method', breaks = 10, xlab = '')
xy.fit.line(prod.mem.3.yr.avg.pct.change[,2], prod.mem.3.yr.avg.pct.change[,1], text = T,
            xtext = 'Decrease in union share %',
            ytext = 'Percent increase in productivity (RGDP/#employed)', 
            maintext = '1976 - 2010; 3 year avg of intitial and final')

# 6/15 plots; redoing the above with publication ready appeal...?

xy.fit.line = function(x, y, labs = c(), xtext = '', ytext = '', maintext = '', cex = .8, text = F,
                       pch = 20, col = rgb(.2, .3, .9, alpha = .5), plot.type = 'p')


x = prod.mem.3.yr.avg.pct.change[,2]
y = prod.mem.3.yr.avg.pct.change[,1]
index = !is.na(x) & !is.na(y)
newx = x[index]
newy = y[index]
lf = lm(newy~newx)
labs = state.abb[is.element(state.name, states)]
rtw.states = unique(dat$state)[which(rtw.column[dat$year == 2010] == 1)]
rtw.index = is.element(unique(dat$state), rtw.states)
par(mfrow = c(1,1), mar = c(5,2.75,2.25,1), lheight = .5)

plot(newx, newy, 
     ylab = '', 
     xlab = '
Notes: Horizontal axis measures decline in percentage of union workforce that is unionized.
        Vertical axis measures productivity growth, defined as percent change in GDP per 
              employed worker. GDP per worker is averaged over three years: 1976-78 and 2008-10.
      Sources: Unionstats.com and U.S. Bureau of Labor Statistics. See text for details.',
     main = 'Productivity growth versus decrease in union share, 1976-2010.', 
     type = 'n',
     axes = F,
     cex.lab = .65,
     cex.main = .75)
xticks = c(0,5,10,15,20)
axis(1, at = xticks, labels = paste(xticks, '%', sep = ''), lwd = 2,
     lwd.ticks = 0, cex.axis = .7, padj = -2, pos = 17.5)
yticks = c(17.5,20,40,60,80,100, 110)
axis(2, at = yticks, labels = c('', paste(yticks[-c(1,length(yticks))], '%', sep = ''), ''), lwd = 2,
     lwd.ticks = 0, las = 1, hadj = .5, cex.axis = .7)
rtw.col = rgb(.7, .1, .1)
n.rtw.col = rgb(.1, .1, .7)
text(newx, newy, labs, cex = .5, col = ifelse(rtw.index, rtw.col, n.rtw.col))#col = rgb(0, .2, .7, alpha = .9))
abline(lf[1], lf[2], col = rgb(.1, .1, .1))#col = rgb(.7, .2, .2, alpha = .7))

text(2.8, 35, '    slope = 
\n       SE = 
\n p-value = ', cex = .55)
text(4.1, 35, '  -2.019

   0.740

   0.009', cex = .55)

legend(17, 106, cex = .55, bty = 'o', box.lty = 0, bg = rgb(.9, .88, .9),
       c('   RTW State ', 'Not RTW state'), text.col = c(rtw.col, n.rtw.col),
       title = ' right-to-work status in 2010 ', title.col = rgb(.1, .1, .1),
       xjust = .5)

# with 64-75 data range:
s.prod.3.yr.avgs = k.wk.beg.and.end.avgs(dat = dat, feature.name = 's.prod', k = 3, begin = 1964, end = 1975)
mem.3.yr.avgs = k.wk.beg.and.end.avgs(dat = dat, feature.name = 'mem', k =3, begin = 1964, end = 1975)
s.prod.mem.3.yr.avg.pct.change = cbind(100*(s.prod.3.yr.avgs[,2]-s.prod.3.yr.avgs[,1])/s.prod.3.yr.avgs[,1], -(mem.3.yr.avgs[,2]-mem.3.yr.avgs[,1]))
xy.fit.line(s.prod.mem.3.yr.avg.pct.change[,2], s.prod.mem.3.yr.avg.pct.change[,1], text = T,
            xtext = '% decrease in union share',
            ytext = '% change in productivity', 
            maintext = '1964 to 1975; three year average method')
#6/19 plots
# 85 to 10
prod.85to10 = k.wk.beg.and.end.avgs(dat = dat, feature.name = 'productivity', k = 3, begin = 1985, end = 2010)
mem.85to10 = k.wk.beg.and.end.avgs(dat = dat, feature.name = 'mem', k = 3, begin = 1985, end = 2010)
# changed to have union decrease, not pct decrease

newx = -(mem.85to10[,2] - mem.85to10[,1])
newy = 100*(prod.85to10[,2] - prod.85to10[,1])/prod.85to10[,1]

par(mfrow = c(1,1), mar = c(7,2.75,3.75,1))
plot(newx, newy,
     type = 'n',
     ylab = '',
     xlab = '',
     axes = F,
     cex.lab = .65,
     cex.main = .75, xlim = c(-.5,10), ylim = c(21.66, 120))
title(sub = '\n \n
Notes: Horizontal axis measures decline in percentage of union workforce that is unionized.
         Vertical axis measures productivity growth, defined as percent change in GDP per 
                  employed worker. GDP per worker is averaged over three years: 1985-87 and 2008-10.
       Sources: Unionstats.com and U.S. Bureau of Labor Statistics. See text for details.',
      line = 5.5, cex.sub = .8)
title(main = 'Productivity growth versus decrease in union share, 1985-2010', cex.main = 1)
xticks = c(-1.5, seq(0,10,by=2))
axis(1, at = xticks, labels = c(paste(xticks[], sep = '')), lwd = 2,
     lwd.ticks = 1, cex.axis = .7, padj = -.5, pos = 18)
yticks = c(18,40,60,80,100,120)
axis(2, at = yticks, labels = c('',paste(yticks[-1], sep = '')), 
     lwd = 2, lwd.ticks = 1, las = 1, hadj = .8, cex.axis = .7, pos = -.9)
rtw.col = rgb(.7, .1, .1)
n.rtw.col = rgb(.1, .1, .7)
text(newx, newy, labs, cex = .5, col = ifelse(rtw.index, rtw.col, n.rtw.col))#col = rgb(0, .2, .7, alpha = .9))
lf = lm(newy~newx)
abline(lf[1], lf[2], col = rgb(.1, .1, .1, alpha = .8))#col = rgb(.7, .2, .2, alpha = .7))

text(0, 78, '    slope = 
\n       SE = 
\n p-value = ', cex = .55)
summary(lf)
text(.5, 78, '  -2.320
\n   0.883
\n   0.012', cex = .55)

legend(8, 120, cex = .55, bty = 'o', box.lty = 0, bg = rgb(.9, .88, .9),
       c('   RTW State ', 'Not RTW state'), text.col = c(rtw.col, n.rtw.col),
       title = ' right-to-work status in 2010 ', title.col = rgb(.1, .1, .1),
       xjust = .5)

# 6/7 plots
pdf(onefile = F, height = 4, width = 7)
lf = lm(dat$d.ue[index]~dat$pct.d.prod[index])
plot(dat$pct.d.prod[index], dat$d.ue[index],
     xlab = '% change in productivity (state rgdp / # employed)',
     ylab = 'Percentage point change in UE',
     main = '1976 - 2010',
     cex = .8, pch = 20, col = rgb(.2, .3, .9, alpha = .5))
abline(lf[1], lf[2], col = rgb(.7, .2, .2))

# compared to the okun inspection on ue vs rgdp
lf = lm(dat$d.ue[index]~dat$pct.d.rgdp[index])
plot(dat$pct.d.rgdp[index], dat$d.ue[index],
     xlab = '% change in state rgdp',
     ylab = 'Percentage point change in UE',
     main = '1976 - 2010',
     cex = .8, pch = 20, col = rgb(.2, .3, .9, alpha = .5))
abline(lf[1], lf[2], col = rgb(.7, .2, .2))

# and now okun approach with all years:

lf = lm(dat$d.ue~dat$pct.d.rgdp)
plot(dat$pct.d.rgdp, dat$d.ue,
     xlab = '% change in state rgdp',
     ylab = 'Percentage point change in UE',
     main = '1964 - 2010',
     cex = .8, pch = 20, col = rgb(.2, .3, .9, alpha = .5))
abline(lf[1], lf[2], col = rgb(.7, .2, .2))

# productivity vs unionization:
lf = lm(dat$pct.d.prod[index]~dat$d.mem[index])
plot(dat$d.mem[index], dat$pct.d.prod[index],
     xlab = 'Percentage point change in unionization',
     ylab = '% change in productivity (state rgdp / # employed)',
     main = '1976 - 2010',
     cex = .8, pch = 20, col = rgb(.2, .3, .9, alpha = .5))
abline(lf[1], lf[2], col = rgb(.7, .2, .2))

# productivity vs unionization 2:
lf = lm(dat$d.prod[index]~dat$d.mem[index])
plot(dat$d.mem[index], dat$d.prod[index],
     xlab = 'Percentage point change in unionization',
     ylab = 'Change in productivity (state rgdp / # employed)',
     main = '1976 - 2010',
     cex = .8, pch = 20, col = rgb(.2, .3, .9, alpha = .5))
abline(lf[1], lf[2], col = rgb(.7, .2, .2))
dev.off()

hist(dat$d.prod)

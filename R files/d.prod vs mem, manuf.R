load('.Rdata')

# dat = dat[(!(is.element(dat$state, c('Louisiana', 'North Dakota', 'Wyoming')))),]
# 
# manuf.share = round(dat$manemp*1000/dat$bea.empl*100, 3)
# dat$manuf.share = manuf.share


#####################
##                 ##
##  prod vs manuf  ##
##                 ##
#####################

top = 2.785; right = .605; bottom = -.2; left = -.069
x.ticks = c(left-1, seq(0,.6, by = .1), right+1)
y.ticks = c(bottom-1, seq(0, 2.5, by = .5), top+1)

par(mar = c(1.7, 2, .7, .5))
xy.fit.line(avg.annual.d.manuf, avg.annual.d.prod, text = T,
            slope.text = T, slope.text.x = .35, slope.text.y = .55,
            s.t.str.vec = c('slope = ', '\n      p = '),
            manual.axes = T, xticks = x.ticks, yticks = y.ticks,
            slope.text.cex = .65, x.pos = bottom, y.pos = left, 
            top.pos = top, right.pos = right)

#####################
##                 ##
##  prod vs union  ##
##                 ##
#####################

top = 2.785; right = .73; bottom = -.2; left = .068
x.ticks = c(left-1, seq(.1,.7, by = .1), right+1)
y.ticks = c(bottom-1, seq(0, 2.5, by = .5), top+1)

par(mar = c(1.7, 2, .7, .5))
xy.fit.line(avg.annual.d.mem, avg.annual.d.prod, text = T,
            maintext = NA, xtext = NA, ytext = NA, slope.text = T,
            slope.text.x = .22, slope.text.y = .5, s.t.str.vec = c('slope = ', '\n      p = '),
            manual.axes = T, xticks = x.ticks, yticks = y.ticks, slope.text.cex = .65,
            x.pos = bottom, y.pos = left, top.pos = top, right.pos = right)

output9.29 = data.frame(avg.annual.d.mem, avg.annual.d.prod)
names(output9.29) = c('Annual decline in unionization',
                      'Annual change in productivity')
write.table(output9.29, file = '9.29.output.txt')
write.table(state.abb[is.element(state.name, states)], file = 'state.abbs.txt')


end.early = 1984
begin.late = end.early+1
num.early.years = 1984-1969+1-2
num.late.years = 2010-1985+1-2

  ###################
  ##               ##
  ##  1969 - 1984  ##
  ##               ##
  ###################

 #####################
 ##                 ##
 ##  prod vs manuf  ##
 ##                 ##
 #####################

top = 2.2; right = .6485; bottom = -.825; left = -.1925
x.ticks = c(left-1, seq(-.2, 1, by = .2), right+.1)
y.ticks = c(bottom-.1, seq(-.5, 2, by = .5), top+.1)

par(mar = c(1.7, 2, .7, .5))

xy.fit.line(avg.ann.early.d.manuf, avg.ann.early.d.prod, text = T,
            maintext = NA, xtext = NA, ytext = NA, slope.text = T,
            slope.text.x = .38, slope.text.y = 0, s.t.str.vec = c('slope = ', '\n      p = '),
            manual.axes = T, xticks = x.ticks, yticks = y.ticks, slope.text.cex = .65,
            x.pos = bottom, y.pos = left, top.pos = top, right.pos = right)

 #####################
 ##                 ##
 ##   prod vs mem   ##
 ##                 ##
 #####################

top = 2.2; right = 1.0425; bottom = -.825; left = -.27
x.ticks = c(left-1, seq(-.2, 1, by = .2), right+.1)
y.ticks = c(bottom-.1, seq(-.5, 2, by = .5), top+.1)

par(mar = c(1.7, 2, .7, .5))

xy.fit.line(avg.ann.early.d.mem, avg.ann.early.d.prod, text = T,
            maintext = NA, xtext = NA, ytext = NA, slope.text = T,
            slope.text.x = .02, slope.text.y = -.1, s.t.str.vec = c('slope = ', '\n      p = '),
            manual.axes = T, xticks = x.ticks, yticks = y.ticks, slope.text.cex = .65,
            x.pos = bottom, y.pos = left, top.pos = top, right.pos = right)

 ###################
 ##               ##
 ##  1985 - 2010  ##
 ##               ##
 ###################

#####################
##                 ##
##  prod vs manuf  ##
##                 ##
#####################

top = 3.66; right = .66; bottom = .30; left = 0
x.ticks = c(left-1, seq(0, 1, by = .1), right+.1)
y.ticks = c(bottom-.1, seq(-.5, 3.5, by = .5), top+.1)

par(mar = c(1.7, 2, .7, .5))

xy.fit.line(avg.ann.late.d.manuf, avg.ann.late.d.prod, text = T,
            maintext = NA, xtext = NA, ytext = NA, slope.text = T,
            slope.text.x = .55, slope.text.y = .9, s.t.str.vec = c('slope = ', '\n      p = '),
            manual.axes = T, xticks = x.ticks, yticks = y.ticks, slope.text.cex = .65,
            x.pos = bottom, y.pos = left, top.pos = top, right.pos = right)

 #####################
 ##                 ##
 ##   prod vs mem   ##
 ##                 ##
 #####################

top = 3.66; right = .4335; bottom = .30; left = -.015
x.ticks = c(left-1, seq(-.2, 1, by = .1), right+.1)
y.ticks = c(bottom-.1, seq(-.5, 3.5, by = .5), top+.1)

par(mar = c(1.7, 2, .7, .5))

xy.fit.line(avg.ann.late.d.mem, avg.ann.late.d.prod, text = T,
            maintext = NA, xtext = NA, ytext = NA, slope.text = T,
            slope.text.x = .1, slope.text.y = .9, s.t.str.vec = c('slope = ', '\n      p = '),
            manual.axes = T, xticks = x.ticks, yticks = y.ticks, slope.text.cex = .65,
            x.pos = bottom, y.pos = left, top.pos = top, right.pos = right)

######################
##                  ##
##     RHS vars     ##
##                  ##
######################

par(mar = c(2,2,1,1))
# plot(1,1,type ='n', xlim = mem.range, ylim = manuf.range)
plot(1,1,type ='n', xlim = c(-.2, .4), ylim = c(0,.5), axes = F)
abline(full.rhs.var.fit$coef[1], full.rhs.var.fit$coef[2], col = 1, lty = 1)
abline(early.rhs.var.fit$coef[1], early.rhs.var.fit$coef[2], col = 'blue', lty = 2)
abline(late.rhs.var.fit$coef[1], late.rhs.var.fit$coef[2], col = 'red', lty = 3)
legend(.188, .47, cex = .7, legend = c('1969 - 2010     ', '1969 - 1984     ', '1985 - 2010     '), lty = 1:3, col = c('black', 'blue', 'red'))
# labels = state.abb[is.element(state.name, unique(dat$state))]
# points(avg.ann.early.d.mem, avg.ann.early.d.manuf, pch = 20 , cex = .8, col = 'blue')
text(-.12, .34, paste('   slope =  0.070', '\n        p = ', round(summary(late.rhs.var.fit)$coef[2,4], 3)), col = 'red', cex = .6)
text(-.12, .22, paste(' slope = ', round(full.rhs.var.fit$coef[2], 3), '\n      p = ', round(summary(full.rhs.var.fit)$coef[2,4], 3)), col = 'black', cex = .6)
text(-.12, .12, paste(' slope = ', round(early.rhs.var.fit$coef[2], 3), '\n      p = ', round(summary(early.rhs.var.fit)$coef[2,4], 3)), col = 'blue', cex = .6)
xticks = c(-1, seq(-.2, .4, by = .2), 2)
x.pos = -.02
axis(1, at = xticks, labels = c(NA, paste(xticks[-c(1)])), lwd = 1,
     lwd.ticks = 1, cex.axis = .5, padj = -1, pos = x.pos)
yticks = c(-1, seq(-0, .5, by = .1), 1)
y.pos = -.225
axis(2, at = yticks, labels = c(NA, paste(yticks[-1])), lwd = 1,
     lwd.ticks = 1, cex.axis = .5, padj = .3, pos = y.pos, las = 1)
axis(3, lwd = 1, lwd.ticks = 0, pos = .52, labels = NA, at = xticks)
axis(4, lwd = 1, lwd.ticks = 0, pos = .425, labels = NA, at = yticks)

###########
##
## prod vs mem regr summ
##
###########
par(mar = c(2,2,1,1))
plot(1,1,type ='n', xlim = d.mem.range, ylim = d.prod.range, axes = F)
abline(full.betas[1], full.betas[2], col = 1, lty = 1)
abline(early.betas[1], early.betas[2], col = 'blue', lty = 2)
abline(late.betas[1], late.betas[2], col = 'red', lty = 3)
legend(.562, 3.31, cex = .7, legend = c('1969 - 2010     ', '1969 - 1984     ', '1985 - 2010     '), lty = 1:3, col = c('black', 'blue', 'red'))
text(-.05, 2.5, paste('   slope = ', late.betas[2], '\n       p = ', '0.002'), col = 'red', cex = .6)
text(-.05, 1.5, paste(' slope = ', full.betas[2], '\n     p = ', '0.010'), col = 'black', cex = .6)
text(-.05, .7, paste(' slope = ', early.betas[2], '\n     p = ', '0.015'), col = 'blue', cex = .6)
xticks = c(-1, seq(-.2, 1, by = .2), 2)
x.pos = -.894
axis(1, at = xticks, labels = c(NA, paste(xticks[-c(1)])), lwd = 1,
     lwd.ticks = 1, cex.axis = .5, padj = -1, pos = x.pos)
yticks = c(-1.5, seq(-0, 3, by = 1), 4)
y.pos = -.27
axis(2, at = yticks, labels = c(NA, paste(yticks[-1])), lwd = 1,
     lwd.ticks = 1, cex.axis = .5, padj = .3, pos = y.pos, las = 1)
axis(3, lwd = 1, lwd.ticks = 0, pos = 3.72, labels = NA, at = xticks)
axis(4, lwd = 1, lwd.ticks = 0, pos = 1.041, labels = NA, at = yticks)

cor(avg.annual.d.mem, avg.annual.d.manuf)
cor(avg.ann.early.d.mem, avg.ann.early.d.manuf)
cor(avg.ann.late.d.mem, avg.ann.late.d.manuf)




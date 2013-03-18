par(mfrow = c(2,1), mar = c(4, 6, 2, 4))
k = 3

#69 to 10
begin = 1969
end = 2010
num.years = end - begin+1-2 #because of three years averages, the d.prod is the est change from begin+1 to end-1

prod.dat = k.wk.beg.and.end.avgs(dat, 'productivity', k, 1969, 2010)
d.prod = 100*(prod.dat[,2]-prod.dat[,1])/prod.dat[,1]
avg.annual.d.prod = d.prod/num.years

mem.dat = k.wk.beg.and.end.avgs(dat, 'mem', k, 1969, 2010)
d.mem = -(mem.dat[,2]-mem.dat[,1])
avg.annual.d.mem = d.mem/num.years

x.ticks = c(seq(0,.7, by = .1), .73)
y.ticks = c(-.2,seq(0, 3, by = .5))

par(mar = c(1.7, 2, .7, .5))
xy.fit.line(avg.annual.d.mem, avg.annual.d.prod, text = T,
            maintext = NA, xtext = NA, ytext = NA, slope.text = T, 
            slope.text.x = .22, slope.text.y = .5, s.t.str.vec = c('slope = ', '\n      p = '),
            manual.axes = T, xticks = x.ticks, yticks = y.ticks, slope.text.cex = .65,
            x.pos = -.195, y.pos = .0675, top.pos = 2.785, right.pos = .73)

end.early = 1984
begin.late = end.early+1
num.early.years = 1984-1969+1-2
num.late.years = 2010-1985+1-2

early.prod = k.wk.beg.and.end.avgs(dat, 'productivity', k, 1969, end.early)
late.prod = k.wk.beg.and.end.avgs(dat, 'productivity', k, begin.late, 2010)
early.d.prod = 100*(early.prod[,2]-early.prod[,1])/early.prod[,1]
late.d.prod = 100*(late.prod[,2]-late.prod[,1])/late.prod[,1]
avg.ann.early.d.prod = early.d.prod/num.early.years
avg.ann.late.d.prod = late.d.prod/num.late.years

early.mem = k.wk.beg.and.end.avgs(dat, 'mem', k, 1969, end.early)
late.mem = k.wk.beg.and.end.avgs(dat, 'mem', k, begin.late, 2010)
early.d.mem = -(early.mem[,2]-early.mem[,1])
late.d.mem = -(late.mem[,2]-late.mem[,1])
avg.ann.early.d.mem = early.d.mem/num.early.years
avg.ann.late.d.mem = late.d.mem/num.late.years

x.ticks = c(-.29,seq(-.2, 1, by = .2), 1.055)
y.ticks = c(-.85, seq(-.5, 2, by = .5), 2.2)

par(mfrow = c(1,1), mar = c(1.6, 1.9, .6, .5))
xy.fit.line(avg.ann.early.d.mem, avg.ann.early.d.prod, text = T,
            maintext = NA, xtext = NA, ytext = NA, slope.text = T, 
            slope.text.x = -.00, slope.text.y = .1, s.t.str.vec = c('slope = ', '\n      p = '),
            manual.axes = T, xticks = x.ticks, yticks = y.ticks, slope.text.cex = .65,
            x.pos = -.82, y.pos = -.27, top.pos = 2.195, right.pos = 1.042)

x.ticks = c(-.05,seq(0, .4, by = .1), .45)
y.ticks = c(.25, seq(.5, 3.5, by = .5), 3.7)

par(mfrow = c(1,1), mar = c(1.6, 1.9, .6, .5))
xy.fit.line(avg.ann.late.d.mem, avg.ann.late.d.prod, text = T,
            maintext = NA, xtext = NA, ytext = NA, slope.text = T, 
            slope.text.x = .095, slope.text.y = .85, s.t.str.vec = c('slope = ', '\n      p = '),
            manual.axes = T, xticks = x.ticks, yticks = y.ticks, slope.text.cex = .65,
            x.pos = .295, y.pos = -0.0135, top.pos = 3.66, right.pos = .435)

d.prod.range = range(c(avg.annual.d.prod, avg.ann.early.d.prod, avg.ann.late.d.prod))
d.mem.range = range(c(avg.annual.d.mem, avg.ann.early.d.mem, avg.ann.late.d.mem))

par(mar = c(3, 3, 2, 2))
full.betas = c(1.889134, -1.456732)
early.betas = c(1.1087369, -0.8285131)
late.betas = c(1.996925, -2.471660)
plot(1,1,type ='n', xlim = d.mem.range, ylim = d.prod.range)
abline(full.betas[1], full.betas[2], col = 1)
abline(early.betas[1], early.betas[2], col = 'blue')
abline(late.betas[1], late.betas[2], col = 'red')
legend(.6, 3.2, cex = .7, legend = c('1969 - 2010     ', '1969 - 1984     ', '1985 - 2010     '), lty = 1, col = c('black', 'blue', 'red'))
title(main = 'Relationship between productivity and union share by period', cex.main = .6)


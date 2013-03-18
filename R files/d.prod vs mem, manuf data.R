load('.Rdata')

###################
##               ##
##  1969 - 2010  ##
##               ##
###################

begin = 1969
end = 2010
num.years = end - begin+1-2 #because of three years averages, the d.prod is the est change from begin+1 to end-1

prod.dat = k.wk.beg.and.end.avgs(dat, 'productivity', k, 1969, 2010)
d.prod = 100*(prod.dat[,2]-prod.dat[,1])/prod.dat[,1]
avg.annual.d.prod = d.prod/num.years

manuf.dat = k.wk.beg.and.end.avgs(dat, 'manuf.share', k, 1969, 2010)
d.manuf = -(manuf.dat[,2]-manuf.dat[,1])
avg.annual.d.manuf = d.manuf/num.years

mem.dat = k.wk.beg.and.end.avgs(dat, 'mem', k, 1969, 2010)
d.mem = -(mem.dat[,2]-mem.dat[,1])
avg.annual.d.mem = d.mem/num.years

####################
##                ##
##  split period  ##
##                ##
####################

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

early.manuf = k.wk.beg.and.end.avgs(dat, 'manuf.share', k, 1969, end.early)
late.manuf = k.wk.beg.and.end.avgs(dat, 'manuf.share', k, begin.late, 2010)
early.d.manuf = -(early.manuf[,2]-early.manuf[,1])
late.d.manuf = -(late.manuf[,2]-late.manuf[,1])
avg.ann.early.d.manuf = early.d.manuf/num.early.years
avg.ann.late.d.manuf = late.d.manuf/num.late.years

early.mem = k.wk.beg.and.end.avgs(dat, 'mem', k, 1969, end.early)
late.mem = k.wk.beg.and.end.avgs(dat, 'mem', k, begin.late, 2010)
early.d.mem = -(early.mem[,2]-early.mem[,1])
late.d.mem = -(late.mem[,2]-late.mem[,1])
avg.ann.early.d.mem = early.d.mem/num.early.years
avg.ann.late.d.mem = late.d.mem/num.late.years

##
## union vs manuf regression
##

full.rhs.var.fit = lm(avg.annual.d.manuf ~ avg.annual.d.mem)
early.rhs.var.fit = lm(avg.ann.early.d.manuf ~ avg.ann.early.d.mem)
late.rhs.var.fit = lm(avg.ann.late.d.manuf ~ avg.ann.late.d.mem)

##
## prod vs union regression summary
##

full.betas = c(1.889, -1.457); early.betas = c(1.109, -0.829); late.betas = c(1.997, -2.472)
d.prod.range = range(c(avg.annual.d.prod, avg.ann.early.d.prod, avg.ann.late.d.prod))
d.mem.range = range(c(avg.annual.d.mem, avg.ann.early.d.mem, avg.ann.late.d.mem))

#####################
##                 ##
##    bivariate    ##
##                 ##
#####################

early.bivar.fit = lm(avg.ann.early.d.prod ~ avg.ann.early.d.manuf + avg.ann.early.d.mem)
late.bivar.fit = lm(avg.ann.late.d.prod ~ avg.ann.late.d.manuf + avg.ann.late.d.mem)
full.bivar.fit = lm(avg.annual.d.prod ~ avg.annual.d.manuf + avg.annual.d.mem)
summary(early.bivar.fit)$coef
summary(late.bivar.fit)$coef
summary(full.bivar.fit)$coef

# summary statistics

values = data.frame(avg.annual.d.mem, avg.ann.early.d.mem, avg.ann.late.d.mem,
                    avg.annual.d.manuf, avg.ann.early.d.manuf, avg.ann.late.d.manuf,
                    avg.annual.d.prod, avg.ann.early.d.prod, avg.ann.late.d.prod)

summ = matrix(c(apply(values, 2, mean), apply(values, 2, sd),
                apply(values, 2, range)[1,], apply(values, 2, range)[2,]), 
              byrow = F, ncol = 4)
rownames(summ) = names(values)
colnames(summ) = c('mean', 'sd', 'min', 'max')

write.table(summ, file = 'prod mem manuf summary.txt')

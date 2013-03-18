load('.Rdata')

# want to write a function that:
1. Determines the slope of empl/non.farm.empl ratio by state over a given range of years
  a) define ratio = empl/non.farm.empl
  b) define s = slope
  c) define r = range of years used to determine s
2. Uses s to extrapolate the feature back a given number of years
  a) define n = number of years to extrapolate with s
  b) define e.val = n*(number of states) estimated feature values
3. Compares the extrapolated values with true values (if known), determines error
  a) define t.val = n*(number of states) true feature values
  b) define e.err = e.val - t.val
4. Returns 

ratio = dat$LAUS.empl/dat$CPS.empl
# going to use 1981-1985 data to predict 76-80 data:
begin = 1976
num.years = 20
r = begin:(begin+num.years-1)
big.r = 1964:(begin+num.years-1)
x.vec = ts(r)
big.x = ts(big.r)
temp = matrix(ratio[is.element(dat$year, r)], byrow = T, nrow = length(unique(dat$state)))
colnames(temp) = r
rownames(temp) = unique(dat$state)
for (i in 1:nrow(temp)){
  state.y = temp[i,]
  lf = lm(state.y~x.vec)
  plot(x.vec, state.y, xlim = range(big.x), ylim = c(1, 1.4), cex.main = .8,
       ylab = unique(dat$state)[i], type = 'p',
       xlab = 'total/non farm empl ratio in given year',
       main = paste('slope = ',
                    round(lf$coef[2], 3), ';  1964 estimate = ',
                    round(lf$coef[1]+1964*lf$coef[2], 3), ';  p.val = ',
                    round(summary(lf)$coef[2,4], 3), sep = ''))
  abline(lf$coef[1], lf$coef[2], col = 'red')
#   laus.est = 
}


load('okun.dataframe.Rout')

extrapolate.plot.productivity = function(data = dat, begin.learn = 1978, 
                                         num.years.learn = 3, begin.extrap = 1964,
                                         plotted.rows = 3, plotted.columns = 1){
end.extrap = begin.learn - 1
learn.range = begin.learn:(begin.learn+num.years.learn-1)
full.range = 1964:2010
extrap.range = begin.extrap:end.extrap
error.check.range = 1976:end.extrap
non.test.known.ratio.range = (begin.learn+num.years.learn):2010

x.vec = ts(learn.range)
full.x = ts(full.range)
ratio = dat$LAUS.empl/data$CPS.empl
ratio.matrix = matrix(ratio[is.element(data$year, learn.range)], byrow = T, nrow = length(unique(data$state)))
colnames(ratio.matrix) = learn.range
rownames(ratio.matrix) = unique(data$state)

state.list = list()
prod.rmses = c()
prod.means = c()

for (s in 1:nrow(ratio.matrix)){
  state.y = ratio.matrix[s,]
  lf = lm(state.y~x.vec)
  extrapolated.ratio = lf$coef[1]+(extrap.range)*lf$coef[2]
  predict.index.data = data$state == unique(data$state)[s] & is.element(data$year, extrap.range)
  predicted.empl = extrapolated.ratio * data$CPS[predict.index.data]
  names(predicted.empl) = extrap.range
  error.check.index.data = data$state == unique(data$state)[s] & is.element(data$year, error.check.range)
  state.empl.err = round((predicted.empl[is.element(names(predicted.empl), error.check.range)] 
                    - data$LAUS.empl[error.check.index.data]), 3)
  pct.state.empl.err = round(100*state.empl.err/data$LAUS.empl[error.check.index.data], 3) # 100 for percent
  predicted.prod = round(1000*data$rgdp[predict.index.data]/predicted.empl, 3) # 1000 for prod in thou per empl
  names(predicted.prod) = extrap.range
  state.prod.err = round((predicted.prod[is.element(names(predicted.prod), error.check.range)] 
                    - data$productivity[error.check.index.data]), 3)
  pct.state.prod.err = round(100*state.prod.err/data$productivity[error.check.index.data], 3)
  pad = rep(NA, length(begin.extrap:(error.check.range[1]-1)))
  state.info = cbind(predicted.empl, c(pad, state.empl.err), c(pad, pct.state.empl.err),
                     predicted.prod, c(pad, state.prod.err), c(pad, pct.state.prod.err))
  means = round(apply(na.exclude(state.info), 2, mean), 3)
  rmses = round(apply(sqrt(na.exclude(state.info)^2), 2, mean), 3)
  state.info = rbind(state.info, means, rmses)  
  colnames(state.info) = c('pred.empl', 'err.empl', '% err.empl', 'pred.prod', 'err.prod', '% err.prod')
  state.list[[paste(rownames(ratio.matrix)[s])]] = state.info
  
  prod.rmses[s] = state.info['rmses', 'err.prod']
  prod.means[s] = state.info['means', 'err.prod']
  
}
print(mean(prod.rmses)); print(mean(prod.means))

  #this loop plots each state ratio vs year with regression line
par(mfrow = c(plotted.rows,plotted.columns))
for (i in 1:nrow(ratio.matrix)){
  state.y = ratio.matrix[i,]
  lf = lm(state.y~x.vec)
  plot(x.vec, state.y, xlim = range(full.x), ylim = c(1, 1.4), cex.main = .9,
       ylab = unique(data$state)[i], type = 'p',
       xlab = 'total/non farm empl ratio in given year',
       main = paste('slope = ',
                    round(lf$coef[2], 3), ';  1964 estimate = ',
                    round(lf$coef[1]+1964*lf$coef[2], 3), ';  p.val = ',
                    round(summary(lf)$coef[2,4], 3), sep = ''))
  abline(lf$coef[1], lf$coef[2], col = 'red')
  actual.ratio = (data$LAUS.empl/data$CPS.empl)[data$state == unique(data$state)[i] & is.element(data$year, error.check.range)]
  points(ts(error.check.range), actual.ratio, type = 'p', pch = 20)
  actual.ratio = (data$LAUS.empl/data$CPS.empl)[data$state == unique(data$state)[i] & is.element(data$year, non.test.known.ratio.range)]
  points(ts(non.test.known.ratio.range), actual.ratio, type = 'p', pch = 20)
  text(1995, 1.35, paste('pct.prod.error rmse  =', prod.rmses[i], 
                        '\nprct.prod.error mean  =', prod.means[i], sep = '  '), cex = .8)
}
par(mfrow = c(1,1))
}
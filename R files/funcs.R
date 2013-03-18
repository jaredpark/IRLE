#################
#
#    Functions
#
#################


boot.trend = function(data, fixed.effects = T){
  index = sample(1:nrow(data), size = nrow(data), replace = T)
  fit = lm(d.ue~pct.d.rgdp + state, data = data[index, ])
  int = as.numeric(coef(fit)[1])
  slope = as.numeric(coef(fit)[2])
  if (fixed.effects) {
    offsets = as.numeric(coef(fit)[3:length(coef(fit))])
    offsets = c(0, offsets)
    mean.intercept = mean(offsets) + int
    boot.trend.est = -mean.intercept/slope
  } else {
    boot.trend.est = -int/slope
  }
  return(boot.trend.est)
}

stage1 = function(data, B = 100, fixed.effects = T){

  lm1 = lm(d.ue~pct.d.rgdp + state, data = data)
  int = as.numeric(coef(lm1)[1])
  slope = as.numeric(coef(lm1)[2])
  if (fixed.effects){
    offsets = as.numeric(coef(lm1)[3:length(coef(lm1))])
    offsets = c(0, offsets)
    mean.intercept = mean(offsets) + int
    trend.est = -mean.intercept/slope
  } else {
    trend.est = -int/slope
  }
  trend.samples = replicate(B, boot.trend(data = data, fixed.effects = fixed.effects))
  values = list('fit' = lm1, 'trend.estimate' = mean(trend.samples), 'trend.var' = var(trend.samples))
  return(values)
}


plot.abline = function(x, y){
  fit = lm(y~x)
  coefs = fit$coefficients
  plot(x, y, pch = 20, cex = .9, col = rgb(.2, .2, .8, alpha = .8))
  abline(coefs[1], coefs[2])
}

states = unique(dat$state)
xy.fit.line = function(x, y, labs = c(), xtext = '', ytext = '', maintext = '', cex = .8, text = F,
                       pch = 20, col = rgb(.2, .3, .9, alpha = .5), plot.type = 'p', year.coverage = '',
                       slope.text = F, slope.text.x = 0, slope.text.y = 1, s.t.str.vec = c('slope', 'p value'), slope.text.cex = .8,
                       manual.axes = F, xticks, yticks, x.pos, y.pos, top.pos, right.pos,
                       text.col = 'black', line.col = 'black'){
  index = !is.na(x) & !is.na(y)
  newx = as.numeric(x[index])
  newy = as.numeric(y[index])
  lf = lm(newy~newx)
  if (text){
    plot.type = 'n'
    labels = state.abb[is.element(state.name, states)]
    plot(newx, newy, xlab = xtext, ylab = ytext, main = maintext, type = plot.type,
         cex = cex, pch = pch, col = col, axes = !manual.axes)
    text(newx, newy, labels, cex = .5, col = text.col)#  rgb(0, .2, .7, alpha = .9)  ifelse(rtw.index, rtw.col, n.rtw.col))
    title(main = year.coverage, cex.main = .8)
  } else {
    plot(newx, newy, xlab = xtext, ylab = ytext, main = maintext, type = plot.type,
         cex = cex, pch = pch, col = col)
  }
  if (manual.axes){
    axis(1, at = xticks, labels = c(NA, paste(xticks[-1])), lwd = 1,
         lwd.ticks = 1, cex.axis = .5, padj = -1, pos = x.pos)
    axis(2, at = yticks, labels = c('', paste(yticks[-c(1,length(yticks))]), ''), lwd = 1,
         lwd.ticks = 1, las = 1, hadj = .7, cex.axis = .5, pos = y.pos)
    axis(3, lwd = 1, lwd.ticks = 0, pos = top.pos, labels = rep(NA, length(xticks)), at = xticks)
    axis(4, lwd = 1, lwd.ticks = 0, pos = right.pos, labels = rep(NA, length(yticks)), at = yticks)
  }
  abline(lf$coef[1], lf$coef[2], col = line.col) # rgb(.7, .2, .2, alpha = .5))
  if(slope.text){
    text(slope.text.x, slope.text.y, paste(s.t.str.vec[1], round(lf$coef[2], 3),
                                           s.t.str.vec[2], round(summary(lf)$coefficients[2,4], 4)), cex = slope.text.cex)
  }
 print(summary(lf)$coefficient)
}

k.wk.beg.and.end.avgs = function(dat, feature.name, k, begin, end){
  n.states = length(unique(dat$state))
  newdat = matrix(rep(NA, n.states*2), nrow = n.states)
  rownames(newdat) = unique(dat$state)
  colnames(newdat) = c('initial avg', 'final avg')
  for (state.number in 1:n.states){
    THIS.STATE = unique(dat$state)[state.number]
    temp = c(NA, NA)
    for (i in 1:2){
      if (i == 1){
        year.range = begin:(begin+k-1)
      } else {
        year.range = (end-k+1):end
      }
      temp[i] = mean(dat[, feature.name][dat$state == THIS.STATE][is.element(unique(dat$year), year.range)])
    }
    newdat[state.number, ] = temp
  }
  return(newdat)
}
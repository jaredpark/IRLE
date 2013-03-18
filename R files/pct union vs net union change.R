union.diff = dat$mem[dat$yr == 47] - dat$mem[dat$yr == 1]
plot(1:46, union.diff, xlab = 'States by alphabetical order', pch = 20, 
     main = 'Net change in unionization rate', col = rgb(.2,.2,.8),
     ylab = 'Pct point change in unionization rate')
identify(1:46, union.diff, labels = unique(dat$state), cex = .8)
hist(union.diff, breaks = 20, main = 'Change in unionization rate by state', xlab = '(dashed red line through mean of observed changes)')
abline(v = mean(union.diff), lty = 2, col = 'red')
union.diff.order = order(union.diff)

pct.union.diff = union.diff/dat$mem[dat$yr == 1]*100
abbreviated = state.abb[match(unique(dat$state), state.name)]
plot(1:46, pct.union.diff, type = 'n', pch = 20,
     xlab = 'States ordered alphabetically',
     ylab = 'Percentage point change in unionization rate',
     main = 'Percentage point change by state')
text(1:46, pct.union.diff, labels = abbreviated, cex = .7,
     col = ifelse(pct.union.diff> (-62.5) & pct.union.diff< (-56.8), 'red', 1))
pct.diff.order = order(pct.union.diff)

index = pct.diff.order[-]
plot(1:45, pct.union.diff[pct.union.diff<0], type = 'n', pch = 20,
     xlab = 'States ordered alphabetically',
     ylab = 'Percentage point change in unionization rate',
     main = '', axes = F)
text(1:45, pct.union.diff[pct.union.diff<0], labels = abbreviated[-10], cex = .7,
     col = ifelse(pct.union.diff[pct.union.diff<0] > (-62.5) & pct.union.diff[pct.union.diff<0]< (-56.8), 'red', 1))
axis(2, at = c(-81, seq(-80, -30, by = 10)), 
     labels = c('', '-80%', '-70%', '-60%', '-50%','-40%', '-30%'))
axis(1, at = c(-1,50))

hist(pct.union.diff, breaks = 20, main = 'Percentage point change in unionization rate', xlab = '(dashed red line through mean)')
abline(v = mean(pct.union.diff), lty = 2, col = 'red')

pct.union.decline.order = order(pct.union.diff, decreasing = F) #so most negative is first

pct.d.union.table = data.frame(unique(dat$state)[pct.union.decline.order], round(pct.union.diff[pct.union.decline.order], 3)
d.union.table = data.frame(unique(dat$state)[union.diff.order], union.diff[union.diff.order])

write.csv(pct.d.union.table, row.names = F, file = 'pct.d.union.csv')
write.csv(d.union.table, row.names = F, file = 'd.union.csv')


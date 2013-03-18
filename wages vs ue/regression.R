setwd("~/Projects/Reich/wages vs ue")
source('cleandata.R')
library('car')
View(data)
#data$real_hrearn = (data$wage_change)
attach(data)

#First a look at all of the data:
lm.all = lm(real_hrearn~ue)

plot(ue, real_hrearn)
abline(coef(lm.all)[1], coef(lm.all)[2], col = 'red')
summary(lm.all)

#Now all data 1977-2010
lm.post76 = lm(real_hrearn[year>=1977]~ue[year>=1977])
plot(ue[year>=1977], real_hrearn[year>=1977])
abline(coef(lm.post76)[1], coef(lm.post76)[2], col = 'red')
summary(lm.post76)

#Now all states for pre and post 1985
lm.pre85 = lm(real_hrearn[period=='pre85']~ue[period=='pre85'])
plot(ue[period=='pre85'], real_hrearn[period=='pre85'])
abline(coef(lm.pre85)[1], coef(lm.pre85)[2], col = 'red')
summary(lm.pre85)

lm.post85 = lm(real_hrearn[period=='post85']~ue[period=='post85'])
plot(ue[period=='post85'], real_hrearn[period=='post85'])
abline(coef(lm.post85)[1], coef(lm.post85)[2], col = 'red')
summary(lm.post85)

#Now more and less union decline for all time periods
lm.less = lm(real_hrearn[union_decline=='less']~ue[union_decline=='less'])
plot(ue[union_decline=='less'], real_hrearn[union_decline=='less'])
abline(coef(lm.less)[1], coef(lm.less)[2], col = 'red')
summary(lm.less)

lm.more = lm(real_hrearn[union_decline=='more']~ue[union_decline=='more'])
plot(ue[union_decline=='more'], real_hrearn[union_decline=='more'])
abline(coef(lm.more)[1], coef(lm.more)[2], col = 'red')
summary(lm.more)

# I want to regress y, real_hrearn, on x, ue, for the four combinations of mean_mu and period

groups = matrix(c(period == 'pre85' & union_decline == 'less',
                  period == 'pre85' & union_decline == 'more',
                  period == 'post85' & union_decline == 'less',
                  period == 'post85' & union_decline == 'more'),
                byrow = F, ncol = 4)
sum(groups) == nrow(data)

fits = list()
for (i in 1:ncol(groups)){
  fits[[paste("fit", i, sep ="")]] = lm(real_hrearn~ue, data = data[groups[,i], ])
}

# par(mfrow=c(2,2))
# for (i in 1:4){
#   plot.lm(fits[[i]])
# }

xlabels = c('', '', 'Unemployment Rate', 'Unemployment Rate')
ylabels = c('Hourly earnings', '', 'Hourly earnings', '')
titles = c('Pre 1985, less decline', 'Pre 1985, more decline', 'Post 1985, less decline', 'Post 1985, more decline'  )

par(mfrow=c(2,2))
for (i in 1:4){
  plot(ue[groups[,i]], real_hrearn[groups[,i]],
       xlab = xlabels[i], ylab = ylabels[i], main = titles[i])
  abline(coef(fits[[i]])[1], coef(fits[[i]])[2], col = 'red')
}

for (i in 1:4){
  print(summary(fits[[i]]))
}
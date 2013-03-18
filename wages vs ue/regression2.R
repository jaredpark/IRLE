setwd("~/Projects/Reich/wages vs ue")
# load('.Rdata')
source('cleandata.R')
# attach(data)
# View(data)

# I am going to run a total of 20 regressions. I am going to save the coefficients,
# standard errors, t values and p values in a matrix, which I will export with
# write.table at the end. For each regression I use a different subset of 'data';
# I will make a matrix with each column corresponding to the subset needed for a
# particular regression. This will be a logical matrix with 20 columns and N rows.
# The column names will be 1:20, which will correspond to a string vector with 
# descriptive titles for each regression subset.

wage.names = c('All states, 1964-2010',
            'All states, 1977-2010',
            'All states, 1964-1984',
            'All states, 1985-2010',
            'Less union decline, 1964-2010',
            'More union decline, 1964-2010',
            'Less union decline, 1964-1984',
            'More union decline, 1964-1984',
            'Less union decline, 1985-2010',
            'More union decline, 1985-2010')
all.names = c(wage.names, wage.names)

R = 20 # the number of Regressions being run in total
subsets = matrix(rep(NA, R*nrow(data)), ncol = R)
subsets[,c(1,11)] = T 
subsets[,c(2,12)] = year>=1977
subsets[,c(3,13)] = year<1985
subsets[,c(4,14)] = year>=1985
subsets[,c(5,15)] = mu == 'less'
subsets[,c(6,16)] = mu == 'more'
subsets[,c(7,17)] = mu == 'less' & year<1985
subsets[,c(8,18)] = mu == 'more' & year<1985
subsets[,c(9,19)] = mu == 'less' & year>=1985
subsets[,c(10,20)] = mu == 'more' & year>=1985
# View(subsets)

attach(data)
# First the 10 wage~ue regressions
out = matrix(rep(NA, 4*2*R), ncol = 4)
for (i in 1:10){
  index = subsets[,i]
  temp = lm(log(wage)~ue, data = data[index,])
  out[(2*i-1):(2*i),] = summary(temp)[[4]]
  pdf(paste('~/Projects/Reich/wages vs ue/plots/regression', i, '.pdf', sep = ''))
  plot(ue[index], log(wage[index]),
       xlab = paste('UE rate        ', 'Int = ', round(coef(temp)[1],3), ' Slope = ', round(coef(temp)[2], 3), sep = ''),
       ylab = 'ln(Hourly Earnings)',
       main = paste(all.names[i], ', #', i, sep = ''))
  abline(temp$coefficient[1], temp$coefficient[2], col = 'red')
  dev.off()
}
# and now the 10 identical d.wage~ue regressions
for (i in 11:20){
  index = subsets[,i]
  temp = lm(d.wage~d.ue, data = data[index,])
  out[(2*i-1):(2*i),] = summary(temp)[[4]]
  pdf(paste('~/Projects/Reich/wages vs ue/plots/regression', i, '.pdf', sep = ''))
  plot(d.ue[index], d.wage[index],
       xlab = paste('Change in UE rate        ', 'Int = ', round(coef(temp)[1],3), ' Slope = ', round(coef(temp)[2], 3), sep = ''),
       ylab = 'Change in Hourly Earnings',
       main = paste(all.names[i], ', #', i, sep = ''))
  abline(temp$coefficient[1], temp$coefficient[2], col = 'red')
  dev.off()
}
# View(out)

# feed 'out' to a .csv file
new.all.names = rep(NA, 40)
new.all.names[2*(1:20)-1] = all.names
new.all.names[2*(1:20)] = all.names

write.table(x = round(out,3), 
            file = 'regression_coefs_4.txt', 
            row.names = new.all.names)
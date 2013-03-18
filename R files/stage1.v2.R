
#################
#
#  Setting up data indexes for 3 union groups
#
#################

i1 = 1:nrow(dat)
i2 = which(dat$year < 1986)
i3 = which(dat$year >= 1986)
i4 = which(dat$union.decr == 'large')
i5 = which(dat$union.decr == 'med')
i6 = which(dat$union.decr == 'small')

indexes = list(reg1 = intersect(i1, i6), reg2 = intersect(i1, i5),
                     reg3 = intersect(i1, i4), reg4 = intersect(i2, i6),
                     reg5 = intersect(i2, i5), reg6 = intersect(i2, i4), 
                     reg7 = intersect(i3, i6), reg8 = intersect(i3, i5), 
                     reg9 = intersect(i3, i4))

#################
#
#  Setting up data indexes for 2 union groups
#
#################

i1 = 1:nrow(dat)
i2 = which(dat$year < 1986)
i3 = which(dat$year >= 1986)
i4 = which(dat$union.decr.2 == 'large')
i5 = which(dat$union.decr.2 == 'small')

indexes = list(reg1 = intersect(i1, i5), reg2 = intersect(i1, i4),
               reg3 = intersect(i2, i5), reg4 = intersect(i2, i4),
               reg5 = intersect(i3, i5), reg6 = intersect(i3, i4))


##################
#
#  Now regressions
#
##################

#with fixed effects
results = list()
for (i in 1:length(indexes)){
  results[[i]] = stage1(data = dat[indexes[[i]], ])
}

t2output = matrix(rep(NA, 8*length(results)), ncol = length(results))
for ( i in 1:length(results)){
  temporary = round(c(summary(results[[i]]$fit)$coefficients[2,1],
                summary(results[[i]]$fit)$coefficients[2,2],
                summary(results[[i]]$fit)$coefficients[1,1],
                summary(results[[i]]$fit)$coefficients[1,2],
                as.numeric(results[[i]][2]),
                as.numeric(results[[i]][3]),
                length(summary(results[[i]]$fit)$residuals),
                summary(results[[i]]$fit)$adj.r.squared), 3)
  t2output[,i] = temporary
}

colnames(t2output) = 1:length(results)
rownames(t2output) = c('slope', 'se', 'constant', 'se', 'trend', 'se', 'n', 'r sqrd')
write.csv(t2output, 'table2.5_14.v3.csv')

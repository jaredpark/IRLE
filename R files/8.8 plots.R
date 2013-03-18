load('.Rdata')

mean.mem.69to10 = rep(NA, length(unique(dat$state)))
for(i in 1:length(unique(dat$state))){
  mean.mem.69to10[i] = mean(dat$mem[dat$state == unique(dat$state)[i] & dat$year>=1969])
}

plots.8.8 = data.frame('mean.mem' = mean.mem.69to10[order(mean.mem.69to10)],
                       'prod.growth.69.10' = avg.annual.d.prod[order(mean.mem.69to10)], 
                       'prod.growth.69.84' = avg.ann.early.d.prod[order(mean.mem.69to10)], 
                       'prod.growth.85.10' = avg.ann.late.d.prod[order(mean.mem.69to10)])

#3 plots, avg ann prod growth in period vs avg union share in 69 to 10
for (i in 2:4){
  plot(plots.8.8[,1], plots.8.8[,i])
  abline(v = mean(plots.8.8[23:24,1]))
}

#6 plots, avg ann prod growth in period vs avg union share in 69 to 10 by mem group

for (i in 2:4){
  
  for (j in 1:2){
    if(j == 1){
      plot(plots.8.8[1:23,1], plots.8.8[1:23,i])
    } else {
      plot(plots.8.8[24:46,1], plots.8.8[24:46,i])
    }
  }
}

#6 boxplots, each period by group

boxplot(avg.annual.d.prod[order(mean.mem.69to10)][1:23], #full/low
        avg.annual.d.prod[order(mean.mem.69to10)][24:46]) #full/high
boxplot(avg.ann.early.d.prod[order(mean.mem.69to10)][1:23], #early/low
        avg.ann.early.d.prod[order(mean.mem.69to10)][24:46]) #early/high
boxplot(avg.ann.late.d.prod[order(mean.mem.69to10)][1:23], #late/low
        avg.ann.late.d.prod[order(mean.mem.69to10)][24:46]) #late/high
        
        
        



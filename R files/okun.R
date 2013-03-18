setwd("~/R files/Okun")
okun.data = read.csv("okuns_regsdata.csv", header = T)
View(okun.data)
okun.data$mu = factor(okun.data$mu, labels =c("less", "more"))
colnames(okun.data)

#regressing on all data
lm(due~pc_rgdp, data = okun.data)
#and with state effects
okun.data$fips = factor(okun.data$fips)
sheesh = lm(due~pc_rgdp+fips, data = okun.data)
mean(coef(sheesh)[2:length(coef(sheesh))])+coef(sheesh)[1]

#regressing on all periods by mu
less.index = okun.data$mu == "less"
rmu1 = lm(due~pc_rgdp, data = okun.data[less.index,])
rmu2 = lm(due~pc_rgdp, data = okun.data[!less.index,])

#regressing on all mu by periods
period.index.1 = okun.data$decade == "1964-1979"
period.index.2 = okun.data$decade == "1980-1989"
period.index.3 = okun.data$decade == "3"
lm(due~pc_rgdp, data = okun.data[period.index.1,])
lm(due~pc_rgdp, data = okun.data[period.index.2,])
lm(due~pc_rgdp, data = okun.data[period.index.3,])

#regressing all 3*2 combinations of mu and period
less.1.index = less.index & period.index.1
less.2.index = less.index & period.index.2
less.3.index = less.index & period.index.3
more.1.index = !less.index & period.index.1
more.2.index = !less.index & period.index.2
more.3.index = !less.index & period.index.3
all = less.1.index | less.2.index | less.3.index | more.1.index | more.2.index | more.3.index
sum(all) == nrow(okun.data) #whew
r1 = lm(due~pc_rgdp, data = okun.data[less.1.index,])
r2 = lm(due~pc_rgdp, data = okun.data[less.2.index,])
r3 = lm(due~pc_rgdp, data = okun.data[less.3.index,])
r4 =lm(due~pc_rgdp, data = okun.data[more.1.index,])
r5 =lm(due~pc_rgdp, data = okun.data[more.2.index,])
r6 =lm(due~pc_rgdp, data = okun.data[more.3.index,])

coef(r4)[1]/coef(r4)[2]; coef(r1)[1]/coef(r1)[2]
coef(r5)[1]/coef(r5)[2]; coef(r2)[1]/coef(r2)[2]
coef(r6)[1]/coef(r6)[2]; coef(r3)[1]/coef(r3)[2]


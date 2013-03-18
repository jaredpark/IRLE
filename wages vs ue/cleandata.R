data = read.delim("data_3_30.txt", header = T, sep = "\t")
#names(data)
  # grabbing only the data I need:
mydata = data[, c(2, 3, 10, 24, 13)]
#View(mydata)
#summary(mydata)
#mydata$dunionrate[is.na(mydata$dunionrate)]
  # getting rid of phantom entries
mydata = mydata[!is.na(mydata$dunionrate),]
  # making state a factor
mydata$state = factor(mydata$state)

  # setting up the unionization level groups of states
mydata$mu = ifelse(mydata$dunionrate >= median(mydata$dunionrate), 'more', 'less')

  # setting up the year to year ue rate changes
ue.change = mydata$ue[2:nrow(mydata)] - mydata$ue[1:(nrow(mydata)-1)]
#the first entry is the change in wage in the first state from the first year to the second year
ue.change = c(NA, ue.change) #and now the entries correspond to the ue rate that 'led' to the change in wage
ue.change[1+(0:48)*47] = NA
mydata$d.ue = ue.change

  # setting up the year to year wage changes
wage.change = mydata$real_hrearn[2:nrow(mydata)] - mydata$real_hrearn[1:(nrow(mydata)-1)]
#the first entry is the change in wage in the first state from the first year to the second year
wage.change = c(NA, wage.change) #and now the entries correspond to the ue rate that 'led' to the change in wage
wage.change[1+(0:48)*47] = NA
mydata$d.wage = wage.change


  # reorganizing the data prior to analysis
#View(mydata)
names(mydata)
attach(mydata)

data = data.frame(year, state, 'd.union' = dunionrate, mu, ue, d.ue, 'wage' = real_hrearn, d.wage, row.names = NULL)
#View(data)

rm(mydata)
require(foreign)
data = read.dta('~/GitHub/IRLE/countyPairs/county-pairs with codes.dta')
fipsPairs = data[, 1]
fipsPair1 = as.numeric(gsub('^(.*)-(.*)', '\\1', fipsPairs))
fipsPair2 = as.numeric(gsub('^(.*)-(.*)', '\\2', fipsPairs))
pairsData = cbind(fipsPair1, fipsPair2)
write.table(pairsData, file = './county-pairs fips.txt', row.names = F, col.names = F)



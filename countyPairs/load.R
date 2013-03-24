require(foreign)
data = read.dta('~/GitHub/IRLE/countyPairs/county-pairs with codes.dta')
fipsPairs = data[, 1]
fipsPair1 = as.numeric(gsub('^(.*)-(.*)', '\\1', fipsPairs))
fipsPair2 = as.numeric(gsub('^(.*)-(.*)', '\\2', fipsPairs))
pairsData = cbind(fipsPair1, fipsPair2)
write.table(pairsData, file = './county-pairs fips.txt', row.names = F, col.names = F)

alabama = c()
for (i in 1:nrow(a)){
  rowString = as.character(a[i,])
  rowString = gsub('\\.', '', rowString)
  if (grepl('([[:digit:]]+  [[:alpha:]]+ ?[[:alpha:]]+).*', rowString)){
    alabama = c(alabama, gsub('([[:digit:]]+  [[:alpha:]]+ ?[[:alpha:]]+).*', '\\1', rowString))
  }
  if (grepl('.* ([[:digit:]]+  [[:alpha:]]+ ?[[:alpha:]]+) +.*', rowString)){
    alabama = c(alabama, gsub('.* ([[:digit:]]+  [[:alpha:]]+ ?[[:alpha:]]+) +.*', '\\1', rowString))
  }
  if (grepl('.* ([[:digit:]]+  [[:alpha:]]+ ?[[:alpha:]]+)$', rowString)){
    alabama = c(alabama, gsub('.* ([[:digit:]]+  [[:alpha:]]+ ?[[:alpha:]]+)$', '\\1', rowString))
  }
}
alabama
  
testString = '111 Fine     23442  The Problem   234234  Go GO'
gsub('.* ([[:digit:]]+  [[:alpha:]]+ ?[[:alpha:]]+)$', '\\1', testString)

alabama

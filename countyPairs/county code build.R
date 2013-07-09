statePrefixes = data.frame(state.name[-2], fips.prefix = c(1, 4, 5, 6, 8, 9, 10, 12, 13,
                                                           15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
                                                           25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
                                                           35, 36, 37, 38, 39, 40, 41, 42, 44, 45,
                                                           46, 47, 48, 49, 50, 51, 53, 54, 55, 56))

stateTables = list()
counter = 1
for (state in as.character(statePrefixes$state.name)){
  print(paste('beginning', state))
  stateData = read.table(paste('~/GitHub/IRLE/countyPairs/fips codes/', state, '.txt', sep = ''), 
                         skip = 4, sep = '\t')
  outData = c()
  for (i in 1:nrow(stateData)){
    rowString = as.character(stateData[i,])
    rowString = gsub('\\.', '', rowString)
    rowString = gsub('\\*', ' ', rowString)
    # the first one in a line
    if (grepl('([[:digit:]]+  ([[:alpha:]]+ ?)+).*', rowString)){
      outData = c(outData, gsub('([[:digit:]]+  ([[:alpha:]]+ ?)+).*', '\\1', rowString))
    }
    # not the first one in a line, and not the last one in a line
    if (grepl('.* ([[:digit:]]+  ([[:alpha:]]+ ?)+) +[[:digit:]]+.*', rowString)){
      outData = c(outData, gsub('.* ([[:digit:]]+  ([[:alpha:]]+ ?)+) +[[:digit:]]+.*', '\\1', rowString))
    }
    # the last one in a line, with white space following
    if (grepl('.* ([[:digit:]]+  ([[:alpha:]]+ ?)+) +$', rowString)){
      outData = c(outData, gsub('.* ([[:digit:]]+  ([[:alpha:]]+ ?)+) +$', '\\1', rowString))
    }
    # the last one in a line with no white space following
    if (grepl('.* ([[:digit:]]+  ([[:alpha:]]+ ?)+)$', rowString)){
      outData = c(outData, gsub('.* ([[:digit:]]+  ([[:alpha:]]+ ?)+)$', '\\1', rowString))
    }
  }
  outData = ifelse(grepl(' $', outData), gsub('(.*) $', '\\1', outData), outData)
  codes = as.numeric(gsub('([[:digit:]]{3})  (.*)', '\\1', outData))
  print(codes)
  county = as.character(gsub('([[:digit:]]{3})  (.*)', '\\2', outData))
  print(county)
  stateTables[[state]] = data.frame(state = rep(state, length(codes)), 
                                    prefix = rep(statePrefixes$fips.prefix[counter], length(codes)), 
                                    suffix = codes, county)
  counter = counter + 1
}

out = NULL
for (state in 1:length(stateTables)){
  
  data = stateTables[[state]]
  statePrefix = unique(data$prefix)
  if (length(statePrefix)>1){print('state prefix error')}  
  if (statePrefix < 10){
    statePrefix = paste('0', statePrefix, sep = '')
  }
  for (county in 1:nrow(data)){
    countySuffix = data$suffix[county]
    if (countySuffix < 10){
      countySuffix = paste('00', countySuffix, sep = '')
    } else if (countySuffix < 100){
      countySuffix = paste('0', countySuffix, sep = '')
    }
    countyFips = paste(statePrefix, countySuffix, sep = '')
    row = c(code = countyFips, county = as.character(data$county)[county], state = names(stateTables)[[state]])
    out = rbind(out, row)
  }
}
out = out[!duplicated(out[,1]),]
fipsTable = data.frame(code = out[,1], county = out[,2], state = out[,3])
write.table(fipsTable, file = 'county fips codes.txt', row.names = F, sep = ',')
begin = 1985; end = 2002; k = 3
prod.column.name = 'productivity'
temp = k.wk.beg.and.end.avgs(dat, prod.column.name, 3, begin, end)
mem.dat = k.wk.beg.and.end.avgs(dat, 'mem', 3, begin, end)
y.axis = 100*(temp[,2]-temp[,1])/temp[,1]
x.axis = -(mem.dat[,2] - mem.dat[,1])
xy.fit.line(x.axis, y.axis, text = T, xtext = 'decrease in union share',
            ytext = '% increase in productivity',
            maintext = paste(begin, '-', end, '; ', k, ' year avgs', sep = ''),
            sub = T)
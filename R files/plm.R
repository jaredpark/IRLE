x1 = seq(1,15, length.out = 99)
x2 = c(rep(1, 33), rep(2, 33), rep(3, 33))
y = 4*x1 + 10*x2 + rnorm(99, 0, .1)

library('lmtest')
lm(y~x1+as.factor(x2))
coeftest(lm(y~x1+as.factor(x2)))

data = data.frame('x2' = as.factor(x2), x1, y)

library('plm')
plm(y~x1, data = data, model = 'within', effects = 'individual')

library('datasets')
library(help = 'datasets')

gcenter = function(df1,group) {
  variables = paste(rep("C", ncol(df1))
                    , colnames(df1), sep="."
                    )
  copydf = df1
  for (i in 1:ncol(df1)) {
    copydf[,i] <- df1[,i] - ave(df1[,i], group,FUN=mean)
  }
  colnames(copydf) = variables
  return(cbind(df1,copydf))
}

c.data = gcenter(data, x2)
View(c.data)

lm(C.y~C.x1, data = c.data)
coeftest(lm(C.y~C.x1, data = c.data))
cons_pre = c(1.09, 0.50, 0.10, 2.33)
cons_post = c(0.73, 0.40, -0.23, 1.85)
beta_pre = c(-0.29, 0.11, -0.48, -0.03)
beta_post = c(-0.23, 0.11, -0.47, 0.03)

okun = data.frame(cons_pre, cons_post, beta_pre, beta_post)
rownames(okun) = c("mu", "SE","a","b")
okun = okun[1:2,]
okun = as.data.frame(t(okun))

points = seq(0,.5, length.out = 10)
plot(points, okun$mu[1]+points*okun$mu[3], ylim = c(.6,1.7))
#arrows(x0 = points, y0 = okun$mu[1]+points*okun$mu[3]-okun$SE[1], y1 = okun$mu[1]+points*okun$mu[3]+okun$SE[1], code = 3, angle = 90, col = rgb(.5, 0, 0, alpha = .3), length = .05)

library("MASS")
#cov = corr*sigmax*sigmay = corr*SEx*SEy ; used a guess of corr = -0.6
covariance = matrix(c(okun$SE[1], -.12, -.12, okun$SE[3]), nrow = 2)
N = 10000
sims = mvrnorm(N, mu = okun$mu[c(1,3)], Sigma = covariance)
sims = as.data.frame(sims)
sims[,3] = as.numeric(-sims[,1]/sims[,2])
colnames(sims) = c("cons_pre", "beta_pre", "ratio_pre")
hist(sims[,3])

mean(sims[sims[,2]<(-.1), 3])
sqrt(var(sims[sims[,2]<(-.1), 3]))
hist(sims[sims[,2]<(-.1), 3])

mean(sims[sims[,2]<(-.1) & sims[,1] < 2, 3])
sqrt(var(sims[sims[,2]<(-.1) & sims[,1] < 2, 3]))
hist(sims[sims[,2]<(-.1) & sims[,1] < 2, 3])

sims[which(sims[,3]==max(sims[sims[,2]<(-.1),3])),]
sims[which(sims[,3]==min(sims[,3])),]
sims = sims[-(c(which(sims[,3]==min(sims[,3])),
                 which(sims[,3]==max(sims[,3]))))
                 , ]
nrow(sims)
hist(sims[,3])


boots = function(n, mean, SE){
  r = rnorm(n, mean, SE)
  print(mean(r))
  print(sqrt(var(r)))
}

attach(okun)

N = 1000
okun.trial = matrix(rep(NA, 8), )
for (i in 1:nrow(okun)){
  boots(N, mu[i], SE[i])
}
boots(1000, 0, 1)
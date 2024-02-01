library(rstan)
maxSamples = 1000
numSpecies = 1000
alphaDispersion = 0.1
samp_data <- rmultinom(1,maxSamples,prob=rep(1/numSpecies, numSpecies))

data=list(K=numSpecies, alpha=alphaDispersion, N=maxSamples)
fit1 <- stan("dirichletsamp.stan",
             data=data,
             algorithm = "Fixed_param")
checksamp = 2
theta <- extract(fit1)$theta[checksamp,]
y <- extract(fit1)$y[checksamp,]
plot(sort(theta))
abline(h=1/1000)
plot(sort(y))
points(sort(samp_data), col='red')

## Posterior predictive check
which.max(extract(fit1)$theta[1,])
which.max(extract(fit1)$y[1,])

plot(samp_data)
plot(extract(fit1)$y[2,])

theta <- extract(fit1)$theta
ds=1:5
boxplot(theta[,1:100])
points(x=seq(ds), prob[ds], col='red')
tab_val = 5

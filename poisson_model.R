library(rstan)
set.seed(1)
x = rpois(100, 50)
N<- length(x)
data=list(x=x, N=N)

fitpois <-
  stan(file="poisson.stan",
       data=data)

hist(rstan::extract(fitpois)$y)
hist(x)


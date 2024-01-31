library(rstan)
data=list(K=10, alpha=10)
fit1 <- stan("dirichletsamp.stan",
             data=data,
             algorithm = "Fixed_param")
extract(fit1)$theta[1:5,]

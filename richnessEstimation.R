# libraries
library(tidyverse)
library(gtools)
library(shinystan)
library(VGAM)
library(rstan)

# Two possible states
set.seed(4)
N <- c(10,100) # Number of Species
samp <- 1000
dirRand1<- rdirichlet(1, rep(1,N[1]))
sample1 <- rmultinom(1, samp, prob=dirRand1)
dirRand2<- rdirichlet(1, rep(1,N[2]))
sample2 <- rmultinom(1, samp, prob=dirRand2)
par(mfrow=c(1,2))
hist(sample1)
hist(sample2)

data_nonzero_s1 <- sample1[sample1 > 0]
data_nonzero_s2 <- sample2[sample2 > 0]
K1=N[1]
K2=N[2]

fit2 <- stan("richnessEstimator.stan",
                    data = list("detected" = length(data_nonzero_s2),
                                "nout" = K2,
                                "pop" = data_nonzero_s2,
                                "alpha" = rep(0.01,K2)),
                    chains=2,
                    warmup = 500,
                    iter = 1000,
                    thin = 2,
                    algorithm = "NUTS",
                    cores = 2,
                    verbose = T)

theta_sampled <- extract(fit1)$theta
theta_sampled2 <- extract(fit2)$theta

plotParam <- function(param_sampled, param_real) {
  par(mfrow=c(1,1))
  plot(1:length(param_sampled), param_sampled, xlab="Parameter", ylab="Density")
  points(1:length(param_real), param_real, pch=19, col='red')
}
plotParam(theta_sampled[3,], dirRand1)
plotParam(theta_sampled2[3,], dirRand2)

postPredict <- function() {

}
points(t(dirRand1),col='red')

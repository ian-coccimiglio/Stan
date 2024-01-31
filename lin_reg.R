# Schools teach students using a new methodology, where each school designed their own.
# This method had varying levels of effectiveness
# There is a within school variation as well
library(tidyverse)
library(tidymodels)
library(rstan)
library(MASS)
set.seed(2)
beta = 5 # relationship between x and y
mu = 10
sigma = 15 # the random variation in beta
x = seq(0, 50, by=1)
N = length(x)
y <- rnorm(N, mu+beta*x, sigma)
plot(x, y)

data_1 <- list(
  N = N,
  y = y,
  x = x
)
fit1 <- stan(
  file = "lin_reg.stan",  # Stan program
  data = data_1,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 4000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
)

betas = extract(fit1)$beta
mus = extract(fit1)$mu
sigmas = extract(fit1)$sigma
y_rep = extract(fit1)$y_rep
plot(x, extract(fit1)$y_rep[2,])
summary(lm(y~x))

length(y_rep)
low <- 1:ncol(y_rep) %>% map(\(x) quantile(y_rep[,x], 0.05)) %>% unlist
high <- 1:ncol(y_rep) %>% map(\(x) quantile(y_rep[,x], 0.95)) %>% unlist

betas_iqr <- betas[which((betas > quantile(betas, 0.25)) & (betas < quantile(betas, 0.75)))]
mus_iqr <- mus[which((mus > quantile(mus, 0.25)) & (mus < quantile(mus, 0.75)))]

plot(x, mean(mus)+mean(betas)*x,
     type='l',
     ylim=(c(0,250)))
for (n in seq(1, length(betas))) {
  lines(x, mus_iqr[n]+(x*betas_iqr[n]), col=rgb(0.85, 0.85, 0.85, alpha=0.5))
}
lines(x, mean(mus)+mean(betas)*x, type='l')
points(x,y)

contour2d <- function(x, y, ...){
  DENS = kde2d(x, y)
  filled.contour(DENS,plot.axes = {
    axis(1)
    axis(2)
    contour(DENS,add = TRUE)}, ...)i
}
contour2d(mus, betas, xlab="mus", ylab="betas")
contour2d(mus, sigmas, xlab="mus", ylab="betas")

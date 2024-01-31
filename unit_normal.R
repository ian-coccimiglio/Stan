library(rstan)
source("vis_functions.R")
schools_data <- list(
  J = 8,
  y = c(28,  8, -3,  7, -1,  1, 18, 12),
  sigma = c(15, 10, 16, 11,  9, 11, 10, 18)
)

N = 10
dat = rnorm(N, 15, 4)
new_data <- list(
  J = N,
  y = dat
)

fit1 <- stan(
  file = "unit_normal.stan",  # Stan program
  data = new_data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 1             # no progress shown
)
x = seq(0,80, by=0.1)
plot(x, dnorm(x, 45, 1))
den_mu <- density(extract(fit1)$mu)
points(x, dnorm(x, mean(new_data$y)),col='red')
den_pred <- density(extract(fit1)$y_sim)
points(den_pred$x, den_pred$y, col='blue')
boxplot(new_data$y, extract(fit1)$y_sim[4,])

plot(extract(fit1)$sigma, extract(fit1)$mu)
contour2d(extract(fit1)$sigma, extract(fit1)$mu, xlab="Sigma", ylab="mu")

# A simple dirichlet-multinomial model to find alpha values
library(tidyverse)
library(tidymodels)
library(rstan)
set.seed(2)
zf <- dzipf(seq(1,50), 50, 1)
dat <- rmultinom(1, 50, zf)

dirmult_data <- list(
  N = length(dat),
  y = as.numeric(dat)
)
mod <- cmdstan_model(stan_file="dirmult.stan")

fit1 <- stan(
  file = "dirmult.stan",  # Stan program
  data = dirmult_data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 4000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
)
plot(fit1)
alph <- extract(fit1)$alpha
N_post <- length(alph[,1])
#seq(1, N_post) %>% map()

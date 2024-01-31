# Schools teach students using a new methodology, where each school designed their own.
# This method had varying levels of effectiveness
# There is a within school variation as well
library(tidyverse)
library(tidymodels)
library(rstan)
set.seed(2)
alpha = 5 # equivalent to the generating number at the top of the hierarchy
sigma = 3 # the random variation in the coaching effect
eta = 5  # the random variation between schools
numSchools = 100
theta = rnorm(numSchools, alpha, sigma) # the generated variation
random_schools = seq(1,numSchools) %>% map(\(x) rnorm(1, theta[x], eta))
boxplot(random_schools)
dat <- random_schools %>% unlist

schools_data_2 <- list(
  J = numSchools,
  y = dat
)
schools_data_2
fit1 <- stan(
  file = "schools.stan",  # Stan program
  data = schools_data_2,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 4000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
)
plot(fit1)

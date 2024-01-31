file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
mod <- cmdstan_model(file)

mod.beta ="data {
  int<lower=0> N;
  array[N] int<lower=0,upper=1> y;
}
parameters {
  real<lower=0,upper=1> theta;
  real<lower=0,upper=1> beta;
}
model {
  theta ~ beta(0.5,beta);  // uniform prior on interval 0,1
  y ~ bernoulli(theta);
}"
write_stan_file(mod.beta, dir=getwd(), basename="mod_beta")
mod_beta <- cmdstan_model(stan_file = "mod_beta.stan")

data = rep(c(0,1,0,0,0,0,0,1,0,1),20)
N = length(data)
data_list <- list(N = N, y = data)

fit <- mod_beta$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)

fit$summary()
mcmc_hist(fit$draws())

mod$optimize(data=data_list, seed=123)$print()
#plot(dbinom(seq(0,10), 10, 0.2), type='l')

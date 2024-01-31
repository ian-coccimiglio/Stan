//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data {
  int<lower=0> J;
  real y[J];
}

parameters {
  real mu;
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  mu ~ normal(45, 10);
  y ~ normal(mu, sigma);
}

generated quantities {
  // Simulate new data based on the estimated posterior distribution
  vector[J] y_sim;
  for (i in 1:J) {
    y_sim[i] = normal_rng(mu, sigma);
  }
}

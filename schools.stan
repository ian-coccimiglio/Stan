data {
  int<lower=0> J;          // number of schools
  real y[J];               // estimated treatment effects
}

parameters {
  real alpha;
  real<lower=0> sigma;
}

model {
  y ~ normal(alpha, sigma);
}
/*
generated quantities {
  real y_rep[J, 1];
  for (n in 1:J)
    y_rep[n] = normal_rng(theta, sigma);
}*/

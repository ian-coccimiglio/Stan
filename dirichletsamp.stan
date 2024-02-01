data {
  int<lower=1> K;
  real<lower=0> alpha;
  int<lower=0> N;
}

generated quantities {
  array[K] int<lower=0> y;
  vector[K] theta = dirichlet_rng(rep_vector(alpha, K));
  y = multinomial_rng(theta, N);
}

data {
  int<lower=1> K;
  real<lower=0> alpha;
}

generated quantities {
  vector[K] theta = dirichlet_rng(rep_vector(alpha, K));
}

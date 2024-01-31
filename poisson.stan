
data {
  int<lower=0> N;
  int x[N];
}

parameters {
  real lambda;
}

model {
  x ~ poisson(lambda);
}

generated quantities {
  real y;
  y = poisson_rng(lambda);
}

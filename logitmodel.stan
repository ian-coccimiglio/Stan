data {
  int<lower=1> N;
  array[N] int<lower=0, upper=1> y;
  vector[N] x;
}
parameters {
  real alpha;
  real beta;
}
model {
  y ~ bernoulli_logit(alpha + beta * x);
}

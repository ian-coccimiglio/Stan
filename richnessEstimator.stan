//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
// The input data is a vector of positive integers pop of length nout
data {
  int<lower=0> detected;
  int<lower=0> K;
  int<lower=1> upper;
  int<lower=0> pop[detected];
  vector[K] alpha;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  simplex[nout] theta;
}

transformed parameters {
  vector[upper] lp;
  lp = rep_vector(log_unif, upper);
  for (K in 1:upper) {
    lp[K] = lp[K] + multinomial_lpmf(pop | theta);
  }
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  theta ~ dirichlet(alpha);
  pop ~ multinomial(theta);
}


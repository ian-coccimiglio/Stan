data {
  real<lower=0> r_e; // early rate
  real<lower=0> r_l; // late rate
  int<lower=1> T; // T is total
  array[T] int<lower=0> D; // Disaster
}
transformed data {
  real log_unif; // uniform log
  log_unif = -log(T); // uniform log
}
parameters {
  real<lower=0> e; // early rate
  real<lower=0> l; // later rate
}
// transformed parameters {
//   vector[T] lp; // log posterior
//   lp = rep_vector(log_unif, T); // a vector of uniform dates
//   for (s in 1:T) {
//     for (t in 1:T) {
//       lp[s] = lp[s] + poisson_lpmf(D[t] | t < s ? e : l); // calculates a
//     }
//   }
// }
transformed parameters {
    vector[T] lp;
    {
      vector[T + 1] lp_e;
      vector[T + 1] lp_l;
      lp_e[1] = 0;
      lp_l[1] = 0;
      for (t in 1:T) {
        print(D);
        lp_e[t + 1] = lp_e[t] + poisson_lpmf(D[t] | e);
        lp_l[t + 1] = lp_l[t] + poisson_lpmf(D[t] | l);
      }
      lp = rep_vector(log_unif + lp_l[T + 1], T)
           + head(lp_e, T) - head(lp_l, T);
    }
  }
model {
  e ~ exponential(r_e);
  l ~ exponential(r_l);
  target += log_sum_exp(lp);
}

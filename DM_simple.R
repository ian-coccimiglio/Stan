library(gtools)
library(rstan)
library(shinystan)
library(VGAM)
rstan_options(auto_write=TRUE)

# A dirichlet-multinomial model, as specified in https://onlinelibrary.wiley.com/doi/10.1111/1755-0998.13128
notus <- 50
nsamples <- 5000
nreps <- 50
comprop <- matrix(0, ncol = notus, nrow = 1)
indprop <- matrix(0, ncol = notus, nrow = nreps)
comprop
comprop[1, ] <- rdirichlet(1, c(rep(15, 5), rep(1, notus - 5)))
com <- matrix(0, ncol = notus, nrow = nreps)
com
for (i in 1:nreps) {
  indprop[i, ] <- rdirichlet(1, comprop[1, ])
  com[i, ] <- rmultinom(1, nsamples, prob = indprop[i, ])
}
com <- com + 1
nsamples <- nsamples + 50

DM <- stan_model("DM_simple.stan", model_name = "DM_simple")

ptm <- proc.time()
fitstan_HMC <- sampling(DM,
                        data = list("datamatrix" = com,
                                    "nreps" = nrow(com),
                                    "notus" = ncol(com),
                                    "start" = 1,
                                    "end" = nreps),
                        chains=2,
                        warmup = 500,
                        iter = 1000,
                        thin = 2,
                        algorithm = "NUTS",
                        cores = 1,
                        pars <- c("pi", "theta", "p"),
                        verbose = T)
hmcTime <- c(proc.time() - ptm)[3]

est.pi <- extract(fitstan_HMC,"pi")
est.pi

calc_certain_diffs <- function(mcmc_of_diffs, dimension){
  positives <- vector()
  negatives <- vector()
  for(i in 1:dim(mcmc_of_diffs)[dimension]){
    if(dimension == 2){
      perc <- length(which(mcmc_of_diffs[,i] > 0 ))/ length(mcmc_of_diffs[,i])
    }else{
      perc <- length(which(mcmc_of_diffs[i,] > 0)) / length(mcmc_of_diffs[i,])
    }
    if(perc >= 0.95 | perc <= 0.05){
      positives <- c(positives, i)
    }else{
      negatives <- c(negatives, i)
    }
  }
  return(list(positives = positives,
              negatives = negatives))
}
#diffs_HMC <- est.pi$pi[,1]
#outHMC <- calc_certain_diffs(diffs_HMC,2)
#outHMC


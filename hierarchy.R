library(gtools)
library(rstan)
library(shinystan)
library(VGAM)
library(ProbBayes)

df = olympic_butterfly[!is.na(olympic_butterfly$Time),]
df =df[!df$Year == 1964,]
olympic_butterfly$Year
olympic_butterfly$Gender
olympic_butterfly$Time
Years <- factor(df$Year)
Nlevels <- length(levels(Years))
rstan_options(auto_write=TRUE)

data = list(
  D = 1,
  N = length(df$Gender),
  L = Nlevels,
  y = ifelse(df$Gender == "Men", 0, 1),
  ll = rep(seq(13,1),2),
  x = as.matrix(df$Time),
)

fitstan_HMC <- stan(file = "stan_hierarchy.stan",
                        data = data,
                        chains=2,
                        warmup = 500,
                        iter = 1000,
                        thin = 2,
                        cores = 2,
                        verbose = T)

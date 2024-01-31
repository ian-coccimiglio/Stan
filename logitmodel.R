library(gtools)
library(rstan)
library(shinystan)
library(VGAM)
library(ProbBayes)
source("vis_functions.R")

df = olympic_butterfly[!is.na(olympic_butterfly$Time),]
df =df[!df$Year == 1964,]
rstan_options(auto_write=TRUE)

data = list(
  N = length(df$Gender),
  y = ifelse(df$Gender == "Men", 0, 1),
  x = df$Time
)

fit1 <- stan(file = "logitmodel.stan",
                    data = data,
                    chains=2,
                    warmup = 500,
                    iter = 1000,
                    thin = 2,
                    cores = 2,
                    verbose = T)
alphas = extract(fit1)$alpha
betas = extract(fit1)$beta
plot(mean(alphas) + mean(betas)*df$Time, data$y)
points(df$Time, data$y)

library(tidyverse)
alph <- alphas[alphas > quantile(alphas, 0.25) & alphas < quantile(alphas, 0.75)]
bet <- betas[betas > quantile(betas, 0.25) & betas < quantile(betas, 0.75)]

plot(df$Time, inv.logit(alphas[1]+betas[1]*df$Time))
points(df$Time, inv.logit(alph[20]+bet[20]*df$Time))
seq(1, length(alph)) %>% map(\(x) inv.logit(alph[x]+(bet[x]*df$Time))) %>% map(\(y) lines(df$Time, y))

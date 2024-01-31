library(boot)
library(tidyverse)
library(VGAM)
library(rstan)
library(MASS)

plot(density(coal$date, bw = 3))
disaster <- table(as.integer(coal$date))
# dates = sort(unique(as.integer(coal$date)))
dates = seq(min(as.integer(coal$date)), max(as.integer(coal$date)))
ncoal = length(seq(min(coal$date), max(coal$date)))
disaster_f <- table(factor(as.integer(coal$date), levels=dates))
dates
disaster_f
data = list(
  T = ncoal,
  D = disaster_f,
  r_e = 1,
  r_l = 1
)
fit1<- stan("coal_miner.stan",
     data=data,
     chains=1,
     cores=4,
     warmup=1500,
     iter=3000,
     verbose = T)

swp <- extract(fit1)$lp
plot(dates, swp[1,])
seq(1:length(dates)) %>% map(\(x) lines(dates, extract(fit1)$lp[x,]))
# lines(sort(unique(as.integer(coal$date))), extract(fit1)$lp[3,])

llow <- 1:ncol(swp) %>% map(\(x) quantile(swp[,x], 0.25))
lmean <- 1:ncol(swp) %>% map(\(x) quantile(swp[,x], 0.5)) %>% unlist
lhigh <- 1:ncol(swp) %>% map(\(x) quantile(swp[,x], 0.75)) %>% unlist
plot(dates, lmean)
lines(dates, llow)
lines(dates, lhigh)

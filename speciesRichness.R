library(gtools)
library(tidyverse)
library(rstan)
library(shinystan)
library(VGAM)
rstan_options(auto_write=TRUE)

# A dirichlet-multinomial model, as specified in https://onlinelibrary.wiley.com/doi/10.1111/1755-0998.13128
set.seed(2)
lambda = 50
val <- rpois(1, lambda) # val number of species
draws <- 10*val # draws number of draws
prob=dzipf(seq(1,val),val,0.1) # zipf prob
prob=rep(1/val, val) # uniform prob
dat <- sort(rmultinom(1,draws, prob=prob), decreasing = TRUE)
overZero <- dat %>% subset(dat > 0)
hist(overZero)
K = length(dat)
new_dat <- dat+1
repl_unif = rep(1000, 43)
fit1 <- stan("speciesRichness.stan",
             data = list(pop=as.vector(dat),
                         K=K,
                         alpha=as.vector(repl_unif)),
             chains=2,
             warmup = 2000,
             iter = 4000,
             algorithm = "NUTS",
             cores = 2,
             verbose = T)
theta <- extract(fit1)$theta
ds=1:43
boxplot(theta[,1])
points(x=seq(ds), prob[ds], col='red')
tab_val = 5
gen_data <- seq(1, 4000) %>%
  map(\(x) rmultinom(1, draws, theta[x,])) %>%
  map(table) %>%
  map(`[`, tab_val) %>%
  unlist
hist(gen_data)
abline(v=table(dat)[[tab_val]])
table(dat)
# table(rmultinom(1, 1959, gen_data))

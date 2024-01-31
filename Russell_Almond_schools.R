library(rstan)

schools_data <- list(
  J = 8,
  y = c(28,  8, -3,  7, -1,  1, 18, 12),
  sigma = c(15, 10, 16, 11,  9, 11, 10, 18)
)

fit1 <- stan(
  file = "schools.stan",  # Stan program
  data = schools_data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 1,              # number of cores (could use one per chain)
  refresh = 0             # no progress shown
)

print(fit1, pars=c("theta", "mu", "tau", "eta", "lp__"), probs=c(.1,.5,.9))

# weighted?
sum(schools_data$y*schools_data$sigma)/sum(schools_data$sigma)

weight = 1/schools_data$sigma^2
m = sum(weight*schools_data$y)/sum(weight)

ord <- order(schools_data$y)
plot(schools_data$y[ord[c(1,8)]]+c(-2,2)*schools_data$sigma[ord[c(1,8)]],
     c(length(schools_data$y),1),main = "8 Schools data.",type="n",yaxt="n",
     xlab="Effect Size",ylab="School")
points(schools_data$y[ord],length(schools_data$y):1,pch=rownames(schools_data$y)[ord])
segments(schools_data$y[ord]-2*schools_data$sigma[ord],schools_data$J:1,
         schools_data$y[ord]+2*schools_data$sigma[ord],schools_data$J:1)
abline(v=m,col="blue")


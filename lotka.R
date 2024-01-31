library(rstan)
library(bayesplot)
library(tidyverse)
file = "https://raw.githubusercontent.com/stan-dev/example-models/master/knitr/lotka-volterra/hudson-bay-lynx-hare.csv"
# https://github.com/bblais/Systems-Modeling-Spring-2015-Notebooks/blob/master/data/Lynx%20and%20Hare%20Data/lynxhare.csv
lynx_hare_df = read.csv(file, header=TRUE, comment.char="#")
# lynx_hare_df
# N = length(lynx_hare_df$Year) - 1
ts = 1:N
y_init = c(lynx_hare_df$Hare[1], lynx_hare_df$Lynx[1])
y = as.matrix(lynx_hare_df[2:(N + 1), 2:3])
y = cbind(y[ , 2], y[ , 1]); # hare, lynx order
Nproj = 70
seq_vector = seq(1, 70)
lynx_hare_data = list(N=N, ts=ts, y_init=y_init, y=y, Nproj=Nproj, seq_vector=seq_vector)
thething = "mod_lotka.stan"
fit = stan(file=thething, data=lynx_hare_data, iter=5000, chains=3, cores=3)
fit
print(fit, pars=c("theta", "sigma", "z_init"),
      probs=c(0.1, 0.5, 0.9), digits = 3)

plot(summary(fit))
y_rep <- extract(fit)$y_rep
plot(y_rep[3,,2])
?plot
plot(y_rep[1,,1], y_rep[1,,2], type='l',xlim=c(0,180), ylim=c(0,100))
for (n in seq(2,100)) {
  points(y_rep[n,,1], y_rep[n,,2], type='l', col='blue')
}


plot(apply(y_rep[1:5,,1], 2, mean), col="red", type='l', ylim=c(0,80))
points(lynx_hare_df$Hare[2:20], col="blue")
points(apply(y_rep[1:5,,2], 2, mean), col="red", type='l')
points(lynx_hare_df$Lynx[2:20], col="green")


z_init_draws <- extract(fit)$z_init
z_draws <- extract(fit)$z
y_init_rep_draws <- extract(fit)$y_init_rep
y_rep_draws <- extract(fit)$y_rep
predicted_pelts <- matrix(NA, 70, 2)
min_pelts <- matrix(NA, 70, 2)
max_pelts <- matrix(NA, 70, 2)
for (k in 1:2) {
  predicted_pelts[1, k] <- mean(y_init_rep_draws[ , k])
  min_pelts[1, k] <- quantile(y_init_rep_draws[ , k], 0.25)
  max_pelts[1, k] <- quantile(y_init_rep_draws[ , k], 0.75)
  for (n in 2:70) {
    predicted_pelts[n, k] <- mean(y_rep_draws[ , n - 1, k])
    min_pelts[n, k] <- quantile(y_rep_draws[ , n - 1, k], 0.25)
    max_pelts[n, k] <- quantile(y_rep_draws[ , n - 1, k], 0.75)
  }
}
lynx_hare_melted_df <- melt(as.matrix(lynx_hare_df[, 2:3]))
colnames(lynx_hare_melted_df) <- c("year", "species", "pelts")
lynx_hare_melted_df$year <-
  lynx_hare_melted_df$year +
  rep(1899, length(lynx_hare_melted_df$year))

Nmelt <- dim(lynx_hare_melted_df)[1]
lynx_hare_observe_df <- lynx_hare_melted_df
lynx_hare_observe_df$source <- rep("measurement", Nmelt)

lynx_hare_predict_df <-
  data.frame(year = rep(1900:1969, 2),
             species = c(rep("Lynx", 70), rep("Hare", 70)),
             pelts = c(predicted_pelts[, 2],
                       predicted_pelts[, 1]),
             min_pelts = c(min_pelts[, 2], min_pelts[, 1]),
             max_pelts = c(max_pelts[, 2], max_pelts[, 1]),
             source = rep("prediction", 140))

lynx_hare_observe_df$min_pelts = lynx_hare_predict_df$min_pelts
lynx_hare_observe_df$max_pelts = lynx_hare_predict_df$max_pelts

lynx_hare_observe_predict_df <-
  rbind(lynx_hare_observe_df, lynx_hare_predict_df)

population_plot2 <-
  ggplot(data = lynx_hare_observe_predict_df,
         aes(x = year, y = pelts, color = source)) +
  geom_vline(xintercept = 1900, color = "grey") +
  geom_hline(yintercept = 0, color = "grey") +
  facet_wrap( ~ species, ncol = 1) +
  geom_ribbon(aes(ymin = min_pelts, ymax = max_pelts),
              colour = NA, fill = "black", alpha = 0.1) +
  geom_line() +
  geom_point() +
  ylab("pelts (thousands)") +
  theme(legend.position="bottom")

population_plot2

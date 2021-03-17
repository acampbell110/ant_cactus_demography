setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
#### Growth Visuals
## Overlay Plots
y <- y_grow
yrep_grow <- rstan::extract(fit_grow_mix_ant, pars = "y_rep")[["y_rep"]]
samp100 <- sample(nrow(yrep_grow), 500)
png(file = "grow_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_grow[samp100,])
dev.off()
png(file = "grow_ant_post1.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_grow[samp100,], x = vol_data, group = ant_data)
dev.off()
## Convergence Plots
pdf("grow_conv2.pdf")
ggtitle("Growth Model Parameter Convergence")
bayesplot::mcmc_trace(As.mcmc.list(fit_grow_mix_ant, pars=c("beta0", "beta1")))
title()dev.off()
## Panels
png(file = "grow_param_dist.png")
par(mfrow = c(2,3))
#other
stan_plot(fit_grow_mix_ant, pars = c("beta0[1]", "beta1[1]"), point_est = "mean", show_density = TRUE, fill_color = "darkgrey", alpha = 0.5)
#crem
stan_plot(fit_grow_mix_ant, pars = c("beta0[2]", "beta1[2]"), point_est = "mean", show_density = TRUE, fill_color = "red")
#liom
stan_plot(fit_grow_mix_ant, pars = c("beta0[3]", "beta1[3]"), point_est = "mean", show_density = TRUE, fill_color = "blue")
#vacant
stan_plot(fit_grow_mix_ant, pars = c("beta0[4]", "beta1[4]"), point_est = "mean", show_density = TRUE, fill_color = "pink")
#intercepts
stan_plot(fit_grow_mix_ant, pars = c("beta0[1]", "beta0[2]","beta0[3]","beta0[4]"), 
          point_est = "mean", show_density = TRUE, 
          fill_color = c("darkgrey","red","blue","pink"))
#slopes
stan_plot(fit_grow_mix_ant, pars = c("beta1[1]", "beta1[2]","beta1[3]","beta1[4]"), 
          point_est = "mean", show_density = TRUE, 
          fill_color = c("darkgrey","red","blue","pink"))
dev.off()
## Ribbons
bayesplot::ppc_ribbon_grouped(y, yrep_grow, group = ant_data, prob = 0.5, prob_outer = 0.95)
## Panel Plots
extract_grow <- rstan::extract(fit_grow_mix_ant, pars = c("beta0","beta1","mu"))
beta0 <- as.data.frame(extract_grow$beta0)
beta1 <- as.data.frame(extract_grow$beta1)
grow_extract <- cbind(beta0,beta1)
colnames(grow_extract) <- c("Beta0_1","Beta0_2","Beta0_3","Beta0_4", "Beta1_1","Beta1_2","Beta1_3","Beta1_4")
for(i in 1:15000){
  grow_extract$pred_1 <- grow_extract$Beta0_1 + grow_extract$Beta1_1
  grow_extract$pred_2 <- grow_extract$Beta0_2 + grow_extract$Beta1_2
  grow_extract$pred_3 <- grow_extract$Beta0_3 + grow_extract$Beta1_3
  grow_extract$pred_4 <- grow_extract$Beta0_4 + grow_extract$Beta1_4
}
#Intercepts
grow_beta0_1_mean <- mean(grow_extract$Beta0_1)
grow_beta0_2_mean <- mean(grow_extract$Beta0_2)
grow_beta0_3_mean <- mean(grow_extract$Beta0_3)
grow_beta0_4_mean <- mean(grow_extract$Beta0_4)
#Slopes
grow_beta1_1_mean <- mean(grow_extract$Beta0_1)
grow_beta1_2_mean <- mean(grow_extract$Beta0_2)
grow_beta1_3_mean <- mean(grow_extract$Beta0_3)
grow_beta1_4_mean <- mean(grow_extract$Beta0_4)
summary(grow_extract)
png("Figures/grow_panel.png")
par(mfrow = c(2,3))
# Other
plot(x = log(data$volume_t)  ,y = grow_beta0_1_mean + log(data$volume_t) * grow_beta1_1_mean, type = "l", col = "black")
lines(x = log(data$volume_t), y = quantile(grow_extract$Beta0_1,0.75) + log(data$volume_t) * quantile(grow_extract$Beta1_1,0.75), type = "l", col = "darkgrey", lty = 2)
lines(x = log(data$volume_t), y = quantile(grow_extract$Beta0_1,0.25) + log(data$volume_t) * quantile(grow_extract$Beta1_1,0.25), type = "l", col = "darkgrey", lty = 2)
# Crem
plot(x = log(data$volume_t)  ,y = grow_beta0_2_mean + log(data$volume_t) * grow_beta1_2_mean, type = "l", col  = "red")
lines(x = log(data$volume_t), y = quantile(grow_extract$Beta0_2,0.75) + log(data$volume_t) * quantile(grow_extract$Beta1_2,0.75), type = "l", col = "darkgrey", lty = 2)
lines(x = log(data$volume_t), y = quantile(grow_extract$Beta0_2,0.25) + log(data$volume_t) * quantile(grow_extract$Beta1_2,0.25), type = "l", col = "darkgrey", lty = 2)
# Liom
plot(x = log(data$volume_t)  ,y = grow_beta0_3_mean + log(data$volume_t) * grow_beta1_3_mean, type = "l", col = "blue")
lines(x = log(data$volume_t), y = quantile(grow_extract$Beta0_3,0.75) + log(data$volume_t) * quantile(grow_extract$Beta1_3,0.75), type = "l", col = "darkgrey", lty = 2)
lines(x = log(data$volume_t), y = quantile(grow_extract$Beta0_3,0.25) + log(data$volume_t) * quantile(grow_extract$Beta1_3,0.25), type = "l", col = "darkgrey", lty = 2)
# Vacant
plot(x = log(data$volume_t)  ,y = grow_beta0_4_mean + log(data$volume_t) * grow_beta1_4_mean, type = "l", col = "pink")
lines(x = log(data$volume_t), y = quantile(grow_extract$Beta0_4,0.75) + log(data$volume_t) * quantile(grow_extract$Beta1_4,0.75), type = "l", col = "darkgrey", lty = 2)
lines(x = log(data$volume_t), y = quantile(grow_extract$Beta0_4,0.25) + log(data$volume_t) * quantile(grow_extract$Beta1_4,0.25), type = "l", col = "darkgrey", lty = 2)
# All together
plot(x = log(data$volume_t)  ,y = grow_beta0_1_mean + log(data$volume_t) * grow_beta1_1_mean, type = "l")
lines(x = log(data$volume_t), y = grow_beta0_2_mean + log(data$volume_t) * grow_beta1_2_mean, type = "l", col = "red")
lines(x = log(data$volume_t), y = grow_beta0_3_mean + log(data$volume_t) * grow_beta1_3_mean, type = "l", col = "blue")
lines(x = log(data$volume_t), y = grow_beta0_4_mean + log(data$volume_t) * grow_beta1_4_mean, type = "l", col = "pink")
dev.off()
## Interval Plots
samp10 <- sample(nrow(yrep_grow), 1)
png("Figures/grow_intervals.png")
bayesplot::ppc_intervals_grouped(y = y_grow, yrep = yrep_grow[samp10,], x = vol_data, group = ant_data)
dev.off()

plot(data)


#### Survival Visuals
## Overlay Plots
y <- y_surv
yrep_surv <- rstan::extract(fit_surv_mix_ant, pars = "y_rep")[["y_rep"]]
samp100 <- sample(nrow(yrep_surv), 500)
png(file = "surv_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_surv[samp100,])
dev.off()
png(file = "surv_ant_post1.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_surv[samp100,],group = ant_surv)
dev.off()
## Convergence Plots
png(file = "surv_conv1.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_surv_mix_ant, pars=c("beta0", "beta1")))
dev.off()
## Interval Plots
samp1 <- sample(nrow(yrep1), 2)
png("surv_intervals.png")
bayesplot::ppc_intervals_grouped(y = y_surv, yrep = yrep_surv[samp1,], x = vol_surv, group = ant_surv)
dev.off()
## GGplots
surv_a <- as.data.frame(get_posterior_mean(fit_surv_mix_ant, pars = c("y_rep")))
mean(surv_a$`mean-all chains`)
data$surv_a <- surv_a$`mean-all chains`[1:5231]
ggplot(data = data, aes(x = volume_t, y = surv_a)) + 
  geom_point() +
  geom_smooth()

#### Flowering Visuals
## Overlay Plots
y <- y_flow
yrep_flow <- rstan::extract(fit_flow_mix_ant, pars = "y_rep")[["y_rep"]]
samp100 <- sample(nrow(yrep_flow), 500)
png(file = "flow_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_flow[samp100,])
dev.off()
png(file = "flow_ant_post1.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_flow[samp100,],group = ant_flower)
dev.off()
## Convergence Plots
png(file = "flow_conv1")
bayesplot::mcmc_trace(As.mcmc.list(fit_flow_mix_ant, pars=c("beta0", "beta1")))
dev.off()
## Interval Plots
samp1 <- sample(nrow(yrep_flow), 2)
png("flow_intervals.png")
bayesplot::ppc_intervals_grouped(y = y_flow, yrep = yrep_flow[samp1,], x = vol_flower, group = ant_flower)
dev.off()
## GGplots
flow_a <- as.data.frame(get_posterior_mean(fit_flow_mix_ant, pars = c("y_rep")))
mean(flow_a$`mean-all chains`)
flower$flow_a <- flow_a$`mean-all chains`
ggplot(data = flower, aes(x = volume_t, y = flow_a)) + 
  geom_point() +
  geom_smooth()

#### Reproductive Visuals
## Overlay Plots
y <- y_repro
yrep_repro <- rstan::extract(fit_repro_mix_ant, pars = "y_rep")[["y_rep"]]
samp100 <- sample(nrow(yrep_repro), 500)
png(file = "repro_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_repro[samp100,])
dev.off()
png(file = "repro_ant_post1.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_repro[samp100,],group = ant_flower)
dev.off()
## Convergence Plots
png(file = "repro_conv1.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_repro_mix_ant, pars=c("beta0", "beta1")))
dev.off()
## Interval Plots
samp1 <- sample(nrow(yrep_repro), 2)
png("repro_intervals.png")
bayesplot::ppc_intervals_grouped(y = y_repro, yrep = yrep_repro[samp1,], x = vol_flower, group = ant_flower)
dev.off()
## GGplots
repro_a <- as.data.frame(get_posterior_mean(fit_repro_mix_ant, pars = c("y_rep")))
mean(repro_a$`mean-all chains`)
flower$repro_a <- repro_a$`mean-all chains`
ggplot(data = flower, aes(x = volume_t, y = repro_a)) + 
  geom_point() +
  geom_smooth()

#### Viability Visuals
## Overlay Plots
y <- good
yrep_viab <- rstan::extract(fit_viab_mix_ant, pars = "y_rep")[["y_rep"]]
samp100 <- sample(nrow(yrep_viab), 500)
png(file = "viab_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_viab[samp100,])
dev.off()
png(file = "viab_ant_post1.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_viab[samp100,],group = ant_flower)
dev.off()
## Convergence Plots
png(file = "viab_conv1")
bayesplot::mcmc_trace(As.mcmc.list(fit_viab_mix_ant, pars=c("beta0")))
dev.off()
## Interval Plots
samp1 <- sample(nrow(yrep_viab), 2)
png("viab_intervals.png")
bayesplot::ppc_intervals_grouped(y = good, yrep = yrep_viab[samp1,], x = vol_flower, group = ant_flower)
dev.off()

y <- flower$prop
yrep1 <- rstan::extract(fitty, pars = "y_rep")[["y_rep"]]
samp100 <- sample(nrow(yrep1), 5)
png(file = "viab_post1.png")
bayesplot::ppc_dens_overlay(y, yrep1[samp100,])
dev.off()
## Convergence Plots
png(file = "viab_conv1")
bayesplot::mcmc_trace(As.mcmc.list(fitty, pars=c("beta0")))
dev.off()



#### Multinomial 1
## Overlay Plots
plot_title <- ggtitle("Mixed Ant Effects Multinomial1 Model Simulated Data")
bayesplot::ppc_dens_overlay(y = fit_multi1_mix_ant@mean_y_rep,
                            yrep = posterior_predict(fit_multi1_mix_ant@y_rep, draws = 5)) + plot_title
## Convergence Plots
bayesplot::mcmc_trace(As.mcmc.list(fit_multi1_mix_ant, pars=c("beta0", "beta1")))

#### Multinomial 2
## Overlay Plots
plot_title <- ggtitle("Mixed Ant Effects Multinomial2 Model Simulated Data")
bayesplot::ppc_dens_overlay(y = fit_multi2_mix_ant@mean_y_rep,
                            yrep = posterior_predict(fit_multi2_mix_ant@y_rep, draws = 5)) + plot_title
## Convergence Plots
bayesplot::mcmc_trace(As.mcmc.list(fit_multi2_mix_ant, pars=c("beta0", "beta1")))

#### Multinomial 3
## Overlay Plots
plot_title <- ggtitle("Mixed Ant Effects Multinomial3 Model Simulated Data")
bayesplot::ppc_dens_overlay(y = fit_multi3_mix_ant@mean_y_rep,
                            yrep = posterior_predict(fit_multi3_mix_ant@y_rep, draws = 5)) + plot_title
## Convergence Plots
bayesplot::mcmc_trace(As.mcmc.list(fit_multi3_mix_ant, pars=c("beta0", "beta1")))




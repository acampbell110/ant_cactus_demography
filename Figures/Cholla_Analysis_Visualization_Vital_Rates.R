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
## Interval Plots
samp10 <- sample(nrow(yrep_grow), 1)
png("grow_intervals.png")
bayesplot::ppc_intervals_grouped(y = y_grow, yrep = yrep_grow[samp10,], x = vol_data, group = ant_data)
dev.off()
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
## ggplots
rstanarm::posterior_traj(fit_grow_mix_ant, interpolate = TRUE, extrapolate = TRUE)
a <- as.data.frame(get_posterior_mean(fit_grow_mix_ant, pars = c("y_rep")))
mean(a$`mean-all chains`)
data$a <- a$`mean-all chains`
ggplot(data = data, aes(x = volume_t, y = a)) + 
  geom_point() +
  geom_smooth( )


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




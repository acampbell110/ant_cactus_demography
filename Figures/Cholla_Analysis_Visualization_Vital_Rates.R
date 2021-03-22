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
## Parameter Spreads
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
## Panel Plots
extract_grow <- rstan::extract(fit_grow_mix_ant, pars = c("beta0","beta1"))
beta0 <- as.data.frame(extract_grow$beta0)
beta1 <- as.data.frame(extract_grow$beta1)
mu <- as.data.frame(extract_grow$mu)
grow_extract <- cbind(beta0,beta1)
colnames(grow_extract) <- c("Beta0_1","Beta0_2","Beta0_3","Beta0_4", "Beta1_1","Beta1_2","Beta1_3","Beta1_4")
png("grow_panel.png")
par(mfrow = c(2,3))
# Other
plot(x = log(data$volume_t)  ,y = (quantile(grow_extract$Beta0_1,0.5) + log(data$volume_t) * quantile(grow_extract$Beta1_1,0.5)), type = "l", col = "black")
lines(x = log(data$volume_t), y = quantile(grow_extract$Beta0_1,0.95) + log(data$volume_t) * quantile(grow_extract$Beta1_1,0.95), type = "l", col = "darkgrey", lty = 2)
lines(x = log(data$volume_t), y = quantile(grow_extract$Beta0_1,0.05) + log(data$volume_t) * quantile(grow_extract$Beta1_1,0.05), type = "l", col = "darkgrey", lty = 2)
# Crem
plot(x = log(data$volume_t)  ,y = quantile(grow_extract$Beta0_2,0.5) + log(data$volume_t) * quantile(grow_extract$Beta1_2,0.5), type = "l", col = "red")
lines(x = log(data$volume_t), y = quantile(grow_extract$Beta0_2,0.95) + log(data$volume_t) * quantile(grow_extract$Beta1_2,0.95), type = "l", col = "darkgrey", lty = 2)
lines(x = log(data$volume_t), y = quantile(grow_extract$Beta0_2,0.05) + log(data$volume_t) * quantile(grow_extract$Beta1_2,0.05), type = "l", col = "darkgrey", lty = 2)
# Liom
plot(x = log(data$volume_t)  ,y = quantile(grow_extract$Beta0_3,0.5) + log(data$volume_t) * quantile(grow_extract$Beta1_3,0.5), type = "l", col = "blue")
lines(x = log(data$volume_t), y = quantile(grow_extract$Beta0_3,0.95) + log(data$volume_t) * quantile(grow_extract$Beta1_3,0.95), type = "l", col = "darkgrey", lty = 2)
lines(x = log(data$volume_t), y = quantile(grow_extract$Beta0_3,0.05) + log(data$volume_t) * quantile(grow_extract$Beta1_3,0.05), type = "l", col = "darkgrey", lty = 2)
# Vacant
plot(x = log(data$volume_t)  ,y = quantile(grow_extract$Beta0_4,0.5) + log(data$volume_t) * quantile(grow_extract$Beta1_4,0.5), type = "l", col = "pink")
lines(x = log(data$volume_t), y = quantile(grow_extract$Beta0_4,0.95) + log(data$volume_t) * quantile(grow_extract$Beta1_4,0.95), type = "l", col = "darkgrey", lty = 2)
lines(x = log(data$volume_t), y = quantile(grow_extract$Beta0_4,0.05) + log(data$volume_t) * quantile(grow_extract$Beta1_4,0.05), type = "l", col = "darkgrey", lty = 2)
# All together
plot(x = log(data$volume_t)  ,y = quantile(grow_extract$Beta0_1,0.5) + log(data$volume_t) * quantile(grow_extract$Beta1_1,0.5), type = "l")
lines(x = log(data$volume_t), y = quantile(grow_extract$Beta0_2,0.5) + log(data$volume_t) * quantile(grow_extract$Beta1_2,0.5), type = "l", col = "red")
lines(x = log(data$volume_t), y = quantile(grow_extract$Beta0_3,0.5) + log(data$volume_t) * quantile(grow_extract$Beta1_3,0.5), type = "l", col = "blue")
lines(x = log(data$volume_t), y = quantile(grow_extract$Beta0_4,0.5) + log(data$volume_t) * quantile(grow_extract$Beta1_4,0.5), type = "l", col = "pink")
dev.off()
##GGplots
par(mfrow = c(2,2))
ggplot(data = data, aes(x = log(volume_t), y = quantile(grow_extract$Beta0_1,0.5) + log(volume_t) * quantile(grow_extract$Beta1_1,0.5), col = "other"))+
  geom_smooth(col = "black") 
ggplot(data = data, aes(x = log(volume_t), y = quantile(grow_extract$Beta0_2,0.5) + log(volume_t) * quantile(grow_extract$Beta1_2,0.5), col = "crem"))+
  geom_smooth(col = "red") 
ggplot(data = data, aes(x = log(volume_t), y = quantile(grow_extract$Beta0_3,0.5) + log(volume_t) * quantile(grow_extract$Beta1_3,0.5), col = "liom"))+
  geom_smooth(col = "blue") 
ggplot(data = data, aes(x = log(volume_t), y = quantile(grow_extract$Beta0_4,0.5) + log(volume_t) * quantile(grow_extract$Beta1_4,0.5), col = "vacant"))+
  geom_smooth(col = "pink") 
## GG Plots
grow_a <- as.data.frame(get_posterior_mean(fit_grow_mix_ant, pars = c("y_rep")))
mean(grow_a$`mean-all chains`)
data$grow_a <- grow_a$`mean-all chains`
png("grow_panel1.png")
ggplot(data = data, aes(x = log(volume_t), y = grow_a)) + 
  geom_point() +
  geom_smooth() + 
  facet_wrap(data$ant)
dev.off()


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
## Panel Plots
extract_surv <- rstan::extract(fit_surv_mix_ant, pars = c("beta0","beta1"))
beta0 <- as.data.frame(extract_surv$beta0)
beta1 <- as.data.frame(extract_surv$beta1)
surv_extract <- cbind(beta0,beta1)
colnames(surv_extract) <- c("Beta0_1","Beta0_2","Beta0_3","Beta0_4", "Beta1_1","Beta1_2","Beta1_3","Beta1_4")
png("surv_panels1.png")
par(mfrow = c(2,3))
# Other
size_dummy <- seq(min(log(data$volume_t), na.rm = TRUE), max(log(data$volume_t), na.rm = TRUE), by = 0.1)
y_other = quantile(surv_extract$Beta0_1,0.5) + size_dummy * quantile(surv_extract$Beta1_1,0.5)
y_other_low = quantile(surv_extract$Beta0_1,0.05) + size_dummy * quantile(surv_extract$Beta1_1,0.05)
y_other_high = quantile(surv_extract$Beta0_1,0.95) + size_dummy * quantile(surv_extract$Beta1_1,0.95)
plot(x = size_dummy  ,y = invlogit(y_other), type = "l", col = "black")
points(x = size_dummy, y = invlogit(y_other))
lines(x = size_dummy, y = invlogit(y_other_low), type = "l", col = "darkgrey", lty = 2)
lines(x = size_dummy, y = invlogit(y_other_high), type = "l", col = "darkgrey", lty = 2)
# Crem
y_crem = quantile(surv_extract$Beta0_2,0.5) + size_dummy * quantile(surv_extract$Beta1_2,0.5)
y_crem_low = quantile(surv_extract$Beta0_2,0.05) + size_dummy * quantile(surv_extract$Beta1_2,0.05)
y_crem_high = quantile(surv_extract$Beta0_2,0.95) + size_dummy * quantile(surv_extract$Beta1_2,0.95)
plot(x = size_dummy  ,y = invlogit(y_crem), type = "l", col = "red")
points(x = size_dummy, y = invlogit(y_crem))
lines(x = size_dummy, y = invlogit(y_crem_low), type = "l", col = "darkgrey", lty = 2)
lines(x = size_dummy, y = invlogit(y_crem_high), type = "l", col = "darkgrey", lty = 2)
# Liom
y_liom = quantile(surv_extract$Beta0_3,0.5) + log(data$volume_t) * quantile(surv_extract$Beta1_3,0.5)
y_liom_low = quantile(surv_extract$Beta0_3,0.05) + log(data$volume_t) * quantile(surv_extract$Beta1_3,0.05)
y_liom_high = quantile(surv_extract$Beta0_3,0.95) + log(data$volume_t) * quantile(surv_extract$Beta1_3,0.95)
plot(x = log(data$volume_t)  ,y = invlogit(y_liom), type = "l", col = "blue")
points(x = log(data$volume_t), y = invlogit(y_liom))
lines(x = log(data$volume_t), y = invlogit(y_liom_low), type = "l", col = "darkgrey", lty = 2)
lines(x = log(data$volume_t), y = invlogit(y_liom_high), type = "l", col = "darkgrey", lty = 2)# Vacant
# All together
y_vac = quantile(surv_extract$Beta0_4,0.5) + log(data$volume_t) * quantile(surv_extract$Beta1_4,0.5)
y_vac_low = quantile(surv_extract$Beta0_4,0.05) + log(data$volume_t) * quantile(surv_extract$Beta1_4,0.05)
y_vac_high = quantile(surv_extract$Beta0_4,0.95) + log(data$volume_t) * quantile(surv_extract$Beta1_4,0.95)
plot(x = log(data$volume_t)  ,y = invlogit(y_vac), type = "l", col = "pink")
points(x = log(data$volume_t), y = invlogit(y_vac))
lines(x = log(data$volume_t), y = invlogit(y_vac_low), type = "l", col = "darkgrey", lty = 2)
lines(x = log(data$volume_t), y = invlogit(y_vac_high), type = "l", col = "darkgrey", lty = 2)# Vacant
dev.off()
## GG Plots
mean_surv01 <- mean(surv_extract$Beta0_1)
mean_surv02 <- mean(surv_extract$Beta0_2)
mean_surv03 <- mean(surv_extract$Beta0_3)
mean_surv04 <- mean(surv_extract$Beta0_4)
mean_surv11 <- mean(surv_extract$Beta1_1)
mean_surv12 <- mean(surv_extract$Beta1_2)
mean_surv13 <- mean(surv_extract$Beta1_3)
mean_surv14 <- mean(surv_extract$Beta1_4)
surv_gg <- as.data.frame(mean_surv11*log(data$volume_t) + mean_surv01)
colnames(surv_gg) <- c("ant_1")
surv_gg$ant_2 <- mean_surv12*log(data$volume_t) + mean_surv02
surv_gg$ant_3 <- mean_surv13*log(data$volume_t) + mean_surv03
surv_gg$ant_4 <- mean_surv14*log(data$volume_t) + mean_surv04
surv_gg$vol <- data$volume_t
png("surv_panel2.png")
ant1 <- ggplot(data = surv_gg, aes(y= invlogit(ant_1), x=log(vol))) +  
  geom_point(alpha = 0.1) +
  geom_smooth(data=surv_gg, method = "glm", formula='y~x', method.args = list(family = "binomial"), se = FALSE) 
ant2 <- ggplot(data = surv_gg, aes(y= invlogit(ant_2), x=log(vol))) +  
  geom_point(alpha = 0.1) +
  geom_smooth(data=surv_gg, method = "glm", formula='y~x', method.args = list(family = "binomial"), se = FALSE) 
ant3 <- ggplot(data = surv_gg, aes(y= invlogit(ant_3), x=log(vol))) +  
  geom_point(alpha = 0.1) +
  geom_smooth(data=surv_gg, method = "glm", formula='y~x', method.args = list(family = "binomial"), se = FALSE) 
ant4 <- ggplot(data = surv_gg, aes(y= invlogit(ant_4), x=log(vol))) +  
  geom_point(alpha = 0.1) +
  geom_smooth(data=surv_gg, method = "glm", formula='y~x', method.args = list(family = "binomial"), se = FALSE) 
grid.arrange(ant1,ant2,ant3,ant4)
dev.off()
## GG Plots
surv_a <- as.data.frame(get_posterior_mean(fit_surv_mix_ant, pars = c("y_rep")))
mean(surv_a$`mean-all chains`)
survival_data$surv_a <- surv_a$`mean-all chains`
png("surv_panel1.png")
ggplot(data = survival_data, aes(x = log(volume_t), y = surv_a)) + 
  geom_point() +
  geom_smooth() + 
  facet_wrap(survival_data$ant)
dev.off()

#### Flowering Visuals
extract_flow <- rstan::extract(fit_flow_mix_ant, pars = c("beta0","beta1"))
beta0 <- as.data.frame(extract_flow$beta0)
beta1 <- as.data.frame(extract_flow$beta1)
flow_extract <- cbind(beta0,beta1)
colnames(flow_extract) <- c("Beta0","Beta1")
## Overlay Plots
y <- y_flow
yrep_flow <- rstan::extract(fit_flow_mix_ant, pars = "y_rep")[["y_rep"]]
samp100 <- sample(nrow(yrep_flow), 500)
png(file = "flow_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_flow[samp100,])
dev.off()
png("flow_panels1.png")
par(mfrow = c(1,1))
y = quantile(flow_extract$Beta0,0.5) + log(flower$volume_t) * quantile(flow_extract$Beta1,0.5)
y_low = quantile(flow_extract$Beta0,0.05) + log(flower$volume_t) * quantile(flow_extract$Beta1,0.05)
y_high = quantile(flow_extract$Beta0,0.95) + log(flower$volume_t) * quantile(flow_extract$Beta1,0.95)
plot(x = log(flower$volume_t)  ,y = exp(y), type = "l", col = "black")
points(x = log(flower$volume_t), y = exp(y))
lines(x = log(flower$volume_t), y = exp(y_low), type = "l", col = "darkgrey", lty = 2)
lines(x = log(flower$volume_t), y = exp(y_high), type = "l", col = "darkgrey", lty = 2)
dev.off()
## Convergence Plots
png(file = "flow_conv1")
bayesplot::mcmc_trace(As.mcmc.list(fit_flow_mix_ant, pars=c("beta0", "beta1")))
dev.off()
## GGplots
flow_a <- as.data.frame(get_posterior_mean(fit_flow_mix_ant, pars = c("y_rep")))
mean(flow_a$`mean-all chains`)
flower$flow_a <- flow_a$`mean-all chains`
png("flow_panel2.png")
ggplot(data = flower, aes(x = log(volume_t), y = flow_a)) + 
  geom_point() +
  geom_smooth() 
dev.off()
## GG Plots
mean_flow0 <- mean(flow_extract$Beta0)
mean_flow1 <- mean(flow_extract$Beta1)
flow_gg <- as.data.frame(mean_flow1*log(flower$volume_t) + mean_flow0)
colnames(flow_gg) <- c("ant")
flow_gg$vol <- flower$volume_t
png("flow_panel2.png")
ggplot(data = flow_gg, aes(y= ant, x=log(vol))) +  
  geom_point(alpha = 0.1) +
  geom_smooth(data=flow_gg, method = "glm", formula='y~x', method.args = list(family = "poisson"), se = FALSE) 
dev.off()


#### Reproductive Visuals
## Overlay Plots
y <- y_repro
yrep_repro <- rstan::extract(fit_repro_mix_ant, pars = "y_rep")[["y_rep"]]
samp100 <- sample(nrow(yrep_repro), 500)
png(file = "repro_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_repro[samp100,])
dev.off()
## Convergence Plots
png(file = "repro_conv1.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_repro_mix_ant, pars=c("beta0", "beta1")))
dev.off()
## GGplots
repro_a <- as.data.frame(get_posterior_mean(fit_repro_mix_ant, pars = c("y_rep")))
mean(repro_a$`mean-all chains`)
flower$repro_a <- repro_a$`mean-all chains`
png("repro_panel2.png")
ggplot(data = flower, aes(x = log(volume_t), y = repro_a)) + 
  geom_point() +
  geom_smooth() 
dev.off()
##GG Plots
extract_repro <- rstan::extract(fit_repro_mix_ant, pars = c("beta0","beta1"))
beta0 <- as.data.frame(extract_repro$beta0)
beta1 <- as.data.frame(extract_repro$beta1)
repro_extract <- cbind(beta0,beta1)
colnames(repro_extract) <- c("Beta0","Beta1")
mean_repro0 <- mean(repro_extract$Beta0)
mean_repro1 <- mean(repro_extract$Beta1)
repro_gg <- as.data.frame(mean_repro1*log(flower$volume_t) + mean_repro0)
colnames(repro_gg) <- c("ant")
repro_gg$vol <- flower$volume_t
png("repro_panel2.png")
ggplot(data = repro_gg, aes(y= invlogit(ant), x=(vol))) +  
  geom_point(alpha = 0.1) +
  geom_smooth(data=repro_gg, method = "glm", formula='y~x', method.args = list(family = "binomial"), se = FALSE) 
dev.off()
## Panel Plots
png("repro_panel1.png")
par(mfrow = c(1,1))
y = quantile(repro_extract$Beta0,0.5) + log(flower$volume_t) * quantile(repro_extract$Beta1,0.5)
y_low = quantile(repro_extract$Beta0,0.05) + log(flower$volume_t) * quantile(repro_extract$Beta1,0.05)
y_high = quantile(repro_extract$Beta0,0.95) + log(flower$volume_t) * quantile(repro_extract$Beta1,0.95)
plot(x = log(flower$volume_t)  ,y = invlogit(y), type = "l", col = "red")
points(x = log(flower$volume_t), y = invlogit(y))
lines(x = log(flower$volume_t), y = invlogit(y_low), type = "l", col = "darkgrey", lty = 2)
lines(x = log(flower$volume_t), y = invlogit(y_high), type = "l", col = "darkgrey", lty = 2)
dev.off()
plot(x = size_dummy, y = invlogit(-5+5*size_dummy))
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
## Convergence Plots
png(file = "viab_conv1")
bayesplot::mcmc_trace(As.mcmc.list(fitty, pars=c("beta0")))
dev.off()
##GG Plots
viab_a <- as.data.frame(get_posterior_mean(fit_viab_mix_ant, pars = c("y_rep")))
mean(viab_a$`mean-all chains`)
flower$viab_a <- viab_a$`mean-all chains`
png("flow_panel1.png")
ggplot(data = flower, aes(x = log(volume_t), y = viab_a)) + 
  geom_point() +
  geom_smooth() + 
  facet_wrap(flower$ant)
dev.off()
## GG Plots
extract_viab <- rstan::extract(fit_viab_mix_ant, pars = c("beta0"))
beta0 <- as.data.frame(extract_viab$beta0)
viab_extract <- beta0
colnames(viab_extract) <- c("Beta0_1","Beta0_2","Beta0_3","Beta0_4")
mean_viab01 <- mean(viab_extract$Beta0_1)
mean_viab02 <- mean(viab_extract$Beta0_2)
mean_viab03 <- mean(viab_extract$Beta0_3)
mean_viab04 <- mean(viab_extract$Beta0_4)
viab_gg <- as.data.frame( mean_viab01)
colnames(viab_gg) <- c("ant_1")
viab_gg$ant_2 <-  mean_viab02
viab_gg$ant_3 <-  mean_viab03
viab_gg$ant_4 <-  mean_viab04
viab_gg$vol <- flower$volume_t
png("viab_panel2.png")
ant1 <- ggplot(data = viab_gg, aes(y= ant_1, x=log(flower$volume_t))) +  
  geom_point(alpha = 0.1) +
  geom_smooth(data=viab_gg, method = "glm", formula='y~x', method.args = list(family = "binomial"), se = FALSE) 
ant2 <- ggplot(data = viab_gg, aes(y= ant_2, x=log(flower$volume_t))) +  
  geom_point(alpha = 0.1) +
  geom_smooth(data=viab_gg, method = "glm", formula='y~x', method.args = list(family = "binomial"), se = FALSE) 
ant3 <- ggplot(data = viab_gg, aes(y= ant_3, x=log(flower$volume_t))) +  
  geom_point(alpha = 0.1) +
  geom_smooth(data=viab_gg, method = "glm", formula='y~x', method.args = list(family = "binomial"), se = FALSE) 
ant4 <- ggplot(data = viab_gg, aes(y= ant_4, x=log(flower$volume_t))) +  
  geom_point(alpha = 0.1) +
  geom_smooth(data=viab_gg, method = "glm", formula='y~x', method.args = list(family = "binomial"), se = FALSE) 
grid.arrange(ant1,ant2,ant3,ant4)
dev.off()
## Panel Plots
png("repro_panels1.png")
par(mfrow = c(2,2))
# Other
y_other = quantile(viab_extract$Beta0_1,0.5) + log(flower$volume_t) * quantile(viab_extract$Beta1_1,0.5)
y_other_low = quantile(viab_extract$Beta0_1,0.05) + log(flower$volume_t) * quantile(viab_extract$Beta1_1,0.05)
y_other_high = quantile(viab_extract$Beta0_1,0.95) + log(flower$volume_t) * quantile(viab_extract$Beta1_1,0.95)
plot(x = log(flower$volume_t)  ,y = invlogit(y_other), type = "l", col = "black", ylim = c(0,100))
points(x = log(flower$volume_t), y = invlogit(y_other))
lines(x = log(flower$volume_t), y = invlogit(y_other_low), type = "l", col = "darkgrey", lty = 2)
lines(x = log(flower$volume_t), y = invlogit(y_other_high), type = "l", col = "darkgrey", lty = 2)
# Crem
y_crem = quantile(repro_extract$Beta0_2,0.5) + log(flower$volume_t) * quantile(repro_extract$Beta1_2,0.5)
y_crem_low = quantile(repro_extract$Beta0_2,0.05) + log(flower$volume_t) * quantile(repro_extract$Beta1_2,0.05)
y_crem_high = quantile(repro_extract$Beta0_2,0.95) + log(flower$volume_t) * quantile(repro_extract$Beta1_2,0.95)
plot(x = log(flower$volume_t)  ,y = invlogit(y_crem), type = "l", col = "red")
points(x = log(flower$volume_t), y = invlogit(y_crem))
lines(x = log(flower$volume_t), y = invlogit(y_crem_low), type = "l", col = "darkgrey", lty = 2)
lines(x = log(flower$volume_t), y = invlogit(y_crem_high), type = "l", col = "darkgrey", lty = 2)
# Liom
y_liom = quantile(repro_extract$Beta0_3,0.5) + log(flower$volume_t) * quantile(repro_extract$Beta1_3,0.5)
y_liom_low = quantile(repro_extract$Beta0_3,0.05) + log(flower$volume_t) * quantile(repro_extract$Beta1_3,0.05)
y_liom_high = quantile(repro_extract$Beta0_3,0.95) + log(flower$volume_t) * quantile(repro_extract$Beta1_3,0.95)
plot(x = log(flower$volume_t)  ,y = (invlogit(y_liom)), type = "l", col = "blue")
points(x = log(flower$volume_t), y = (invlogit(y_liom)))
lines(x = log(flower$volume_t), y = invlogit(y_liom_low), type = "l", col = "darkgrey", lty = 2)
lines(x = log(flower$volume_t), y = invlogit(y_liom_high), type = "l", col = "darkgrey", lty = 2)# Vacant
# All together
y_vac = quantile(repro_extract$Beta0_4,0.5) + log(flower$volume_t) * quantile(repro_extract$Beta1_4,0.5)
y_vac_low = quantile(repro_extract$Beta0_4,0.05) + log(flower$volume_t) * quantile(repro_extract$Beta1_4,0.05)
y_vac_high = quantile(repro_extract$Beta0_4,0.95) + log(flower$volume_t) * quantile(repro_extract$Beta1_4,0.95)
plot(x = log(flower$volume_t)  ,y = (invlogit(y_vac)), type = "l", col = "pink")
points(x = log(flower$volume_t), y = (invlogit(y_vac)))
lines(x = log(flower$volume_t), y = invlogit(y_vac_low), type = "l", col = "darkgrey", lty = 2)
lines(x = log(flower$volume_t), y = invlogit(y_vac_high), type = "l", col = "darkgrey", lty = 2)# Vacant
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




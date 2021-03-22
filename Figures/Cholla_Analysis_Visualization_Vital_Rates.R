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
extract_grow <- rstan::extract(fit_grow_mix_ant, pars = c("beta0","beta1","mu"))
beta0 <- as.data.frame(extract_grow$beta0)
beta1 <- as.data.frame(extract_grow$beta1)
mu <- as.data.frame(extract_grow$mu)
grow_extract <- cbind(beta0,beta1)
colnames(grow_extract) <- c("Beta0_1","Beta0_2","Beta0_3","Beta0_4", "Beta1_1","Beta1_2","Beta1_3","Beta1_4")
for(i in 1:15000){
  grow_extract$pred_1 <- grow_extract$Beta0_1 + grow_extract$Beta1_1
  grow_extract$pred_2 <- grow_extract$Beta0_2 + grow_extract$Beta1_2
  grow_extract$pred_3 <- grow_extract$Beta0_3 + grow_extract$Beta1_3
  grow_extract$pred_4 <- grow_extract$Beta0_4 + grow_extract$Beta1_4
}
png("grow_panel.png")
par(mfrow = c(2,3))
# Other
plot(x = log(data$volume_t)  ,y = quantile(grow_extract$Beta0_1,0.5) + log(data$volume_t) * quantile(grow_extract$Beta1_1,0.5), type = "l", col = "black")
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
for(i in 1:15000){
  surv_extract$pred_1 <- surv_extract$Beta0_1 + surv_extract$Beta1_1
  surv_extract$pred_2 <- surv_extract$Beta0_2 + surv_extract$Beta1_2
  surv_extract$pred_3 <- surv_extract$Beta0_3 + surv_extract$Beta1_3
  surv_extract$pred_4 <- surv_extract$Beta0_4 + surv_extract$Beta1_4
}
png("surv_panels1.png")
par(mfrow = c(2,3))
# Other
y_other = quantile(surv_extract$Beta0_1,0.5) + log(data$volume_t) * quantile(surv_extract$Beta1_1,0.5)
y_other_low = quantile(surv_extract$Beta0_1,0.05) + log(data$volume_t) * quantile(surv_extract$Beta1_1,0.05)
y_other_high = quantile(surv_extract$Beta0_1,0.95) + log(data$volume_t) * quantile(surv_extract$Beta1_1,0.95)
plot(x = log(data$volume_t)  ,y = (exp(y_other))/(1 + exp(y_other)), type = "l", col = "black")
points(x = log(data$volume_t), y = (exp(y_other))/(1 + exp(y_other)))
lines(x = log(data$volume_t), y = exp(y_other_low)/(1 + exp(y_other_low)), type = "l", col = "darkgrey", lty = 2)
lines(x = log(data$volume_t), y = exp(y_other_high)/(1 + exp(y_other_high)), type = "l", col = "darkgrey", lty = 2)
# Crem
y_crem = quantile(surv_extract$Beta0_2,0.5) + log(data$volume_t) * quantile(surv_extract$Beta1_2,0.5)
y_crem_low = quantile(surv_extract$Beta0_2,0.05) + log(data$volume_t) * quantile(surv_extract$Beta1_2,0.05)
y_crem_high = quantile(surv_extract$Beta0_2,0.95) + log(data$volume_t) * quantile(surv_extract$Beta1_2,0.95)
plot(x = log(data$volume_t)  ,y = (exp(y_crem))/(1 + exp(y_crem)), type = "l", col = "red")
points(x = log(data$volume_t), y = (exp(y_crem))/(1 + exp(y_crem)))
lines(x = log(data$volume_t), y = exp(y_crem_low)/(1 + exp(y_crem_low)), type = "l", col = "darkgrey", lty = 2)
lines(x = log(data$volume_t), y = exp(y_crem_high)/(1 + exp(y_crem_high)), type = "l", col = "darkgrey", lty = 2)
# Liom
y_liom = quantile(surv_extract$Beta0_3,0.5) + log(data$volume_t) * quantile(surv_extract$Beta1_3,0.5)
y_liom_low = quantile(surv_extract$Beta0_3,0.05) + log(data$volume_t) * quantile(surv_extract$Beta1_3,0.05)
y_liom_high = quantile(surv_extract$Beta0_3,0.95) + log(data$volume_t) * quantile(surv_extract$Beta1_3,0.95)
plot(x = log(data$volume_t)  ,y = (exp(y_liom))/(1 + exp(y_liom)), type = "l", col = "blue")
points(x = log(data$volume_t), y = (exp(y_liom))/(1 + exp(y_liom)))
lines(x = log(data$volume_t), y = exp(y_liom_low)/(1 + exp(y_liom_low)), type = "l", col = "darkgrey", lty = 2)
lines(x = log(data$volume_t), y = exp(y_liom_high)/(1 + exp(y_liom_high)), type = "l", col = "darkgrey", lty = 2)# Vacant
# All together
y_vac = quantile(surv_extract$Beta0_4,0.5) + log(data$volume_t) * quantile(surv_extract$Beta1_4,0.5)
y_vac_low = quantile(surv_extract$Beta0_4,0.05) + log(data$volume_t) * quantile(surv_extract$Beta1_4,0.05)
y_vac_high = quantile(surv_extract$Beta0_4,0.95) + log(data$volume_t) * quantile(surv_extract$Beta1_4,0.95)
plot(x = log(data$volume_t)  ,y = (exp(y_vac))/(1 + exp(y_vac)), type = "l", col = "pink")
points(x = log(data$volume_t), y = (exp(y_vac))/(1 + exp(y_vac)))
lines(x = log(data$volume_t), y = exp(y_vac_low)/(1 + exp(y_vac_low)), type = "l", col = "darkgrey", lty = 2)
lines(x = log(data$volume_t), y = exp(y_vac_high)/(1 + exp(y_vac_high)), type = "l", col = "darkgrey", lty = 2)# Vacant
dev.off()
## All lines panels
png("surv_panels2.png")
par(mfrow = c(2,3))
# Other
y_other = quantile(surv_extract$Beta0_1,0.5) + log(data$volume_t) * quantile(surv_extract$Beta1_1,0.5)
y_other_low = quantile(surv_extract$Beta0_1,0.05) + log(data$volume_t) * quantile(surv_extract$Beta1_1,0.05)
y_other_high = quantile(surv_extract$Beta0_1,0.95) + log(data$volume_t) * quantile(surv_extract$Beta1_1,0.95)
plot(x = log(data$volume_t)  ,y = (exp(y_other))/(1 + exp(y_other)), type = "l", col = "black")
#points(x = log(data$volume_t), y = (exp(y_other))/(1 + exp(y_other)))
#lines(x = log(data$volume_t), y = exp(y_other_low)/(1 + exp(y_other_low)), type = "l", col = "darkgrey", lty = 2)
#lines(x = log(data$volume_t), y = exp(y_other_high)/(1 + exp(y_other_high)), type = "l", col = "darkgrey", lty = 2)
# Crem
y_crem = quantile(surv_extract$Beta0_2,0.5) + log(data$volume_t) * quantile(surv_extract$Beta1_2,0.5)
y_crem_low = quantile(surv_extract$Beta0_2,0.05) + log(data$volume_t) * quantile(surv_extract$Beta1_2,0.05)
y_crem_high = quantile(surv_extract$Beta0_2,0.95) + log(data$volume_t) * quantile(surv_extract$Beta1_2,0.95)
plot(x = log(data$volume_t)  ,y = (exp(y_crem))/(1 + exp(y_crem)), type = "l", col = "red")
#points(x = log(data$volume_t), y = (exp(y_crem))/(1 + exp(y_crem)))
#lines(x = log(data$volume_t), y = exp(y_crem_low)/(1 + exp(y_crem_low)), type = "l", col = "darkgrey", lty = 2)
#lines(x = log(data$volume_t), y = exp(y_crem_high)/(1 + exp(y_crem_high)), type = "l", col = "darkgrey", lty = 2)
# Liom
y_liom = quantile(surv_extract$Beta0_3,0.5) + log(data$volume_t) * quantile(surv_extract$Beta1_3,0.5)
y_liom_low = quantile(surv_extract$Beta0_3,0.05) + log(data$volume_t) * quantile(surv_extract$Beta1_3,0.05)
y_liom_high = quantile(surv_extract$Beta0_3,0.95) + log(data$volume_t) * quantile(surv_extract$Beta1_3,0.95)
plot(x = log(data$volume_t)  ,y = (exp(y_liom))/(1 + exp(y_liom)), type = "l", col = "blue")
#points(x = log(data$volume_t), y = (exp(y_liom))/(1 + exp(y_liom)))
#lines(x = log(data$volume_t), y = exp(y_liom_low)/(1 + exp(y_liom_low)), type = "l", col = "darkgrey", lty = 2)
#lines(x = log(data$volume_t), y = exp(y_liom_high)/(1 + exp(y_liom_high)), type = "l", col = "darkgrey", lty = 2)# Vacant
# All together
y_vac = quantile(surv_extract$Beta0_4,0.5) + log(data$volume_t) * quantile(surv_extract$Beta1_4,0.5)
y_vac_low = quantile(surv_extract$Beta0_4,0.05) + log(data$volume_t) * quantile(surv_extract$Beta1_4,0.05)
y_vac_high = quantile(surv_extract$Beta0_4,0.95) + log(data$volume_t) * quantile(surv_extract$Beta1_4,0.95)
plot(x = log(data$volume_t)  ,y = (exp(y_vac))/(1 + exp(y_vac)), type = "l", col = "pink")
#points(x = log(data$volume_t), y = (exp(y_vac))/(1 + exp(y_vac)))
#lines(x = log(data$volume_t), y = exp(y_vac_low)/(1 + exp(y_vac_low)), type = "l", col = "darkgrey", lty = 2)
#lines(x = log(data$volume_t), y = exp(y_vac_high)/(1 + exp(y_vac_high)), type = "l", col = "darkgrey", lty = 2)# Vacant
dev.off()
## All lines panels with data points
png("surv_panels3.png")
par(mfrow = c(2,3))
# Other
y_other = quantile(surv_extract$Beta0_1,0.5) + log(data$volume_t) * quantile(surv_extract$Beta1_1,0.5)
y_other_low = quantile(surv_extract$Beta0_1,0.05) + log(data$volume_t) * quantile(surv_extract$Beta1_1,0.05)
y_other_high = quantile(surv_extract$Beta0_1,0.95) + log(data$volume_t) * quantile(surv_extract$Beta1_1,0.95)
plot(x = log(data$volume_t)  ,y = (exp(y_other))/(1 + exp(y_other)), type = "l", col = "black")
points(x = log(data$volume_t), y = (exp(y_other))/(1 + exp(y_other)))
#lines(x = log(data$volume_t), y = exp(y_other_low)/(1 + exp(y_other_low)), type = "l", col = "darkgrey", lty = 2)
#lines(x = log(data$volume_t), y = exp(y_other_high)/(1 + exp(y_other_high)), type = "l", col = "darkgrey", lty = 2)
# Crem
y_crem = quantile(surv_extract$Beta0_2,0.5) + log(data$volume_t) * quantile(surv_extract$Beta1_2,0.5)
y_crem_low = quantile(surv_extract$Beta0_2,0.05) + log(data$volume_t) * quantile(surv_extract$Beta1_2,0.05)
y_crem_high = quantile(surv_extract$Beta0_2,0.95) + log(data$volume_t) * quantile(surv_extract$Beta1_2,0.95)
plot(x = log(data$volume_t)  ,y = (exp(y_crem))/(1 + exp(y_crem)), type = "l", col = "red")
points(x = log(data$volume_t), y = (exp(y_crem))/(1 + exp(y_crem)))
#lines(x = log(data$volume_t), y = exp(y_crem_low)/(1 + exp(y_crem_low)), type = "l", col = "darkgrey", lty = 2)
#lines(x = log(data$volume_t), y = exp(y_crem_high)/(1 + exp(y_crem_high)), type = "l", col = "darkgrey", lty = 2)
# Liom
y_liom = quantile(surv_extract$Beta0_3,0.5) + log(data$volume_t) * quantile(surv_extract$Beta1_3,0.5)
y_liom_low = quantile(surv_extract$Beta0_3,0.05) + log(data$volume_t) * quantile(surv_extract$Beta1_3,0.05)
y_liom_high = quantile(surv_extract$Beta0_3,0.95) + log(data$volume_t) * quantile(surv_extract$Beta1_3,0.95)
plot(x = log(data$volume_t)  ,y = (exp(y_liom))/(1 + exp(y_liom)), type = "l", col = "blue")
points(x = log(data$volume_t), y = (exp(y_liom))/(1 + exp(y_liom)))
#lines(x = log(data$volume_t), y = exp(y_liom_low)/(1 + exp(y_liom_low)), type = "l", col = "darkgrey", lty = 2)
#lines(x = log(data$volume_t), y = exp(y_liom_high)/(1 + exp(y_liom_high)), type = "l", col = "darkgrey", lty = 2)# Vacant
# All together
y_vac = quantile(surv_extract$Beta0_4,0.5) + log(data$volume_t) * quantile(surv_extract$Beta1_4,0.5)
y_vac_low = quantile(surv_extract$Beta0_4,0.05) + log(data$volume_t) * quantile(surv_extract$Beta1_4,0.05)
y_vac_high = quantile(surv_extract$Beta0_4,0.95) + log(data$volume_t) * quantile(surv_extract$Beta1_4,0.95)
plot(x = log(data$volume_t)  ,y = (exp(y_vac))/(1 + exp(y_vac)), type = "l", col = "pink")
points(x = log(data$volume_t), y = (exp(y_vac))/(1 + exp(y_vac)))
#lines(x = log(data$volume_t), y = exp(y_vac_low)/(1 + exp(y_vac_low)), type = "l", col = "darkgrey", lty = 2)
#lines(x = log(data$volume_t), y = exp(y_vac_high)/(1 + exp(y_vac_high)), type = "l", col = "darkgrey", lty = 2)# Vacant
dev.off()
## Mean linea nd conf interval
png("surv_panels2.png")
par(mfrow = c(2,3))
# Other
y_other = quantile(surv_extract$Beta0_1,0.5) + log(data$volume_t) * quantile(surv_extract$Beta1_1,0.5)
y_other_low = mean(quantile(surv_extract$Beta0_1,0.05)) + log(data$volume_t) * mean(quantile(surv_extract$Beta1_1,0.05))
y_other_high = mean(quantile(surv_extract$Beta0_1,0.95)) + log(data$volume_t) * mean(quantile(surv_extract$Beta1_1,0.95))
plot(x = log(data$volume_t)  ,y = (exp(y_other))/(1+exp(y_other)), type = "l", col = "black")
#points(x = log(data$volume_t), y = (exp(y_other))/(1 + exp(y_other)))
lines(x = log(data$volume_t), y = exp(y_other_low)/(1 + exp(y_other_low)), type = "l", col = "darkgrey", lty = 2)
lines(x = log(data$volume_t), y = exp(y_other_high)/(1 + exp(y_other_high)), type = "l", col = "darkgrey", lty = 2)
# Crem
y_crem = quantile(surv_extract$Beta0_2,0.5) + log(data$volume_t) * quantile(surv_extract$Beta1_2,0.5)
y_crem_low = quantile(surv_extract$Beta0_2,0.05) + log(data$volume_t) * quantile(surv_extract$Beta1_2,0.05)
y_crem_high = quantile(surv_extract$Beta0_2,0.95) + log(data$volume_t) * quantile(surv_extract$Beta1_2,0.95)
plot(x = log(data$volume_t)  ,y = (exp(y_crem))/(1 + exp(y_crem)), type = "l", col = "red")
points(x = log(data$volume_t), y = (exp(y_crem))/(1 + exp(y_crem)))
#lines(x = log(data$volume_t), y = exp(y_crem_low)/(1 + exp(y_crem_low)), type = "l", col = "darkgrey", lty = 2)
#lines(x = log(data$volume_t), y = exp(y_crem_high)/(1 + exp(y_crem_high)), type = "l", col = "darkgrey", lty = 2)
# Liom
y_liom = quantile(surv_extract$Beta0_3,0.5) + log(data$volume_t) * quantile(surv_extract$Beta1_3,0.5)
y_liom_low = quantile(surv_extract$Beta0_3,0.05) + log(data$volume_t) * quantile(surv_extract$Beta1_3,0.05)
y_liom_high = quantile(surv_extract$Beta0_3,0.95) + log(data$volume_t) * quantile(surv_extract$Beta1_3,0.95)
plot(x = log(data$volume_t)  ,y = (exp(y_liom))/(1 + exp(y_liom)), type = "l", col = "blue")
points(x = log(data$volume_t), y = (exp(y_liom))/(1 + exp(y_liom)))
#lines(x = log(data$volume_t), y = exp(y_liom_low)/(1 + exp(y_liom_low)), type = "l", col = "darkgrey", lty = 2)
#lines(x = log(data$volume_t), y = exp(y_liom_high)/(1 + exp(y_liom_high)), type = "l", col = "darkgrey", lty = 2)# Vacant
# All together
y_vac = quantile(surv_extract$Beta0_4,0.5) + log(data$volume_t) * quantile(surv_extract$Beta1_4,0.5)
y_vac_low = quantile(surv_extract$Beta0_4,0.05) + log(data$volume_t) * quantile(surv_extract$Beta1_4,0.05)
y_vac_high = quantile(surv_extract$Beta0_4,0.95) + log(data$volume_t) * quantile(surv_extract$Beta1_4,0.95)
plot(x = log(data$volume_t)  ,y = (exp(y_vac))/(1 + exp(y_vac)), type = "l", col = "pink")
points(x = log(data$volume_t), y = (exp(y_vac))/(1 + exp(y_vac)))
#lines(x = log(data$volume_t), y = exp(y_vac_low)/(1 + exp(y_vac_low)), type = "l", col = "darkgrey", lty = 2)
#lines(x = log(data$volume_t), y = exp(y_vac_high)/(1 + exp(y_vac_high)), type = "l", col = "darkgrey", lty = 2)# Vacant
dev.off()
##GGplots
par(mfrow = c(2,2))
ggplot(data = data, aes(x = log(volume_t), y = quantile(surv_extract$Beta0_1,0.5) + log(volume_t) * quantile(surv_extract$Beta1_1,0.5), col = "other"))+
  geom_smooth(col = "black") 
ggplot(data = data, aes(x = log(volume_t), y = quantile(surv_extract$Beta0_2,0.5) + log(volume_t) * quantile(surv_extract$Beta1_2,0.5), col = "crem"))+
  geom_smooth(col = "red") 
ggplot(data = data, aes(x = log(volume_t), y = quantile(surv_extract$Beta0_3,0.5) + log(volume_t) * quantile(surv_extract$Beta1_3,0.5), col = "liom"))+
  geom_smooth(col = "blue") 
ggplot(data = data, aes(x = log(volume_t), y = quantile(surv_extract$Beta0_4,0.5) + log(volume_t) * quantile(surv_extract$Beta1_4,0.5), col = "vacant"))+
  geom_smooth(col = "pink") 
## Line Plots
par(mfrow = c(1,1))
x <- data$volume_t
y <- predict_posterior(fit_surv_mix_ant, list(vol = x), type = "response")
vol_range <- seq(from=min(data$volume_t), to=max(data$volume_t), by=.01)
a_logits <- mean(surv_extract$Beta0_1) + x*vol_range
a_probs <- exp(a_logits)/(1 + exp(a_logits))
plot(vol_range, a_probs, ylim=c(0,1),type="l", lwd=3,  lty=2, col="gold", 
     xlab="X1", ylab="P(outcome)", main="Probability of super important outcome")
plot(dbinom(x,size = length(x),prob = 0.5))

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




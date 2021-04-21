setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")

size_dummy <- seq(min(log(cactus$volume_t), na.rm = TRUE), max(log(cactus$volume_t1), na.rm = TRUE), by = 0.1)
#### Growth Visuals #####################################################################################################
## Extract & Format Data
#For overlay plots
y <- y_grow
yrep_grow <- rstan::extract(fit_grow_mix_ant, pars = "y_rep")[["y_rep"]]
samp100 <- sample(nrow(yrep_grow), 500)
#extract from STAN models
extract_grow <- rstan::extract(fit_grow_mix_ant, pars = c("beta0","beta1"))
beta0 <- as.data.frame(extract_grow$beta0)
beta1 <- as.data.frame(extract_grow$beta1)
grow_extract <- cbind(beta0,beta1)
colnames(grow_extract) <- c("Beta0_1","Beta0_2","Beta0_3","Beta0_4", "Beta1_1","Beta1_2","Beta1_3","Beta1_4")
#format for yrep gg plots
grow_a <- as.data.frame(get_posterior_mean(fit_grow_mix_ant, pars = c("y_rep")))
mean(grow_a$`mean-all chains`)
data$grow_a <- grow_a$`mean-all chains`
#extract from original data
y_subset <- data[,c("volume_t1","ant", "volume_t")]
## Formulas
y_other_mean_grow <- quantile(grow_extract$Beta0_3,0.5) + size_dummy * quantile(grow_extract$Beta1_3,0.5)
y_other_low_grow <- quantile(grow_extract$Beta0_3,0.05) + size_dummy * quantile(grow_extract$Beta1_3,0.05)
y_other_high_grow <- quantile(grow_extract$Beta0_3,0.95) + size_dummy * quantile(grow_extract$Beta1_3,0.95)
y_other_subset_grow <- subset(y_subset, ant == 3)
y_crem_mean_grow <- quantile(grow_extract$Beta0_1,0.5) + size_dummy * quantile(grow_extract$Beta1_1,0.5)
y_crem_low_grow <- quantile(grow_extract$Beta0_1,0.05) + size_dummy * quantile(grow_extract$Beta1_1,0.05)
y_crem_high_grow <- quantile(grow_extract$Beta0_1,0.95) + size_dummy * quantile(grow_extract$Beta1_1,0.95)
y_crem_subset_grow <- subset(y_subset, ant == 1)
y_liom_mean_grow <- quantile(grow_extract$Beta0_2,0.5) + size_dummy * quantile(grow_extract$Beta1_2,0.5)
y_liom_low_grow <- quantile(grow_extract$Beta0_2,0.05) + size_dummy * quantile(grow_extract$Beta1_2,0.05)
y_liom_high_grow <- quantile(grow_extract$Beta0_2,0.95) + size_dummy * quantile(grow_extract$Beta1_2,0.95)
y_liom_subset_grow <- subset(y_subset, ant == 2)
y_vac_mean_grow <- quantile(grow_extract$Beta0_4,0.5) + size_dummy * quantile(grow_extract$Beta1_4,0.5)
y_vac_low_grow <- quantile(grow_extract$Beta0_4,0.05) + size_dummy * quantile(grow_extract$Beta1_4,0.05)
y_vac_high_grow <- quantile(grow_extract$Beta0_4,0.95) + size_dummy * quantile(grow_extract$Beta1_4,0.95)
y_vac_subset_grow <- subset(y_subset, ant == 4)
## Overlay Plots
png(file = "grow_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_grow[samp100,])
dev.off()
png(file = "grow_ant_post1.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_grow[samp100,], group = ant_grow)
dev.off()
## Convergence Plots
png("grow_conv2.png")
ggtitle("Growth Model Parameter Convergence")
bayesplot::mcmc_trace(As.mcmc.list(fit_grow_mix_ant, pars=c("beta0", "beta1")))
title()
dev.off()
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
png("grow_panel.png")
par(mar=c(2,2,2,2))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(0.5,1.5,1.5), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.5,"Growth Rates of Cacti by Ant State",cex=2,font=2)
# Other (3)
plot(x = size_dummy  ,y = y_other_mean_grow, type = "l", col = "black", lwd = 4,
     xlab = "Log of the Volume of Cacti year t", ylab = "Log of the Volume of Cacti Year t+1") + 
#lines(x = size_dummy, y = y_other_high_grow, type = "l", col = "darkgrey", lty = 2, lwd = 2) + 
#lines(x = size_dummy, y = y_other_low_grow, type = "l", col = "darkgrey", lty = 2, lwd = 2) + 
points(x = log(y_other_subset_grow$volume_t), y = log(y_other_subset_grow$volume_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) + 
polygon(c(size_dummy,rev(size_dummy)),c(y_other_high_grow, rev(y_other_low_grow)),
          col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3), border = NA)
# Crem (1)
plot(x = size_dummy  ,y = y_crem_mean_grow, type = "l", col = "red", lwd = 4,
     xlab = "Log of the Volume of Cacti year t", ylab = "Log of the Volume of Cacti Year t+1") + 
#lines(x = size_dummy, y = y_crem_high_grow, type = "l", col = "darkgrey", lty = 2, lwd = 2) + 
#lines(x = size_dummy, y = y_crem_low_grow, type = "l", col = "darkgrey", lty = 2, lwd = 2) + 
points(x = log(y_crem_subset_grow$volume_t), y = log(y_crem_subset_grow$volume_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) + 
  polygon(c(size_dummy,rev(size_dummy)),c(y_crem_high_grow, rev(y_crem_low_grow)),
          col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3), border = NA)
# Liom (2)
plot(x = size_dummy  ,y = y_liom_mean_grow, type = "l", col = "blue", lwd = 4,
     xlab = "Log of the Volume of Cacti year t", ylab = "Log of the Volume of Cacti Year t+1") + 
#lines(x = size_dummy, y = y_liom_high_grow, type = "l", col = "darkgrey", lty = 2, lwd = 2) + 
#lines(x = size_dummy, y = y_liom_low_grow, type = "l", col = "darkgrey", lty = 2, lwd = 2) + 
  points(x = log(y_liom_subset_grow$volume_t), y = log(y_liom_subset_grow$volume_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) + 
  polygon(c(size_dummy,rev(size_dummy)),c(y_liom_high_grow, rev(y_liom_low_grow)),
          col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3), border = NA)
# Vacant (4)s
plot(x = size_dummy  ,y = y_vac_mean_grow, type = "l", col = "pink", lwd = 4,
     xlab = "Log of the Volume of Cacti year t", ylab = "Log of the Volume of Cacti Year t+1") + 
#lines(x = size_dummy, y = y_vac_high_grow, type = "l", col = "darkgrey", lty = 2, lwd = 2) + 
#lines(x = size_dummy, y = y_vac_low_grow, type = "l", col = "darkgrey", lty = 2, lwd = 2) + 
  points(x = log(y_vac_subset_grow$volume_t), y = log(y_vac_subset_grow$volume_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) + 
  polygon(c(size_dummy,rev(size_dummy)),c(y_vac_high_grow, rev(y_vac_low_grow)),
          col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3), border = NA)
# All together
plot(x = size_dummy  ,y = y_other_mean_grow, type = "l",lwd = 2,
     xlab = "Log of the Volume of Cacti year t", ylab = "Log of the Volume of Cacti Year t+1",
     ylim = c(8.8,9.5), xlim = c(8.8,9.2)) + 
lines(x = size_dummy, y = y_crem_mean_grow, type = "l", col = "red", lwd = 2) + 
lines(x = size_dummy, y = y_liom_mean_grow, type = "l", col = "blue",lwd = 2) + 
lines(x = size_dummy, y = y_vac_mean_grow, type = "l", col = "pink", lwd = 2) + 
  abline(a = 1, b = 1, col = "darkgrey", lty = 2)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c("black","red","blue","pink"), pch = 16)
dev.off()
## Panels 2
png("grow_panel2.png")
par(mar=c(5,5,0,1))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(0.5,1.5,1.5), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.5,"Growth Rates of Cacti by Ant State",cex=2,font=2)
# Other (3)
samp <- sample(nrow(grow_extract), 500)
plot(x = size_dummy  ,y = y_other_mean_grow, type = "l", col = "black", lwd = 4,
     xlab = "Log of the Volume of Cacti year t", ylab = "Log of the Volume of Cacti Year t+1") 
for(i in 1:samp){
  lines(x = size_dummy, y = (grow_extract$Beta0_3[i] + size_dummy * grow_extract$Beta1_3[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy  ,y = y_other_mean_grow, type = "l", col = "black", lwd = 4)
points(x = log(y_other_subset_grow$volume_t), y = log(y_other_subset_grow$volume_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
# Crem (1)
plot(x = size_dummy  ,y = y_crem_mean_grow, type = "l", col = "red", lwd = 4,
     xlab = "Log of the Volume of Cacti year t", ylab = "Log of the Volume of Cacti Year t+1") 
for(i in 1:samp){
  lines(x = size_dummy, y = (grow_extract$Beta0_1[i] + size_dummy * grow_extract$Beta1_1[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy  ,y = y_crem_mean_grow, type = "l", col = "red", lwd = 4)
points(x = log(y_crem_subset_grow$volume_t), y = log(y_crem_subset_grow$volume_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
# Liom (2)
plot(x = size_dummy  ,y = y_liom_mean_grow, type = "l", col = "blue", lwd = 4,
     xlab = "Log of the Volume of Cacti year t", ylab = "Log of the Volume of Cacti Year t+1")
for(i in 1:samp){
  lines(x = size_dummy, y = (grow_extract$Beta0_2[i] + size_dummy * grow_extract$Beta1_2[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy  ,y = y_liom_mean_grow, type = "l", col = "blue", lwd = 4)
points(x = log(y_liom_subset_grow$volume_t), y = log(y_liom_subset_grow$volume_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3))
# Vacant (4)s
plot(x = size_dummy  ,y = y_vac_mean_grow, type = "l", col = "pink", lwd = 4,
     xlab = "Log of the Volume of Cacti year t", ylab = "Log of the Volume of Cacti Year t+1") 
for(i in 1:samp){
  lines(x = size_dummy, y = (grow_extract$Beta0_4[i] + size_dummy * grow_extract$Beta1_4[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy  ,y = y_vac_mean_grow, type = "l", col = "pink", lwd = 4)
points(x = log(y_vac_subset_grow$volume_t), y = log(y_vac_subset_grow$volume_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) + 
  polygon(c(size_dummy,rev(size_dummy)),c(y_vac_high_grow, rev(y_vac_low_grow)),
          col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3), border = NA)
# All together
plot(x = size_dummy  ,y = y_other_mean_grow, type = "l",lwd = 2,
     xlab = "Log of the Volume of Cacti year t", ylab = "Log of the Volume of Cacti Year t+1",
     ylim = c(8.8,9.5), xlim = c(8.8,9.2)) + 
  lines(x = size_dummy, y = y_crem_mean_grow, type = "l", col = "red", lwd = 2) + 
  lines(x = size_dummy, y = y_liom_mean_grow, type = "l", col = "blue",lwd = 2) + 
  lines(x = size_dummy, y = y_vac_mean_grow, type = "l", col = "pink", lwd = 2) + 
  abline(a = 1, b = 1, col = "darkgrey", lty = 2)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c("black","red","blue","pink"), pch = 16)
dev.off()



#### Survival Visuals #####################################################################################################
## Extract & Format Data
#overlay plot data
y <- y_surv
yrep_surv <- rstan::extract(fit_surv_mix_ant, pars = "y_rep")[["y_rep"]]
samp100 <- sample(nrow(yrep_surv), 500)
#extract from STAN Models
extract_surv <- rstan::extract(fit_surv_mix_ant, pars = c("beta0","beta1"))
beta0 <- as.data.frame(extract_surv$beta0)
beta1 <- as.data.frame(extract_surv$beta1)
surv_extract <- cbind(beta0,beta1)
colnames(surv_extract) <- c("Beta0_1","Beta0_2","Beta0_3","Beta0_4", "Beta1_1","Beta1_2","Beta1_3","Beta1_4")
#format for gg plots y rep
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
#extract from original ddata
y_subset <- survival_data[,c("Survival_t1","ant", "volume_t")]
## Formulas
y_other_surv = quantile(surv_extract$Beta0_3,0.5) + size_dummy * quantile(surv_extract$Beta1_3,0.5)
y_other_low_surv = quantile(surv_extract$Beta0_3,0.05) + size_dummy * quantile(surv_extract$Beta1_3,0.05)
y_other_high_surv = quantile(surv_extract$Beta0_3,0.95) + size_dummy * quantile(surv_extract$Beta1_3,0.95)
y_other_subset_surv <- subset(y_subset, ant == 3)
y_crem_surv = quantile(surv_extract$Beta0_1,0.5) + size_dummy * quantile(surv_extract$Beta1_1,0.5)
y_crem_low_surv = quantile(surv_extract$Beta0_1,0.05) + size_dummy * quantile(surv_extract$Beta1_1,0.05)
y_crem_high_surv = quantile(surv_extract$Beta0_1,0.95) + size_dummy * quantile(surv_extract$Beta1_1,0.95)
y_crem_subset_surv <- subset(y_subset, ant == 1)
y_liom_surv = quantile(surv_extract$Beta0_2,0.5) + size_dummy * quantile(surv_extract$Beta1_2,0.5)
y_liom_low_surv = quantile(surv_extract$Beta0_2,0.05) + size_dummy * quantile(surv_extract$Beta1_2,0.05)
y_liom_high_surv = quantile(surv_extract$Beta0_2,0.95) + size_dummy * quantile(surv_extract$Beta1_2,0.95)
y_liom_subset_surv <- subset(y_subset, ant == 2)
y_vac_surv = quantile(surv_extract$Beta0_4,0.5) + size_dummy * quantile(surv_extract$Beta1_4,0.5)
y_vac_low_surv = quantile(surv_extract$Beta0_4,0.05) + size_dummy * quantile(surv_extract$Beta1_4,0.05)
y_vac_high_surv = quantile(surv_extract$Beta0_4,0.95) + size_dummy * quantile(surv_extract$Beta1_4,0.95)
y_vac_subset_surv <- subset(y_subset, ant == 4)
## Overlay Plots
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
png("surv_panels1.png")
par(mar=c(2,2,2,2))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(1,2,2), widths = c(4,4,4))
plot.new()
text(0.5,0.1,"Survival Rates of Cacti by Ant State",cex=2,font=2)
# Other (3)
plot(x = size_dummy  ,y = invlogit(y_other_surv), type = "l", col = "black", lwd = 4, ylim = c(0,1))
points(x = log(y_other_subset_surv$volume_t), y = y_other_subset_surv$Survival_t1, col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.4))
#lines(x = size_dummy, y = invlogit(y_other_low_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_other_high_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(y_other_high_surv), rev(invlogit(y_other_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Crem (1)
plot(x = size_dummy  ,y = invlogit(y_crem_surv), type = "l", col = "red", lwd = 4, ylim = c(0,1))
points(x = log(y_crem_subset_surv$volume_t), y = y_crem_subset_surv$Survival_t1, col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.4))
#lines(x = size_dummy, y = invlogit(y_crem_low_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_crem_high_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(y_crem_high_surv), rev(invlogit(y_crem_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Liom (2)
plot(x = size_dummy  ,y = invlogit(y_liom_surv), type = "l", col = "blue", lwd = 4, ylim = c(0,1))
points(x = log(y_liom_subset_surv$volume_t), y = y_liom_subset_surv$Survival_t1, col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.4))
#lines(x = size_dummy, y = invlogit(y_liom_low_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_liom_high_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)# Vacant
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(y_liom_high_surv), rev(invlogit(y_liom_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# vacant (4)
plot(x = size_dummy  ,y = invlogit(y_vac_surv), type = "l", col = "pink", lwd = 4, ylim = c(0,1))
points(x = log(y_vac_subset_surv$volume_t), y = y_vac_subset_surv$Survival_t1, col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.4))
#lines(x = size_dummy, y = invlogit(y_vac_low_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_vac_high_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(y_vac_high_surv), rev(invlogit(y_vac_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# All Together
plot(x = size_dummy, y = invlogit(y_other_surv), type = "l", col = "black", lwd = 2, ylim = c(0,1))
lines(x = size_dummy, y = invlogit(y_crem_surv), col = "red",lwd = 2)
lines(x = size_dummy, y = invlogit(y_liom_surv), col = "blue", lwd = 2)
lines(x = size_dummy, y = invlogit(y_vac_surv), col = "pink", lwd = 2)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c("black","red","blue","pink"), pch = 16)
dev.off()
## GG Plots
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
## Panels 2
png("surv_panel2.png")
par(mar=c(5,5,0,1))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(0.5,1.5,1.5), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.5,"Survival Rates of Cacti by Ant State",cex=2,font=2)
# Other (3)
samp <- sample(nrow(surv_extract), 50)
plot(x = size_dummy  ,y = invlogit(y_other_surv), type = "l", col = "black", lwd = 4, ylim = c(0,1))
for(i in 1:50){
  lines(x = size_dummy, y = invlogit(surv_extract$Beta0_3[i] + size_dummy * surv_extract$Beta1_3[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy  ,y = invlogit(y_other_surv), type = "l", col = "black", lwd = 4)
points(x = log(y_other_subset_surv$volume_t), y = (y_other_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
# Crem (1)
plot(x = size_dummy  ,y = invlogit(y_crem_surv), type = "l", col = "red", lwd = 4, ylim = c(0,1),
     xlab = "Log of the Volume of Cacti year t", ylab = "Log of the Volume of Cacti Year t+1") 
for(i in 1:50){
  lines(x = size_dummy, y = invlogit(surv_extract$Beta0_1[i] + size_dummy * surv_extract$Beta1_1[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy  ,y = invlogit(y_crem_surv), type = "l", col = "red", lwd = 4)
points(x = log(y_crem_subset_surv$volume_t), y = (y_crem_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
# Liom (2)
plot(x = size_dummy  ,y = invlogit(y_liom_surv), type = "l", col = "blue", lwd = 4, ylim = c(0,1),
     xlab = "Log of the Volume of Cacti year t", ylab = "Log of the Volume of Cacti Year t+1")
for(i in 1:50){
  lines(x = size_dummy, y = invlogit(surv_extract$Beta0_2[i] + size_dummy * surv_extract$Beta1_2[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy  ,y = invlogit(y_liom_surv), type = "l", col = "blue", lwd = 4)
points(x = log(y_liom_subset_surv$volume_t), y = (y_liom_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3))
# Vacant (4)s
plot(x = size_dummy  ,y = invlogit(y_vac_surv), type = "l", col = "pink", lwd = 4, ylim = c(0,1),
     xlab = "Log of the Volume of Cacti year t", ylab = "Log of the Volume of Cacti Year t+1") 
for(i in 1:50){
  lines(x = size_dummy, y = invlogit(surv_extract$Beta0_4[i] + size_dummy * surv_extract$Beta1_4[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy  ,y = invlogit(y_vac_surv), type = "l", col = "pink", lwd = 4)
points(x = log(y_vac_subset_surv$volume_t), y = (y_vac_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) + 
# All together
plot(x = size_dummy  ,y = invlogit(y_other_surv), type = "l",lwd = 2,
     xlab = "Log of the Volume of Cacti year t", ylab = "Log of the Volume of Cacti Year t+1")
  lines(x = size_dummy, y = invlogit(y_crem_surv), type = "l", col = "red", lwd = 2)
  lines(x = size_dummy, y = invlogit(y_liom_surv), type = "l", col = "blue",lwd = 2) 
  lines(x = size_dummy, y = invlogit(y_vac_surv), type = "l", col = "pink", lwd = 2)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c("black","red","blue","pink"), pch = 16)
dev.off()


#### Flowering Visuals #####################################################################################################
## Extract & Format Data
#extract from StAN models
flow_data <- read.csv("/Users/alicampbell/Cactus Dropbox/Ant-Demography-Project/Model Outputs/flow_outputs.csv", header = TRUE,stringsAsFactors=T)
#format for overlay plots
y <- y_flow
yrep_flow <- subset(flow_data, select = -c(1:3))
samp100 <- sample(nrow(yrep_flow), 500)
## Formulas
y_flow = quantile(flow_data$beta0,0.5) + size_dummy * quantile(flow_data$beta1,0.5)
y_low_flow = quantile(flow_data$beta0,0.05) + size_dummy * quantile(flow_data$beta1,0.05)
y_high_flow = quantile(flow_data$beta0,0.95) + size_dummy * quantile(flow_data$beta1,0.95)
## Overlay Plots
png(file = "flow_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_flow[samp100,])
dev.off()
## Convergence Plots
png(file = "flow_conv1")
bayesplot::mcmc_trace(As.mcmc.list(fit_flow_mix_ant, pars=c("beta0", "beta1")))
dev.off()
##Panel Plot
png("flow_panels1.png")
par(mar=c(5,5,0,1))
layout(matrix(c(1,2),
              ncol = 1, byrow = TRUE), heights = c(1.2,2))
plot.new()
text(0.5,0.1,"Number of Flowers Produced",cex=2,font=2)
plot(x = size_dummy  ,y = exp(y_flow), type = "l", col = "chartreuse4", lwd = 4, ylim = c(0,60), xlim = c(-5,15))
points(x = log(flower_data$volume_t), y = (flower_data$TotFlowerbuds_t),col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.6))
lines(x = size_dummy, y = exp(y_low_flow), type = "l", col = "darkgrey", lty = 2, lwd = 2)
lines(x = size_dummy, y = exp(y_high_flow), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_dummy,rev(size_dummy)),c(exp(y_high_flow), rev(exp(y_low_flow))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
dev.off()
png("flow_panels2.png")
par(mar=c(2,2,2,2))
layout(matrix(c(1,2),
              ncol = 1, byrow = TRUE), heights = c(1,2))
plot.new()
text(0.5,0.1,"Total Number of Flowers Produced",cex=2,font=2)
plot(x = size_dummy  ,y = exp(y_flow), type = "l", col = "chartreuse4", lwd = 4, xlim = c(-5,15), ylim = c(0,100))
for(i in 1:1500){
  lines(x = size_dummy, y = exp(flow_data$beta0[i] + size_dummy * flow_data$beta1[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy  ,y = exp(y_flow), type = "l", col = "chartreuse4", lwd = 4)
points(x = log(flower_data$volume_t), y = (flower_data$TotFlowerbuds_t),col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.6))
dev.off()

#### Reproductive Visuals #####################################################################################################
## Extract & Format Data
#format for overlay plots
#extract from STAN models
extract_repro <- rstan::extract(fit_repro_mix_ant, pars = c("beta0","beta1"))
beta0 <- as.data.frame(extract_repro$beta0)
beta1 <- as.data.frame(extract_repro$beta1)
repro_extract <- cbind(beta0,beta1)
colnames(repro_extract) <- c("Beta0","Beta1")

repro_data <- read.csv("/Users/alicampbell/Cactus Dropbox/Ant-Demography-Project/Model Outputs/repro_outputs.csv", header = TRUE,stringsAsFactors=T)
#format for overlay plots
y <- y_repro
yrep_repro <- subset(repro_data, select = -c(1:3))
samp100 <- sample(nrow(yrep_repro), 500)


#extract from original data
y_subset <- repro_data[,c("flower1_YN","ant", "volume_t")]
## Formulas
size_dummy2 <- seq(min(log(repro_data$volume_t)),max(log(repro_data$volume_t)), by = 0.1)
y_repro = quantile(repro_extract$Beta0,0.5) + size_dummy2 * quantile(repro_extract$Beta1,0.5)
y_low_repro = quantile(repro_extract$Beta0,0.05) + size_dummy2 * quantile(repro_extract$Beta1,0.05)
y_high_repro = quantile(repro_extract$Beta0,0.95) + size_dummy2 * quantile(repro_extract$Beta1,0.95)
## Overlay Plots
png(file = "repro_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_repro[samp100,])
dev.off()
## Convergence Plots
png(file = "repro_conv1.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_repro_mix_ant, pars=c("beta0", "beta1")))
dev.off()
## Panel Plots
png("repro_panel1.png")
par(mar=c(5,5,0,1))
layout(matrix(c(1,2),
              ncol = 1, byrow = TRUE), heights = c(1.2,2))
plot.new()
text(0.5,0.1,"Probability of Reproducing",cex=2,font=2)
plot(x = (size_dummy2)  ,y = invlogit(y_repro), type = "l", col = "chartreuse4",  lwd = 4, ylim = c(0,1))
points(x = log(repro_data$volume_t), y =(repro_data$flower1_YN))
lines(x = (size_dummy2), y = invlogit(y_low_repro), type = "l", col = "darkgrey", lty = 2, lwd = 2)
lines(x = (size_dummy2), y = invlogit(y_high_repro), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c((size_dummy2),rev((size_dummy2))),c(invlogit(y_high_repro), rev(invlogit(y_low_repro))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
dev.off()
png("repro_panel2.png")
par(mar=c(5,5,0,1))
layout(matrix(c(1,2),
              ncol = 1, byrow = TRUE), heights = c(1.2,2))
plot.new()
text(0.5,0.1,"Probability of Reproducing",cex=2,font=2)
plot(x = (size_dummy2)  ,y = invlogit(y_repro), type = "l", col = "chartreuse4", ylim = c(0,1), lwd = 4, xlab = "log of volume at year t", ylab = "probability of producing flowers")
points(x = log(y_subset$volume_t), y = (y_subset$flower1_YN))
for(i in 200:1000){
  lines(x = (size_dummy2), y = invlogit(repro_extract$Beta0[i] + size_dummy2 * repro_extract$Beta1[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = (size_dummy2), y = invlogit(y_repro), type = "l", col = "chartreuse4", lwd = 4)
dev.off()

## Stan Plots
png("repro_stan_dist.png")
stan_plot(fit_repro_mix_ant, pars = c("beta0","beta1"), 
          point_est = "mean", show_density = TRUE)
dev.off()



#### Viability Visuals #####################################################################################################
## Extract & Format Data
#format for overlay plots
y_good <- good
yrep_viab <- rstan::extract(fit_viab_mix_ant, pars = "y_rep")[["y_rep"]]
samp100 <- sample(nrow(yrep_viab), 500)
#extract from STAN models
extract_viab <- rstan::extract(fit_viab_mix_ant, pars = c("beta0","beta1"))
beta0 <- as.data.frame(extract_viab$beta0)
beta1 <- as.data.frame(extract_viab$beta1)
viab_extract <- cbind(beta0,beta1)
colnames(viab_extract) <- c("Beta0_1","Beta0_2","Beta0_3","Beta0_4","Beta1_1","Beta1_2","Beta1_3","Beta1_4")
#extract from original data
y_subset_good <- viability_data[,c("Goodbuds_t1","ant", "volume_t")]
## Formulas
y_other_viab = quantile(viab_extract$Beta0_3,0.5) + size_dummy * quantile(viab_extract$Beta1_3,0.5)
y_other_low_viab = quantile(viab_extract$Beta0_3,0.05) + size_dummy * quantile(viab_extract$Beta1_3,0.05)
y_other_high_viab = quantile(viab_extract$Beta0_3,0.95) + size_dummy * quantile(viab_extract$Beta1_3,0.95)
y_other_subset_viab = subset(y_subset, ant == 3)
y_crem_viab = quantile(viab_extract$Beta0_1,0.5) + size_dummy * quantile(viab_extract$Beta1_1,0.5)
y_crem_low_viab = quantile(viab_extract$Beta0_1,0.05) + size_dummy * quantile(viab_extract$Beta1_1,0.05)
y_crem_high_viab = quantile(viab_extract$Beta0_1,0.95) + size_dummy * quantile(viab_extract$Beta1_1,0.95)
y_crem_subset_viab = subset(y_subset, ant == 1)
y_liom_viab = quantile(viab_extract$Beta0_2,0.5) + size_dummy * quantile(viab_extract$Beta1_2,0.5)
y_liom_low_viab = quantile(viab_extract$Beta0_2,0.05) + size_dummy * quantile(viab_extract$Beta1_2,0.05)
y_liom_high_viab = quantile(viab_extract$Beta0_2,0.95) + size_dummy * quantile(viab_extract$Beta1_2,0.95)
y_liom_subset_viab = subset(y_subset, ant == 2)
y_vac_viab = quantile(viab_extract$Beta0_4,0.5) + size_dummy * quantile(viab_extract$Beta1_4,0.5)
y_vac_low_viab = quantile(viab_extract$Beta0_4,0.05) + size_dummy * quantile(viab_extract$Beta1_4,0.05)
y_vac_high_viab = quantile(viab_extract$Beta0_4,0.95) + size_dummy * quantile(viab_extract$Beta1_4,0.95)
y_vac_subset_viab = subset(y_subset, ant == 4)
#total flowers info
extract_flow <- rstan::extract(fit_flow_mix_ant, pars = c("beta0","beta1"))
beta0 <- as.data.frame(extract_flow$beta0)
beta1 <- as.data.frame(extract_flow$beta1)
flow_extract <- cbind(beta0,beta1)
colnames(flow_extract) <- c("Beta0","Beta1")
#format for overlay plots
y_tot <- y_flow
yrep_flow <- rstan::extract(fit_flow_mix_ant, pars = "y_rep")[["y_rep"]]
samp100 <- sample(nrow(yrep_flow), 500)
## Formulas
y_flow = quantile(flow_extract$Beta0,0.5) + size_dummy * quantile(flow_extract$Beta1,0.5)
y_low_flow = quantile(flow_extract$Beta0,0.05) + size_dummy * quantile(flow_extract$Beta1,0.05)
y_high_flow = quantile(flow_extract$Beta0,0.95) + size_dummy * quantile(flow_extract$Beta1,0.95)
## Subsets
subset <- cactus[,c("volume_t","TotFlowerbuds_t1","Goodbuds_t1","ant_t")]
other_subset <- subset(subset, ant_t == "other")
crem_subset <- subset(subset, ant == "crem")
liom_subset <- subset(subset, ant == "liom")
vac_subset <- subset(subset, ant == "vacant")
## Overlay Plots
png(file = "viab_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_viab[samp100,])
dev.off()
## Convergence Plots
png(file = "viab_conv1")
bayesplot::mcmc_trace(As.mcmc.list(fit_viab_mix_ant, pars=c("beta0")))
dev.off()
## Convergence Plots
png(file = "viab_conv1")
bayesplot::mcmc_trace(As.mcmc.list(fitty, pars=c("beta0")))
dev.off()
## Panel Plots (proportion of viable buds)
plot(x = log(cactus$volume_t),y = cactus$Goodbuds_t1/cactus$TotFlowerbuds_t1)
png("viab_panels1.png")
par(mar=c(2,2,2,2))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(0.5,1.5,1.5), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.5,"Proportion of Viable Buds by Ant State",cex=2,font=2)
# Other
plot(x = size_dummy  ,y = invlogit((y_other_viab)/(y_flow)), type = "l", col = "black", lwd = 4)
#points(x = log(subset$volume_t), y = invlogit(other_subset$Goodbuds_t1/subset$TotFlowerbuds_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.4))
#lines(x = size_dummy, y = invlogit(y_other_low_viab), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_other_high_viab), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(y_other_high_viab/y_flow), rev(invlogit(y_other_low_viab/y_flow))),col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Crem
plot(x = size_dummy  ,y = invlogit(y_crem_viab/y_flow), type = "l", col = "red", lwd = 4, ylim = c(0,1))
#points(x = log(y_crem_subset_viab$volume_t), y = invlogit(y_crem_subset_viab$Goodbuds_t), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.4))
#lines(x = size_dummy, y = invlogit(y_crem_low_viab), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_crem_high_viab), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(y_crem_high_viab/y_flow), rev(invlogit(y_crem_low_viab/y_flow))),col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Liom
plot(x = size_dummy  ,y = (invlogit(y_liom_viab/y_flow)), type = "l", col = "blue", lwd = 4, ylim = c(0,1))
#points(x = log(y_liom_subset_viab$volume_t), y = invlogit(y_liom_subset_viab$Goodbuds_t), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.4))
#lines(x = size_dummy, y = invlogit(y_liom_low_viab), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_liom_high_viab), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(y_liom_high_viab/y_flow), rev(invlogit(y_liom_low_viab/y_flow))),col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Vacant
plot(x = size_dummy  ,y = (invlogit(y_vac_viab/y_flow)), type = "l", col = "pink", lwd = 4, ylim = c(0,1), xlim = c(0,15))
#points(x = log(y_vac_subset_viab$volume_t), y = invlogit(y_vac_subset_viab$Goodbuds_t), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.4))
#lines(x = size_dummy, y = invlogit(y_vac_low_viab), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_vac_high_viab), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(y_vac_high_viab/y_flow), rev(invlogit(y_vac_low_viab/y_flow))),col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
#All Together
plot(x = size_dummy  ,y = invlogit(y_other_viab/y_flow), type = "l", col = "black", lwd = 2, ylim = c(0,1))
lines(x = size_dummy  ,y = invlogit(y_crem_viab/y_flow), type = "l", col = "red", lwd = 2)
lines(x = size_dummy  ,y = invlogit(y_liom_viab/y_flow), type = "l", col = "blue", lwd = 2)
lines(x = size_dummy  ,y = invlogit(y_vac_viab/y_flow), type = "l", col = "pink", lwd = 2)
dev.off()
## Panels 2
png("viab_panel2.png")
par(mar=c(2,2,2,2))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(0.5,1.5,1.5), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.5,"Viability of Flowerbuds by Ant State",cex=2,font=2)
# Other (3)
samp <- sample(nrow(viab_extract), 150)
plot(x = size_dummy  ,y = invlogit(y_other_viab/y_flow), type = "l", col = "black", lwd = 4, ylim = c(0,1))
for(i in 1:1500){
  lines(x = size_dummy, y = invlogit((viab_extract$Beta0_3[samp[i]] + size_dummy * viab_extract$Beta1_3[samp[i]])/y_flow),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy  ,y = invlogit(y_other_viab/y_flow), type = "l", col = "black", lwd = 4)
#points(x = log(y_other_subset_viab$volume_t), y = invlogit(y_other_subset_viab$Goodbuds_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
# Crem (1)
plot(x = size_dummy  ,y = invlogit(y_crem_viab/y_flow), type = "l", col = "red", lwd = 4,
     xlab = "Log of the Volume of Cacti year t", ylab = "Log of the Volume of Cacti Year t+1", ylim = c(0,1)) 
for(i in 1:1500){
  lines(x = size_dummy, y = invlogit((viab_extract$Beta0_1[samp[i]] + size_dummy * viab_extract$Beta1_1[samp[i]])/y_flow),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy, y = invlogit((mean(viab_extract$Beta0_1) + size_dummy * mean(viab_extract$Beta1_1))/y_flow),col = "lightgrey", alpha = 0.1)
lines(x = size_dummy  ,y = invlogit(y_crem_viab/y_flow), type = "l", col = "red", lwd = 4)
#points(x = log(y_crem_subset_viab$volume_t), y = invlogit(y_crem_subset_viab$Goodbuds_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
# Liom (2)
plot(x = size_dummy  ,y = invlogit(y_liom_viab/y_flow), type = "l", col = "blue", lwd = 4, ylim = c(0,1),
     xlab = "Log of the Volume of Cacti year t", ylab = "Log of the Volume of Cacti Year t+1")
for(i in 1:1500){
  lines(x = size_dummy, y = invlogit((viab_extract$Beta0_2[samp[i]] + size_dummy * viab_extract$Beta1_2[samp[i]])/y_flow),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy  ,y = invlogit(y_liom_viab/y_flow), type = "l", col = "blue", lwd = 4)
#points(x = log(y_liom_subset_viab$volume_t), y = invlogit(y_liom_subset_viab$Goodbuds_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3))
# Vacant (4)s
plot(x = size_dummy  ,y = invlogit(y_vac_viab/y_flow), type = "l", col = "pink", lwd = 4, ylim = c(0,1),
     xlab = "Log of the Volume of Cacti year t", ylab = "Log of the Volume of Cacti Year t+1") 
for(i in 1:1500){
  lines(x = size_dummy, y = invlogit((viab_extract$Beta0_4[samp[i]] + size_dummy * viab_extract$Beta1_4[samp[i]])/y_flow),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy  ,y = invlogit(y_vac_viab/y_flow), type = "l", col = "pink", lwd = 4)
#points(x = log(y_vac_subset_viab$volume_t), y = invlogit(y_vac_subset_viab$Goodbuds_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) + 
  # All together
  plot(x = size_dummy  ,y = invlogit(y_other_viab/y_flow), type = "l",lwd = 2, ylim = c(0,1),
       xlab = "Log of the Volume of Cacti year t", ylab = "Log of the Volume of Cacti Year t+1")
lines(x = size_dummy, y = invlogit(y_crem_viab/y_flow), type = "l", col = "red", lwd = 2)
lines(x = size_dummy, y = invlogit(y_liom_viab/y_flow), type = "l", col = "blue",lwd = 2) 
lines(x = size_dummy, y = invlogit(y_vac_viab/y_flow), type = "l", col = "pink", lwd = 2)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c("black","red","blue","pink"), pch = 16)
dev.off()




#### Viability and Total together #####################################################################################################

par(mar=c(2,2,2,2))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(0.5,1.5,1.5), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.1,"Viable and Total Flowerbuds",cex=2,font=2)
#other
plot(x = size_dummy  ,y = exp(y_flow), type = "l", col = "chartreuse4", lwd = 4, ylim = c(0,60), xlim = c(-5,15))
points(x = log(flower$volume_t), y = (flower$TotFlowerbuds_t),col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.6))
lines(x = size_dummy, y = exp(y_low_flow), type = "l", col = "darkgrey", lty = 2, lwd = 2)
lines(x = size_dummy, y = exp(y_high_flow), type = "l", col = "darkgrey", lty = 2, lwd = 2)
lines(x = size_dummy  ,y = exp(y_other_viab), type = "l", col = "black", lwd = 4)
polygon(c(size_dummy,rev(size_dummy)),c(exp(y_high_flow), rev(exp(y_low_flow))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
#crem
plot(x = size_dummy  ,y = exp(y_flow), type = "l", col = "chartreuse4", lwd = 4, ylim = c(0,60), xlim = c(-5,15))
points(x = log(flower$volume_t), y = (flower$TotFlowerbuds_t),col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.6))
lines(x = size_dummy, y = exp(y_low_flow), type = "l", col = "darkgrey", lty = 2, lwd = 2)
lines(x = size_dummy, y = exp(y_high_flow), type = "l", col = "darkgrey", lty = 2, lwd = 2)
lines(x = size_dummy  ,y = exp(y_crem_viab), type = "l", col = "red", lwd = 4)
polygon(c(size_dummy,rev(size_dummy)),c(exp(y_high_flow), rev(exp(y_low_flow))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
#liom
plot(x = size_dummy  ,y = exp(y_flow), type = "l", col = "chartreuse4", lwd = 4, ylim = c(0,60), xlim = c(-5,15))
points(x = log(flower$volume_t), y = (flower$TotFlowerbuds_t),col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.6))
lines(x = size_dummy, y = exp(y_low_flow), type = "l", col = "darkgrey", lty = 2, lwd = 2)
lines(x = size_dummy, y = exp(y_high_flow), type = "l", col = "darkgrey", lty = 2, lwd = 2)
lines(x = size_dummy  ,y = exp(y_liom_viab), type = "l", col = "blue", lwd = 4)
polygon(c(size_dummy,rev(size_dummy)),c(exp(y_high_flow), rev(exp(y_low_flow))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
#vacant
plot(x = size_dummy  ,y = exp(y_flow), type = "l", col = "chartreuse4", lwd = 4, ylim = c(0,60), xlim = c(-5,15))
points(x = log(flower$volume_t), y = (flower$TotFlowerbuds_t),col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.6))
lines(x = size_dummy, y = exp(y_low_flow), type = "l", col = "darkgrey", lty = 2, lwd = 2)
lines(x = size_dummy, y = exp(y_high_flow), type = "l", col = "darkgrey", lty = 2, lwd = 2)
lines(x = size_dummy  ,y = exp(y_vac_viab), type = "l", col = "pink", lwd = 4)
polygon(c(size_dummy,rev(size_dummy)),c(exp(y_high_flow), rev(exp(y_low_flow))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
#all
plot(x = size_dummy  ,y = exp(y_other_viab), type = "l", col = "black", lwd = 2, ylim = c(0,60))
lines(x = size_dummy  ,y = exp(y_crem_viab), type = "l", col = "red", lwd = 2)
lines(x = size_dummy  ,y = exp(y_liom_viab), type = "l", col = "blue", lwd = 2)
lines(x = size_dummy  ,y = exp(y_vac_viab), type = "l", col = "pink", lwd = 2)
lines(x = size_dummy, y = exp(y_flow), type = "l", col = "chartreuse4", lwd = 2)

#### Multinomial 1 #####################################################################################################
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




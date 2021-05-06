setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")

size_dummy <- seq(min(log(cactus$volume_t), na.rm = TRUE), max(log(cactus$volume_t), na.rm = TRUE), by = 0.1)
#### Growth Visuals #####################################################################################################
## Extract & Format Data
#For overlay plots
y <- y_grow
grow_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow_outputs.csv", header = TRUE,stringsAsFactors=T)
yrep_grow <- grow_yrep
samp100 <- sample(nrow(yrep_grow), 500)
#extract from original data
y_subset <- growth_data[,c("volume_t1","ant", "volume_t")]
## Formulas
y_other_mean_grow <- quantile(grow_data$beta0.3,0.5) + size_dummy * quantile(grow_data$beta1.3,0.5)
y_other_low_grow <- quantile(grow_data$beta0.3,0.05) + size_dummy * quantile(grow_data$beta1.3,0.05)
y_other_high_grow <- quantile(grow_data$beta0.3,0.95) + size_dummy * quantile(grow_data$beta1.3,0.95)
y_other_subset_grow <- subset(y_subset, ant == 3)
y_crem_mean_grow <- quantile(grow_data$beta0.1,0.5) + size_dummy * quantile(grow_data$beta1.1,0.5)
y_crem_low_grow <- quantile(grow_data$beta0.1,0.05) + size_dummy * quantile(grow_data$beta1.1,0.05)
y_crem_high_grow <- quantile(grow_data$beta0.1,0.95) + size_dummy * quantile(grow_data$beta1.1,0.95)
y_crem_subset_grow <- subset(y_subset, ant == 1)
y_liom_mean_grow <- quantile(grow_data$beta0.2,0.5) + size_dummy * quantile(grow_data$beta1.2,0.5)
y_liom_low_grow <- quantile(grow_data$beta0.2,0.05) + size_dummy * quantile(grow_data$beta1.2,0.05)
y_liom_high_grow <- quantile(grow_data$beta0.2,0.95) + size_dummy * quantile(grow_data$beta1.2,0.95)
y_liom_subset_grow <- subset(y_subset, ant == 2)
y_vac_mean_grow <- quantile(grow_data$beta0.4,0.5) + size_dummy * quantile(grow_data$beta1.4,0.5)
y_vac_low_grow <- quantile(grow_data$beta0.4,0.05) + size_dummy * quantile(grow_data$beta1.4,0.05)
y_vac_high_grow <- quantile(grow_data$beta0.4,0.95) + size_dummy * quantile(grow_data$beta1.4,0.95)
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
## Panel Plots
png("grow_panel.png")
par(mar=c(2,2,2,2),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(0.5,1.5,1.5), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.5,"Growth Rates of Cacti \nof by Ant State and Size",cex=2,font=2)
# Other (3)
plot(x = size_dummy  ,y = y_other_mean_grow, type = "l", col = "black", lwd = 4,
     xlab = "", ylab = "") + 
  #lines(x = size_dummy, y = y_other_high_grow, type = "l", col = "darkgrey", lty = 2, lwd = 2) + 
  #lines(x = size_dummy, y = y_other_low_grow, type = "l", col = "darkgrey", lty = 2, lwd = 2) + 
  points(x = log(y_other_subset_grow$volume_t), y = log(y_other_subset_grow$volume_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) + 
  polygon(c(size_dummy,rev(size_dummy)),c(y_other_high_grow, rev(y_other_low_grow)),
          col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3), border = NA)
# Crem (1)
plot(x = size_dummy  ,y = y_crem_mean_grow, type = "l", col = "red", lwd = 4,
     xlab = "", ylab = "") + 
  #lines(x = size_dummy, y = y_crem_high_grow, type = "l", col = "darkgrey", lty = 2, lwd = 2) + 
  #lines(x = size_dummy, y = y_crem_low_grow, type = "l", col = "darkgrey", lty = 2, lwd = 2) + 
  points(x = log(y_crem_subset_grow$volume_t), y = log(y_crem_subset_grow$volume_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) + 
  polygon(c(size_dummy,rev(size_dummy)),c(y_crem_high_grow, rev(y_crem_low_grow)),
          col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3), border = NA)
# Liom (2)
plot(x = size_dummy  ,y = y_liom_mean_grow, type = "l", col = "blue", lwd = 4,
     xlab = "", ylab = "") + 
  #lines(x = size_dummy, y = y_liom_high_grow, type = "l", col = "darkgrey", lty = 2, lwd = 2) + 
  #lines(x = size_dummy, y = y_liom_low_grow, type = "l", col = "darkgrey", lty = 2, lwd = 2) + 
  points(x = log(y_liom_subset_grow$volume_t), y = log(y_liom_subset_grow$volume_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) + 
  polygon(c(size_dummy,rev(size_dummy)),c(y_liom_high_grow, rev(y_liom_low_grow)),
          col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3), border = NA)
# Vacant (4)s
plot(x = size_dummy  ,y = y_vac_mean_grow, type = "l", col = "pink", lwd = 4,
     xlab = "", ylab = "") + 
  #lines(x = size_dummy, y = y_vac_high_grow, type = "l", col = "darkgrey", lty = 2, lwd = 2) + 
  #lines(x = size_dummy, y = y_vac_low_grow, type = "l", col = "darkgrey", lty = 2, lwd = 2) + 
  points(x = log(y_vac_subset_grow$volume_t), y = log(y_vac_subset_grow$volume_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) + 
  polygon(c(size_dummy,rev(size_dummy)),c(y_vac_high_grow, rev(y_vac_low_grow)),
          col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3), border = NA)
# All together
plot(x = size_dummy  ,y = y_other_mean_grow, type = "l",lwd = 2,
     xlab = "", ylab = "",
     ylim = c(8.8,9.5), xlim = c(8.8,9.2)) + 
  lines(x = size_dummy, y = y_crem_mean_grow, type = "l", col = "red", lwd = 2) + 
  lines(x = size_dummy, y = y_liom_mean_grow, type = "l", col = "blue",lwd = 2) + 
  lines(x = size_dummy, y = y_vac_mean_grow, type = "l", col = "pink", lwd = 2) + 
  abline(a = 1, b = 1, col = "darkgrey", lty = 2)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c("black","red","blue","pink"), pch = 16)
mtext("Log(Volume) year 1",side=1,line=0,outer=TRUE,cex=1.3)
mtext("Log(Volume) year t+1",side=2,line=0,outer=TRUE,cex=1.3,las=0)
dev.off()
## Panels 2
png("grow_panel2.png")
par(mar=c(5,5,0,2),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(0.9,1.5,1.5), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.5,"Growth Rates of Cacti \nof by Ant State and Size",cex=2,font=2)
# Other (3)
samp <- sample(nrow(grow_data), 500)
plot(x = size_dummy  ,y = y_other_mean_grow, type = "l", col = "black", lwd = 4,
     xlab = "", ylab = "") 
for(i in 1:samp){
  lines(x = size_dummy, y = (grow_data$beta0.3[i] + size_dummy * grow_data$beta1.3[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy  ,y = y_other_mean_grow, type = "l", col = "black", lwd = 4)
points(x = log(y_other_subset_grow$volume_t), y = log(y_other_subset_grow$volume_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
# Crem (1)
plot(x = size_dummy  ,y = y_crem_mean_grow, type = "l", col = "red", lwd = 4,
     xlab = "", ylab = "") 
for(i in 1:samp){
  lines(x = size_dummy, y = (grow_data$beta0.1[i] + size_dummy * grow_data$beta1.1[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy  ,y = y_crem_mean_grow, type = "l", col = "red", lwd = 4)
points(x = log(y_crem_subset_grow$volume_t), y = log(y_crem_subset_grow$volume_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
# Liom (2)
plot(x = size_dummy  ,y = y_liom_mean_grow, type = "l", col = "blue", lwd = 4,
     xlab = "", ylab = "")
for(i in 1:samp){
  lines(x = size_dummy, y = (grow_data$beta0.2[i] + size_dummy * grow_data$beta1.2[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy  ,y = y_liom_mean_grow, type = "l", col = "blue", lwd = 4)
points(x = log(y_liom_subset_grow$volume_t), y = log(y_liom_subset_grow$volume_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3))
# Vacant (4)s
plot(x = size_dummy  ,y = y_vac_mean_grow, type = "l", col = "pink", lwd = 4,
     xlab = "", ylab = "") 
for(i in 1:samp){
  lines(x = size_dummy, y = (grow_data$beta0.4[i] + size_dummy * grow_data$beta1.4[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy  ,y = y_vac_mean_grow, type = "l", col = "pink", lwd = 4)
points(x = log(y_vac_subset_grow$volume_t), y = log(y_vac_subset_grow$volume_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) + 
  polygon(c(size_dummy,rev(size_dummy)),c(y_vac_high_grow, rev(y_vac_low_grow)),
          col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3), border = NA)
# All together
plot(x = size_dummy  ,y = y_other_mean_grow, type = "l",lwd = 2,
     xlab = "", ylab = "",
     ylim = c(8.8,9.5), xlim = c(8.8,9.2)) + 
  lines(x = size_dummy, y = y_crem_mean_grow, type = "l", col = "red", lwd = 2) + 
  lines(x = size_dummy, y = y_liom_mean_grow, type = "l", col = "blue",lwd = 2) + 
  lines(x = size_dummy, y = y_vac_mean_grow, type = "l", col = "pink", lwd = 2) + 
  abline(a = 1, b = 1, col = "darkgrey", lty = 2)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c("black","red","blue","pink"), pch = 16)
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.3)
mtext("Log(Volume) year t+1",side=2,line=0,outer=TRUE,cex=1.3,las=0)
dev.off()
## Make distribution plots
plot(density(x = y_other_mean_grow), ylim = c(0,0.065), xlim = c(-10,20), xlab = "Log(Volume)", ylab = "Density")
lines(density(x = y_crem_mean_grow), col = "red")
lines(density(x = y_liom_mean_grow), col = "blue")
lines(density(x = y_vac_mean_grow), col = "pink")

dist_other <- grow_data$beta0.3 + mean(size_dummy) * grow_data$beta1.3
dist_crem <- grow_data$beta0.1 + mean(size_dummy) * grow_data$beta1.1
dist_liom <- grow_data$beta0.2 + mean(size_dummy) * grow_data$beta1.2
dist_vac <- grow_data$beta0.4 + mean(size_dummy) * grow_data$beta1.4
dist_other_h <- grow_data$beta0.3 + quantile(size_dummy, 0.95) * grow_data$beta1.3
dist_crem_h <- grow_data$beta0.1 + quantile(size_dummy, 0.95) * grow_data$beta1.1
dist_liom_h <- grow_data$beta0.2 + quantile(size_dummy, 0.95) * grow_data$beta1.2
dist_vac_h <- grow_data$beta0.4 + quantile(size_dummy, 0.95) * grow_data$beta1.4
dist_other_l <- grow_data$beta0.3 + quantile(size_dummy, 0.05) * grow_data$beta1.3
dist_crem_l <- grow_data$beta0.1 + quantile(size_dummy, 0.05) * grow_data$beta1.1
dist_liom_l <- grow_data$beta0.2 + quantile(size_dummy, 0.05) * grow_data$beta1.2
dist_vac_l <- grow_data$beta0.4 + quantile(size_dummy, 0.05) * grow_data$beta1.4

png("grow_dist1.png")
par(mar=c(2,2,2,2),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4),
              ncol = 3, byrow = TRUE), heights = c(0.5,2.5), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.5,"Distributions of Growth Rates by Ant",cex=2,font=2)
#par(mfrow = c(1,3))
plot(density(dist_other_l), xlab = "Growth Rate \nof for Small Cacti",ylab = "Density", main = "",xlim = c(-4,-1), ylim = c(0,4))
lines(density(dist_crem_l), col = "red")
lines(density(dist_liom_l), col = "blue")
lines(density(dist_vac_l), col = "pink")
plot(density(dist_other), xlab = "Growth Rate \nof for Medium Cacti",ylab = "", main = "", xlim = c(4.8,6.3), ylim = c(0,5.5))
lines(density(dist_crem), col = "red")
lines(density(dist_liom), col = "blue")
lines(density(dist_vac), col = "pink")
plot(density(dist_other_h), xlab = "Growth Rate \nof for Large Cacti",ylab = "", main = "", xlim = c(13,14.4), ylim = c(0,5))
lines(density(dist_crem_h), col = "red")
lines(density(dist_liom_h), col = "blue")
lines(density(dist_vac_h), col = "pink")
dev.off()

#### Survival Visuals #####################################################################################################
## Extract & Format Data
#overlay plot data
y <- y_surv
surv_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/surv_outputs.csv", header = TRUE,stringsAsFactors=T)
yrep_surv <- subset(surv_data, select = -c(1:9))
samp100 <- sample(nrow(yrep_surv), 500)
#extract from original ddata
y_subset <- survival_data[,c("Survival_t1","ant", "volume_t")]
## Formulas
y_other_surv = quantile(surv_data$beta0.3,0.5) + size_dummy * quantile(surv_data$beta1.3,0.5)
y_other_low_surv = quantile(surv_data$beta0.3,0.05) + size_dummy * quantile(surv_data$beta1.3,0.05)
y_other_high_surv = quantile(surv_data$beta0.3,0.95) + size_dummy * quantile(surv_data$beta1.3,0.95)
y_other_subset_surv <- subset(y_subset, ant == 3)
y_crem_surv = quantile(surv_data$beta0.1,0.5) + size_dummy * quantile(surv_data$beta1.1,0.5)
y_crem_low_surv = quantile(surv_data$beta0.1,0.05) + size_dummy * quantile(surv_data$beta1.1,0.05)
y_crem_high_surv = quantile(surv_data$beta0.1,0.95) + size_dummy * quantile(surv_data$beta1.1,0.95)
y_crem_subset_surv <- subset(y_subset, ant == 1)
y_liom_surv = quantile(surv_data$beta0.2,0.5) + size_dummy * quantile(surv_data$beta1.2,0.5)
y_liom_low_surv = quantile(surv_data$beta0.2,0.05) + size_dummy * quantile(surv_data$beta1.2,0.05)
y_liom_high_surv = quantile(surv_data$beta0.2,0.95) + size_dummy * quantile(surv_data$beta1.2,0.95)
y_liom_subset_surv <- subset(y_subset, ant == 2)
y_vac_surv = quantile(surv_data$beta0.4,0.5) + size_dummy * quantile(surv_data$beta1.4,0.5)
y_vac_low_surv = quantile(surv_data$Bebeta0.4ta0_4,0.05) + size_dummy * quantile(surv_data$beta1.4,0.05)
y_vac_high_surv = quantile(surv_data$beta0.4,0.95) + size_dummy * quantile(surv_data$beta1.4,0.95)
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
text(0.5,0.1,"Survival Rates of Cacti \nof by Ant State and Size",cex=2,font=2)
# Other (3)
plot(x = size_dummy  ,y = invlogit(y_other_surv), type = "l", col = "black", lwd = 4, ylim = c(0,1))
points(x = log(y_other_subset_surv$volume_t), y = y_other_subset_surv$Survival_t1, col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.4))
#lines(x = size_dummy, y = invlogit(y_other_low_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_other_high_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(y_other_high_surv), rev(invlogit(y_other_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Crem (1)
plot(x = size_dummy  ,y = invlogit(y_crem_surv), type = "l", col = "red", lwd = 4, ylim = c(0,1))
points(x = log(y_crem_subset_surv$volume_t), y = (y_crem_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.4))
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
par(mfrow = c(1,1))

## Panels 2
png("surv_panel2.png")
par(mar=c(5,5,0,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(0.5,1.5,1.5), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.5,"Survival Rates of Cacti \nof by Ant State and Size",cex=2,font=2)
# Other (3)
samp <- sample(nrow(surv_data), 50)
plot(x = size_dummy  ,y = invlogit(y_other_surv), type = "l", col = "black", lwd = 4, ylim = c(0,1),xlab = "",ylab = "")
for(i in 1:500){
  lines(x = size_dummy, y = invlogit(surv_data$beta0.3[i] + size_dummy * surv_data$beta1.3[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy  ,y = invlogit(y_other_surv), type = "l", col = "black", lwd = 4)
points(x = log(y_other_subset_surv$volume_t), y = (y_other_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
# Crem (1)
plot(x = size_dummy  ,y = invlogit(y_crem_surv), type = "l", col = "red", lwd = 4, ylim = c(0,1),xlab = "",ylab = "") 
for(i in 1:500){
  lines(x = size_dummy, y = invlogit(surv_data$beta0.1[i] + size_dummy * surv_data$beta1.1[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy  ,y = invlogit(y_crem_surv), type = "l", col = "red", lwd = 4)
points(x = log(y_crem_subset_surv$volume_t), y = (y_crem_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
# Liom (2)
plot(x = size_dummy  ,y = invlogit(y_liom_surv), type = "l", col = "blue", lwd = 4, ylim = c(0,1),xlab = "",ylab = "")
for(i in 1:500){
  lines(x = size_dummy, y = invlogit(surv_data$beta0.2[i] + size_dummy * surv_data$beta1.2[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy  ,y = invlogit(y_liom_surv), type = "l", col = "blue", lwd = 4)
points(x = log(y_liom_subset_surv$volume_t), y = (y_liom_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3))
# Vacant (4)s
plot(x = size_dummy  ,y = invlogit(y_vac_surv), type = "l", col = "pink", lwd = 4, ylim = c(0,1),xlab = "",ylab = "") 
for(i in 1:500){
  lines(x = size_dummy, y = invlogit(surv_data$beta0.4[i] + size_dummy * surv_data$beta1.4[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy  ,y = invlogit(y_vac_surv), type = "l", col = "pink", lwd = 4)
points(x = log(y_vac_subset_surv$volume_t), y = (y_vac_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) + 
  # All together
  plot(x = size_dummy  ,y = invlogit(y_other_surv), type = "l",lwd = 2,xlab = "",ylab = "")
lines(x = size_dummy, y = invlogit(y_crem_surv), type = "l", col = "red", lwd = 2)
lines(x = size_dummy, y = invlogit(y_liom_surv), type = "l", col = "blue",lwd = 2) 
lines(x = size_dummy, y = invlogit(y_vac_surv), type = "l", col = "pink", lwd = 2)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c("black","red","blue","pink"), pch = 16)
mtext("Log(Volume)",side=1,line=0,outer=TRUE,cex=1.3)
mtext("Probability of Survival",side=2,line=0,outer=TRUE,cex=1.3,las=0)
dev.off()


#### Flowering Visuals #####################################################################################################
## Extract & Format Data
#extract from StAN models
flow_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/flow_outputs.csv", header = TRUE,stringsAsFactors=T)

size_dummy_3 <- seq(min(log(flower_data$volume_t), na.rm = TRUE), max(log(flower_data$volume_t), na.rm = TRUE), by = 0.1)
#format for overlay plots
y <- y_flow
yrep_flow <- flow_yrep
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
png("flow_panels3.png")
par(mar=c(5,5,0,1))
layout(matrix(c(1,2),
              ncol = 1, byrow = TRUE), heights = c(1.2,2))
plot.new()
text(0.5,0.1,"Number of Flowers Produced by Size",cex=2,font=2)
plot(x = size_dummy  ,y = exp(y_flow), type = "l", col = "chartreuse4", lwd = 4, ylim = c(0,100), xlab = "Log(Volume)",ylab = "Flowerbuds Produced")
points(x = log(flower_data$volume_t), y = (flower_data$TotFlowerbuds_t),col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.6))
#points( x = log(cactus$volume_t), y = cactus$TotFlowerbuds_t1, col = "red")
lines(x = size_dummy, y = exp(y_low_flow), type = "l", col = "darkgrey", lty = 2, lwd = 2)
lines(x = size_dummy, y = exp(y_high_flow), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_dummy,rev(size_dummy)),c(exp(y_high_flow), rev(exp(y_low_flow))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
dev.off()

png("flow_panels4.png")
par(mar=c(2,2,2,2))
layout(matrix(c(1,2),
              ncol = 1, byrow = TRUE), heights = c(1,2))
plot.new()
text(0.5,0.1,"Total Number of Flowers Produced by Size",cex=2,font=2)
plot(x = size_dummy  ,y = exp(y_flow), type = "l", col = "chartreuse4", lwd = 4, xlim = c(-5,15), ylim = c(0,100), xlab = "Log(Volume)",ylab = "Flowerbuds Produced")
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
repro_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/repro_outputs.csv", header = TRUE,stringsAsFactors=T)
y <- y_repro
yrep_repro <- subset(repro_data, select = -c(1:3))
samp100 <- sample(nrow(yrep_repro), 500)


#extract from original data
y_subset <- reproductive_data[,c("flower1_YN", "volume_t")]
## Formulas
size_dummy2 <- seq(min(log(reproductive_data$volume_t)),max(log(reproductive_data$volume_t)), by = 0.1)
y_repro = quantile(repro_data$beta0,0.5,na.rm = TRUE) + size_dummy2 * quantile(repro_data$beta1,0.5,na.rm = TRUE)
y_low_repro = quantile(repro_data$beta0,0.05) + size_dummy2 * quantile(repro_data$beta1,0.05)
y_high_repro = quantile(repro_data$beta0,0.95) + size_dummy2 * quantile(repro_data$beta1,0.95)
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
text(0.5,0.1,"Probability of Reproducing by Size",cex=2,font=2)
plot(x = (size_dummy2)  ,y = invlogit(y_repro), type = "l", col = "chartreuse4",ylim = c(0,1),  lwd = 4,xlab = "Log(Volume)",ylab = "Reproduction Rate")
points(x = log(reproductive_data$volume_t), y =(reproductive_data$flower1_YN))
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
text(0.5,0.1,"Probability of Reproducing by Size",cex=2,font=2)
plot(x = (size_dummy2)  ,y = invlogit(y_repro), type = "l", col = "chartreuse4", ylim = c(0,1), lwd = 4, xlab = "Log(Volume)", ylab = "Reproduction Rate")
points(x = log(y_subset$volume_t), y = (y_subset$flower1_YN))
for(i in 1:5000){
  lines(x = (size_dummy2), y = invlogit(repro_data$beta0[i] + size_dummy2 * repro_data$beta1[i]),col = "lightgrey", alpha = 0.1)
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
viab_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/viab_outputs.csv", header = TRUE,stringsAsFactors=T)
#format for overlay plots
y <- good_viab
yrep_viab <- viab_yrep
samp100 <- sample(nrow(yrep_viab), 500)
#extract from original data
y_subset_good <- viability_data[,c("Goodbuds_t1","ant", "volume_t")]
## Formulas
y_other_viab = quantile(viab_data$beta0.3,0.5)
y_other_low_viab = quantile(viab_data$beta0.3,0.05)
y_other_high_viab = quantile(viab_data$beta0.3,0.95)
y_other_subset_viab = subset(y_subset_good, ant == 3)
y_crem_viab = quantile(viab_data$beta0.1,0.5)
y_crem_low_viab = quantile(viab_data$beta0.1,0.05)
y_crem_high_viab = quantile(viab_data$beta0.1,0.95)
y_crem_subset_viab = subset(y_subset_good, ant == 1)
y_liom_viab = quantile(viab_data$beta0.2,0.5) 
y_liom_low_viab = quantile(viab_data$beta0.2,0.05)
y_liom_high_viab = quantile(viab_data$beta0.2,0.95)
y_liom_subset_viab = subset(y_subset_good, ant == 2)
y_vac_viab = quantile(viab_data$beta0.4,0.5)
y_vac_low_viab = quantile(viab_data$beta0.4,0.05)
y_vac_high_viab = quantile(viab_data$beta0.4,0.95) 
y_vac_subset_viab = subset(y_subset_good, ant == 4)
## Subsets
subset <- cactus[,c("volume_t","TotFlowerbuds_t1","Goodbuds_t1","ant_t")]
other_subset <- subset(subset, ant_t == "other")
crem_subset <- subset(subset, ant_t == "crem")
liom_subset <- subset(subset, ant_t == "liom")
vac_subset <- subset(subset, ant_t == "vacant")
###### Weird mix data
mixed <- as.data.frame(matrix(nrow = 4 * nrow(viab_data)))
for(i in 1:nrow(viab_data)){
  mixed$beta[i] <- viab_data$beta0.1[i]
  mixed$ant[i] <- "crem"
  mixed$low[i] <- y_other_low_viab
  mixed$high[i] <- y_other_high_viab
}
for(i in 1:nrow(viab_data)){
  mixed$beta[nrow(viab_data) + i] <- viab_data$beta0.2[i]
  mixed$ant[nrow(viab_data) + i] <- "liom"
  mixed$low[i] <- y_crem_low_viab
  mixed$high[i] <- y_crem_high_viab
}
for(i in 1:nrow(viab_data)){
  mixed$beta[2*nrow(viab_data) + i] <- viab_data$beta0.3[i]
  mixed$ant[2*nrow(viab_data) + i] <- "other"
  mixed$low[i] <- y_liom_low_viab
  mixed$high[i] <- y_liom_high_viab
}
for(i in 1:nrow(viab_data)){
  mixed$beta[3*nrow(viab_data) + i] <- viab_data$beta0.4[i]
  mixed$ant[3*nrow(viab_data) + i] <- "vacant"
  mixed$low[i] <- y_vac_low_viab
  mixed$high[i] <- y_vac_high_viab
}
## Overlay Plots
png(file = "viab_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_viab[samp100,])
dev.off()
png(file = "viab_ant_post1.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_viab[samp100,],group = ant_viab)
dev.off()
## Convergence Plots
png(file = "viab_conv1")
bayesplot::mcmc_trace(As.mcmc.list(fit_viab_mix_ant, pars=c("beta0")))
dev.off()
## Convergence Plots
png(file = "viab_conv1")
bayesplot::mcmc_trace(As.mcmc.list(fit_viab_mix_ant, pars=c("beta0")))
dev.off()
## Panel Plots (proportion of viable buds)
plot(x = log(other_subset$volume_t),y = other_subset$Goodbuds_t1/other_subset$TotFlowerbuds_t1)

means <- c(mean(viab_data$beta0.1), mean(viab_data$beta0.2), mean(viab_data$beta0.3), mean(viab_data$beta0.4))
ants <- c("crem","liom","crem","vacant")
sd <- c(sd(viab_data$beta0.1),sd(viab_data$beta0.2),sd(viab_data$beta0.3),sd(viab_data$beta0.4))
data <- data.frame(ants,means, sd)

png("viab_bars1.png")
ggplot(data = data, aes(x=(ants), y=invlogit(means))) +
  geom_bar(stat="identity", fill=c("red","blue","black","pink"), alpha = 0.9) +
  geom_errorbar(aes(x=factor(ants), ymin=invlogit(means-sd), ymax=invlogit(means+sd)), width=0.2, colour="grey", alpha=0.9, size=1) + 
  xlab("Ant Species") + ylab("Viability Rate") + labs(title = "Proportion of Viable Flowerbuds\nof  by Ant Species Present") + 
  theme_classic() + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, family = "Helvetica"), axis.text = element_text(face ="bold", hjust = 0.5, size = 12, family = "Helvetica"), axis.title = element_text(face = "bold", hjust = 0.5, size = 15, family = "Helvetica"), legend.text = element_text(hjust = 0.5, size = 12, family = "Helvetica"), legend.title = element_text(face = "bold", hjust = 0.5, size = 15, family = "Helvetica"), panel.border = element_rect(colour = "black", fill=NA, size=1))
dev.off()

png("viab_bars2.png")
plot(invlogit(mixed$beta) ~ factor(mixed$ant), xlab = "Ant Species",ylab = "Viability Rate", col = c("red","blue","black","pink"), alpha = 0.5)
dev.off()


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




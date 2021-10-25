##################################################################################################################
##
##              This file visualizes the outputs of the Bayesian Models created in the sourced code
##
##################################################################################################################
##################################################################################################################
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
source("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Data Analysis/Cholla_Analysis_Vital_Rates.R")
size_dummy <- seq(min(log(cactus$volume_t), na.rm = TRUE), max(log(cactus$volume_t), na.rm = TRUE), by = 0.1)
#########################################################################################################################
#### Growth Visuals #####################################################################################################
##########################################################################################################################################################################################################
## Extract & Format Data
#extract from original data

y_subset <- growth_data[,c("volume_t1","ant", "volume_t")]
y_crem_subset_grow <- subset(y_subset, ant == 1)
y_liom_subset_grow <- filter(y_subset, ant == 2)
y_vac_subset_grow <- filter(y_subset, ant == 4)
y_other_subset_grow <- filter(y_subset, ant == 3)
grow_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow_outputs.csv", header = TRUE,stringsAsFactors=T)
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
## Panel Plots
png("grow_panel.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
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
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(0.5,1.5,1.5), widths = c(3.9,3.9,3.9))
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
## Make Distribution Plots
dist_other <- mean(grow_data$beta0.3) + mean(size_dummy) * mean(grow_data$beta1.3)
dist_crem <- mean(grow_data$beta0.1) + mean(size_dummy) * mean(grow_data$beta1.1)
dist_liom <- mean(grow_data$beta0.2) + mean(size_dummy) * mean(grow_data$beta1.2)
dist_vac <- mean(grow_data$beta0.4) + mean(size_dummy) * mean(grow_data$beta1.4)
dist_other_h <- mean(grow_data$beta0.3) + quantile(size_dummy, 0.95) * mean(grow_data$beta1.3)
dist_crem_h <- mean(grow_data$beta0.1) + quantile(size_dummy, 0.95) * mean(grow_data$beta1.1)
dist_liom_h <- mean(grow_data$beta0.2) + quantile(size_dummy, 0.95) * mean(grow_data$beta1.2)
dist_vac_h <- mean(grow_data$beta0.4) + quantile(size_dummy, 0.95) * mean(grow_data$beta1.4)
dist_other_l <- mean(grow_data$beta0.3) + quantile(size_dummy, 0.25) * mean(grow_data$beta1.3)
dist_crem_l <- mean(grow_data$beta0.1) + quantile(size_dummy, 0.25) * mean(grow_data$beta1.1)
dist_liom_l <- mean(grow_data$beta0.2) + quantile(size_dummy, 0.25) * mean(grow_data$beta1.2)
dist_vac_l <- mean(grow_data$beta0.4) + quantile(size_dummy, 0.25) * mean(grow_data$beta1.4)
#### Plot the growth at year t1
png("grow_dist1b.png")
par(mar=c(2,2,2,2),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4),
              ncol = 3, byrow = TRUE), heights = c(0.3,1), widths = c(3.9,4,4))
plot.new()
text(0.5,0.1,"Distributions Next Sizes by Ant",cex=2,font=2)
#par(mfrow = c(1,3))
plot(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_other_l, sd = mean(grow_data$sigma)), type = "l", xlab = "Medium Cacti",ylab = "", main = "", xlim = c(-5,2))
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_crem_l, sd = mean(grow_data$sigma)), col = "red", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_liom_l, sd = mean(grow_data$sigma)), col = "blue", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_vac_l, sd = mean(grow_data$sigma)), col = "pink", lwd = 2)
abline(v = quantile(size_dummy, 0.05), col = "grey", lty = 2, lwd = 2)
plot(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_other, sd = mean(grow_data$sigma)), type = "l", xlab = "Medium Cacti",ylab = "", main = "", xlim = c(2,9))
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_crem, sd = mean(grow_data$sigma)), col = "red", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_liom, sd = mean(grow_data$sigma)), col = "blue", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_vac, sd = mean(grow_data$sigma)), col = "pink", lwd = 2)
abline(v = mean(size_dummy), col = "grey", lty = 2, lwd = 2)
plot(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_other_h, sd = mean(grow_data$sigma)), type = "l", xlab = "Medium Cacti",ylab = "", main = "", xlim = c(11,15))
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_crem_h, sd = mean(grow_data$sigma)), col = "red", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_liom_h, sd = mean(grow_data$sigma)), col = "blue", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_vac_h, sd = mean(grow_data$sigma)), col = "pink", lwd = 2)
abline(v = quantile(size_dummy, 0.95), col = "grey", lty = 2, lwd = 2)
dev.off()
####

##Density of Growth Rates
png("grow_dist3.png")
par(mar=c(2,2,2,2),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4),
              ncol = 3, byrow = TRUE), heights = c(0.5,2.5), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.5,"Distributions of Growth Rates by Ant",cex=2,font=2)
#par(mfrow = c(1,3))
plot(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_other_l/quantile(size_dummy, 0.25), sd = mean(grow_data$sigma)), type = "l", xlab = "Medium Cacti",ylab = "", main = "", xlim = c(0,15))
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_crem_l/quantile(size_dummy, 0.25), sd = mean(grow_data$sigma)), col = "red", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_liom_l/quantile(size_dummy, 0.25), sd = mean(grow_data$sigma)), col = "blue", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_vac_l/quantile(size_dummy, 0.25), sd = mean(grow_data$sigma)), col = "pink", lwd = 2)
abline(v = 1, col = "grey", lty = 2, lwd = 2)
plot(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_other/mean(size_dummy), sd = mean(grow_data$sigma)), type = "l", xlab = "Medium Cacti",ylab = "", main = "", xlim = c(-2,4))
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_crem/mean(size_dummy,), sd = mean(grow_data$sigma)), col = "red", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_liom/mean(size_dummy), sd = mean(grow_data$sigma)), col = "blue", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_vac/mean(size_dummy), sd = mean(grow_data$sigma)), col = "pink", lwd = 2)
abline(v = 1, col = "grey", lty = 2, lwd = 2)
plot(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_other_h/quantile(size_dummy, 0.95), sd = mean(grow_data$sigma)), type = "l", xlab = "Medium Cacti",ylab = "", main = "", xlim = c(-2,4))
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_crem_h/quantile(size_dummy, 0.95), sd = mean(grow_data$sigma)), col = "red", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_liom_h/quantile(size_dummy, 0.95), sd = mean(grow_data$sigma)), col = "blue", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_vac_h/quantile(size_dummy, 0.95), sd = mean(grow_data$sigma)), col = "pink", lwd = 2)
abline(v = 1, col = "grey", lty = 2, lwd = 2)
dev.off()

###########################################################################################################################
#### Survival Visuals #####################################################################################################
#########################################################################################################################
## Extract & Format Data
#extract from original ddata
surv_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/surv_outputs.csv", header = TRUE,stringsAsFactors=T)
y_subset <- survival_data[,c("Survival_t1","ant", "volume_t")]
y_crem_subset_surv <- subset(y_subset, ant == 1)
y_liom_subset_surv <- filter(y_subset, ant == 2)
y_vac_subset_surv <- filter(y_subset, ant == 4)
y_other_subset_surv <- filter(y_subset, ant == 3)
#Size Dummies for every ant
size_crem = seq(min(log(y_crem_subset_surv$volume_t), na.rm = TRUE), max (log(y_crem_subset_surv$volume_t), na.rm = TRUE), by = 0.1)
size_other = seq(min(log(y_other_subset_surv$volume_t), na.rm = TRUE), max (log(y_other_subset_surv$volume_t), na.rm = TRUE), by = 0.1)
size_liom = seq(min(log(y_liom_subset_surv$volume_t), na.rm = TRUE), max (log(y_liom_subset_surv$volume_t), na.rm = TRUE), by = 0.1)
size_vac = seq(min(log(y_vac_subset_surv$volume_t), na.rm = TRUE), max (log(y_vac_subset_surv$volume_t), na.rm = TRUE), by = 0.1)
## Formulas
y_other_surv = quantile(surv_data$beta0.3,0.5) + size_other * quantile(surv_data$beta1.3,0.5)
y_other_low_surv = quantile(surv_data$beta0.3,0.05) + size_other * quantile(surv_data$beta1.3,0.05)
y_other_high_surv = quantile(surv_data$beta0.3,0.95) + size_other * quantile(surv_data$beta1.3,0.95)
other_extr = quantile(surv_data$beta0.3,0.5) + size_dummy * quantile(surv_data$beta1.3,0.5)
y_other_subset_surv <- subset(y_subset, ant == 3)
y_crem_surv = quantile(surv_data$beta0.1,0.5) + size_crem * quantile(surv_data$beta1.1,0.5)
y_crem_low_surv = quantile(surv_data$beta0.1,0.05) + size_crem * quantile(surv_data$beta1.1,0.05)
y_crem_high_surv = quantile(surv_data$beta0.1,0.95) + size_crem * quantile(surv_data$beta1.1,0.95)
crem_extr = quantile(surv_data$beta0.1,0.5) + size_dummy * quantile(surv_data$beta1.1,0.5)
y_crem_subset_surv <- subset(y_subset, ant == 1)
y_liom_surv = quantile(surv_data$beta0.2,0.5) + size_liom * quantile(surv_data$beta1.2,0.5)
y_liom_low_surv = quantile(surv_data$beta0.2,0.05) + size_liom * quantile(surv_data$beta1.2,0.05)
y_liom_high_surv = quantile(surv_data$beta0.2,0.95) + size_liom * quantile(surv_data$beta1.2,0.95)
liom_extr = quantile(surv_data$beta0.2,0.5) + size_dummy * quantile(surv_data$beta1.2,0.5)
y_liom_subset_surv <- subset(y_subset, ant == 2)
y_vac_surv = quantile(surv_data$beta0.4,0.5) + size_vac * quantile(surv_data$beta1.4,0.5)
y_vac_low_surv = quantile(surv_data$Bebeta0.4ta0_4,0.05) + size_vac * quantile(surv_data$beta1.4,0.05)
y_vac_high_surv = quantile(surv_data$beta0.4,0.95) + size_vac * quantile(surv_data$beta1.4,0.95)
vac_extr = quantile(surv_data$beta0.4,0.5) + size_dummy * quantile(surv_data$beta1.4,0.5)
y_vac_subset_surv <- subset(y_subset, ant == 4)
## Panel Plots
png("surv_panels1.png")
par(mar=c(2,2,2,2))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(1,2,2), widths = c(4,4,4))
plot.new()
text(0.5,0.1,"Survival Rates of Cacti \nof by Ant State and Size",cex=2,font=2)
# Other (3)
plot(x = size_other  ,y = invlogit(y_other_surv), type = "l", col = "black", lwd = 4, ylim = c(0,1), xlim = c(-5,15))
points(x = log(y_other_subset_surv$volume_t), y = y_other_subset_surv$Survival_t1, col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.4))
#lines(x = size_dummy, y = invlogit(y_other_low_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_other_high_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_other,rev(size_other)),c(invlogit(y_other_high_surv), rev(invlogit(y_other_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Crem (1)
plot(x = size_crem  ,y = invlogit(y_crem_surv), type = "l", col = "red", lwd = 4, ylim = c(0,1), xlim = c(-5,15))
points(x = log(y_crem_subset_surv$volume_t), y = (y_crem_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.4))
#lines(x = size_dummy, y = invlogit(y_crem_low_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_crem_high_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_crem,rev(size_crem)),c(invlogit(y_crem_high_surv), rev(invlogit(y_crem_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Liom (2)
plot(x = size_liom  ,y = invlogit(y_liom_surv), type = "l", col = "blue", lwd = 4, ylim = c(0,1), xlim = c(-5,15))
points(x = log(y_liom_subset_surv$volume_t), y = y_liom_subset_surv$Survival_t1, col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.4))
#lines(x = size_dummy, y = invlogit(y_liom_low_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_liom_high_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)# Vacant
polygon(c(size_liom,rev(size_liom)),c(invlogit(y_liom_high_surv), rev(invlogit(y_liom_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# vacant (4)
plot(x = size_vac  ,y = invlogit(y_vac_surv), type = "l", col = "pink", lwd = 4, ylim = c(0,1), xlim = c(-5,15))
points(x = log(y_vac_subset_surv$volume_t), y = y_vac_subset_surv$Survival_t1, col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.4))
#lines(x = size_dummy, y = invlogit(y_vac_low_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_vac_high_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_vac,rev(size_vac)),c(invlogit(y_vac_high_surv), rev(invlogit(y_vac_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# All Together
plot(x = size_dummy, y = invlogit(other_extr), type = "l", col = "black", lwd = 2, ylim = c(0,1), lty = 2)
lines(x = size_dummy, y = invlogit(crem_extr), col = "red",lwd = 2, lty = 2)
lines(x = size_dummy, y = invlogit(liom_extr), col = "blue", lwd = 2, lty = 2)
lines(x = size_dummy, y = invlogit(vac_extr), col = "pink", lwd = 2, lty = 2)
lines(x = size_other, y = invlogit(y_other_surv), col = "black", lwd = 2)
lines(x = size_crem, y = invlogit(y_crem_surv), col = "red", lwd = 2)
lines(x = size_liom, y = invlogit(y_liom_surv), col = "blue", lwd = 2)
lines(x = size_vac, y = invlogit(y_vac_surv), col = "pink", lwd = 2)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c("black","red","blue","pink"), pch = 16)
dev.off()


par(mfrow = c(1,1))

## Panels 2
png("surv_panel2.png")
par(mar=c(5,5,0,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(1,2,2), widths = c(4,4,4))
plot.new()
text(0.5,0.5,"Survival Rates of Cacti \nof by Ant State and Size",cex=2,font=2)
# Other (3)
samp <- sample(nrow(surv_data), 50)
plot(x = size_other  ,y = invlogit(y_other_surv), type = "l", col = "black", lwd = 4, ylim = c(0,1),xlim = c(-5,15), xlab = "",ylab = "")
for(i in 1:1500){
  lines(x = size_other, y = invlogit(surv_data$beta0.3[i] + size_other * surv_data$beta1.3[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_other  ,y = invlogit(y_other_surv), type = "l", col = "black", lwd = 4)
points(x = log(y_other_subset_surv$volume_t), y = (y_other_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
# Crem (1)
plot(x = size_crem  ,y = invlogit(y_crem_surv), type = "l", col = "red", lwd = 4, ylim = c(0,1),xlim = c(-5,15),xlab = "",ylab = "") 
for(i in 1:1500){
  lines(x = size_crem, y = invlogit(surv_data$beta0.1[i] + size_crem * surv_data$beta1.1[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_crem  ,y = invlogit(y_crem_surv), type = "l", col = "red", lwd = 4)
points(x = log(y_crem_subset_surv$volume_t), y = (y_crem_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
# Liom (2)
plot(x = size_liom  ,y = invlogit(y_liom_surv), type = "l", col = "blue", lwd = 4, ylim = c(0,1),xlim = c(-5,15),xlab = "",ylab = "")
for(i in 1:1500){
  lines(x = size_liom, y = invlogit(surv_data$beta0.2[i] + size_liom * surv_data$beta1.2[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_liom  ,y = invlogit(y_liom_surv), type = "l", col = "blue", lwd = 4)
points(x = log(y_liom_subset_surv$volume_t), y = (y_liom_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3))
# Vacant (4)s
plot(x = size_vac  ,y = invlogit(y_vac_surv), type = "l", col = "pink", lwd = 4, ylim = c(0,1),xlim = c(-5,15),xlab = "",ylab = "") 
for(i in 1:1500){
  lines(x = size_vac, y = invlogit(surv_data$beta0.4[i] + size_vac * surv_data$beta1.4[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_vac  ,y = invlogit(y_vac_surv), type = "l", col = "pink", lwd = 4)
points(x = log(y_vac_subset_surv$volume_t), y = (y_vac_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) + 
  # All together
plot(x = size_dummy, y = invlogit(other_extr), type = "l", col = "black", lwd = 2, ylim = c(0,1), lty = 2, xlab = "", ylab = "")
lines(x = size_dummy, y = invlogit(crem_extr), col = "red",lwd = 2, lty = 2)
lines(x = size_dummy, y = invlogit(liom_extr), col = "blue", lwd = 2, lty = 2)
lines(x = size_dummy, y = invlogit(vac_extr), col = "pink", lwd = 2, lty = 2)
lines(x = size_other, y = invlogit(y_other_surv), col = "black", lwd = 2)
lines(x = size_crem, y = invlogit(y_crem_surv), col = "red", lwd = 2)
lines(x = size_liom, y = invlogit(y_liom_surv), col = "blue", lwd = 2)
lines(x = size_vac, y = invlogit(y_vac_surv), col = "pink", lwd = 2)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c("black","red","blue","pink"), pch = 16)
mtext("Log(Volume)",side=1,line=0,outer=TRUE,cex=1.3)
mtext("Probability of Survival",side=2,line=0,outer=TRUE,cex=1.3,las=0)
dev.off()

png("surv_panel3.png")
par(mfrow = c(2,2))
samp <- sample(nrow(surv_data), 50)
plot(x = size_other  ,y = invlogit(y_other_surv), type = "l", col = "black", lwd = 4, ylim = c(0,1),xlim = c(-5,15), xlab = "",ylab = "")
for(i in 1:1500){
  lines(x = size_other, y = invlogit(surv_data$beta0.3[i] + size_other * surv_data$beta1.3[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_other  ,y = invlogit(y_other_surv), type = "l", col = "black", lwd = 4)
points(x = log(y_other_subset_surv$volume_t), y = (y_other_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
# Crem (1)
plot(x = size_crem  ,y = invlogit(y_crem_surv), type = "l", col = "red", lwd = 4, ylim = c(0,1),xlim = c(-5,15),xlab = "",ylab = "") 
for(i in 1:1500){
  lines(x = size_crem, y = invlogit(surv_data$beta0.1[i] + size_crem * surv_data$beta1.1[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_crem  ,y = invlogit(y_crem_surv), type = "l", col = "red", lwd = 4)
points(x = log(y_crem_subset_surv$volume_t), y = (y_crem_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
# Liom (2)
plot(x = size_liom  ,y = invlogit(y_liom_surv), type = "l", col = "blue", lwd = 4, ylim = c(0,1),xlim = c(-5,15),xlab = "",ylab = "")
for(i in 1:1500){
  lines(x = size_liom, y = invlogit(surv_data$beta0.2[i] + size_liom * surv_data$beta1.2[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_liom  ,y = invlogit(y_liom_surv), type = "l", col = "blue", lwd = 4)
points(x = log(y_liom_subset_surv$volume_t), y = (y_liom_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3))
# Vacant (4)s
plot(x = size_vac  ,y = invlogit(y_vac_surv), type = "l", col = "pink", lwd = 4, ylim = c(0,1),xlim = c(-5,15),xlab = "",ylab = "") 
for(i in 1:1500){
  lines(x = size_vac, y = invlogit(surv_data$beta0.4[i] + size_vac * surv_data$beta1.4[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_vac  ,y = invlogit(y_vac_surv), type = "l", col = "pink", lwd = 4)
points(x = log(y_vac_subset_surv$volume_t), y = (y_vac_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) + 
dev.off()

#########################################################################################################################
#### Flowering Visuals #####################################################################################################
#########################################################################################################################
## Extract & Format Data
flow_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/flow_outputs.csv", header = TRUE,stringsAsFactors=T)
flow_data_trunc <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/flow_outputs_trunc.csv", header = TRUE,stringsAsFactors=T)
## Formulas
size_dummy_3 <- seq(min(log(flower_data$volume_t), na.rm = TRUE), max(log(flower_data$volume_t), na.rm = TRUE), by = 0.1)
#format for overlay plots
## Formulas
y_flow = quantile(flow_data$beta0,0.5) + size_dummy * quantile(flow_data$beta1,0.5)
y_low_flow = quantile(flow_data$beta0,0.05) + size_dummy * quantile(flow_data$beta1,0.05)
y_high_flow = quantile(flow_data$beta0,0.95) + size_dummy * quantile(flow_data$beta1,0.95)
y_flow_trunc = quantile(flow_data_trunc$beta0,0.5) + size_dummy * quantile(flow_data_trunc$beta1,0.5)
y_low_flow_trunc = quantile(flow_data_trunc$beta0,0.05) + size_dummy * quantile(flow_data_trunc$beta1,0.05)
y_high_flow_trunc = quantile(flow_data_trunc$beta0,0.95) + size_dummy * quantile(flow_data_trunc$beta1,0.95)
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
png("flow_panels_trunc3.png")
par(mar=c(5,5,0,1))
layout(matrix(c(1,2),
              ncol = 1, byrow = TRUE), heights = c(1.2,2))
plot.new()
text(0.5,0.1,"Number of Flowers Produced by Size",cex=2,font=2)
plot(x = size_dummy  ,y = exp(y_flow_trunc), type = "l", col = "chartreuse4", lwd = 4, ylim = c(0,100), xlab = "Log(Volume)",ylab = "Flowerbuds Produced")
points(x = log(flower_data$volume_t), y = (flower_data$TotFlowerbuds_t),col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.6))
#points( x = log(cactus$volume_t), y = cactus$TotFlowerbuds_t1, col = "red")
lines(x = size_dummy, y = exp(y_low_flow_trunc), type = "l", col = "darkgrey", lty = 2, lwd = 2)
lines(x = size_dummy, y = exp(y_high_flow_trunc), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_dummy,rev(size_dummy)),c(exp(y_high_flow_trunc), rev(exp(y_low_flow_trunc))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
dev.off()

png("flow_panels_trunc4.png")
par(mar=c(2,2,2,2))
layout(matrix(c(1,2),
              ncol = 1, byrow = TRUE), heights = c(1,2))
plot.new()
text(0.5,0.1,"Total Number of Flowers Produced by Size",cex=2,font=2)
plot(x = size_dummy  ,y = exp(y_flow_trunc), type = "l", col = "chartreuse4", lwd = 4, xlim = c(-5,15), ylim = c(0,100), xlab = "Log(Volume)",ylab = "Flowerbuds Produced")
for(i in 1:1500){
  lines(x = size_dummy, y = exp(flow_data_trunc$beta0[i] + size_dummy * flow_data_trunc$beta1[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_dummy  ,y = exp(y_flow_trunc), type = "l", col = "chartreuse4", lwd = 4)
points(x = log(flower_data$volume_t), y = (flower_data$TotFlowerbuds_t),col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.6))
dev.off()

#########################################################################################################################
#### Reproductive Visuals #####################################################################################################
#########################################################################################################################
## Extract & Format Data
#format for overlay plots
#extract from STAN models
#extract from original data
repro_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/repro_outputs.csv", header = TRUE,stringsAsFactors=T)
y_subset <- reproductive_data[,c("flower1_YN", "volume_t")]
## Formulas
size_dummy2 <- seq(min(log(reproductive_data$volume_t)),max(log(reproductive_data$volume_t)), by = 0.1)
y_repro = quantile(repro_data$beta0,0.5,na.rm = TRUE) + size_dummy2 * quantile(repro_data$beta1,0.5,na.rm = TRUE)
y_low_repro = quantile(repro_data$beta0,0.05) + size_dummy2 * quantile(repro_data$beta1,0.05)
y_high_repro = quantile(repro_data$beta0,0.95) + size_dummy2 * quantile(repro_data$beta1,0.95)
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


#########################################################################################################################
#### Viability Visuals #####################################################################################################
#########################################################################################################################
## Extract & Format Data
viab_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/viab_outputs.csv", header = TRUE,stringsAsFactors=T)
#format for overlay plots
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
  mixed$low[i] <- y_crem_low_viab
  mixed$high[i] <- y_crem_high_viab
}
for(i in 1:nrow(viab_data)){
  mixed$beta[nrow(viab_data) + i] <- viab_data$beta0.2[i]
  mixed$ant[nrow(viab_data) + i] <- "liom"
  mixed$low[i] <- y_liom_low_viab
  mixed$high[i] <- y_liom_high_viab
}
for(i in 1:nrow(viab_data)){
  mixed$beta[2*nrow(viab_data) + i] <- viab_data$beta0.3[i]
  mixed$ant[2*nrow(viab_data) + i] <- "other"
  mixed$low[i] <- y_other_low_viab
  mixed$high[i] <- y_other_high_viab
}
for(i in 1:nrow(viab_data)){
  mixed$beta[3*nrow(viab_data) + i] <- viab_data$beta0.4[i]
  mixed$ant[3*nrow(viab_data) + i] <- "vacant"
  mixed$low[i] <- y_vac_low_viab
  mixed$high[i] <- y_vac_high_viab
}
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

#########################################################################################################################
#### Seed Produced Visuals #####################################################################################################
#########################################################################################################################
seed_outputs <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/seed_outputs.csv", header = TRUE,stringsAsFactors=T)
y_crem <- seed_outputs$beta0.1
y_vac <- seed_outputs$beta0.3
y_liom <- seed_outputs$beta0.2
y_crem_l <- quantile(seed_outputs$beta0.1,0.05)
y_vac_l <- quantile(seed_outputs$beta0.3,0.05)
y_liom_l <- quantile(seed_outputs$beta0.2,0.05)
y_crem_h <- quantile(seed_outputs$beta0.1,0.95)
y_vac_h <- quantile(seed_outputs$beta0.3,0.95)
y_liom_h <- quantile(seed_outputs$beta0.2,0.95)

seeds <- as.data.frame(matrix(nrow = 3 * nrow(seed_outputs)))
for(i in 1:nrow(seed_outputs)){
  seeds$beta[i] <- seed_outputs$beta0.1[i]
  seeds$ant[i] <- "crem"
  seeds$low[i] <- y_crem_l
  seeds$high[i] <- y_crem_h
}
for(i in 1:nrow(seed_outputs)){
  seeds$beta[nrow(seed_outputs) + i] <- seed_outputs$beta0.2[i]
  seeds$ant[nrow(seed_outputs) + i] <- "liom"
  seeds$low[i] <- y_liom_l
  seeds$high[i] <- y_liom_h
}
for(i in 1:nrow(seed_outputs)){
  seeds$beta[2*nrow(seed_outputs) + i] <- seed_outputs$beta0.3[i]
  seeds$ant[2*nrow(seed_outputs) + i] <- "vacant"
  seeds$low[i] <- y_vac_l
  seeds$high[i] <- y_vac_h
}

png("seeds_bars.png")
plot(exp(seeds$beta) ~ factor(seeds$ant), xlab = "Ant Species",ylab = "Expected Seeds", col = c("red","blue","pink"), main = "Liom. Provides Advantages for Seed Prod.")
dev.off()
y <- seed$seed_count
samp100 <- sample(nrow(seed_yrep), 500)
png("seeds_post.png")
bayesplot::ppc_dens_overlay(y, seed_yrep[samp100,])
dev.off()


barplot(means, col = c("grey","white"), names.arg = c("vacant","occupied"), ylim = c(0,150))
arrows(x0 = 1, x1 = 1, y0 = low, y1 = high, angle = 90)

########################################################################################################
#### Seed Survival #################################################################################
###################################################################################################
y_subset <- (precensus.dat)
seed_surv_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/seed_surv_outputs.csv", header = TRUE,stringsAsFactors=T)
#Size Dummies for every ant
size_dummy = seq(min(precensus.dat$Log.size, na.rm = TRUE), max(precensus.dat$Log.size, na.rm = TRUE), by = 0.1)
## Formulas
y_surv = quantile(seed_surv_data$beta0,0.5) + size_dummy * quantile(seed_surv_data$beta1,0.5)
y_low_surv = quantile(seed_surv_data$beta0,0.05) + size_dummy * quantile(seed_surv_data$beta1,0.05)
y_high_surv = quantile(seed_surv_data$beta0,0.95) + size_dummy * quantile(seed_surv_data$beta1,0.95)
## Panel Plots
png("seed_surv_panels1.png")
plot(x = size_dummy  ,y = invlogit(y_surv), type = "l", col = "black", lwd = 4, ylim = c(0,1))
points(x = log(y_subset$volume_t), y = y_subset$Survival_t1, col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.4))
#lines(x = size_dummy, y = invlogit(y_other_low_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_other_high_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(y_high_surv), rev(invlogit(y_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
dev.off()

plot(x = size_dummy, y = invlogit(y_surv), ylim = c(0,1))

par(mfrow = c(1,1))

## Panels 2
png("surv_panel2.png")
par(mar=c(5,5,0,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(1,2,2), widths = c(4,4,4))
plot.new()
text(0.5,0.5,"Survival Rates of Cacti \nof by Ant State and Size",cex=2,font=2)
# Other (3)
samp <- sample(nrow(surv_data), 50)
plot(x = size_other  ,y = invlogit(y_other_surv), type = "l", col = "black", lwd = 4, ylim = c(0,1),xlim = c(-5,15), xlab = "",ylab = "")
for(i in 1:1500){
  lines(x = size_other, y = invlogit(surv_data$beta0.3[i] + size_other * surv_data$beta1.3[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_other  ,y = invlogit(y_other_surv), type = "l", col = "black", lwd = 4)
points(x = log(y_other_subset_surv$volume_t), y = (y_other_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
# Crem (1)
plot(x = size_crem  ,y = invlogit(y_crem_surv), type = "l", col = "red", lwd = 4, ylim = c(0,1),xlim = c(-5,15),xlab = "",ylab = "") 
for(i in 1:1500){
  lines(x = size_crem, y = invlogit(surv_data$beta0.1[i] + size_crem * surv_data$beta1.1[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_crem  ,y = invlogit(y_crem_surv), type = "l", col = "red", lwd = 4)
points(x = log(y_crem_subset_surv$volume_t), y = (y_crem_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
# Liom (2)
plot(x = size_liom  ,y = invlogit(y_liom_surv), type = "l", col = "blue", lwd = 4, ylim = c(0,1),xlim = c(-5,15),xlab = "",ylab = "")
for(i in 1:1500){
  lines(x = size_liom, y = invlogit(surv_data$beta0.2[i] + size_liom * surv_data$beta1.2[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_liom  ,y = invlogit(y_liom_surv), type = "l", col = "blue", lwd = 4)
points(x = log(y_liom_subset_surv$volume_t), y = (y_liom_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3))
# Vacant (4)s
plot(x = size_vac  ,y = invlogit(y_vac_surv), type = "l", col = "pink", lwd = 4, ylim = c(0,1),xlim = c(-5,15),xlab = "",ylab = "") 
for(i in 1:1500){
  lines(x = size_vac, y = invlogit(surv_data$beta0.4[i] + size_vac * surv_data$beta1.4[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_vac  ,y = invlogit(y_vac_surv), type = "l", col = "pink", lwd = 4)
points(x = log(y_vac_subset_surv$volume_t), y = (y_vac_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) + 
  # All together
  plot(x = size_dummy, y = invlogit(other_extr), type = "l", col = "black", lwd = 2, ylim = c(0,1), lty = 2, xlab = "", ylab = "")
lines(x = size_dummy, y = invlogit(crem_extr), col = "red",lwd = 2, lty = 2)
lines(x = size_dummy, y = invlogit(liom_extr), col = "blue", lwd = 2, lty = 2)
lines(x = size_dummy, y = invlogit(vac_extr), col = "pink", lwd = 2, lty = 2)
lines(x = size_other, y = invlogit(y_other_surv), col = "black", lwd = 2)
lines(x = size_crem, y = invlogit(y_crem_surv), col = "red", lwd = 2)
lines(x = size_liom, y = invlogit(y_liom_surv), col = "blue", lwd = 2)
lines(x = size_vac, y = invlogit(y_vac_surv), col = "pink", lwd = 2)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c("black","red","blue","pink"), pch = 16)
mtext("Log(Volume)",side=1,line=0,outer=TRUE,cex=1.3)
mtext("Probability of Survival",side=2,line=0,outer=TRUE,cex=1.3,las=0)
dev.off()

png("surv_panel3.png")
par(mfrow = c(2,2))
samp <- sample(nrow(surv_data), 50)
plot(x = size_other  ,y = invlogit(y_other_surv), type = "l", col = "black", lwd = 4, ylim = c(0,1),xlim = c(-5,15), xlab = "",ylab = "")
for(i in 1:1500){
  lines(x = size_other, y = invlogit(surv_data$beta0.3[i] + size_other * surv_data$beta1.3[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_other  ,y = invlogit(y_other_surv), type = "l", col = "black", lwd = 4)
points(x = log(y_other_subset_surv$volume_t), y = (y_other_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
# Crem (1)
plot(x = size_crem  ,y = invlogit(y_crem_surv), type = "l", col = "red", lwd = 4, ylim = c(0,1),xlim = c(-5,15),xlab = "",ylab = "") 
for(i in 1:1500){
  lines(x = size_crem, y = invlogit(surv_data$beta0.1[i] + size_crem * surv_data$beta1.1[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_crem  ,y = invlogit(y_crem_surv), type = "l", col = "red", lwd = 4)
points(x = log(y_crem_subset_surv$volume_t), y = (y_crem_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
# Liom (2)
plot(x = size_liom  ,y = invlogit(y_liom_surv), type = "l", col = "blue", lwd = 4, ylim = c(0,1),xlim = c(-5,15),xlab = "",ylab = "")
for(i in 1:1500){
  lines(x = size_liom, y = invlogit(surv_data$beta0.2[i] + size_liom * surv_data$beta1.2[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_liom  ,y = invlogit(y_liom_surv), type = "l", col = "blue", lwd = 4)
points(x = log(y_liom_subset_surv$volume_t), y = (y_liom_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3))
# Vacant (4)s
plot(x = size_vac  ,y = invlogit(y_vac_surv), type = "l", col = "pink", lwd = 4, ylim = c(0,1),xlim = c(-5,15),xlab = "",ylab = "") 
for(i in 1:1500){
  lines(x = size_vac, y = invlogit(surv_data$beta0.4[i] + size_vac * surv_data$beta1.4[i]),col = "lightgrey", alpha = 0.1)
}
lines(x = size_vac  ,y = invlogit(y_vac_surv), type = "l", col = "pink", lwd = 4)
points(x = log(y_vac_subset_surv$volume_t), y = (y_vac_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) + 
  dev.off()

################################################################################################################################################
##################### Germination Yr 1&2 Visuals ########################################################################
###############################################################################################################################################
germ1_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/germ1_outputs.csv", header = TRUE,stringsAsFactors=T)
trials_dummy <- seq(min(germ.dat$Input), max(germ.dat$Input), by = 0.1)
y_germ1 <- quantile(germ1_data$beta0, 0.5) + quantile(germ1_data$beta1, 0.5)
y_germ1_low <- quantile(germ1_data$beta0, 0.05) + quantile(germ1_data$beta1, 0.05)
y_germ1_high <- quantile(germ1_data$beta0, 0.95) + quantile(germ1_data$beta1, 0.95)
germ2_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/germ2_outputs.csv", header = TRUE,stringsAsFactors=T)
y_germ2 <- quantile(germ2_data$beta0, 0.5) + quantile(germ2_data$beta1, 0.5)

plot(c(invlogit(y_germ1),invlogit(y_germ2)), ylim = c(0,1))


################################################################################################################################################
##################### Recruit Sizes Visuals ########################################################################
###############################################################################################################################################
recruit_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/rec_outputs.csv", header = TRUE,stringsAsFactors=T)
y_rec = quantile(recruit_data$beta0,0.5)
y_rec_low = quantile(recruit_data$beta0,0.05)
y_rec_high = quantile(recruit_data$beta0,0.95)

plot(y_rec, ylim = c(0,0.25))
arrows(x0 = 1, x1 = 1, y0 = y_rec_low, y1 = y_rec_high, angle = 90)

################################################################################################################################################
##################### Binomial Visuals ########################################################################
###############################################################################################################################################
binom_occ <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/ant_outputs_occ.csv", header = TRUE,stringsAsFactors=T)
size_dummy_rec <- seq(min(log(ant.dat2$volume_t)), max(log(ant.dat2$volume_t)), by = 0.1)
occ_beta0 = median(binom_occ$beta0.1)
occ_beta1 = median(binom_occ$beta1.1)
vac_beta0 = median(binom_occ$beta0.2)
vac_beta2 = median(binom_occ$beta1.2)

y_occ <- occ_beta0 + occ_beta1*size_dummy_rec
y_vac <- quantile(binom_occ$beta0.2, 0.5) + quantile(binom_occ$beta1.2, 0.5)*size_dummy_rec

plot(size_dummy_rec, invlogit(y_occ), type = "l", col = "green", ylim = c(0,1))
lines(size_dummy_rec, invlogit(y_vac), col = "red")

################################################################################################################################################
##################### Multinomial Visuals ########################################################################
###############################################################################################################################################
multi_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/size_ant_outputs2.csv", header = TRUE,stringsAsFactors=T)
head(multi_data)



par(mfrow = c(2,2))
plot(x_vol,p_o_o, type = "n", ylim = c(0,1), main = "From Other to ...", xlab = "size", ylab = "Next Ant")
lines(x_vol, p_o_o, col = "pink")
lines(x_vol, p_o_l, col = "blue")
lines(x_vol, p_o_c, col = "red")
lines(x_vol, p_o_v, col = "black")

plot(x_vol,p_l_o, type = "n", ylim = c(0,1), main = "From Liom to ...", xlab = "size", ylab = "Next Ant")
lines(x_vol, p_l_o, col = "pink")
lines(x_vol, p_l_l, col = "blue")
lines(x_vol, p_l_c, col = "red")
lines(x_vol, p_l_v, col = "black")

plot(x_vol,p_c_o, type = "n", ylim = c(0,1), main = "From Crem to ...", xlab = "size", ylab = "Next Ant")
lines(x_vol, p_c_o, col = "pink")
lines(x_vol, p_c_l, col = "blue")
lines(x_vol, p_c_c, col = "red")
lines(x_vol, p_c_v, col = "black")

plot(x_vol,p_v_o, type = "n", ylim = c(0,1), main = "From Vacant to ...", xlab = "size", ylab = "Next Ant")
lines(x_vol, p_v_o, col = "pink")
lines(x_vol, p_v_l, col = "blue")
lines(x_vol, p_v_c, col = "red")
lines(x_vol, p_v_v, col = "black")


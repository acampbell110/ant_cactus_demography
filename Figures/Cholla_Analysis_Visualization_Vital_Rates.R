##################################################################################################################
##
##              This file visualizes the outputs of the Bayesian Models created in the sourced code
##
##################################################################################################################
##################################################################################################################
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
source( "/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Cholla_Analysis_Vital_Rates.R")

str(cactus)
##### Size variable used in most visualizations
size_dummy <- seq(min(cactus$logsize_t, na.rm = TRUE), max(cactus$logsize_t, na.rm = TRUE), by = 0.1)

########################################################################################################
#### Herbivores Visuals ################################################################################
########################################################################################################
str(cactus_herb)
cactus_herb <- na.omit(cactus_herb)
summary(cactus_herb$NP_juv)
summary(cactus_herb$MA)
cactus_herb$herb <- 0
for(i in 1:nrow(cactus_herb)){
if(cactus_herb$NP_juv[i] > 0 | cactus_herb$MA[i] > 0){
  cactus_herb$herb[i] <- 1
}
}
barplot(table(cactus_herb$herb[],cactus_herb$ant_t), col = c("pink","orange"))
table(cactus_herb$herb, cactus_herb$ant_t)


#########################################################################################################################
#### Growth Visuals #####################################################################################################
##########################################################################################################################################################################################################
## Extract & Format Data
growth_data_orig <- cactus[,c("Plot","Year_t","logsize_t","logsize_t1","ant_t")]
growth_data <- na.omit(growth_data_orig)
#extract from original data

y_subset <- growth_data[,c("logsize_t1","ant_t", "logsize_t")]
y_crem_subset_grow <- subset(y_subset, ant_t == "crem")
y_liom_subset_grow <- subset(y_subset, ant_t == "liom")
y_vac_subset_grow <- subset(y_subset, ant_t == "vacant")
y_other_subset_grow <- subset(y_subset, ant_t == "other")
grow_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow_outputs.csv", header = TRUE,stringsAsFactors=T)
## Formulas
y_other_mean_grow <- quantile(grow_out$beta0.3,0.5) + size_dummy * quantile(grow_out$beta1.3,0.5)
y_other_low_grow <- quantile(grow_out$beta0.3,0.05) + size_dummy * quantile(grow_out$beta1.3,0.05)
y_other_high_grow <- quantile(grow_out$beta0.3,0.95) + size_dummy * quantile(grow_out$beta1.3,0.95)
y_crem_mean_grow <- quantile(grow_out$beta0.1,0.5) + size_dummy * quantile(grow_out$beta1.1,0.5)
y_crem_low_grow <- quantile(grow_out$beta0.1,0.05) + size_dummy * quantile(grow_out$beta1.1,0.05)
y_crem_high_grow <- quantile(grow_out$beta0.1,0.95) + size_dummy * quantile(grow_out$beta1.1,0.95)
y_liom_mean_grow <- quantile(grow_out$beta0.2,0.5) + size_dummy * quantile(grow_out$beta1.2,0.5)
y_liom_low_grow <- quantile(grow_out$beta0.2,0.05) + size_dummy * quantile(grow_out$beta1.2,0.05)
y_liom_high_grow <- quantile(grow_out$beta0.2,0.95) + size_dummy * quantile(grow_out$beta1.2,0.95)
y_vac_mean_grow <- quantile(grow_out$beta0.4,0.5) + size_dummy * quantile(grow_out$beta1.4,0.5)
y_vac_low_grow <- quantile(grow_out$beta0.4,0.05) + size_dummy * quantile(grow_out$beta1.4,0.05)
y_vac_high_grow <- quantile(grow_out$beta0.4,0.95) + size_dummy * quantile(grow_out$beta1.4,0.95)
## Panels 2
png("grow_panel.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(0.7,1.4,1.4), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.5,"Growth Rates of Cacti \nof by Ant State and Size",cex=2,font=2)
# Other (3)
samp <- sample(nrow(grow_data), 500)
plot(x = size_dummy  ,y = y_other_mean_grow, type = "l", col = "black", lwd = 4,
     xlab = "", ylab = "") 
for(i in 1:samp){
  lines(x = size_dummy, y = (grow_data$beta0.3[i] + size_dummy * grow_data$beta1.3[i]),col = "lightgrey", alpha = 0.1)
}
points(x = (y_other_subset_grow$logsize_t), y = (y_other_subset_grow$logsize_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
lines(x = size_dummy  ,y = y_other_mean_grow, type = "l", col = "black", lwd = 4)

# Crem (1)
plot(x = size_dummy  ,y = y_crem_mean_grow, type = "l", col = "red", lwd = 4,
     xlab = "", ylab = "") 
for(i in 1:samp){
  lines(x = size_dummy, y = (grow_data$beta0.1[i] + size_dummy * grow_data$beta1.1[i]),col = "lightgrey", alpha = 0.1)
}
points(x = (y_crem_subset_grow$logsize_t), y = (y_crem_subset_grow$logsize_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
lines(x = size_dummy  ,y = y_crem_mean_grow, type = "l", col = "red", lwd = 4)
# Liom (2)
plot(x = size_dummy  ,y = y_liom_mean_grow, type = "l", col = "blue", lwd = 4,
     xlab = "", ylab = "")
for(i in 1:samp){
  lines(x = size_dummy, y = (grow_data$beta0.2[i] + size_dummy * grow_data$beta1.2[i]),col = "lightgrey", alpha = 0.1)
}
points(x = (y_liom_subset_grow$logsize_t), y = (y_liom_subset_grow$logsize_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3))
lines(x = size_dummy  ,y = y_liom_mean_grow, type = "l", col = "blue", lwd = 4)
# Vacant (4)s
plot(x = size_dummy  ,y = y_vac_mean_grow, type = "l", col = "pink", lwd = 4,
     xlab = "", ylab = "") 
for(i in 1:samp){
  lines(x = size_dummy, y = (grow_data$beta0.4[i] + size_dummy * grow_data$beta1.4[i]),col = "lightgrey", alpha = 0.1)
}
points(x = (y_vac_subset_grow$logsize_t), y = (y_vac_subset_grow$logsize_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3))
lines(x = size_dummy  ,y = y_vac_mean_grow, type = "l", col = "pink", lwd = 4)
# All together
plot(x = size_dummy  ,y = y_other_mean_grow, type = "l",lwd = 2,
     xlab = "", ylab = "",
     ylim = c(8.8,9.5), xlim = c(8.8,9.2)) + 
  lines(x = size_dummy, y = y_crem_mean_grow, type = "l", col = "red", lwd = 2) + 
  lines(x = size_dummy, y = y_liom_mean_grow, type = "l", col = "blue",lwd = 2) + 
  lines(x = size_dummy, y = y_vac_mean_grow, type = "l", col = "pink", lwd = 2) + 
  abline(a = 1, b = 1, col = "darkgrey", lty = 2)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c("black","red","blue","pink"), pch = 16)
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Log(Volume) year t+1",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
## Make Distribution Plots
dist_other <- mean(grow_out$beta0.3) + mean(size_dummy) * mean(grow_out$beta1.3)
dist_crem <- mean(grow_out$beta0.1) + mean(size_dummy) * mean(grow_out$beta1.1)
dist_liom <- mean(grow_out$beta0.2) + mean(size_dummy) * mean(grow_out$beta1.2)
dist_vac <- mean(grow_out$beta0.4) + mean(size_dummy) * mean(grow_out$beta1.4)
dist_other_h <- mean(grow_out$beta0.3) + quantile(size_dummy, 0.95) * mean(grow_out$beta1.3)
dist_crem_h <- mean(grow_out$beta0.1) + quantile(size_dummy, 0.95) * mean(grow_out$beta1.1)
dist_liom_h <- mean(grow_out$beta0.2) + quantile(size_dummy, 0.95) * mean(grow_out$beta1.2)
dist_vac_h <- mean(grow_out$beta0.4) + quantile(size_dummy, 0.95) * mean(grow_out$beta1.4)
dist_other_l <- mean(grow_out$beta0.3) + quantile(size_dummy, 0.25) * mean(grow_out$beta1.3)
dist_crem_l <- mean(grow_out$beta0.1) + quantile(size_dummy, 0.25) * mean(grow_out$beta1.1)
dist_liom_l <- mean(grow_out$beta0.2) + quantile(size_dummy, 0.25) * mean(grow_out$beta1.2)
dist_vac_l <- mean(grow_out$beta0.4) + quantile(size_dummy, 0.25) * mean(grow_out$beta1.4)
#### Plot the growth distributions at year t1
png("grow_dist.png")
par(mar=c(2,2,2,2),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4),
              ncol = 3, byrow = TRUE), heights = c(0.3,1), widths = c(3.9,4,4))
plot.new()
text(0.5,0.1,"Difference in Sizes (t1-t) by Ant Partner",cex=2,font=2)
#par(mfrow = c(1,3))
plot(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_other_l, sd = mean(grow_out$sigma)), type = "l", xlab = "Medium Cacti",ylab = "", main = "", xlim = c(-5,2))
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_crem_l, sd = mean(grow_out$sigma)), col = "red", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_liom_l, sd = mean(grow_out$sigma)), col = "blue", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_vac_l, sd = mean(grow_out$sigma)), col = "pink", lwd = 2)
abline(v = quantile(size_dummy, 0.05), col = "grey", lty = 2, lwd = 2)
legend("topleft", legend = c("Other","Crem.","Liom.","Vacant"), col = c("black","red","blue","pink"), pch = 16)
plot(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_other, sd = mean(grow_out$sigma)), type = "l", xlab = "Medium Cacti",ylab = "", main = "", xlim = c(2,9))
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_crem, sd = mean(grow_out$sigma)), col = "red", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_liom, sd = mean(grow_out$sigma)), col = "blue", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_vac, sd = mean(grow_out$sigma)), col = "pink", lwd = 2)
abline(v = mean(size_dummy), col = "grey", lty = 2, lwd = 2)
plot(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_other_h, sd = mean(grow_out$sigma)), type = "l", xlab = "Medium Cacti",ylab = "", main = "", xlim = c(11,15))
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_crem_h, sd = mean(grow_out$sigma)), col = "red", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_liom_h, sd = mean(grow_out$sigma)), col = "blue", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_vac_h, sd = mean(grow_out$sigma)), col = "pink", lwd = 2)
abline(v = quantile(size_dummy, 0.95), col = "grey", lty = 2, lwd = 2)
mtext("Log(Volume) year 1",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability based on Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
####

##Density of Growth Rates
png("grow_dist2.png")
par(mar=c(2,2,2,2),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4),
              ncol = 3, byrow = TRUE), heights = c(0.45,2.55), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.5,"Distributions of Growth Rates by Ant",cex=2.3,font=2)
#par(mfrow = c(1,3))
plot(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_other_l/quantile(size_dummy, 0.25), sd = mean(grow_out$sigma)), type = "l", xlab = "Medium Cacti",ylab = "", main = "", xlim = c(0,15))
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_crem_l/quantile(size_dummy, 0.25), sd = mean(grow_out$sigma)), col = "red", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_liom_l/quantile(size_dummy, 0.25), sd = mean(grow_out$sigma)), col = "blue", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_vac_l/quantile(size_dummy, 0.25), sd = mean(grow_out$sigma)), col = "pink", lwd = 2)
abline(v = 1, col = "grey", lty = 2, lwd = 2)
plot(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_other/mean(size_dummy), sd = mean(grow_out$sigma)), type = "l", xlab = "Medium Cacti",ylab = "", main = "", xlim = c(-2,4))
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_crem/mean(size_dummy,), sd = mean(grow_out$sigma)), col = "red", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_liom/mean(size_dummy), sd = mean(grow_out$sigma)), col = "blue", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_vac/mean(size_dummy), sd = mean(grow_out$sigma)), col = "pink", lwd = 2)
abline(v = 1, col = "grey", lty = 2, lwd = 2)
plot(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_other_h/quantile(size_dummy, 0.95), sd = mean(grow_out$sigma)), type = "l", xlab = "Medium Cacti",ylab = "", main = "", xlim = c(-2,4))
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_crem_h/quantile(size_dummy, 0.95), sd = mean(grow_out$sigma)), col = "red", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_liom_h/quantile(size_dummy, 0.95), sd = mean(grow_out$sigma)), col = "blue", lwd = 2)
lines(x = size_dummy, y = dnorm(x = size_dummy, mean = dist_vac_h/quantile(size_dummy, 0.95), sd = mean(grow_out$sigma)), col = "pink", lwd = 2)
abline(v = 1, col = "grey", lty = 2, lwd = 2)
legend("topright", legend = c("Other","Crem.","Liom.","Vacant"), col = c("black","red","blue","pink"), pch = 16)
mtext("Log(Volume) year 1",side=1,line=0,outer=TRUE,cex=1.0)
mtext("Probability based on Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()

###########################################################################################################################
#### Survival Visuals #####################################################################################################
#########################################################################################################################
## Extract & Format Data
survival_data_orig <- subset(cactus, is.na(Survival_t1) == FALSE,c("Plot","Year_t","Survival_t1","ant_t","logsize_t"))
survival_data_orig <- cactus[,c("Plot","Year_t","Survival_t1","ant_t","logsize_t")]
survival_data <- na.omit(survival_data_orig)
#extract from original ddata
surv_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/surv_outputs.csv", header = TRUE,stringsAsFactors=T)
y_subset <- survival_data[,c("Survival_t1","ant_t", "logsize_t")]
y_crem_subset_surv <- subset(y_subset, ant_t == "crem")
y_liom_subset_surv <- subset(y_subset, ant_t == "liom")
y_vac_subset_surv <- subset(y_subset, ant_t == "vacant")
y_other_subset_surv <- subset(y_subset, ant_t == "other")
#Size Dummies for every ant
size_crem = seq(min((y_crem_subset_surv$logsize_t), na.rm = TRUE), max ((y_crem_subset_surv$logsize_t), na.rm = TRUE), by = 0.1)
size_other = seq(min((y_other_subset_surv$logsize_t), na.rm = TRUE), max ((y_other_subset_surv$logsize_t), na.rm = TRUE), by = 0.1)
size_liom = seq(min((y_liom_subset_surv$logsize_t), na.rm = TRUE), max ((y_liom_subset_surv$logsize_t), na.rm = TRUE), by = 0.1)
size_vac = seq(min((y_vac_subset_surv$logsize_t), na.rm = TRUE), max ((y_vac_subset_surv$logsize_t), na.rm = TRUE), by = 0.1)
## Formulas
y_other_surv = quantile(surv_out$beta0.3,0.5) + size_other * quantile(surv_out$beta1.3,0.5)
y_other_low_surv = quantile(surv_out$beta0.3,0.05) + size_other * quantile(surv_out$beta1.3,0.05)
y_other_high_surv = quantile(surv_out$beta0.3,0.95) + size_other * quantile(surv_out$beta1.3,0.95)
other_extr = quantile(surv_out$beta0.3,0.5) + size_dummy * quantile(surv_out$beta1.3,0.5)
y_crem_surv = quantile(surv_out$beta0.1,0.5) + size_crem * quantile(surv_out$beta1.1,0.5)
y_crem_low_surv = quantile(surv_out$beta0.1,0.05) + size_crem * quantile(surv_out$beta1.1,0.05)
y_crem_high_surv = quantile(surv_out$beta0.1,0.95) + size_crem * quantile(surv_out$beta1.1,0.95)
crem_extr = quantile(surv_out$beta0.1,0.5) + size_dummy * quantile(surv_out$beta1.1,0.5)
y_liom_surv = quantile(surv_out$beta0.2,0.5) + size_liom * quantile(surv_out$beta1.2,0.5)
y_liom_low_surv = quantile(surv_out$beta0.2,0.05) + size_liom * quantile(surv_out$beta1.2,0.05)
y_liom_high_surv = quantile(surv_out$beta0.2,0.95) + size_liom * quantile(surv_out$beta1.2,0.95)
liom_extr = quantile(surv_out$beta0.2,0.5) + size_dummy * quantile(surv_out$beta1.2,0.5)
y_vac_surv = quantile(surv_out$beta0.4,0.5) + size_vac * quantile(surv_out$beta1.4,0.5)
y_vac_low_surv = quantile(surv_out$Bebeta0.4ta0_4,0.05) + size_vac * quantile(surv_out$beta1.4,0.05)
y_vac_high_surv = quantile(surv_out$beta0.4,0.95) + size_vac * quantile(surv_out$beta1.4,0.95)
vac_extr = quantile(surv_out$beta0.4,0.5) + size_dummy * quantile(surv_out$beta1.4,0.5)
## Panel Plots
png("surv_panels.png")
par(mar=c(2,2,2,2))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(1,2,2), widths = c(4,4,4))
plot.new()
text(0.5,0.25,"Survival Rates of Cacti \nof by Ant State and Size",cex=2,font=2)
# Other (3)
plot(x = size_other  ,y = invlogit(y_other_surv), type = "l", col = "black", lwd = 4, ylim = c(0,1), xlim = c(-5,15))
points(x = (y_other_subset_surv$logsize_t), y = y_other_subset_surv$Survival_t1, col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.4))
#lines(x = size_dummy, y = invlogit(y_other_low_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_other_high_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_other,rev(size_other)),c(invlogit(y_other_high_surv), rev(invlogit(y_other_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Crem (1)
plot(x = size_crem  ,y = invlogit(y_crem_surv), type = "l", col = "red", lwd = 4, ylim = c(0,1), xlim = c(-5,15))
points(x = (y_crem_subset_surv$logsize_t), y = (y_crem_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.4))
#lines(x = size_dummy, y = invlogit(y_crem_low_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_crem_high_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_crem,rev(size_crem)),c(invlogit(y_crem_high_surv), rev(invlogit(y_crem_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Liom (2)
plot(x = size_liom  ,y = invlogit(y_liom_surv), type = "l", col = "blue", lwd = 4, ylim = c(0,1), xlim = c(-5,15))
points(x = (y_liom_subset_surv$logsize_t), y = y_liom_subset_surv$Survival_t1, col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.4))
#lines(x = size_dummy, y = invlogit(y_liom_low_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_liom_high_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)# Vacant
polygon(c(size_liom,rev(size_liom)),c(invlogit(y_liom_high_surv), rev(invlogit(y_liom_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# vacant (4)
plot(x = size_vac  ,y = invlogit(y_vac_surv), type = "l", col = "pink", lwd = 4, ylim = c(0,1), xlim = c(-5,15))
points(x = (y_vac_subset_surv$logsize_t), y = y_vac_subset_surv$Survival_t1, col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.4))
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
mtext("Log(Volume)",side=1,line=0,outer=TRUE,cex=1.0)
mtext("Probability of Survival",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()


par(mfrow = c(1,1))

## Panels 2
png("surv_panel_lines.png")
par(mar=c(5,5,0,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(1,2,2), widths = c(4,4,4))
plot.new()
text(0.5,0.5,"Survival Rates of Cacti \nof by Ant State and Size",cex=2,font=2)
# Other (3)
samp <- sample(nrow(surv_out), 50)
plot(x = size_other  ,y = invlogit(y_other_surv), type = "l", col = "black", lwd = 4, ylim = c(0,1),xlim = c(-5,15), xlab = "",ylab = "")
for(i in 1:1500){
  lines(x = size_other, y = invlogit(surv_out$beta0.3[i] + size_other * surv_out$beta1.3[i]),col = "lightgrey", alpha = 0.1)
}
points(x = (y_other_subset_surv$logsize_t), y = (y_other_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
lines(x = size_other  ,y = invlogit(y_other_surv), type = "l", col = "black", lwd = 4)
# Crem (1)
plot(x = size_crem  ,y = invlogit(y_crem_surv), type = "l", col = "red", lwd = 4, ylim = c(0,1),xlim = c(-5,15),xlab = "",ylab = "") 
for(i in 1:1500){
  lines(x = size_crem, y = invlogit(surv_out$beta0.1[i] + size_crem * surv_out$beta1.1[i]),col = "lightgrey", alpha = 0.1)
}
points(x = (y_crem_subset_surv$logsize_t), y = (y_crem_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) 
lines(x = size_crem  ,y = invlogit(y_crem_surv), type = "l", col = "red", lwd = 4)
# Liom (2)
plot(x = size_liom  ,y = invlogit(y_liom_surv), type = "l", col = "blue", lwd = 4, ylim = c(0,1),xlim = c(-5,15),xlab = "",ylab = "")
for(i in 1:1500){
  lines(x = size_liom, y = invlogit(surv_out$beta0.2[i] + size_liom * surv_out$beta1.2[i]),col = "lightgrey", alpha = 0.1)
}
points(x = (y_liom_subset_surv$logsize_t), y = (y_liom_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3))
lines(x = size_liom  ,y = invlogit(y_liom_surv), type = "l", col = "blue", lwd = 4)
# Vacant (4)s
plot(x = size_vac  ,y = invlogit(y_vac_surv), type = "l", col = "pink", lwd = 4, ylim = c(0,1),xlim = c(-5,15),xlab = "",ylab = "") 
for(i in 1:1500){
  lines(x = size_vac, y = invlogit(surv_out$beta0.4[i] + size_vac * surv_out$beta1.4[i]),col = "lightgrey", alpha = 0.1)
}
points(x = (y_vac_subset_surv$logsize_t), y = (y_vac_subset_surv$Survival_t1), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.3)) + 
lines(x = size_vac  ,y = invlogit(y_vac_surv), type = "l", col = "pink", lwd = 4)
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

#########################################################################################################################
#### Flowering Visuals #####################################################################################################
#########################################################################################################################
## Extract & Format Data
flower_data_orig <- cactus[ , c("TotFlowerbuds_t", "logsize_t","Year_t","Plot")]
flower_data_orig <- subset(flower_data_orig, TotFlowerbuds_t > 0)
flower_data <- na.omit(flower_data_orig)
flow_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/flow_trunc_outputs.csv", header = TRUE,stringsAsFactors=T)
## Formulas
size_dummy <- seq(min((flower_data$logsize_t), na.rm = TRUE), max((flower_data$logsize_t), na.rm = TRUE), by = 0.1)
#format for overlay plots
## Formulas
y_flow = quantile(flow_out$beta0,0.5) + size_dummy * quantile(flow_out$beta1,0.5)
y_low_flow = quantile(flow_out$beta0,0.05) + size_dummy * quantile(flow_out$beta1,0.05)
y_high_flow = quantile(flow_out$beta0,0.95) + size_dummy * quantile(flow_out$beta1,0.95)
##Panel Plot
png("flow_panels.png")
par(mar=c(4,4,0,1))
layout(matrix(c(1,2),
              ncol = 1, byrow = TRUE), heights = c(1.2,2))
plot.new()
text(0.5,0.25,"Number of Flowers \n Produced by Size",cex=2,font=2)
plot(x = size_dummy  ,y = exp(y_flow), type = "l", col = "chartreuse4", lwd = 4, ylim = c(0,100), xlab = "Log(Volume)", ylab = "Total Flowerbuds")
points(x = flower_data$logsize_t, y = flower_data$TotFlowerbuds_t,col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.6))
#points( x = log(cactus$volume_t), y = cactus$TotFlowerbuds_t1, col = "red")
lines(x = size_dummy, y = exp(y_low_flow), type = "l", col = "darkgrey", lty = 2, lwd = 2)
lines(x = size_dummy, y = exp(y_high_flow), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_dummy,rev(size_dummy)),c(exp(y_high_flow), rev(exp(y_low_flow))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
dev.off()

png("flow_lines.png")
par(mar=c(4.8,4.8,0,1))
layout(matrix(c(1,2),
              ncol = 1, byrow = TRUE), heights = c(1.2,2))
plot.new()
text(0.5,0.45,"Total Number of Flowers \n Produced by Size",cex=2,font=2)
plot(x = size_dummy  ,y = exp(y_flow), type = "l", col = "chartreuse4", lwd = 4, xlim = c(10,14.9), ylim = c(0,150), xlab = "Log(Volume)",ylab = "Flowerbuds Produced")
for(i in 1:1500){
  lines(x = size_dummy, y = exp(flow_out$beta0[i] + size_dummy * flow_out$beta1[i]),col = "lightgrey", alpha = 0.1)
}
points(x = (flower_data$logsize_t), y = (flower_data$TotFlowerbuds_t),col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.6))
lines(x = size_dummy  ,y = exp(y_flow), type = "l", col = "chartreuse4", lwd = 4)
dev.off()

#########################################################################################################################
#### Viability Visuals #####################################################################################################
#########################################################################################################################
## Extract & Format Data
viability_data_orig <- cactus[ , c("TotFlowerbuds_t1","Goodbuds_t1","ABFlowerbuds_t1","ant_t", "logsize_t","Year_t","Plot")]
viability_data_orig <- subset(viability_data_orig, TotFlowerbuds_t1 > 0)
viability_data <- na.omit(viability_data_orig)
viab_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/viab_outputs.csv", header = TRUE,stringsAsFactors=T)
#format for overlay plots
#extract from original data
y_subset_good <- viability_data[,c("Goodbuds_t1","ant_t", "logsize_t")]
## Formulas
y_other_viab = quantile(viab_out$beta0.3,0.5)
y_other_low_viab = quantile(viab_out$beta0.3,0.05)
y_other_high_viab = quantile(viab_out$beta0.3,0.95)
y_other_subset_viab = subset(y_subset_good, ant_t == "other")
y_crem_viab = quantile(viab_out$beta0.1,0.5)
y_crem_low_viab = quantile(viab_out$beta0.1,0.05)
y_crem_high_viab = quantile(viab_out$beta0.1,0.95)
y_crem_subset_viab = subset(y_subset_good, ant_t == "crem")
y_liom_viab = quantile(viab_out$beta0.2,0.5) 
y_liom_low_viab = quantile(viab_data$beta0.2,0.05)
y_liom_high_viab = quantile(viab_out$beta0.2,0.95)
y_liom_subset_viab = subset(y_subset_good, ant_t == "liom")
y_vac_viab = quantile(viab_out$beta0.4,0.5)
y_vac_low_viab = quantile(viab_out$beta0.4,0.05)
y_vac_high_viab = quantile(viab_out$beta0.4,0.95) 
y_vac_subset_viab = subset(y_subset_good, ant_t == "vacant")
## Subsets
for(i in 1:nrow(viability_data)){
  viability_data$viab[i] <- viability_data$Goodbuds_t1[i]/viability_data$TotFlowerbuds_t1[i]
}
other_subset <- subset(viability_data, ant_t == "other")
crem_subset <- subset(viability_data, ant_t == "crem")
liom_subset <- subset(viability_data, ant_t == "liom")
vac_subset <- subset(viability_data, ant_t == "vacant")

## Panel Plots (proportion of viable buds)
means <- c(mean(viab_out$beta0.1), mean(viab_out$beta0.2), mean(viab_out$beta0.3), mean(viab_out$beta0.4))
low <- c(quantile(viab_out$beta0.1,0.05),quantile(viab_out$beta0.2,0.05),quantile(viab_out$beta0.3,0.05),quantile(viab_out$beta0.4,0.05))
high <- c(quantile(viab_out$beta0.1,0.95),quantile(viab_out$beta0.2,0.95),quantile(viab_out$beta0.3,0.95),quantile(viab_out$beta0.4,0.95))

png("viab_bars.png")
barplot(invlogit(means), ylim = c(0,1.1), col = c("red","blue","black","pink"), xlab = "Ant Partner", ylab = "Viability Rate",
        main = "Proportion of Viable Flowerbuds \n by Ant Species", names.arg = c("Crem.","Liom.","Other","Vacant"))
dev.off()

list <- cbind(invlogit(viab_out$beta0.1),invlogit(viab_out$beta0.2),invlogit(viab_out$beta0.3),invlogit(viab_out$beta0.4))
png("viab_box.png")
boxplot((list), ylim = c(0,1),
        col = c("red","blue","black","pink"), names.arg = c("Crem.","Liom.","Other","Vacant"),
        xlab = "Ant Partner", ylab = "Viability Rate", main = "Proportion of Viable Flowerbuds \n by Ant Species"
)
dev.off()
## Compare the predicted box plot based on the models w the box plot based on the data

list <- cbind(as.numeric(crem_subset$viab),as.numeric(liom_subset$viab),as.numeric(other_subset$viab),as.numeric(vac_subset$viab))
boxplot((list), ylim = c(0,1),
        col = c("red","blue","black","pink"), names.arg = c("Crem.","Liom.","Other","Vacant"),
        xlab = "Ant Partner", ylab = "Viability Rate", main = "Proportion of Viable Flowerbuds \n by Ant Species"
)
dev.off()
#########################################################################################################################
#### Reproductive Visuals #####################################################################################################
#########################################################################################################################
## Extract & Format Data
#format for overlay plots
#extract from STAN models
#extract from original data
repro_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/repro_outputs.csv", header = TRUE,stringsAsFactors=T)
## Formulas
size_dummy <- seq(min(reproductive_data$logsize_t),max((reproductive_data$logsize_t)), by = 0.1)
y_repro = quantile(repro_out$beta0,0.5,na.rm = TRUE) + size_dummy * quantile(repro_out$beta1,0.5,na.rm = TRUE)
y_low_repro = quantile(repro_out$beta0,0.05) + size_dummy * quantile(repro_out$beta1,0.05)
y_high_repro = quantile(repro_out$beta0,0.95) + size_dummy * quantile(repro_out$beta1,0.95)
## Panel Plots
png("repro_panel.png")
par(mar=c(5,5,0,1))
layout(matrix(c(1,2),
              ncol = 1, byrow = TRUE), heights = c(1.2,2))
plot.new()
text(0.5,0.25,"Probability of Reproducing \n by Size",cex=2,font=2)
plot(x = (size_dummy)  ,y = invlogit(y_repro), type = "l", col = "chartreuse4",ylim = c(0,1),  lwd = 4,xlab = "Log(Volume)",ylab = "Reproduction Rate")
points(x = (y_subset$logsize_t), y =(y_subset$flower1_YN))
lines(x = (size_dummy), y = invlogit(y_low_repro), type = "l", col = "darkgrey", lty = 2, lwd = 2)
lines(x = (size_dummy), y = invlogit(y_high_repro), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c((size_dummy),rev((size_dummy))),c(invlogit(y_high_repro), rev(invlogit(y_low_repro))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
dev.off()
png("repro_panel_lines.png")
par(mar=c(5,5,0,1))
layout(matrix(c(1,2),
              ncol = 1, byrow = TRUE), heights = c(1.2,2))
plot.new()
text(0.5,0.25,"Probability of Reproducing \n by Size",cex=2,font=2)
plot(x = (size_dummy)  ,y = invlogit(y_repro), type = "l", col = "chartreuse4", ylim = c(0,1), lwd = 4, xlab = "Log(Volume)", ylab = "Reproduction Rate")
for(i in 1:5000){
  lines(x = (size_dummy), y = invlogit(repro_out$beta0[i] + size_dummy * repro_out$beta1[i]),col = "lightgrey", alpha = 0.1)
}
points(x = (reproductive_data$logsize_t), y = (reproductive_data$flower1_YN))
lines(x = (size_dummy), y = invlogit(y_repro), type = "l", col = "chartreuse4", lwd = 4)
dev.off()



#########################################################################################################################
#### Seed Produced Visuals #####################################################################################################
#########################################################################################################################
#make the column for the ant state of the part of the plant producing seeds
for(i in 1:nrow(seed)){
  #If there is no ant access then vacant
  if(seed$ant.access[i] == "n" & is.na(seed$ant.access[i]) == FALSE){
    seed$ant_state[i] <- "Vacant"
  }
  #If there is ant access but it is still vacant then vacant
  if(seed$ant.access[i] == "y" & is.na(seed$ant.access[i]) == FALSE & seed$vacant[i] == "y"){
    seed$ant_state[i] <- "Vacant"
  }
  #if there is ant access and it is not vacant and the ant is crem then crem
  if(seed$ant.access[i] == "y" & is.na(seed$ant.access[i]) == FALSE & seed$vacant[i] == "n" & seed$species[i] == "c"){
    seed$ant_state[i] <- "Crem"
  }
  #if there is ant access and it is not vacant and the ant is liom then liom
  if(seed$ant.access[i] == "y" & is.na(seed$ant.access[i]) == FALSE & seed$vacant[i] == "n" & seed$species[i] == "l"){
    seed$ant_state[i] <- "Liom"
  }
}

seed_data <- seed
seed_data <- na.omit(seed_data)
seed_data$ant <- as.integer(as.factor(seed_data$ant_state))
seed_data <- subset(seed_data, seed_count > 0)
seed_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/seed_outputs.csv", header = TRUE,stringsAsFactors=T)
y_crem <- seed_out$beta0.1
y_vac <- seed_out$beta0.3
y_liom <- seed_out$beta0.2
y_crem_l <- quantile(seed_out$beta0.1,0.05)
y_vac_l <- quantile(seed_out$beta0.3,0.05)
y_liom_l <- quantile(seed_out$beta0.2,0.05)
y_crem_h <- quantile(seed_out$beta0.1,0.95)
y_vac_h <- quantile(seed_out$beta0.3,0.95)
y_liom_h <- quantile(seed_out$beta0.2,0.95)

seeds <- as.data.frame(matrix(nrow = 3 * nrow(seed_out)))
for(i in 1:nrow(seed_out)){
  seeds$beta[i] <- seed_out$beta0.1[i]
  seeds$ant[i] <- "crem"
  seeds$low[i] <- y_crem_l
  seeds$high[i] <- y_crem_h
}
for(i in 1:nrow(seed_out)){
  seeds$beta[nrow(seed_out) + i] <- seed_out$beta0.2[i]
  seeds$ant[nrow(seed_out) + i] <- "liom"
  seeds$low[i] <- y_liom_l
  seeds$high[i] <- y_liom_h
}
for(i in 1:nrow(seed_out)){
  seeds$beta[2*nrow(seed_out) + i] <- seed_out$beta0.3[i]
  seeds$ant[2*nrow(seed_out) + i] <- "vacant"
  seeds$low[i] <- y_vac_l
  seeds$high[i] <- y_vac_h
}
unique(seeds$ant)
boxplot(seed$seed_count~seed$ant_state)

png("seeds_bars.png")
plot(exp(seeds$beta) ~ factor(seeds$ant), xlab = "Ant Species",ylab = "Expected Seeds", col = c("red","blue","pink"), main = "Liom. Provides Advantages \n for Seed Production")
dev.off()

########################################################################################################
#### Pre-Censyse Seed Survival #########################################################################
########################################################################################################
precensus.dat.orig<-read.csv("Data Analysis/PrecensusSurvival.csv") 
precensus.dat <- precensus.dat.orig[ , c("Transect","Seed","Log.size","survive0405")]
precensus.dat <- na.omit(precensus.dat)
y_subset <- (precensus.dat)
seed_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/seed_surv_outputs.csv", header = TRUE,stringsAsFactors=T)
## Formulas
y_surv = seed_out$beta0
y_low_surv = quantile(seed_out$beta0,0.05) 
y_high_surv = quantile(seed_out$beta0,0.95)
y_surv <- cbind(invlogit(quantile(seed_out$beta0,0.5)), invlogit(quantile(seed_out$beta0,0.05)), invlogit(quantile(seed_out$beta0,0.95)))

png("seed_surv_box.png")
boxplot(invlogit(y_surv), col = "chartreuse4", ylim = c(-0.1,1),
        main = "Proportion of Seeds that \n Survive to Census", ylab = "Survival Rate")
dev.off()

################################################################################################################################################
##################### Germination Yr 1&2 Visuals ########################################################################
###############################################################################################################################################
germ1_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/germ1_outputs.csv", header = TRUE,stringsAsFactors=T)
germ2_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/germ2_outputs.csv", header = TRUE,stringsAsFactors=T)
y_germ1 <- (germ1_out$beta0) 
y_germ2 <- (germ2_out$beta0) 

plot(c(invlogit(y_germ1),invlogit(y_germ2)))

germ <- cbind(y_germ1,y_germ2)
colnames(germ) <- c("Year 1","Year 2")
png("germination.png")
boxplot(invlogit(germ), col = "chartreuse4", names.arg = c("Yr 1","Yr 2"), 
        xlab = "Year in Seedbank", ylab = "Probability of Germinating", main = "Seeds Are More Likely to \n Germinate in Year 1")
dev.off()


################################################################################################################################################
##################### Recruit Sizes Visuals ########################################################################
###############################################################################################################################################
seedling.dat_orig <- cactus[,c("logsize_t1","Recruit")]
seedling.dat_orig <- filter(seedling.dat_orig, Recruit == 1)
seedling.dat <- na.omit(seedling.dat_orig)
recruit_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/rec_outputs.csv", header = TRUE,stringsAsFactors=T)
y_rec = quantile(recruit_out$beta0,0.5)
y_rec_low = quantile(recruit_out$beta0,0.05)
y_rec_high = quantile(recruit_out$beta0,0.95)
size_dummy_rec <- seq(min(seedling.dat$logsize_t1), max(seedling.dat$logsize_t1), by = 0.1)


png("rec_size.png")
boxplot(recruit_out$beta0, col = "chartreuse4", ylab = "Log(Volume)", main = "Recruit Size Distribution")
dev.off()

boxplot(seedling.dat$logsize_t1)

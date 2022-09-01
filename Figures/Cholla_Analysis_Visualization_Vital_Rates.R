##################################################################################################################
##
##              This file visualizes the outputs of the Bayesian Models created in the sourced code
##
##################################################################################################################
##################################################################################################################
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
source( "/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Cholla_Analysis_Vital_Rates.R")

## Color Codes
## Retro bright
cremcol <- "#9239F6"
liomcol <- "#00A08A"
othercol <- "#FF0076"
vaccol <- "#F8B660"

str(cactus)
##### Size variable used in most visualizations
size_dummy <- seq(min(cactus$logsize_t, na.rm = T), max(cactus$logsize_t, na.rm = TRUE), by = 0.1)

#######################################################################################################
#### Hypotheses #######################################################################################
#######################################################################################################
barplot(c(0.1014,0.06296,0.1265,0.09043), col = c(othercol, cremcol, liomcol, vaccol), names.arg = c("Other","Crem.","Liom.","Vacant"),
        ylab = "Herbivory Prob.", main = "Proportion of Plants with Evidence of Herbivory")
#### Sampling Effect
heights <- c(0.9,1.2,1.2)
png("Sampling_Effect.png")
barplot(heights, col = c("Chartreuse4","Pink", "Purple"),names.arg = c("A","B","A & B"), ylab = "Lambda (Fitness)", main = "Sampling Effect")
dev.off()

#### Complementarity
heights <- c(0.9,1.2,1.6)
png("Complementarity.png")
barplot(heights, col = c("Chartreuse4","Pink", "Purple"),names.arg = c("A","B","A & B"), ylab = "Lambda (Fitness)", main = "Complementarity")
dev.off()

#### Portfolio Effect
yr <- seq(2000,2010,by = 1)
yr1 <- c(1,1.5,.2,0,.5,.7,.6,1.2,1.3,1.8,1)
yr2 <- c(1,.2,1.2,1.7,1.5,1.1,.9,.5,.5,.6,0.8)
png("Portfolio_Effect.png")
plot(x = yr, y = yr1, type = "l", col = "Chartreuse4", xlab = "Year", ylab = "Lambda (Fitness)", main = "Portfolio Effect", lwd = 2)
lines(x = yr, y = yr2, col = "Pink", lwd = 2)
legend("bottomright", legend = c("A","B"), fill = c("Chartreuse4","Pink"))
dev.off()

########################################################################################################
#### Herbivores Visuals ################################################################################
########################################################################################################
str(cactus_herb)
## Breakdown of the herbivore data -- what years we have info
## herbivore data does not start until 2013 so I am going to make it a subset excluding those years without data
cactus_herb <- subset(cactus_herb, cactus_herb$Year_t >= 2013 & cactus_herb$Year_t != 2019)
## Mean herbivore probabilities by ant species
summary(cactus_herb$herb_YN[cactus_herb$ant_t == "liom" & cactus_herb$Year_t >= 2013 & cactus_herb$Year_t <=2018])
## 0.1014
summary(cactus_herb$herb_YN[cactus_herb$ant_t == "vacant" & cactus_herb$Year_t >= 2013 & cactus_herb$Year_t <=2018])
## 0.06296
summary(cactus_herb$herb_YN[cactus_herb$ant_t == "crem" & cactus_herb$Year_t >= 2013 & cactus_herb$Year_t <=2018])
## 0.1265
summary(cactus_herb$herb_YN[cactus_herb$ant_t == "other" & cactus_herb$Year_t >= 2013 & cactus_herb$Year_t <=2018])
## 0.09043
## Now break this further into flowering and not flowering
cactus_herb_flow <- subset(cactus_herb, cactus_herb$TotFlowerbuds_t1 > 0 | cactus_herb$Goodbuds_t1 > 0 | cactus_herb$ABFlowerbuds_t1 > 0)
summary(cactus_herb_flow$herb_YN[cactus_herb_flow$ant_t == "liom"])
## 0.1284
summary(cactus_herb_flow$herb_YN[cactus_herb_flow$ant_t == "vacant"])
## 0.2048
summary(cactus_herb_flow$herb_YN[cactus_herb_flow$ant_t == "crem"])
## 0.1532
summary(cactus_herb_flow$herb_YN[cactus_herb_flow$ant_t == "other"])
## 0.1216

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
## All plants -- flowering and non
png("herb_ant_only.png")
barplot(c(0.1014,0.06296,0.1265,0.09043), col = c(othercol, cremcol, liomcol, vaccol), names.arg = c("Other","Crem.","Liom.","Vacant"),
        ylab = "Herbivory Prob.", main = "Proportion of Plants with Evidence of Herbivory")
dev.off()
## Flowering plants only
png("herb_ant_only_flow.png")
barplot(c(0.1216, 0.1532, 0.1284, 0.2048), col = c(othercol, cremcol, liomcol, vaccol), names.arg = c("Other","Crem.","Liom.","Vacant"),
        ylab = "Herbivory Prob.", main = "Proportion of Plants with Evidence of Herbivory")
dev.off()

#### All plants -- including flowering and non
## Bin the data to get the probability of herbivory based on the size and ant species
herb_crem <- subset(cactus_herb, cactus_herb$ant_t == "crem")
herb_liom <- subset(cactus_herb, cactus_herb$ant_t == "liom")
herb_other <- subset(cactus_herb, cactus_herb$ant_t == "other")
herb_vac <- subset(cactus_herb, cactus_herb$ant_t == "vacant")

## Bin the size data
## Crem
herb_plot_crem <- herb_crem %>% 
  mutate(size_bin = cut_interval((logsize_t1),10)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t1),na.rm=T),
            surv = mean(herb_YN,na.rm=T),
            N = length(logsize_t1))
herb_plot_crem$N_mod <- log(herb_plot_crem$N)
## Liom
herb_plot_liom <- herb_liom %>% 
  mutate(size_bin = cut_interval((logsize_t1),10)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t1),na.rm=T),
            surv = mean(herb_YN,na.rm=T),
            N = length(logsize_t1))
herb_plot_liom$N_mod <- log(herb_plot_liom$N)
## Other
herb_plot_other <- herb_other %>% 
  mutate(size_bin = cut_interval((logsize_t1),10)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t1),na.rm=T),
            surv = mean(herb_YN,na.rm=T),
            N = length(logsize_t1))
herb_plot_other$N_mod <- log(herb_plot_other$N)
## Vac
herb_plot_vac <- herb_vac %>% 
  mutate(size_bin = cut_interval((logsize_t1),10)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t1),na.rm=T),
            surv = mean(herb_YN,na.rm=T),
            N = length(logsize_t1))
herb_plot_vac$N_mod <- log(herb_plot_vac$N)

## Plot the proportion of plants that have evidence of herbivory by ant species
prop <- c(0.1014,0.06296,0.1265,0.09043)
ant <- c("liom","vacant","crem","other")
plot(c(1,2,3,4),prop)

## Look at the relationship between size, ant state, and herbivory
mod1 <- glm(cactus_herb$herb_YN ~ cactus_herb$ant_t + cactus_herb$logsize_t1, family = "binomial", data = cactus_herb)
coef(mod1)
## Calculate the probabilities of herbivory based on size data
size_vac <- seq(min(cactus_herb$logsize_t1[cactus_herb$ant_t == "vacant"],na.rm = T), max(cactus_herb$logsize_t1[cactus_herb$ant_t == "vacant"],na.rm = T), by = 0.1)
vac_prob <- coef(mod1)[1] + coef(mod1)[5]*size_vac
size_liom <- seq(min(cactus_herb$logsize_t1[cactus_herb$ant_t == "liom"],na.rm = T), max(cactus_herb$logsize_t1[cactus_herb$ant_t == "liom"],na.rm = T), by = 0.1)
liom_prob <- coef(mod1)[1] + coef(mod1)[4] + coef(mod1)[5]*size_liom
size_crem <- seq(min(cactus_herb$logsize_t1[cactus_herb$ant_t == "crem"],na.rm = T), max(cactus_herb$logsize_t1[cactus_herb$ant_t == "crem"],na.rm = T), by = 0.1)
crem_prob <- coef(mod1)[1] + coef(mod1)[3] + coef(mod1)[5]*size_crem
size_other <- seq(min(cactus_herb$logsize_t1[cactus_herb$ant_t == "other"],na.rm = T), max(cactus_herb$logsize_t1[cactus_herb$ant_t == "other"],na.rm = T), by = 0.1)
other_prob <- coef(mod1)[1] + coef(mod1)[2] + coef(mod1)[5]*size_other
## Plot it
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("herb_ant_size.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(0.7,1.4,1.4), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.5,"Growth Rates of Cacti \nof by Ant State and Size",cex=2,font=2)
# Other 
plot(herb_other$logsize_t1,herb_other$herb_YN)
lines(size_other, invlogit(other_prob), col = othercol, lwd = 2)
# Liom
plot(herb_liom$logsize_t1,herb_liom$herb_YN)
lines(size_liom, invlogit(liom_prob), col = liomcol, lwd = 2)
# Crem 
plot(herb_crem$logsize_t1,herb_crem$herb_YN)
lines(size_crem, invlogit(crem_prob), col = cremcol, lwd = 2)
# Vac 
plot(herb_vac$logsize_t1,herb_vac$herb_YN)
lines(size_vac, invlogit(vac_prob), col = vaccol, lwd = 2)
## All together
plot(size_other, invlogit(other_prob), col = othercol, lwd = 2, type = "l", ylim = c(0,0.2))
lines(size_liom, invlogit(liom_prob), col = liomcol, lwd = 2)
lines(size_crem, invlogit(crem_prob), col = cremcol, lwd = 2)
lines(size_vac, invlogit(vac_prob), col = vaccol, lwd = 2)
legend("topleft", legend = c("Other","Crem.","Liom.","Vacant"), col = c(othercol,cremcol,liomcol,vaccol), pch = 16)
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Herbivory",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()

#### Flowering plants only
## Bin the data to get the probability of herbivory based on the size and ant species
herb_crem <- subset(cactus_herb_flow, cactus_herb_flow$ant_t == "crem")
herb_liom <- subset(cactus_herb_flow, cactus_herb_flow$ant_t == "liom")
herb_other <- subset(cactus_herb_flow, cactus_herb_flow$ant_t == "other")
herb_vac <- subset(cactus_herb_flow, cactus_herb_flow$ant_t == "vacant")

## Bin the size data
## Crem
herb_plot_crem <- herb_crem %>% 
  mutate(size_bin = cut_interval((logsize_t1),10)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t1),na.rm=T),
            surv = mean(herb_YN,na.rm=T),
            N = length(logsize_t1))
herb_plot_crem$N_mod <- log(herb_plot_crem$N)
## Liom
herb_plot_liom <- herb_liom %>% 
  mutate(size_bin = cut_interval((logsize_t1),10)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t1),na.rm=T),
            surv = mean(herb_YN,na.rm=T),
            N = length(logsize_t1))
herb_plot_liom$N_mod <- log(herb_plot_liom$N)
## Other
herb_plot_other <- herb_other %>% 
  mutate(size_bin = cut_interval((logsize_t1),10)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t1),na.rm=T),
            surv = mean(herb_YN,na.rm=T),
            N = length(logsize_t1))
herb_plot_other$N_mod <- log(herb_plot_other$N)
## Vac
herb_plot_vac <- herb_vac %>% 
  mutate(size_bin = cut_interval((logsize_t1),10)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t1),na.rm=T),
            surv = mean(herb_YN,na.rm=T),
            N = length(logsize_t1))
herb_plot_vac$N_mod <- log(herb_plot_vac$N)

## Plot the proportion of plants that have evidence of herbivory by ant species
prop <- c(0.1284, 0.2048, 0.1532, 0.1216)
ant <- c("liom","vacant","crem","other")
plot(c(1,2,3,4),prop)

## Look at the relationship between size, ant state, and herbivory
mod2 <- glm(cactus_herb_flow$herb_YN ~ cactus_herb_flow$ant_t + cactus_herb_flow$logsize_t1, family = "binomial", data = cactus_herb_flow)
coef(mod2)
## Calculate the probabilities of herbivory based on size data
size_vac <- seq(min(cactus_herb_flow$logsize_t1[cactus_herb_flow$ant_t == "vacant"],na.rm = T), max(cactus_herb_flow$logsize_t1[cactus_herb_flow$ant_t == "vacant"],na.rm = T), by = 0.1)
vac_prob <- coef(mod2)[1] + coef(mod2)[5]*size_vac
size_liom <- seq(min(cactus_herb_flow$logsize_t1[cactus_herb_flow$ant_t == "liom"],na.rm = T), max(cactus_herb_flow$logsize_t1[cactus_herb_flow$ant_t == "liom"],na.rm = T), by = 0.1)
liom_prob <- coef(mod2)[1] + coef(mod2)[4] + coef(mod2)[5]*size_liom
size_crem <- seq(min(cactus_herb_flow$logsize_t1[cactus_herb_flow$ant_t == "crem"],na.rm = T), max(cactus_herb_flow$logsize_t1[cactus_herb_flow$ant_t == "crem"],na.rm = T), by = 0.1)
crem_prob <- coef(mod2)[1] + coef(mod2)[3] + coef(mod2)[5]*size_crem
size_other <- seq(min(cactus_herb_flow$logsize_t1[cactus_herb_flow$ant_t == "other"],na.rm = T), max(cactus_herb_flow$logsize_t1[cactus_herb_flow$ant_t == "other"],na.rm = T), by = 0.1)
other_prob <- coef(mod2)[1] + coef(mod2)[2] + coef(mod2)[5]*size_other
## Plot it
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("herb_ant_size_flow.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(0.7,1.4,1.4), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.5,"Growth Rates of Cacti \nof by Ant State and Size",cex=2,font=2)
# Other 
plot(herb_other$logsize_t1,herb_other$herb_YN)
lines(size_other, invlogit(other_prob), col = othercol, lwd = 2)
# Liom
plot(herb_liom$logsize_t1,herb_liom$herb_YN)
lines(size_liom, invlogit(liom_prob), col = liomcol, lwd = 2)
# Crem 
plot(herb_crem$logsize_t1,herb_crem$herb_YN)
lines(size_crem, invlogit(crem_prob), col = cremcol, lwd = 2)
# Vac 
plot(herb_vac$logsize_t1,herb_vac$herb_YN)
lines(size_vac, invlogit(vac_prob), col = vaccol, lwd = 2)
## All together
plot(size_other, invlogit(other_prob), col = othercol, lwd = 2, type = "l", ylim = c(0.05,0.3))
lines(size_liom, invlogit(liom_prob), col = liomcol, lwd = 2)
lines(size_crem, invlogit(crem_prob), col = cremcol, lwd = 2)
lines(size_vac, invlogit(vac_prob), col = vaccol, lwd = 2)
legend("topleft", legend = c("Other","Crem.","Liom.","Vacant"), col = c(othercol,cremcol,liomcol,vaccol), pch = 16)
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Herbivory",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()

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
## Size dummies for each subset
size_crem <- seq(min(y_crem_subset_grow$logsize_t, na.rm = TRUE), max(y_crem_subset_grow$logsize_t, na.rm = TRUE), by = 0.1)
size_liom <- seq(min(y_liom_subset_grow$logsize_t, na.rm = TRUE), max(y_liom_subset_grow$logsize_t, na.rm = TRUE), by = 0.1)
size_other <- seq(min(y_other_subset_grow$logsize_t, na.rm = TRUE), max(y_other_subset_grow$logsize_t, na.rm = TRUE), by = 0.1)
size_vac <- seq(min(y_vac_subset_grow$logsize_t, na.rm = TRUE), max(y_vac_subset_grow$logsize_t, na.rm = TRUE), by = 0.1)

grow_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow_outputs.csv", header = TRUE,stringsAsFactors=T)
## Formulas
## Other
y_other_mean_grow <- quantile(grow_out$beta0.2,0.5) + size_other * quantile(grow_out$beta1.2,0.5)
y_other_low_grow <- quantile(grow_out$beta0.2,0.05) + size_other * quantile(grow_out$beta1.2,0.05)
y_other_high_grow <- quantile(grow_out$beta0.2,0.95) + size_other * quantile(grow_out$beta1.2,0.95)
## Crem
y_crem_mean_grow <- quantile(grow_out$beta0.3,0.5) + size_crem * quantile(grow_out$beta1.3,0.5)
y_crem_low_grow <- quantile(grow_out$beta0.3,0.05) + size_crem * quantile(grow_out$beta1.3,0.05)
y_crem_high_grow <- quantile(grow_out$beta0.3,0.95) + size_crem * quantile(grow_out$beta1.3,0.95)
## Liom
y_liom_mean_grow <- quantile(grow_out$beta0.4,0.5) + size_liom * quantile(grow_out$beta1.4,0.5)
y_liom_low_grow <- quantile(grow_out$beta0.4,0.05) + size_liom * quantile(grow_out$beta1.4,0.05)
y_liom_high_grow <- quantile(grow_out$beta0.4,0.95) + size_liom * quantile(grow_out$beta1.4,0.95)
## Vac
y_vac_mean_grow <- quantile(grow_out$beta0.1,0.5) + size_vac * quantile(grow_out$beta1.1,0.5)
y_vac_low_grow <- quantile(grow_out$beta0.1,0.05) + size_vac * quantile(grow_out$beta1.1,0.05)
y_vac_high_grow <- quantile(grow_out$beta0.1,0.95) + size_vac * quantile(grow_out$beta1.1,0.95)

## Panels 2
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("grow_panel.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(1,1.4,1.4), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.3,"Crem. Offer Higher Growth \n Rates for Small Cacti
",cex=4,font=2)
# Other (2)
samp <- sample(nrow(grow_out), 50)
plot(x = size_other  ,y = y_other_mean_grow, type = "l", col = othercol, lwd = 4,
     main = "Other",cex.main = 2) 
for(i in 1:samp){
  lines(x = size_other, y = (grow_out$beta0.2[i] + size_other * grow_out$beta1.2[i]),col = "lightgrey", alpha = 0.1)
}
points(y_other_subset_grow$logsize_t,y_other_subset_grow$logsize_t1,pch=16,col= alpha("black", 0.4))
lines(x = size_other  ,y = y_other_mean_grow, type = "l", col = othercol, lwd = 4)
# Crem (3)
plot(x = size_crem  ,y = y_crem_mean_grow, type = "l", col = cremcol, lwd = 4,
     main = "Crem.",cex.main = 2) 
for(i in 1:samp){
  lines(x = size_crem, y = (grow_out$beta0.3[i] + size_crem * grow_out$beta1.3[i]),col = "lightgrey", alpha = 0.1)
}
points(y_crem_subset_grow$logsize_t,y_crem_subset_grow$logsize_t1,pch=16,col= alpha("black", 0.4))
lines(x = size_crem  ,y = y_crem_mean_grow, type = "l", col = cremcol, lwd = 4)
# Liom (4)
plot(x = size_liom  ,y = y_liom_mean_grow, type = "l", col = liomcol, lwd = 4,
     main = "Liom.",cex.main = 2)
for(i in 1:samp){
  lines(x = size_liom, y = (grow_out$beta0.4[i] + size_liom * grow_out$beta1.4[i]),col = "lightgrey", alpha = 0.1)
}
points(y_liom_subset_grow$logsize_t,y_liom_subset_grow$logsize_t1,pch=16,col= alpha("black", 0.4))
lines(x = size_liom  ,y = y_liom_mean_grow, type = "l", col = liomcol, lwd = 4)
# Vacant (1)s
plot(x = size_vac  ,y = y_vac_mean_grow, type = "l", col = vaccol, lwd = 4,
     main = "Vacant",cex.main=2) 
for(i in 1:samp){
  lines(x = size_vac, y = (grow_out$beta0.1[i] + size_vac * grow_out$beta1.1[i]),col = "lightgrey", alpha = 0.1)
}
points(y_vac_subset_grow$logsize_t,y_vac_subset_grow$logsize_t1,pch=16,col= alpha("black", 0.4))
lines(x = size_vac  ,y = y_vac_mean_grow, type = "l", col = vaccol, lwd = 4)

# All together
plot(x = size_other  ,y = y_other_mean_grow, type = "l",lwd = 2,col = othercol,
     main = "All Ants",cex.main = 2,
     ylim = c(9,11), xlim = c(9,11))  
  lines(x = size_crem, y = y_crem_mean_grow, type = "l", col = cremcol, lwd = 2) 
  lines(x = size_liom, y = y_liom_mean_grow, type = "l", col = liomcol,lwd = 2) 
  lines(x = size_vac, y = y_vac_mean_grow, type = "l", col = vaccol, lwd = 2) 
  lines(x = size_dummy, y = size_dummy, type= "l", col = "darkgrey", lwd = 2, lty = 2)
  abline(a = 1, b = 1, col = "darkgrey", lty = 20)
legend("topleft", legend = c("Other","Crem.","Liom.","Vacant"), col = c(othercol,cremcol,liomcol,vaccol), pch = 16,
       cex=1.6)
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.5)
mtext("Log(Volume) year t+1",side=2,line=0,outer=TRUE,cex=1.5,las=0)
dev.off()

## Contour plot ###############################################
## Try 1
x <- seq(min(cactus$logsize_t, na.rm = T),max(cactus$logsize_t,na.rm = T), length = 25); # three columns
y <- seq(min(cactus$logsize_t1, na.rm = T),max(cactus$logsize_t1,na.rm = T), length = 25); # five rows

z <- 2
rand <- function(x,y,z){
  return((x+y)*z)
}

crem <- outer (
  y,     # First dimension:  the columns (y)
  x,     # Second dimension: the rows    (x)
  function (x, y)   dnorm(y,mean = quantile(grow_out$beta0.3,0.5) + x * quantile(grow_out$beta1.3,0.5), sd = mean(quantile(grow_out$d_0,0.5) + x * quantile(grow_out$d_size,0.5)))
);

outer(
  y,x,
  rand,z
)

liom <- outer (
  y,     # First dimension:  the columns (y)
  x,     # Second dimension: the rows    (x)
  function (x, y)   dnorm(y,mean = quantile(grow_out$beta0.4,0.5) + x * quantile(grow_out$beta1.4,0.5), sd = mean(quantile(grow_out$d_0,0.5) + x * quantile(grow_out$d_size,0.5)))
);
other <- outer (
  y,     # First dimension:  the columns (y)
  x,     # Second dimension: the rows    (x)
  function (x, y)   dnorm(y,mean = quantile(grow_out$beta0.2,0.5) + x * quantile(grow_out$beta1.2,0.5), sd = mean(quantile(grow_out$d_0,0.5) + x * quantile(grow_out$d_size,0.5)))
);
vacant <- outer (
  y,     # First dimension:  the columns (y)
  x,     # Second dimension: the rows    (x)
  function (x, y)   dnorm(y,mean = quantile(grow_out$beta0.1,0.5) + x * quantile(grow_out$beta1.1,0.5), sd = mean(quantile(grow_out$d_0,0.5) + x * quantile(grow_out$d_size,0.5)))
);


## Try a heatmap method.
heatmap(crem) ## Fine, except the axes are not ordered
heatmap(crem,Colv = NA, Rowv = NA, 
        xlab = "Log(Volume) in Year t", ylab = "Log(Volume) in Year t+1", main = "")
par(mfrow = c(2,2))
## Crem
heatmap(crem,Colv = NA, Rowv = NA, 
        xlab = "Log(Volume) in Year t", ylab = "Log(Volume) in Year t+1", main = "Crem Tended"
        ,col = gray.colors(24, start =1, end = 0))
## Liom
heatmap(liom,Colv = NA, Rowv = NA, 
        xlab = "Log(Volume) in Year t", ylab = "Log(Volume) in Year t+1", main = "Liom Tended"
        ,col = gray.colors(24, start =1, end = 0))
## Other
heatmap(other,Colv = NA, Rowv = NA, 
        xlab = "Log(Volume) in Year t", ylab = "Log(Volume) in Year t+1", main = "Other Tended"
        ,col = gray.colors(24, start =1, end = 0))
## Vacant
heatmap(vacant,Colv = NA, Rowv = NA, 
        xlab = "Log(Volume) in Year t", ylab = "Log(Volume) in Year t+1", main = "Not Tended"
        ,col = gray.colors(24, start =1, end = 0))

## Try with filled.contour
## Crem
filled.contour(x,y,crem, xlab = "",ylab = "", main = "Crem. Tended",col = gray.colors(24, start =1, end = 0))
## Liom
filled.contour(x,y,liom, xlab = "",ylab = "", main = "Liom. Tended",col = gray.colors(24, start =1, end = 0))
## Other
filled.contour(x,y,other, xlab = "",ylab = "", main = "Other Tended",col = gray.colors(24, start =1, end = 0))
## Vacant
filled.contour(x,y,crem, xlab = "",ylab = "", main = " Not Tended",col = gray.colors(24, start =1, end = 0))


x <- seq(min(cactus$logsize_t, na.rm = T),max(cactus$logsize_t,na.rm = T), length = 25); # three columns
y <- seq(min(cactus$logsize_t1, na.rm = T),max(cactus$logsize_t1,na.rm = T), length = 25); # five rows
## Contour plots
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("grow_contour_lines.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(0.7,1.4,1.4), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.5,"Growth Rates of Cacti \nof by Ant State and Size",cex=2,font=2)
## Crem
contour(x,y,crem, nlevels = 7
        , xlim = c(2,10), ylim = c(0,10)
        , col = gray.colors(10, start =1, end = 0))
points(y_crem_subset_grow$logsize_t,y_crem_subset_grow$logsize_t1,pch=16,col= alpha("black", 0.4))
lines(size_crem, y_crem_mean_grow, col = othercol, lwd = 3)
## Liom
contour(x,y,liom, nlevels = 8
        , xlim = c(2,10), ylim = c(0,10)
        , col = gray.colors(10, start =1, end = 0))
points(y_liom_subset_grow$logsize_t,y_liom_subset_grow$logsize_t1,pch=16,col= alpha("black", 0.4))
lines(size_liom, y_liom_mean_grow, col = liomcol, lwd = 3)
## Other
contour(x,y,crem, nlevels = 8
        , xlim = c(2,10), ylim = c(0,10)
        , col = gray.colors(10, start =1, end = 0))
points(y_other_subset_grow$logsize_t,y_other_subset_grow$logsize_t1,pch=16,col= alpha("black", 0.4))
lines(size_other, y_other_mean_grow, col = othercol, lwd = 3)
## Vacant
contour(x,y,crem, nlevels = 8
        , xlim = c(2,10), ylim = c(0,10)
        , col = gray.colors(10, start =1, end = 0))
points(y_vac_subset_grow$logsize_t,y_vac_subset_grow$logsize_t1,pch=16,col= alpha("black", 0.4))
lines(size_vac, y_vac_mean_grow, col = vaccol, lwd = 3)
## All together
plot(size_crem, y_crem_mean_grow, type = "l", col = cremcol, lwd = 3, xlim = c(5,6), ylim = c(5.3,6.5))
lines(size_liom, y_liom_mean_grow, col = liomcol, lwd = 3)
lines(size_other, y_other_mean_grow, col = othercol, lwd = 3)
lines(size_vac, y_vac_mean_grow, col = vaccol, lwd = 3)
lines(size_vac, size_vac, col = "grey", lty = 2)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c(othercol,cremcol,liomcol,vaccol), pch = 16)
mtext("Log(Volume) in Year t",side=1,line=0,outer=TRUE,cex=1.0)
mtext("Log(Volume) in Year t+1",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
## Now without the mean lines
png("grow_contour_lines_color.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(0.7,1.4,1.4), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.5,"Growth Rates of Cacti \nof by Ant State and Size",cex=2,font=2)
## Crem
contour(x,y,crem, nlevels = 10, col = gray.colors(10, start =1, end = 0), xlim = c(2,10), ylim = c(0,10)) 
## Liom
contour(x,y,liom, nlevels = 10, col = gray.colors(10, start =1, end = 0), xlim = c(2,10), ylim = c(0,10))
## Other
contour(x,y,crem, nlevels = 10, col = gray.colors(10, start =1, end = 0), xlim = c(2,10), ylim = c(0,10))
## Vacant
contour(x,y,crem, nlevels = 10, col = gray.colors(10, start =1, end = 0), xlim = c(2,10), ylim = c(0,10))
## All together
plot(size_crem, y_crem_mean_grow, type = "l", col = cremcol, lwd = 3, xlim = c(5,6), ylim = c(5.3,6.5))
lines(size_liom, y_liom_mean_grow, col = liomcol, lwd = 3)
lines(size_other, y_other_mean_grow, col = othercol, lwd = 3)
lines(size_vac, y_vac_mean_grow, col = vaccol, lwd = 3)
lines(size_vac, size_vac, col = "grey", lty = 2)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c(othercol,cremcol,liomcol,vaccol), pch = 16)
legend("topleft", legend = c("1","0"), fill = c("black","white"))
mtext("Log(Volume)",side=1,line=0,outer=TRUE,cex=1.0)
mtext("Probability of Survival",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()

## GGplot 
data <- expand.grid(x,y)
colnames(data) <- c("x","y")
data$c_prob <- NA
for(i in 1:length(data$x)){
  data$c_prob[i] <- dnorm(data$y[i], quantile(grow_out$beta0.3,0.5) + data$x[i] * quantile(grow_out$beta1.3,0.5), sd = quantile(grow_out$sigma,0.5))
}
data$c_prob = dnorm(data$y,quantile(grow_out$beta0.3,0.5) + data$x * quantile(grow_out$beta1.3,0.5), sd = quantile(grow_out$sigma,0.5))
ggplot(data, aes(x = x, y = y, col = c_prob)) + 
  geom_tile() +scale_color_gradient(low="white", high="black") + theme_classic()

cactus_grow <- cactus[,c("logsize_t","logsize_t1","ant_t")]
cactus_grow <- subset(cactus_grow, is.na(cactus_grow$ant_t) == F)
cactus_grow$c_prob <- dnorm(cactus_grow$logsize_t1, quantile(grow_out$beta0.3,0.5) + cactus_grow$logsize_t * quantile(grow_out$beta1.3,0.5), sd = quantile(grow_out$sigma,0.5))
ggplot(cactus_grow, aes(x = logsize_t, y = logsize_t1, col = c_prob)) + 
  geom_tile() + 
  facet_grid(cactus_grow$ant_t)

## Show the correlation between ant and year -- from growth model random effects
vac_vec <- (colMeans(grow_outputs$w[,1,]))
liom_vec <- colMeans(grow_outputs$w[,4,])
other_vec <- colMeans(grow_outputs$w[,2,])
crem_vec <- colMeans(grow_outputs$w[,3,])
years <- c(2004,2005,2006,2007,2009,2010,2011,2012,2013,2014,2015,2016,2017,2019)

png("year_ant_grow1.png")
pairs(data.frame(cbind(vac_vec,liom_vec,other_vec,crem_vec)))
dev.off()

png("year_ant_grow2.png")
corrplot(cor(cbind(vac_vec,liom_vec,other_vec,crem_vec)), method = "color", 
         main = "Correlation of Ant and Year Effects")
dev.off()

png("year_ant_grow3.png")
corrplot(cor(cbind(vac_vec,liom_vec,other_vec,crem_vec)), method = "color", main = "Correlation of Ant and Year Effects", diag = F)
dev.off()

png("year_ant.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,2,3,4,5),
              ncol = 2, byrow = TRUE), heights = c(0.7,1.4,1.4), widths = c(3.9,3.9))
plot.new()
text(0.5,0.5,"Interaction of Ant State and Year",cex=2,font=2)
barplot(vac_vec,
        main = "Vacant Cacti", xlab = "Year", ylab = "Year-Ant Factor")
barplot(liom_vec,
        main = "Liom. Tended Cacti", xlab = "Year", ylab = "Year-Ant Factor")
barplot(crem_vec,
        main = "Crem. Tended Cacti", xlab = "Year", ylab = "Year-Ant Factor")
barplot(other_vec,
        main = "Other Tended Cacti", xlab = "Year", ylab = "Year-Ant Factor")
mtext("Year",side=1,line=0,outer=TRUE,cex=1.0)
mtext("Year-Ant Factor",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()

png("grow_year_ant_timeseries.png")
plot(years,liom_vec,col = liomcol, cex.main = 2,type = "b",lwd = 4, pch = 16,cex = 2,
     main = "Liom. and Crem. Effects on Growth are \n Closely Coupled",
     ylim = c(-0.4,0.4), xlab = "Years",ylab = "Year-Ant Effect on Growth",cex.lab = 1.5)
lines(years, crem_vec, type = "b", col = cremcol, lwd = 4, pch = 16, cex = 2)
legend("topright",legend = c("Liom.","Crem."),fill = c(liomcol,cremcol))
dev.off()
###########################################################################################################################
#### Survival Visuals #####################################################################################################
#########################################################################################################################
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
## Extract & Format Data
survival_data_orig <- subset(cactus, is.na(Survival_t1) == FALSE,c("Plot","Year_t","Survival_t1","ant_t","logsize_t"))
survival_data_orig <- cactus[,c("Plot","Year_t","Survival_t1","ant_t","logsize_t")]
survival_data <- na.omit(survival_data_orig)
#extract from original ddata
surv_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/surv_outputs.csv", header = TRUE,stringsAsFactors=T)
## Create subsets for each ant species
y_crem_subset_surv <- subset(survival_data, ant_t == "crem")
y_liom_subset_surv <- subset(survival_data, ant_t == "liom")
y_vac_subset_surv <- subset(survival_data, ant_t == "vacant")
y_other_subset_surv <- subset(survival_data, ant_t == "other")
#Size Dummies for every ant
size_dummy <- seq(min(survival_data$logsize_t, na.rm = TRUE), max(survival_data$logsize_t, na.rm = TRUE), by = 0.1)
size_crem = seq(min((y_crem_subset_surv$logsize_t), na.rm = TRUE), max ((y_crem_subset_surv$logsize_t), na.rm = TRUE), by = 0.1)
size_other = seq(min((y_other_subset_surv$logsize_t), na.rm = TRUE), max ((y_other_subset_surv$logsize_t), na.rm = TRUE), by = 0.1)
size_liom = seq(min((y_liom_subset_surv$logsize_t), na.rm = TRUE), max ((y_liom_subset_surv$logsize_t), na.rm = TRUE), by = 0.1)
size_vac = seq(min((y_vac_subset_surv$logsize_t), na.rm = TRUE), max ((y_vac_subset_surv$logsize_t), na.rm = TRUE), by = 0.1)
## Formulas -- mean, 95% and 5% percentiles
## Other -- 2
y_other_surv = quantile(surv_out$beta0.2,0.5) + size_other * quantile(surv_out$beta1.2,0.5)
y_other_low_surv = quantile(surv_out$beta0.2,0.05) + size_other * quantile(surv_out$beta1.2,0.05)
y_other_high_surv = quantile(surv_out$beta0.2,0.95) + size_other * quantile(surv_out$beta1.2,0.95)
other_extr = quantile(surv_out$beta0.2,0.5) + size_dummy * quantile(surv_out$beta1.2,0.5)
## Crem -- 3
y_crem_surv = quantile(surv_out$beta0.3,0.5) + size_crem * quantile(surv_out$beta1.3,0.5)
y_crem_low_surv = quantile(surv_out$beta0.3,0.05) + size_crem * quantile(surv_out$beta1.3,0.05)
y_crem_high_surv = quantile(surv_out$beta0.3,0.95) + size_crem * quantile(surv_out$beta1.3,0.95)
crem_extr = quantile(surv_out$beta0.3,0.5) + size_dummy * quantile(surv_out$beta1.3,0.5)
## Liom -- 4
y_liom_surv = quantile(surv_out$beta0.4,0.5) + size_liom * quantile(surv_out$beta1.4,0.5)
y_liom_low_surv = quantile(surv_out$beta0.4,0.05) + size_liom * quantile(surv_out$beta1.4,0.05)
y_liom_high_surv = quantile(surv_out$beta0.4,0.95) + size_liom * quantile(surv_out$beta1.4,0.95)
liom_extr = quantile(surv_out$beta0.4,0.5) + size_dummy * quantile(surv_out$beta1.4,0.5)
## Vac -- 1
y_vac_surv = quantile(surv_out$beta0.1,0.5) + size_vac * quantile(surv_out$beta1.1,0.5)
y_vac_low_surv = quantile(surv_out$beta0.1,0.05) + size_vac * quantile(surv_out$beta1.1,0.05)
y_vac_high_surv = quantile(surv_out$beta0.1,0.95) + size_vac * quantile(surv_out$beta1.1,0.95)
vac_extr = quantile(surv_out$beta0.1,0.5) + size_dummy * quantile(surv_out$beta1.1,0.5)
## Bin the size data
## Crem
surv_plot_crem <- y_crem_subset_surv %>% 
  mutate(size_bin = cut_interval((logsize_t),10)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t),na.rm=T),
            surv = mean(Survival_t1,na.rm=T),
            N = length(logsize_t))
surv_plot_crem$N_mod <- log(surv_plot_crem$N)
## Liom
surv_plot_liom <- y_liom_subset_surv %>% 
  mutate(size_bin = cut_interval((logsize_t),10)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t),na.rm=T),
            surv = mean(Survival_t1,na.rm=T),
            N = length(logsize_t))
surv_plot_liom$N_mod <- log(surv_plot_liom$N)
## Other
surv_plot_other <- y_other_subset_surv %>% 
  mutate(size_bin = cut_interval((logsize_t),10)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t),na.rm=T),
            surv = mean(Survival_t1,na.rm=T),
            N = length(logsize_t))
surv_plot_other$N_mod <- log(surv_plot_other$N)
## Vac
surv_plot_vac <- y_vac_subset_surv %>% 
  mutate(size_bin = cut_interval((logsize_t),10)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t),na.rm=T),
            surv = mean(Survival_t1,na.rm=T),
            N = length(logsize_t))
surv_plot_vac$N_mod <- log(surv_plot_vac$N)
## Look at the actual raw proportions of dead and alive plants
tab<-table(survival_data$Survival_t1, survival_data$ant_t)
tab[,1]/colSums(tab)[1] ## Vac
tab[,2]/colSums(tab)[2] ## Other
tab[,3]/colSums(tab)[3] ## Crem
tab[,4]/colSums(tab)[4] ## Liom
mean(invlogit(surv_out_ant$beta0.1)) ## Vac
mean(invlogit(surv_out_ant$beta0.2)) ## Other
mean(invlogit(surv_out_ant$beta0.3)) ## Crem
mean(invlogit(surv_out_ant$beta0.4)) ## Liom
## Panel Plots
png("surv_panels.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(0.7,1.4,1.4), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.25,"Crem. Offer Higher Survival \n Rates for Small Cacti",cex=4,font=2)
# Crem
plot(x = size_crem  ,y = invlogit(y_crem_surv), type = "l", col = cremcol, lwd = 4, ylim = c(0,1), xlim = c(-5,15),
     cex.main = 2, main = "Crem.")
points(surv_plot_crem$mean_size,surv_plot_crem$surv,pch=16,cex=surv_plot_crem$N_mod,col= alpha(cremcol, 0.4))
polygon(c(size_crem,rev(size_crem)),c(invlogit(y_crem_high_surv), rev(invlogit(y_crem_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Liom
plot(x = size_liom  ,y = invlogit(y_liom_surv), type = "l", col = liomcol, lwd = 4, ylim = c(0,1), xlim = c(-5,15),
     cex.main = 2, main = "Liom.")
points(surv_plot_liom$mean_size,surv_plot_liom$surv,pch=16,cex=surv_plot_liom$N_mod,col= alpha(liomcol, 0.4))
polygon(c(size_liom,rev(size_liom)),c(invlogit(y_liom_high_surv), rev(invlogit(y_liom_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Other
plot(x = size_other  ,y = invlogit(y_other_surv), type = "l", col = othercol, lwd = 4, ylim = c(0.3,1), xlim = c(-5,15),
     cex.main = 2, main = "Other")
points(surv_plot_other$mean_size,surv_plot_other$surv,pch=16,cex=surv_plot_other$N_mod,col= alpha(othercol, 0.4))
polygon(c(size_other,rev(size_other)),c(invlogit(y_other_high_surv), rev(invlogit(y_other_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Vac
plot(x = size_vac  ,y = invlogit(y_vac_surv), type = "l", col = vaccol, lwd = 4, ylim = c(0,1), xlim = c(-5,15),
     cex.main = 2, main = "Vacant")
points(surv_plot_vac$mean_size,surv_plot_vac$surv,pch=16,cex=surv_plot_vac$N_mod,col= alpha(vaccol, 0.4))
polygon(c(size_vac,rev(size_vac)),c(invlogit(y_vac_high_surv), rev(invlogit(y_vac_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# All Together
plot(x = size_dummy, y = invlogit(other_extr), type = "l", col = othercol, lwd = 2, ylim = c(0,1), lty = 2, xlim = c(-5,15),
     cex.main = 2,main = "All Ants")
lines(x = size_dummy, y = invlogit(crem_extr), col = cremcol,lwd = 2, lty = 2)
lines(x = size_dummy, y = invlogit(liom_extr), col = liomcol, lwd = 2, lty = 2)
lines(x = size_dummy, y = invlogit(vac_extr), col = vaccol, lwd = 2, lty = 2)
lines(x = size_other, y = invlogit(y_other_surv), col = othercol, lwd = 2)
lines(x = size_crem, y = invlogit(y_crem_surv), col = cremcol, lwd = 2)
lines(x = size_liom, y = invlogit(y_liom_surv), col = liomcol, lwd = 2)
lines(x = size_vac, y = invlogit(y_vac_surv), col = vaccol, lwd = 2)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c(othercol,cremcol,liomcol,vaccol), pch = 16,
       cex = 2)
mtext("Log(Volume)",side=1,line=0,outer=TRUE,cex=1.0)
mtext("Probability of Survival",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()

png("surv_panels_cropped.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(1,1.4,1.4), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.5,"Crem. Offer Higher Survival \n Rates for Small Cacti",cex=4,font=2)
# Crem
plot(x = size_crem  ,y = invlogit(y_crem_surv), type = "l", col = cremcol, lwd = 4, ylim = c(0.6,1), xlim = c(1,15),
     cex.main = 2, main = "Crem.")
points(surv_plot_crem$mean_size,surv_plot_crem$surv,pch=16,cex=surv_plot_crem$N_mod,col= alpha(cremcol, 0.4))
#lines(x = size_dummy, y = invlogit(y_crem_low_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_crem_high_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_crem,rev(size_crem)),c(invlogit(y_crem_high_surv), rev(invlogit(y_crem_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Liom
plot(x = size_liom  ,y = invlogit(y_liom_surv), type = "l", col = liomcol, lwd = 4, ylim = c(0.6,1), xlim = c(1,15),
     cex.main = 2, main = "Liom.")
points(surv_plot_liom$mean_size,surv_plot_liom$surv,pch=16,cex=surv_plot_liom$N_mod,col= alpha(liomcol, 0.4))
#lines(x = size_dummy, y = invlogit(y_liom_low_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_liom_high_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)# Vacant
polygon(c(size_liom,rev(size_liom)),c(invlogit(y_liom_high_surv), rev(invlogit(y_liom_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Other
plot(x = size_other  ,y = invlogit(y_other_surv), type = "l", col = othercol, lwd = 4, ylim = c(0.6,1), xlim = c(1,15),
     cex.main = 2, main = "Other")
points(surv_plot_other$mean_size,surv_plot_other$surv,pch=16,cex=surv_plot_other$N_mod,col= alpha(othercol, 0.4))
#lines(x = size_dummy, y = invlogit(y_other_low_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_other_high_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_other,rev(size_other)),c(invlogit(y_other_high_surv), rev(invlogit(y_other_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Vac
plot(x = size_vac  ,y = invlogit(y_vac_surv), type = "l", col = vaccol, lwd = 4, ylim = c(0.2,1), xlim = c(-5,15),
     cex.main = 2, main = "Vacant")
points(surv_plot_vac$mean_size,surv_plot_vac$surv,pch=16,cex=surv_plot_vac$N_mod,col= alpha(vaccol, 0.4))
#lines(x = size_dummy, y = invlogit(y_vac_low_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_vac_high_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_vac,rev(size_vac)),c(invlogit(y_vac_high_surv), rev(invlogit(y_vac_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# All Together
plot(x = size_dummy, y = invlogit(other_extr), type = "l", col = othercol, lwd = 2, ylim = c(0.6,1), lty = 2, xlim = c(1,15),
     cex.main = 2, main = "All Ants")
lines(x = size_dummy, y = invlogit(crem_extr), col = cremcol,lwd = 2, lty = 2)
lines(x = size_dummy, y = invlogit(liom_extr), col = liomcol, lwd = 2, lty = 2)
lines(x = size_dummy, y = invlogit(vac_extr), col = vaccol, lwd = 2, lty = 2)
lines(x = size_other, y = invlogit(y_other_surv), col = othercol, lwd = 2)
lines(x = size_crem, y = invlogit(y_crem_surv), col = cremcol, lwd = 2)
lines(x = size_liom, y = invlogit(y_liom_surv), col = liomcol, lwd = 2)
lines(x = size_vac, y = invlogit(y_vac_surv), col = vaccol, lwd = 2)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c(othercol,cremcol,liomcol,vaccol), pch = 16,
       cex = 1.6)
mtext("Log(Volume)",side=1,line=0,outer=TRUE,cex=1.7)
mtext("Probability of Survival",side=2,line=0,outer=TRUE,cex=1.7,las=0)
dev.off()

## Panels 2
png("surv_panel_lines.png")
par(mar=c(5,5,0,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(1,2,2), widths = c(4,4,4))
plot.new()
text(0.5,0.5,"Survival Rates of Cacti \nof by Ant State and Size",cex=2,font=2)
# Other (3)
samp <- sample(nrow(surv_out), 50)
plot(x = size_other  ,y = invlogit(y_other_surv), type = "l", col = othercol, lwd = 4, ylim = c(0,1),xlim = c(-5,15), xlab = "",ylab = "")
for(i in 1:1500){
  lines(x = size_other, y = invlogit(surv_out$beta0.2[i] + size_other * surv_out$beta1.2[i]),col = "lightgrey", alpha = 0.1)
}
points(surv_plot_other$mean_size,surv_plot_other$surv,pch=16,cex=surv_plot_other$N_mod,col= alpha(othercol, 0.4))
lines(x = size_other  ,y = invlogit(y_other_surv), type = "l", col = othercol, lwd = 4)
# Crem (1)
plot(x = size_crem  ,y = invlogit(y_crem_surv), type = "l", col = cremcol, lwd = 4, ylim = c(0,1),xlim = c(-5,15),xlab = "",ylab = "") 
for(i in 1:1500){
  lines(x = size_crem, y = invlogit(surv_out$beta0.3[i] + size_crem * surv_out$beta1.3[i]),col = "lightgrey", alpha = 0.1)
}
points(surv_plot_crem$mean_size,surv_plot_crem$surv,pch=16,cex=surv_plot_crem$N_mod,col= alpha(cremcol, 0.4))
lines(x = size_crem  ,y = invlogit(y_crem_surv), type = "l", col = cremcol, lwd = 4)
# Liom (2)
plot(x = size_liom  ,y = invlogit(y_liom_surv), type = "l", col = liomcol, lwd = 4, ylim = c(0,1),xlim = c(-5,15),xlab = "",ylab = "")
for(i in 1:1500){
  lines(x = size_liom, y = invlogit(surv_out$beta0.2[i] + size_liom * surv_out$beta1.2[i]),col = "lightgrey", alpha = 0.1)
}
points(surv_plot_liom$mean_size,surv_plot_liom$surv,pch=16,cex=surv_plot_liom$N_mod,col= alpha(liomcol, 0.4))
lines(x = size_liom  ,y = invlogit(y_liom_surv), type = "l", col = liomcol, lwd = 4)
# Vacant (4)s
plot(x = size_vac  ,y = invlogit(y_vac_surv), type = "l", col = vaccol, lwd = 4, ylim = c(0,1),xlim = c(-5,15),xlab = "",ylab = "") 
for(i in 1:1500){
  lines(x = size_vac, y = invlogit(surv_out$beta0.4[i] + size_vac * surv_out$beta1.4[i]),col = "lightgrey", alpha = 0.1)
}
points(surv_plot_vac$mean_size,surv_plot_vac$surv,pch=16,cex=surv_plot_vac$N_mod,col= alpha(vaccol, 0.4))
lines(x = size_vac  ,y = invlogit(y_vac_surv), type = "l", col = vaccol, lwd = 4)
  # All together
plot(x = size_dummy, y = invlogit(other_extr), type = "l", col = othercol, lwd = 2, ylim = c(0,1), lty = 2, xlab = "", ylab = "")
lines(x = size_dummy, y = invlogit(crem_extr), col = cremcol,lwd = 2, lty = 2)
lines(x = size_dummy, y = invlogit(liom_extr), col = liomcol, lwd = 2, lty = 2)
lines(x = size_dummy, y = invlogit(vac_extr), col = vaccol, lwd = 2, lty = 2)
lines(x = size_other, y = invlogit(y_other_surv), col = othercol, lwd = 2)
lines(x = size_crem, y = invlogit(y_crem_surv), col = cremcol, lwd = 2)
lines(x = size_liom, y = invlogit(y_liom_surv), col = liomcol, lwd = 2)
lines(x = size_vac, y = invlogit(y_vac_surv), col = vaccol, lwd = 2)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c(othercol,cremcol,liomcol,vaccol), pch = 16)
mtext("Log(Volume)",side=1,line=0,outer=TRUE,cex=1.3)
mtext("Probability of Survival",side=2,line=0,outer=TRUE,cex=1.3,las=0)
dev.off()

############
## Show the correlation between ant and year -- from growth model random effects
vac_vec <- (colMeans(surv_outputs$w[,1,]))
liom_vec <- colMeans(surv_outputs$w[,4,])
other_vec <- colMeans(surv_outputs$w[,2,])
crem_vec <- colMeans(surv_outputs$w[,3,])
years <- c(2004,2005,2006,2007,2009,2010,2011,2012,2013,2014,2015,2016,2017,2019)
png("surv_year_ant_timeseries.png")
plot(years,liom_vec,col = liomcol, cex.main = 2,type = "b",lwd = 4, pch = 16,cex = 2,
     main = "Liom. and Crem. Effects on Growth are \n Decoupled in Some Years",
     ylim = c(-1,1), xlab = "Years",ylab = "Year-Ant Effect on Growth",cex.lab = 1.5)
lines(years, crem_vec, type = "b", col = cremcol, lwd = 4, pch = 16, cex = 2)
legend("topright",legend = c("Liom.","Crem."),fill = c(liomcol,cremcol))
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
## Bin the data
flow_plot <- flower_data %>% 
  mutate(size_bin = cut_interval((logsize_t),10)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t),na.rm=T),
            tot = mean(TotFlowerbuds_t,na.rm=T),
            N = length(logsize_t),
            N_mod = log(N))
## Plot the probabilities
##Panel Plot
png("flow_panels.png")
par(mar=c(4,4,0,1))
layout(matrix(c(1,2),
              ncol = 1, byrow = TRUE), heights = c(1.2,2))
plot.new()
text(0.5,0.25,"Number of Flowers\nProduced by Size",cex=2,font=2)
plot(x = size_dummy  ,y = exp(y_flow), type = "l", col = "chartreuse4", lwd = 4, ylim = c(0,100), xlab = "Log(Volume)", ylab = "Total Flowerbuds")
points(flow_plot$mean_size,flow_plot$tot,pch=16,cex=flow_plot$N_mod,col= alpha("chartreuse4", 0.4))
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
plot(x = size_dummy  ,y = exp(y_flow), type = "l", col = "chartreuse4", lwd = 4, ylim = c(0,150), xlab = "Log(Volume)",ylab = "Flowerbuds Produced")
for(i in 1:1500){
  lines(x = size_dummy, y = exp(flow_out$beta0[i] + size_dummy * flow_out$beta1[i]),col = "lightgrey", alpha = 0.1)
}
points(flow_plot$mean_size,flow_plot$tot,pch=16,cex=flow_plot$N_mod,col= alpha("chartreuse4", 0.4))
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
nrow(viability_data_orig)
nrow(viability_data)
cactus$viab <- cactus$Goodbuds_t1/cactus$TotFlowerbuds_t1
mean(na.omit(cactus$viab))
#format for overlay plots
#extract from original data
y_subset_good <- viability_data[,c("Goodbuds_t1","ant_t", "logsize_t")]
## Formulas
y_other_viab = invlogit(viab_out$beta0.3)
y_other_low_viab = quantile(viab_out$beta0.3,0.05)
y_other_high_viab = quantile(viab_out$beta0.3,0.95)
y_crem_viab = invlogit(viab_out$beta0.1)
y_crem_low_viab = quantile(viab_out$beta0.1,0.05)
y_crem_high_viab = quantile(viab_out$beta0.1,0.95)
y_liom_viab = invlogit(viab_out$beta0.2) 
y_liom_low_viab = quantile(viab_out$beta0.2,0.05)
y_liom_high_viab = quantile(viab_out$beta0.2,0.95)
y_vac_viab = invlogit(viab_out$beta0.4)
y_vac_low_viab = quantile(viab_out$beta0.4,0.05)
y_vac_high_viab = quantile(viab_out$beta0.4,0.95) 
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

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")

png("viab_bars.png")
barplot(invlogit(means), ylim = c(0,1.1), col = c(vaccol,othercol,cremcol,liomcol),xlab = "Ant Partner", ylab = "Viability Rate",
        main = "Liom. Offers Higher Viability \nRates for Flowerbuds", names.arg = c("Vacant","Other","Crem.","Liom."),
        cex.main = 2.6, cex.lab = 1.5)
dev.of############
## Show the correlation between ant and year -- from growth model random effects
vac_vec <- (colMeans(viab_outputs$w[,1,]))
liom_vec <- colMeans(viab_outputs$w[,4,])
other_vec <- colMeans(viab_outputs$w[,2,])
crem_vec <- colMeans(viab_outputs$w[,3,])
years <- c(2004,2005,2006,2012,2013,2014,2015,2016,2017,2018,2019)
png("viab_year_ant_timeseries.png")
plot(years,liom_vec,col = liomcol, cex.main = 2,type = "b",lwd = 4, pch = 16,cex = 2,
     main = "Liom. and Crem. Effects on Vibaility are \n Decoupled in Some Years",
     ylim = c(-1.5,2.1), xlab = "Years",ylab = "Year-Ant Effect on Growth",cex.lab = 1.5)
lines(years, crem_vec, type = "b", col = cremcol, lwd = 4, pch = 16, cex = 2)
legend("topright",legend = c("Liom.","Crem."),fill = c(liomcol,cremcol))
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
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
seed_uncleaned <- read.csv("Data Analysis/JO_fruit_data_final_dropplant0.csv", header = TRUE,stringsAsFactors=T)
## PEAA = Ant Access
## PAAA = Ant Access
## PEAE = Ant Exclusion
## PAAE = Ant Exclusion
seed <- subset(seed_uncleaned, treatment == "PAAA" | treatment == "PAAE")#make the column for the ant state of the part of the plant producing seeds
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
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
## Subset
subset_crem <- subset(seed_data, seed_data$ant_state == "Crem")
summary(subset_crem$seed_count)
subset_liom <- subset(seed_data, seed_data$ant_state == "Liom")
subset_vac <- subset(seed_data, seed_data$ant_state == "Vacant")
## Formulas
y_crem <- seed_out$beta0.1
summary(exp(y_crem))
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

png("seed_prod_hist.png")
par(mar=c(5,5,0,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(.6,1,1,1))
plot.new()
text(0.5,0.25,"Seeds Produced by Ants",cex=2,font=2)
## Crem
hist(subset_crem$seed_count, freq = F, xlab = "", ylab = "", main = "", ylim = c(0,0.05))
lines(density(exp(y_crem)), type = "l", col = cremcol, lwd = 2)
legend("topleft", legend = c("Crem.","Liom.","Vacant"), col = c(cremcol,liomcol,vaccol), pch = 16)
## Liom
hist(subset_liom$seed_count, freq = F, xlab = "", ylab = "", main = "", ylim = c(0,0.05))
lines(density(exp(y_liom)), type = "l", col = liomcol, lwd = 2)
## Vac
hist(subset_vac$seed_count, freq = F, xlab = "", ylab = "", main = "", ylim = c(0,0.06))
lines(density(exp(y_vac)), type = "l", col = vaccol, lwd = 2)
mtext("Number of Seeds Produced",side=1,line=-2,outer=TRUE,cex=1.3)
mtext("Density",side=2,line=-3,outer=TRUE,cex=1.3,las=0)
dev.off()

png("seeds_bars.png")
plot(exp(seeds$beta) ~ factor(seeds$ant), xlab = "Ant Species",ylab = "Expected Seeds", col = c("red","blue","pink"), main = "Liom. Provides Advantages \n for Seed Production")
dev.off()

########################################################################################################
#### Pre-Census Seed Survival #########################################################################
########################################################################################################
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
precensus.dat.orig<-read.csv("Data Analysis/PrecensusSurvival.csv") 
precensus.dat <- precensus.dat.orig[ , c("Transect","Seed","Log.size","survive0405")]
precensus.dat <- na.omit(precensus.dat)
y_subset <- (precensus.dat)
seed_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/seed_surv_outputs.csv", header = TRUE,stringsAsFactors=T)
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
## Formulas
y_surv = seed_out$beta0
y_low_surv = quantile(seed_out$beta0,0.05) 
y_high_surv = quantile(seed_out$beta0,0.95)
y_surv <- cbind(invlogit(quantile(seed_out$beta0,0.5)), invlogit(quantile(seed_out$beta0,0.05)), invlogit(quantile(seed_out$beta0,0.95)))

png("seed_surv.png")
plot(density(invlogit(y_surv)), col = "chartreuse4",lwd = 2, xlab = "Pre-census Survival Probability", ylab = "Density",main = "Probability of Seedlings\nSurviving to Census")
abline(v = mean(precensus.dat$survive0405), lty = 2)
legend("topright",legend = c("Predicted Pre-census Survival","Real Pre-census Survival"), col = c("chartreuse4","black"), pch = 16)
dev.off()

precensus.dat$survive0405
png("seed_surv_box.png")
boxplot(invlogit(y_surv), col = "chartreuse4", ylim = c(-0.1,1),
        main = "Proportion of Seeds that \n Survive to Census", ylab = "Survival Rate")
dev.off()

################################################################################################################################################
##################### Germination Yr 1&2 Visuals ########################################################################
###############################################################################################################################################
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
germ.dat_orig<-read.csv("Data Analysis/Germination.csv") 
germ.dat_orig$rate <- 0
for(i in 1:nrow(germ.dat_orig)){
  if(germ.dat_orig$Seedlings04[i] != 0){
    germ.dat_orig$rate[i] <- (germ.dat_orig$Seedlings04[i] - germ.dat_orig$Seedlings05[i])/germ.dat_orig$Seedlings04[i]
  }
}
germ.dat <- na.omit(germ.dat_orig)
germ.dat <- germ.dat[-c(42,39,40),]
germ1_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/germ1_outputs.csv", header = TRUE,stringsAsFactors=T)
germ2_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/germ2_outputs.csv", header = TRUE,stringsAsFactors=T)
y_germ1 <- (germ1_out$beta0) 
y_germ2 <- (germ2_out$beta0) 
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")

hist(germ.dat$rate, freq = F)
plot(density(invlogit(y_germ1)))

plot(c(invlogit(y_germ1),invlogit(y_germ2)))

germ <- cbind(y_germ1,y_germ2)
colnames(germ) <- c("Year 1","Year 2")
png("germination.png")
boxplot(invlogit(germ), col = "chartreuse4", names.arg = c("Yr 1","Yr 2"), 
        xlab = "Year in Seedbank", ylab = "Probability of Germinating", main = "Seeds Are More Likely to \n Germinate in Year 1")
dev.off()

plot(density(y_germ1))

png("germ1_hist.png")
hist(germ.dat$Seedlings04)
lines(density(y_germ1))
dev.off()


################################################################################################################################################
##################### Recruit Sizes Visuals ########################################################################
###############################################################################################################################################
seedling.dat_orig <- cactus[,c("logsize_t1","Recruit")]
seedling.dat_orig <- filter(seedling.dat_orig, Recruit == 1)
seedling.dat <- na.omit(seedling.dat_orig)
summary(seedling.dat$logsize_t1)
recruit_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/rec_outputs.csv", header = TRUE,stringsAsFactors=T)
y_rec = quantile(recruit_out$beta0,0.5)
y_rec_low = quantile(recruit_out$beta0,0.05)
y_rec_high = quantile(recruit_out$beta0,0.95)
size_dummy_rec <- seq(min(seedling.dat$logsize_t1), max(seedling.dat$logsize_t1), by = 0.1)
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")


png("rec_size.png")
boxplot(recruit_out$beta0, col = "chartreuse4", ylab = "Log(Volume)", main = "Recruit Size Distribution")
dev.off()

png("rec_size_hist.png")
hist(seedling.dat$logsize_t1, freq = FALSE, xlab = "Log(Volume) of Recruits", ylab = "Density", main = "Size Distribution of New Recruits",
     ylim = c(0,0.5))
lines(x = size_dummy_rec, y = dnorm(x = size_dummy_rec, mean = y_rec, sd = mean(grow_out$sigma)), col = "chartreuse4", lwd = 2)
dev.off()

boxplot(seedling.dat$logsize_t1)

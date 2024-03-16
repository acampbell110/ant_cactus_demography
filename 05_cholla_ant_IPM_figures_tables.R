##################################################################################################################
##
##              This file visualizes the outputs of the Bayesian Models created in the sourced code
##
##################################################################################################################
##################################################################################################################
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
source( "/Users/alicampbell/Documents/GitHub/ant_cactus_demography/04_cholla_ant_IPM_analysis.R")

## Color Codes
## Retro bright
cremcol <- "#9239F6"
liomcol <- "#00A08A"
othercol <- "#FF0076"
vaccol <- "#F8B660"
vcol <- "#ad90ec"
lcol <- "#084f98"
ccol <- "#e9a67a"
ocol <- "#93022f"
lccol <- "#5dc9cf"
locol <- "#cf3545"
cocol <- "#ab59c8"
acol <- "#5d906b"
cols <- c(vcol, ccol, lcol, ocol, lccol, locol, cocol, acol)
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
#### Timeseries Visuals ################################################################################
########################################################################################################
##Number of Cacti by ant partners
ant_freq_ts <- cactus %>% 
  select(Year_t,ant_t) %>% 
  drop_na() %>% 
  group_by(Year_t,ant_t) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n))
png("Timeseries.png")
plot(ant_freq_ts$Year_t[ant_freq_ts$ant_t == "crem"], ant_freq_ts$freq[ant_freq_ts$ant_t == "crem"], 
     type = "b",pch = 20, cex = 3,col = cremcol, ylim =c(0,1),
     xlab = "", ylab = "")
lines(ant_freq_ts$Year_t[ant_freq_ts$ant_t == "liom"], ant_freq_ts$freq[ant_freq_ts$ant_t == "liom"], 
      type = "b",pch = 20, cex = 3,col = liomcol)
lines(ant_freq_ts$Year_t[ant_freq_ts$ant_t == "vacant"], ant_freq_ts$freq[ant_freq_ts$ant_t == "vacant"], 
      type = "b",pch = 20, cex = 3,col = vaccol)
lines(ant_freq_ts$Year_t[ant_freq_ts$ant_t == "other"], ant_freq_ts$freq[ant_freq_ts$ant_t == "other"], 
      type = "b",pch = 20, cex = 3,col = othercol)
legend("topright", legend = c("Liom.","Crem.","Other","Vacant"), fill = c(liomcol,cremcol,othercol,vaccol),
       cex = 1.5)
mtext("Year",side=1,line=-1.5,outer=TRUE,cex=1.5)
mtext("Frequency of Ant Species",side=2,line=-1.5,outer=TRUE,cex=1.5,las=0)
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



fit_grow_stud<-readRDS("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_grow_student_t.rds")
grow_out <- rstan::extract(fit_grow_stud)
fit_grow_stud@model_pars
# Formulas
# Other
y_other_mean_grow <- quantile(grow_out$beta0[,3],0.5) + (size_dummy) * quantile(grow_out$beta1[,3],0.5) + (size_dummy)^2 * quantile(grow_out$beta2[,3],0.5)
## Crem
y_crem_mean_grow <- quantile(grow_out$beta0[,1],0.5) + (size_dummy) * quantile(grow_out$beta1[,1],0.5) + (size_dummy)^2 * quantile(grow_out$beta2[,1],0.5)
## Liom
y_liom_mean_grow <- quantile(grow_out$beta0[,2],0.5) + (size_dummy) * quantile(grow_out$beta1[,2],0.5) + (size_dummy)^2 * quantile(grow_out$beta2[,2],0.5)
## Vac
y_vac_mean_grow <-  quantile(grow_out$beta0[,4],0.5) + (size_dummy) * quantile(grow_out$beta1[,4],0.5) + (size_dummy)^2 * quantile(grow_out$beta2[,4],0.5)


## create the panel figure
## For posters
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("grow_all_skew.png")
par(mar=c(5,5,1,1),oma=c(2,2,0,0))
layout(matrix(c(1),
              ncol = 1, byrow = TRUE), heights = c(1.4), widths = c(3.9))
plot(x = size_dummy  ,y = y_other_mean_grow, type = "l",lwd = 3,col = othercol,
     main = "",cex.main = 1.5, xlab = "Log(Volume) year t", ylab = "Log(Volume) year t+1",
     cex.lab = 2, xlim = c(3,7), ylim = c(3,7))  
lines(x = size_dummy, y = y_crem_mean_grow, type = "l", col = cremcol, lwd = 3) 
lines(x = size_dummy, y = y_liom_mean_grow, type = "l", col = liomcol,lwd = 3) 
lines(x = size_dummy, y = y_vac_mean_grow, type = "l", col = vaccol, lwd = 3) 
lines(x = size_dummy, y = size_dummy, type= "l", col = "darkgrey", lwd = 2, lty = 2)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c(othercol,cremcol,liomcol,vaccol), pch = 16,
       cex=1.6)
dev.off()
png("grow_panel2021.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4,5,5),
              ncol = 3, byrow = TRUE), heights = c(1.4,1.4), widths = c(3.9,3.9,3.9))
# Other (2)
samp <- sample(nrow(grow_out), 50)
plot(x = size_dummy  ,y = y_other_mean_grow, type = "l", col = othercol, lwd = 4,
     main = "a)         Other   ",cex.main = 1.5) 
points(y_other_subset_grow$logsize_t,y_other_subset_grow$logsize_t1,pch=16,col= alpha("black", 0.4))
lines(x = size_dummy  ,y = y_other_mean_grow, type = "l", col = othercol, lwd = 4)
# Crem (3)
plot(x = size_dummy  ,y = y_crem_mean_grow, type = "l", col = cremcol, lwd = 4,
     main = "b)         Crem.       ",cex.main = 1.5) 
points(y_crem_subset_grow$logsize_t,y_crem_subset_grow$logsize_t1,pch=16,col= alpha("black", 0.4))
lines(x = size_dummy  ,y = y_crem_mean_grow, type = "l", col = cremcol, lwd = 4)
# Liom (4)
plot(x = size_dummy  ,y = y_liom_mean_grow, type = "l", col = liomcol, lwd = 4,
     main = "c)         Liom.       ",cex.main = 1.5)
points(y_liom_subset_grow$logsize_t,y_liom_subset_grow$logsize_t1,pch=16,col= alpha("black", 0.4))
lines(x = size_dummy  ,y = y_liom_mean_grow, type = "l", col = liomcol, lwd = 4)
# Vacant (1)s
plot(x = size_dummy  ,y = y_vac_mean_grow, type = "l", col = vaccol, lwd = 4,
     main = "d)      Vac.      ",cex.main = 1.5) 
points(y_vac_subset_grow$logsize_t,y_vac_subset_grow$logsize_t1,pch=16,col= alpha("black", 0.4))
lines(x = size_dummy  ,y = y_vac_mean_grow, type = "l", col = vaccol, lwd = 4)

# All together
plot(x = size_dummy  ,y = y_other_mean_grow, type = "l",lwd = 2,col = othercol,
     main = "e)              All Ants                 ",cex.main = 1.5,
     ylim = c(4,7), xlim = c(4,7))  
lines(x = size_dummy, y = y_crem_mean_grow, type = "l", col = cremcol, lwd = 2) 
lines(x = size_dummy, y = y_liom_mean_grow, type = "l", col = liomcol,lwd = 2) 
lines(x = size_dummy, y = y_vac_mean_grow, type = "l", col = vaccol, lwd = 2) 
lines(x = size_dummy, y = size_dummy, type= "l", col = "darkgrey", lwd = 2, lty = 2)
legend("topleft", legend = c("Other","Crem.","Liom.","Vacant"), col = c(othercol,cremcol,liomcol,vaccol), pch = 16,
       cex=1.6)
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.5)
mtext("Log(Volume) year t+1",side=2,line=0,outer=TRUE,cex=1.5,las=0)
dev.off()

## For posters
png("grow_panel_title.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(1,1.4,1.4), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.3,"Crem. Offer Higher Growth \n Rates for Small Cacti
",cex=4,font=2)
# Other (2)
plot(x = size_dummy  ,y = y_other_mean_grow, type = "l", col = othercol, lwd = 4,
     main = "Other",cex.main = 2) 
points(y_other_subset_grow$logsize_t,y_other_subset_grow$logsize_t1,pch=16,col= alpha("black", 0.4))
lines(x = size_dummy  ,y = y_other_mean_grow, type = "l", col = othercol, lwd = 4)
# Crem (3)
plot(x = size_dummy  ,y = y_crem_mean_grow, type = "l", col = cremcol, lwd = 4,
     main = "Crem.",cex.main = 2) 
points(y_crem_subset_grow$logsize_t,y_crem_subset_grow$logsize_t1,pch=16,col= alpha("black", 0.4))
lines(x = size_dummy  ,y = y_crem_mean_grow, type = "l", col = cremcol, lwd = 4)
# Liom (4)
plot(x = size_dummy  ,y = y_liom_mean_grow, type = "l", col = liomcol, lwd = 4,
     main = "Liom.",cex.main = 2)
points(y_liom_subset_grow$logsize_t,y_liom_subset_grow$logsize_t1,pch=16,col= alpha("black", 0.4))
lines(x = size_dummy  ,y = y_liom_mean_grow, type = "l", col = liomcol, lwd = 4)
# Vacant (1)s
plot(x = size_dummy  ,y = y_vac_mean_grow, type = "l", col = vaccol, lwd = 4,
     main = "Vacant",cex.main=2) 
points(y_vac_subset_grow$logsize_t,y_vac_subset_grow$logsize_t1,pch=16,col= alpha("black", 0.4))
lines(x = size_dummy  ,y = y_vac_mean_grow, type = "l", col = vaccol, lwd = 4)
# All together
plot(x = size_dummy  ,y = y_other_mean_grow, type = "l",lwd = 2,col = othercol,
     main = "All Ants",cex.main = 2,
     ylim = c(-5,7), xlim = c(-5,7))  
lines(x = size_dummy, y = y_crem_mean_grow, type = "l", col = cremcol, lwd = 2) 
lines(x = size_dummy, y = y_liom_mean_grow, type = "l", col = liomcol,lwd = 2) 
lines(x = size_dummy, y = y_vac_mean_grow, type = "l", col = vaccol, lwd = 2) 
lines(x = size_dummy, y = size_dummy, type= "l", col = "darkgrey", lwd = 2, lty = 2)
legend("topleft", legend = c("Other","Crem.","Liom.","Vacant"), col = c(othercol,cremcol,liomcol,vaccol), pch = 16,
       cex=1.6)
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.5)
mtext("Log(Volume) year t+1",side=2,line=0,outer=TRUE,cex=1.5,las=0)
dev.off()

## Contour plot ###############################################
## Try 1
x <- seq(min(cactus$logsize_t, na.rm = T),max(cactus$logsize_t,na.rm = T), length = 25); # three columns
y <- seq(min(cactus$logsize_t1, na.rm = T),max(cactus$logsize_t1,na.rm = T), length = 25); # five rows

other <- outer (
  y,     # First dimension:  the columns (y)
  x,     # Second dimension: the rows    (x)
  function (x, y)   dlst(y,mu=quantile(grow_out$beta0[,3],0.5) + quantile(grow_out$beta1[,3],0.5)*x + quantile(grow_out$beta2[,3],0.5)*x^2, 
                         sigma = exp(quantile(grow_out$d_0,0.5) + x * quantile(grow_out$d_size,0.5)), 
                         df = exp(quantile(grow_out$a_0,0.5) + x * quantile(grow_out$a_size,0.5)))
);
vacant <- outer (
  y,     # First dimension:  the columns (y)
  x,     # Second dimension: the rows    (x)
  function (x, y)   dlst(y,mu=quantile(grow_out$beta0[,4],0.5) + quantile(grow_out$beta1[,4],0.5)*x + quantile(grow_out$beta2[,4],0.5)*x^2, 
                         sigma = exp(quantile(grow_out$d_0,0.5) + x * quantile(grow_out$d_size,0.5)), 
                         df = exp(quantile(grow_out$a_0,0.5) + x * quantile(grow_out$a_size,0.5)))
);
liom <- outer (
  y,     # First dimension:  the columns (y)
  x,     # Second dimension: the rows    (x)
  function (x, y)   dlst(y,mu=quantile(grow_out$beta0[,2],0.5) + quantile(grow_out$beta1[,2],0.5)*x + quantile(grow_out$beta2[,2],0.5)*x^2, 
                         sigma = exp(quantile(grow_out$d_0,0.5) + x * quantile(grow_out$d_size,0.5)), 
                         df = exp(quantile(grow_out$a_0,0.5) + x * quantile(grow_out$a_size,0.5)))
);
crem <- outer (
  y,     # First dimension:  the columns (y)
  x,     # Second dimension: the rows    (x)
  function (x, y)   dlst(y,mu=quantile(grow_out$beta0[,1],0.5) + quantile(grow_out$beta1[,1],0.5)*x + quantile(grow_out$beta2[,1],0.5)*x^2, 
                         sigma = exp(quantile(grow_out$d_0,0.5) + x * quantile(grow_out$d_size,0.5)), 
                         df = exp(quantile(grow_out$a_0,0.5) + x * quantile(grow_out$a_size,0.5)))
);

## Contour plots
## Now without the mean lines
## For gray scale contour lines, col = gray.colors(10, start =1, end = 0)
png("grow_contour_lines_color.png")
par(mar=c(3,3,3,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4,5,5),
              ncol = 3, byrow = TRUE), heights = c(1.4,1.4), widths = c(3.9,3.9,3.9))
## Crem
contour(x,y,crem, nlevels = 10, col = "darkgrey", xlim = c(2,10), ylim = c(0,10), 
        main = "a)       Crem.               ", cex.main = 2) 
lines(size_dummy, y_crem_mean_grow, col = cremcol, lwd = 3)
## Liom
contour(x,y,liom, nlevels = 10, col = "darkgrey", xlim = c(2,10), ylim = c(0,10), 
        main = "b)      Liom.                ", cex.main = 2) 
lines(size_dummy, y_liom_mean_grow, col = liomcol, lwd = 3)
## Other
contour(x,y,other, nlevels = 10, col = "darkgrey", xlim = c(2,10), ylim = c(0,10), 
        main = "c)       Other                ", cex.main = 2) 
lines(size_dummy, y_other_mean_grow, col = othercol, lwd = 3)
## Vacant
contour(x,y,vacant, nlevels = 10, col = "darkgrey", xlim = c(2,10), ylim = c(0,10), 
        main = "d)      Vacant                ", cex.main = 2) 
lines(size_dummy, y_vac_mean_grow, col = vaccol, lwd = 3)
## All together
plot(size_dummy, y_crem_mean_grow, type = "l", col = cremcol, lwd = 3, xlim = c(-5,6), ylim = c(-5,6), 
     main = "e)                      All Ants                           ", cex.main = 2) 
lines(size_dummy, y_liom_mean_grow, col = liomcol, lwd = 3)
lines(size_dummy, y_other_mean_grow, col = othercol, lwd = 3)
lines(size_dummy, y_vac_mean_grow, col = vaccol, lwd = 3)
lines(size_dummy, size_dummy, col = "grey", lty = 2)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c(othercol,cremcol,liomcol,vaccol), pch = 16)
mtext("Log(Volume Year t)",side=1,line=0,outer=TRUE,cex=2)
mtext("Log(Volume Year t+1)",side=2,line=0,outer=TRUE,cex=2,las=0)
dev.off()

## Show the correlation between ant and year -- from growth model random effects
g_vac_vec <- colMeans(cbind(grow_out$w.1.1,grow_out$w.1.2,grow_out$w.1.3,grow_out$w.1.4,grow_out$w.1.5,grow_out$w.1.6,
                            grow_out$w.1.7,grow_out$w.1.8,grow_out$w.1.9,grow_out$w.1.10,grow_out$w.1.11,grow_out$w.1.12,
                            grow_out$w.1.13,grow_out$w.1.14))
g_liom_vec <- colMeans(cbind(grow_out$w.4.1,grow_out$w.4.2,grow_out$w.4.3,grow_out$w.4.4,grow_out$w.4.5,grow_out$w.4.6,
                             grow_out$w.4.7,grow_out$w.4.8,grow_out$w.4.9,grow_out$w.4.10,grow_out$w.4.11,grow_out$w.4.12,
                             grow_out$w.4.13,grow_out$w.4.14))
g_other_vec <- colMeans(cbind(grow_out$w.2.1,grow_out$w.2.2,grow_out$w.2.3,grow_out$w.2.4,grow_out$w.2.5,grow_out$w.2.6,
                              grow_out$w.2.7,grow_out$w.2.8,grow_out$w.2.9,grow_out$w.2.10,grow_out$w.2.11,grow_out$w.2.12,
                              grow_out$w.2.13,grow_out$w.2.14))
g_crem_vec <- colMeans(cbind(grow_out$w.3.1,grow_out$w.3.2,grow_out$w.3.3,grow_out$w.3.4,grow_out$w.3.5,grow_out$w.3.6,
                             grow_out$w.3.7,grow_out$w.3.8,grow_out$w.3.9,grow_out$w.3.10,grow_out$w.3.11,grow_out$w.3.12,
                             grow_out$w.3.13,grow_out$w.3.14))
g_years <- c(2004,2005,2006,2007,2009,2010,2011,2012,2013,2014,2015,2016,2017,2019)
## Show the correlation between ant and year -- from survival model random effects
s_vac_vec <- colMeans(cbind(surv_out$w.1.1,surv_out$w.1.2,surv_out$w.1.3,surv_out$w.1.4,surv_out$w.1.5,surv_out$w.1.6,
                            surv_out$w.1.7,surv_out$w.1.8,surv_out$w.1.9,surv_out$w.1.10,surv_out$w.1.11,surv_out$w.1.12,
                            surv_out$w.1.13,surv_out$w.1.14))
s_liom_vec <- colMeans(cbind(surv_out$w.4.1,surv_out$w.4.2,surv_out$w.4.3,surv_out$w.4.4,surv_out$w.4.5,surv_out$w.4.6,
                             surv_out$w.4.7,surv_out$w.4.8,surv_out$w.4.9,surv_out$w.4.10,surv_out$w.4.11,surv_out$w.4.12,
                             surv_out$w.4.13,surv_out$w.4.14))
s_other_vec <- colMeans(cbind(surv_out$w.2.1,surv_out$w.2.2,surv_out$w.2.3,surv_out$w.2.4,surv_out$w.2.5,surv_out$w.2.6,
                              surv_out$w.2.7,surv_out$w.2.8,surv_out$w.2.9,surv_out$w.2.10,surv_out$w.2.11,surv_out$w.2.12,
                              surv_out$w.2.13,surv_out$w.2.14))
s_crem_vec <- colMeans(cbind(surv_out$w.3.1,surv_out$w.3.2,surv_out$w.3.3,surv_out$w.3.4,surv_out$w.3.5,surv_out$w.3.6,
                             surv_out$w.3.7,surv_out$w.3.8,surv_out$w.3.9,surv_out$w.3.10,surv_out$w.3.11,surv_out$w.3.12,
                             surv_out$w.3.13,surv_out$w.3.14))
s_years <- c(2004,2005,2006,2007,2009,2010,2011,2012,2013,2014,2015,2016,2017,2019)
## Show the correlation between ant and year -- from growth model random effects
v_vac_vec <- colMeans(cbind(viab_out$w.1.1,viab_out$w.1.2,viab_out$w.1.3,viab_out$w.1.4,viab_out$w.1.5,viab_out$w.1.6,
                            viab_out$w.1.7,viab_out$w.1.8,viab_out$w.1.9,viab_out$w.1.10,viab_out$w.1.11,viab_out$w.1.12,
                            viab_out$w.1.13,viab_out$w.1.14))
v_liom_vec <- colMeans(cbind(viab_out$w.4.1,viab_out$w.4.2,viab_out$w.4.3,viab_out$w.4.4,viab_out$w.4.5,viab_out$w.4.6,
                             viab_out$w.4.7,viab_out$w.4.8,viab_out$w.4.9,viab_out$w.4.10,viab_out$w.4.11,viab_out$w.4.12,
                             viab_out$w.4.13,viab_out$w.4.14))
v_other_vec <- colMeans(cbind(viab_out$w.2.1,viab_out$w.2.2,viab_out$w.2.3,viab_out$w.2.4,viab_out$w.2.5,viab_out$w.2.6,
                              viab_out$w.2.7,viab_out$w.2.8,viab_out$w.2.9,viab_out$w.2.10,viab_out$w.2.11,viab_out$w.2.12,
                              viab_out$w.2.13,viab_out$w.2.14))
v_crem_vec <- colMeans(cbind(viab_out$w.3.1,viab_out$w.3.2,viab_out$w.3.3,viab_out$w.3.4,viab_out$w.3.5,viab_out$w.3.6,
                             viab_out$w.3.7,viab_out$w.3.8,viab_out$w.3.9,viab_out$w.3.10,viab_out$w.3.11,viab_out$w.3.12,
                             viab_out$w.3.13,viab_out$w.3.14))
v_years <- c(2004,2005,2006,2012,2013,2014,2015,2016,2017,2018,2019)

png("year_ant_timeseries.png")
par(mar=c(4,2,2,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3),
              ncol = 3, byrow = TRUE), heights = c(1), widths = c(4,4,4))
## Growth Ant EEffects
plot(g_years,g_liom_vec,col = liomcol, cex.main = 2,type = "b",lwd = 4, pch = 16,cex = 2,
     main = "a)                               ",
     ylim = c(-1.5,2.1), xlab = " ",ylab = " ",cex.lab = 2)
lines(g_years, g_crem_vec, type = "b", col = cremcol, lwd = 4, pch = 16, cex = 2)
lines(g_years, g_vac_vec, type = "b", col = vaccol, lwd = 4, pch = 16, cex = 2)
lines(g_years, g_other_vec, type = "b", col = othercol, lwd = 4, pch = 16, cex = 2)
legend("topleft",legend = c("Liom.","Crem.","Other","Vacant"),fill = c(liomcol,cremcol,othercol,vaccol),cex=1.8)
## Survival ant effects
plot(s_years,s_liom_vec,col = liomcol, cex.main = 2,type = "b",lwd = 4, pch = 16,cex = 2,
     main = "b)                                 ",
     ylim = c(-1.5,2.1), xlab = "",ylab = " ",cex.lab = 1.5)
lines(s_years, s_crem_vec, type = "b", col = cremcol, lwd = 4, pch = 16, cex = 2)
lines(s_years, s_other_vec, type = "b", col = othercol, lwd = 4, pch = 16, cex = 2)
lines(s_years, s_vac_vec, type = "b", col = vaccol, lwd = 4, pch = 16, cex = 2)
## Viability Ant Effects
plot(v_years,v_liom_vec,col = liomcol, cex.main = 2,type = "b",lwd = 4, pch = 16,cex = 2,
     main = "c)                                ",
     ylim = c(-1.5,2.1), xlab = " ",ylab = " ",cex.lab = 1.5)
lines(v_years, v_crem_vec, type = "b", col = cremcol, lwd = 4, pch = 16, cex = 2)
lines(v_years, v_other_vec, type = "b", col = othercol, lwd = 4, pch = 16, cex = 2)
lines(v_years, v_vac_vec, type = "b", col = vaccol, lwd = 4, pch = 16, cex = 2)
mtext("Year",side=1,line=0,outer=TRUE,cex=1.7)
mtext("Ant-Year Effects",side=2,line=0,outer=TRUE,cex=1.7,las=0)
dev.off()


fun_color_range <- colorRampPalette(c("white", "forestgreen")) 
my_colors <- fun_color_range(1500)
g_dat <- cbind(g_vac_vec,g_liom_vec,g_other_vec,g_crem_vec)
colnames(g_dat) <- c("Vacant","Liom.","Other","Crem.")
s_dat <- cbind(s_vac_vec,s_liom_vec,s_other_vec,s_crem_vec)
colnames(s_dat) <- c("Vacant","Liom.","Other","Crem.")
v_dat <- cbind(v_vac_vec,v_liom_vec,v_other_vec,v_crem_vec)
colnames(v_dat) <- c("Vacant","Liom.","Other","Crem.")
png("year_ant_corr.png")
par(mar=c(5,1,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4),
              ncol = 2, nrow = 2, byrow = TRUE), heights = c(1), widths = c(4,4,4))
corrplot(cor(g_dat), method = "color", tl.col = "black",
         col = my_colors, addCoef.col = "black",
         main = "a)                                              ",
         type= "lower")
corrplot(cor(s_dat), method = "color",  tl.col = "black",
         col = my_colors, addCoef.col = "black",
         main = "b)                                              ",
         type= "lower")
corrplot(cor(v_dat), method = "color",  tl.col = "black",
         col = my_colors, addCoef.col = "black",
         main = "c)                                              ",
         type= "lower")
dev.off()




###########################################################################################################################
#### Survival Visuals #####################################################################################################
#########################################################################################################################
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
## Extract & Format Data
survival_data_orig <- subset(cactus, is.na(Survival_t1) == FALSE,c("Plot","Year_t","Survival_t1","ant_t","logsize_t"))
survival_data_orig <- cactus[,c("Plot","Year_t","Survival_t1","ant_t","logsize_t")]
survival_data <- na.omit(survival_data_orig)
#extract from original ddata
surv_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/surv.params.csv", header = TRUE,stringsAsFactors=T)
surv_out <- read.csv("surv.params.csv", header = TRUE,stringsAsFactors=T)
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
y_other_surv = quantile(surv_out$beta0.3,0.5) + size_other * quantile(surv_out$beta1.3,0.5)
y_other_low_surv = quantile(surv_out$beta0.3,0.05) + size_other * quantile(surv_out$beta1.3,0.05)
y_other_high_surv = quantile(surv_out$beta0.3,0.95) + size_other * quantile(surv_out$beta1.3,0.95)
other_extr = quantile(surv_out$beta0.3,0.5) + size_dummy * quantile(surv_out$beta1.3,0.5)
## Crem -- 3
y_crem_surv = quantile(surv_out$beta0.1,0.5) + size_crem * quantile(surv_out$beta1.1,0.5)
y_crem_low_surv = quantile(surv_out$beta0.1,0.05) + size_crem * quantile(surv_out$beta1.1,0.05)
y_crem_high_surv = quantile(surv_out$beta0.1,0.95) + size_crem * quantile(surv_out$beta1.1,0.95)
crem_extr = quantile(surv_out$beta0.1,0.5) + size_dummy * quantile(surv_out$beta1.1,0.5)
## Liom -- 4
y_liom_surv = quantile(surv_out$beta0.2,0.5) + size_liom * quantile(surv_out$beta1.2,0.5)
y_liom_low_surv = quantile(surv_out$beta0.2,0.05) + size_liom * quantile(surv_out$beta1.2,0.05)
y_liom_high_surv = quantile(surv_out$beta0.2,0.95) + size_liom * quantile(surv_out$beta1.2,0.95)
liom_extr = quantile(surv_out$beta0.2,0.5) + size_dummy * quantile(surv_out$beta1.2,0.5)
## Vac -- 1
y_vac_surv = quantile(surv_out$beta0.4,0.5) + size_vac * quantile(surv_out$beta1.4,0.5)
y_vac_low_surv = quantile(surv_out$beta0.4,0.05) + size_vac * quantile(surv_out$beta1.4,0.05)
y_vac_high_surv = quantile(surv_out$beta0.4,0.95) + size_vac * quantile(surv_out$beta1.4,0.95)
vac_extr = quantile(surv_out$beta0.4,0.5) + size_dummy * quantile(surv_out$beta1.4,0.5)
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
## Panel Plots
## For paper -- No title and separated into panels by ant partner
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("surv_panels.png")
par(mar=c(3,3,3,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4,5,5),
              ncol = 3, byrow = TRUE), heights = c(1.5,1.5), widths = c(3.9,3.9,3.9))
# Crem
plot(x = size_crem  ,y = invlogit(y_crem_surv), type = "l", col = cremcol, lwd = 4, ylim = c(0.6,1), xlim = c(1,15),
     cex.main = 2, main = "a)           Crem.         ")
points(surv_plot_crem$mean_size,surv_plot_crem$surv,pch=16,cex=surv_plot_crem$N_mod,col= alpha(cremcol, 0.4))
#lines(x = size_dummy, y = invlogit(y_crem_low_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_crem_high_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_crem,rev(size_crem)),c(invlogit(y_crem_high_surv), rev(invlogit(y_crem_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Liom
plot(x = size_liom  ,y = invlogit(y_liom_surv), type = "l", col = liomcol, lwd = 4, ylim = c(0.6,1), xlim = c(1,15),
     cex.main = 2, main = "b)           Liom.         ")
points(surv_plot_liom$mean_size,surv_plot_liom$surv,pch=16,cex=surv_plot_liom$N_mod,col= alpha(liomcol, 0.4))
#lines(x = size_dummy, y = invlogit(y_liom_low_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_liom_high_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)# Vacant
polygon(c(size_liom,rev(size_liom)),c(invlogit(y_liom_high_surv), rev(invlogit(y_liom_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Other
plot(x = size_other  ,y = invlogit(y_other_surv), type = "l", col = othercol, lwd = 4, ylim = c(0.6,1), xlim = c(1,15),
     cex.main = 2, main = "c)           Other         ")
points(surv_plot_other$mean_size,surv_plot_other$surv,pch=16,cex=surv_plot_other$N_mod,col= alpha(othercol, 0.4))
#lines(x = size_dummy, y = invlogit(y_other_low_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_other_high_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_other,rev(size_other)),c(invlogit(y_other_high_surv), rev(invlogit(y_other_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Vac
plot(x = size_vac  ,y = invlogit(y_vac_surv), type = "l", col = vaccol, lwd = 4, ylim = c(0.2,1), xlim = c(-5,15),
     cex.main = 2, main = "d)        Vacant         ")
points(surv_plot_vac$mean_size,surv_plot_vac$surv,pch=16,cex=surv_plot_vac$N_mod,col= alpha(vaccol, 0.4))
#lines(x = size_dummy, y = invlogit(y_vac_low_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
#lines(x = size_dummy, y = invlogit(y_vac_high_surv), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_vac,rev(size_vac)),c(invlogit(y_vac_high_surv), rev(invlogit(y_vac_low_surv))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# All Together
plot(x = size_dummy, y = invlogit(other_extr), type = "l", col = othercol, lwd = 2, ylim = c(0.6,1), lty = 2, xlim = c(1,15),
     cex.main = 2, main = "e)                          All Ants                          ")
lines(x = size_dummy, y = invlogit(crem_extr), col = cremcol,lwd = 2, lty = 2)
lines(x = size_dummy, y = invlogit(liom_extr), col = liomcol, lwd = 2, lty = 2)
lines(x = size_dummy, y = invlogit(vac_extr), col = vaccol, lwd = 2, lty = 2)
lines(x = size_other, y = invlogit(y_other_surv), col = othercol, lwd = 2)
lines(x = size_crem, y = invlogit(y_crem_surv), col = cremcol, lwd = 2)
lines(x = size_liom, y = invlogit(y_liom_surv), col = liomcol, lwd = 2)
lines(x = size_vac, y = invlogit(y_vac_surv), col = vaccol, lwd = 2)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c(othercol,cremcol,liomcol,vaccol), pch = 16,
       cex = 2)
mtext("Log(Volume)",side=1,line=0,outer=TRUE,cex=2)
mtext("Probability of Survival",side=2,line=0,outer=TRUE,cex=2,las=0)
dev.off()
## Just the image of all the estimations by ants together (all lines on the same panel)
png("surv_all.png")
par(mar=c(5,5,1,1),oma=c(2,2,0,0))
layout(matrix(c(1),
              ncol = 1, byrow = TRUE), heights = c(1.4), widths = c(3.9))
plot(x = size_dummy, y = invlogit(other_extr), type = "l", col = othercol, lwd = 3, ylim = c(0.6,1), lty = 2, xlim = c(1,15),
     main = "", ylab = "Probability of Survival", xlab = "Log (Volume)", cex.lab = 2)
lines(x = size_dummy, y = invlogit(crem_extr), col = cremcol,lwd = 3, lty = 2)
lines(x = size_dummy, y = invlogit(liom_extr), col = liomcol, lwd = 3, lty = 2)
lines(x = size_dummy, y = invlogit(vac_extr), col = vaccol, lwd = 3, lty = 2)
lines(x = size_other, y = invlogit(y_other_surv), col = othercol, lwd = 3)
lines(x = size_crem, y = invlogit(y_crem_surv), col = cremcol, lwd = 3)
lines(x = size_liom, y = invlogit(y_liom_surv), col = liomcol, lwd = 3)
lines(x = size_vac, y = invlogit(y_vac_surv), col = vaccol, lwd = 3)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c(othercol,cremcol,liomcol,vaccol), pch = 16,
       cex = 2)
dev.off()
## For posters
png("surv_panels_title.png")
par(mar=c(2,2,2,1),oma=c(2,2,0,0))
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
mtext("Probability of Survival",side=2,line=0,outer=TRUE,cex=2,las=0)
dev.off()

############
## Show the correlation between ant and year -- from growth model random effects
vac_vec <- (colMeans(surv_outputs$w[,4,]))
liom_vec <- colMeans(surv_outputs$w[,2,])
other_vec <- colMeans(surv_outputs$w[,3,])
crem_vec <- colMeans(surv_outputs$w[,1,])
years <- c(2004,2005,2006,2007,2009,2010,2011,2012,2013,2014,2015,2016,2017,2019)
png("surv_year_ant_timeseries.png")
plot(years,liom_vec,col = liomcol, cex.main = 2,type = "b",lwd = 4, pch = 16,cex = 2,
     main = "Ants have Different Effects on \n Survival Rates Across Years",
     ylim = c(-1,1), xlab = "Years",ylab = "Year-Ant Effect on Growth",cex.lab = 1.5)
lines(years, crem_vec, type = "b", col = cremcol, lwd = 4, pch = 16, cex = 2)
lines(years, other_vec, type = "b", col = othercol, lwd = 4, pch = 16, cex = 2)
lines(years, vac_vec, type = "b", col = vaccol, lwd = 4, pch = 16, cex = 2)
legend("topright",legend = c("Liom.","Crem.","Other","Vacant"),fill = c(liomcol,cremcol,othercol,vaccol))
dev.off()
#########################################################################################################################
#### Flowering Visuals #####################################################################################################
#########################################################################################################################
## Extract & Format Data
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
flower_data_orig <- cactus[ , c("TotFlowerbuds_t", "logsize_t","Year_t","Plot")]
flower_data_orig <- subset(flower_data_orig, TotFlowerbuds_t > 0)
flower_data <- na.omit(flower_data_orig)
flow_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/flow.params.csv", header = TRUE,stringsAsFactors=T)
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
## Plot
## For Posters 
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("flow_title.png")
par(mar=c(4,4,0,1),oma=c(2,2,0,0),layout(matrix(c(1,2,3,4)),
                                         ncol = 4, byrow = TRUE), heights = c(1), widths = c(3.9,3.9,3.9,3.9))
plot.new()
text(0.5,0.25,"Number of Flowers\nProduced by Size",cex=2,font=2)
plot(x = size_dummy  ,y = exp(y_flow), type = "l", col = "chartreuse4", lwd = 4, ylim = c(0,100), xlab = " ", ylab = " ")
points(flow_plot$mean_size,flow_plot$tot,pch=16,cex=flow_plot$N_mod,col= alpha("chartreuse4", 0.4))
lines(x = size_dummy, y = exp(y_low_flow), type = "l", col = "darkgrey", lty = 2, lwd = 2)
lines(x = size_dummy, y = exp(y_high_flow), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_dummy,rev(size_dummy)),c(exp(y_high_flow), rev(exp(y_low_flow))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
mtext("Log(Volume)",side=1,line=-1.5,outer=TRUE,cex=1.7)
mtext("Probability of Survival           ",side=2,line=-1.5,outer=TRUE,cex=1.7,las=0)
dev.off()
## For paper
png("flow.png")
par(mar=c(4,4,1,1))
plot(x = size_dummy  ,y = exp(y_flow), type = "l", col = "chartreuse4", lwd = 4, ylim = c(0,100), xlab = " ", ylab = " ")
points(flow_plot$mean_size,flow_plot$tot,pch=16,cex=flow_plot$N_mod,col= alpha("chartreuse4", 0.4))
lines(x = size_dummy, y = exp(y_low_flow), type = "l", col = "darkgrey", lty = 2, lwd = 2)
lines(x = size_dummy, y = exp(y_high_flow), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_dummy,rev(size_dummy)),c(exp(y_high_flow), rev(exp(y_low_flow))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
mtext("Log(Volume)",side=1,line=-1.5,outer=TRUE,cex=1.7)
mtext("Probability of Survival",side=2,line=-1.5,outer=TRUE,cex=1.7,las=0)
dev.off()

#########################################################################################################################
#### Viability Visuals #####################################################################################################
#########################################################################################################################
## Extract & Format Data
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
viability_data_orig <- cactus[ , c("TotFlowerbuds_t1","Goodbuds_t1","ABFlowerbuds_t1","ant_t", "logsize_t","Year_t","Plot")]
viability_data_orig <- subset(viability_data_orig, TotFlowerbuds_t1 > 0)
viability_data <- na.omit(viability_data_orig)
viab_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/viab.params.csv", header = TRUE,stringsAsFactors=T)
viab_out <- read.csv("viab.params.csv", header = TRUE,stringsAsFactors=T)
nrow(viability_data_orig)
nrow(viability_data)
levels(viability_data$ant_t)
cactus$viab <- as.integer(as.character(cactus$Goodbuds_t1))/as.integer(as.character(cactus$TotFlowerbuds_t1))
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
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
## Histograms of biability rates
png("viab_hist2021.png")
a <- ggplot(viab_out,aes(invlogit(beta0.1))) +
  geom_histogram(bins=30, color = cremcol, fill = cremcol, alpha = 0.3) +
  coord_flip() + xlim(0.4,1) + ylim(0,900) + 
  theme_classic() + labs(x = "", y = " ")
b <- ggplot(viab_out,aes(invlogit(beta0.2))) +
  geom_histogram(bins=30, color = liomcol, fill = liomcol, alpha = 0.3) +
  coord_flip() + xlim(0.4,1) + ylim(0,900) + 
  theme_classic() + labs(x = "", y = " ")
c <- ggplot(viab_out,aes(invlogit(beta0.3))) +
  geom_histogram(bins=30, color = othercol, fill = othercol, alpha = 0.3) +
  coord_flip() + xlim(0.4,1) + ylim(0,900) + 
  theme_classic() + labs(x = "", y = " ")
d <- ggplot(viab_out,aes(invlogit(beta0.4))) +
  geom_histogram(bins=30, color = vaccol, fill = vaccol, alpha = 0.3) +
  coord_flip() + xlim(0.4,1) + ylim(0,900) + 
  theme_classic() + labs(x = "", y = " ")
ggarrange(a, b, c, d,
          labels = c("a)", "b)", "c)","d)"),
          ncol = 4, nrow = 1)
dev.off()

png("Viability2021.png")
par(mar=c(5,6,3,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(1,1,1,1)) 
hist(crem_subset$viab, xlim = c(0,1), prob = TRUE, ylim = c(0,12), col = cremcol, cex.main = 2,xlab = "",ylab = "",main = "a)                                               Crem.                                                 ")
lines(density(invlogit(viab_out$beta0.3)), lwd = 3, col = cremcol)
hist(liom_subset$viab, xlim = c(0,1), prob = TRUE, ylim = c(0,12), col = liomcol, cex.main = 2, xlab = "",ylab = "",main = "b)                                               Liom.                                                 ")
lines(density(invlogit(viab_out$beta0.4)), lwd = 3, col = liomcol)
hist(other_subset$viab, xlim = c(0,1), prob = TRUE, ylim = c(0,12), col = othercol, cex.main = 2, xlab = "",ylab = "",main = "c)                                              Other                                                  ")
lines(density(invlogit(viab_out$beta0.2)), lwd = 3, col = othercol)
hist(vac_subset$viab, xlim = c(0,1), prob = TRUE, ylim = c(0,12), col = vaccol, cex.main = 2, xlab = "",ylab = "",main = "d)                                           Vacant                                               ")
lines(density(invlogit(viab_out$beta0.1)), lwd = 3, col = vaccol) 
mtext("Proportion of Flowerbuds Viable",side=1,line=-1.5,outer=TRUE,cex=2)
mtext("Density",side=2,line=-2,outer=TRUE,cex=2,las=0)
dev.off()


## Format Data for Barplot (proportion of viable buds)
means <- c(mean(viab_out$beta0.1), mean(viab_out$beta0.2), mean(viab_out$beta0.3), mean(viab_out$beta0.4))
low <- c(quantile(viab_out$beta0.1,0.05),quantile(viab_out$beta0.2,0.05),quantile(viab_out$beta0.3,0.05),quantile(viab_out$beta0.4,0.05))
high <- c(quantile(viab_out$beta0.1,0.95),quantile(viab_out$beta0.2,0.95),quantile(viab_out$beta0.3,0.95),quantile(viab_out$beta0.4,0.95))
data <- cbind(viab_out$beta0.1,viab_out$beta0.2,viab_out$beta0.3,viab_out$beta0.4)
data <- invlogit(data)
colnames(data) <- c("Vacant","Other","Crem","Liom")

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
## For Poster
png("viab_bars_title2021.png")
barplot(invlogit(means), ylim = c(0,1.1), col = c(vaccol,othercol,cremcol,liomcol),xlab = "Ant Partner", ylab = "Viability Rate",
        main = "Liom. Offers Higher Viability \nRates for Flowerbuds", names.arg = c("Vacant","Other","Crem.","Liom"),
        cex.main = 2.6, cex.lab = 1.5)
dev.off()
## For paper
png("viab_bars.png")
barplot(invlogit(means), ylim = c(0,1.1), col = c(vaccol,othercol,cremcol,liomcol),xlab = "Ant Partner", ylab = "Viability Rate",
        main = "", names.arg = c("Vacant","Other","Crem.","Liom."),
        cex.main = 2.6, cex.lab = 1.5)
dev.off()
############
alpha_val<-0.2
## Tom's version of viability figure
plot(1:5,c(1,1,1,1,1),ylim=c(0,1),type="n",axes=F,xlab="Ant state",ylab="",cex.lab=1.4)
points(jitter(rep(1,nrow(crem_subset))),jitter(crem_subset$viab),
       cex=0.5+(crem_subset$TotFlowerbuds_t1/max(viability_data$TotFlowerbuds_t1))*4,
       col=alpha(cremcol,alpha_val),pch=16)
points(jitter(rep(2,nrow(liom_subset))),jitter(liom_subset$viab),
       cex=0.5+(liom_subset$TotFlowerbuds_t1/max(viability_data$TotFlowerbuds_t1))*4,
       col=alpha(liomcol,alpha_val),pch=16)
points(jitter(rep(3,nrow(other_subset))),jitter(other_subset$viab),
       cex=0.5+(other_subset$TotFlowerbuds_t1/max(viability_data$TotFlowerbuds_t1))*4,
       col=alpha(othercol,alpha_val),pch=16)
points(jitter(rep(4,nrow(vac_subset))),jitter(vac_subset$viab),
       cex=0.5+(vac_subset$TotFlowerbuds_t1/max(viability_data$TotFlowerbuds_t1))*4,
       col=alpha(vaccol,alpha_val),pch=16)
axis(1,at=1:4,labels=c(expression(italic("C.opuntiae")),expression(italic("L.apiculatum")),"Other","Vacant"))
mtext("Flowerbud viability", side = 2, line = 1, cex=1.4)
box()

#########################################################################################################################
#### Reproductive Visuals #####################################################################################################
#########################################################################################################################
## Extract & Format Data
#format for overlay plots
#extract from STAN models
#extract from original data
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
repro_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/repro.params.csv", header = TRUE,stringsAsFactors=T)
repro_out <- read.csv("repro.params.csv", header = TRUE,stringsAsFactors=T)
## Formulas
size_dummy <- seq(min(reproductive_data$logsize_t),max((reproductive_data$logsize_t)), by = 0.1)
y_repro = quantile(repro_out$beta0,0.5,na.rm = TRUE) + size_dummy * quantile(repro_out$beta1,0.5,na.rm = TRUE)
y_low_repro = quantile(repro_out$beta0,0.05) + size_dummy * quantile(repro_out$beta1,0.05)
y_high_repro = quantile(repro_out$beta0,0.95) + size_dummy * quantile(repro_out$beta1,0.95)
## Panel Plots
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
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
seed_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/seed.params.csv", header = TRUE,stringsAsFactors=T)
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
  seeds$beta[nrow(seed_out) + i] <- seed_out$beta0.3[i]
  seeds$ant[nrow(seed_out) + i] <- "vacant"
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

########################################################################################################
#### Pre-Census Seed Survival #########################################################################
########################################################################################################
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
precensus.dat.orig<-read.csv("Data Analysis/PrecensusSurvival.csv") 
precensus.dat <- precensus.dat.orig[ , c("Transect","Seed","Log.size","survive0405")]
precensus.dat <- na.omit(precensus.dat)
y_subset <- (precensus.dat)
seed_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/seed.surv.params.csv", header = TRUE,stringsAsFactors=T)
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
## Formulas
y_surv = seed_out$beta0
y_low_surv = quantile(seed_out$beta0,0.05) 
y_high_surv = quantile(seed_out$beta0,0.95)
#y_surv <- cbind(invlogit(quantile(seed_out$beta0,0.5)), invlogit(quantile(seed_out$beta0,0.05)), invlogit(quantile(seed_out$beta0,0.95)))

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

mean(invlogit(y_surv))
mean(invlogit(y_low_surv))
mean(invlogit(y_high_surv))

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
germ1_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/germ1.params.csv", header = TRUE,stringsAsFactors=T)
germ2_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/germ2.params.csv", header = TRUE,stringsAsFactors=T)
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
seedling.dat_orig <- cactus[,c("logsize_t1","Recruit", "Year_t")]
seedling.dat_orig <- filter(seedling.dat_orig, Recruit == 1)
seedling.dat <- na.omit(seedling.dat_orig)
summary(seedling.dat$logsize_t1)
recruit_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/rec.params.csv", header = TRUE,stringsAsFactors=T)
y_rec = quantile(recruit_out$beta0,0.5)
y_rec_low = quantile(recruit_out$beta0,0.05)
y_rec_high = quantile(recruit_out$beta0,0.95)
size_dummy_rec <- seq(min(seedling.dat$logsize_t1), max(seedling.dat$logsize_t1), by = 0.1)
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")


png("rec_size.png")
boxplot(recruit_out$beta0, col = "chartreuse4", ylab = "Log(Volume)", main = "Recruit Size Distribution")
dev.off()


######################################################################################################################
###################### Ant Partner Transitions #######################################################################
######################################################################################################################
#### Create new column that is a unique ID for every
#### plot and plant then select needed columns
cactus$individual_ID <- paste(cactus$Plot,cactus$TagID)
cactus_ants <- cactus[,c("Recruit","individual_ID","Year_t","ant_t1","Newplant","logsize_t1")]
dim(cactus_ants)
dim(cactus)
## remove all NAs
cactus_ants <- na.omit(cactus_ants)
## Creat a new column that identifies the year the plant was first recorded
cactus_ants$obs_yr <- NA

#### Find the earliest year of observation for each individual plant
cactus_ants %>%
  group_by(individual_ID)  %>%
  summarise(min_yr = min(Year_t)) -> order
for(i in 1:nrow(cactus_ants)){
  for(j in 1:nrow(order)){
    if(cactus_ants$individual_ID[i] == order$individual_ID[j]) cactus_ants$obs_yr[i] <- order$min_yr[j]
  }
}
#### Pull out a subset of plants that we have followed since they were recruits
## first find the plant IDs of all those we have recruit info for
cactus_rec <- subset(cactus_ants, Recruit == 1)
cactus_ants$from_rec <- 0
## Then match each ID to the actual dataset and mark as from recruit
for(i in 1:nrow(cactus_rec)){
  for(j in 1:nrow(cactus_ants)){
    if(cactus_ants$individual_ID[j] == cactus_rec$individual_ID[i]) cactus_ants$from_rec[j] <- 1
  }
}
cactus_ants <- subset(cactus_ants, cactus_ants$from_rec == 1)
## Now use this observed year to calculate the approximate age of the plant 
#View(cactus_ants)
#### Give them an age based on how many years in a row we have observed them
cactus_ants$obs_age <- cactus_ants$Year_t - cactus_ants$obs_yr 
## Now calculate the highest age for each individual and order the rows by that max age
cactus_ants %>%
  group_by(individual_ID) %>%
  summarise(max_age = max(obs_age)) -> ages
cactus_ants$max_age <- 0
## Assign the max age to each identified plant
for(i in 1:nrow(cactus_ants)){
  for(j in 1:nrow(order)){
    if(cactus_ants$individual_ID[i] == ages$individual_ID[j]) cactus_ants$max_age[i] <- ages$max_age[j]
  }
}
## Reorder the dataset by age
cactus_reorder <- cactus_ants[order(cactus_ants$max_age),] 
#View(cactus_reorder)


## Plot the size of the cactus by the individual and track the ant partner across time
png("ant_partner_timeseries_size.png")
plot(cactus_reorder$logsize_t, y = as.factor(cactus_reorder$individual_ID), col = cactus_reorder$ant_t)
legend("bottomleft", legend = c(unique(cactus_reorder$ant_t)), fill = unique(cactus_reorder$ant_t))
dev.off()

## Plot the approximate age of the cactus by the individual and track the ant partner across time
png("ant_partner_timeseries_age.png")
plot(x = cactus_reorder$obs_age, y = as.factor(cactus_reorder$individual_ID), col = cactus_reorder$ant_t1)
legend("topleft", legend = c(unique(cactus$ant_t)), fill = unique(cactus$ant_t))
dev.off()

## Create a matrix where the row is the individual ID and the column is the age and the info in each location
## create an NA vector that is the length of all individual ID * max age
ages_mat <- matrix(0, length(unique(cactus_reorder$individual_ID)), 14)
dim(ages_mat)
rownames(ages_mat) <- unique(cactus_reorder$individual_ID)
colnames(ages_mat) <- 0:13


as.integer(colnames(ages_mat))
unique(cactus_reorder$obs_age)

## For every place in the matrix, find the row of the dataset which matches the rowname (individualID) and colname
## (obs_age) and put the ant info from that row in the matrix location
for(c in 1:nrow(cactus_reorder)){
  ages_mat[cactus_reorder$individual_ID[c],as.character(cactus_reorder$obs_age[c])] <- as.character(cactus_reorder$ant_t1[c])
}

#ages_mat[cactus_reorder$individual_ID[1],as.character(cactus_reorder$obs_age[1])] <- cactus_reorder$ant_t1[1]
#view(ages_mat)
d <- melt(ages_mat)

png("ant_transitions_rec_only.png")
ggplot(data = d, aes(x = Var2, y = Var1, fill = value)) + 
  geom_tile() + 
  xlab("Age") + 
  ylab("Individual") +
  scale_fill_manual(values = c('white',cremcol,liomcol,othercol,vaccol))
dev.off()

######################################################################################################
######################################################################################################
####                PULL IN THE DETERMINISTIC AND STOCHASTIC DISTRIBUTIONS                        ####
######################################################################################################
######################################################################################################
## Read in lambda estimates
## deterministic
lams_dpost <- read.csv("det_post_lambda_mean.csv")
lams_dpost <- lams_dpost[,-c(1)]
## stochastic
lams_stoch <- read.csv("stoch_post_lambda.csv")
lams_stoch <- lams_stoch[,-c(1)]
## stochastic null
lams_stoch_null <- read.csv("stoch_null_post_lambda.csv")
lams_stoch_null <- lams_stoch_null[,-c(1)]
scenario_abv <- c("V","C","L","O","LC","LO","OC","LOC")

######################################################################################################
######################################################################################################
####                     VISUALIZE EACH OF THE POSTERIOR DISTRIBUTIONS                            ####
######################################################################################################
######################################################################################################
# Set the working directory to the figures folder
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
scenario_abv <- c("V","CV","LV","OV","LCV","LOV","OCV","LOCV")
## Plot the means of the deterministic and stochastic distributions together
png("lambda_means.png")
plot(c(1,3,5,7,9,11,13,15),colMeans(lams_stoch), pch = 20, cex = 5,col = cols,
     xlim = c(0,16), ylim = c(0.97,1.007),
     xaxt = "n",cex.lab = 2,
     xlab = "Ant Scenario", ylab = "Mean Lambda Value", main = "Full Partner Diversity Leads to \n Highest Fitness")
text(x = c(1,3,5,7,9,11,13,15)-0.2, y = colMeans(lams_stoch)+0.004,cex = 2, labels = scenario_abv)
legend("topleft",legend = c("Stochastic","Stochastic Null"),pch = c(20,13),cex = 1.5)
points(c(1,3,5,7,9,11,13,15),colMeans(lams_stoch_null), col = cols, cex = 5, pch = 13)
dev.off()

## Plot the distributions of the stochastic lambdas
png("lambda_stoch.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(1,1,1,1))
plot(density(lams_stoch[,1]), col = vcol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "a)                                                                                                       ",ylim = c(0,140), xlim = c(0.97,1.02))
abline(v = mean(lams_stoch[,1]),col = vcol, lty = 2, lwd =3)
legend("topright", legend = c("Vacant"), fill = c(vcol), cex = 1.5)
plot(density(lams_stoch[,2]), col = ccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "b)                                                                                                       ",ylim = c(0,140), xlim = c(0.97,1.02))
abline(v = mean(lams_stoch[,2]),col = ccol, lty = 2, lwd =3)
lines(density(lams_stoch[,3]), col = lcol, lwd =3)
abline(v = mean(lams_stoch[,3]),col = lcol, lty = 2, lwd =3)
lines(density(lams_stoch[,4]), col = ocol, lwd =3)
abline(v = mean(lams_stoch[,4]),col = ocol, lty = 2, lwd =3)
legend("topright", legend = c("C. opun","L. apic", "Other"), fill = c(ccol,lcol,ocol), cex = 1.5)
plot(density(lams_stoch[,5]), col = lccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "c)                                                                                                       ",ylim = c(0,140), xlim = c(0.97,1.02))
abline(v = mean(lams_stoch[,5]),col = lccol, lty = 2, lwd =3)
lines(density(lams_stoch[,6]), col = locol, lwd =3)
abline(v = mean(lams_stoch[,6]),col = locol, lty = 2, lwd =3)
lines(density(lams_stoch[,7]), col = cocol, lwd =3)
abline(v = mean(lams_stoch[,7]),col = cocol, lty = 2, lwd =3)
legend("topright", legend = c("C. opun and L. apic","L. apic and Other", "C. opun and Other"), fill = c(lccol,locol,cocol), cex = 1.5)
plot(density(lams_stoch[,8]), col = acol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "d)                                                                                                       ",ylim = c(0,140), xlim = c(0.97,1.02))
abline(v = mean(lams_stoch[,8]),col = acol, lty = 2, lwd =3)
mtext("Lambda",side=1,line=-2,outer=TRUE,cex=1.3)
mtext("Density",side=2,line=-2,outer=TRUE,cex=1.3,las=0)
legend("topright",legend = c("All Ants"),fill = c(acol),
       cex = 1.5)
dev.off()

## Plot the distributions of the stochastic null lambdas
png("lambda_stoch_null.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(1,1,1,1))
plot(density(lams_stoch_null[,1]), col = vcol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "a)                                                                                                       ",ylim = c(0,100), xlim = c(0.97,1.02))
abline(v = mean(lams_stoch_null[,1]),col = vcol, lty = 2, lwd =3)
legend("topright", legend = c("Vacant"), fill = c(vcol), cex = 1.5)
plot(density(lams_stoch_null[,2]), col = ccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "b)                                                                                                       ",ylim = c(0,100), xlim = c(0.97,1.02))
abline(v = mean(lams_stoch_null[,2]),col = ccol, lty = 2, lwd =3)
lines(density(lams_stoch_null[,3]), col = lcol, lwd =3)
abline(v = mean(lams_stoch_null[,3]),col = lcol, lty = 2, lwd =3)
lines(density(lams_stoch_null[,4]), col = ocol, lwd =3)
abline(v = mean(lams_stoch_null[,4]),col = ocol, lty = 2, lwd =3)
legend("topright", legend = c("Crematogaster","Liometopum", "Other"), fill = c(ccol,lcol,ocol), cex = 1.5)
plot(density(lams_stoch_null[,5]), col = lccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "c)                                                                                                       ",ylim = c(0,100), xlim = c(0.97,1.02))
abline(v = mean(lams_stoch_null[,5]),col = lccol, lty = 2, lwd =3)
lines(density(lams_stoch_null[,6]), col = locol, lwd =3)
abline(v = mean(lams_stoch_null[,6]),col = locol, lty = 2, lwd =3)
lines(density(lams_stoch_null[,7]), col = cocol, lwd =3)
abline(v = mean(lams_stoch_null[,7]),col = cocol, lty = 2, lwd =3)
legend("topright", legend = c("Crematogaster and Liometopum","Liometopum and Other", "Crematogaster and Other"), fill = c(lccol,locol,cocol), cex = 1.5)
plot(density(lams_stoch_null[,8]), col = acol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "d)                                                                                                       ",ylim = c(0,100), xlim = c(0.97,1.02))
abline(v = mean(lams_stoch_null[,8]),col = acol, lty = 2, lwd =3)
mtext("Lambda",side=1,line=-2,outer=TRUE,cex=1.3)
mtext("Density",side=2,line=-2,outer=TRUE,cex=1.3,las=0)
legend("topright",legend = c("All Ants"),fill = c(acol),
       cex = 1.5)
dev.off()

######################################################################################################
######################################################################################################
####                      COMPARE EACH OF THE POSTERIOR DISTRIBUTIONS                             ####
######################################################################################################
######################################################################################################

########################################### STOCHASTIC ##############################################
# Compare the stochastic posterior distributions to vacancy
# Calculate the difference in the between the posterior distributions of lambda
all_vac <- lams_stoch$all - lams_stoch$none
cl_vac <- lams_stoch$liomcremvac - lams_stoch$none
lo_vac <- lams_stoch$liomvacother - lams_stoch$none
co_vac <- lams_stoch$othercremvac - lams_stoch$none
c_vac <- lams_stoch$cremvac - lams_stoch$none
l_vac <- lams_stoch$liomvac - lams_stoch$none
o_vac <- lams_stoch$othervac - lams_stoch$none
vac_vac <- lams_stoch$none - lams_stoch$none
#plot them
png("lambda_stoch_difftovac.png")
par(mar=c(1,1,1,1))
layout(matrix(c(1,2,3,4,5,6,7),
              ncol = 1, nrow = 7), heights = c(1,1,1,1,1,1,1))
## All
plot(density(all_vac), col = acol, xlim = c(-0.025,0.04))
abline(v = 0, col = acol, lty = 2)
## Crem and Liom
plot(density(cl_vac), col = lccol, xlim = c(-0.025,0.04))
abline(v = 0, col = lccol, lty = 2)
## Other and Liom
plot(density(lo_vac), col = locol, xlim = c(-0.025,0.04))
abline(v = 0, col = locol, lty = 2)
## Other and Crem
plot(density(co_vac), col = cocol, xlim = c(-0.025,0.04))
abline(v = 0, col = cocol, lty = 2)
## Crem
plot(density(c_vac), col = ccol, xlim = c(-0.025,0.04))
abline(v = 0, col = ccol, lty = 2)
## Liom
plot(density(l_vac), col = lcol, xlim = c(-0.025,0.04))
abline(v = 0, col = lcol, lty = 2)
## Other
plot(density(o_vac), col = ocol, xlim = c(-0.025,0.04))
abline(v = 0, col = ocol, lty = 2)
dev.off()
#calculate what proportion of each is > 0
#aka what proportion of lambda estimations are greater than when vacant
proportions <- vector()
proportions[1] <- 0
proportions[2] <- length(subset(c_vac, c_vac>0))/50
proportions[3] <- length(subset(l_vac, l_vac>0))/50
proportions[4] <- length(subset(o_vac, o_vac>0))/50
proportions[5] <- length(subset(cl_vac, cl_vac>0))/50
proportions[6] <- length(subset(lo_vac, lo_vac>0))/50
proportions[7] <- length(subset(co_vac, co_vac>0))/50
proportions[8] <- length(subset(all_vac, all_vac>0))/50
proportions
prop <- as.data.frame(matrix(rep(NA,8), ncol = 8))
colnames(prop) <- scenario
prop[1,] <- proportions
prop

## Compare the stochastic posterior distributions to scenarios with liom
none_l <- lams_stoch$liomvac - lams_stoch$none
crem_l <- lams_stoch$liomvac - lams_stoch$cremvac
other_l <- lams_stoch$liomvac - lams_stoch$othervac
co_l <- lams_stoch$liomvac - lams_stoch$othercremvac
proportions <- vector()
proportions[1] <- length(subset(none_l, none_l>=0))/50
proportions[2] <- length(subset(crem_l, crem_l>=0))/50
proportions[3] <- length(subset(other_l, other_l>=0))/50
proportions[4] <- length(subset(co_l, co_l>=0))/50
none_lc <- lams_stoch$liomcremvac - lams_stoch$none
crem_lc <- lams_stoch$liomcremvac - lams_stoch$cremvac
other_lc <- lams_stoch$liomcremvac - lams_stoch$othervac
co_lc <- lams_stoch$liomcremvac - lams_stoch$othercremvac
proportions[5] <- length(subset(none_lc, none_lc>=0))/50
proportions[6] <- length(subset(crem_lc, crem_lc>=0))/50
proportions[7] <- length(subset(other_lc, other_lc>=0))/50
proportions[8] <- length(subset(co_lc, co_lc>=0))/50
none_lo <- lams_stoch$liomvacother - lams_stoch$none
crem_lo <- lams_stoch$liomvacother - lams_stoch$cremvac
other_lo <- lams_stoch$liomvacother - lams_stoch$othervac
co_lo <- lams_stoch$liomvacother - lams_stoch$othercremvac
proportions[9] <- length(subset(none_lo, none_lo>=0))/50
proportions[10] <- length(subset(crem_lo, crem_lo>=0))/50
proportions[11] <- length(subset(other_lo, other_lo>=0))/50
proportions[12] <- length(subset(co_lo, co_lo>=0))/50
none_a <- lams_stoch$all - lams_stoch$none
crem_a <- lams_stoch$all - lams_stoch$cremvac
other_a <- lams_stoch$all - lams_stoch$othervac
co_a <- lams_stoch$all - lams_stoch$othercremvac
proportions[13] <- length(subset(none_a, none_a>=0))/50
proportions[14] <- length(subset(crem_a, crem_a>=0))/50
proportions[15] <- length(subset(other_a, other_a>=0))/50
proportions[16] <- length(subset(co_a, co_a>=0))/50
proportions
prop <- as.data.frame(matrix(rep(NA,16), ncol = 16))
prop[1,] <- proportions
prop
########################################### STOCHASTIC AND DETERMINISTIC #############################
# Compare the deterministic difference distributions to the stochastic difference distributions
# to determine if portfolio effect is at play
all_vac_stoch_null <- lams_stoch_null$all - lams_stoch_null$none
all_vac_stoch <- lams_stoch$all - lams_stoch$none
# Plot the boost offered by the real ant scenario based on stochastic and deterministic lambda estimates
png("Figures/portfolio_effect.png")
plot(density(all_vac_stoch_null), lwd = 3, col = "chartreuse4", ylim = c(0,100), xlab = "Effect of Partner Presence on Fitness", main = "", cex.lab = 2)
lines(density(all_vac_stoch), lwd = 3, col = "violet")
abline(v = 0, lty = 2, lwd = 3)
legend("topright",legend = c("Synchronicity Possible","Synchronicity Excluded"), fill = c("violet","chartreuse4"))
dev.off()
# check the mean density 
mean(all_vac_stoch_null>0)
mean(all_vac_stoch>0)
# there appears to be a stronger fitness effect when the ants can fluctuate independently -- not a very strong portfolio effect

# What proprotion of the difference in these is >0
# 52% confident that there is a fitness boost from partner diversity
length(subset((all_vac_stoch-all_vac_stoch_null), (all_vac_stoch-all_vac_stoch_null)>0))/50

## Check which mean has a larger difference -- the answer is null has a greater difference (portfolio effect)
mean(lams_stoch$all) - mean(lams_stoch$none)
mean(lams_stoch_null$all) - mean(lams_stoch_null$none)

#################################################################################################
#################################################################################################
######## VISUALIZE THE MEAN LAMBDA BY NUMBER OF ANT PARTNERS RATHER THAN IDENTITY ###############
#################################################################################################
#################################################################################################
## No partners mean
zero_part <- mean(lams_stoch$none)
## One partners mean
one_part <- mean(colMeans(cbind(lams_stoch$cremvac,lams_stoch$liomvac, lams_stoch$othervac)))
## Two partners mean
two_part <- mean(colMeans(cbind(lams_stoch$liomcremvac,lams_stoch$liomvacother, lams_stoch$othercremvac)))
## All partners mean
all_part <- mean(lams_stoch$all)

## Plot these means
png("Figures/Lambda_Num_Partners.png")
plot(x = c(0,1,2,3), y = c(zero_part,one_part,two_part,all_part), xlab = "Number of Partners", ylab = "Mean Fitness", pch = 20, cex = 2.5,cex.lab = 1.8)
dev.off()

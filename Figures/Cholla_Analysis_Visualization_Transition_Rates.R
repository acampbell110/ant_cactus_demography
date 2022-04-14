setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
source( "/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Cholla_Analysis_Vital_Rates.R")


################################################################################################################################################
##################### Binomial Visuals ########################################################################
###############################################################################################################################################
size_dummy <- seq(min(cactus$logsize_t, na.rm = T), max(cactus$logsize_t, na.rm = T), by = 0.1)
binom_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/binom_outputs.csv", header = TRUE,stringsAsFactors=T)
occ_occ = (quantile(binom_out$beta1.1, 0.5)) + size_dummy * quantile(binom_out$beta1.2, 0.5)
occ_occ_low = (quantile(binom_out$beta1.1, 0.05)) + size_dummy * quantile(binom_out$beta1.2, 0.05)
occ_occ_high = (quantile(binom_out$beta1.1, 0.95)) + size_dummy * quantile(binom_out$beta1.2, 0.95)
occ_vac = 1 - occ_occ
occ_vac_low = 1 - occ_occ_low
occ_vac_high = 1 - occ_occ_high
vac_occ_low = quantile(binom_out$beta0.1,0.05) + size_dummy * quantile(binom_out$beta0.2, 0.05)
vac_occ = quantile(binom_out$beta0.1,0.5) + size_dummy * quantile(binom_out$beta0.2, 0.5)
vac_occ_high = quantile(binom_out$beta0.1,0.95) + size_dummy * quantile(binom_out$beta0.2, 0.95)
vac_vac = 1 - vac_occ
vac_vac_low = 1 - vac_occ_low
vac_vac_high = 1 - vac_occ_high


png("binom_panels.png")
plot(size_dummy, invlogit(occ_occ), type = "l", ylim = c(0,1), lty = 2, ylab = "Probability of being Occupied", xlab = "Log(Volume)", main = "Transition Probabilities \n Occupied vs. Vacant")
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(occ_occ_high), rev(invlogit(occ_occ_low))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
lines(size_dummy, invlogit(occ_vac), col = "black")
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(occ_vac_high), rev(invlogit(occ_vac_low))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
lines(size_dummy, invlogit(vac_occ), col = "chartreuse4", lty = 2, ylim = c(0,1))
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(vac_occ_high), rev(invlogit(vac_occ_low))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
lines(size_dummy, invlogit(vac_vac), col = "chartreuse4")
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(vac_vac_high), rev(invlogit(vac_vac_low))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
legend("topright",legend = c("Occ -> Occ", "Occ -> Vac", "Vac -> Occ", "Vac -> Vac"), col = c("black","black","chartreuse4","chartreuse4"), lty = c(2,1,2,1))
dev.off()

################################################################################################################################################
##################### Figure out the Orders ########################################################################
###############################################################################################################################################
clv <- subset(cactus, ant_t != "other" & ant_t1 != "other" )
clv$ant_t1_relevel <- droplevels(clv$ant_t1_relevel)
clv$ant_t_relevel <- droplevels(clv$ant_t_relevel)
levels(clv$ant_t_relevel)
clv <- clv[,c("ant_t_relevel","ant_t1_relevel","logsize_t", "ant_t", "ant_t1","Year_t","Plot")]
clv <- na.omit(clv)
multi_dat_real <- list(K = length(unique(clv$ant_t1_relevel)), #number of possible ant species
                       N = dim(clv)[1], #number of observations
                       D = 1, #number of predictors
                       y = as.integer(as.factor(clv$ant_t1_relevel)), #observations
                       x = model.matrix(~ 1, clv)) #design matrix
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
###### Run the model with real data & save the results
clv_null <- stan(file = "Data Analysis/STAN Models/multi_prac_tom_Km1.stan", 
                  data = multi_dat_real, warmup = 150, iter = 1000, chains = 3)
clv_null_out <- rstan::extract(clv_null, pars = "beta")
table(clv$ant_t1_relvel)/nrow(clv)
clv_fit <- summary(multinom(ant_t1_relevel ~ 1, clv))
pred_freq_null<-c(
  #pr(vacant)
  1/(1+sum(exp(clv_fit$coefficients))),
  #pr(crem)
  exp(clv_fit$coefficients[1])/(1+sum(exp(clv_fit$coefficients))),
  #pr(liom)
  exp(clv_fit$coefficients[2])/(1+sum(exp(clv_fit$coefficients)))
)
sum(pred_freq_null)
## Calculate the probabilities for each state
                              ## vac                        ## other                         ##crem
postmean_beta_null <- c(mean(clv_null_out$beta[,,1]),mean(clv_null_out$beta[,,2]))
pred_null<-c(
  #pr(liom)
  1/(1+sum(exp(postmean_beta_null))),
  #pr(vac)
  exp(postmean_beta_null[1])/(1+sum(exp(postmean_beta_null))),
  #pr(crem)
  exp(postmean_beta_null[2])/(1+sum(exp(postmean_beta_null)))
)
sum(pred_null)
## Compare to real data and frequentist outputs
## Real Data
## vac -- crem -- liom
table(clv$ant_t1)/nrow(clv)
## Freq Data
## vac,, crem, liom
pred_freq_null
##Bayes Data
## liom, vac, crem
pred_null


################################################################################################################################################
##################### Three Multinomial Visuals -- clv ########################################################################
###############################################################################################################################################
size_dummy <- seq(min(clv$logsize_t, na.rm = T), max(clv$logsize_t, na.rm = T), by = 0.1)
clv_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/multi_clv_outputs.csv", header = TRUE,stringsAsFactors=T)
## Calculate the probabilities of being tended by each ant species
## Previously tended by none
Denominator_vac <- exp(mean(multi_clv_output$beta[,1,1]) + size_dummy*mean(multi_clv_output$beta[,4,1])) + 
  exp(mean(multi_clv_output$beta[,1,2]) + size_dummy*mean(multi_clv_output$beta[,4,2])) + 
  exp(mean(multi_clv_output$beta[,1,3]) + size_dummy*mean(multi_clv_output$beta[,4,3])) 
pred_vac<-cbind(
  #pr(liom)
  exp(mean(multi_clv_output$beta[,1,1]) + size_dummy*mean(multi_clv_output$beta[,4,1]))/Denominator_vac,
  #pr(vac)
  exp(mean(multi_clv_output$beta[,1,2]) + size_dummy*mean(multi_clv_output$beta[,4,2]))/Denominator_vac,
  #pr(crem)
  exp(mean(multi_clv_output$beta[,1,3]) + size_dummy*mean(multi_clv_output$beta[,4,3]))/Denominator_vac)
for(i in 1:100){
  print(sum(pred_vac[i,]))
}
## Previously tended by Crem
Denominator_crem <- exp(mean(multi_clv_output$beta[,2,1]) + size_dummy*mean(multi_clv_output$beta[,4,1])) + 
  exp(mean(multi_clv_output$beta[,2,2]) + size_dummy*mean(multi_clv_output$beta[,4,2])) + 
  exp(mean(multi_clv_output$beta[,2,3]) + size_dummy*mean(multi_clv_output$beta[,4,3])) 
pred_crem<-cbind(
  #pr(liom)
  exp((mean(multi_clv_output$beta[,2,1])) + size_dummy*mean(multi_clv_output$beta[,4,1]))/Denominator_crem,
  #pr(vac)
  exp((mean(multi_clv_output$beta[,2,2])) + size_dummy*mean(multi_clv_output$beta[,4,2]))/Denominator_crem,
  #pr(crem)
  exp((mean(multi_clv_output$beta[,2,3])) + size_dummy*mean(multi_clv_output$beta[,4,3]))/Denominator_crem)
for(i in 1:100){
  print(sum(pred_crem[i,]))
}## Previously tended by Liom
Denominator_liom <- exp(mean(multi_clv_output$beta[,3,1]) + size_dummy*mean(multi_clv_output$beta[,4,1])) + 
  exp(mean(multi_clv_output$beta[,3,2]) + size_dummy*mean(multi_clv_output$beta[,4,2])) + 
  exp(mean(multi_clv_output$beta[,3,3]) + size_dummy*mean(multi_clv_output$beta[,4,3])) 

pred_liom<-cbind(
  #pr(liom)
  exp(mean(multi_clv_output$beta[,3,1]) + size_dummy*mean(multi_clv_output$beta[,4,1]))/Denominator_liom,
  #pr(vac)
  exp(mean(multi_clv_output$beta[,3,2]) + size_dummy*mean(multi_clv_output$beta[,4,2]))/Denominator_liom,
  #pr(crem)
  exp(mean(multi_clv_output$beta[,3,3]) + size_dummy*mean(multi_clv_output$beta[,4,3]))/Denominator_liom)
for(i in 1:100){
  print(sum(pred_liom[i,]))
}
############ High Conf Int & Low Conf Int
#### Previously Vacant #########################
Denominator_vac_l <- exp(quantile(multi_clv_output$beta[,1,1],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05)) + 
  exp(quantile(multi_clv_output$beta[,1,2],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05)) + 
  exp(quantile(multi_clv_output$beta[,1,3],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05)) 
pred_vac_l<-cbind(
  #pr(liom)
  exp(quantile(multi_clv_output$beta[,1,1],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05))/Denominator_vac_l,
  #pr(vac)
  exp(quantile(multi_clv_output$beta[,1,2],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05))/Denominator_vac_l,
  #pr(crem)
  exp(quantile(multi_clv_output$beta[,1,3],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05))/Denominator_vac_l)
Denominator_vac_h <- exp(quantile(multi_clv_output$beta[,1,1],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05)) + 
  exp(quantile(multi_clv_output$beta[,1,2],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05)) + 
  exp(quantile(multi_clv_output$beta[,1,3],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05)) 
pred_vac_h<-cbind(
  #pr(liom)
  exp(quantile(multi_clv_output$beta[,1,1],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05))/Denominator_vac_h,
  #pr(vac)
  exp(quantile(multi_clv_output$beta[,1,2],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05))/Denominator_vac_h,
  #pr(crem)
  exp(quantile(multi_clv_output$beta[,1,3],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05))/Denominator_vac_h)
#### Preciously Crem ##############################
Denominator_crem_l <- exp(quantile(multi_clv_output$beta[,2,1],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05)) + 
  exp(quantile(multi_clv_output$beta[,2,2],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05)) + 
  exp(quantile(multi_clv_output$beta[,2,3],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05)) 
pred_crem_l<-cbind(
  #pr(liom)
  exp(quantile(multi_clv_output$beta[,2,1],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05))/Denominator_crem_l,
  #pr(vac)
  exp(quantile(multi_clv_output$beta[,2,2],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05))/Denominator_crem_l,
  #pr(crem)
  exp(quantile(multi_clv_output$beta[,2,3],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05))/Denominator_crem_l)
Denominator_crem_h <- exp(quantile(multi_clv_output$beta[,2,1],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05)) + 
  exp(quantile(multi_clv_output$beta[,2,2],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05)) + 
  exp(quantile(multi_clv_output$beta[,2,3],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05)) 
pred_crem_h<-cbind(
  #pr(liom)
  exp(quantile(multi_clv_output$beta[,2,1],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05))/Denominator_crem_h,
  #pr(vac)
  exp(quantile(multi_clv_output$beta[,2,2],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05))/Denominator_crem_h,
  #pr(crem)
  exp(quantile(multi_clv_output$beta[,2,3],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05))/Denominator_crem_h)
#### Preciously Liom ##############################
Denominator_liom_l <- exp(quantile(multi_clv_output$beta[,3,1],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05)) + 
  exp(quantile(multi_clv_output$beta[,3,2],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05)) + 
  exp(quantile(multi_clv_output$beta[,3,3],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05)) 
pred_liom_l<-cbind(
  #pr(liom)
  exp(quantile(multi_clv_output$beta[,3,1],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05))/Denominator_liom_l,
  #pr(vac)
  exp(quantile(multi_clv_output$beta[,3,2],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05))/Denominator_liom_l,
  #pr(crem)
  exp(quantile(multi_clv_output$beta[,3,3],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05))/Denominator_liom_l)
Denominator_liom_h <- exp(quantile(multi_clv_output$beta[,3,1],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05)) + 
  exp(quantile(multi_clv_output$beta[,3,2],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05)) + 
  exp(quantile(multi_clv_output$beta[,3,3],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05)) 
pred_liom_h<-cbind(
  #pr(liom)
  exp(quantile(multi_clv_output$beta[,3,1],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05))/Denominator_liom_h,
  #pr(vac)
  exp(quantile(multi_clv_output$beta[,3,2],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05))/Denominator_liom_h,
  #pr(crem)
  exp(quantile(multi_clv_output$beta[,3,3],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05))/Denominator_liom_h)

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
## Previously Vacant
plot(size_dummy, pred_vac[,1], type = "l", col = "blue", main = "Previously Vacant", ylim = c(0,1))
polygon(c(size_dummy,rev(size_dummy)),c((pred_vac_h[,1]), rev((pred_vac_l[,1]))),
        col = rgb(red = 0.0, blue = 1, green = 0.0,alpha = 0.15), border = NA)
lines(size_dummy, pred_vac[,2], col = "pink")
polygon(c(size_dummy,rev(size_dummy)),c((pred_vac_h[,2]), rev((pred_vac_l[,2]))),
        col = rgb(red = 0.9, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
lines(size_dummy, pred_vac[,3], col = "red")
polygon(c(size_dummy,rev(size_dummy)),c((pred_vac_h[,3]), rev((pred_vac_l[,3]))),
        col = rgb(red = 1, blue = 0.0, green = 0.0,alpha = 0.1), border = NA)
## Previously Crem
plot(size_dummy, pred_crem[,1], type = "l", col = "blue", main = "Previously Crem.", ylim = c(0,1))
polygon(c(size_dummy,rev(size_dummy)),c((pred_crem_h[,1]), rev((pred_crem_l[,1]))),
        col = rgb(red = 0.0, blue = 1, green = 0.0,alpha = 0.15), border = NA)
lines(size_dummy, pred_crem[,2], col = "pink")
polygon(c(size_dummy,rev(size_dummy)),c((pred_crem_h[,2]), rev((pred_crem_l[,2]))),
        col = rgb(red = 0.9, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
lines(size_dummy, pred_crem[,3], col = "red")
polygon(c(size_dummy,rev(size_dummy)),c((pred_crem_h[,3]), rev((pred_crem_l[,3]))),
        col = rgb(red = 1, blue = 0.0, green = 0.0,alpha = 0.1), border = NA)
## Previously Liom
plot(size_dummy, pred_liom[,1], type = "l", col = "blue", main = "Previously Crem.", ylim = c(0,1))
polygon(c(size_dummy,rev(size_dummy)),c((pred_liom_h[,1]), rev((pred_liom_l[,1]))),
        col = rgb(red = 0.0, blue = 1, green = 0.0,alpha = 0.15), border = NA)
lines(size_dummy, pred_liom[,2], col = "pink")
polygon(c(size_dummy,rev(size_dummy)),c((pred_liom_h[,2]), rev((pred_liom_l[,2]))),
        col = rgb(red = 0.9, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
lines(size_dummy, pred_liom[,3], col = "red")
polygon(c(size_dummy,rev(size_dummy)),c((pred_liom_h[,3]), rev((pred_liom_l[,3]))),
        col = rgb(red = 1, blue = 0.0, green = 0.0,alpha = 0.1), border = NA)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")



## Compare probabilities to real proportions
a <- table(clv$ant_t1_relevel,clv$ant_t_relevel)
num_vac <- sum(clv$ant_t_relevel == "vacant")
print(a[,1]/sum(clv$ant_t_relevel == "vacant"))
print(c(mean(pred_vac[,1]),mean(pred_vac[,2]), mean(pred_vac[,3])))
print(c(mean(pred_crem[,1]),mean(pred_crem[,2]), mean(pred_crem[,3])))
print(c(mean(pred_liom[,1]),mean(pred_liom[,2]), mean(pred_liom[,3])))

################################################################################################################################################
##################### Three Multinomial Visuals -- cov ########################################################################
###############################################################################################################################################
size_dummy <- seq(min(clv$logsize_t, na.rm = T), max(clv$logsize_t, na.rm = T), by = 0.1)
clv_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/multi_clv_outputs.csv", header = TRUE,stringsAsFactors=T)
## Calculate the probabilities of being tended by each ant species
## Previously tended by none
Denominator_vac <- exp(mean(multi_clv_output$beta[,1,1]) + size_dummy*mean(multi_clv_output$beta[,4,1])) + 
  exp(mean(multi_clv_output$beta[,1,2]) + size_dummy*mean(multi_clv_output$beta[,4,2])) + 
  exp(mean(multi_clv_output$beta[,1,3]) + size_dummy*mean(multi_clv_output$beta[,4,3])) 
pred_vac<-cbind(
  #pr(liom)
  exp(mean(multi_clv_output$beta[,1,1]) + size_dummy*mean(multi_clv_output$beta[,4,1]))/Denominator_vac,
  #pr(vac)
  exp(mean(multi_clv_output$beta[,1,2]) + size_dummy*mean(multi_clv_output$beta[,4,2]))/Denominator_vac,
  #pr(crem)
  exp(mean(multi_clv_output$beta[,1,3]) + size_dummy*mean(multi_clv_output$beta[,4,3]))/Denominator_vac)
for(i in 1:100){
  print(sum(pred_vac[i,]))
}
## Previously tended by Crem
Denominator_crem <- exp(mean(multi_clv_output$beta[,2,1]) + size_dummy*mean(multi_clv_output$beta[,4,1])) + 
  exp(mean(multi_clv_output$beta[,2,2]) + size_dummy*mean(multi_clv_output$beta[,4,2])) + 
  exp(mean(multi_clv_output$beta[,2,3]) + size_dummy*mean(multi_clv_output$beta[,4,3])) 
pred_crem<-cbind(
  #pr(liom)
  exp((mean(multi_clv_output$beta[,2,1])) + size_dummy*mean(multi_clv_output$beta[,4,1]))/Denominator_crem,
  #pr(vac)
  exp((mean(multi_clv_output$beta[,2,2])) + size_dummy*mean(multi_clv_output$beta[,4,2]))/Denominator_crem,
  #pr(crem)
  exp((mean(multi_clv_output$beta[,2,3])) + size_dummy*mean(multi_clv_output$beta[,4,3]))/Denominator_crem)
for(i in 1:100){
  print(sum(pred_crem[i,]))
}## Previously tended by Liom
Denominator_liom <- exp(mean(multi_clv_output$beta[,3,1]) + size_dummy*mean(multi_clv_output$beta[,4,1])) + 
  exp(mean(multi_clv_output$beta[,3,2]) + size_dummy*mean(multi_clv_output$beta[,4,2])) + 
  exp(mean(multi_clv_output$beta[,3,3]) + size_dummy*mean(multi_clv_output$beta[,4,3])) 

pred_liom<-cbind(
  #pr(liom)
  exp(mean(multi_clv_output$beta[,3,1]) + size_dummy*mean(multi_clv_output$beta[,4,1]))/Denominator_liom,
  #pr(vac)
  exp(mean(multi_clv_output$beta[,3,2]) + size_dummy*mean(multi_clv_output$beta[,4,2]))/Denominator_liom,
  #pr(crem)
  exp(mean(multi_clv_output$beta[,3,3]) + size_dummy*mean(multi_clv_output$beta[,4,3]))/Denominator_liom)
for(i in 1:100){
  print(sum(pred_liom[i,]))
}
############ High Conf Int & Low Conf Int
#### Previously Vacant #########################
Denominator_vac_l <- exp(quantile(multi_clv_output$beta[,1,1],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05)) + 
  exp(quantile(multi_clv_output$beta[,1,2],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05)) + 
  exp(quantile(multi_clv_output$beta[,1,3],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05)) 
pred_vac_l<-cbind(
  #pr(liom)
  exp(quantile(multi_clv_output$beta[,1,1],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05))/Denominator_vac_l,
  #pr(vac)
  exp(quantile(multi_clv_output$beta[,1,2],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05))/Denominator_vac_l,
  #pr(crem)
  exp(quantile(multi_clv_output$beta[,1,3],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05))/Denominator_vac_l)
Denominator_vac_h <- exp(quantile(multi_clv_output$beta[,1,1],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05)) + 
  exp(quantile(multi_clv_output$beta[,1,2],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05)) + 
  exp(quantile(multi_clv_output$beta[,1,3],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05)) 
pred_vac_h<-cbind(
  #pr(liom)
  exp(quantile(multi_clv_output$beta[,1,1],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05))/Denominator_vac_h,
  #pr(vac)
  exp(quantile(multi_clv_output$beta[,1,2],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05))/Denominator_vac_h,
  #pr(crem)
  exp(quantile(multi_clv_output$beta[,1,3],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05))/Denominator_vac_h)
#### Preciously Crem ##############################
Denominator_crem_l <- exp(quantile(multi_clv_output$beta[,2,1],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05)) + 
  exp(quantile(multi_clv_output$beta[,2,2],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05)) + 
  exp(quantile(multi_clv_output$beta[,2,3],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05)) 
pred_crem_l<-cbind(
  #pr(liom)
  exp(quantile(multi_clv_output$beta[,2,1],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05))/Denominator_crem_l,
  #pr(vac)
  exp(quantile(multi_clv_output$beta[,2,2],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05))/Denominator_crem_l,
  #pr(crem)
  exp(quantile(multi_clv_output$beta[,2,3],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05))/Denominator_crem_l)
Denominator_crem_h <- exp(quantile(multi_clv_output$beta[,2,1],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05)) + 
  exp(quantile(multi_clv_output$beta[,2,2],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05)) + 
  exp(quantile(multi_clv_output$beta[,2,3],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05)) 
pred_crem_h<-cbind(
  #pr(liom)
  exp(quantile(multi_clv_output$beta[,2,1],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05))/Denominator_crem_h,
  #pr(vac)
  exp(quantile(multi_clv_output$beta[,2,2],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05))/Denominator_crem_h,
  #pr(crem)
  exp(quantile(multi_clv_output$beta[,2,3],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05))/Denominator_crem_h)
#### Preciously Liom ##############################
Denominator_liom_l <- exp(quantile(multi_clv_output$beta[,3,1],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05)) + 
  exp(quantile(multi_clv_output$beta[,3,2],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05)) + 
  exp(quantile(multi_clv_output$beta[,3,3],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05)) 
pred_liom_l<-cbind(
  #pr(liom)
  exp(quantile(multi_clv_output$beta[,3,1],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05))/Denominator_liom_l,
  #pr(vac)
  exp(quantile(multi_clv_output$beta[,3,2],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05))/Denominator_liom_l,
  #pr(crem)
  exp(quantile(multi_clv_output$beta[,3,3],0.05) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05))/Denominator_liom_l)
Denominator_liom_h <- exp(quantile(multi_clv_output$beta[,3,1],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05)) + 
  exp(quantile(multi_clv_output$beta[,3,2],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05)) + 
  exp(quantile(multi_clv_output$beta[,3,3],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05)) 
pred_liom_h<-cbind(
  #pr(liom)
  exp(quantile(multi_clv_output$beta[,3,1],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,1],0.05))/Denominator_liom_h,
  #pr(vac)
  exp(quantile(multi_clv_output$beta[,3,2],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,2],0.05))/Denominator_liom_h,
  #pr(crem)
  exp(quantile(multi_clv_output$beta[,3,3],0.95) + size_dummy*quantile(multi_clv_output$beta[,4,3],0.05))/Denominator_liom_h)

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
## Previously Vacant
plot(size_dummy, pred_vac[,1], type = "l", col = "blue", main = "Previously Vacant", ylim = c(0,1))
polygon(c(size_dummy,rev(size_dummy)),c((pred_vac_h[,1]), rev((pred_vac_l[,1]))),
        col = rgb(red = 0.0, blue = 1, green = 0.0,alpha = 0.15), border = NA)
lines(size_dummy, pred_vac[,2], col = "pink")
polygon(c(size_dummy,rev(size_dummy)),c((pred_vac_h[,2]), rev((pred_vac_l[,2]))),
        col = rgb(red = 0.9, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
lines(size_dummy, pred_vac[,3], col = "red")
polygon(c(size_dummy,rev(size_dummy)),c((pred_vac_h[,3]), rev((pred_vac_l[,3]))),
        col = rgb(red = 1, blue = 0.0, green = 0.0,alpha = 0.1), border = NA)
## Previously Crem
plot(size_dummy, pred_crem[,1], type = "l", col = "blue", main = "Previously Crem.", ylim = c(0,1))
polygon(c(size_dummy,rev(size_dummy)),c((pred_crem_h[,1]), rev((pred_crem_l[,1]))),
        col = rgb(red = 0.0, blue = 1, green = 0.0,alpha = 0.15), border = NA)
lines(size_dummy, pred_crem[,2], col = "pink")
polygon(c(size_dummy,rev(size_dummy)),c((pred_crem_h[,2]), rev((pred_crem_l[,2]))),
        col = rgb(red = 0.9, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
lines(size_dummy, pred_crem[,3], col = "red")
polygon(c(size_dummy,rev(size_dummy)),c((pred_crem_h[,3]), rev((pred_crem_l[,3]))),
        col = rgb(red = 1, blue = 0.0, green = 0.0,alpha = 0.1), border = NA)
## Previously Liom
plot(size_dummy, pred_liom[,1], type = "l", col = "blue", main = "Previously Crem.", ylim = c(0,1))
polygon(c(size_dummy,rev(size_dummy)),c((pred_liom_h[,1]), rev((pred_liom_l[,1]))),
        col = rgb(red = 0.0, blue = 1, green = 0.0,alpha = 0.15), border = NA)
lines(size_dummy, pred_liom[,2], col = "pink")
polygon(c(size_dummy,rev(size_dummy)),c((pred_liom_h[,2]), rev((pred_liom_l[,2]))),
        col = rgb(red = 0.9, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
lines(size_dummy, pred_liom[,3], col = "red")
polygon(c(size_dummy,rev(size_dummy)),c((pred_liom_h[,3]), rev((pred_liom_l[,3]))),
        col = rgb(red = 1, blue = 0.0, green = 0.0,alpha = 0.1), border = NA)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")



## Compare probabilities to real proportions
a <- table(clv$ant_t1_relevel,clv$ant_t_relevel)
num_vac <- sum(clv$ant_t_relevel == "vacant")
print(a[,1]/sum(clv$ant_t_relevel == "vacant"))
print(c(mean(pred_vac[,1]),mean(pred_vac[,2]), mean(pred_vac[,3])))
print(c(mean(pred_crem[,1]),mean(pred_crem[,2]), mean(pred_crem[,3])))
print(c(mean(pred_liom[,1]),mean(pred_liom[,2]), mean(pred_liom[,3])))




###################################################################################################
#### Full Multinomial Model #######################################################################
###################################################################################################
## Calculate the probabilities of being tended by each ant species
size_dummy_real <- seq(min((cactus_real$logsize_t)), max((cactus_real$logsize_t)), by=0.1)
## Previously tended by none
d_vac <- exp(mean(cholla.multi[1,]) + size_dummy_real*mean(cholla.multi[17,])) + 
  exp(mean(cholla.multi[2,]) + size_dummy_real*mean(cholla.multi[18,])) + 
  exp(mean(cholla.multi[3,]) + size_dummy_real*mean(cholla.multi[19,])) + 
  exp(mean(cholla.multi[4,]) + size_dummy_real*mean(cholla.multi[20,]))
pred_vac<-cbind(
  ##pr(vac)
  exp(mean(cholla.multi[1,]) + size_dummy_real*mean(cholla.multi[17,]))/d_vac,
  ##pr(other)
  exp(mean(cholla.multi[2,]) + size_dummy_real*mean(cholla.multi[18,]))/d_vac,
  ##pr(crem)
  exp(mean(cholla.multi[3,]) + size_dummy_real*mean(cholla.multi[19,]))/d_vac,
  ##pr(liom)
  exp(mean(cholla.multi[4,]) + size_dummy_real*mean(cholla.multi[20,]))/d_vac
)
for(i in 1:ncol(cholla.multi)){
  a[i]<-sum(pred_vac[i,])
  print(a[i])
}
## Previously tended by Other
d_other <- exp(mean(cholla.multi[5,]) + size_dummy_real*mean(cholla.multi[17,])) + 
  exp(mean(cholla.multi[6,]) + size_dummy_real*mean(cholla.multi[18,])) + 
  exp(mean(cholla.multi[7,]) + size_dummy_real*mean(cholla.multi[19,])) + 
  exp(mean(cholla.multi[8,]) + size_dummy_real*mean(cholla.multi[20,]))
pred_other<-cbind(
  ##pr(vac)
  exp(mean(cholla.multi[5,]) + size_dummy_real*mean(cholla.multi[17,]))/d_other,
  ##pr(other)
  exp(mean(cholla.multi[6,]) + size_dummy_real*mean(cholla.multi[18,]))/d_other,
  ##pr(crem)
  exp(mean(cholla.multi[7,]) + size_dummy_real*mean(cholla.multi[19,]))/d_other,
  ##pr(liom)
  exp(mean(cholla.multi[8,]) + size_dummy_real*mean(cholla.multi[20,]))/d_other
)
for(i in 1:ncol(cholla.multi)){
  a[i]<-sum(pred_other[i,])
  print(a[i])
}
## Previously tended by other
d_crem <- exp(mean(cholla.multi[9,]) + size_dummy_real*mean(cholla.multi[17,])) + 
  exp(mean(cholla.multi[10,]) + size_dummy_real*mean(cholla.multi[18,])) + 
  exp(mean(cholla.multi[11,]) + size_dummy_real*mean(cholla.multi[19,])) + 
  exp(mean(cholla.multi[12,]) + size_dummy_real*mean(cholla.multi[20,]))
pred_crem<-cbind(
  ##pr(vac)
  exp(mean(cholla.multi[9,]) + size_dummy_real*mean(cholla.multi[17,]))/d_crem,
  ##pr(other)
  exp(mean(cholla.multi[10,]) + size_dummy_real*mean(cholla.multi[18,]))/d_crem,
  ##pr(crem)
  exp(mean(cholla.multi[11,]) + size_dummy_real*mean(cholla.multi[19,]))/d_crem,
  ##pr(liom)
  exp(mean(cholla.multi[12,]) + size_dummy_real*mean(cholla.multi[20,]))/d_crem
)
for(i in 1:ncol(cholla.multi)){
  a[i]<-sum(pred_crem[i,])
  print(a[i])
}
## Previously tended by Liom
d_liom <- exp(mean(cholla.multi[13,]) + size_dummy_real*mean(cholla.multi[17,])) + 
  exp(mean(cholla.multi[14,]) + size_dummy_real*mean(cholla.multi[18,])) + 
  exp(mean(cholla.multi[15,]) + size_dummy_real*mean(cholla.multi[19,])) + 
  exp(mean(cholla.multi[16,]) + size_dummy_real*mean(cholla.multi[20,]))
pred_liom<-cbind(
  ##pr(vac)
  exp(mean(cholla.multi[13,]) + size_dummy_real*mean(cholla.multi[17,]))/d_liom,
  ##pr(other)
  exp(mean(cholla.multi[14,]) + size_dummy_real*mean(cholla.multi[18,]))/d_liom,
  ##pr(crem)
  exp(mean(cholla.multi[15,]) + size_dummy_real*mean(cholla.multi[19,]))/d_liom,
  ##pr(liom)
  exp(mean(cholla.multi[16,]) + size_dummy_real*mean(cholla.multi[20,]))/d_liom
)
for(i in 1:ncol(cholla.multi)){
  a[i]<-sum(pred_liom[i,])
  print(a[i])
}
## vac -> vac       vac -> other    vac -> crem       vac -> liom
pred_probs_vac <- cbind((pred_vac[,1]) , (pred_vac[,2]) , (pred_vac[,3]) , (pred_vac[,4]))
## other-> vac        other -> other    other -> crem       other -> liom
pred_probs_other <- cbind((pred_other[,1]) , (pred_other[,2]) , (pred_other[,3]) , (pred_other[,4]))
## crem-> vac       crem -> other    crem -> crem      crem -> liom
pred_probs_crem <- cbind((pred_crem[,1]) , (pred_crem[,2]) , (pred_crem[,3]) , (pred_crem[,4]))
## liom-> vac       liom -> other    liom -> crem       liom -> liom
pred_probs_liom <- cbind((pred_liom[,1]) , (pred_liom[,2]) , (pred_liom[,3]) , (pred_liom[,4]))





setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")


multi_dat_real <- list(K = length(unique(cactus_real$ant_t1_relevel)), #number of possible ant species
                       N = dim(cactus_real)[1], #number of observations
                       D = 5, #number of predictors
                       y = as.integer(as.factor(cactus_real$ant_t1_relevel)), #observations
                       x = model.matrix(~ 0 + (as.factor(ant_t_relevel)) + logsize_t, cactus_real)
                       ) #design matrix
## Run the model & save the results
real_ant_size <- stan(file = "Data Analysis/STAN Models/multi_prac_tom_Km1.stan", 
                      data = multi_dat_real, warmup = 100, iter = 1000, chains = 1)
real_ant_size_out <- rstan::extract(real_ant_size, pars = c("beta"))
write.csv(real_ant_size_out, "multi_ant_size_out")
## plot the chains
mcmc_trace(real_ant_size)
summary(real_ant_size)
## this looks pretty good. All betas converge

## Calculate the probabilities of being tended by each ant species
## Previously tended by none
Denominator_vac <- exp(mean(real_ant_size_out$beta[,1,1]) + size_dummy_real*mean(real_ant_size_out$beta[,5,1])) + 
  exp(mean(real_ant_size_out$beta[,1,2]) + size_dummy_real*mean(real_ant_size_out$beta[,5,2])) + 
  exp(mean(real_ant_size_out$beta[,1,3]) + size_dummy_real*mean(real_ant_size_out$beta[,5,3])) + 
  exp(mean(real_ant_size_out$beta[,1,4]) + size_dummy_real*mean(real_ant_size_out$beta[,5,4]))
pred_vac<-cbind(
  #pr(vacant)
  exp(mean(real_ant_size_out$beta[,1,1]) + size_dummy_real*mean(real_ant_size_out$beta[,5,1]))/Denominator_vac,
  #pr(other)
  exp(mean(real_ant_size_out$beta[,1,2]) + size_dummy_real*mean(real_ant_size_out$beta[,5,2]))/Denominator_vac,
  #pr(crem)
  exp(mean(real_ant_size_out$beta[,1,3]) + size_dummy_real*mean(real_ant_size_out$beta[,5,3]))/Denominator_vac,
  #pr(liom)
  exp(mean(real_ant_size_out$beta[,1,4]) + size_dummy_real*mean(real_ant_size_out$beta[,5,4]))/Denominator_vac)
sum(pred_vac[1,])
## Previously tended by Crem
Denominator_other <- exp(mean(real_ant_size_out$beta[,2,1]) + size_dummy_real*mean(real_ant_size_out$beta[,5,1])) + 
  exp(mean(real_ant_size_out$beta[,2,2]) + size_dummy_real*mean(real_ant_size_out$beta[,5,2])) + 
  exp(mean(real_ant_size_out$beta[,2,3]) + size_dummy_real*mean(real_ant_size_out$beta[,5,3])) + 
  exp(mean(real_ant_size_out$beta[,2,4]) + size_dummy_real*mean(real_ant_size_out$beta[,5,4]))
pred_other<-cbind(
  #pr(vacant)
  exp((mean(real_ant_size_out$beta[,2,1])) + size_dummy_real*mean(real_ant_size_out$beta[,5,1]))/Denominator_other,
  #pr(other)
  exp((mean(real_ant_size_out$beta[,2,2])) + size_dummy_real*mean(real_ant_size_out$beta[,5,2]))/Denominator_other,
  #pr(crem)
  exp((mean(real_ant_size_out$beta[,2,3])) + size_dummy_real*mean(real_ant_size_out$beta[,5,3]))/Denominator_other,
  #pr(liom)
  exp((mean(real_ant_size_out$beta[,2,4])) + size_dummy_real*mean(real_ant_size_out$beta[,5,4]))/Denominator_other)
sum(pred_other[1,])
## Previously tended by Liom
Denominator_crem <- exp(mean(real_ant_size_out$beta[,3,1]) + size_dummy_real*mean(real_ant_size_out$beta[,5,1])) + 
  exp(mean(real_ant_size_out$beta[,3,2]) + size_dummy_real*mean(real_ant_size_out$beta[,5,2])) + 
  exp(mean(real_ant_size_out$beta[,3,3]) + size_dummy_real*mean(real_ant_size_out$beta[,5,3])) + 
  exp(mean(real_ant_size_out$beta[,3,4]) + size_dummy_real*mean(real_ant_size_out$beta[,5,4]))

pred_crem<-cbind(
  #pr(vacant)
  exp(mean(real_ant_size_out$beta[,3,1]) + size_dummy_real*mean(real_ant_size_out$beta[,5,1]))/Denominator_crem,
  #pr(other)
  exp(mean(real_ant_size_out$beta[,3,2]) + size_dummy_real*mean(real_ant_size_out$beta[,5,2]))/Denominator_crem,
  #pr(crem)
  exp(mean(real_ant_size_out$beta[,3,3]) + size_dummy_real*mean(real_ant_size_out$beta[,5,3]))/Denominator_crem,
  #pr(liom)
  exp(mean(real_ant_size_out$beta[,3,4]) + size_dummy_real*mean(real_ant_size_out$beta[,5,4]))/Denominator_crem)
sum(pred_crem[1,])
## Previously tended by Other
Denominator_liom <- exp(mean(real_ant_size_out$beta[,4,1]) + size_dummy_real*mean(real_ant_size_out$beta[,5,1])) + 
  exp(mean(real_ant_size_out$beta[,4,2]) + size_dummy_real*mean(real_ant_size_out$beta[,5,2])) + 
  exp(mean(real_ant_size_out$beta[,4,3]) + size_dummy_real*mean(real_ant_size_out$beta[,5,3])) + 
  exp(mean(real_ant_size_out$beta[,4,4]) + size_dummy_real*mean(real_ant_size_out$beta[,5,4]))

pred_liom<-cbind(
  #pr(vacant)
  exp(mean(real_ant_size_out$beta[,4,1]) + size_dummy_real*mean(real_ant_size_out$beta[,5,1]))/Denominator_liom,
  #pr(other)
  exp(mean(real_ant_size_out$beta[,4,2]) + size_dummy_real*mean(real_ant_size_out$beta[,5,2]))/Denominator_liom,
  #pr(crem)
  exp(mean(real_ant_size_out$beta[,4,3]) + size_dummy_real*mean(real_ant_size_out$beta[,5,3]))/Denominator_liom,
  #pr(liom)
  exp(mean(real_ant_size_out$beta[,4,4]) + size_dummy_real*mean(real_ant_size_out$beta[,5,4]))/Denominator_liom)
sum(pred_liom[1,])
## vac -> vac       vac -> other    vac -> crem       vac -> liom
pred_probs_vac <- cbind((pred_vac[,1]) , (pred_vac[,2]) , (pred_vac[,3]) , (pred_vac[,4]))
## other-> vac        other -> other    other -> crem       other -> liom
pred_probs_other <- cbind((pred_other[,1]) , (pred_other[,2]) , (pred_other[,3]) , (pred_other[,4]))
## crem-> vac       crem -> other    crem -> crem      crem -> liom
pred_probs_crem <- cbind((pred_crem[,1]) , (pred_crem[,2]) , (pred_crem[,3]) , (pred_crem[,4]))
## liom-> vac       liom -> other    liom -> crem       liom -> liom
pred_probs_liom <- cbind((pred_liom[,1]) , (pred_liom[,2]) , (pred_liom[,3]) , (pred_liom[,4]))

colMeans(pred_probs_vac)
colMeans(pred_probs_other)
colMeans(pred_probs_crem)
colMeans(pred_probs_liom)

obstab <- table(cactus_real$ant_t1_relevel,cactus_real$ant_t_relevel)
obstab[,1]/colSums(obstab)[1]
obstab[,2]/colSums(obstab)[2]
obstab[,3]/colSums(obstab)[3]
obstab[,4]/colSums(obstab)[4]

## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_Size_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,2,3,4,5),
              ncol = 2, nrow = 3, byrow = TRUE), heights = c(0.6,1.4,1.4), widths = c(3.9,3.9))
plot.new()
text(0.5,0.5,"Ant States",cex=2,font=2)
plot(size_dummy_real, pred_vac[,1], type = "l", col = "pink",main = "Previously Vacant", ylim = c(0,1))
lines(size_dummy_real, pred_vac[,2], col = "black")
lines(size_dummy_real, pred_vac[,3], col = "red")
lines(size_dummy_real, pred_vac[,4], col = "blue")
plot(size_dummy_real, pred_other[,1], type = "l", col = "pink",main = "Previously Other", ylim = c(0,1))
lines(size_dummy_real, pred_other[,2], col = "black")
lines(size_dummy_real, pred_other[,3], col = "red")
lines(size_dummy_real, pred_other[,4], col = "blue")
legend("topright",c("vacant","other","crem.","liom."), fill = c("pink","black","red","blue"))
plot(size_dummy_real, pred_crem[,1], type = "l", col = "pink",main = "Previously Crem", ylim = c(0,1))
lines(size_dummy_real, pred_crem[,2], col = "black")
lines(size_dummy_real, pred_crem[,3], col = "red")
lines(size_dummy_real, pred_crem[,4], col = "blue")
plot(size_dummy_real, pred_liom[,1], type = "l", col = "pink",main = "Previously Liom", ylim = c(0,1))
lines(size_dummy_real, pred_liom[,2], col = "black")
lines(size_dummy_real, pred_liom[,3], col = "red")
lines(size_dummy_real, pred_liom[,4], col = "blue")
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")







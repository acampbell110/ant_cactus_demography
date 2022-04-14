## simulation code from: https://stats.stackexchange.com/questions/103728/simulating-multinomial-logit-data-with-r
#Adding library for multinomial logit regression
setwd("C:/Users/tm9/Dropbox/github/ant_cactus_demography")
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
library("nnet")
library(rstan)
library(bayesplot)

###### Simulate Fake Data 
#Genarating 500 random numbers with zero mean
x = rnorm(3885,0)
#Assigning the values of beta1 and beta2
Beta1 = 20
Beta2 = .05
#Calculation of denominator for probability calculation
Denominator= exp(Beta1*x)+exp(Beta2*x)
#Calculating the matrix of probabilities for three choices
vProb = cbind(1/Denominator, exp(x*Beta1)/Denominator, exp(x*Beta2)/Denominator )
## check that all rows sum to 1
sum(vProb[100,])
# Assigning the value one to maximum probability and zero for rest to get the appropriate choices for value of x
mChoices = t(apply(vProb, 1, rmultinom, n = 1, size = 1))
# Value of Y and X together
dfM = cbind.data.frame(y = apply(mChoices, 1, function(x) which(x==1)), x)
#We want zero intercept hence x+0 hence the foumula of regression as below
fit<-(multinom(y ~ x + 0, dfM))
#This function uses first y as base class 
#hence upper probability calculation is changed
summary(fit)
#In case we do not keep intercept as zero
fit2<-multinom(y ~ x, dfM)
summary(fit2)


###### Import real data and format it
cactus_real <- cactus[,c("ant_t","ant_t1","logsize_t","Year_t","Plot")]
cactus_real <- na.omit(cactus_real)
cactus_real$ant_t1_relevel <- relevel(cactus_real$ant_t1,ref = "vacant")
cactus_real$ant_t_relevel <- relevel(cactus_real$ant_t, ref = "vacant")
cactus_real <- cactus_real[,c("ant_t_relevel","ant_t1_relevel","logsize_t", "ant_t", "ant_t1","Year_t","Plot")]

#### fit model of the mean using the nnet function
table(cactus_real$ant_t1)/nrow(cactus_real)
cactus_fit <- summary(multinom(ant_t1_relevel ~ 1, cactus_real))
pred_freq_null<-c(
#pr(vacant)
1/(1+sum(exp(cactus_fit$coefficients))),
#pr(other)
exp(cactus_fit$coefficients[1])/(1+sum(exp(cactus_fit$coefficients))),
#pr(crem)
exp(cactus_fit$coefficients[2])/(1+sum(exp(cactus_fit$coefficients))),
#pr(liom)
exp(cactus_fit$coefficients[3])/(1+sum(exp(cactus_fit$coefficients))))
sum(pred_freq_null)


####################################################################################################
###### Null Stan Model -- Real Data ################################################################
####################################################################################################
### now fit model of the mean with stan
multi_dat_real <- list(K = length(unique(cactus_real$ant_t1)), #number of possible ant species
                       N = dim(cactus_real)[1], #number of observations
                       D = 1, #number of predictors
                       y = as.integer(as.factor(cactus_real$ant_t1)), #observations
                       x = model.matrix(~ 1, cactus_real)) #design matrix

###### Run the model with real data & save the results
real_null <- stan(file = "Data Analysis/STAN Models/multi_prac_tom_Km1.stan", 
                        data = multi_dat_real, warmup = 500, iter = 4000, chains = 3)
real_null_out <- rstan::extract(real_null, pars = "beta")
## plot the chains
mcmc_trace(real_null)
## all chains converge

## Calculate the probabilities for each state
                          ## vac                        ## other                         ##crem
postmean_beta_null <- c(mean(real_null_out$beta[,,1]),mean(real_null_out$beta[,,2]),mean(real_null_out$beta[,,3]))
pred_null<-c(
  #pr(liom)
  1/(1+sum(exp(postmean_beta_null))),
  #pr(vac)
  exp(postmean_beta_null[1])/(1+sum(exp(postmean_beta_null))),
  #pr(other)
  exp(postmean_beta_null[2])/(1+sum(exp(postmean_beta_null))),
  #pr(crem)
  exp(postmean_beta_null[3])/(1+sum(exp(postmean_beta_null))))
sum(pred_null)
## Compare to real data and frequentist outputs
## Real Data
table(cactus_real$ant_t1)/nrow(cactus_real)
## Freq Data
## vac, other, crem, liom
pred_freq_null
##Bayes Data
## liom, vac, other, crem
pred_null


############################################################################################
###### now include size as the only predictor ####################################################
###### Data Analysis/STAN Models/multi_prac_tom_Km1.stan" ##################################
############################################################################################
as.integer(as.factor(cactus_real$ant_t1_relevel[1:100]))
(as.factor(cactus_real$ant_t1_relevel[1:100]))
multi_dat_real <- list(K = length(unique(cactus_real$ant_t1_relevel)), #number of possible ant species
                       N = dim(cactus_real)[1], #number of observations
                       D = 2, #number of predictors
                       y = as.integer(as.factor(cactus_real$ant_t1_relevel)), #observations
                       x = model.matrix(~ logsize_t, cactus_real)) #design matrix
## run the model & save the results
real_size <- stan(file = "Data Analysis/STAN Models/multi_prac_tom_Km1.stan", 
                 data = multi_dat_real, warmup = 100, iter = 1000, chains = 3)
real_size_out <- rstan::extract(real_size, pars = c("beta"))
write.csv(real_size_out, "multi_size_out")
## plot the chains
mcmc_trace(real_size)
## this looks pretty good. All betas converge

## Calculate the probabilities for each state
## Create a fake size variable that spans the range of observed sizes
size_dummy_real <- seq(min((cactus_real$logsize_t)), max((cactus_real$logsize_t)), by=0.1)
                    ##  vac
Denominator <- 1 + exp(mean(real_size_out$beta[,1,1]) + mean(real_size_out$beta[,2,1])*size_dummy_real) + 
                    ## other
  exp(mean(real_size_out$beta[,1,2]) + mean(real_size_out$beta[,2,2])*size_dummy_real) + 
                    ## crem
  exp(mean(real_size_out$beta[,1,3]) + mean(real_size_out$beta[,2,3])*size_dummy_real)

pred_size <- cbind(
  ## Pr(liom)
  1/ Denominator,
  ## Pr(vac)
  (exp(mean(real_size_out$beta[,1,1]) + mean(real_size_out$beta[,2,1])*size_dummy_real))/Denominator,
  ## Pr(other)
  (exp(mean(real_size_out$beta[,1,2]) + mean(real_size_out$beta[,2,2])*size_dummy_real))/Denominator,
  ## Pr(crem)
  (exp(mean(real_size_out$beta[,1,3]) + mean(real_size_out$beta[,2,3])*size_dummy_real))/Denominator
)

## Check if rows of this sum to 1
sum(pred_size[1,])
## all rows sum to 1


###### plot the probabilities
## prob of being occupied by liom
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Size_Only_Multinom.png")
plot(size_dummy_real, pred_size[,1],ylim = c(0,1), type = "l", col = "blue", xlab = "size", ylab = "probability")
## prob of being occupied by vac
lines(size_dummy_real, pred_size[,2], col = "pink")
## prob of being occupied by other
lines(size_dummy_real, pred_size[,3], col = "black")
## prob of being occupied by crem
lines(size_dummy_real, pred_size[,4], col = "red")
legend("topleft", c("vac","crem","liom","other"), fill = c("pink","red","blue","black"))
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
## Check against Frequentist model 
## freq model 
cactus_fit_size <- summary(multinom(ant_t1_relevel ~ logsize_t, cactus_real))
## Calculate the probabilities of each state
                        ## other
Denominator <- 1 + sum(exp(cactus_fit_size$coefficients[1,1] + cactus_fit_size$coefficients[1,2]*size_dummy_real) + 
                         ## crem
                         exp(cactus_fit_size$coefficients[2,1] + cactus_fit_size$coefficients[2,2]*size_dummy_real) + 
                         ## liom
                         exp(cactus_fit_size$coefficients[3,1] + cactus_fit_size$coefficients[3,2]*size_dummy_real)
                         )
pred_freq_size<-cbind(
  #pr(vacant)
  1/(Denominator),
  #pr(other)
  exp(cactus_fit_size$coefficients[1,1] + cactus_fit_size$coefficients[1,2]*size_dummy_real)/(Denominator),
  #pr(crem)
  exp(cactus_fit_size$coefficients[2,1] + cactus_fit_size$coefficients[2,2]*size_dummy_real)/(Denominator),
  #pr(liom)
  exp(cactus_fit_size$coefficients[3,1] + cactus_fit_size$coefficients[3,2]*size_dummy_real)/(Denominator))
sum(pred_freq_size)

## Compare the probabilities of the frequentist model to the bayesian model to the real data
## Real data
table(cactus_real$ant_t1_relevel)/nrow(cactus_real)
## Freq data 
## vac, other, crem, liom
(c(mean(pred_freq_size[,1]), mean(pred_freq_size[,2]), mean(pred_freq_size[,3]), mean(pred_freq_size[,4])))
## Bayes data
## liom, vac, other, crem
(c(mean(pred_size[,1]), mean(pred_size[,2]), mean(pred_size[,3]), mean(pred_size[,4])))
## The freq model is still very close to the real data. The Bayesian model is not as close, but 
## isn't that bad. It seems to underestimate all of them just a bit (except liom)

# #############################################################################################
# ###### now include categroical as the only predictor ########################################
# ###### Data Analysis/STAN Models/multi_prac_size_ant_Km1.stan ###############################
# #############################################################################################
multi_dat_real <- list(K = length(unique(cactus_real$ant_t1_relevel)), #number of possible ant species
                       N = dim(cactus_real)[1], #number of observations
                       D = 4, #number of predictors
                       y = as.integer(as.factor(cactus_real$ant_t1_relevel)), #observations
                       x = model.matrix(~ 0 + (as.factor(ant_t_relevel)), cactus_real)) #design matrix
## Run the model & save the results
real_ant_noint <- stan(file = "Data Analysis/STAN Models/multi_prac_tom_Km1.stan", 
                  data = multi_dat_real, warmup = 100, iter = 1000, chains = 3)
real_ant_out <- rstan::extract(real_ant_noint, pars = c("beta"))
write.csv(real_ant_out, "multi_ant_out")
## plot the chains
mcmc_trace(real_ant_noint)
summary(real_ant_noint)
## this looks pretty good. All betas converge


## Calculate the probabilities for each state
## Previous ant was vacant
##real_ant_out$beta[iteration,previous ant state,next ant state]
## Previously tended by none
Denominator_vac <- exp(real_ant_out$beta[,1,1]) + exp(real_ant_out$beta[,1,2]) + exp(real_ant_out$beta[,1,3]) + exp(real_ant_out$beta[,1,4])
pred_vac<-cbind(
  #pr(vacant)
  exp(real_ant_out$beta[,1,1])/Denominator_vac,
  #pr(other)
  exp(real_ant_out$beta[,1,2])/Denominator_vac,
  #pr(crem)
  exp(real_ant_out$beta[,1,3])/Denominator_vac,
  #pr(liom)
  exp(real_ant_out$beta[,1,4])/Denominator_vac)
sum(pred_vac[1,])
## Previously tended by Other
Denominator_other <- exp(real_ant_out$beta[,2,1]) + exp(real_ant_out$beta[,2,2]) + exp(real_ant_out$beta[,2,3]) + exp(real_ant_out$beta[,2,4])
pred_other<-cbind(
  #pr(vacant)
  exp(real_ant_out$beta[,2,1])/Denominator_other,
  #pr(other)
  exp(real_ant_out$beta[,2,2])/Denominator_other,
  #pr(crem)
  exp(real_ant_out$beta[,2,3])/Denominator_other,
  #pr(liom)
  exp(real_ant_out$beta[,2,4])/Denominator_other)
sum(pred_other[1,])
## Previously tended by Crem
Denominator_crem <- exp(real_ant_out$beta[,3,1]) + exp(real_ant_out$beta[,3,2]) + exp(real_ant_out$beta[,3,3]) + exp(real_ant_out$beta[,3,4])
pred_crem<-cbind(
  #pr(vacant)
  exp(real_ant_out$beta[,3,1])/Denominator_crem,
  #pr(other)
  exp(real_ant_out$beta[,3,2])/Denominator_crem,
  #pr(crem)
  exp(real_ant_out$beta[,3,3])/Denominator_crem,
  #pr(liom)
  exp(real_ant_out$beta[,3,4])/Denominator_crem)
sum(pred_crem[1,])
## Previously tended by Liom
Denominator_liom <- exp(real_ant_out$beta[,4,1]) + exp(real_ant_out$beta[,4,2]) + exp(real_ant_out$beta[,4,3]) + exp(real_ant_out$beta[,4,4])
pred_liom<-cbind(
  #pr(vacant)
  exp(real_ant_out$beta[,4,1])/Denominator_liom,
  #pr(other)
  exp(real_ant_out$beta[,4,2])/Denominator_liom,
  #pr(crem)
  exp(real_ant_out$beta[,4,3])/Denominator_liom,
  #pr(liom)
  exp(real_ant_out$beta[,4,4])/Denominator_liom)
sum(pred_liom[1,])
                      ## vac-> vac        vac -> other    vac -> crem       vac -> liom
pred_probs_vac <- cbind((pred_vac[,1]) , (pred_vac[,2]) , (pred_vac[,3]) , (pred_vac[,4]))
                        ## other-> vac        other -> other    other -> crem       other -> liom
pred_probs_other <- cbind((pred_other[,1]) , (pred_other[,2]) , (pred_other[,3]) , (pred_other[,4]))
                        ## crem-> vac       crem -> other    crem -> crem       crem -> liom
pred_probs_crem <- cbind((pred_crem[,1]) , (pred_crem[,2]) , (pred_crem[,3]) , (pred_crem[,4]))
                        ## liom-> vac       liom -> other    liom -> crem       liom -> liom
pred_probs_liom <- cbind((pred_liom[,1]) , (pred_liom[,2]) , (pred_liom[,3]) , (pred_liom[,4]))

###### plot the probabilities
## Barplot
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
## means
means_vac <- c(mean(pred_probs_vac[,1]),mean(pred_probs_vac[,2]), mean(pred_probs_vac[,3]), mean(pred_probs_vac[,4]))
means_other <-c(mean(pred_probs_other[,1]),mean(pred_probs_other[,2]), mean(pred_probs_other[,3]), mean(pred_probs_other[,4]))
means_crem <- c(mean(pred_probs_crem[,1]),mean(pred_probs_crem[,2]), mean(pred_probs_crem[,3]), mean(pred_probs_crem[,4]))
means_liom <- c(mean(pred_probs_liom[,1]),mean(pred_probs_liom[,2]), mean(pred_probs_liom[,3]), mean(pred_probs_liom[,4]))
## 95%
h_vac <- c(quantile(pred_probs_vac[,1],0.95),quantile(pred_probs_vac[,2],0.95), quantile(pred_probs_vac[,3],0.95), quantile(pred_probs_vac[,4],0.95))
h_other <-c(quantile(pred_probs_other[,1],0.95),quantile(pred_probs_other[,2],0.95), quantile(pred_probs_other[,3],0.95), quantile(pred_probs_other[,4],0.95))
h_crem <- c(quantile(pred_probs_crem[,1],0.95),quantile(pred_probs_crem[,2],0.95), quantile(pred_probs_crem[,3],0.95), quantile(pred_probs_crem[,4],0.95))
h_liom <- c(quantile(pred_probs_liom[,1],0.95),quantile(pred_probs_liom[,2],0.95), quantile(pred_probs_liom[,3],0.95), quantile(pred_probs_liom[,4],0.95))
## 5%
l_vac <- c(quantile(pred_probs_vac[,1],0.05),quantile(pred_probs_vac[,2],0.05), quantile(pred_probs_vac[,3],0.05), quantile(pred_probs_vac[,4],0.05))
l_other <-c(quantile(pred_probs_other[,1],0.05),quantile(pred_probs_other[,2],0.05), quantile(pred_probs_other[,3],0.05), quantile(pred_probs_other[,4],0.05))
l_crem <- c(quantile(pred_probs_crem[,1],0.05),quantile(pred_probs_crem[,2],0.05), quantile(pred_probs_crem[,3],0.05), quantile(pred_probs_crem[,4],0.05))
l_liom <- c(quantile(pred_probs_liom[,1],0.05),quantile(pred_probs_liom[,2],0.05), quantile(pred_probs_liom[,3],0.05), quantile(pred_probs_liom[,4],0.05))

png("Ant_Only_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,2,3,4,5),
              ncol = 2, nrow = 3, byrow = TRUE), heights = c(0.6,1.4,1.4), widths = c(3.9,3.9))
plot.new()
text(0.5,0.5,"Probability of Next Ant Partner By Previous",cex=2,font=2)

barplot(means_vac, col = c("pink","black","red","blue"), main = "Previously Vacant", xlab = "Next Ant State", ylim = c(0,1))
arrows(x0=0.7, h_vac[1], x1=0.7, l_vac[1], angle=90, code=3)
arrows(x0=1.9, h_vac[2], x1=1.9, l_vac[2], angle=90, code=3)
arrows(x0=3.1, h_vac[3], x1=3.1, l_vac[3], angle=90, code=3)
arrows(x0=4.3, h_vac[4], x1=4.3, l_vac[4], angle=90, code=3)
barplot(means_other, col = c("pink","black","red","blue"), main = "Previously Other", xlab = "Next Ant State", ylim = c(0,1))
arrows(x0=0.7, h_other[1], x1=0.7, l_other[1], angle=90, code=3)
arrows(x0=1.9, h_other[2], x1=1.9, l_other[2], angle=90, code=3)
arrows(x0=3.1, h_other[3], x1=3.1, l_other[3], angle=90, code=3)
arrows(x0=4.3, h_other[4], x1=4.3, l_other[4], angle=90, code=3)
legend("topright",c("vacant","other","crem.","liom."), fill = c("pink","black","red","blue"))
barplot(means_crem, col = c("pink","black","red","blue"), main = "Previously Crem", xlab = "Next Ant State", ylim = c(0,1))
arrows(x0=0.7, h_crem[1], x1=0.7, l_crem[1], angle=90, code=3)
arrows(x0=1.9, h_crem[2], x1=1.9, l_crem[2], angle=90, code=3)
arrows(x0=3.1, h_crem[3], x1=3.1, l_crem[3], angle=90, code=3)
arrows(x0=4.3, h_crem[4], x1=4.3, l_crem[4], angle=90, code=3)
barplot(means_liom, col = c("pink","black","red","blue"), main = "Previously Liom", xlab = "Next Ant State", ylim = c(0,1))
arrows(x0=0.7, h_liom[1], x1=0.7, l_liom[1], angle=90, code=3)
arrows(x0=1.9, h_liom[2], x1=1.9, l_liom[2], angle=90, code=3)
arrows(x0=3.1, h_liom[3], x1=3.1, l_liom[3], angle=90, code=3)
arrows(x0=4.3, h_liom[4], x1=4.3, l_liom[4], angle=90, code=3)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

## Check against Frequentist model 
## freq model 
cactus_fit <- summary(multinom(ant_t1_relevel ~ 0 + ant_t_relevel, cactus_real))
cactus_fit_coef <- coef(cactus_fit)

pred_vac_freq<-c(
  #pr(vacant)
  1/(1+sum(exp(cactus_fit_coef[,1]))),
  #pr(other)
  exp(cactus_fit_coef[1,1])/(1+sum(exp(cactus_fit_coef[,1]))),
  #pr(crem)
  exp(cactus_fit_coef[2,1])/(1+sum(exp(cactus_fit_coef[,1]))),
  #pr(liom)
  exp(cactus_fit_coef[3,1])/(1+sum(exp(cactus_fit_coef[,1]))))
sum(pred_vac_freq)

pred_crem_freq<-c(
  #pr(vacant)
  1/(1+sum(exp(cactus_fit_coef[,2]))),
  #pr(other)
  exp(cactus_fit_coef[1,2])/(1+sum(exp(cactus_fit_coef[,2]))),
  #pr(crem)
  exp(cactus_fit_coef[2,2])/(1+sum(exp(cactus_fit_coef[,2]))),
  #pr(liom)
  exp(cactus_fit_coef[3,2])/(1+sum(exp(cactus_fit_coef[,2]))))
sum(pred_crem_freq)

pred_liom_freq<-c(
  #pr(vacant)
  1/(1+sum(exp(cactus_fit_coef[,3]))),
  #pr(other)
  exp(cactus_fit_coef[1,3])/(1+sum(exp(cactus_fit_coef[,3]))),
  #pr(crem)
  exp(cactus_fit_coef[2,3])/(1+sum(exp(cactus_fit_coef[,3]))),
  #pr(liom)
  exp(cactus_fit_coef[3,3])/(1+sum(exp(cactus_fit_coef[,3]))))
sum(pred_liom_freq)

pred_other_freq<-c(
  #pr(vacant)
  1/(1+sum(exp(cactus_fit_coef[,4]))),
  #pr(other)
  exp(cactus_fit_coef[1,4])/(1+sum(exp(cactus_fit_coef[,4]))),
  #pr(crem)
  exp(cactus_fit_coef[2,4])/(1+sum(exp(cactus_fit_coef[,4]))),
  #pr(liom)
  exp(cactus_fit_coef[3,4])/(1+sum(exp(cactus_fit_coef[,4]))))
sum(pred_other_freq)


## Compare the probabilities of the frequentist model to the bayesian model to the real data (total for y)
## Real data -- each row sums to the probability of being in state a in year t1
obstab <- table(cactus_real$ant_t1_relevel,cactus_real$ant_t_relevel)
obstab[,1]/colSums(obstab)[1]
obstab[,2]/colSums(obstab)[2]
obstab[,3]/colSums(obstab)[3]
obstab[,4]/colSums(obstab)[4]
## Freq data 
## vac, other, crem, liom
pred_mat <- cbind(pred_vac_freq,pred_crem_freq,pred_liom_freq,pred_other_freq)
colSums(pred_mat)
## Bayes data
colSums(pred_probs_vac)
colSums(pred_probs_other)
colSums(pred_probs_crem)
colSums(pred_probs_liom)

##The Bayesian model is also very close!! Looks good


      
# #############################################################################################
# ###### now include ant and size as predictors  ##############################################
# ###### Data Analysis/STAN Models/multi_prac_size_ant_Km1.stan ###############################
# #############################################################################################
multi_dat_real <- list(K = length(unique(cactus_real$ant_t1_relevel)), #number of possible ant species
                       N = dim(cactus_real)[1], #number of observations
                       D = 5, #number of predictors
                       y = as.integer(as.factor(cactus_real$ant_t1_relevel)), #observations
                       x = model.matrix(~ 0 + (as.factor(ant_t_relevel)) + logsize_t + as.factor(Year_t) + Plot, cactus_real)) #design matrix
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


## Look into the frequentist model
cactus_fit <- summary(multinom(ant_t1_relevel ~ 0 + ant_t_relevel + logsize_t, cactus_real))
cactus_fit_coef <- coef(cactus_fit)

Denominator_freq_vac <- 1+sum(exp(cactus_fit_coef[1,1] + size_dummy_real*cactus_fit_coef[1,5]), 
                               exp(cactus_fit_coef[2,1] + size_dummy_real*cactus_fit_coef[2,5]),
                               exp(cactus_fit_coef[3,1] + size_dummy_real*cactus_fit_coef[3,5]))
pred_vac_freq<-c(
  #pr(vacant)
  1/Denominator_freq_vac,
  #pr(other)
  exp(cactus_fit_coef[1,1] + size_dummy_real*cactus_fit_coef[1,5])/Denominator_freq_vac,
  #pr(crem)
  exp(cactus_fit_coef[2,1] + size_dummy_real*cactus_fit_coef[2,5])/Denominator_freq_vac,
  #pr(liom)
  exp(cactus_fit_coef[3,1] + size_dummy_real*cactus_fit_coef[3,5])/Denominator_freq_vac
  )
sum(pred_vac_freq)

Denominator_freq_crem <- 1+sum(exp(cactus_fit_coef[1,2] + size_dummy_real*cactus_fit_coef[1,5]), 
                              exp(cactus_fit_coef[2,2] + size_dummy_real*cactus_fit_coef[2,5]),
                              exp(cactus_fit_coef[3,2] + size_dummy_real*cactus_fit_coef[3,5]))
pred_crem_freq<-c(
  #pr(vacant)
  1/Denominator_freq_crem,
  #pr(other)
  exp(cactus_fit_coef[1,2] + size_dummy_real*cactus_fit_coef[1,5])/Denominator_freq_crem,
  #pr(crem)
  exp(cactus_fit_coef[2,2] + size_dummy_real*cactus_fit_coef[2,5])/Denominator_freq_crem,
  #pr(liom)
  exp(cactus_fit_coef[3,2] + size_dummy_real*cactus_fit_coef[3,5])/Denominator_freq_crem
)
sum(pred_crem_freq)

Denominator_freq_liom <- 1+sum(exp(cactus_fit_coef[1,3] + size_dummy_real*cactus_fit_coef[1,5]), 
                               exp(cactus_fit_coef[2,3] + size_dummy_real*cactus_fit_coef[2,5]),
                               exp(cactus_fit_coef[3,3] + size_dummy_real*cactus_fit_coef[3,5]))
pred_liom_freq<-c(
  #pr(vacant)
  1/Denominator_freq_liom,
  #pr(other)
  exp(cactus_fit_coef[1,3] + size_dummy_real*cactus_fit_coef[1,5])/Denominator_freq_liom,
  #pr(crem)
  exp(cactus_fit_coef[2,3] + size_dummy_real*cactus_fit_coef[2,5])/Denominator_freq_liom,
  #pr(liom)
  exp(cactus_fit_coef[3,3] + size_dummy_real*cactus_fit_coef[3,5])/Denominator_freq_liom
)
sum(pred_liom_freq)

Denominator_freq_other <- 1+sum(exp(cactus_fit_coef[1,4] + size_dummy_real*cactus_fit_coef[1,5]), 
                               exp(cactus_fit_coef[2,4] + size_dummy_real*cactus_fit_coef[2,5]),
                               exp(cactus_fit_coef[3,4] + size_dummy_real*cactus_fit_coef[3,5]))
pred_other_freq<-c(
  #pr(vacant)
  1/Denominator_freq_other,
  #pr(other)
  exp(cactus_fit_coef[1,4] + size_dummy_real*cactus_fit_coef[1,5])/Denominator_freq_other,
  #pr(crem)
  exp(cactus_fit_coef[2,4] + size_dummy_real*cactus_fit_coef[2,5])/Denominator_freq_other,
  #pr(liom)
  exp(cactus_fit_coef[3,4] + size_dummy_real*cactus_fit_coef[3,5])/Denominator_freq_other
)
sum(pred_other_freq)

## Compare outputs to real data and frequentist poutputs
## Real data

## Freq data 
## vac, other, crem, liom
pred_mat <- cbind(mean(pred_vac_freq),mean(pred_crem_freq),mean(pred_liom_freq),mean(pred_other_freq))
colSums(pred_mat)




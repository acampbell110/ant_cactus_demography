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
cactus_real <- cactus[,c("ant_t","ant_t1","logsize_t")]
cactus_real <- na.omit(cactus_real)
cactus_real$ant_t1_relevel <- relevel(cactus_real$ant_t1,ref = "vacant")
cactus_real$ant_t_relevel <- relevel(cactus_real$ant_t, ref = "vacant")
cactus_real <- cactus_real[,c("ant_t_relevel","ant_t1_relevel","logsize_t")]

#### fit model of the mean using the nnet function
table(cactus_real$ant_t1)/nrow(cactus_real)
cactus_fit <- summary(multinom(ant_t1_relevel ~ 1, cactus_real))
pred_freq<-c(
#pr(vacant)
1/(1+sum(exp(cactus_fit$coefficients))),
#pr(other)
exp(cactus_fit$coefficients[1])/(1+sum(exp(cactus_fit$coefficients))),
#pr(crem)
exp(cactus_fit$coefficients[2])/(1+sum(exp(cactus_fit$coefficients))),
#pr(liom)
exp(cactus_fit$coefficients[3])/(1+sum(exp(cactus_fit$coefficients))))
sum(pred_freq)

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
k_fit_stan_real <- stan(file = "Data Analysis/STAN Models/multi_prac_tom_Km1.stan", 
                        data = multi_dat_real, warmup = 500, iter = 4000, chains = 3)
k_mod_real_out <- rstan::extract(k_fit_stan_real, pars = "beta")
## plot the chains
mcmc_trace(k_fit_stan_real)
                  ## other                        ## crem                         ##liom
postmean_beta <- c(mean(k_mod_real_out$beta[,,1]),mean(k_mod_real_out$beta[,,2]),mean(k_mod_real_out$beta[,,3]))
stan_pred_freq<-c(
  #pr(vacant)
  1/(1+sum(exp(postmean_beta))),
  #pr(other)
  exp(postmean_beta[1])/(1+sum(exp(postmean_beta))),
  #pr(crem)
  exp(postmean_beta[2])/(1+sum(exp(postmean_beta))),
  #pr(liom)
  exp(postmean_beta[3])/(1+sum(exp(postmean_beta))))
sum(pred_freq)
# 
# ################################################################################################
# ###### Now Stan model -- size only ############################################################# 
# ###### Data Analysis/STAN Models/multi_prac_tom_K.stan #########################################
# ################################################################################################
# multi_dat <- list(K = length(unique(dfM$y)), # number of possible outcomes
#                   N = (dim(dfM)[1]), # number of observations
#                   D = 1, # number of predictors
#                   y = dfM$y, # observations
#                   x = as.matrix(dfM$x)) # design matrix
# ###### Run the model with simulated data & save the results
# k_fit_stan <- stan(file = "Data Analysis/STAN Models/multi_prac_tom_K.stan", 
#                data = multi_dat, warmup = 100, iter = 1000, chains = 3)
# k_mod_sim_out <- rstan::extract(k_fit_stan, pars = "beta")
# ## plot the chains
# mcmc_trace(k_fit_stan)
# 
# 
# multi_dat_real <- list(K = length(unique(cactus_real$ant_t1)), #number of possible ant species
#                        N = dim(cactus_real)[1], #number of observations
#                        D = 1, #number of predictors
#                        y = as.integer(as.factor(cactus_real$ant_t1)), #observations
#                        x = as.matrix(cactus_real$volume_t) #design matrix
#                        )
# ###### Run the model with real data & save the results
# k_fit_stan_real <- stan(file = "Data Analysis/STAN Models/multi_prac_tom_K.stan", 
#                  data = multi_dat_real, warmup = 5000, iter = 10000, chains = 3)
# k_mod_real_out <- rstan::extract(k_fit_stan_real, pars = "beta")
# ## plot the chains
# mcmc_trace(k_fit_stan_real)
# 
# 

############################################################################################
###### now try K-1 version -- size only ####################################################
###### Data Analysis/STAN Models/multi_prac_tom_Km1.stan" ##################################
############################################################################################
###### run the model with simulated data & save the results
multi_dat_real <- list(K = length(unique(cactus_real$ant_t1_relevel)), #number of possible ant species
                       N = dim(cactus_real)[1], #number of observations
                       D = 2, #number of predictors
                       y = as.integer(as.factor(cactus_real$ant_t1_relevel)), #observations
                       x = model.matrix(~ logsize_t, cactus_real)) #design matrix

km1_fit_stan <- stan(file = "Data Analysis/STAN Models/multi_prac_tom_Km1.stan", 
                 data = multi_dat_real, warmup = 100, iter = 1000, chains = 3)
km1_mod_real_out <- rstan::extract(km1_fit_stan, pars = c("beta"))
write.csv(km1_mod_real_out, "km1_mod_real_outputs.csv")
## plot the chains
mcmc_trace(km1_fit_stan)
## this looks pretty good. All betas converge

## create a fake size variable that spans the range of observed sizes
## Ali's path
km1_real <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/km1_mod_real_outputs.csv", header = TRUE,stringsAsFactors=T)
##Tom's path

km1_real <- read.csv("C:/Users/tm9/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/km1_mod_real_outputs.csv", header = TRUE,stringsAsFactors=T)
size_dummy_real <- seq(min((cactus_real$logsize_t)), max((cactus_real$logsize_t)), by=0.1)
Denominator <- 1 + exp(mean(km1_real$beta.1.1) + mean(km1_real$beta.2.1)*size_dummy_real) + 
  exp(mean(km1_real$beta.1.2) + mean(km1_real$beta.2.2)*size_dummy_real) + 
  exp(mean(km1_real$beta.1.3) + mean(km1_real$beta.2.3)*size_dummy_real)
stan_pred_freq_real <- cbind(
  1/ Denominator,
  (exp(mean(km1_mod_real_out$beta[,1,1]) + mean(km1_mod_real_out$beta[,2,1])*size_dummy_real))/Denominator,
  (exp(mean(km1_mod_real_out$beta[,1,2]) + mean(km1_mod_real_out$beta[,2,2])*size_dummy_real))/Denominator,
  (exp(mean(km1_mod_real_out$beta[,1,3]) + mean(km1_mod_real_out$beta[,2,3])*size_dummy_real))/Denominator
)

mean(km1_real$beta.1.2)
## Check if rows of this sum to 1
sum(stan_pred_freq_real[1,])
## all rows sum to 1

stan_pred_freq_real<-matrix(4,length(size_dummy_real))
for(i in 1:length(size_dummy_real)){
  Denominator <- 1 + exp(mean(km1_real$beta.1.1) + mean(km1_real$beta.2.1)*size_dummy_real[i]) + 
    exp(mean(km1_real$beta.1.2) + mean(km1_real$beta.2.2)*size_dummy_real[i]) + 
    exp(mean(km1_real$beta.1.3) + mean(km1_real$beta.2.3)*size_dummy_real[i])
  stan_pred_freq_real[,i] <- cbind(
    1/ Denominator,
    (exp(mean(km1_real$beta.1.1) + mean(km1_real$beta.2.1)*size_dummy_real[i]))/Denominator,
    (exp(mean(km1_real$beta.1.2) + mean(km1_real$beta.2.2)*size_dummy_real[i]))/Denominator,
    (exp(mean(km1_real$beta.1.3) + mean(km1_real$beta.2.3)*size_dummy_real[i]))/Denominator
  )
}


###### plot the probabilities
levels(cactus_real$ant_t1)
## prob of being occupied by vacant
plot(size_dummy_real, stan_pred_freq_real[,1],ylim = c(0,1), type = "l", col = "black", xlab = "size", ylab = "probability")
## prob of being occupied by other
lines(size_dummy_real, stan_pred_freq_real[,2], col = "red")
## prob of being occupied by crem
lines(size_dummy_real, stan_pred_freq_real[,3], col = "blue")
## prob of being occupied by liom
lines(size_dummy_real, stan_pred_freq_real[,4], col = "pink")
legend("topleft", c("other","crem","liom","vac"), fill = c("black","red","blue","pink"))

## Check against real data


# #############################################################################################
# ###### now add categroical ##################################################################
# ###### Data Analysis/STAN Models/multi_prac_size_ant_Km1.stan ###############################
# #############################################################################################
# ## create the categorical data & update the simulated data
# x_cat = matrix(0, nrow = 1000, ncol = 3) ## Categorical variable with 3 choices
# a <- vector()
# j <- vector()
# for(i in 1:1000){
#   a[i] <- rbinom(1,1,0.5)
#   x_cat[i,1] <- a[i]
#   if(x_cat[i,1] == 0) {
#     j[i] <- rbinom(1,1,0.5)
#     if(j[i] == 1){x_cat[i,2] <- 1}
#     if(j[i] == 0){x_cat[i,3] <- 1}
#   }
# }
# x_t <- apply(x_cat, 1, function(x_cat) which(x_cat==1))
# ## Create the y of the dataset (Ant_t1)
# alpha <- c(-1,2)
# beta <- c(0,0,1,2,1,2,2,0)
# Denominator= 1+exp(alpha[1] + beta[1]*x + beta[3]*x_cat[,1] + beta[5]*x_cat[,2] + beta[7]*x_cat[,3])+
#   exp(alpha[2] + beta[2]*x + beta[4]*x_cat[1] + beta[6]*x_cat[2] + beta[8]*x_cat[3])
# #Calculating the matrix of probabilities for three choices
# vProb = cbind(1/Denominator, exp(alpha[1] + beta[1]*x + beta[3]*x_cat[,1] + beta[5]*x_cat[,2] + beta[7]*x_cat[,3])/Denominator, 
#               exp(alpha[2] + beta[2]*x + beta[4]*x_cat[,1] + beta[6]*x_cat[,2] + beta[8]*x_cat[,3])/Denominator )
# # Assigning the value one to maximum probability and zero for rest to get the appropriate choices for value of x
# mChoices = t(apply(vProb, 1, rmultinom, n = 1, size = 1))
# # Value of Y and X together
# dfM = cbind.data.frame(y = apply(mChoices, 1, function(x) which(x==1)), x,x_t)
# summary(multinom(y ~ x+x_t, dfM))
# 
# multi_dat <- list(K = length(unique(dfM$y)), # number of possible outcomes
#                   N = (dim(dfM)[1]), # number of observations
#                   D = (dim(dfM)[2]), # number of predictors
#                   y = dfM$y, # observations
#                   x = model.matrix(~ x+x_t, dfM)) # design matrix
# 
# 
# ###### run the model with simulated data & save results
# km1_full_fit_stan <- stan(file = "Data Analysis/STAN Models/multi_prac_size_ant_Km1.stan", 
#                  data = multi_dat, warmup = 500, iter = 1000, chains = 3)
# km1_full_mod_sim_out <- rstan::extract(km1_full_fit_stan, pars = "beta")
# write.csv(km1_full_mod_sim_out, "size_data_sim.csv")
# ## plot chains
# mcmc_trace(km1_full_fit_stan)
# 
# ###### Import data from simulated run model
# full_data_sim <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/size_data_sim.csv", header = TRUE,stringsAsFactors=T)
# size_dummy_sim <- seq(min(dfM$x), max(dfM$x), by=0.1)
# 
# ###### try 1
# Denominator <- 1*size_dummy_sim + exp(quantile(full_data_sim$beta.1.1,0.5)*size_dummy_sim) + exp(quantile(full_data_sim$beta.1.2,0.5)*size_dummy_sim) + exp(quantile(full_data_sim$beta.1.3,0.5)*size_dummy_sim) + ## prob of choice 1
#   exp(quantile(full_data_sim$beta.2.1,0.5)*size_dummy_sim) + exp(quantile(full_data_sim$beta.2.2,0.5)*size_dummy_sim) + exp(quantile(full_data_sim$beta.2.3,0.5)*size_dummy_sim)
# km1_full_vProb <- cbind(1*size_dummy_sim/Denominator, (exp(quantile(full_data_sim$beta.1.1,0.5)*size_dummy_sim) + exp(quantile(full_data_sim$beta.1.2,0.5)*size_dummy_sim) + exp(quantile(full_data_sim$beta.1.3,0.5)*size_dummy_sim))/Denominator,
#                         (exp(quantile(full_data_sim$beta.2.1,0.5)*size_dummy_sim) + exp(quantile(full_data_sim$beta.2.2,0.5)*size_dummy_sim) + exp(quantile(full_data_sim$beta.2.3,0.5)*size_dummy_sim))/Denominator)
# ## Check if the rows sum to 1
# check <- vector()
# for(i in 1:61){
#   check[i] <- sum(km1_full_vProb[i,])
# }
# check ## This works
# 
# ###### plot the probabilities -- This is where it falls apart. There are negative probs and probs greater than 1
# plot(size_dummy_sim, km1_full_vProb[,1], type = "l", col = "red",ylim = c(-1,1))
# lines(size_dummy_sim, km1_full_vProb[,2], col = "blue")
# lines(size_dummy_sim, km1_full_vProb[,3], col = "green")
# legend("bottomright", c("option 1","option 2","option 3"), fill = c("blue","green","red"))
# 
# ###### Try 2
# Denominator <- 1*size_dummy_sim + ## baseline option
#                         exp(quantile(full_data_sim$beta.1.1,0.5)*size_dummy_sim + quantile(full_data_sim$beta.1.2,0.5)*size_dummy_sim + quantile(full_data_sim$beta.1.3,0.5)*size_dummy_sim) + ## prob of choice 1
#                         exp(quantile(full_data_sim$beta.2.1,0.5)*size_dummy_sim + quantile(full_data_sim$beta.2.2,0.5)*size_dummy_sim + quantile(full_data_sim$beta.2.3,0.5)*size_dummy_sim) ## prob of choice 2
# km1_full_vProb <- cbind((1*size_dummy_sim)/Denominator,  ## baseline prob
#                         exp(quantile(full_data_sim$beta.1.1,0.5)*size_dummy_sim + quantile(full_data_sim$beta.1.2,0.5)*size_dummy_sim + quantile(full_data_sim$beta.1.3,0.5)*size_dummy_sim)/Denominator, ## choice 1 prob
#                         exp(quantile(full_data_sim$beta.2.1,0.5)*size_dummy_sim + quantile(full_data_sim$beta.2.2,0.5)*size_dummy_sim + quantile(full_data_sim$beta.2.3,0.5)*size_dummy_sim)/Denominator) ## choice 2 prob
# ## check if the rows sum to 1
# check <- vector()
# for(i in 1:61){
#   check[i] <- sum(km1_full_vProb[i,])
# }
# check ## This works!!
# 
# ###### plot the probabilities -- something is going wrong with the blyes and reds
# plot(size_dummy_sim, km1_full_vProb[,1], type = "l", col = "red")
# lines(size_dummy_sim, km1_full_vProb[,2], col = "blue")
# plot(size_dummy_sim, km1_full_vProb[,3], col = "green")
# 
# ###### Try 3
# Denominator <- exp(quantile(full_data_sim$beta.1.1,0.5)*size_dummy_sim + quantile(full_data_sim$beta.2.1,0.5)*size_dummy_sim) + ## prob of choice 1
#   exp(quantile(full_data_sim$beta.1.2,0.5)*size_dummy_sim + quantile(full_data_sim$beta.2.2,0.5)*size_dummy_sim) + ## prob of choice 2
#   exp(quantile(full_data_sim$beta.1.3,0.5)*size_dummy_sim + quantile(full_data_sim$beta.2.3,0.5)*size_dummy_sim) ## prob of choice 3
# km1_full_vProb <- cbind(exp(quantile(full_data_sim$beta.1.1,0.5)*size_dummy_sim + quantile(full_data_sim$beta.2.1,0.5)*size_dummy_sim)/Denominator, ## prob of choice 1
#                         exp(quantile(full_data_sim$beta.1.2,0.5)*size_dummy_sim + quantile(full_data_sim$beta.2.2,0.5)*size_dummy_sim)/Denominator, ## prob of choice 2
#                         exp(quantile(full_data_sim$beta.1.3,0.5)*size_dummy_sim + quantile(full_data_sim$beta.2.3,0.5)*size_dummy_sim)/Denominator) ## prob of choice 3
# ## check if the rows sum to 1
# check <- vector()
# for(i in 1:61){
#   check[i] <- sum(km1_full_vProb[i,])
# }
# check ## This works!!
# 
# ##### plot the probabilities -- something is going wrong with the blyes and reds
# plot(size_dummy_sim, km1_full_vProb[,1], type = "l", col = "red", ylim = c(0,1))
# lines(size_dummy_sim, km1_full_vProb[,2], col = "blue")
# lines(size_dummy_sim, km1_full_vProb[,3], col = "green")
# legend("topright", c("option 1","option 2","option 3"), fill = c("red","blue","green"))


######################################################################################################
######         km1 model -- categorical variable added                  ##############################
######         
multi_dat_real <- list(K = length(unique(cactus_real$ant_t1_relevel)), #number of possible ant species
                       N = dim(cactus_real)[1], #number of observations
                       D = dim(cactus_real)[2], #number of predictors -- prev ant state, prev size
                       y = as.integer(as.factor(cactus_real$ant_t1_relevel)), #observations
                       x = model.matrix(~ logsize_t+as.integer(as.factor(ant_t_relevel)), cactus_real) #design matrix
)
#### run the model with real data & save results
km1_full_fit_real <- stan(file = "Data Analysis/STAN Models/multi_prac_size_ant_Km1.stan", 
                          data = multi_dat_real, warmup = 500, iter = 1000, chains = 3)
km1_full_mod_real_out <- rstan::extract(km1_full_fit_real, pars = "beta")
write.csv(km1_full_mod_real_out, "full_data_real.csv")
## plot the chains
mcmc_trace(km1_full_fit_real)

###### Import data from simulated run model
full_data_real <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/full_data_real.csv", header = TRUE,stringsAsFactors=T)
size_dummy_real <- seq(min((cactus_real$logsize_t)), max((cactus_real$logsize_t)), by=0.1)
## make a denominator
Denominator <- 1 + exp(mean(full_data_real$beta.1.1)*size_dummy_real + mean(full_data_real$beta.2.1)*size_dummy_real + mean(full_data_real$beta.3.1)*size_dummy_real) + 
  exp(mean(full_data_real$beta.1.2)*size_dummy_real + mean(full_data_real$beta.2.2)*size_dummy_real + mean(full_data_real$beta.3.2)*size_dummy_real) + 
  exp(mean(full_data_real$beta.1.3)*size_dummy_real + mean(full_data_real$beta.2.3)*size_dummy_real + mean(full_data_real$beta.3.3)*size_dummy_real) 
## 
stan_pred_freq_real <- cbind(
  1/ Denominator,
  (exp(mean(km1_mod_real_out$beta[,1,1]) + mean(km1_mod_real_out$beta[,2,1])*size_dummy_real))/Denominator,
  (exp(mean(km1_mod_real_out$beta[,1,2]) + mean(km1_mod_real_out$beta[,2,2])*size_dummy_real))/Denominator,
  (exp(mean(km1_mod_real_out$beta[,1,3]) + mean(km1_mod_real_out$beta[,2,3])*size_dummy_real))/Denominator
)
## Check if rows of this sum to 1
sum(stan_pred_freq_real[1,])
## all rows sum to 1



Denominator <- exp(quantile(full_data_real$beta.1.1,0.5)*size_dummy_real + quantile(full_data_real$beta.2.1,0.5)*size_dummy_real + quantile(full_data_real$beta.3.1,0.5)*size_dummy_real) + ## prob of choice 1
  exp(quantile(full_data_real$beta.1.2,0.5)*size_dummy_real + quantile(full_data_real$beta.2.2,0.5)*size_dummy_real + quantile(full_data_real$beta.3.2,0.5)*size_dummy_real) + ## prob of choice 2
  exp(quantile(full_data_real$beta.1.3,0.5)*size_dummy_real + quantile(full_data_real$beta.2.3,0.5)*size_dummy_real + quantile(full_data_real$beta.3.3,0.5)*size_dummy_real) + ## prob of choice 3
  exp(quantile(full_data_real$beta.1.4,0.5)*size_dummy_real + quantile(full_data_real$beta.2.4,0.5)*size_dummy_real + quantile(full_data_real$beta.3.4,0.5)*size_dummy_real)   ## prob of choice 4
km1_full_vProb <- cbind(exp(quantile(full_data_real$beta.1.1,0.5)*size_dummy_real + quantile(full_data_real$beta.2.1,0.5)*size_dummy_real + quantile(full_data_real$beta.3.1,0.5)*size_dummy_real)/Denominator, ## prob of choice 1
                        exp(quantile(full_data_real$beta.1.2,0.5)*size_dummy_real + quantile(full_data_real$beta.2.2,0.5)*size_dummy_real + quantile(full_data_real$beta.3.2,0.5)*size_dummy_real)/Denominator, ## prob of choice 2
                        exp(quantile(full_data_real$beta.1.3,0.5)*size_dummy_real + quantile(full_data_real$beta.2.3,0.5)*size_dummy_real + quantile(full_data_real$beta.3.3,0.5)*size_dummy_real)/Denominator, ## prob of choice 3
                        exp(quantile(full_data_real$beta.1.4,0.5)*size_dummy_real + quantile(full_data_real$beta.2.4,0.5)*size_dummy_real + quantile(full_data_real$beta.3.4,0.5)*size_dummy_real)/Denominator) ## prob of choice 4
## check if the rows sum to 1
check <- vector()
for(i in 1:length(size_dummy_real)){
  check[i] <- sum(km1_full_vProb[i,])
}
check ## This works!!

##### plot the probabilities -- something is going wrong with the blyes and reds
## prob of being occupied by other
plot(size_dummy_real, km1_full_vProb[,1], type = "l", col = "black", ylim = c(0,1))
## prob of being occupied by crem
lines(size_dummy_real, km1_full_vProb[,2], col = "red")
## prob of being occupied by liom
lines(size_dummy_real, km1_full_vProb[,3], col = "blue")
## prob of being occupied by no one
lines(size_dummy_real, km1_full_vProb[,4], col = "pink")
legend("topright", c("other","crem","liom","vac"), fill = c("black","red","blue","pink"))




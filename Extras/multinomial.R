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
cactus_real <- cactus_real[,c("ant_t_relevel","ant_t1_relevel","logsize_t", "ant_t", "ant_t1")]

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
                          ## other                        ## crem                         ##liom
postmean_beta_null <- c(mean(real_null_out$beta[,,1]),mean(real_null_out$beta[,,2]),mean(real_null_out$beta[,,3]))
pred_null<-c(
  #pr(vacant)
  1/(1+sum(exp(postmean_beta_null))),
  #pr(other)
  exp(postmean_beta_null[1])/(1+sum(exp(postmean_beta_null))),
  #pr(crem)
  exp(postmean_beta_null[2])/(1+sum(exp(postmean_beta_null))),
  #pr(liom)
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
plot(size_dummy_real, pred_size[,1],ylim = c(0,1), type = "l", col = "blue", xlab = "size", ylab = "probability")
## prob of being occupied by vac
lines(size_dummy_real, pred_size[,2], col = "black")
## prob of being occupied by other
lines(size_dummy_real, pred_size[,3], col = "pink")
## prob of being occupied by crem
lines(size_dummy_real, pred_size[,4], col = "red")
legend("topleft", c("vac","crem","liom","other"), fill = c("black","red","blue","pink"))

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
multi_dat_real <- list(K = length(unique(cactus_real$ant_t1_relevel)), #number of possible ant species
                       N = dim(cactus_real)[1], #number of observations
                       D = 4, #number of predictors
                       y = as.integer(as.factor(cactus_real$ant_t1_relevel)), #observations
                       x = model.matrix(~ (as.factor(ant_t_relevel)), cactus_real)) #design matrix
real_ant_int <- stan(file = "Data Analysis/STAN Models/multi_prac_tom_Km1.stan", 
                       data = multi_dat_real, warmup = 100, iter = 1000, chains = 3)
real_ant_out <- rstan::extract(real_ant_int, pars = c("beta"))
write.csv(real_ant_out, "multi_ant_out")
## plot the chains
mcmc_trace(real_ant)
summary(real_ant_int)

## Calculate the probabilities for each state
#### Try 1 -- 
                      ##  prob of being occupied by liom (prev order: vac, other, crem, liom)
Denominator_liom  <- 1 + exp(real_ant_out$beta[,1,1] + real_ant_out$beta[,1,2] + real_ant_out$beta[,1,3])
    ## prob of being occupied by vac
Denominator_vac  <- 1 +   exp(real_ant_out$beta[,2,1] + real_ant_out$beta[,2,2] + real_ant_out$beta[,2,3])
    ## other
Denominator_other  <- 1 +  exp(real_ant_out$beta[,3,1] + real_ant_out$beta[,3,2] + real_ant_out$beta[,3,3])
    ## crem
Denominator_crem  <- 1 +  exp(real_ant_out$beta[,4,1] + real_ant_out$beta[,4,2] + real_ant_out$beta[,4,3])
pred_ant <- cbind(
  ## Liom
  1/Denominator_liom,
  exp(real_ant_out$beta[,1,1])/Denominator_liom,
  exp(real_ant_out$beta[,1,2])/Denominator_liom,
  exp(real_ant_out$beta[,1,3])/Denominator_liom,
  ## Vac
  1/Denominator_vac,
  exp(real_ant_out$beta[,2,1])/Denominator_vac,
  exp(real_ant_out$beta[,2,2])/Denominator_vac,
  exp(real_ant_out$beta[,2,3])/Denominator_vac,
  ## Other
  1/Denominator_other,
  exp(real_ant_out$beta[,3,1])/Denominator_other,
  exp(real_ant_out$beta[,3,2])/Denominator_other,
  exp(real_ant_out$beta[,3,3])/Denominator_other,
  ## Crem
  1/Denominator_crem,
  exp(real_ant_out$beta[,4,1])/Denominator_crem,
  exp(real_ant_out$beta[,4,2])/Denominator_crem,
  exp(real_ant_out$beta[,4,3])/Denominator_crem
)

pred_probs <- c(mean(pred_ant[,1]), ## vac-> liom, 
                mean(pred_ant[,2]), ## other -> liom,
                mean(pred_ant[,3]), ## crem -> liom,
                mean(pred_ant[,4]), ## liom -> liom,
                mean(pred_ant[,5]), ## vac-> vac, 
                mean(pred_ant[,6]), ## other -> vac,
                mean(pred_ant[,7]), ## crem -> vac,
                mean(pred_ant[,8]), ## liom -> vac,
                mean(pred_ant[,9]), ## vac-> other, 
                mean(pred_ant[,10]), ## other -> other,
                mean(pred_ant[,11]), ## crem -> other,
                mean(pred_ant[,12]), ## liom -> other,
                mean(pred_ant[,13]), ## vac-> crem, 
                mean(pred_ant[,14]), ## other -> crem,
                mean(pred_ant[,15]), ## crem -> crem,
                mean(pred_ant[,16]) ## liom -> crem,
                )
sum(pred_probs)


### #Try 2
## Probability o fbeing occupied by ____
Denominator  <- 1 + exp(real_ant_out$beta[,1,1] + real_ant_out$beta[,2,1] + real_ant_out$beta[,3,1] + real_ant_out$beta[,4,1]) + 
  exp(real_ant_out$beta[,1,2] + real_ant_out$beta[,2,2] + real_ant_out$beta[,3,2] + real_ant_out$beta[,4,2]) + 
  exp(real_ant_out$beta[,1,3] + real_ant_out$beta[,2,3] + real_ant_out$beta[,3,3] + real_ant_out$beta[,4,3]) 
pred_ant <- cbind(
  ## Liom
  4/Denominator,
  exp(real_ant_out$beta[,1,1] + real_ant_out$beta[,2,1] + real_ant_out$beta[,3,1] + real_ant_out$beta[,4,1])/Denominator,
  exp(real_ant_out$beta[,1,2] + real_ant_out$beta[,2,2] + real_ant_out$beta[,3,2] + real_ant_out$beta[,4,2]) /Denominator,
  exp(real_ant_out$beta[,1,3] + real_ant_out$beta[,2,3] + real_ant_out$beta[,3,3] + real_ant_out$beta[,4,3])/Denominator
)

pred_probs <- c(mean(pred_ant[,1]), ## vac-> liom, 
                mean(pred_ant[,2]), ## other -> liom,
                mean(pred_ant[,3]), ## crem -> liom,
                mean(pred_ant[,4])## liom -> crem,
)
sum(pred_probs)
## all rows sum to 1


#### Try 3 -- 
##  prob of being occupied by liom (prev order: vac, other, crem, liom)
Denominator  <- 1 + exp(real_ant_out$beta[,1,1] + real_ant_out$beta[,1,2] + real_ant_out$beta[,1,3]) + 
## prob of being occupied by vac
  1 +   exp(real_ant_out$beta[,2,1] + real_ant_out$beta[,2,2] + real_ant_out$beta[,2,3]) + 
## other
  1 +  exp(real_ant_out$beta[,3,1] + real_ant_out$beta[,3,2] + real_ant_out$beta[,3,3]) + 
## crem
  1 +  exp(real_ant_out$beta[,4,1] + real_ant_out$beta[,4,2] + real_ant_out$beta[,4,3])
pred_ant <- cbind(
  ## Liom
  1/Denominator,
  exp(real_ant_out$beta[,1,1])/Denominator,
  exp(real_ant_out$beta[,1,2])/Denominator,
  exp(real_ant_out$beta[,1,3])/Denominator,
  ## Vac
  1/Denominator,
  exp(real_ant_out$beta[,2,1])/Denominator,
  exp(real_ant_out$beta[,2,2])/Denominator,
  exp(real_ant_out$beta[,2,3])/Denominator,
  ## Other
  1/Denominator,
  exp(real_ant_out$beta[,3,1])/Denominator,
  exp(real_ant_out$beta[,3,2])/Denominator,
  exp(real_ant_out$beta[,3,3])/Denominator,
  ## Crem
  1/Denominator,
  exp(real_ant_out$beta[,4,1])/Denominator,
  exp(real_ant_out$beta[,4,2])/Denominator,
  exp(real_ant_out$beta[,4,3])/Denominator
)

pred_probs <- c(mean(pred_ant[,1]), ## vac-> liom, 
                mean(pred_ant[,2]), ## other -> liom,
                mean(pred_ant[,3]), ## crem -> liom,
                mean(pred_ant[,4]), ## liom -> liom,
                mean(pred_ant[,5]), ## vac-> vac, 
                mean(pred_ant[,6]), ## other -> vac,
                mean(pred_ant[,7]), ## crem -> vac,
                mean(pred_ant[,8]), ## liom -> vac,
                mean(pred_ant[,9]), ## vac-> other, 
                mean(pred_ant[,10]), ## other -> other,
                mean(pred_ant[,11]), ## crem -> other,
                mean(pred_ant[,12]), ## liom -> other,
                mean(pred_ant[,13]), ## vac-> crem, 
                mean(pred_ant[,14]), ## other -> crem,
                mean(pred_ant[,15]), ## crem -> crem,
                mean(pred_ant[,16]) ## liom -> crem,
)
sum(pred_probs)




## Try 4 -- 
summary(real_ant_int)
real_ant_out <- rstan::extract(real_ant_int, pars = c("beta"))
                ## vac - > liom                       other - > liom            crem - > liom                 liom - > liom
Denominator <- exp(real_ant_out$beta[,1,1]) + exp(real_ant_out$beta[,1,2]) + exp(real_ant_out$beta[,1,3]) + exp(real_ant_out$beta[,1,4]) + 
  ## vac - > vac                        other -> vac                crem - > vac                        liom - > vac
  exp(real_ant_out$beta[,2,1]) + exp(real_ant_out$beta[,2,2]) + exp(real_ant_out$beta[,2,3]) + exp(real_ant_out$beta[,2,4]) + 
  ## vac - > other                      other -> other              crem -> other                       liom -> other
  exp(real_ant_out$beta[,3,1]) + exp(real_ant_out$beta[,3,2]) + exp(real_ant_out$beta[,3,3]) + exp(real_ant_out$beta[,3,4]) + 
  ## vac -> crem                        other -> crem               crem -> crem                        liom -> crem
  exp(real_ant_out$beta[,4,1]) + exp(real_ant_out$beta[,4,2]) + exp(real_ant_out$beta[,4,3]) + exp(real_ant_out$beta[,4,4])

pred_ant <- cbind(
  exp(real_ant_out$beta[,1,1])/Denominator,
  exp(real_ant_out$beta[,1,2])/Denominator,
  exp(real_ant_out$beta[,1,3])/Denominator,
  exp(real_ant_out$beta[,1,4])/Denominator,
  exp(real_ant_out$beta[,2,1])/Denominator,
  exp(real_ant_out$beta[,2,2])/Denominator,
  exp(real_ant_out$beta[,2,3])/Denominator,
  exp(real_ant_out$beta[,2,4])/Denominator,
  exp(real_ant_out$beta[,3,1])/Denominator,
  exp(real_ant_out$beta[,3,2])/Denominator,
  exp(real_ant_out$beta[,3,3])/Denominator,
  exp(real_ant_out$beta[,3,4])/Denominator,
  exp(real_ant_out$beta[,4,1])/Denominator,
  exp(real_ant_out$beta[,4,2])/Denominator,
  exp(real_ant_out$beta[,4,3])/Denominator,
  exp(real_ant_out$beta[,4,4])/Denominator
)
pred_probs <- c(mean(pred_ant[,1]), ## vac-> liom, 
                mean(pred_ant[,2]), ## other -> liom,
                mean(pred_ant[,3]), ## crem -> liom,
                mean(pred_ant[,4]), ## liom -> liom,
                mean(pred_ant[,5]), ## vac-> vac, 
                mean(pred_ant[,6]), ## other -> vac,
                mean(pred_ant[,7]), ## crem -> vac,
                mean(pred_ant[,8]), ## liom -> vac,
                mean(pred_ant[,9]), ## vac-> other, 
                mean(pred_ant[,10]), ## other -> other,
                mean(pred_ant[,11]), ## crem -> other,
                mean(pred_ant[,12]), ## liom -> other,
                mean(pred_ant[,13]), ## vac-> crem, 
                mean(pred_ant[,14]), ## other -> crem,
                mean(pred_ant[,15]), ## crem -> crem,
                mean(pred_ant[,16]) ## liom -> crem,
)
sum(pred_probs)


###### plot the probabilities
## Barplot
means <- c(mean(pred_ant[,1]),mean(pred_ant[,2]),mean(pred_ant[,3]),mean(pred_ant[,4]))
barplot(means, col = c("blue","black","pink","red"))
legend("topright", c("vac","crem","liom","other"), fill = c("black","red","blue","pink"))


## Check against Frequentist model 
## freq model 
cactus_fit_ant <- summary(multinom(ant_t1_relevel ~ (as.factor(ant_t_relevel)), cactus_real))
## Calculate the probabilities of each state
                                                                    ## other <- other               other <- crem                         other <- liom
Denominator <- 1 + sum(exp(cactus_fit_ant$coefficients[1,1] + cactus_fit_ant$coefficients[1,2] + cactus_fit_ant$coefficients[1,3] + cactus_fit_ant$coefficients[1,4]) + 
                                                                    ## crem <- other                crem <- crem                          crem <- liom
                         exp(cactus_fit_ant$coefficients[2,1] + cactus_fit_ant$coefficients[2,2] + cactus_fit_ant$coefficients[2,3] + cactus_fit_ant$coefficients[2,4]) + 
                                                                    ## liom <- other                liom <- crem                          liom <- liom
                         exp(cactus_fit_ant$coefficients[3,1] + cactus_fit_ant$coefficients[3,2] + cactus_fit_ant$coefficients[3,3] + cactus_fit_ant$coefficients[3,4]))

pred_freq_ant<-cbind(
  #pr(vacant)
  1/(Denominator),
  #pr(other)
  exp(cactus_fit_ant$coefficients[1,1] + cactus_fit_ant$coefficients[1,2] + cactus_fit_ant$coefficients[1,3] + cactus_fit_ant$coefficients[1,4])/(Denominator),
  #pr(crem)
  exp(cactus_fit_ant$coefficients[2,1] + cactus_fit_ant$coefficients[2,2] + cactus_fit_ant$coefficients[2,3] + cactus_fit_ant$coefficients[2,4])/(Denominator),
    #pr(liom)
  exp(cactus_fit_ant$coefficients[3,1] + cactus_fit_ant$coefficients[3,2] + cactus_fit_ant$coefficients[3,3] + cactus_fit_ant$coefficients[3,4])/(Denominator)
)
sum(pred_freq_ant)

## Compare the probabilities of the frequentist model to the bayesian model to the real data (total for y)
## Real data -- each row sums to the probability of being in state a in year t1
probs <- table(cactus_real$ant_t1_relevel, cactus_real$ant_t_relevel)/nrow(cactus_real)
c(sum(probs[1,]), sum(probs[2,]), sum(probs[3,]), sum(probs[4,]))
## Freq data 
## vac, other, crem, liom
pred_freq_ant
## Bayes data
pred_probs
c(sum(pred_probs[c(1,2,3,4)]), sum(pred_probs[c(5,6,7,8)]), sum(pred_probs[c(9,10,11,12)]), sum(pred_probs[c(13,14,15,16)]))
## The freq model is still very close to the real data. The Bayesian model is not as close, but 
## isn't that bad. It seems to underestimate all of them just a bit (except liom)


      
# #############################################################################################
# ###### now include ant and size as predictors  ##############################################
# ###### Data Analysis/STAN Models/multi_prac_size_ant_Km1.stan ###############################
# #############################################################################################
multi_dat_real <- list(K = length(unique(cactus_real$ant_t1_relevel)), #number of possible ant species
                       N = dim(cactus_real)[1], #number of observations
                       D = 3, #number of predictors
                       y = as.integer(as.factor(cactus_real$ant_t1_relevel)), #observations
                       x = model.matrix(~ as.integer(as.factor(ant_t_relevel)) + logsize_t, cactus_real)) #design matrix
## Run the model & save the results
real_ant_size <- stan(file = "Data Analysis/STAN Models/multi_prac_tom_Km1.stan", 
                 data = multi_dat_real, warmup = 100, iter = 1000, chains = 3)
real_ant_size_out <- rstan::extract(real_ant_size, pars = c("beta"))
write.csv(real_ant_size_out, "multi_ant_size_out")
## plot the chains
mcmc_trace(real_ant)
mean(real_ant_out$beta[,1,2])
summary(real_ant)
## this looks pretty good. All betas converge







###### Import data from simulated run model
full_data_real <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/full_data_real.csv", header = TRUE,stringsAsFactors=T)
size_dummy_real <- seq(min((cactus_real$logsize_t)), max((cactus_real$logsize_t)), by=0.1)
## make a denominator
Denominator <- 1 + exp(mean(full_data_real$beta.1.1)*size_dummy_real + mean(full_data_real$beta.2.1)*size_dummy_real + mean(full_data_real$beta.3.1)*size_dummy_real) + 
  exp(mean(full_data_real$beta.1.2)*size_dummy_real + mean(full_data_real$beta.2.2)*size_dummy_real + mean(full_data_real$beta.3.2)*size_dummy_real) + 
  exp(mean(full_data_real$beta.1.3)*size_dummy_real + mean(full_data_real$beta.2.3)*size_dummy_real + mean(full_data_real$beta.3.3)*size_dummy_real) 
## 
Denominator <- 1 + exp(mean(full_data_real$beta.1.1)*size_dummy_real + mean(full_data_real$beta.2.1)*size_dummy_real + mean(full_data_real$beta.3.1)*size_dummy_real) + 
  exp(mean(full_data_real$beta.1.2)*size_dummy_real + mean(full_data_real$beta.2.2)*size_dummy_real + mean(full_data_real$beta.3.2)*size_dummy_real) + 
  exp(mean(full_data_real$beta.1.3)*size_dummy_real + mean(full_data_real$beta.2.3)*size_dummy_real + mean(full_data_real$beta.3.3)*size_dummy_real) 

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




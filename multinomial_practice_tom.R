## Purpose: build understanding of multinomial regression
library("nnet")
library(tidyverse)
library(tidyr)
library(rstan)
library(bayesplot)
library(dplyr)
library(raster)
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
invlogit<-function(x){exp(x)/(1+exp(x))}

# Real data analysis ------------------------------------------------------
ants <- read.csv("Data Analysis/cholla_demography_20042019_cleaned.csv")
str(ants)
## convert ants to factor and set vacant as reference level
ants$ant_t <- relevel(factor(ants$ant_t),ref="vacant")
ants$ant_t1 <- relevel(factor(ants$ant_t1),ref="vacant")
## define log volume
ants$logsize <- log(ants$volume_t)

##stan
write("data {
  int K;
  int N;
  int D;
  int y[N];
  matrix[N, D] x;
}
parameters {
  matrix[D, K] beta;
}
model {
  matrix[N, K] x_beta = x * beta;

  to_vector(beta) ~ normal(0, 5);

  for (n in 1:N)
    y[n] ~ categorical_logit(x_beta[n]');
}",
"Data Analysis/STAN Models/size_only_model.stan")

## select variables we need, and drop na's
ants %>% 
  dplyr::select(ant_t,ant_t1,logsize,Year_t) %>% 
  drop_na() -> ants_stan
  # %>% slice_sample(n=1000) ## slicing out this data subset will make things faster for practice
  

#the code above should work now with dplyr::select
#ants_na <- ants[,c("ant_t1","logsize")]
#ants_na <- na.omit(ants_na)
#ants_stan <- slice_sample(ants_na, n = 1000)

size_only_dat <- list(K = length(levels(ants_stan$ant_t1)), # number of possible outcomes
                  N = (dim(ants_stan)[1]), # number of observations
                  D = 1, # number of predictors
                  y = as.integer(ants_stan$ant_t1), # observations
                  x = as.matrix(ants_stan$logsize)) # design matrix

fit_size_only <- stan(file = "Data Analysis/STAN Models/size_only_model.stan", 
                 data = size_only_dat, warmup = 100, iter = 1000, chains = 2)
fit_summary <- summary(fit_size_only, par="beta", probs=.5)$summary %>% as.data.frame
size_only <- rstan::extract(fit_size_only, pars = c("beta"))
write.csv(size_only,"size_only_outputs.csv")

## compare to ML multinomial
ants_fit <- multinom(ant_t1 ~ 0 + logsize, data=ants_stan)
## note that the coefficients are expressing log odds with respect to the reference level
coef(ants_fit)
dses <- data.frame(logsize = mean(ants_stan$logsize))
transition_beta <- predict(ants_fit, newdata = dses, "probs")
## A one unit increase in size is associated with a 0.15 DECREASE in log odds of having crem vs vacant
## Liom is the opposite sign, so increasing size makes it more likely to have liom than vacant
newdata=data.frame(logsize=quantile(ants_stan$logsize,probs=c(0.05,0.25,0.5,0.75,0.95)))
predict(ants_fit,newdata,"probs")
##this comes from here: https://stackoverflow.com/questions/22293517/multinomial-regression-using-multinom-function-in-r?rq=1

## if we subtract the reference level from all other levels, we recover the ML estimates
## see: https://stackoverflow.com/questions/60551126/compare-multinom-to-stan-multi-logit-regression
## Note here that fit_summary[ant,1]
## 1 = ref = vacant, 2 = crem, 3 = liom, 4 = other
fit_summary[2,1]-fit_summary[1,1] 
fit_summary[3,1]-fit_summary[1,1]
fit_summary[4,1]-fit_summary[1,1]

## A one unit increase in size is associated with a 0.16 decrease in log odds of having crem vs vacant
## Liom means that increasing size is associated with a 0.03 increase in log odds of having liom vs vacant
## An increase in size is associated with a 0.27 decrease in log odds of having other vs vacant
size_only_out <- read.csv("size_only_outputs.csv")

size_only_out$beta.1.2 - size_only_out$beta.1.1 ## vector of all crem
size_only_out$beta.1.3 - size_only_out$beta.1.1 ## vector of all liom
size_only_out$beta.1.4 - size_only_out$beta.1.1 ## vector of all other

mean(size_only_out$beta.1.1)*2
mean(size_only_out$beta.1.2)*2
mean(size_only_out$beta.1.3)*2
mean(size_only_out$beta.1.4)*2

model.matrix(~0 + ant_t + logsize, data=ants_stan[1:10,])

## Visualize the outcomes
x_dummy <- seq(min(ants_stan$logsize), max(ants_stan$logsize), by = 0.1)
plot(x = x_dummy, y = invlogit(x_dummy*mean(size_only_out$beta.1.1)), type = "l", ylim = c(0,1), col = "pink") ##vac
lines(x = x_dummy, y = invlogit(x_dummy*mean(size_only_out$beta.1.2 - size_only_out$beta.1.1)), col = "red") ##crem
lines(x = x_dummy, y = invlogit(x_dummy*mean(size_only_out$beta.1.3 - size_only_out$beta.1.1)), col = "blue") ##liom
lines(x = x_dummy, y = invlogit(x_dummy*mean(size_only_out$beta.1.4 - size_only_out$beta.1.1)), col = "black") ##other

## So basically as I understand it these are the probabilities of being occupied by each ant at a given size
## cool
##TOM: no, I don't think these are the probabilities! They do not sum to 1. 

### Include Previous Ant state ---------------------------------------------
## to get a sense of what we are actually trying to do, start with the ML fit
ants_fit2 <- multinom(ant_t1 ~ 0 + ant_t + logsize, data=ants_stan)
## check out the coefficients and note that we have 12 coefficients to estimate
coef(ants_fit2) 

## we can use the same stan model as before, since it is coded very generally
## we just need to update the dimensions of the design matrix
size_ant_t_dat <- list(K = length(levels(ants_stan$ant_t1)), # number of possible outcomes
                      N = (dim(ants_stan)[1]), # number of observations
                      D = 5, # number of predictors: size and four levels of prior ant
                      y = as.integer(ants_stan$ant_t1), # observations
                      x = model.matrix(~ 0 + ant_t + logsize, data=ants_stan)) # design matrix

fit_size_ant_t <- stan(file = "Data Analysis/STAN Models/size_only_model.stan", 
                      data = size_ant_t_dat, warmup = 100, iter = 1000, chains = 2)
fit_summary_size_ant_t <- summary(fit_size_ant_t, par="beta", probs=.5)$summary %>% as.data.frame
size_ant_t <- rstan::extract(fit_size_ant_t, pars = c("beta"))
write.csv(size_ant_t,"size_ant_t_outputs.csv")

fit_summary_size_ant_t-fit_summary_size_ant_t[1,"mean"]

## Tom's scratch pad here
ants_MoM <- multinom(ant_t1 ~ 1, data=ants_stan)
coef(ants_MoM)

MoM_dat <- list(K = length(levels(ants_stan$ant_t1)), # number of possible outcomes
                       N = (dim(ants_stan)[1]), # number of observations
                       D = 1, # number of predictors: intercept only
                       y = as.integer(ants_stan$ant_t1), # observations
                       x = model.matrix(~ , data=ants_stan)) # design matrix

fit_MoM <- stan(file = "Data Analysis/STAN Models/size_only_model.stan", 
                       data = MoM_dat, warmup = 100, iter = 1000, chains = 3)
mcmc_dens_overlay(fit_MoM, pars = c("beta[2,1]",
                                    "beta[2,2]",
                                    "beta[2,3]",
                                    "beta[1,4]"))

betas <- get_posterior_mean(fit_MoM)[,"mean-all chains"][1:4]
betas[2:4]-betas[1];coef(ants_MoM)
invlogit(betas)
exp(betas[2:4]-betas[1])/(1+exp(betas[2:4]-betas[1]))

### Ali's work starts here
# Real data analysis ------------------------------------------------------
##stan
size_ant_model <- "data {
  int K;
  int N;
  int D;
  int y[N];
  matrix[N, D] x;
}
parameters {
  matrix[D, K] beta;
}
model {
  matrix[N, K] x_beta = x * beta;

  to_vector(beta) ~ normal(0, 5);

  for (n in 1:N)
    y[n] ~ categorical_logit(x_beta[n]');
}"

## select variables we need, and drop na's
ants %>% select(ant_t1,logsize) %>% drop_na() %>% slice_sample(n=1000)-> ants_stan

ants_na <- ants[,c("ant_t1","logsize","ant_t")]
ants_na <- na.omit(ants_na)
ants_stan <- slice_sample(ants_na, n = 1000)
ants_stan$logsize <- as.numeric(ants_stan$logsize)
ants_stan$ant_t <- (as.factor(ants_stan$ant_t))
ants_stan$ant_t <- relevel(factor(ants_stan$ant_t),ref="vacant")
ants_stan$ant_t1 <- relevel(factor(ants_stan$ant_t1),ref="vacant")
x = as.matrix(ants_stan$logsize)
x = cbind(x,ants_stan$ant_t)
ants_stan$ant_t <- relevel(factor(ants_stan$ant_t),ref="vacant")
ants_stan$ant_t1 <- relevel(factor(ants_stan$ant_t1),ref="vacant")
size_ant_dat <- list(K = 4, # number of possible outcomes
                      N = (dim(ants_stan)[1]), # number of observations
                      D = 2, # number of predictors
                      y = as.integer(as.factor(ants_stan$ant_t1)), # observations
                      x = x) # design matrix

fit_size_ant <- stan(model_code = size_ant_model, 
                      data = size_ant_dat, warmup = 100, iter = 1000, chains = 3, algorithm="Fixed_param")

fit_size_ant_summary <- summary(fit_size_ant, par="beta", probs=.5)$summary %>% as.data.frame
size_ant <- rstan::extract(fit_size_ant, pars = c("beta"))
write.csv(size_ant,"size_ant_outputs.csv")

## Make each prev ant species have its own column
crem <- as.integer(ants_stan$ant_t == "crem")
vac <- as.integer(ants_stan$ant_t == "vacant")
liom <- as.integer(ants_stan$ant_t == "liom")
other <- as.integer(ants_stan$ant_t == "other")
ants_stan$ant_t1 <- relevel(factor(ants_stan$ant_t1),ref="vacant")
x = as.matrix(ants_stan$logsize)
x = cbind(x,crem)
x = cbind(x,liom)
x = cbind(x,other)
x = cbind(x, vac)
size_ant_dat2 <- list(K = 4, # number of possible outcomes
                     N = (dim(ants_stan)[1]), # number of observations
                     D = 5, # number of predictors
                     y = as.integer(as.factor(ants_stan$ant_t1)), # observations
                     x = x) # design matrix

## Getting no parameters error so here are different ways to run the code to try and fix this
fit_size_ant2 <- stan(model_code = size_ant_model, 
                      data = size_ant_dat, warmup = 100, iter = 1000, chains = 3, algorithm="Fixed_param")

fit_size_ant_summary2 <- summary(fit_size_ant2, par="beta", probs=.5)$summary %>% as.data.frame
size_ant2 <- rstan::extract(fit_size_ant, pars = c("beta"))
write.csv(size_ant2,"size_ant_outputs2.csv")

## compare to ML multinomial
ants_fit <- multinom(ant_t1 ~ 0 + logsize + ant_t, data=ants_stan)
## note that the coefficients are expressing log odds with respect to the reference level
coef(ants_fit)
## A one unit increase in size is associated with a 0.15 DECREASE in log odds of having crem vs vacant
## Liom is the opposite sign, so increasing signs makes it more likely to have liom than vacant
## A one unit increase in size is associated with a 0.16 decrease in log odds of having crem vs vacant
## Liom means that increasing size is associated with a 0.03 increase in log odds of having liom vs vacant
## An increase in size is associated with a 0.27 decrease in log odds of having other vs vacant
dses <- data.frame(ant_t = c("vacant","liom","crem","other"), logsize = mean(ants_stan$logsize))
transition_beta <- predict(ants_fit, newdata = dses, "probs")


size_ant_out <- read.csv("size_ant_outputs.csv")
size_ant_out2 <- read.csv("size_ant_outputs2.csv")
invlogit((size_ant_out$beta.1.1)* + (size_ant_out$beta.2.1))
invlogit((size_ant_out$beta.1.2 - size_ant_out$beta.1.1) + (size_ant_out$beta.2.2 - size_ant_out$beta.2.1)) ## vector of all crem
invlogit((size_ant_out$beta.1.3 - size_ant_out$beta.1.1) + (size_ant_out$beta.2.3 - size_ant_out$beta.2.1)) ## vector of all liom
invlogit((size_ant_out$beta.1.4 - size_ant_out$beta.1.1) + (size_ant_out$beta.2.4 - size_ant_out$beta.2.1)) ## vector of all other

## If I am doing these correctly, then 1. ... = the volume param and 2. ... = the prev ant param
## This means that the probability of being colonized by any ant species is what is put out based on the size and prev ant
## My problem w these outputs is which ant species is it colonized by?????
## Do I maybe need to make a different column for each ant species???

## Visualize the outcomes
size_ant <- read.csv("size_ant_outputs.csv")
x_dummy <- seq(min(ants_stan$logsize), max(ants_stan$logsize), by = 0.1)
plot(x = x_dummy, y = invlogit(x_dummy*mean(size_ant_out$beta.1.1) + mean(size_ant_out$beta.2.1)), type = "l", ylim = c(0,1), col = "black") ##vac
lines(x = x_dummy, y = invlogit(x_dummy*mean(size_ant_out$beta.1.2 - size_ant_out$beta.1.1) + mean(size_ant_out$beta.2.2 - size_ant_out$beta.2.1)), col = "red") ##crem
lines(x = x_dummy, y = invlogit(x_dummy*mean(size_ant_out$beta.1.3 - size_ant_out$beta.1.1) + mean(size_ant_out$beta.2.3 - size_ant_out$beta.2.1)), col = "blue") ##liom
lines(x = x_dummy, y = invlogit(x_dummy*mean(size_ant_out$beta.1.4 - size_ant_out$beta.1.1) + mean(size_ant_out$beta.2.4 - size_ant_out$beta.2.1)), col = "pink") ##other


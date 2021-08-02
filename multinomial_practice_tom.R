## Purpose: build understanding of multinomial regression
library("nnet")
library(tidyverse)
library(tidyr)
library(rstan)
library(bayesplot)
library(dplyr)
library(raster)
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

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
ants %>% select(ant_t1,logsize) %>% drop_na() %>% slice_sample(n=1000)-> ants_stan

ants_na <- ants[,c("ant_t1","logsize")]
ants_na <- na.omit(ants_na)
ants_stan <- slice_sample(ants_na, n = 1000)


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
## A one unit increase in size is associated with a 0.15 DECREASE in log odds of having crem vs vacant
## Liom is the opposite sign, so increasing signs makes it more likely to have liom than vacant

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

## Visualize the outcomes
x_dummy <- seq(min(ants_na$logsize), max(ants_na$logsize), by = 0.1)
plot(x = x_dummy, y = invlogit(x_dummy*mean(size_only_out$beta.1.1)), type = "l", ylim = c(0,1), col = "pink") ##vac
lines(x = x_dummy, y = invlogit(x_dummy*mean(size_only_out$beta.1.2 - size_only_out$beta.1.1)), col = "red") ##crem
lines(x = x_dummy, y = invlogit(x_dummy*mean(size_only_out$beta.1.3 - size_only_out$beta.1.1)), col = "blue") ##liom
lines(x = x_dummy, y = invlogit(x_dummy*mean(size_only_out$beta.1.4 - size_only_out$beta.1.1)), col = "black") ##other


### Include Previous Ant state ---------------------------------------------
# Real data analysis ------------------------------------------------------
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
"Data Analysis/STAN Models/size_ant_model.stan")

## select variables we need, and drop na's
ants %>% select(ant_t1,logsize) %>% drop_na() %>% slice_sample(n=1000)-> ants_stan

ants_na <- ants[,c("ant_t1","logsize","ant_t")]
ants_na <- na.omit(ants_na)
ants_stan <- slice_sample(ants_na, n = 1000)
ants_stan$logsize <- as.numeric(ants_stan$logsize)
ants_stan$ant_t <- as.integer(as.factor(ants_stan$ant_t))

x = as.matrix(ants_stan$logsize)
x = cbind(x,ants_stan$ant_t)
size_ant_dat <- list(K = length(levels(ants_stan$ant_t1)), # number of possible outcomes
                      N = (dim(ants_stan)[1]), # number of observations
                      D = 2, # number of predictors
                      y = as.integer(as.factor(ants_stan$ant_t1)), # observations
                      x = x) # design matrix

fit_size_ant <- stan(file = "Data Analysis/STAN Models/size_ant_model.stan", 
                      data = size_ant_dat, warmup = 1, iter = 10, chains = 2)
fit_size_ant_summary <- summary(fit_size_ant, par="beta", probs=.5)$summary %>% as.data.frame
size_ant <- rstan::extract(fit_size_ant, pars = c("beta"))
write.csv(size_ant,"size_ant_outputs.csv")

## compare to ML multinomial
ants_fit <- multinom(ant_t1 ~ 0 + logsize + ant_t, data=ants_stan)
## note that the coefficients are expressing log odds with respect to the reference level
coef(ants_fit)
## A one unit increase in size is associated with a 0.15 DECREASE in log odds of having crem vs vacant
## Liom is the opposite sign, so increasing signs makes it more likely to have liom than vacant

## if we subtract the reference level from all other levels, we recover the ML estimates
## see: https://stackoverflow.com/questions/60551126/compare-multinom-to-stan-multi-logit-regression
## 1 = ref = vacant, 2 = crem, 3 = liom, 4 = other
## 1 & 2 = coefficient for size and prev ant
fit_size_ant_summary
(fit_size_ant_summary[2,1]-fit_size_ant_summary[1,1]) 
fit_size_ant_summary[3,1]-fit_size_ant_summary[1,1]
fit_size_ant_summary[4,1]-fit_size_ant_summary[1,1]

## A one unit increase in size is associated with a 0.16 decrease in log odds of having crem vs vacant
## Liom means that increasing size is associated with a 0.03 increase in log odds of having liom vs vacant
## An increase in size is associated with a 0.27 decrease in log odds of having other vs vacant

size_ant_out <- read.csv("size_ant_outputs.csv")

(size_ant_out$beta.1.2 - size_ant_out$beta.1.1) + (size_ant_out$beta.2.2 - size_ant_out$beta.2.1) ## vector of all crem
(size_ant_out$beta.1.3 - size_ant_out$beta.1.1) ## vector of all liom
(size_ant_out$beta.1.4 - size_ant_out$beta.1.1) ## vector of all other

## Visualize the outcomes
size_ant <- read.csv("size_ant_outputs.csv")
x_dummy <- seq(min(ants_na$logsize), max(ants_na$logsize), by = 0.1)
plot(x = x_dummy, y = invlogit(x_dummy*mean(size_ant$beta.1.1) + mean(size_ant$beta.2.1)), type = "l", ylim = c(0,1), col = "pink") ##vac
lines(x = x_dummy, y = invlogit(x_dummy*mean(size_ant$beta.1.2 - size_ant$beta.1.1) + mean(size_ant$beta.2.2 - size_ant$beta.2.1)), col = "red") ##crem
lines(x = x_dummy, y = invlogit(x_dummy*mean(size_ant$beta.1.3 - size_ant$beta.1.1) + mean(size_ant$beta.2.3 - size_ant$beta.2.1)), col = "blue") ##liom
lines(x = x_dummy, y = invlogit(x_dummy*mean(size_ant$beta.1.4 - size_ant$beta.1.1) + mean(size_ant$beta.2.4 - size_ant$beta.2.1)), col = "black") ##other


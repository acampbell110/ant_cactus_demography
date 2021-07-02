## Purpose: build understanding of multinomial regression
library("nnet")
library(tidyverse)
library(rstan)
library(bayesplot)

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

size_only_dat <- list(K = length(levels(ants_stan$ant_t1)), # number of possible outcomes
                  N = (dim(ants_stan)[1]), # number of observations
                  D = 1, # number of predictors
                  y = as.integer(ants_stan$ant_t1), # observations
                  x = as.matrix(ants_stan$logsize)) # design matrix

fit_size_only <- stan(file = "Data Analysis/STAN Models/size_only_model.stan", 
                 data = size_only_dat, warmup = 100, iter = 1000, chains = 2)
fit_summary <- summary(fit_size_only, par="beta", probs=.5)$summary %>% as.data.frame

## compare to ML multinomial
ants_fit <- multinom(ant_t1 ~ 0 + logsize, data=ants_stan)
## note that the coefficients are expressing log odds with respect to the reference level
coef(ants_fit)
## A one unit increase in size is associated with a 0.15 DECREASE in log odds of having crem vs vacant
## Liom is the opposite sign, so increasing signs makes it more likely to have liom than vacant

## if we subtract the reference level from all other levels, we recover the ML estimates
## see: https://stackoverflow.com/questions/60551126/compare-multinom-to-stan-multi-logit-regression
fit_summary[2,1]-fit_summary[1,1]
fit_summary[3,1]-fit_summary[1,1]
fit_summary[4,1]-fit_summary[1,1]


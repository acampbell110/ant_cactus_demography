## simulation code from: https://stats.stackexchange.com/questions/103728/simulating-multinomial-logit-data-with-r
#Adding library for multinomial logit regression
#setwd("C:/Users/tm9/Dropbox/github/ant_cactus_demography")
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
library("nnet")
library(rstan)
library(bayesplot)

#Genarating 500 random numbers with zero mean
x = rnorm(3885,0)
#Assigning the values of beta1 and beta2
Beta1 = 20
Beta2 = .05
#Calculation of denominator for probability calculation
Denominator= 1+exp(Beta1*x)+exp(Beta2*x)
#Calculating the matrix of probabilities for three choices
vProb = cbind(1/Denominator, exp(x*Beta1)/Denominator, exp(x*Beta2)/Denominator )
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

## Now Stan model -- size only
write("data {
  int<lower=2> K;
  int<lower=0> N;
  int<lower=1> D;
  int<lower=1, upper=K> y[N];
  matrix[N, D] x;
}

parameters {
  matrix[D, K] beta;
}

model {
  matrix[N, K] x_beta = x * beta;

  to_vector(beta) ~ normal(0, 5);

  for (n in 1:N) {
    y[n] ~ categorical_logit(x_beta[n]');

  }
}",
"Data Analysis/STAN Models/multi_prac_tom_K.stan")

multi_dat <- list(K = length(unique(dfM$y)), # number of possible outcomes
                  N = (dim(dfM)[1]), # number of observations
                  D = 1, # number of predictors
                  y = dfM$y, # observations
                  x = as.matrix(dfM$x)) # design matrix

#thing <- model.matrix(y ~ x + 0, dfM)
fit_stan <- stan(file = "Data Analysis/STAN Models/multi_prac_tom_K.stan", 
               data = multi_dat, warmup = 100, iter = 1000, chains = 3)

mcmc_trace(fit_stan)

cactus_real<-cactus[, c("ant_t1","volume_t")]
cactus_real<-na.omit(cactus_real)
multi_dat_real <- list(K = length(unique(cactus_real$ant_t1)), #number of possible ant species
                       N = dim(cactus_real)[1], #number of observations
                       D = 1, #number of predictors
                       y = as.integer(as.factor(cactus_real$ant_t1)), #observations
                       x = as.matrix(cactus_real$volume_t) #design matrix
                       )

fit_stan_real <- stan(file = "Data Analysis/STAN Models/multi_prac_tom_K.stan", 
                 data = multi_dat_real, warmup = 100, iter = 1000, chains = 3)


mcmc_trace(fit_stan_real)


### now try K-1 version

## Now Stan model -- size only
write("data {
  int<lower=2> K;
  int<lower=0> N;
  int<lower=1> D;
  int<lower=1, upper=K> y[N];
  matrix[N, D] x;
}

transformed data {
  vector[D] zeros = rep_vector(0, D);
}

parameters {
  matrix[D, K - 1] beta_raw;
}

transformed parameters {
  matrix[D, K] beta = append_col(beta_raw, zeros);
}

model {
  matrix[N, K] x_beta = x * beta;

  to_vector(beta) ~ normal(0, 5);

  for (n in 1:N) {
    y[n] ~ categorical_logit(x_beta[n]');

  }
}",
"Data Analysis/STAN Models/multi_prac_tom_Km1.stan")

fit_stan <- stan(file = "Data Analysis/STAN Models/multi_prac_tom_Km1.stan", 
                 data = multi_dat, warmup = 100, iter = 1000, chains = 3)

mcmc_trace(fit_stan)

fit<-(multinom(multi_dat$y ~ multi_dat$x + 0))
summary(fit)

## Now run this model with the real data
fit_stan_real <- stan(file = "Data Analysis/STAN Models/multi_prac_tom_Km1.stan", 
                      data = multi_dat_real, warmup = 300, iter = 5000, chains = 3)

mcmc_trace(fit_stan_real)

###### now add intercept
x = rnorm(1000,0)
alpha <- c(-1,2)
beta <- c(0,0)
Denominator= 1+exp(alpha[1] + beta[1]*x)+exp(alpha[2] + beta[2]*x)
#Calculating the matrix of probabilities for three choices
vProb = cbind(1/Denominator, exp(alpha[1] + beta[1]*x)/Denominator, exp(alpha[2] + beta[2]*x)/Denominator )
# Assigning the value one to maximum probability and zero for rest to get the appropriate choices for value of x
mChoices = t(apply(vProb, 1, rmultinom, n = 1, size = 1))
# Value of Y and X together
dfM = cbind.data.frame(y = apply(mChoices, 1, function(x) which(x==1)), x)
summary(multinom(y ~ x, dfM))

multi_dat <- list(K = length(unique(dfM$y)), # number of possible outcomes
                  N = (dim(dfM)[1]), # number of observations
                  D = (dim(dfM)[2]), # number of predictors
                  y = dfM$y, # observations
                  x = model.matrix(~ x, dfM)) # design matrix


fit_stan <- stan(file = "Data Analysis/STAN Models/multi_prac_tom_Km1.stan", 
                 data = multi_dat, warmup = 5000, iter = 10000, chains = 3)
mcmc_trace(fit_stan)

mcmc_hist(fit_stan,pars=c("beta_raw[2,2]","beta_raw[2,1]",
                          "beta_raw[1,2]","beta_raw[1,1]"))

###### now add categorical variable
x = rnorm(1000,0)
## Create the x categorical variable of the dataset (Ant_t)
x_cat = matrix(0, nrow = 1000, ncol = 3) ## Categorical variable with 3 choices
a <- vector()
j <- vector()
for(i in 1:1000){
  a[i] <- rbinom(1,1,0.5)
  x_cat[i,1] <- a[i]
  if(x_cat[i,1] == 0) {
    j[i] <- rbinom(1,1,0.5)
    if(j[i] == 1){x_cat[i,2] <- 1}
    if(j[i] == 0){x_cat[i,3] <- 1}
  }
}
x_t <- apply(x_cat, 1, function(x_cat) which(x_cat==1))
## Create the y of the dataset (Ant_t1)
alpha <- c(-1,2)
beta <- c(0,0,1,2,1,2,2,0)
Denominator= 1+exp(alpha[1] + beta[1]*x_quant + beta[3]*x_cat[,1] + beta[5]*x_cat[,2] + beta[7]*x_cat[,3])+
  exp(alpha[2] + beta[2]*x_quant + beta[4]*x_cat[1] + beta[6]*x_cat[2] + beta[8]*x_cat[3])
#Calculating the matrix of probabilities for three choices
vProb = cbind(1/Denominator, exp(alpha[1] + beta[1]*x_quant + beta[3]*x_cat[,1] + beta[5]*x_cat[,2] + beta[7]*x_cat[,3])/Denominator, 
              exp(alpha[2] + beta[2]*x_quant + beta[4]*x_cat[,1] + beta[6]*x_cat[,2] + beta[8]*x_cat[,3])/Denominator )
# Assigning the value one to maximum probability and zero for rest to get the appropriate choices for value of x
mChoices = t(apply(vProb, 1, rmultinom, n = 1, size = 1))
# Value of Y and X together
dfM = cbind.data.frame(y = apply(mChoices, 1, function(x) which(x==1)), x,x_t)
summary(multinom(y ~ x+x_t, dfM))

multi_dat <- list(K = length(unique(dfM$y)), # number of possible outcomes
                  N = (dim(dfM)[1]), # number of observations
                  D = (dim(dfM)[2]), # number of predictors
                  y = dfM$y, # observations
                  x = model.matrix(~ x+x_t, dfM)) # design matrix


## Now Stan model -- size + ant state
write("data {
  int<lower=2> K;
  int<lower=0> N;
  int<lower=1> D;
  int<lower=1, upper=K> y[N];
  matrix[N, D] x;
}

transformed data {
  vector[D] zeros = rep_vector(0, D);
}

parameters {
  matrix[D, K - 1] beta_raw;
}

transformed parameters {
  matrix[D, K] beta = append_col(beta_raw, zeros);
}

model {
  matrix[N, K] x_beta = x * beta;

  to_vector(beta) ~ normal(0, 5);

  for (n in 1:N) {
    y[n] ~ categorical_logit(x_beta[n]');

  }
}",
"Data Analysis/STAN Models/multi_prac_size_ant_Km1.stan")


fit_stan <- stan(file = "Data Analysis/STAN Models/multi_prac_size_ant_Km1.stan", 
                 data = multi_dat, warmup = 50, iter = 100, chains = 3)
mcmc_trace(fit_stan)

mcmc_hist(fit_stan,pars=c("beta_raw[2,2]","beta_raw[2,1]",
                          "beta_raw[1,2]","beta_raw[1,1]"))




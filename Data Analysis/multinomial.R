## simulation code from: https://stats.stackexchange.com/questions/103728/simulating-multinomial-logit-data-with-r
#Adding library for multinomial logit regression
setwd("C:/Users/tm9/Dropbox/github/ant_cactus_demography/Data Analysis")
library("nnet")
library(rstan)

#Genarating 500 random numbers with zero mean
x = rnorm(100,0)
#Assigning the values of beta1 and beta2
Beta1 = 2
Beta2 = .5
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

## Now Stan model
write("data {
  int K;
  int N;
  int D;
  int y[N];
  matrix[N, D] x;
}
transformed data {
  row_vector[D] zeros = rep_row_vector(0, D);
}
parameters {
  matrix[K - 1, D] beta_raw;
}
transformed parameters {
  matrix[K, D] beta;
  beta = append_row(beta_raw, zeros);
}
model {
  matrix[N, K] x_beta = x * beta;

  to_vector(beta) ~ normal(0, 5);

  for (n in 1:N)
    y[n] ~ categorical_logit(x_beta[n]');
}",
"STAN Models/multi_prac_tom.stan")

multi_dat <- list(K = length(unique(dfM$y)), # number of possible outcomes
                  N = (dim(dfM)[1]), # number of observations
                  D = 1, # number of predictors
                  y = dfM$y, # observations
                  x = as.matrix(dfM$x)) # design matrix

#thing <- model.matrix(y ~ x + 0, dfM)
fit_stan <- stan(file = "STAN Models/multi_prac_tom.stan", 
               data = multi_dat, warmup = 100, iter = 1000, chains = 3)

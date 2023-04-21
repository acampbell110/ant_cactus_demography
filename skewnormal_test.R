library(sn)
library(rstan)
library(bayesplot)

y<-rsn(n=10000,xi=0,omega=1,alpha=-5)
mean(y);hist(y)

stan_data <- list(y=y,N=length(y))
fit_skew <- stan(file="Data Analysis/STAN Models/skewnormal_test.stan", 
                      data = stan_data, warmup = 150, iter = 1000, chains = 3, cores = 3, thin = 2)
mcmc_trace(fit_skew)

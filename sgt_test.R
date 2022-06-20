library(sgt)
library(rstan)
# set rstan options
rstan_options( auto_write = TRUE )
options( mc.cores = parallel::detectCores() )
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')

y<-rsgt(n=1000,mu=10,sigma=2,lambda=0.5,p=1,q=20)
hist(y)

stan_dat <- list(N = length(y),y=y) 

## Run the Model
fit <- stan(file = "Data Analysis/STAN Models/sgt.stan", data = stan_dat, warmup = 1000, iter = 5000, chains = 3, cores = 3, thin = 3)


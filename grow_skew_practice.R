### Simulate skew normal data and try to run a model to match it
# fake volume variable 
vol <- rsn(n = 100,xi = 1, omega = 2, alpha = 10)
beta0 <- 2
beta1 <- 3
d_0 <- 1
d_1 <- 1
a_0 <- 3
a_1 <- 5
data <- rsn(n = 100, xi = beta0 + beta1 * vol,
            omega = (d_0 + d_1 * vol),
            alpha = a_0 + a_1 * vol)
plot(density(data))
plot(vol,data)

## Make a list of all necessary variables so they are properly formatted to feed into the stan model
stan_data_grow_skew <- list(N = 100,                                     ## number of observations
                            vol = vol,                             ## predictor volume year t
                            y = (data)                              ## response volume next year
)
## Run the growth model with a student t distribution -- fixed effects: previous size and a non linear previous size variable and ant state; random effects: plot and year; size variation is included for both the omega and alpha estimates
grow_skew_model <- rstan::stan_model("Data Analysis/STAN Models/grow_skew.stan")
fit_grow_skew<-sampling(grow_skew_model,data = stan_data_grow_skew,chains=3,
                       control = list(adapt_delta=0.99,stepsize=0.1),
                       iter=10000,cores=3,thin=2,
                       pars = c("beta0","beta1", #location coefficients
                                "d_0","d_size", #scale coefficiences
                                "a_0","a_size"), #shape coefficients
                       save_warmup=F)
## Check that it converged
bayesplot::mcmc_trace(fit_grow_skew)
## Extract the parameters and compare to the real ones
grow.params <- rstan::extract(fit_grow_skew)
beta0_sim <- grow.params$beta0
beta1_sim <- grow.params$beta1
d_0_sim <- grow.params$d_0
d_1_sim <- grow.params$d_size
a_0_sim <- grow.params$a_0
a_1_sim <- grow.params$a_size
# beta0 = 2, beta0 sim = 1.7
mean(beta0_sim)
# beta1 = 3, beta1 sim = 3.06
mean(beta1_sim)
# d0 = 1, d0 sim = 0.7 or 2
exp(mean(d_0_sim))
# d1 = 1, d1 sim = 0.2 or 1.22
exp(mean(d_1_sim))
# a0 = 3, a0 sim = 
mean(a_0_sim)
# a1 = 5, a1 sim = 
mean(a_1_sim)


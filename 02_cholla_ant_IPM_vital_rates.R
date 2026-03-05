################################################################################ 
################################################################################
## The purpose of this script is to load in the "cleaned" data, subset it 
## properly, run the data through the stan models, and export the outputs as RDS
## files.
################################################################################
################################################################################
## Read the data in
source("01_cholla_ant_IPM_setup.R")
cactus <- read.csv("Data/cholla_demography_20042023_cleaned.csv", header = TRUE,stringsAsFactors=T)

################################################################################
##   Skew Growth Model -- What size will the cacti be next time step?
################################################################################
# Run the growth model with a student t distribution -- fixed effects: previous size and a non linear previous size variable and ant state; random effects: plot and year; size variation is included for both the omega and alpha estimates
grow_skew_model <- stan_model("STAN Models/grow_skew.stan")
fit_grow_skew<-sampling(grow_skew_model,data = stan_data_grow_skew,chains=3,
                       control = list(adapt_delta=0.99,stepsize=0.1),
                       iter=10000,cores=3,thin=2,
                       pars = c("u","w",          # plot and year random effects
                                "beta0","beta1","beta2", #location coefficients
                                "d_0","d_size", #scale coefficiences
                                "a_0","a_size"), #shape coefficients
                       save_warmup=F)

################################################################################
##  Survival Model -- What is the probability of surviving to the next time step?   
################################################################################
## Run the survival model with a bernoulli distribution ---- fixed effects: previous size and ant state; random effects: plot and year
surv_model <- stan_model("STAN Models/surv_code.stan")
fit_surv<-sampling(surv_model, data = stan_data_surv,chains=3,
                   control = list(adapt_delta=0.99,stepsize=0.1),
                   iter=10000,cores=3,thin=2,
                   pars = c("beta0","beta1","u","w"),   #location coefficients)
                   save_warmup=F)



################################################################################
## Flowering Model -- What are the total number of fruits produced in the next time step?
################################################################################
## Run the flower model with a negative binomial distribution ---- fixed effects: previous size; random effects: plot and year
flow_model <- stan_model("STAN Models/flower_trunc_code.stan")
fit_flow<-sampling(flow_model, data = stan_data_flow_trunc,chains=3,
                                                control = list(adapt_delta=0.99,stepsize=0.1),
                                                iter=10000,cores=3,thin=2,
                                                pars = c("u","w",          # plot and year random effects
                                                         "beta0","beta1",  #location coefficients
                                                         "phi"),save_warmup=F)


################################################################################
## Viability Model -- What proportion of fruit are viable? 
################################################################################
# ## Run the Viability Modelwith a binomial distribution ---- fixed effects: ant partner ; random effects: plot and year
viab_model <- stan_model("STAN Models/viab_code.stan")
fit_viab<-sampling(viab_model, data = stan_data_viab,chains=3,
                                                control = list(adapt_delta=0.99,stepsize=0.1),
                                                iter=10000,cores=3,thin=2,
                                                pars = c("u","w",          # plot and year random effects
                                                         "beta0"           #location coefficients
                                                         ),save_warmup=F)


################################################################################
## Reproductive State Model -- Prob of reproducing at next time step   
################################################################################
# ## Run the reproductive Model with a bernoulli distribution ---- fixed effects: previous size ; random effects: plot and year
repro_model <- stan_model("STAN Models/repro_code.stan")
fit_repro<-sampling(repro_model, data = stan_data_repro,chains=3,
                                                control = list(adapt_delta=0.99,stepsize=0.1),
                                                iter=10000,cores=3,thin=2,
                                                pars = c("u","w",          # plot and year random effects
                                                         "beta0","beta1"   #location coefficients
                                                         ),save_warmup=F)


################################################################################
#### Seeds Model -- # Seeds per fruit/flower 
################################################################################
# ## Run the seeds Model with a negative binomial distribution ---- fixed effects: ant partner ;
seed_model <- stan_model("STAN Models/seed_code.stan")
fit_seed<-sampling(seed_model, data = stan_data_seed,chains=3,
                                                control = list(adapt_delta=0.99,stepsize=0.1),
                                                iter=10000,cores=3,thin=2,
                                                pars = c("beta0","phi"   #location coefficients
                                                         ),save_warmup=F)


################################################################################
## Seedling Survival Pre Census Model -- Proportion of seedlings that survive from germination to Census
################################################################################
## Run the precensus plant survival Model with a negative binomial distribution ---- random effects: transect
seed_surv_model <- stan_model("STAN Models/seed_surv_code.stan")
fit_seed_surv<-sampling(seed_surv_model, data = stan_data_seed_surv,chains=3,
                                                control = list(adapt_delta=0.99,stepsize=0.1),
                                                iter=10000,cores=3,thin=2,
                                                pars = c("beta0"   #location coefficients
                                                         ),save_warmup=F)


################################################################################
## Germination Model -- Probability of germinating by the next time step (yr 1&2) 
################################################################################
## Run the precensus plant survival Model with a negative binomial distribution ---- random effects: transect
germ_model <- stan_model("STAN Models/germ_code.stan")
fit_germ<-sampling(germ_model, data = stan_data_germ,chains=3,
                        control = list(adapt_delta=0.99,stepsize=0.1),
                        iter=10000,cores=3,thin=2,
                        pars = c("beta0"   #location coefficients
                        ),save_warmup=F)


################################################################################
## Fruit Survival Model -- Proportion of flowers which survive to produce fruits
################################################################################
## Run the Viability Modelwith a binomial distribution ---- fixed effects: ant partner ; random effects: plot and year
fruit_model <- stan_model("STAN Models/fruit_code.stan")
fit_fruit<-sampling(fruit_model, data = stan_data_fruit,chains=3,
                                                control = list(adapt_delta=0.99,stepsize=0.1),
                                                iter=10000,cores=3,thin=2,
                                                pars = c("beta0"),save_warmup=F)



################################################################################
## Recruits -- Size Distribution of recruits
################################################################################
# ## Run the precensus plant survival Model with a negative binomial distribution ---- random effects: transect
rec_model <- stan_model("STAN Models/rec_code.stan")
fit_rec<-sampling(rec_model, data = stan_data_rec,chains=3,
                        control = list(adapt_delta=0.99,stepsize=0.1),
                        iter=10000,cores=3,thin=2,
                        pars = c("beta0","sigma"   #location coefficients
                        ),save_warmup=F)


################################################################################
## Transition Model -- Probability of each ant state as the next partner
################################################################################
## Run the precensus plant survival Model with a negative binomial distribution ---- random effects: transect
multi_model <- stan_model("STAN Models/multi_mixed.stan")
fit_multi<-sampling(multi_model, data = stan_data_multi,chains=3,
                        control = list(adapt_delta=0.99,stepsize=0.1),
                        iter=10000,cores=3,thin=3,
                        pars = c("beta"   #location coefficients
                        ),save_warmup=F)




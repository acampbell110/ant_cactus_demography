################################################################################ 
################################################################################
## The purpose of this script is to load in the "cleaned" data, subset it 
## properly, run the data through the stan models, and export the outputs as RDS
## files.
################################################################################
################################################################################
## Read the data in
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
source("01_cholla_ant_IPM_setup.R")
cactus <- read.csv("Data Analysis/cholla_demography_20042023_cleaned.csv", header = TRUE,stringsAsFactors=T)
################################################################################
## Growth Model Selection with WAIC
################################################################################
## Make a brms model
# grow_skew <- brm(brmsformula(logsize_t1 ~ logsize_t * ant_t + logsize_t^2 * ant_t,
#                              sigma ~ logsize_t,
#                              alpha ~ logsize_t),
#                  data = growth_data,
#                  family = skew_normal(link = "identity",link_sigma = "log",link_alpha = "identity"),
#                  chains = 3,
#                  iter = 1000,
#                  cores = 3,
#                  thin = 2)
# grow_stu <- brm(brmsformula(logsize_t1 ~ logsize_t * ant_t + logsize_t^2 * ant_t,
#                              sigma ~ logsize_t,
#                              nu ~ logsize_t),
#                  data = growth_data,
#                  family = student(link = "identity",link_sigma = "log",link_nu = "logm1"),
#                  chains = 3,
#                  iter = 1000,
#                  cores = 3,
#                  thin = 2)
# ## Model selection between null and parameterized model
# WAIC(grow_skew,grow_stu)

################################################################################
##   Student T Growth Model -- What size will the cacti be next time step?
################################################################################
## Pull all necessary variables together, remove NAs, and put them into a list so they are ready to feed into the stan model
growth_data_orig <- cactus[,c("Plot","Year_t","logsize_t","logsize_t1","ant_t")]
growth_data <- na.omit(growth_data_orig) # Lose 2032 rows (due to plant death & recruit status)
# nrow(growth_data_orig)
# nrow(growth_data)
# check that you are happy with the subsetting by plotting the original and cleaned data
# plot(growth_data$logsize_t, growth_data$logsize_t1)
# points((cactus$logsize_t), (cactus$logsize_t1), col = "red")
## Make a list of all necessary variables so they are properly formatted to feed into the stan model
stan_data_grow_stud <- list(N = nrow(growth_data),                                     ## number of observations
                            vol = (growth_data$logsize_t),                             ## predictor volume year t
                            vol2 = (growth_data$logsize_t)^2,                          ## non linear volume year t predictor
                            y = (growth_data$logsize_t1),                              ## response volume next year
                            ant = as.integer(as.factor(growth_data$ant_t)),            ## predictor ant state
                            K = 4,                                                     ## number of ant states
                            N_Year = max(as.integer(as.factor(growth_data$Year_t))),   ## number of years
                            N_Plot = max(as.integer(growth_data$Plot)),                ## number of plots
                            plot = as.integer(growth_data$Plot),                       ## predictor plots
                            year = as.integer(as.factor(growth_data$Year_t))           ## predictor years
)
# ## Run the growth model with a student t distribution -- fixed effects: previous size and a non linear previous size variable and ant state; random effects: plot and year; size variation is included for both the omega and alpha estimates
# grow_stud_model <- stan_model("Data Analysis/STAN Models/grow_student_t.stan")
# fit_grow_stud<-sampling(grow_stud_model,data = stan_data_grow_stud,chains=3,
#                        control = list(adapt_delta=0.99,stepsize=0.1),
#                        iter=10000,cores=3,thin=2,
#                        pars = c("u","w",          # plot and year random effects
#                                 "beta0","beta1","beta2", #location coefficients
#                                 "d_0","d_size","d_size2", #scale coefficiences
#                                 "a_0","a_size","a_size2"), #shape coefficients
#                        save_warmup=F)
# ## Save the RDS file which saves all parameters, draws, and other information
# saveRDS(fit_grow_stud, "/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_grow_student_t.rds")
# # grow_stud_model_null <- stan_model("Data Analysis/STAN Models/grow_student_t_null.stan")
# fit_grow_stud_null<-sampling(grow_stud_model_null,data = stan_data_grow_stud,chains=3,
#                        control = list(adapt_delta=0.99,stepsize=0.1),
#                        iter=10000,cores=3,thin=2,
#                        pars = c("u","w",          # plot and year random effects
#                                 "beta0","beta1","beta2", #location coefficients
#                                 "d_0","d_size","d_size2", #scale coefficiences
#                                 "a_0","a_size","a_size2"), #shape coefficients
#                        save_warmup=F)
# ## Save the RDS file which saves all parameters, draws, and other information
# saveRDS(fit_grow_stud_null, "/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_grow_student_t_null.rds")

################################################################################
##   Skew Growth Model -- What size will the cacti be next time step?
################################################################################
## Pull all necessary variables together, remove NAs, and put them into a list so they are ready to feed into the stan model
growth_data_orig <- cactus[,c("Plot","Year_t","logsize_t","logsize_t1","ant_t")]
growth_data <- na.omit(growth_data_orig) # Lose 2032 rows (due to plant death & recruit status)
#check the years and plots we are working with
unique(growth_data$Year_t);table(growth_data$Year_t)
unique(growth_data$Plot);table(growth_data$Plot)

# nrow(growth_data_orig)
# nrow(growth_data)
# check that you are happy with the subsetting by plotting the original and cleaned data
# plot(growth_data$logsize_t, growth_data$logsize_t1)
# points((cactus$logsize_t), (cactus$logsize_t1), col = "red")
## Make a list of all necessary variables so they are properly formatted to feed into the stan model
stan_data_grow_skew <- list(N = nrow(growth_data),                                     ## number of observations
                            vol = (growth_data$logsize_t),                             ## predictor volume year t
                            vol2 = (growth_data$logsize_t)^2,                          ## non linear volume year t predictor
                            y = (growth_data$logsize_t1),                              ## response volume next year
                            ant = as.integer(as.factor(growth_data$ant_t)),            ## predictor ant state
                            K = 4,                                                     ## number of ant states
                            N_Year = max(as.integer(as.factor(growth_data$Year_t))),   ## number of years
                            N_Plot = max(as.integer(growth_data$Plot)),                ## number of plots
                            plot = as.integer(growth_data$Plot),                       ## predictor plots
                            year = as.integer(as.factor(growth_data$Year_t))           ## predictor years
)
# Run the growth model with a student t distribution -- fixed effects: previous size and a non linear previous size variable and ant state; random effects: plot and year; size variation is included for both the omega and alpha estimates
 grow_skew_model <- stan_model("Data Analysis/STAN Models/grow_skew.stan")
 fit_grow_skew<-sampling(grow_skew_model,data = stan_data_grow_skew,chains=3,
                       control = list(adapt_delta=0.99,stepsize=0.1),
                       iter=5000,cores=3,thin=2,
                       pars = c("u","w",          # plot and year random effects
                                "beta0","beta1","beta2", #location coefficients
                                "d_0","d_size", #scale coefficiences
                                "a_0","a_size"), #shape coefficients
                       save_warmup=F)
# 
bayesplot::mcmc_trace(fit_grow_skew,pars = c("beta0[1]","beta0[2]","beta0[3]","beta0[4]"))
bayesplot::mcmc_trace(fit_grow_skew,pars = c("beta1[1]","beta1[2]","beta1[3]","beta1[4]"))
bayesplot::mcmc_trace(fit_grow_skew,pars = c("beta2[1]","beta2[2]","beta2[3]","beta2[4]"))
bayesplot::mcmc_trace(fit_grow_skew,pars = c("d_0","d_size","a_0","a_size"))
# 
# # ## Save the RDS file which saves all parameters, draws, and other information
saveRDS(fit_grow_skew, "H:/Shared drives/Miller Lab/Sevilleta/Cholla/Model Outputs/fit_grow_skew.rds")
# saveRDS(fit_grow_skew, "C:/Users/tm9/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_grow_skew.rds")
# saveRDS(fit_grow_skew,"/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_grow_skew1.rds")
# saveRDS(fit_grow_skew, "C:/Users/LabUser/Documents/GitHub/ant_cactus_demography/fit_grow_skew.rds")

# Run the null growth model with a skew normal distribution -- fixed effects: previous size and a non linear previous size variable and ant state; random effects: plot; size variation is included for both the omega and alpha estimates
# grow_skew_model_null <- stan_model("Data Analysis/STAN Models/grow_skew_null.stan")
# fit_grow_skew_null<-sampling(grow_skew_model_null,data = stan_data_grow_skew,chains=3,
#                         control = list(adapt_delta=0.99,stepsize=0.1),
#                         iter=10000,cores=3,thin=2,
#                         pars = c("u","w",          # plot and year random effects
#                                  "beta0","beta1","beta2", #location coefficients
#                                  "d_0","d_size", #scale coefficiences
#                                  "a_0","a_size"), #shape coefficients
#                         save_warmup=F)
# 
# bayesplot::mcmc_trace(fit_grow_skew_null,pars = c("beta0[1]","beta0[2]","beta0[3]","beta0[4]"))
# bayesplot::mcmc_trace(fit_grow_skew_null,pars = c("beta1[1]","beta1[2]","beta1[3]","beta1[4]"))
# bayesplot::mcmc_trace(fit_grow_skew_null,pars = c("beta2[1]","beta2[2]","beta2[3]","beta2[4]"))
# bayesplot::mcmc_trace(fit_grow_skew_null,pars = c("d_0","d_size","a_0","a_size"))

# # ## Save the RDS file which saves all parameters, draws, and other information
# saveRDS(fit_grow_skew_null, "C:/Users/tm9/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_grow_skew_null.rds")
# saveRDS(fit_grow_skew_null,"/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_grow_skew_null.rds")
# saveRDS(fit_grow_skew_null, "C:/Users/LabUser/Documents/GitHub/ant_cactus_demography/fit_grow_skew_null.rds")

################################################################################
##  Survival Model -- What is the probability of surviving to the next time step?   
################################################################################
## Pull all necessary variables together, remove NAs, and put them into a list so they are ready to feed into the stan model
survival_data_orig <- subset(cactus, is.na(Survival_t1) == FALSE,c("Plot","Year_t","Survival_t1","ant_t","logsize_t"))
survival_data_orig <- cactus[,c("Plot","Year_t","Survival_t1","ant_t","logsize_t")]
survival_data <- na.omit(survival_data_orig)
survival_data <- subset(survival_data, survival_data$Survival_t1 != 2)
# levels(survival_data$ant_t)
# # check how many rows of data your lose: 1619 rows due to recruit status
# nrow(survival_data_orig)
# nrow(survival_data)
## Create Stan Data
stan_data_surv <- list(N = nrow(survival_data),                                    ## number of observations
                       vol = (survival_data$logsize_t),                            ## predictors volume
                       y_surv = (survival_data$Survival_t1),                       ## response survival next year
                       ant = as.integer(as.factor(survival_data$ant_t)),           ## predictors ants
                       K = 4,                                                      ## number of ant states
                       N_Year = max(as.integer(as.factor(survival_data$Year_t))),  ## number of years
                       N_Plot = max(as.integer(as.factor(survival_data$Plot))),    ## number of plots
                       plot = as.integer(as.factor(survival_data$Plot)),           ## predictor plots
                       year = as.integer(as.factor(survival_data$Year_t))          ## predictor years
) 
# ## Run the survival model with a bernoulli distribution ---- fixed effects: previous size and ant state; random effects: plot and year
# surv_model <- stan_model("Data Analysis/STAN Models/surv_code.stan")
# fit_surv<-sampling(surv_model, data = stan_data_surv,chains=3,
#                    control = list(adapt_delta=0.99,stepsize=0.1),
#                    iter=10000,cores=3,thin=2,
#                    pars = c("beta0","beta1","u","w"),   #location coefficients)
#                    save_warmup=F)
# ##Save the RDS file which saves all parameters, draws, and other information
# saveRDS(fit_surv, "/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_surv.rds")
# saveRDS(fit_surv,"/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_surv.rds")
# surv_model_null <- stan_model("Data Analysis/STAN Models/surv_code_null.stan")
# fit_surv_null<-sampling(surv_model_null, data = stan_data_surv,chains=3,
#                    control = list(adapt_delta=0.99,stepsize=0.1),
#                    iter=10000,cores=3,thin=2,
#                    pars = c("beta0","beta1","u","w"),   #location coefficients)
#                    save_warmup=F)
# ##Save the RDS file which saves all parameters, draws, and other information
# saveRDS(fit_surv_null, "/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_surv_null.rds")
# saveRDS(fit_surv_null,"/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_surv_null.rds")


################################################################################
## Flowering Model -- What are the total number of fruits produced in the next time step?
################################################################################
## Pull all necessary variables together, remove NAs, and put them into a list so they are ready to feed into the stan model
flower_data_orig <- cactus[ , c("TotFlowerbuds_t", "logsize_t","Year_t","Plot")]
flower_data_orig <- subset(flower_data_orig, TotFlowerbuds_t > 0)
flower_data <- na.omit(flower_data_orig)
# # Lose 6605 rows of data due to no flower data
# nrow(flower_data_orig)
# nrow(flower_data)
# # check that you're happy with the subsetting
# plot(flower_data$logsize_t, flower_data$TotFlowerbuds_t)
# points(cactus$logsize_t, cactus$TotFlowerbuds_t, col = "red")
## Create Stan Data
stan_data_flow_trunc <- list(N = nrow(flower_data),                                   ## number of observations
                             lower_limit = 1,                                         ## we want the 0s to be removed
                             vol = (flower_data$logsize_t),                           ## predictors volume
                             y_flow = flower_data$TotFlowerbuds_t,                    ## response flowers next year
                             N_Year = max(as.integer(as.factor(flower_data$Year_t))), ## number of years
                             N_Plot = max(as.integer(as.factor(flower_data$Plot))),   ## number of plots
                             plot = as.integer(as.factor(flower_data$Plot)),          ## predictor plots
                             year = as.integer(as.factor(flower_data$Year_t))         ## predictor years
) 
# ## Run the flower model with a negative binomial distribution ---- fixed effects: previous size; random effects: plot and year
# flow_model <- stan_model("Data Analysis/STAN Models/flower_trunc_code.stan")
# fit_flow<-sampling(flow_model, data = stan_data_flow_trunc,chains=3,
#                                                 control = list(adapt_delta=0.99,stepsize=0.1),
#                                                 iter=10000,cores=3,thin=2,
#                                                 pars = c("u","w",          # plot and year random effects
#                                                          "beta0","beta1",  #location coefficients
#                                                          "phi"),save_warmup=F)
# ## Save the RDS file which saves all parameters, draws, and other information
# saveRDS(fit_flow, "/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_flow.rds")
# saveRDS(fit_flow,"/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_flow.rds")


################################################################################
## Viability Model -- What proportion of fruit are viable? 
################################################################################
## Pull all necessary variables together, remove NAs, and put them into a list so they are ready to feed into the stan model
viability_data_orig <- cactus[ , c("TotFlowerbuds_t","Goodbuds_t","ABFlowerbuds_t","ant_t", "logsize_t","Year_t","Plot")]
viability_data_orig <- subset(viability_data_orig, TotFlowerbuds_t > 0)
viability_data <- na.omit(viability_data_orig)

# levels(viability_data$ant_t)
# unique(viability_data_orig$Year_t)
# # Lose Rows of data
# view(viability_data_orig)
# nrow(viability_data)
# # check if you're happy with the subsetting
# plot(viability_data$logsize_t, viability_data$ABFlowerbuds_t1)
# plot(viability_data_orig$logsize_t, viability_data_orig$ABFlowerbuds_t1, col = "red")
## Create stan data subset
stan_data_viab <- list(N = nrow(viability_data),                                   ## number of observations
                       good = viability_data$Goodbuds_t,                          ## number of good flowerbuds 
                       abort = viability_data$ABFlowerbuds_t,                     ## aborted buds data
                       tot = viability_data$TotFlowerbuds_t,                      ## number of trials
                       ant = as.integer(as.factor(viability_data$ant)),            ## predictors ants
                       K = 4,                                                      ## number of ant states
                       N_Year = max(as.integer(as.factor(viability_data$Year_t))), ## number of years
                       N_Plot = max(as.integer(as.factor(viability_data$Plot))),   ## number of plots
                       plot = as.integer(as.factor(viability_data$Plot)),          ## predictor plots
                       year = as.integer(as.factor(viability_data$Year_t))         ## predictor years
) 
# ## Run the Viability Modelwith a binomial distribution ---- fixed effects: ant partner ; random effects: plot and year
# viab_model <- stan_model("Data Analysis/STAN Models/viab_code.stan")
# fit_viab<-sampling(viab_model, data = stan_data_viab,chains=3,
#                                                 control = list(adapt_delta=0.99,stepsize=0.1),
#                                                 iter=10000,cores=3,thin=2,
#                                                 pars = c("u","w",          # plot and year random effects
#                                                          "beta0"           #location coefficients
#                                                          ),save_warmup=F)
# ## Save the RDS file which saves all parameters, draws, and other information
# saveRDS(fit_viab, "/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_viab.rds")
# saveRDS(fit_viab,"/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_viab.rds")
# ## Run the Viability model with a binomial distribution but no ant effects on the random effects of years
# viab_model_null <- stan_model("Data Analysis/STAN Models/viab_code_null.stan")
# fit_viab_null<-sampling(viab_model_null, data = stan_data_viab,chains=3,
#                                                 control = list(adapt_delta=0.99,stepsize=0.1),
#                                                 iter=10000,cores=3,thin=2,
#                                                 pars = c("u","w",          # plot and year random effects
#                                                          "beta0"           #location coefficients
#                                                          ),save_warmup=F)
# ## Save the RDS file which saves all parameters, draws, and other information
# saveRDS(fit_viab_null, "/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_viab_null.rds")
# saveRDS(fit_viab_null,"/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_viab_null.rds")

################################################################################
## Reproductive State Model -- Prob of reproducing at next time step   
################################################################################
## Pull all necessary variables together, remove NAs, and put them into a list so they are ready to feed into the stan model
reproductive_data_orig <- cactus[ , c("flower1_YN","logsize_t","Year_t","Plot")]
reproductive_data <- na.omit(reproductive_data_orig)
# # check that you're happy with the subsetting
# plot(reproductive_data$logsize_t, reproductive_data$flower1_YN)
# points(cactus$logsize_t, cactus$flower1_YN, col = "red")
# # Lose 3332 rows of data because only including
# nrow(reproductive_data_orig)
# nrow(reproductive_data)
## Create Stan Data
stan_data_repro <- list(N = nrow(reproductive_data),                                   ## number of observations
                        vol = reproductive_data$logsize_t,                            ## predictors volume
                        y_repro = reproductive_data$flower1_YN,                        ## response volume next year
                        N_Year = max(as.integer(as.factor(reproductive_data$Year_t))), ## number of years
                        N_Plot = max(as.integer(as.factor(reproductive_data$Plot))),   ## number of plots
                        plot = as.integer(as.factor(reproductive_data$Plot)),          ## predictor plots
                        year = as.integer(as.factor(reproductive_data$Year_t))         ## predictor years
) 
# ## Run the reproductive Model with a bernoulli distribution ---- fixed effects: previous size ; random effects: plot and year
# repro_model <- stan_model("Data Analysis/STAN Models/repro_code.stan")
# fit_repro<-sampling(repro_model, data = stan_data_repro,chains=3,
#                                                 control = list(adapt_delta=0.99,stepsize=0.1),
#                                                 iter=10000,cores=3,thin=2,
#                                                 pars = c("u","w",          # plot and year random effects
#                                                          "beta0","beta1"   #location coefficients
#                                                          ),save_warmup=F)
# ## Save the RDS file which saves all parameters, draws, and other information
# saveRDS(fit_repro, "/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_repro.rds")
# saveRDS(fit_repro,"/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_repro.rds")


################################################################################
#### Seeds Model -- # Seeds per fruit/flower 
################################################################################
## Pull all necessary variables together, remove NAs, and put them into a list so they are ready to feed into the stan model
seed_uncleaned <- read.csv("Data Analysis/JO_fruit_data_final_dropplant0.csv", header = TRUE,stringsAsFactors=T)
# PEAA = Ant Access
# PAAA = Ant Access
# PEAE = Ant Exclusion
# PAAE = Ant Exclusion
seed <- subset(seed_uncleaned, treatment == "PAAA" | treatment == "PAAE")
#make the column for the ant state of the part of the plant producing seeds
for(i in 1:nrow(seed)){
  #If there is no ant access then vacant
  if(seed$ant.access[i] == "n" & is.na(seed$ant.access[i]) == FALSE){
    seed$ant_state[i] <- "Vacant"
  }
  #If there is ant access but it is still vacant then vacant
  if(seed$ant.access[i] == "y" & is.na(seed$ant.access[i]) == FALSE & seed$vacant[i] == "y"){
    seed$ant_state[i] <- "Vacant"
  }
  #if there is ant access and it is not vacant and the ant is crem then crem
  if(seed$ant.access[i] == "y" & is.na(seed$ant.access[i]) == FALSE & seed$vacant[i] == "n" & seed$species[i] == "c"){
    seed$ant_state[i] <- "Crem"
  }
  #if there is ant access and it is not vacant and the ant is liom then liom
  if(seed$ant.access[i] == "y" & is.na(seed$ant.access[i]) == FALSE & seed$vacant[i] == "n" & seed$species[i] == "l"){
    seed$ant_state[i] <- "Liom"
  }
}
seed_data <- seed
seed_data <- na.omit(seed_data)
seed_data$ant <- as.integer(as.factor(seed_data$ant_state))
seed_data <- subset(seed_data, seed_count > 0)
# str(seed_data)
 levels(as.factor(seed_data$ant_state))
# # check if you're happy with the subsetting
# plot(seed$fruit_number)
# points(seed_data$fruit_number, col = "red")
# nrow(seed)
# nrow(seed_data)
## Create Stan Data
stan_data_seed <- list(N = nrow(seed_data),                            ## number of observations
                       K = 3,                                          ## number of ant states
                       ant = as.integer(as.factor(seed_data$ant)),     ## ant partners data
                       seed = seed_data$seed_count)                    ## number of seeds data
# ## Run the seeds Model with a negative binomial distribution ---- fixed effects: ant partner ;
# seed_model <- stan_model("Data Analysis/STAN Models/seed_code.stan")
# fit_seed<-sampling(seed_model, data = stan_data_seed,chains=3,
#                                                 control = list(adapt_delta=0.99,stepsize=0.1),
#                                                 iter=10000,cores=3,thin=2,
#                                                 pars = c("beta0","phi"   #location coefficients
#                                                          ),save_warmup=F)
# ## Save the RDS file which saves all parameters, draws, and other information
# saveRDS(fit_seed, "/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_seed.rds")
# saveRDS(fit_seed,"/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_seed.rds")


################################################################################
## Seedling Survival Pre Census -- Proportion of seedlings that survive from germination to Census
################################################################################
## Pull all necessary variables together, remove NAs, and put them into a list so they are ready to feed into the stan model
precensus.dat.orig<-read.csv("Data Analysis/PrecensusSurvival.csv") 
precensus.dat <- precensus.dat.orig[ , c("Transect","Seed","Log.size","survive0405")]
precensus.dat <- na.omit(precensus.dat)
# # You lose no data here, but it is still a small dataset.
# nrow(precensus.dat)
# nrow(precensus.dat.orig)
# # check that you're happy with the subsetting
# plot(precensus.dat$Log.size, jitter(precensus.dat$survive0405))
# points(precensus.dat.orig$Log.size, jitter(precensus.dat.orig$survive0405), col = "red")
## Create Stan Data
stan_data_seed_surv <- list(N = nrow(precensus.dat),                                  ## number of observations
                            N_Transect = length(unique(precensus.dat$Transect)),      ## Number of transects
                            transect = as.integer(as.factor(precensus.dat$Transect)), ## transect
                            y = precensus.dat$survive0405)                            ## survival data
## Run the precensus plant survival Model with a negative binomial distribution ---- random effects: transect
# seed_surv_model <- stan_model("Data Analysis/STAN Models/seed_surv_code.stan")
# fit_seed_surv<-sampling(seed_surv_model, data = stan_data_seed_surv,chains=3,
#                                                 control = list(adapt_delta=0.99,stepsize=0.1),
#                                                 iter=10000,cores=3,thin=2,
#                                                 pars = c("beta0"   #location coefficients
#                                                          ),save_warmup=F)
# ## Save the RDS file which saves all parameters, draws, and other information
# saveRDS(fit_seed_surv, "/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_seed_surv.rds")
# saveRDS(fit_seed_surv,"/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_seed_surv.rds")

################################################################################
## Germination -- Probability of germinating by the next time step (yr 1&2) 
################################################################################
germ.dat_orig<-read.csv("Data Analysis/Germination.csv") 
germ.dat_orig$rate <- 0
for(i in 1:nrow(germ.dat_orig)){
  if(germ.dat_orig$Seedlings04[i] != 0){
    germ.dat_orig$rate[i] <- (germ.dat_orig$Seedlings04[i] - germ.dat_orig$Seedlings05[i])/germ.dat_orig$Seedlings04[i]
  }
}
germ.dat <- na.omit(germ.dat_orig)
germ.dat <- germ.dat[-c(42,39,40),]
## Most of the rows are included. We only lose 3, but it is still a small dataset. 
# nrow(germ.dat)
# nrow(germ.dat_orig)
# #Check that you are happy with the subsetting
# plot(germ.dat$rate)
# points(germ.dat_orig$rate, col = "red")
# nrow(germ.dat)
## Create Stan Data
stan_data_germ1 <- list(N = nrow(germ.dat),
                        y_germ = as.integer(germ.dat$Seedlings04),
                        trials = germ.dat$Input)
stan_data_germ2 <- list(N = nrow(germ.dat),
                        y_germ = germ.dat$Seedlings05,
                        trials = germ.dat$Input-germ.dat$Seedlings04)
## Run the precensus plant survival Model with a negative binomial distribution ---- random effects: transect
# germ_model <- stan_model("Data Analysis/STAN Models/germ_code.stan")
# fit_germ1<-sampling(germ_model, data = stan_data_germ1,chains=3,
#                         control = list(adapt_delta=0.99,stepsize=0.1),
#                         iter=10000,cores=3,thin=2,
#                         pars = c("beta0"   #location coefficients
#                         ),save_warmup=F)
# fit_germ2<-sampling(germ_model, data = stan_data_germ2,chains=3,
#                         control = list(adapt_delta=0.99,stepsize=0.1),
#                         iter=10000,cores=3,thin=2,
#                         pars = c("beta0"   #location coefficients
#                         ),save_warmup=F)
# ## Save the RDS file which saves all parameters, draws, and other information
# saveRDS(fit_germ1, "/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_germ1.rds")
# saveRDS(fit_germ1,"/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_germ1.rds")
# saveRDS(fit_germ2, "/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_germ2.rds")
# saveRDS(fit_germ2,"/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_germ2.rds")

################################################################################
##.    Fruit Survival Model 
################################################################################
## Pull all necessary variables together, remove NAs, and put them into a list so they are ready to feed into the stan model
fruit_data_orig <- read.csv("Data Analysis/FruitSurvival.csv", header = TRUE,stringsAsFactors=T)
fruit_data <- na.omit(fruit_data_orig)
## Create stan data subset
stan_data_fruit <- list(N = nrow(fruit_data),                                   ## number of observations
                       good = fruit_data$Fr.on.plant,                          ## number of good flowerbuds 
                       abort = fruit_data$Fr.on.grnd.not.chewed,                     ## aborted buds data
                       tot = fruit_data$Fr.on.plant+fruit_data$Fr.on.grnd                      ## number of trials
) 
## Run the Viability Modelwith a binomial distribution ---- fixed effects: ant partner ; random effects: plot and year
# fruit_model <- stan_model("Data Analysis/STAN Models/fruit_code.stan")
# fit_fruit<-sampling(fruit_model, data = stan_data_fruit,chains=3,
#                                                 control = list(adapt_delta=0.99,stepsize=0.1),
#                                                 iter=10000,cores=3,thin=2,
#                                                 pars = c("beta0"),save_warmup=F)
## Save the RDS file which saves all parameters, draws, and other information
#saveRDS(fit_fruit, "/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_fruit.rds")
#saveRDS(fit_fruit,"/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_fruit.rds")


################################################################################
#### Recruits -- Size Distribution of  ################################################################################
################################################################################
seedling.dat_orig <- cactus[,c("logsize_t1","Recruit","Year_t1")]
seedling.dat_orig <- filter(seedling.dat_orig, Recruit == 1)
seedling.dat <- na.omit(seedling.dat_orig)
# nrow(seedling.dat_orig)
# nrow(seedling.dat)
# # check that you are happy with the subsetting
# plot(seedling.dat$logsize_t1, seedling.dat$Recruit, xlim = c(-5,15), ylim = c(0,1))
# points(cactus$logsize_t1, cactus$Recruit, col = "red")
## Create Stan Data
stan_data_rec <- list(N = length(seedling.dat$logsize_t1),
                      y_rec = (seedling.dat$logsize_t1)
)
# ## Run the precensus plant survival Model with a negative binomial distribution ---- random effects: transect
# rec_model <- stan_model("Data Analysis/STAN Models/rec_code.stan")
# fit_rec<-sampling(rec_model, data = stan_data_rec,chains=3,
#                         control = list(adapt_delta=0.99,stepsize=0.1),
#                         iter=10000,cores=3,thin=2,
#                         pars = c("beta0","sigma"   #location coefficients
#                         ),save_warmup=F)
# ## Save the RDS file which saves all parameters, draws, and other information
# saveRDS(fit_rec, "/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_rec.rds")
# saveRDS(fit_rec,"/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_rec.rds")




##############################################################################
###### now include ant and size as predictors
###### Data Analysis/STAN Models/multi_prac_size_ant_Km1.stan
##############################################################################
cactus_real <- cactus[,c("ant_t","ant_t1","logsize_t","Year_t","Plot")]
cactus_real <- na.omit(cactus_real)
cactus_real$ant_t1 <- relevel(cactus_real$ant_t1,ref = "vacant")
cactus_real$ant_t <- relevel(cactus_real$ant_t, ref = "vacant")
cactus_real <- cactus_real[,c("logsize_t", "ant_t", "ant_t1","Year_t","Plot")]

unique(cactus_real$Year_t)
cactus_real$ant_t1 <- factor(cactus_real$ant_t1, levels = c("crem","liom","other","vacant"))
cactus_real$ant_t <- factor(cactus_real$ant_t, levels = c("crem","liom","other","vacant"))
levels(cactus_real$ant_t)
levels(cactus_real$ant_t1)
(stan_data_multi$x)
cactus_real <- subset(cactus_real, cactus_real$Year_t != 2022 & cactus_real$Year_t != 2021)
## make stan data set
stan_data_multi <- list(K = length(unique(cactus_real$ant_t1)), #number of possible ant species
                       N = dim(cactus_real)[1], #number of observations
                       D = 5, #number of predictors
                       P = 16, #number of random effect predictors
                       y = as.integer(as.factor(cactus_real$ant_t1)), #observations
                       x = model.matrix(~ 0 + (as.factor(ant_t)) + logsize_t, cactus_real), #design matrix
                       z = model.matrix(~0 + as.factor(Year_t), cactus_real),
                       N_Year = as.integer(length(unique(cactus_real$Year_t)))
)
## Run the precensus plant survival Model with a negative binomial distribution ---- random effects: transect
#multi_model <- stan_model("Data Analysis/STAN Models/multi_mixed.stan")
# fit_multi<-sampling(multi_model, data = stan_data_multi,chains=3,
#                         control = list(adapt_delta=0.99,stepsize=0.1),
#                         iter=7500,cores=3,thin=3,
#                         pars = c("beta"   #location coefficients
#                         ),save_warmup=F)
## Save the RDS file which saves all parameters, draws, and other information
# saveRDS(fit_multi, "/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_multi.rds")
#saveRDS(fit_multi,"/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_multi.rds")

## Calculate global turnover rate
# transition_rate_tended <- subset(cactus_real, cactus_real$ant_t != "vacant" & cactus_real$ant_t != cactus_real$ant_t1)
# tt_rate <- nrow(transition_rate_tended)/nrow(cactus_real)
# transition_rate <- subset(cactus_real, cactus_real$ant_t != cactus_real$ant_t1)
# t_rate <- nrow(transition_rate)/nrow(cactus_real)
# return_rate <- subset(cactus_real, cactus_real$ant_t == cactus_real$ant_t1)
# r_rate <- nrow(return_rate)/nrow(cactus_real)






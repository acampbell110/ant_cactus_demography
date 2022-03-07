#setwd("C:/Users/tm9/Dropbox/github/ant_cactus_demography")
#######################################################################################################
##
##                  The purpose of this file is to run each vital rate sub model separately
##                          and run the vital rate models as one large model 
##
#######################################################################################################
options(mc.cores = parallel::detectCores())
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
source("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Setup_Script.R")

#######################################################################################################
#### Growth Model ############################################################################
##############################################################################################
## Create Stan Data for all ant states
stan_data_grow <- list(N = nrow(growth_data), ## number of observations
                       y_grow = (growth_data$logsize_t1), ## response volume next year
                       K = 4, ## number of ant states
                       D = 6, ## number of predictors
                       x = model.matrix(~ logsize_t*as.integer(as.factor(ant_t_relevel)) + (1|as.integer(as.factor(growth_data$Year_t))) + (1|as.integer(as.factor(growth_data$Plot))), growth_data)
) 
## Run the Model
fit_grow_all <- stan(file = "STAN Models/grow_mix_ant.stan", data = stan_data_grow, warmup = 500, iter = 1000, chains = 3, cores = 3, thin = 1)
grow_outputs_all <- rstan::extract(fit_grow_all)
grow_yrep_all <- rstan::extract(fit_grow_all, pars = c("y_rep"))$y_rep
write.csv(grow_yrep_all, "grow_yrep_all.csv")
write.csv(grow_outputs_all, "grow_outputs_all.csv")

## Create stan data for one ant state
i = "vacant"
growth_data_one <- subset(growth_data, ant_t == i & ant_t1 == i)
stan_data_grow_one <- list(N_grow = nrow(growth_data_one), ## number of observations
                         vol_grow = (growth_data_one$logsize_t), ## predictors volume
                         y_grow = (growth_data_one$logsize_t1), ## response volume next year
                         N_Year_grow = max(as.integer(as.factor(growth_data_one$Year_t))), ## number of years
                         N_Plot_grow = max(as.integer(as.factor(growth_data_one$Plot))), ## number of plots
                         plot_grow = as.integer(as.factor(growth_data_one$Plot)), ## predictor plots
                         year_grow = as.integer(as.factor(growth_data_one$Year_t)) ## predictor years
) 
fit_grow_vac <- stan(file = "STAN Models/grow_one.stan", data = stan_data_grow_one, warmup = 500, iter = 1000, chains = 3, cores = 3, thin = 1)
grow_outputs_vac <- rstan::extract(fit_grow_vac)
grow_yrep_vac <- rstan::extract(fit_grow_vac, pars = c("y_rep"))$y_rep
write.csv(grow_yrep_vac, "grow_yrep_vac.csv")
write.csv(grow_outputs_vac, "grow_outputs_vac.csv")
## Create stan data for two ant states (occupied and vacant)
i = "liom"
j = "vacant"
growth_data_two <- growth_data[(growth_data$ant_t == i & growth_data$ant_t1 == i | growth_data$ant_t == i & growth_data$ant_t1 == j | growth_data$ant_t == j & growth_data$ant_t1 == j),]
drop_levels(growth_data_two,)
stan_data_grow_two <- list(N_grow = nrow(growth_data_two), ## number of observations
                           vol_grow = (growth_data_two$logsize_t), ## predictors volume
                           y_grow = (growth_data_two$logsize_t1), ## response volume next year
                           ant_grow = as.integer(as.factor(growth_data_two$ant_t)),## predictors ants
                           N_ant = 2, ## number of ant states
                           N_Year_grow = max(as.integer(as.factor(growth_data_two$Year_t))), ## number of years
                           N_Plot_grow = max(as.integer(as.factor(growth_data_two$Plot))), ## number of plots
                           plot_grow = as.integer(as.factor(growth_data_two$Plot)), ## predictor plots
                           year_grow = as.integer(as.factor(growth_data_two$Year_t)) ## predictor years
) 
fit_grow_liom_vac <- stan(file = "STAN Models/grow_mix_ant.stan", data = stan_data_grow_two, warmup = 500, iter = 1000, chains = 3, cores = 3, thin = 1)
grow_outputs_liom_vac <- rstan::extract(fit_grow_liom_vac)
grow_yrep_vac <- rstan::extract(fit_grow_vac, pars = c("y_rep"))$y_rep
write.csv(grow_yrep_liom_vac, "grow_yrep_liom_vac.csv")
write.csv(grow_outputs_liom_vac, "grow_outputs_liom_vac.csv")

## Create stan data for three ant states
i = "liom"
j = "vac"
k = "other"
growth_data_three <- subset(growth_data, (ant_t == i & ant_t1 == i | ant_t == i & ant_t1 == j | ant_t == i & ant_t1 == k | ant_t == j & ant_t1 == i | ant_t == j & ant_t1 == j | ant_t == j & ant_t1 == k | ant_t == k & ant_t1 == i | ant_t == k & ant_t1 == j | ant_t == k & ant_t1 == k))
stan_data_grow_three <- list(N_grow = nrow(growth_data_three), ## number of observations
                           vol_grow = (growth_data_three$logsize_t), ## predictors volume
                           y_grow = (growth_data_three$logsize_t1), ## response volume next year
                           ant_grow = growth_data_three$ant_t,## predictors ants
                           N_ant = 3, ## number of ant states
                           N_Year_grow = max(growth_data_three$Year_t), ## number of years
                           N_Plot_grow = max(as.integer(as.factor(growth_data_three$Plot))), ## number of plots
                           plot_grow = growth_data_three$Plot, ## predictor plots
                           year_grow = growth_data_three$Year_t ## predictor years
) 

#### Survival Model ########################################################################################
########################################################################################################
## Create Stan Data
stan_data_surv_all <- list(N_surv = nrow(survival_data), ## number of observations
                  vol_surv = (survival_data$logsize_t), ## predictors volume
                  y_surv = (survival_data$Survival_t1), ## response survival next year
                  ant_surv = as.integer(as.factor(survival_data$ant_t)),## predictors ants
                  N_ant = 4, ## number of ant states
                  N_Year_surv = max(as.integer(as.factor(survival_data$Year_t))), ## number of years
                  N_Plot_surv = max(as.integer(as.factor(survival_data$Plot))), ## number of plots
                  plot_surv = as.integer(as.factor(survival_data$Plot)), ## predictor plots
                  year_surv = as.integer(as.factor(survival_data$Year_t)) ## predictor years
) 
#Check that you are happy with the subsetting
plot(survival_data$logsize_t, survival_data$Survival_t1)
points(cactus$logsize_t, cactus$Survival_t1, col = "red")
## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/surv_mix_ant.stan")
fit_surv_null <- stan(file = "STAN Models/surv_null.stan", data = stan_data_surv_all, warmup = 10, iter = 100, chains = 3, cores = 3, thin = 1)
fit_surv_all <- stan(file = "STAN Models/surv_mix_ant.stan", data = stan_data_surv_all, warmup = 100, iter = 1000, chains = 3, cores = 3, thin = 1)
surv_outputs_all <- rstan::extract(fit_surv_all, pars = c("beta0","beta1"))
surv_yrep_all <- rstan::extract(fit_surv_all, pars = c("y_rep"))$y_rep
write.csv(surv_outputs_all, "surv_outputs_all.csv")
write.csv(surv_yrep_all, "surv_yrep_all.csv")

bayesplot::mcmc_trace(As.mcmc.list(fit_surv_null, pars=c("beta0", "beta1")))


#### Flowering Model #################################################################################
######################################################################################################
## Create Stan Data
stan_data_flow_trunc_all <- list(N_flower = nrow(flower_data), ## number of observations
                             lower_limit = 1, ## we want the 0s to be removed
                             vol_flower = (flower_data$logsize_t), ## predictors volume
                             y_flow = flower_data$TotFlowerbuds_t, ## response flowers next year
                             N_Year_flower = max(as.integer(as.factor(flower_data$Year_t))), ## number of years
                             N_Plot_flower = max(as.integer(as.factor(flower_data$Plot))), ## number of plots
                             plot_flower = as.integer(as.factor(flower_data$Plot)), ## predictor plots
                             year_flower = as.integer(as.factor(flower_data$Year_t)) ## predictor years
) 
## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/flower_mix_ant.stan")
fit_flow_trunc_all <- stan(file = "STAN Models/flower_mix_ant_trunc.stan", data = stan_data_flow_trunc_all, warmup = 1000, iter = 10000, chains = 3, cores = 3, thin = 1)
flow_outputs_trunc_all <- rstan::extract(fit_flow_trunc_all, pars = c("phi","beta0","beta1","u","w","sigma","sigma_u","sigma_w"))
write.csv(flow_outputs_trunc_all, "flow_outputs_trunc_all.csv")
flow_y_rep_all <- rstan::extract(fit_flow_trunc_all, pars = c("mu"))$mu
write.csv(flow_y_rep_all, "flow_y_rep_all.csv")
flow_phi_all <- rstan::extract(fit_flow_trunc_all, pars = c("phi"))$phi
write.csv(flow_phi_all, "flow_phi_all.csv")

#### Viability Model #################################################################################
#######################################################################################################
## Create Stan Data
stan_data_viab_all <- list(N_viab = nrow(viability_data), ## number of observations
                       good_viab = viability_data$Goodbuds_t1,
                       abort_viab = viability_data$ABFlowerbuds_t1, ## aborted buds data
                       tot_viab = viability_data$TotFlowerbuds_t1, ## number of trials
                       ant_viab = as.integer(as.factor(viability_data$ant)),## predictors ants
                       N_ant = 4, ## number of ant states
                       N_Year_viab = max(as.integer(as.factor(viability_data$Year_t))), ## number of years
                       N_Plot_viab = max(as.integer(as.factor(viability_data$Plot))), ## number of plots
                       plot_viab = as.integer(as.factor(viability_data$Plot)), ## predictor plots
                       year_viab = as.integer(as.factor(viability_data$Year_t)) ## predictor years
) 
# Check that you are happy with the subsetting
## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/viab_mix_ant.stan")
fit_viab_all <- stan(file = "Data Analysis/STAN Models/viab_mix_ant2.stan", data = stan_data_viab_all, warmup = 1000, iter = 10000, chains = 3, cores = 3, thin = 1)
viab_yrep_all <- rstan::extract(fit_viab_all, pars = c("y_rep"))$y_rep
viab_outputs_all <- rstan::extract(fit_viab_all, pars = c("beta0"))
write.csv(viab_outputs_all, "viab_outputs_all.csv")
write.csv(viab_yrep_all, "viab_yrep_all.csv")


 #### Reproductive State Model #######################################################################################
################################################################################################################################
## Create Stan Data
stan_data_repro <- list(N_repro = nrow(reproductive_data), ## number of observations
                  vol1_repro = reproductive_data$volume_t1, ## predictors volume
                  y_repro = reproductive_data$flower1_YN, ## response volume next year
                  N_Year_repro = max(reproductive_data$year), ## number of years
                  N_Plot_repro = max(reproductive_data$Plot), ## number of plots
                  plot_repro = reproductive_data$Plot, ## predictor plots
                  year_repro = reproductive_data$year ## predictor years
) 
# Check that you are happy with the subsetting
plot(stan_data_repro$vol1_repro, stan_data_repro$y_repro)
plot(log(cactus$volume_t1), cactus$flower1_YN)
## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/repro_mix_ant.stan")
fit_repro_mix_ant <- stan(file = "STAN Models/repro_mix_ant.stan", data = stan_data_repro, warmup = 500, iter = 1000, chains = 3, cores = 3, thin = 1)
repro_outputs <- rstan::extract(fit_repro_mix_ant, pars = c("beta0","beta1"))
repro_yrep <- rstan::extract(fit_repro_mix_ant, pars = c("y_rep"))$y_rep
write.csv(repro_outputs, "repro_outputs.csv")


#### Seeds Model (# Seeds per fruit/flower) ##########################################################
###############################################################################################
## Create Stan Data
stan_data_seed <- list(N_obs_seed = nrow(seed_data),
                        N_ant_seed = 3,
                        N_Plant_ID = length(unique(seed_data$plant)),
                        ant_seed = as.integer(as.factor(seed_data$ant)),
                        plant_seed = seed_data$plant_fac,
                        seed = seed_data$seed_count)
fit_seed <- stan(file = "STAN Models/seed_prod.stan", data = stan_data_seed, warmup = 500, iter = 1000, chains = 3, cores = 3, thin = 1)
seed_outputs <- rstan::extract(fit_seed, pars = c("beta0","phi","sigma_v","v","sigma"))
write.csv(seed_outputs, "seed_outputs.csv")
seed_yrep <- rstan::extract(fit_seed, pars = c("y_rep"))$y_rep

#### Seed Survival Pre Census #####################################################################################################################################
## Create Stan Data
stan_data_seed_surv <- list(N_ss = nrow(precensus.dat),
                            N_Plant_ss = length(unique(precensus.dat$plant.ID)),
                            N_Transect_ss = length(unique(precensus.dat$Transect)),
                            vol_ss = precensus.dat$Log.size,
                            plant_ss = as.integer(as.factor(precensus.dat$plant.ID)),
                            transect_ss = as.integer(as.factor(precensus.dat$Transect)),
                            y_ss = precensus.dat$survive0405)
fit_seed_surv <- stan(file = "STAN Models/seed_survival.stan", data = stan_data_seed_surv, warmup = 500, iter = 1000, chains = 3, cores = 3, thin = 1)
seed_surv_outputs <- rstan::extract(fit_seed_surv, pars = c("beta0","beta1","sigma_u","u","sigma_w","w"))
write.csv(seed_surv_outputs, "seed_surv_outputs.csv")
seed_surv_yrep <- rstan::extract(fit_seed_surv, pars = c("y_rep"))$y_rep

#### Number Fruits Per Plant ######################################################################
###################################################################################################
## Create Stan Data
stan_data_fruit <- list()
fit_fruit <- stan(file = "STAN Models/fruit.stan", data = stan_data_fruit, warmup = 500, iter = 1000, chains = 3, cores = 3, thin = 1)
fruit_outputs <- rstan::extract(fit_fruit, pars = c("beta0","beta1","sigma_u","u","sigma_w","w"))
write.csv(fruit_outputs, "fruit_outputs.csv")
fruit_yrep <- rstan::extract(fit_fruit, pars = c("y_rep"))$y_rep

#### Fruit Survival ######################################################################################
#####################################################################################################
## Create Stan Data
Fr.on.grnd.not.chewed/Fr.on.plant
stan_fruit_surv <- list(on_ground = fruit.surv$Fr.on.grnd.not.chewed,
                        on_plant = fruit.surv$Fr.on.plant,
                        fr_prop = fruit.surv$Fr.on.grnd.not.chewed/fruit.surv$Fr.on.plant,
                        N_fruit = nrow(fruit.surv),
                        N_Transect = 3,
                        N_Plant = 8,
                        transect = as.integer(factor(fruit.surv$Transect)),
                        plant = as.integer(factor(fruit.surv$Plant))
                        
)
fit_fruit_surv <- stan(file = "STAN Models/fruit_surv.stan", data = stan_fruit_surv, warmup = 500, iter = 1000, chains = 3, cores = 3, thin = 1)
fruit_surv_outputs <- rstan::extract(fit_fruit_surv, pars = c("beta0"))
write.csv(fruit_surv_outputs, "fruit_surv_outputs.csv")
fruit_surv_yrep <- rstan::extract(fit_fruit_surv, pars = c("y_rep"))$y_rep

#### Germination ################################################################################################
######################################################################################################
y_germ1 <- germ.dat$Seedlings04
stan_data_germ1 <- list(N_germ = nrow(germ.dat),
                       y_germ1 = as.integer(germ.dat$Seedlings04),
                       trials_germ1 = germ.dat$Input)
stan_data_germ2 <- list(N_germ = nrow(germ.dat),
                       y_germ2 = germ.dat$Seedlings05,
                       trials_germ2 = germ.dat$Input-germ.dat$Seedlings04)
## Create Stan Data
fit_germ1 <- stan(file = "STAN Models/germ_yr1.stan", data = stan_data_germ1, warmup = 500, iter = 1000, chains = 3, cores = 3, thin = 1)
fit_germ2 <- stan(file = "STAN Models/germ_yr2.stan", data = stan_data_germ2, warmup = 500, iter = 1000, chains = 3, cores = 3, thin = 1)
germ1_outputs <- rstan::extract(fit_germ1, pars = c("beta0","beta1"))
germ2_outputs <- rstan::extract(fit_germ2, pars = c("beta0","beta1"))
write.csv(germ1_outputs, "germ1_outputs.csv")
write.csv(germ2_outputs, "germ2_outputs.csv")
germ1_yrep <- rstan::extract(fit_germ1, pars = c("y_rep"))$y_rep
germ2_yrep <- rstan::extract(fit_germ2, pars = c("y_rep"))$y_rep

#### Recruits #######################################################################################3
####################################################################################################
## Create Stan Data
stan_data_rec <- list(N_rec = length(seedling.dat$volume_t),
                      y_rec = log(seedling.dat$volume_t)
)
fit_rec <- stan(file = "STAN Models/rec.stan",data = stan_data_rec, warmup = 500, iter = 1000, chains = 3, cores = 3, thin = 1)
rec_outputs <- rstan::extract(fit_rec, pars = c("beta0"))
write.csv(rec_outputs,"rec_outputs.csv")
rec_yrep <- rstan::extract(fit_rec,pars = c("y_rep"))$y_rep


#### Binomial transitions between vacant and ant ####################################################
########################################################################################################
## Create Stan Data
stan_data_binom <- list(N_binom = nrow(binom_ant), ## number of observations
                        vol_binom = binom_ant$logsize_t, ## predictors volume
                        y_binom = as.integer(as.factor(binom_ant$occ_t1))-1, ## response survival next year
                        ant_binom = as.integer(as.factor(binom_ant$occ_t)), ## ant predictors
                        N_ant = 2 ## number of ant states
                        )

fit_binom <- stan(file = "STAN Models/binom_ant.stan", data = stan_data_binom, warmup = 100, iter = 1000, chains = 3, cores = 3, thin = 1)

binom_outputs <- rstan::extract(fit_binom, pars = c("beta0","beta1"))
traceplot(fit_binom, pars = c("beta0"))

summary(fit_binom)
y_occ_sub <- binom_ant[binom_ant$occ_t == "occ",]
y_vac_sub <- binom_ant[binom_ant$occ_t == "vac",]
#Size Dummies for every ant
size_occ = seq(min(y_occ_sub$logsize_t, na.rm = TRUE), max (y_occ_sub$logsize_t, na.rm = TRUE), by = 0.1)
size_vac = seq(min(y_vac_sub$logsize_t, na.rm = TRUE), max (y_vac_sub$logsize_t, na.rm = TRUE), by = 0.1)
## Formulas
y_occ = quantile(binom_outputs$beta0[,1],0.5) + size_occ * quantile(binom_outputs$beta1[,1],0.5)
y_occ_low = quantile(binom_outputs$beta0[,1],0.05) + size_occ * quantile(binom_outputs$beta1[,1],0.05)
y_occ_high = quantile(binom_outputs$beta0[,1],0.95) + size_occ * quantile(binom_outputs$beta1[,1],0.95)
other_extr = quantile(surv_data$beta0.3,0.5) + size_dummy * quantile(surv_data$beta1.3,0.5)
y_vac = quantile(binom_outputs$beta0[,2],0.5) + size_vac * quantile(binom_outputs$beta1[,2],0.5)
y_vac_low = quantile(binom_outputs$beta0[,2],0.05) + size_vac * quantile(binom_outputs$beta1[,2],0.05)
y_vac_high = quantile(binom_outputs$beta0[,2],0.95) + size_vac * quantile(binom_outputs$beta1[,2],0.95)
vac_extr = quantile(surv_data$beta0.4,0.5) + size_dummy * quantile(surv_data$beta1.4,0.5)
y_vac_subset_surv <- subset(y_subset, ant == 4)

plot(x = size_occ  ,y = invlogit(y_occ), type = "l", col = "black", lwd = 4, ylim = c(0,1), xlim = c(-5,15))
points(x = y_occ_sub$logsize_t, y = y_occ_sub$occ_t1, col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.4))
lines(x = size_vac , y = invlogit(y_vac), type = "l", col = "red", lwd = 4, ylim = c(0,1), xlim = c(-1,15))

############################################################3333#######################################
########################################## The Full Model (All ant states) ###########################################################
stan_data <- list(
  #### Growth Variables
  N_grow = nrow(growth_data), ## number of observations
  vol_grow = log(growth_data$volume_t), ## predictors volume
  y_grow = log(growth_data$volume_t1), ## response volume next year
  ant_grow = growth_data$ant,## predictors ants
  N_ant = 4, ## number of ant states
  N_Year_grow = max(growth_data$year), ## number of years
  N_Plot_grow = max(growth_data$Plot), ## number of plots
  plot_grow = growth_data$Plot, ## predictor plots
  year_grow = growth_data$year, ## predictor years
  #### Survival Variables
  N_surv = nrow(survival_data), ## number of observations
  vol_surv = log(survival_data$volume_t), ## predictors volume
  y_surv = survival_data$Survival_t1, ## response survival next year
  ant_surv = survival_data$ant,## predictors ants
  N_ant = 4, ## number of ant states
  N_Year_surv = max(survival_data$year), ## number of years
  N_Plot_surv = max(survival_data$Plot), ## number of plots
  plot_surv = survival_data$Plot, ## predictor plots
  year_surv = survival_data$year, ## predictor years
  #### Flowering Variables
  N_flower = nrow(flower_data), ## number of observations
  lower_limit = 1, ## we want the 0s to be removed
  vol_flower = log(flower_data$volume_t), ## predictors volume
  y_flow = flower_data$TotFlowerbuds_t, ## response flowers next year
  N_Year_flower = max(flower_data$year), ## number of years
  N_Plot_flower = max(flower_data$Plot), ## number of plots
  plot_flower = flower_data$Plot, ## predictor plots
  year_flower = flower_data$year, ## predictor years
  #### Viability Variables
  N_viab = nrow(viability_data), ## number of observations
  good_viab = viability_data$Goodbuds_t1,
  abort_viab = viability_data$ABFlowerbuds_t1, ## aborted buds data
  tot_viab = viability_data$TotFlowerbuds_t1, ## number of trials
  ant_viab = viability_data$ant,## predictors ants
  N_ant = 4, ## number of ant states
  N_Year_viab = max(viability_data$year), ## number of years
  N_Plot_viab = max(viability_data$Plot), ## number of plots
  plot_viab = viability_data$Plot, ## predictor plots
  year_viab = viability_data$year, ## predictor years
  #### Reproductive State Variables
  N_repro = nrow(reproductive_data), ## number of observations
  vol1_repro = reproductive_data$volume_t1, ## predictors volume
  y_repro = reproductive_data$flower1_YN, ## response volume next year
  N_Year_repro = max(reproductive_data$year), ## number of years
  N_Plot_repro = max(reproductive_data$Plot), ## number of plots
  plot_repro = reproductive_data$Plot, ## predictor plots
  year_repro = reproductive_data$year, ## predictor years
  #### Seed Prod Variables
  N_seed = nrow(seed_data),
  N_ant_seed = 3,
  ant_seed = seed_data$ant,
  seed = seed_data$seed_count,
  #### Seed Surv Variables
  on_ground = fruit.surv$Fr.on.grnd.not.chewed,
  on_plant = fruit.surv$Fr.on.plant,
  fr_prop = fruit.surv$Fr.on.grnd.not.chewed/fruit.surv$Fr.on.plant,
  N_seed_s = nrow(fruit.surv),
  #### Germ 1 Variables
  N_germ = nrow(germ.dat),
  y_germ1 = as.integer(germ.dat$Seedlings04),
  trials_germ1 = germ.dat$Input,
  #### Germ 2 Variables
  N_germ = nrow(germ.dat),
  y_germ2 = germ.dat$Seedlings05,
  trials_germ2 = germ.dat$Input-germ.dat$Seedlings04,
  #### Precensus Surv Variables
  N_precen = nrow(precensus.dat),
  N_Plant_precen = length(unique(precensus.dat$plant.ID)),
  N_Transect_precen = length(unique(precensus.dat$Transect)),
  vol_precen = precensus.dat$Log.size,
  plant_precen = as.integer(as.factor(precensus.dat$plant.ID)),
  transect_precen = as.integer(as.factor(precensus.dat$Transect)),
  y_precen = precensus.dat$survive0405,
  #### Recruit Variables
  N_rec = length(seedlings$volume_t),
  y_rec = log(seedlings$volume_t)
)

fit_full <- stan(file = "STAN Models/full_vitals.stan", data = stan_data, warmup = 500, iter = 1000, chains = 3, cores = 2, thin = 1)
full_outputs <- rstan::extract(fit_full, pars = c("beta0_g","beta1_g","u_g","w_g","sigma_g","sigma_u_g","sigma_w_g", 
                                                  "beta0_s","beta1_s","u_s","w_s","sigma_s","sigma_u_s","sigma_w_s",
                                                  "beta0_f","beta1_f","u_f","w_f","sigma_f","sigma_u_f","sigma_w_f", "phi_f",
                                                  "beta0_r","beta1_r","u_r","w_r","sigma_r","sigma_u_r","sigma_w_r",
                                                  "beta0_v","u_v","w_v","sigma_v","sigma_u_v","sigma_w_v",
                                                  "beta0_seed","sigma_seed","phi_seed",
                                                  "beta0_seed_s","sigma_seed_s",
                                                  "beta0_germ1","beta1_germ1",
                                                  "beta0_germ2","beta1_germ2",
                                                  "beta0_precen","beta1_precen","sigma_precen",
                                                  "beta0_rec","sigma_rec"
                                                  )
                               )
write.csv(full_outputs, "params_outputs.csv")

#####################################################################################################
########################################## The Full Model Occuopied or Vacant Only ###########################################################
cactus$ant_t1_relevel <- relevel(cactus$ant_t1,ref = "vacant")
cactus$occ_t <- "occupied"
cactus$occ_t[cactus$ant_t == "vacant"] <- "vacant"
cactus$occ_t1 <- "occupied"
cactus$occ_t1[cactus$ant_t1 == "vacant"] <- "vacant"
growth_data <- cactus[ ,c("Plot","Year_t","Survival_t1","occ_t","occ_t1","volume_t","volume_t1","flower1_YN")]
growth_data <- na.omit(growth_data)
growth_data$ant <- as.integer(as.factor(growth_data$occ_t))
growth_data$ant1 <- as.integer(as.factor(growth_data$occ_t1))
growth_data$Year_t <- as.factor(growth_data$Year_t)
growth_data$year <- as.integer(growth_data$Year_t)
growth_data$Plot <- as.factor(growth_data$Plot)
growth_data$plot <- as.integer(growth_data$Plot)
## Flower Data Set (Total)
flower_data <- cactus[ , c("TotFlowerbuds_t", "volume_t","Year_t","Plot")]
flower_data <- na.omit(flower_data)
flower_data$Year_t <- as.factor(flower_data$Year_t)
flower_data$year <- as.integer(flower_data$Year_t)
flower_data$Plot <- as.factor(flower_data$Plot)
flower_data$plot <- as.integer(flower_data$Plot)
flower_data <- subset(flower_data, TotFlowerbuds_t > 0)
## Repro Data Set
reproductive_data <- cactus[ , c("flower1_YN","volume_t","Year_t","Plot", "volume_t1")]
reproductive_data <- na.omit(reproductive_data)
reproductive_data$Year_t <- as.factor(reproductive_data$Year_t)
reproductive_data$year <- as.integer(reproductive_data$Year_t)
reproductive_data$Plot <- as.factor(reproductive_data$Plot)
reproductive_data$plot <- as.integer(reproductive_data$Plot)
## Viability Data Set
viability_data <- cactus[ , c("TotFlowerbuds_t1","Goodbuds_t1","ABFlowerbuds_t1","occ_t", "volume_t","Year_t","Plot")]
viability_data <- na.omit(viability_data)
viability_data <- subset(viability_data, TotFlowerbuds_t1 > 0)
viability_data$ant <- as.integer(as.factor(viability_data$occ_t))
viability_data$Year_t <- as.factor(viability_data$Year_t)
viability_data$year <- as.integer(viability_data$Year_t)
viability_data$Plot <- as.factor(viability_data$Plot)
viability_data$plot <- as.integer(viability_data$Plot)
## Survival Data Set
survival_data <- cactus[ , c("Plot","Year_t","Survival_t1","occ_t","volume_t")]
survival_data <- na.omit(survival_data)
survival_data$ant <- as.integer(as.factor(survival_data$occ_t))
survival_data$Year_t <- as.factor(survival_data$Year_t)
survival_data$year <- as.integer(survival_data$Year_t)
survival_data$Plot <- as.factor(survival_data$Plot)
survival_data$plot <- as.integer(survival_data$Plot)
## Seed Data Set
seed_data <- seed
seed_data$occ <- "occupied"
seed_data$occ[seed_data$ant_state == "Vacant"] <- "vacant"
seed_data <- na.omit(seed_data)
seed_data$ant <- as.integer(as.factor(seed_data$occ))
seed_data$plant_fac <- as.integer(as.factor(seed_data$plant))
seed_data <- subset(seed_data, seed_count > 0)
### Fruit Surv
fruit.surv<-read.csv("FruitSurvival.csv",header = TRUE,stringsAsFactors=T) %>% drop_na()
fruit.surv <- fruit.surv[which(fruit.surv$Fr.on.grnd.not.chewed > 0),]
### Germ Data
germ.dat<-read.csv("Germination.csv") 
germ.dat <- na.omit(germ.dat)
germ.dat$rate <- 0
for(i in 1:nrow(germ.dat)){
  if(germ.dat$Seedlings04[i] != 0){
    germ.dat$rate[i] <- (germ.dat$Seedlings04[i] - germ.dat$Seedlings05[i])/germ.dat$Seedlings04[i]
  }
}
germ.dat[-c(42,39,40),]

seedlings <- cactus %>% 
  mutate(volume_t = log(volume(h = Height_t, w = Width_t, p = Perp_t)),
         standvol_t = (volume_t - mean(volume_t,na.rm=T))/sd(volume_t,na.rm=T)) %>% 
  filter(str_sub(Plot,1,1)=="H",
         Recruit==1)

seedling.dat <- cactus %>% filter(str_sub(Plot,1,1)=="H",
                                  Recruit==1)
cholla.dat$N_sdlgsize <- length(seedlings$standvol_t)
cholla.dat$y_sdlgsize <- seedlings$standvol_t

### Pre-Census Surv
precensus.dat<-read.csv("PrecensusSurvival.csv") 
precensus.dat%>%  drop_na(precensus.dat$survive0405)
seedlings <- cactus %>% 
  mutate(vol_t = log(volume(h = Height_t, w = Width_t, p = Perp_t)),
         standvol_t = (vol_t - mean(vol_t,na.rm=T))/sd(vol_t,na.rm=T)) %>% 
  filter(str_sub(Plot,1,1)=="H",
         Recruit==1)
## Name local data variables to input to Stan Data
# volume data
vol_grow = log(growth_data$volume_t)
vol_flower = log(flower_data$volume_t)
vol_surv = log(survival_data$volume_t)
vol_repro = log(reproductive_data$volume_t)
vol_viab = log(viability_data$volume_t)
vol1_repro <- log(reproductive_data$volume_t1)
# flower bud est
good_viab <- viability_data$Goodbuds_t1
viability_data$prop_viab <- NA
for(i in 1:nrow(viability_data)){
  if(viability_data$TotFlowerbuds_t1[i] == 0 & viability_data$Goodbuds_t1[i] == 0){viability_data$prop_viab[i] <- 0}
  else{viability_data$prop_viab[i] <- viability_data$Goodbuds_t1[i]/viability_data$TotFlowerbuds_t1[i]}
}
prop_viab <- viability_data$prop_viab
abort_viab <- viability_data$ABFlowerbuds_t1
tot_viab <- viability_data$TotFlowerbuds_t1
# outcome predictors
y_repro = reproductive_data$flower1_YN
y_grow = log(growth_data$volume_t1)
y_surv = survival_data$Survival_t1
y_flow <- flower_data$TotFlowerbuds_t
# num of obs
N_grow = nrow(growth_data)
N_flower = nrow(flower_data)
N_surv = nrow(survival_data)
N_viab = nrow(viability_data)
N_repro = nrow(reproductive_data)
N_ant = 2
N_Year_grow <- max(growth_data$year)
N_Plot_grow <- max(growth_data$plot)
N_Year_surv <- max(survival_data$year)
N_Plot_surv <- max(survival_data$plot)
N_Year_viab <- max(viability_data$year)
N_Plot_viab <- max(viability_data$plot)
N_Year_repro <- max(reproductive_data$year)
N_Plot_repro <- max(reproductive_data$plot)
N_Year_flower <- max(flower_data$year)
N_Plot_flower <- max(flower_data$plot)
# ant data
ant_grow = growth_data$ant
ant_surv = survival_data$ant
ant_viab = viability_data$ant
# random effects
plot_grow <- growth_data$plot
year_grow <- growth_data$year
plot_flower = flower_data$plot
year_flower = flower_data$year
plot_surv = survival_data$plot
year_surv = survival_data$year
plot_viab <- viability_data$plot
year_viab <- viability_data$year
plot_repro <- reproductive_data$plot
year_repro <- reproductive_data$year


stan_data <- list(
  #### Growth Variables
  N_grow = N_grow, ## number of observations
  vol_grow = vol_grow, ## predictors volume
  y_grow = y_grow, ## response volume next year
  ant_grow = ant_grow,## predictors ants
  N_ant = N_ant, ## number of ant states
  N_Year_grow = N_Year_grow, ## number of years
  N_Plot_grow = N_Plot_grow, ## number of plots
  plot_grow = plot_grow, ## predictor plots
  year_grow = year_grow, ## predictor years
  #### Survival Variables
  N_surv = N_surv, ## number of observations
  vol_surv = vol_surv, ## predictors volume
  y_surv = y_surv, ## response volume next year
  ant_surv = ant_surv,## predictors ants
  N_ant = N_ant, ## number of ant states
  N_Year_surv = N_Year_surv, ## number of years
  N_Plot_surv = N_Plot_surv, ## number of plots
  plot_surv = plot_surv, ## predictor plots
  year_surv = year_surv, ## predictor years
  #### Flowering Variables
  N_flower = N_flower, ## number of observations
  lower_limit = 1, ## we want the 0s to be removed
  vol_flower = vol_flower, ## predictors volume
  y_flow = y_flow, ## response volume next year
  N_Year_flower = N_Year_flower, ## number of years
  N_Plot_flower = N_Plot_flower, ## number of plots
  plot_flower = plot_flower, ## predictor plots
  year_flower = year_flower, ## predictor years
  #### Viability Variables
  N_viab = N_viab, ## number of observations
  good_viab = good_viab,
  abort_viab = abort_viab, ## aborted buds data
  tot_viab = tot_viab, ## number of trials
  ant_viab = ant_viab,## predictors ants
  N_ant = N_ant, ## number of ant states
  N_Year_viab = N_Year_viab, ## number of years
  N_Plot_viab = N_Plot_viab, ## number of plots
  plot_viab = plot_viab, ## predictor plots
  year_viab = year_viab, ## predictor years
  #### Reproductive State Variables
  N_repro = N_repro, ## number of observations
  vol1_repro = vol1_repro, ## predictors volume
  y_repro = y_repro, ## response volume next year
  N_Year_repro = N_Year_repro, ## number of years
  N_Plot_repro = N_Plot_repro, ## number of plots
  plot_repro = plot_repro, ## predictor plots
  year_repro = year_repro, ## predictor years
  #### Seed Prod Variables
  N_seed = nrow(seed_data),
  N_ant_seed = 3,
  ant_seed = seed_data$ant,
  seed = seed_data$seed_count,
  #### Seed Surv Variables
  on_ground = fruit.surv$Fr.on.grnd.not.chewed,
  on_plant = fruit.surv$Fr.on.plant,
  fr_prop = fruit.surv$Fr.on.grnd.not.chewed/fruit.surv$Fr.on.plant,
  N_seed_s = nrow(fruit.surv),
  #### Germ 1 Variables
  N_germ = nrow(germ.dat),
  y_germ1 = as.integer(germ.dat$Seedlings04),
  trials_germ1 = germ.dat$Input,
  #### Germ 2 Variables
  N_germ = nrow(germ.dat),
  y_germ2 = germ.dat$Seedlings05,
  trials_germ2 = germ.dat$Input-germ.dat$Seedlings04,
  #### Precensus Surv Variables
  N_precen = nrow(precensus.dat),
  N_Plant_precen = length(unique(precensus.dat$plant.ID)),
  N_Transect_precen = length(unique(precensus.dat$Transect)),
  vol_precen = precensus.dat$Log.size,
  plant_precen = as.integer(as.factor(precensus.dat$plant.ID)),
  transect_precen = as.integer(as.factor(precensus.dat$Transect)),
  y_precen = precensus.dat$survive0405,
  #### Recruit Variables
  N_rec = length(seedlings$volume_t),
  y_rec = log(seedlings$volume_t)
)

fit_full <- stan(file = "STAN Models/full_vitals.stan", data = stan_data, warmup = 500, iter = 1000, chains = 3, cores = 2, thin = 1)
full_outputs <- rstan::extract(fit_full, pars = c("beta0_g","beta1_g","u_g","w_g","sigma_g","sigma_u_g","sigma_w_g", 
                                                  "beta0_s","beta1_s","u_s","w_s","sigma_s","sigma_u_s","sigma_w_s",
                                                  "beta0_f","beta1_f","u_f","w_f","sigma_f","sigma_u_f","sigma_w_f", "phi_f",
                                                  "beta0_r","beta1_r","u_r","w_r","sigma_r","sigma_u_r","sigma_w_r",
                                                  "beta0_v","u_v","w_v","sigma_v","sigma_u_v","sigma_w_v",
                                                  "beta0_seed","sigma_seed","phi_seed",
                                                  "beta0_seed_s","sigma_seed_s",
                                                  "beta0_germ1","beta1_germ1",
                                                  "beta0_germ2","beta1_germ2",
                                                  "beta0_precen","beta1_precen","sigma_precen",
                                                  "beta0_rec","sigma_rec"
)
)
write.csv(full_outputs, "params_outputs_occ.csv")

######################################################################################################
#### Multinomial Model Ant State ###############################################################################
######################################################################################################
ants_na <- cactus[,c("ant_t1","logsize_t","ant_t")]
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
size_ant_dat <- list(K = 4, # number of possible outcomes
                      N = (dim(ants_stan)[1]), # number of observations
                      D = 5, # number of predictors
                      y = as.integer(as.factor(ants_stan$ant_t1)), # observations
                      x = x) # design matrix

## Getting no parameters error so here are different ways to run the code to try and fix this
fit_size_ant2 <- stan("STAN Models/size_ant_model.stan", 
                      data = size_ant_dat, warmup = 100, iter = 1000, chains = 3, algorithm="Fixed_param")

fit_size_ant_summary2 <- summary(fit_size_ant2, par="beta", probs=.5)$summary %>% as.data.frame
size_ant2 <- rstan::extract(fit_size_ant2, pars = c("beta"))
write.csv(size_ant2,"size_ant_outputs2.csv")

bayesplot::mcmc_trace(fit_size_ant2, pars = c("beta[1,1]"))


########################################################
#### Dummy Model Multinomial 
########################################################
generate_mnl_data <- function(N=1000, C=3, beta=c(-3), alpha = c(1)){
  K <- length(beta)
  Y <- rep(NA, N)
  X <- list(NULL) 
  for (i in 1:N) {
    X[[i]] <- matrix(rnorm(C*K), ncol=K) # normal covariates  
    Y[i] <- sample(x=C, size=1, prob=exp(X[[i]]%*%beta + alpha)) # logit formula
  }
  list(N=N, C=C, K=K, Y=Y, X=X)
}
d1 <- generate_mnl_data(N=1000, C=3, beta=c(1), alpha = c(1))
str(d1)
head(d1$X)


generate_hmnl_data <- function(R=10, S=30, C=3, 
                               Theta=matrix(rep(1, 8), nrow=2), 
                               Sigma=diag(0.1, 4)){
  K <- ncol(Theta)
  G <- nrow(Theta)
  Y <- array(dim=c(R, S))
  X <- array(rnorm(R*S*C*K), dim=c(R, S, C, K)) # normal covariates
  Z <- array(dim=c(G, R))
  Z[1,] <- 1  # intercept
  if (G > 1) {
    Z[2:G,] <- rnorm(R*(G-1)) # normal covariates
  }
  Beta <- array(dim=c(K, R))
  for (r in 1:R) {
    Beta[,r] <- mvrnorm(n=1, mu=Z[,r]%*%Theta, Sigma=Sigma)
    for (s in 1:S)
      Y[r,s] <- sample(x=C, size=1, prob=exp(X[r,s,,]%*%Beta[,r])) # logit formula
  }
  list(R=R, S=S, C=C, K=K, G=G, Y=Y, X=X, Z=Z, 
       beta.true=beta, Theta.true=Theta, Sigma.true=Sigma)
}

d1 <- generate_hmnl_data()
str(d1)
head(d1)

######################################################################################################
#### Multinomial Model Ant State (no size) ###############################################################################
######################################################################################################
ants_na <- cactus[,c("ant_t1","ant_t")]
ants_na <- na.omit(ants_na)
ants_stan <- slice_sample(ants_na, n = 1000)
ants_stan$ant_t <- (as.factor(ants_stan$ant_t))
ants_stan$ant_t <- relevel(factor(ants_stan$ant_t),ref="vacant")
ants_stan$ant_t1 <- relevel(factor(ants_stan$ant_t1),ref="vacant")
x = as.matrix(x,ants_stan$ant_t)
ants_stan$ant_t <- relevel(factor(ants_stan$ant_t),ref="vacant")
ants_stan$ant_t1 <- relevel(factor(ants_stan$ant_t1),ref="vacant")
## Make each prev ant species have its own column
crem <- as.integer(ants_stan$ant_t == "crem")
vac <- as.integer(ants_stan$ant_t == "vacant")
liom <- as.integer(ants_stan$ant_t == "liom")
other <- as.integer(ants_stan$ant_t == "other")
ants_stan$ant_t1 <- relevel(factor(ants_stan$ant_t1),ref="vacant")
x = as.matrix(x,crem)
x = cbind(x,liom)
x = cbind(x,other)
x = cbind(x, vac)
ant_dat <- list(K = 4, # number of possible outcomes
                     N = (dim(ants_stan)[1]), # number of observations
                     D = 8, # number of predictors
                     y = as.integer(as.factor(ants_stan$ant_t1)), # observations
                     x = x) # design matrix

## Getting no parameters error so here are different ways to run the code to try and fix this
fit_ant2 <- stan("Data Analysis/STAN Models/size_ant_model.stan", 
                      data = ant_dat, warmup = 100, iter = 1000, chains = 3, algorithm="Fixed_param")

fit_ant_summary2 <- summary(fit_ant2, par="beta", probs=.5)$summary %>% as.data.frame
ant2 <- rstan::extract(fit_ant2, pars = c("beta"))
write.csv(ant2,"ant_outputs2.csv")

multi_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/size_ant_outputs2.csv", header = TRUE,stringsAsFactors=T)
data = multi_data
multi_params = function(data,i,j){
  vol_l = exp(data$beta.1.2)/(1 + exp(data$beta.1.1) + exp(data$beta.1.2) + exp(data$beta.1.3) + exp(data$beta.1.4))
  vol_o = exp(data$beta.1.3)/(1 + exp(data$beta.1.1) + exp(data$beta.1.2) + exp(data$beta.1.3) + exp(data$beta.1.4))
  vol_v = 1 - (vol_c+vol_l+vol_o)
  
  c_c = exp(data$beta.2.1)/(1 + exp(data$beta.2.1) + exp(data$beta.2.2) + exp(data$beta.2.3) + exp(data$beta.2.4))
  c_l = exp(data$beta.2.2)/(1 + exp(data$beta.2.1) + exp(data$beta.2.2) + exp(data$beta.2.3) + exp(data$beta.2.4))
  c_o = exp(data$beta.2.3)/(1 + exp(data$beta.2.1) + exp(data$beta.2.2) + exp(data$beta.2.3) + exp(data$beta.2.4))
  c_v = 1 - (c_c+c_l+c_o)
  
  l_c = exp(data$beta.3.1)/(1 + exp(data$beta.3.1) + exp(data$beta.3.2) + exp(data$beta.3.3) + exp(data$beta.3.4))
  l_l = exp(data$beta.3.2)/(1 + exp(data$beta.3.1) + exp(data$beta.3.2) + exp(data$beta.3.3) + exp(data$beta.3.4))
  l_o = exp(data$beta.3.3)/(1 + exp(data$beta.3.1) + exp(data$beta.3.2) + exp(data$beta.3.3) + exp(data$beta.3.4))
  l_v = 1 - (l_c+l_l+l_o)
  
  o_c = exp(data$beta.4.1)/(1 + exp(data$beta.4.1) + exp(data$beta.4.2) + exp(data$beta.4.3) + exp(data$beta.4.4))
  o_l = exp(data$beta.4.2)/(1 + exp(data$beta.4.1) + exp(data$beta.4.2) + exp(data$beta.4.3) + exp(data$beta.4.4))
  o_o = exp(data$beta.4.3)/(1 + exp(data$beta.4.1) + exp(data$beta.4.2) + exp(data$beta.4.3) + exp(data$beta.4.4))
  o_v = 1 - (o_c+o_l+o_o)
  
  v_c = exp(data$beta.5.1)/(1 + exp(data$beta.5.1) + exp(data$beta.5.2) + exp(data$beta.5.3) + exp(data$beta.5.4))
  v_l = exp(data$beta.5.2)/(1 + exp(data$beta.5.1) + exp(data$beta.5.2) + exp(data$beta.5.3) + exp(data$beta.5.4))
  v_o = exp(data$beta.5.3)/(1 + exp(data$beta.5.1) + exp(data$beta.5.2) + exp(data$beta.5.3) + exp(data$beta.5.4))
  v_v = 1 - (v_c+v_l+v_o)
  
  if(i == "crem" & j == "crem"){return(list(vol_c=vol_c, c_c=c_c))}
  if(i == "crem" & j == "liom"){return(list(vol_c=vol_c, c_l=c_l))}
  if(i == "crem" & j == "other"){return(list(vol_c=vol_c, c_o=c_o))}
  if(i == "crem" & j == "vac"){return(list(vol_c=vol_c, c_v=c_v))}
  
  if(i == "liom" & j == "crem"){return(list(vol_l=vol_l, l_c=l_c))}
  if(i == "liom" & j == "liom"){return(list(vol_l=vol_l, l_l=l_l))}
  if(i == "liom" & j == "other"){return(list(vol_l=vol_l, l_o=l_o))}
  if(i == "liom" & j == "vac"){return(list(vol_l=vol_l, l_v=l_v))}
  
  if(i == "other" & j == "crem"){return(list(vol_o=vol_o, o_c=o_c))}
  if(i == "other" & j == "liom"){return(list(vol_o=vol_o, o_l=o_l))}
  if(i == "other" & j == "other"){return(list(vol_o=vol_o, o_o=o_o))}
  if(i == "other" & j == "vac"){return(list(vol_o=vol_o, o_v=o_v))}
  
  if(i == "vac" & j == "crem"){return(list(vol_v=vol_v, v_c=v_c))}
  if(i == "vac" & j == "liom"){return(list(vol_v=vol_v, v_l=v_l))}
  if(i == "vac" & j == "other"){return(list(vol_v=vol_v, v_o=v_o))}
  if(i == "vac" & j == "vac"){return(list(vol_v=vol_v, v_v=v_v))}
}


crem<-list()
liom<-list()
other<-list()
vac<-list()
probs<-list()

for(i in 1:nrow(data)){
  crem[[i]]<-c(multi_params(data[i,],"crem","crem")$c_c,multi_params(data[i,],"crem","liom")$c_l,multi_params(data[i,],"crem","other")$c_o,multi_params(data[i,],"crem","vac")$c_v)
  liom[[i]] <- c(multi_params(data[i,],"liom","crem")$l_c,multi_params(data[i,],"liom","liom")$l_l,multi_params(data[i,],"liom","other")$l_o,multi_params(data[i,],"liom","vac")$l_v)
  other[[i]] <- c(multi_params(data[i,],"other","crem")$o_c,multi_params(data[i,],"other","liom")$o_l,multi_params(data[i,],"other","other")$o_o,multi_params(data[i,],"other","vac")$o_v)
  vac[[i]] <- c(multi_params(data[i,],"vac","crem")$v_c,multi_params(data[i,],"vac","liom")$v_l,multi_params(data[i,],"vac","other")$v_o,multi_params(data[i,],"vac","vac")$v_v)
  
  probs[[i]] <- as.matrix(crem[[i]])
  probs[[i]] <- cbind(probs[[i]],liom[[i]])
  probs[[i]] <- cbind(probs[[i]],other[[i]])
  probs[[i]] <- cbind(probs[[i]],vac[[i]])
  colnames(probs[[i]]) <- c("crem","liom","other","vac")
  rownames(probs[[i]]) <- c("crem","liom","other","vac")
}


options(mc.cores = parallel::detectCores())
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Data Analysis")
#setwd("/Users/alicampbell")
# import the data
cactus <- read.csv("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Data Analysis/cholla_demography_20042019_cleaned.csv", header = TRUE,stringsAsFactors=T)
#### Create local data frames to call in the Growth STAN models
cactus$ant_t1_relevel <- relevel(cactus$ant_t1,ref = "vacant")
growth_data <- cactus[ ,c("Plot","Year_t","Survival_t1","ant_t","ant_t1","volume_t","volume_t1","flower1_YN")]
growth_data <- na.omit(growth_data)
growth_data$ant <- as.integer(growth_data$ant_t)
growth_data$ant1 <- as.integer(growth_data$ant_t1)
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
viability_data <- cactus[ , c("TotFlowerbuds_t1","Goodbuds_t1","ABFlowerbuds_t1","ant_t", "volume_t","Year_t","Plot")]
viability_data <- na.omit(viability_data)
viability_data <- subset(viability_data, TotFlowerbuds_t1 > 0)
viability_data$ant <- as.integer(viability_data$ant_t)
viability_data$Year_t <- as.factor(viability_data$Year_t)
viability_data$year <- as.integer(viability_data$Year_t)
viability_data$Plot <- as.factor(viability_data$Plot)
viability_data$plot <- as.integer(viability_data$Plot)
## Survival Data Set
survival_data <- cactus[ , c("Plot","Year_t","Survival_t1","ant_t","volume_t")]
survival_data <- na.omit(survival_data)
survival_data$ant <- as.integer(survival_data$ant_t)
survival_data$Year_t <- as.factor(survival_data$Year_t)
survival_data$year <- as.integer(survival_data$Year_t)
survival_data$Plot <- as.factor(survival_data$Plot)
survival_data$plot <- as.integer(survival_data$Plot)
## Seed Data Set
seed_data <- seed
seed_data <- na.omit(seed_data)
seed_data$ant <- as.integer(as.factor(seed_data$ant_state))
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
N_ant = 4
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

#### Growth Model
## Create Stan Data
stan_data_grow <- list(N_grow = N_grow, ## number of observations
                  vol_grow = vol_grow, ## predictors volume
                  y_grow = y_grow, ## response volume next year
                  ant_grow = ant_grow,## predictors ants
                  N_ant = N_ant, ## number of ant states
                  N_Year_grow = N_Year_grow, ## number of years
                  N_Plot_grow = N_Plot_grow, ## number of plots
                  plot_grow = plot_grow, ## predictor plots
                  year_grow = year_grow ## predictor years
) 
#Check that you are happy with the subsetting
plot(stan_data_grow$vol_grow, stan_data_grow$y_grow)
plot(log(cactus$volume_t), log(cactus$volume_t1))
## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/grow_mix_ant.stan")
fit_grow_mix_ant <- stan(file = "STAN Models/grow_mix_ant.stan", data = stan_data_grow, warmup = 500, iter = 1000, chains = 3, cores = 3, thin = 1)
grow_outputs <- rstan::extract(fit_grow_mix_ant, pars = c("beta0","beta1","sigma"))
grow_yrep <- rstan::extract(fit_grow_mix_ant, pars = c("y_rep"))$y_rep
write.csv(grow_outputs, "grow_outputs.csv")

#### Survival Model
## Create Stan Data
stan_data_surv <- list(N_surv = N_surv, ## number of observations
                  vol_surv = vol_surv, ## predictors volume
                  y_surv = y_surv, ## response volume next year
                  ant_surv = ant_surv,## predictors ants
                  N_ant = N_ant, ## number of ant states
                  N_Year_surv = N_Year_surv, ## number of years
                  N_Plot_surv = N_Plot_surv, ## number of plots
                  plot_surv = plot_surv, ## predictor plots
                  year_surv = year_surv ## predictor years
) 
#Check that you are happy with the subsetting
plot(stan_data_surv$vol_surv,stan_data_surv$y_surv)
plot(log(cactus$volume_t), cactus$Survival_t1)
## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/surv_mix_ant.stan")
fit_surv_mix_ant <- stan(file = "STAN Models/surv_mix_ant.stan", data = stan_data_surv, warmup = 500, iter = 1000, chains = 3, cores = 3, thin = 1)
surv_outputs <- rstan::extract(fit_surv_mix_ant, pars = c("beta0","beta1"))
surv_yrep <- rstan::extract(fit_surv_mix_ant, pars = c("y_rep"))$y_rep
write.csv(surv_outputs, "surv_outputs.csv")


#### Flowering Model
## Create Stan Data
stan_data_flow <- list(N_flower = N_flower, ## number of observations
                       lower_limit = 1, ## we want the 0s to be removed
                  vol_flower = vol_flower, ## predictors volume
                  y_flow = y_flow, ## response volume next year
                  N_Year_flower = N_Year_flower, ## number of years
                  N_Plot_flower = N_Plot_flower, ## number of plots
                  plot_flower = plot_flower, ## predictor plots
                  year_flower = year_flower ## predictor years
) 
# Check that you are happy witht he subsetting
plot(stan_data_flow$vol_flower, stan_data_flow$y_flow)
points(log(flower_data$volume_t), flower_data$TotFlowerbuds_t, col = "red")
plot(log(cactus$volume_t), cactus$TotFlowerbuds_t)
## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/flower_mix_ant.stan")
fit_flow_mix_ant <- stan(file = "STAN Models/flower_mix_ant.stan", data = stan_data_flow, warmup = 500, iter = 1000, chains = 3, cores = 3, thin = 1)
flow_yrep <- rstan::extract(fit_flow_mix_ant, pars = c("y_rep"))$y_rep
flow_outputs <- rstan::extract(fit_flow_mix_ant, pars = c("phi","beta0","beta1","u","w","sigma","sigma_u","sigma_w"))
write.csv(flow_outputs, "flow_outputs.csv")

stan_data_flow_trunc <- list(N_flower = N_flower, ## number of observations
                       lower_limit = 1, ## we want the 0s to be removed
                       vol_flower = vol_flower, ## predictors volume
                       y_flow = y_flow, ## response volume next year
                       N_Year_flower = N_Year_flower, ## number of years
                       N_Plot_flower = N_Plot_flower, ## number of plots
                       plot_flower = plot_flower, ## predictor plots
                       year_flower = year_flower ## predictor years
) 
## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/flower_mix_ant.stan")
fit_flow_mix_ant_trunc <- stan(file = "STAN Models/flower_mix_ant_trunc.stan", data = stan_data_flow_trunc, warmup = 500, iter = 1000, chains = 3, cores = 3, thin = 1)
flow_pred <- rstan::extract(fit_flow_mix_ant_trunc, pars = c("mu"))$mu
phi_flow <- rstan::extract(fit_flow_mix_ant_trunc, pars = c("phi"))$phi
flow_outputs_trunc <- rstan::extract(fit_flow_mix_ant_trunc, pars = c("phi","beta0","beta1","u","w","sigma","sigma_u","sigma_w"))
write.csv(flow_outputs_trunc, "flow_outputs_trunc.csv")


#### Viability Model
## Create Stan Data
stan_data_viab <- list(N_viab = N_viab, ## number of observations
                       good_viab = good_viab,
                       abort_viab = abort_viab, ## aborted buds data
                       tot_viab = tot_viab, ## number of trials
                       ant_viab = ant_viab,## predictors ants
                       N_ant = N_ant, ## number of ant states
                       N_Year_viab = N_Year_viab, ## number of years
                       N_Plot_viab = N_Plot_viab, ## number of plots
                       plot_viab = plot_viab, ## predictor plots
                       year_viab = year_viab ## predictor years
) 
# Check that you are happy with the subsetting
## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/viab_mix_ant.stan")
fit_viab_mix_ant <- stan(file = "STAN Models/viab_mix_ant2.stan", data = stan_data_viab, warmup = 500, iter = 1000, chains = 3, cores = 3, thin = 1)
viab_yrep <- rstan::extract(fit_viab_mix_ant, pars = c("y_rep"))$y_rep
viab_outputs <- rstan::extract(fit_viab_mix_ant, pars = c("beta0"))
write.csv(viab_outputs, "viab_outputs.csv")


 #### Reproductive State Model
## Create Stan Data
stan_data_repro <- list(N_repro = N_repro, ## number of observations
                  vol1_repro = vol1_repro, ## predictors volume
                  y_repro = y_repro, ## response volume next year
                  N_Year_repro = N_Year_repro, ## number of years
                  N_Plot_repro = N_Plot_repro, ## number of plots
                  plot_repro = plot_repro, ## predictor plots
                  year_repro = year_repro ## predictor years
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


#### Seeds Model (# Seeds per fruit)
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

#### Seed Survival Pre Census
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

#### Number Fruits Per Plant
## Create Stan Data
stan_data_fruit <- list()
fit_fruit <- stan(file = "STAN Models/fruit.stan", data = stan_data_fruit, warmup = 500, iter = 1000, chains = 3, cores = 3, thin = 1)
fruit_outputs <- rstan::extract(fit_fruit, pars = c("beta0","beta1","sigma_u","u","sigma_w","w"))
write.csv(fruit_outputs, "fruit_outputs.csv")
fruit_yrep <- rstan::extract(fit_fruit, pars = c("y_rep"))$y_rep

#### Fruit Survival
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

#### Germination 
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

#### Recruits

stan_data_rec <- list(N_rec = length(seedling.dat$volume_t),
                      y_rec = log(seedling.dat$volume_t)
)
fit_rec <- stan(file = "STAN Models/rec.stan",data = stan_data_rec, warmup = 500, iter = 1000, chains = 3, cores = 3, thin = 1)
rec_outputs <- rstan::extract(fit_rec, pars = c("beta0"))
write.csv(rec_outputs,"rec_outputs.csv")
rec_yrep <- rstan::extract(fit_rec,pars = c("y_rep"))$y_rep
########################################## The Full Model ###########################################################
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
write.csv(full_outputs, "params_outputs.csv")




options(mc.cores = parallel::detectCores())
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Data Analysis")
#setwd("/Users/alicampbell")
# import the data
cactus <- read.csv("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Data Analysis/cholla_demography_20042019_cleaned.csv", header = TRUE,stringsAsFactors=T)
#### Create local data frames to call in the Growth STAN models
cactus$ant_t1_relevel <- relevel(cactus$ant_t1,ref = "vacant")
grow_data <- cactus[ ,c("Plot","Year_t","Survival_t1","ant_t","ant_t1","volume_t","volume_t1","flower1_YN")]
grow_data <- na.omit(grow_data)
grow_data$ant <- as.integer(grow_data$ant_t)
grow_data$ant1 <- as.integer(grow_data$ant_t1)
grow_data$Year_t <- as.factor(grow_data$Year_t)
grow_data$year <- as.integer(grow_data$Year_t)
grow_data$Plot <- as.factor(grow_data$Plot)
grow_data$plot <- as.integer(grow_data$Plot)
## Flower Data Set (Total)
flower_data <- cactus[ , c("TotFlowerbuds_t1", "volume_t","Year_t","Plot")]
flower_data <- na.omit(flower_data)
flower_data$Year_t <- as.factor(flower_data$Year_t)
flower_data$year <- as.integer(flower_data$Year_t)
flower_data$Plot <- as.factor(flower_data$Plot)
flower_data$plot <- as.integer(flower_data$Plot)
## Repro Data Set
repro_data <- cactus[ , c("flower1_YN","volume_t","Year_t","Plot", "volume_t1")]
repro_data <- na.omit(repro_data)
repro_data$Year_t <- as.factor(repro_data$Year_t)
repro_data$year <- as.integer(repro_data$Year_t)
repro_data$Plot <- as.factor(repro_data$Plot)
repro_data$plot <- as.integer(repro_data$Plot)
## Viability Data Set
viability_data <- cactus[ , c("TotFlowerbuds_t1","Goodbuds_t1","ABFlowerbuds_t1","ant_t", "volume_t","Year_t","Plot")]
viability_data <- na.omit(viability_data)
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
## Name local data variables to input to Stan Data
# volume data
vol_grow = log(grow_data$volume_t)
vol_flower = log(flower_data$volume_t)
vol_surv = log(survival_data$volume_t)
vol_repro = log(repro_data$volume_t)
vol_viab = log(viability_data$volume_t)
vol1_repro <- log(repro_data$volume_t1)
# flower bud est
good_viab <- viability_data$Goodbuds_t
abort_viab <- viability_data$ABFlowerbuds_t
tot_viab <- viability_data$TotFlowerbuds_t
# outcome predictors
y_repro = repro_data$flower1_YN
y_grow = log(grow_data$volume_t1)
y_surv = survival_data$Survival_t1
y_flow <- flower_data$TotFlowerbuds_t
# num of obs
N_grow = nrow(grow_data)
N_flower = nrow(flower_data)
N_surv = nrow(survival_data)
N_viab = nrow(viability_data)
N_repro = nrow(repro_data)
N_ant = 4
N_Year_grow <- max(grow_data$year)
N_Plot_grow <- max(grow_data$plot)
N_Year_surv <- max(survival_data$year)
N_Plot_surv <- max(survival_data$plot)
N_Year_viab <- max(viability_data$year)
N_Plot_viab <- max(viability_data$plot)
N_Year_repro <- max(repro_data$year)
N_Plot_repro <- max(repro_data$plot)
N_Year_flower <- max(flower_data$year)
N_Plot_flower <- max(flower_data$plot)
# ant data
ant_grow = grow_data$ant
ant_surv = survival_data$ant
ant_viab = viability_data$ant
# random effects
plot_grow <- grow_data$plot
year_grow <- grow_data$year
plot_flower = flower_data$plot
year_flower = flower_data$year
plot_surv = survival_data$plot
year_surv = survival_data$year
plot_viab <- viability_data$plot
year_viab <- viability_data$year
plot_repro <- repro_data$plot
year_repro <- repro_data$year

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
fit_grow_mix_ant <- stan(file = "STAN Models/grow_mix_ant.stan", data = stan_data_grow, warmup = 5000, iter = 10000, chains = 3, cores = 2, thin = 1)
grow_outputs <- rstan::extract(fit_grow_mix_ant, pars = c("beta0","beta1","y_rep"))
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
fit_surv_mix_ant <- stan(file = "STAN Models/surv_mix_ant.stan", data = stan_data_surv, warmup = 500, iter = 1000, chains = 3, cores = 2, thin = 1)
surv_outputs <- rstan::extract(fit_surv_mix_ant, pars = c("beta0","beta1","y_rep"))
write.csv(surv_outputs, "surv_outputs.csv")


#### Flowering Model
## Create Stan Data
stan_data_flow <- list(N_flower = N_flower, ## number of observations
                  vol_flower = vol_flower, ## predictors volume
                  y_flow = y_flow, ## response volume next year
                  ant_flower = ant_flower,## predictors ants
                  N_ant = N_ant, ## number of ant states
                  N_Year_flower = N_Year_flower, ## number of years
                  N_Plot_flower = N_Plot_flower, ## number of plots
                  plot_flower = plot_flower, ## predictor plots
                  year_flower = year_flower ## predictor years
) 
# Check that you are happy witht he subsetting
plot(stan_data_flow$vol_flower, stan_data_flow$y_flow)
plot(log(cactus$volume_t), cactus$TotFlowerbuds_t)
## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/flower_mix_ant.stan")
fit_flow_mix_ant <- stan(file = "STAN Models/flower_mix_ant.stan", data = stan_data_flow, warmup = 500, iter = 1000, chains = 3, cores = 2, thin = 1)
flow_outputs <- rstan::extract(fit_flow_mix_ant, pars = c("beta0","beta1","y_rep"))
write.csv(flow_outputs, "flow_outputs.csv")


#### Viability Model
## Create Stan Data
stan_data_viab <- list(N_viab = N_viab, ## number of observations
                       vol_viab = vol_viab, ## predictors volume
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
plot(stan_data_viab$vol_viab, stan_data_viab$good_viab)
plot(log(cactus$volume_t), cactus$Goodbuds_t)
## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/viab_mix_ant.stan")
fit_viab_mix_ant <- stan(file = "STAN Models/viab_mix_ant.stan", data = stan_data_viab, warmup = 500, iter = 1000, chains = 3, cores = 2, thin = 1)
viab_outputs <- rstan::extract(fit_viab_mix_ant, pars = c("beta0","beta1","y_rep"))
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
fit_repro_mix_ant <- stan(file = "STAN Models/repro_mix_ant.stan", data = stan_data_repro, warmup = 500, iter = 1000, chains = 3, cores = 2, thin = 1)
repro_outputs <- rstan::extract(fit_repro_mix_ant, pars = c("beta0","beta1","y_rep"))
write.csv(repro_outputs, "repro_outputs.csv")


#### Multinomial Model 1
## Create Stan Data
stan_data_multi1 <- list(N_data = N_data, ## number of observations
                  N_ant = N_ant, ## number of ants
                  vol_data = vol_data, ## Volume in year t
                  ant1_data = ant1_data, ## ant state in year t1
                  ant_data = ant_data,
                  N_Year = N_Year,
                  N_Plot = N_Plot,
                  year_data = year_data,
                  plot_data = plot_data
)
## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/multinomial1.stan")
fit_multi1_mix_ant <- stan(file = "STAN Models/multinomial1.stan", data = stan_data_multi1, warmup = 5?


#### Multinomial Model 2
## Create Stan Data
stan_data_multi2 <- list(N_data = N_data, ## number of observations
                         N_ant = N_ant, ## number of ants
                         vol_data = vol_data, ## Volume in year t
                         ant1_data = ant1_data, ## ant state in year t1
                         N_Year = N_Year,
                         N_Plot = N_Plot,
                         year_data = year_data,
                         plot_data = plot_data,
                         ant_data = ant_data ## ant state in year t
)
## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/multinomial2.stan")
fit_multi2_mix_ant <- stan(file = "STAN Models/multinomial2.stan", data = stan_data_multi2, warmup = 5, iter = 10, chains = 1, cores = 2, thin = 1)


#### Multinomail Model 3
## Create Stan Data
stan_data_multi3 <- list(N_data = N_data, ## number of observations
                         N_ant = N_ant, ## number of ants
                         vol_data = vol_data, ## Volume in year t
                         ant1_data = ant1_data, ## ant state in year t1
                         N_Year = N_Year,
                         N_Plot = N_Plot,
                         year_data = year_data,
                         plot_data = plot_data,
                         ant_data = ant_data ## ant state in year t
)
## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/multinomial3.stan")
fit_multi3_mix_ant <- stan(file = "STAN Models/multinomial3.stan", data = stan_data_multi3, warmup = 5, iter = 10, chains = 1, cores = 2, thin = 1)




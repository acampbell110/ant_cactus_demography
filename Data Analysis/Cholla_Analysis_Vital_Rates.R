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
fit_grow_mix_ant <- stan(file = "STAN Models/grow_mix_ant.stan", data = stan_data_grow, warmup = 500, iter = 1000, chains = 3, cores = 2, thin = 1)
grow_outputs <- rstan::extract(fit_grow_mix_ant, pars = c("beta0","beta1"))
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
fit_surv_mix_ant <- stan(file = "STAN Models/surv_mix_ant.stan", data = stan_data_surv, warmup = 500, iter = 1000, chains = 3, cores = 2, thin = 1)
surv_outputs <- rstan::extract(fit_surv_mix_ant, pars = c("beta0","beta1","y_rep"))
write.csv(surv_outputs, "surv_outputs.csv")


#### Flowering Model
## Create Stan Data
stan_data_flow <- list(N_flower = N_flower, ## number of observations
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
fit_flow_mix_ant <- stan(file = "STAN Models/flower_mix_ant.stan", data = stan_data_flow, warmup = 500, iter = 1000, chains = 3, cores = 2, thin = 1)
flow_yrep <- rstan::extract(fit_flow_mix_ant, pars = c("y_rep"))$y_rep
flow_outputs <- rstan::extract(fit_flow_mix_ant, pars = c("beta0","beta1"))
write.csv(flow_outputs, "flow_outputs.csv")


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
fit_viab_mix_ant <- stan(file = "STAN Models/viab_mix_ant2.stan", data = stan_data_viab, warmup = 500, iter = 1000, chains = 3, cores = 2, thin = 1)
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




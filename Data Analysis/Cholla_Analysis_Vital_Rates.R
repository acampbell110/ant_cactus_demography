options(mc.cores = parallel::detectCores())
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Data Analysis")

# import the data
cactus <- read.csv("cholla_demography_20042019_cleaned.csv", header = TRUE,stringsAsFactors=T)
#### Create local data frames to call in the STAN models
cactus$ant_t1_relevel <- relevel(cactus$ant_t1,ref = "vacant")
data <- cactus[ ,c("Plot","Year_t","Survival_t1","ant_t","ant_t1","volume_t","volume_t1","repro_state_t1", "ant_t1_relevel")]
data <- na.omit(data)
data$ant <- as.integer(data$ant_t)
data$ant1 <- as.integer(data$ant_t1_relevel)
## add columns for each ant classification so they can be binary
data$crem <- 0 ##2
data$liom <- 0 ##3
data$other <- 0 ##1
data$vacant <- 0 ##4
for(i in 1:nrow(data)){
  if(data$ant[i] == 1){data$other[i] <- 1}
  if(data$ant[i] == 2){data$crem[i] <- 1}
  if(data$ant[i] == 3){data$liom[i] <- 1}
  if(data$ant[i] == 4){data$vacant[i] <- 1}
}
data$Year_t <- as.factor(data$Year_t)
data$year <- as.integer(data$Year_t)
data$Plot <- as.factor(data$Plot)
data$plot <- as.integer(data$Plot)
## Flower Data Set
flower <- cactus[ , c("TotFlowerbuds_t","ABFlowerbuds_t","Goodbuds_t","ant_t", "repro_state_t1","volume_t","volume_t1","Year_t","Plot")]
flower <- na.omit(flower)
flower$ant <- as.integer(flower$ant_t)
flower$Year_t <- as.factor(flower$Year_t)
flower$year <- as.integer(flower$Year_t)
flower$Plot <- as.factor(flower$Plot)
flower$plot <- as.integer(flower$Plot)

## Name local data variables to input to Stan Data
vol_data = log(data$volume_t)
vol_flower = log(flower$volume_t)
vol1_flower <- flower$volume_t1
good <- flower$Goodbuds_t
abort <- flower$ABFlowerbuds_t
tot <- flower$TotFlowerbuds_t
y_repro = flower$repro_state_t1
y_grow = log(data$volume_t1)
y_surv = data$Survival_t1
y_flow <- flower$TotFlowerbuds_t
N_data = nrow(data)
N_flower = nrow(flower)
N_ant = 4
ant_data = data$ant
ant_flower = flower$ant
ant1_data = data$ant1
N_Year <- max(data$year)
N_Plot <- max(data$plot)
plot_data <- data$plot
year_data <- data$year
plot_flower = flower$plot
year_flower = flower$year

#### Groth Model
## Create Stan Data
stan_data_grow <- list(N_data = N_data, ## number of observations
                  vol_data = vol_data, ## predictors volume
                  y_grow = y_grow, ## response volume next year
                  ant_data = ant_data,## predictors ants
                  N_ant = N_ant, ## number of ant states
                  N_Year = N_Year, ## number of years
                  N_Plot = N_Plot, ## number of plots
                  plot_data = plot_data, ## predictor plots
                  year_data = year_data ## predictor years
) 
## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/grow_mix_ant.stan")
fit_grow_mix_ant <- stan(file = "STAN Models/grow_mix_ant.stan", data = stan_data_grow, warmup = 5000, iter = 10000, chains = 3, cores = 2, thin = 1)
posterior_grow_mix_ant <- as.data.frame(fit_grow_mix_ant)
extract_grow <- extract(fit_grow_mix_ant)

#### Survival Model
## Create Stan Data
stan_data_surv <- list(N_data = N_data, ## number of observations
                  vol_data = vol_data, ## predictors volume
                  y_surv = y_surv, ## response volume next year
                  ant_data = ant_data,## predictors ants
                  N_ant = N_ant, ## number of ant states
                  N_Year = N_Year, ## number of years
                  N_Plot = N_Plot, ## number of plots
                  plot_data = plot_data, ## predictor plots
                  year_data = year_data ## predictor years
) 
## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/surv_mix_ant.stan")
fit_surv_mix_ant <- stan(file = "STAN Models/surv_mix_ant.stan", data = stan_data_surv, warmup = 5, iter = 10, chains = 1, cores = 2, thin = 1)
posterior_surv_mix_ant <- as.data.frame(fit_surv_mix_ant)

#### Flowering Model
## Create Stan Data
stan_data_flow <- list(N_flower = N_flower, ## number of observations
                  vol_flower = vol_flower, ## predictors volume
                  y_flow = y_flow, ## response volume next year
                  ant_flower = ant_flower,## predictors ants
                  N_ant = N_ant, ## number of ant states
                  N_Year = N_Year, ## number of years
                  N_Plot = N_Plot, ## number of plots
                  plot_flower = plot_flower, ## predictor plots
                  year_flower = year_flower ## predictor years
) 
## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/flower_mix_ant.stan")
fit_flow_mix_ant <- stan(file = "STAN Models/flower_mix_ant.stan", data = stan_data_flow, warmup = 5000, iter = 10000, chains = 1, cores = 2, thin = 1)
posterior_flow_mix_ant <- as.data.frame(fit_flow_mix_ant)

#### Viability Model
## Create Stan Data
stan_data_viab <- list(N_flower = N_flower, ## number of observations
                       vol_flower = vol_flower, ## predictors volume
                       good = good, ## good buds = successes
                       abort = abort, ## aborted buds data
                       tot = tot, ## number of trials
                       ant_flower = ant_flower,## predictors ants
                       N_ant = N_ant, ## number of ant states
                       N_Year = N_Year, ## number of years
                       N_Plot = N_Plot, ## number of plots
                       plot_flower = plot_flower, ## predictor plots
                       year_flower = year_flower ## predictor years
) 
## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/viab_mix_ant.stan")
fit_viab_mix_ant <- stan(file = "STAN Models/viab_mix_ant.stan", data = stan_data_viab, warmup = 5000, iter = 10000, chains = 1, cores = 2, thin = 1)
posterior_viab_mix_ant <- as.data.frame(fit_viab_mix_ant)

#### Reproductive State Model
## Create Stan Data
stan_data_repro <- list(N_flower = N_flower, ## number of observations
                  vol1_flower = vol1_flower, ## predictors volume
                  y_repro = y_repro, ## response volume next year
                  ant_flower = ant_flower,## predictors ants
                  N_ant = N_ant, ## number of ant states
                  N_Year = N_Year, ## number of years
                  N_Plot = N_Plot, ## number of plots
                  plot_flower = plot_flower, ## predictor plots
                  year_flower = year_flower ## predictor years
) 
## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/repro_mix_ant.stan")
fit_repro_mix_ant <- stan(file = "STAN Models/repro_mix_ant.stan", data = stan_data_repro, warmup = 5000, iter = 10000, chains = 3, cores = 2, thin = 1)
posterior_repro_mix_ant <- as.data.frame(fit_repro_mix_ant)

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
fit_multi1_mix_ant <- stan(file = "STAN Models/multinomial1.stan", data = stan_data_multi1, warmup = 5, iter = 10, chains = 1, cores = 2, thin = 1)
posterior_multi1 <- as.data.frame(fit_multi1_mix_ant)

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
posterior_multi2 <- as.data.frame(fit_multi2_mix_ant)

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
posterior_multi3 <- as.data.frame(fit_multi3_mix_ant)



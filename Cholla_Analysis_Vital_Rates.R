#setwd("C:/Users/tm9/Dropbox/github/ant_cactus_demography")
#######################################################################################################
##
##                  The purpose of this file is to run each vital rate sub model separately,
##                          save the outputs, and check the posterior distributions 
##
#######################################################################################################
options(mc.cores = parallel::detectCores())
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
source("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Setup_Script.R")
cactus <- read.csv("cholla_demography_20042019_cleaned.csv", header = TRUE,stringsAsFactors=T)

##############################################################################################
#### Growth Model -- What size will the cacti be next time step? #############################
##############################################################################################
growth_data_orig <- cactus[,c("Plot","Year_t","logsize_t","logsize_t1","ant_t")]
growth_data <- na.omit(growth_data_orig)
## Lose 2032 rows (due to plant death & recruit status)
nrow(growth_data_orig)
nrow(growth_data)
# check that you are happy with the subsetting
plot(growth_data$logsize_t, growth_data$logsize_t1)
points((cactus$logsize_t), (cactus$logsize_t1), col = "red")

## Create Stan Data for all ant states
stan_data_grow <- list(N = nrow(growth_data), ## number of observations
                       vol = (growth_data$logsize_t), ## predictors volume
                       y_grow = (growth_data$logsize_t1), ## response survival next year
                       ant = as.integer(as.factor(growth_data$ant_t)),## predictors ants
                       K = 4, ## number of ant states
                       N_Year = max(as.integer(as.factor(growth_data$Year_t))), ## number of years
                       N_Plot = max(as.integer(as.factor(growth_data$Plot))), ## number of plots
                       plot = as.integer(as.factor(growth_data$Plot)), ## predictor plots
                       year = as.integer(as.factor(growth_data$Year_t)) ## predictor years
) 

grow_i <- list(N = nrow(growth_data), ## number of observations
               #vol = (growth_data$logsize_t), ## predictors volume
               y_grow = (growth_data$logsize_t1), ## response survival next year
               vol = replace_na(growth_data$logsize_t1,mean(growth_data$logsize_t1)),
               vol_obs = as.numeric(
                 !is.na(growth_data$logsize_t)
               ),
               ant = as.integer(as.factor(growth_data$ant_t_relevel)),## predictors ants
               K = 4, ## number of ant states
               N_Year = max(as.integer(as.factor(growth_data$Year_t))), ## number of years
               N_Plot = max(as.integer(as.factor(growth_data$Plot))), ## number of plots
               plot = as.integer(as.factor(growth_data$Plot)), ## predictor plots
               year = as.integer(as.factor(growth_data$Year_t)) ## predictor years
               
)
fit_grow <- stan(file = "Data Analysis/STAN Models/growth_code.stan", data = stan_data_grow, warmup = 150, iter = 1000, chains = 3, cores = 3, thin = 1)
grow_outputs <- rstan::extract(fit_grow)
write.csv(grow_outputs, "grow_outputs.csv")
yrep_grow <- rstan::extract(fit_grow, pars = c("y_rep"))$y_rep
write.csv(yrep_grow, "grow_ypred.csv")
summary(fit_grow)

## Check the posterior distributions
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
#For overlay plots
y <- growth_data$logsize_t1
grow_out_all <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow_outputs_all.csv", header = TRUE,stringsAsFactors=T)
samp100 <- sample(nrow(yrep_grow), 500)
## Overlay Plots
png(file = "grow_post.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_grow[samp100,], group = ant_grow)
dev.off()
## Convergence Plots
png("grow_conv.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_grow_all, pars=c("beta0", "beta1")))
dev.off()
## They all converge
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

#######################################################################################################
#### Survival Model -- What is the probability of surviving to the next time step?  ###################
#######################################################################################################
survival_data_orig <- subset(cactus, is.na(Survival_t1) == FALSE,c("Plot","Year_t","Survival_t1","ant_t","logsize_t"))
survival_data_orig <- cactus[,c("Plot","Year_t","Survival_t1","ant_t","logsize_t")]
survival_data <- na.omit(survival_data_orig)
## Lose 1619 rows due to recruit status 
nrow(survival_data_orig)
nrow(survival_data)
# Create Stan Data
stan_data_surv <- list(N = nrow(survival_data), ## number of observations
                           vol = (survival_data$logsize_t), ## predictors volume
                           y_surv = (survival_data$Survival_t1), ## response survival next year
                           ant = as.integer(as.factor(survival_data$ant_t)),## predictors ants
                           K = 4, ## number of ant states
                           N_Year = max(as.integer(as.factor(survival_data$Year_t))), ## number of years
                           N_Plot = max(as.integer(as.factor(survival_data$Plot))), ## number of plots
                           plot = as.integer(as.factor(survival_data$Plot)), ## predictor plots
                           year = as.integer(as.factor(survival_data$Year_t)) ## predictor years
) 

## Run the Model
fit_surv <- stan(file = "Data Analysis/STAN Models/surv_code.stan", data = stan_data_surv, warmup = 150, iter = 1000, chains = 3, cores = 3, thin = 1)
fit_surv_rand <- stan(file = "Data Analysis/STAN Models/surv_code.stan", data = stan_data_surv, warmup = 150, iter = 1000, chains = 3, cores = 3, thin = 1)
surv_outputs <- rstan::extract(fit_surv)
yrep_surv <- rstan::extract(fit_surv, pars = c("y_rep"))$y_rep
write.csv(surv_outputs, "surv_outputs.csv")
write.csv(yrep_surv, "surv_yrep.csv")

## Visualize the posterior distributions
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
#overlay plot data
y <- survival_data$Survival_t1
surv_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/surv_outputs.csv", header = TRUE,stringsAsFactors=T)
samp100 <- sample(nrow(yrep_surv), 500)
## Overlay Plots
png(file = "surv_ant_post.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_surv[samp100,],group = as.integer(as.factor(survival_data$ant_t)))
dev.off()
## Convergence Plots
png(file = "surv_conv.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_surv, pars=c("beta0","beta1")))
dev.off()
bayesplot::mcmc_trace(As.mcmc.list(fit_surv_rand, pars=c("beta0","beta1")))
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

######################################################################################################
#### Flowering Model -- What are the total number of fruits produced in the next time step? ##########
######################################################################################################
flower_data_orig <- cactus[ , c("TotFlowerbuds_t", "logsize_t","Year_t","Plot")]
flower_data_orig <- subset(flower_data_orig, TotFlowerbuds_t > 0)
flower_data <- na.omit(flower_data_orig)
## Lose 6605 rows of data due to no flower data
nrow(flower_data_orig)
nrow(flower_data)
# check that you're happy with the subsetting
plot(flower_data$logsize_t, flower_data$TotFlowerbuds_t)
points(cactus$logsize_t, cactus$TotFlowerbuds_t, col = "red")
## Create Stan Data
stan_data_flow_trunc <- list(N = nrow(flower_data), ## number of observations
                             lower_limit = 1, ## we want the 0s to be removed
                             vol = (flower_data$logsize_t), ## predictors volume
                             y_flow = flower_data$TotFlowerbuds_t, ## response flowers next year
                             N_Year = max(as.integer(as.factor(flower_data$Year_t))), ## number of years
                             N_Plot = max(as.integer(as.factor(flower_data$Plot))), ## number of plots
                             plot = as.integer(as.factor(flower_data$Plot)), ## predictor plots
                             year = as.integer(as.factor(flower_data$Year_t)) ## predictor years
) 
## Run the Model
fit_flow_trunc <- stan(file = "Data Analysis/STAN Models/flower_trunc_code.stan", data = stan_data_flow_trunc, warmup = 150, iter = 1000, chains = 3, cores = 3, thin = 1)
flow_trunc_outputs <- rstan::extract(fit_flow_trunc)
write.csv(flow_trunc_outputs, "flow_trunc_outputs.csv")
flow_yrep <- rstan::extract(fit_flow_trunc, pars = c("mu"))$mu
write.csv(flow_yrep, "flow_yrep.csv")
flow_phi <- rstan::extract(fit_flow_trunc, pars = c("phi"))$phi

## Check the posterior distributions
y <- flower_data$TotFlowerbuds_t
flow_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/flow_trunc_outputs.csv", header = TRUE,stringsAsFactors=T)
# Create the y rep (needs to be done outside of STAN because of the 0 truncation)
n_post_draws <- 500
post_draws <- sample.int(dim(flow_yrep)[1], n_post_draws)

y_sim <- matrix(NA,n_post_draws,length(y))
for(i in 1:n_post_draws){
  ## sample panicle data (zero-truncated NB)
  for(j in 1:length(y)){
    y_sim[i,j] <- sample(x=1:1000,size=1,replace=T,prob=dnbinom(1:1000, mu = exp(flow_yrep[i,j]), size=flow_phi[i]) / (1 - dnbinom(0, mu = exp(flow_yrep[i,j]), size=flow_phi[i])))
  }
}
## Plot the posterior distributions
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("flow_trunc.png")
plot(density(flower_data$TotFlowerbuds_t), lwd = 2, ylim = c(0,.35), main = "Posterior Distribution Total Flowers")
for(i in 1:n_post_draws){
  lines(density(y_sim[i,]), col = "light blue")
}
lines(density(flower_data$TotFlowerbuds_t))
dev.off()
png(file = "flow_trunc_post.png")
bayesplot::ppc_dens_overlay(y, y_sim[n_post_draws,])
dev.off()
## Convergence Plots
png(file = "flow_trunc_conv.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_flow_trunc, pars=c("beta0", "beta1")))
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")


#######################################################################################################
#### Viability Model -- What proportion of fruit are viable? ##########################################
#######################################################################################################
## Create Stan Data
viability_data_orig <- cactus[ , c("TotFlowerbuds_t1","Goodbuds_t1","ABFlowerbuds_t1","ant_t", "logsize_t","Year_t","Plot")]
viability_data_orig <- subset(viability_data_orig, TotFlowerbuds_t1 > 0)
viability_data <- na.omit(viability_data_orig)
## Lose __ Rows of data
nrow(viability_data_orig)
nrow(viability_data)
# check if you're happy with the subsetting
plot(viability_data$logsize_t, viability_data$ABFlowerbuds_t1)
plot(viability_data_orig$logsize_t, viability_data_orig$ABFlowerbuds_t1, col = "red") 

stan_data_viab <- list(N = nrow(viability_data), ## number of observations
                       good = viability_data$Goodbuds_t1,
                       abort = viability_data$ABFlowerbuds_t1, ## aborted buds data
                       tot = viability_data$TotFlowerbuds_t1, ## number of trials
                       ant = as.integer(as.factor(viability_data$ant)),## predictors ants
                       K = 4, ## number of ant states
                       N_Year = max(as.integer(as.factor(viability_data$Year_t))), ## number of years
                       N_Plot = max(as.integer(as.factor(viability_data$Plot))), ## number of plots
                       plot = as.integer(as.factor(viability_data$Plot)), ## predictor plots
                       year = as.integer(as.factor(viability_data$Year_t)) ## predictor years
) 
## Run the Model
fit_viab <- stan(file = "Data Analysis/STAN Models/viab_code.stan", data = stan_data_viab, warmup = 150, iter = 1000, chains = 3, cores = 3, thin = 1)
viab_yrep <- rstan::extract(fit_viab, pars = c("y_rep"))$y_rep
viab_outputs <- rstan::extract(fit_viab)
write.csv(viab_outputs, "viab_outputs.csv")
write.csv(viab_yrep, "viab_yrep.csv")
summary(fit_viab)

## Check the Posterior Distribution
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
y <- viability_data$Goodbuds_t1
yrep_viab <- viab_yrep
samp100 <- sample(nrow(yrep_viab), 500)
## Overlay Plots
png(file = "viab_post.png")
bayesplot::ppc_dens_overlay(y, yrep_viab[samp100,])
dev.off()
png(file = "viab_ant_post.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_viab[samp100,],group = as.integer(as.factor(viability_data$ant)))
dev.off()
## Convergence Plots
png(file = "viab_conv.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_viab, pars=c("beta0")))
dev.off()
png(file = "viab_post_rand.png")
bayesplot::ppc_dens_overlay(y, yrep_viab[samp100,])
dev.off()
png(file = "viab_ant_post_rand.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_viab[samp100,],group = as.integer(as.factor(viability_data$ant)))
dev.off()
## Convergence Plots
png(file = "viab_conv_rand.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_viab, pars=c("beta0")))
dev.off()

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")


#####################################################################################################
##### Reproductive State Model -- Prob of reproducing at next time step #############################
#####################################################################################################
## Repro Data Set
reproductive_data_orig <- cactus[ , c("flower1_YN","logsize_t","Year_t","Plot", "logsize_t1")]
reproductive_data <- na.omit(reproductive_data_orig)
# check that you're happy with the subsetting
plot(reproductive_data$logsize_t, reproductive_data$flower1_YN)
points(cactus$logsize_t, cactus$flower1_YN, col = "red")
## Lose 3332 rows of data because only including 
nrow(reproductive_data_orig)
nrow(reproductive_data)
## Create Stan Data
stan_data_repro <- list(N = nrow(reproductive_data), ## number of observations
                  vol = reproductive_data$logsize_t1, ## predictors volume
                  y_repro = reproductive_data$flower1_YN, ## response volume next year
                  N_Year = max(as.integer(as.factor(reproductive_data$Year_t))), ## number of years
                  N_Plot = max(as.integer(as.factor(reproductive_data$Plot))), ## number of plots
                  plot = as.integer(as.factor(reproductive_data$Plot)), ## predictor plots
                  year = as.integer(as.factor(reproductive_data$Year_t)) ## predictor years
) 
## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/repro_mix_ant.stan")
fit_repro <- stan(file = "Data Analysis/STAN Models/repro_code.stan", data = stan_data_repro, warmup = 150, iter = 1000, chains = 3, cores = 3, thin = 1)
repro_outputs <- rstan::extract(fit_repro)
repro_yrep <- rstan::extract(fit_repro, pars = c("y_rep"))$y_rep
write.csv(repro_outputs, "repro_outputs.csv")

## Check the Posteriors
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
repro_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/repro_outputs.csv", header = TRUE,stringsAsFactors=T)
y <- as.numeric(reproductive_data$flower1_YN)
yrep_repro <- repro_yrep
samp100 <- sample(nrow(yrep_repro), 500)
## Overlay Plots
png(file = "repro_post.png")
bayesplot::ppc_dens_overlay(y, yrep_repro[samp100,])
dev.off()
## Convergence Plots
png(file = "repro_conv.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_repro, pars=c("beta0","beta1")))
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

######################################################################################################
#### Seeds Model -- # Seeds per fruit/flower #########################################################
######################################################################################################
seed_uncleaned <- read.csv("Data Analysis/JO_fruit_data_final_dropplant0.csv", header = TRUE,stringsAsFactors=T)
## PEAA = Ant Access
## PAAA = Ant Access
## PEAE = Ant Exclusion
## PAAE = Ant Exclusion
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

boxplot(seed$seed_count~seed$ant_state)

seed_data <- seed
seed_data <- na.omit(seed_data)
seed_data$ant <- as.integer(as.factor(seed_data$ant_state))
seed_data <- subset(seed_data, seed_count > 0)
# check if you're happy with the subsetting
plot(seed$fruit_number)
points(seed_data$fruit_number, col = "red")
nrow(seed)
nrow(seed_data)
## Create Stan Data
stan_data_seed <- list(N = nrow(seed_data),
                        K = 3,
                        ant = as.integer(as.factor(seed_data$ant)),
                        seed = seed_data$seed_count)

## Run the model
fit_seed <- stan(file = "Data Analysis/STAN Models/seed_code.stan", data = stan_data_seed, warmup = 150, iter = 1000, chains = 3, cores = 3, thin = 1)
seed_outputs <- rstan::extract(fit_seed)
write.csv(seed_outputs, "seed_outputs.csv")
seed_yrep <- rstan::extract(fit_seed, pars = c("y_rep"))$y_rep

## Check the Posteriors
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
y <- seed_data$seed_count
yrep_seed <- seed_yrep
samp100 <- sample(nrow(yrep_seed), 500)
## Overlay Plots
png(file = "seed_post.png")
bayesplot::ppc_dens_overlay(y, yrep_seed[samp100,])
dev.off()
png(file = "seed_ant_post.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_seed[samp100,],group = as.integer(as.factor(seed_data$ant)))
dev.off()
## Convergence Plots
png(file = "seed_conv.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_seed, pars=c("beta0")))
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

###############################################################################################################################
#### Seed Survival Pre Census -- Proportion of seeds that survive from germination to Census ##################################
###############################################################################################################################
precensus.dat.orig<-read.csv("Data Analysis/PrecensusSurvival.csv") 
precensus.dat <- precensus.dat.orig[ , c("Transect","Seed","Log.size","survive0405")]
precensus.dat <- na.omit(precensus.dat)
## You lose no data here, but it is still a small dataset. 
nrow(precensus.dat)
nrow(precensus.dat.orig)
# check that you're happy with the subsetting
plot(precensus.dat$Log.size, jitter(precensus.dat$survive0405))
points(precensus.dat.orig$Log.size, jitter(precensus.dat.orig$survive0405), col = "red")
## Create Stan Data
stan_data_seed_surv <- list(N = nrow(precensus.dat),
                            N_Transect = length(unique(precensus.dat$Transect)),
                            transect = as.integer(as.factor(precensus.dat$Transect)),
                            y = precensus.dat$survive0405)

## Run the model
fit_seed_surv <- stan(file = "Data Analysis/STAN Models/seed_surv_code.stan", data = stan_data_seed_surv, warmup = 500, iter = 1000, chains = 3, cores = 3, thin = 1)
seed_surv_outputs <- rstan::extract(fit_seed_surv)
write.csv(seed_surv_outputs, "seed_surv_outputs.csv")
seed_surv_yrep <- rstan::extract(fit_seed_surv, pars = c("y_rep"))$y_rep

## Check the Posteriors
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
y <- precensus.dat$survive0405
yrep_seed_surv <- seed_surv_yrep
samp100 <- sample(nrow(yrep_seed_surv), 500)
## Overlay Plots
png(file = "seed_surv_post.png")
bayesplot::ppc_dens_overlay(y, yrep_seed_surv[samp100,])
dev.off()
## Convergence Plots
png(file = "seed_surv_conv.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_seed_surv, pars=c("beta0")))
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")


######################################################################################################
#### Germination -- Probability of germinating by the next time step (yr 1&2) ########################
######################################################################################################
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
nrow(germ.dat)
nrow(germ.dat_orig)
#Check that you are happy with the subsetting
plot(germ.dat$rate)
points(germ.dat_orig$rate, col = "red")
nrow(germ.dat)

stan_data_germ1 <- list(N = nrow(germ.dat),
                       y_germ = as.integer(germ.dat$Seedlings04),
                       trials = germ.dat$Input)
stan_data_germ2 <- list(N = nrow(germ.dat),
                       y_germ = germ.dat$Seedlings05,
                       trials = germ.dat$Input-germ.dat$Seedlings04)

## Run a model 
fit_germ1 <- stan(file = "Data Analysis/STAN Models/germ_code.stan", data = stan_data_germ1, warmup = 500, iter = 1000, chains = 3, cores = 3, thin = 1)
fit_germ2 <- stan(file = "Data Analysis/STAN Models/germ_code.stan", data = stan_data_germ2, warmup = 500, iter = 1000, chains = 3, cores = 3, thin = 1)
germ1_outputs <- rstan::extract(fit_germ1)
germ2_outputs <- rstan::extract(fit_germ2)
write.csv(germ1_outputs, "germ1_outputs.csv")
write.csv(germ2_outputs, "germ2_outputs.csv")
germ1_yrep <- rstan::extract(fit_germ1, pars = c("y_rep"))$y_rep
germ2_yrep <- rstan::extract(fit_germ2, pars = c("y_rep"))$y_rep

## Check the Posteriors
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
## Germ yr 1
y <- germ.dat$Seedlings04/germ.dat$Input
yrep_germ1 <- germ1_yrep
samp100 <- sample(nrow(yrep_germ1), 500)
## Overlay Plots
png(file = "germ1_post.png")
p<-bayesplot::ppc_dens_overlay(y, yrep_germ1[samp100,])
dev.off()
## Convergence Plots
png(file = "germ1_conv.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_germ1, pars=c("beta0")))
dev.off()
## Germ yr 2
y <- germ.dat$Seedlings05/(germ.dat$Input - germ.dat$Seedlings04)
yrep_germ2 <- germ2_yrep
samp100 <- sample(nrow(yrep_germ2), 500)
## Overlay Plots
png(file = "germ2_post.png")
bayesplot::ppc_dens_overlay(y, yrep_germ2[samp100,])
dev.off()
## Convergence Plots
png(file = "germ2_conv.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_germ2, pars=c("beta0")))
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

#######################################################################################################
#### Recruits -- Size Distribution of  #########################################################################################
#######################################################################################################
seedling.dat_orig <- cactus[,c("logsize_t1","Recruit")]
seedling.dat_orig <- filter(seedling.dat_orig, Recruit == 1)
seedling.dat <- na.omit(seedling.dat_orig)
nrow(seedling.dat_orig)
nrow(seedling.dat)
# check that you are happy with the subsetting
plot(seedling.dat$logsize_t1, seedling.dat$Recruit, xlim = c(-5,15), ylim = c(0,1))
points(cactus$logsize_t1, cactus$Recruit, col = "red")
## Create Stan Data
stan_data_rec <- list(N = length(seedling.dat$logsize_t1),
                      y_rec = (seedling.dat$logsize_t1)
)

## Run the model 
fit_rec <- stan(file = "Data Analysis/STAN Models/rec_code.stan",data = stan_data_rec, warmup = 150, iter = 1000, chains = 3, cores = 3, thin = 1)
rec_outputs <- rstan::extract(fit_rec)
write.csv(rec_outputs,"rec_outputs.csv")
rec_yrep <- rstan::extract(fit_rec,pars = c("y_rep"))$y_rep

## Check Posterior Dist
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
y <- seedling.dat$logsize_t1
yrep_rec <- rec_yrep
samp100 <- sample(nrow(yrep_rec), 500)
## Overlay Plots
png(file = "rec_post.png")
bayesplot::ppc_dens_overlay(y, yrep_rec[samp100,])
dev.off()
## Convergence Plots
png(file = "rec_conv.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_rec, pars=c("beta0")))
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

# #############################################################################################
# ###### now include ant and size as predictors  ##############################################
# ###### Data Analysis/STAN Models/multi_prac_size_ant_Km1.stan ###############################
# #############################################################################################
cactus_real <- cactus[,c("ant_t","ant_t1","logsize_t","Year_t","Plot")]
cactus_real <- na.omit(cactus_real)
cactus_real$ant_t1_relevel <- relevel(cactus_real$ant_t1,ref = "vacant")
cactus_real$ant_t_relevel <- relevel(cactus_real$ant_t, ref = "vacant")
cactus_real <- cactus_real[,c("ant_t_relevel","ant_t1_relevel","logsize_t", "ant_t", "ant_t1","Year_t","Plot")]
## make stan data set
multi_dat_real <- list(K = length(unique(cactus_real$ant_t1_relevel)), #number of possible ant species
                       N = dim(cactus_real)[1], #number of observations
                       D = 5, #number of predictors
                       y = as.integer(as.factor(cactus_real$ant_t1_relevel)), #observations
                       x = model.matrix(~ 0 + (as.factor(ant_t_relevel)) + logsize_t, cactus_real)) #design matrix
## Run the model & save the results
fit_multi <- stan(file = "Data Analysis/STAN Models/multi_code.stan", 
                      data = multi_dat_real, warmup = 100, iter = 1000, chains = 3)
multi_out <- rstan::extract(fit_multi)
write.csv(multi_out, "multi_outputs.csv")
## plot the chains
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("multi_conv.png")
bayesplot::mcmc_trace(fit_multi)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

summary(fit_multi)
## this looks pretty good. All betas converge

########################################## ALL ANTS ############################################################################
## Calculate the probabilities of being tended by each ant species
## Previously tended by none
Denominator_vac <- exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,1,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,1,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,1,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_vac<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_vac,
  #pr(other)
  exp(mean(multi_out$beta[,1,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_vac,
  #pr(crem)
  exp(mean(multi_out$beta[,1,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_vac,
  #pr(liom)
  exp(mean(multi_out$beta[,1,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_vac)
sum(pred_vac[1,])

## Previously tended by Other
Denominator_other <- exp(mean(multi_out$beta[,2,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,2,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,2,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,2,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_other<-cbind(
  #pr(vacant)
  exp((mean(multi_out$beta[,2,1])) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_other,
  #pr(other)
  exp((mean(multi_out$beta[,2,2])) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_other,
  #pr(crem)
  exp((mean(multi_out$beta[,2,3])) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_other,
  #pr(liom)
  exp((mean(multi_out$beta[,2,4])) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_other)
sum(pred_other[1,])

## Previously tended by Crem
Denominator_crem <- exp(mean(multi_out$beta[,3,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,3,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_crem<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,3,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_crem,
  #pr(other)
  exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_crem,
  #pr(crem)
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_crem,
  #pr(liom)
  exp(mean(multi_out$beta[,3,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_crem)
sum(pred_crem[1,])

## Previously tended by Liom
Denominator_liom <- exp(mean(multi_out$beta[,4,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,4,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,4,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_liom<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,4,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_liom,
  #pr(other)
  exp(mean(multi_out$beta[,4,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_liom,
  #pr(crem)
  exp(mean(multi_out$beta[,4,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_liom,
  #pr(liom)
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_liom)
sum(pred_liom[1,])
## vac -> vac       vac -> other    vac -> crem       vac -> liom
pred_probs_vac <- cbind((pred_vac[,1]) , (pred_vac[,2]) , (pred_vac[,3]) , (pred_vac[,4]))
## other-> vac        other -> other    other -> crem       other -> liom
pred_probs_other <- cbind((pred_other[,1]) , (pred_other[,2]) , (pred_other[,3]) , (pred_other[,4]))
## crem-> vac       crem -> other    crem -> crem      crem -> liom
pred_probs_crem <- cbind((pred_crem[,1]) , (pred_crem[,2]) , (pred_crem[,3]) , (pred_crem[,4]))
## liom-> vac       liom -> other    liom -> crem       liom -> liom
pred_probs_liom <- cbind((pred_liom[,1]) , (pred_liom[,2]) , (pred_liom[,3]) , (pred_liom[,4]))

## Plot the probabilities

cactus$occ_YN <- NA
for(i in 1:length(cactus)){
  if(is.na(cactus$occ_t1[i]) == FALSE & cactus$occ_t1[i] == "occ"){
    cactus$occ_YN[i] <- 1
  }
  if(is.na(cactus$occ_t1[i]) == FALSE & cactus$occ_t1[i] == "vac"){
    cactus$occ_YN[i] <- 0
  }
}
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_Size_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,2,3,4,5),
              ncol = 2, nrow = 3, byrow = TRUE), heights = c(0.6,1.4,1.4), widths = c(3.9,3.9))
plot.new()
text(0.5,0.5,"Ant States",cex=2,font=2)
## Prev Vac
plot(size_dummy_real, pred_vac[,1], type = "l", col = "pink",main = "Previously Vacant", ylim = c(0,1))
lines(size_dummy_real, pred_vac[,2], col = "black")
lines(size_dummy_real, pred_vac[,3], col = "red")
lines(size_dummy_real, pred_vac[,4], col = "blue")
points(cactus$logsize_t[cactus$ant_t1 == "crem" & cactus$ant_t == "vacant"], (cactus$occ_YN[cactus$ant_t1 == "crem" & cactus$ant_t == "vacant"]), col = "red", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "liom" & cactus$ant_t == "vacant"], cactus$occ_YN[cactus$ant_t1 == "liom" & cactus$ant_t == "vacant"], col = "blue", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "other" & cactus$ant_t == "vacant"], cactus$occ_YN[cactus$ant_t1 == "other" & cactus$ant_t == "vacant"], col = "black", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "vacant" & cactus$ant_t == "vacant"], cactus$occ_YN[cactus$ant_t1 == "vacant" & cactus$ant_t == "vacant"], col = "pink", alpha = 0.2)
## Prev Other
plot(size_dummy_real, pred_other[,1], type = "l", col = "pink",main = "Previously Other", ylim = c(0,1))
lines(size_dummy_real, pred_other[,2], col = "black")
lines(size_dummy_real, pred_other[,3], col = "red")
lines(size_dummy_real, pred_other[,4], col = "blue")
points(cactus$logsize_t[cactus$ant_t1 == "crem" & cactus$ant_t == "other"], (cactus$occ_YN[cactus$ant_t1 == "crem" & cactus$ant_t == "other"]), col = "red", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "liom" & cactus$ant_t == "other"], cactus$occ_YN[cactus$ant_t1 == "liom" & cactus$ant_t == "other"], col = "blue", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "other" & cactus$ant_t == "other"], cactus$occ_YN[cactus$ant_t1 == "other" & cactus$ant_t == "other"], col = "black", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "vacant" & cactus$ant_t == "other"], cactus$occ_YN[cactus$ant_t1 == "vacant" & cactus$ant_t == "other"], col = "pink", alpha = 0.2)
legend("topright",c("vacant","other","crem.","liom."), fill = c("pink","black","red","blue"))
## Prev Crem
plot(size_dummy_real, pred_crem[,1], type = "l", col = "pink",main = "Previously Crem", ylim = c(0,1))
lines(size_dummy_real, pred_crem[,2], col = "black")
lines(size_dummy_real, pred_crem[,3], col = "red")
lines(size_dummy_real, pred_crem[,4], col = "blue")
points(cactus$logsize_t[cactus$ant_t1 == "crem" & cactus$ant_t == "crem"], (cactus$occ_YN[cactus$ant_t1 == "crem" & cactus$ant_t == "crem"]), col = "red", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "liom" & cactus$ant_t == "crem"], cactus$occ_YN[cactus$ant_t1 == "liom" & cactus$ant_t == "crem"], col = "blue", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "other" & cactus$ant_t == "crem"], cactus$occ_YN[cactus$ant_t1 == "other" & cactus$ant_t == "crem"], col = "black", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "vacant" & cactus$ant_t == "crem"], cactus$occ_YN[cactus$ant_t1 == "vacant" & cactus$ant_t == "crem"], col = "pink", alpha = 0.2)
## Prev Liom
plot(size_dummy_real, pred_liom[,1], type = "l", col = "pink",main = "Previously Liom", ylim = c(0,1))
lines(size_dummy_real, pred_liom[,2], col = "black")
lines(size_dummy_real, pred_liom[,3], col = "red")
lines(size_dummy_real, pred_liom[,4], col = "blue")
points(cactus$logsize_t[cactus$ant_t1 == "crem" & cactus$ant_t == "liom"], (cactus$occ_YN[cactus$ant_t1 == "crem" & cactus$ant_t == "liom"]), col = "red", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "liom" & cactus$ant_t == "liom"], cactus$occ_YN[cactus$ant_t1 == "liom" & cactus$ant_t == "liom"], col = "blue", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "other" & cactus$ant_t == "liom"], cactus$occ_YN[cactus$ant_t1 == "other" & cactus$ant_t == "liom"], col = "black", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "vacant" & cactus$ant_t == "liom"], cactus$occ_YN[cactus$ant_t1 == "vacant" & cactus$ant_t == "liom"], col = "pink", alpha = 0.2)
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")


## Look into the frequentist model
cactus_fit <- summary(multinom(ant_t1_relevel ~ 0 + ant_t_relevel + logsize_t, cactus_real))
cactus_fit_coef <- coef(cactus_fit)

Denominator_freq_vac <- 1+sum(exp(cactus_fit_coef[1,1] + size_dummy_real*cactus_fit_coef[1,5]), 
                              exp(cactus_fit_coef[2,1] + size_dummy_real*cactus_fit_coef[2,5]),
                              exp(cactus_fit_coef[3,1] + size_dummy_real*cactus_fit_coef[3,5]))
pred_vac_freq<-c(
  #pr(vacant)
  1/Denominator_freq_vac,
  #pr(other)
  exp(cactus_fit_coef[1,1] + size_dummy_real*cactus_fit_coef[1,5])/Denominator_freq_vac,
  #pr(crem)
  exp(cactus_fit_coef[2,1] + size_dummy_real*cactus_fit_coef[2,5])/Denominator_freq_vac,
  #pr(liom)
  exp(cactus_fit_coef[3,1] + size_dummy_real*cactus_fit_coef[3,5])/Denominator_freq_vac
)
sum(pred_vac_freq)

Denominator_freq_crem <- 1+sum(exp(cactus_fit_coef[1,2] + size_dummy_real*cactus_fit_coef[1,5]), 
                               exp(cactus_fit_coef[2,2] + size_dummy_real*cactus_fit_coef[2,5]),
                               exp(cactus_fit_coef[3,2] + size_dummy_real*cactus_fit_coef[3,5]))
pred_crem_freq<-c(
  #pr(vacant)
  1/Denominator_freq_crem,
  #pr(other)
  exp(cactus_fit_coef[1,2] + size_dummy_real*cactus_fit_coef[1,5])/Denominator_freq_crem,
  #pr(crem)
  exp(cactus_fit_coef[2,2] + size_dummy_real*cactus_fit_coef[2,5])/Denominator_freq_crem,
  #pr(liom)
  exp(cactus_fit_coef[3,2] + size_dummy_real*cactus_fit_coef[3,5])/Denominator_freq_crem
)
sum(pred_crem_freq)

Denominator_freq_liom <- 1+sum(exp(cactus_fit_coef[1,3] + size_dummy_real*cactus_fit_coef[1,5]), 
                               exp(cactus_fit_coef[2,3] + size_dummy_real*cactus_fit_coef[2,5]),
                               exp(cactus_fit_coef[3,3] + size_dummy_real*cactus_fit_coef[3,5]))
pred_liom_freq<-c(
  #pr(vacant)
  1/Denominator_freq_liom,
  #pr(other)
  exp(cactus_fit_coef[1,3] + size_dummy_real*cactus_fit_coef[1,5])/Denominator_freq_liom,
  #pr(crem)
  exp(cactus_fit_coef[2,3] + size_dummy_real*cactus_fit_coef[2,5])/Denominator_freq_liom,
  #pr(liom)
  exp(cactus_fit_coef[3,3] + size_dummy_real*cactus_fit_coef[3,5])/Denominator_freq_liom
)
sum(pred_liom_freq)

Denominator_freq_other <- 1+sum(exp(cactus_fit_coef[1,4] + size_dummy_real*cactus_fit_coef[1,5]), 
                                exp(cactus_fit_coef[2,4] + size_dummy_real*cactus_fit_coef[2,5]),
                                exp(cactus_fit_coef[3,4] + size_dummy_real*cactus_fit_coef[3,5]))
pred_other_freq<-c(
  #pr(vacant)
  1/Denominator_freq_other,
  #pr(other)
  exp(cactus_fit_coef[1,4] + size_dummy_real*cactus_fit_coef[1,5])/Denominator_freq_other,
  #pr(crem)
  exp(cactus_fit_coef[2,4] + size_dummy_real*cactus_fit_coef[2,5])/Denominator_freq_other,
  #pr(liom)
  exp(cactus_fit_coef[3,4] + size_dummy_real*cactus_fit_coef[3,5])/Denominator_freq_other
)
sum(pred_other_freq)

## Compare outputs to real data and frequentist poutputs
## Real data
a<-table(cactus$ant_t1, cactus$ant_t)
a[,1]/colSums(a)[1]
a[,2]/colSums(a)[2]
a[,3]/colSums(a)[3]
a[,4]/colSums(a)[4]

#######################################################################################################################
#### 3 ANTS TRANSITION PROBABILITIES ##################################################################################
#######################################################################################################################

##################################### LIOM & VAC & CREM ###############################################################
## Calculate the probabilities of being tended by each ant species
## Previously tended by none
Denominator_vac <- exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,1,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,1,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_vac<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_vac,
  #pr(crem)
  exp(mean(multi_out$beta[,1,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_vac,
  #pr(liom)
  exp(mean(multi_out$beta[,1,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_vac)
sum(pred_vac[1,])

## Previously tended by Crem
Denominator_crem <- exp(mean(multi_out$beta[,3,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,3,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_crem<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,3,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_crem,
  #pr(crem)
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_crem,
  #pr(liom)
  exp(mean(multi_out$beta[,3,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_crem)
sum(pred_crem[1,])

## Previously tended by Liom
Denominator_liom <- exp(mean(multi_out$beta[,4,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,4,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_liom<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,4,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_liom,
  #pr(crem)
  exp(mean(multi_out$beta[,4,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_liom,
  #pr(liom)
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_liom)
sum(pred_liom[1,])
                        ## vac -> vac      vac -> crem       vac -> liom
pred_probs_vac <- cbind((pred_vac[,1]) , (pred_vac[,2]) , (pred_vac[,3]))
                        ## crem-> vac      crem -> crem      crem -> liom
pred_probs_crem <- cbind((pred_crem[,1]) ,  (pred_crem[,2]) , (pred_crem[,3]))
                        ## liom-> vac      liom -> crem       liom -> liom
pred_probs_liom <- cbind((pred_liom[,1]) ,  (pred_liom[,2]) , (pred_liom[,3]))

## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_3_LVC_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4, byrow = TRUE), heights = c(0.6,1,1,1), widths = c(5))
plot.new()
text(0.5,0.5,"Ant States \n Vacant, Crem., and Liom.",cex=2,font=2)
plot(size_dummy_real, pred_vac[,1], type = "l", col = "pink",main = "Previously Vacant", ylim = c(0,1))
lines(size_dummy_real, pred_vac[,2], col = "red")
lines(size_dummy_real, pred_vac[,3], col = "blue")
points(cactus$logsize_t[cactus$ant_t1 == "crem" & cactus$ant_t == "vacant"], (cactus$occ_YN[cactus$ant_t1 == "crem" & cactus$ant_t == "vacant"]), col = "red", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "liom" & cactus$ant_t == "vacant"], cactus$occ_YN[cactus$ant_t1 == "liom" & cactus$ant_t == "vacant"], col = "blue", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "vacant" & cactus$ant_t == "vacant"], cactus$occ_YN[cactus$ant_t1 == "vacant" & cactus$ant_t == "vacant"], col = "pink", alpha = 0.2)
legend("topleft",c("vacant","other","crem.","liom."), fill = c("pink","black","red","blue"))
plot(size_dummy_real, pred_crem[,1], type = "l", col = "pink",main = "Previously Crem", ylim = c(0,1))
lines(size_dummy_real, pred_crem[,2], col = "red")
lines(size_dummy_real, pred_crem[,3], col = "blue")
points(cactus$logsize_t[cactus$ant_t1 == "crem" & cactus$ant_t == "crem"], (cactus$occ_YN[cactus$ant_t1 == "crem" & cactus$ant_t == "crem"]), col = "red", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "liom" & cactus$ant_t == "crem"], cactus$occ_YN[cactus$ant_t1 == "liom" & cactus$ant_t == "crem"], col = "blue", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "vacant" & cactus$ant_t == "crem"], cactus$occ_YN[cactus$ant_t1 == "vacant" & cactus$ant_t == "crem"], col = "pink", alpha = 0.2)
plot(size_dummy_real, pred_liom[,1], type = "l", col = "pink",main = "Previously Liom", ylim = c(0,1))
lines(size_dummy_real, pred_liom[,2], col = "red")
lines(size_dummy_real, pred_liom[,3], col = "blue")
points(cactus$logsize_t[cactus$ant_t1 == "crem" & cactus$ant_t == "liom"], (cactus$occ_YN[cactus$ant_t1 == "crem" & cactus$ant_t == "liom"]), col = "red", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "liom" & cactus$ant_t == "liom"], cactus$occ_YN[cactus$ant_t1 == "liom" & cactus$ant_t == "liom"], col = "blue", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "vacant" & cactus$ant_t == "liom"], cactus$occ_YN[cactus$ant_t1 == "vacant" & cactus$ant_t == "liom"], col = "pink", alpha = 0.2)
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
####################################### LIOM & OTHER & CREM ####################################################
## Calculate the probabilities of being tended by each ant species
## Previously tended by Other
Denominator_other <- exp(mean(multi_out$beta[,2,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,2,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,2,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_other<-cbind(
  #pr(other)
  exp((mean(multi_out$beta[,2,2])) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_other,
  #pr(crem)
  exp((mean(multi_out$beta[,2,3])) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_other,
  #pr(liom)
  exp((mean(multi_out$beta[,2,4])) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_other)
sum(pred_other[1,])

## Previously tended by Crem
Denominator_crem <- exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,3,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_crem<-cbind(
  #pr(other)
  exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_crem,
  #pr(crem)
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_crem,
  #pr(liom)
  exp(mean(multi_out$beta[,3,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_crem)
sum(pred_crem[1,])

## Previously tended by Liom
Denominator_liom <- exp(mean(multi_out$beta[,4,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,4,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_liom<-cbind(
  #pr(other)
  exp(mean(multi_out$beta[,4,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_liom,
  #pr(crem)
  exp(mean(multi_out$beta[,4,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_liom,
  #pr(liom)
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_liom)
sum(pred_liom[1,])
                    ## other -> other    other -> crem       other -> liom
pred_probs_other <- cbind((pred_other[,1]) , (pred_other[,2]) , (pred_other[,2]))
                    ## crem -> other    crem -> crem      crem -> liom
pred_probs_crem <- cbind((pred_crem[,1]) , (pred_crem[,2]) , (pred_crem[,3]))
                    ## liom -> other    liom -> crem       liom -> liom
pred_probs_liom <- cbind((pred_liom[,1]) , (pred_liom[,2]) , (pred_liom[,3]))

## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_3_LOC_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4, byrow = TRUE), heights = c(0.6,1,1,1), widths = c(5))
plot.new()
text(0.5,0.5,"Ant States \n Other, Crem., and Liom.",cex=2,font=2)
plot(size_dummy_real, pred_other[,1], type = "l", col = "black",main = "Previously Other", ylim = c(0,1))
lines(size_dummy_real, pred_other[,2], col = "red")
lines(size_dummy_real, pred_other[,3], col = "blue")
points(cactus$logsize_t[cactus$ant_t1 == "crem" & cactus$ant_t == "other"], (cactus$occ_YN[cactus$ant_t1 == "crem" & cactus$ant_t == "other"]), col = "red", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "liom" & cactus$ant_t == "other"], cactus$occ_YN[cactus$ant_t1 == "liom" & cactus$ant_t == "other"], col = "blue", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "other" & cactus$ant_t == "other"], cactus$occ_YN[cactus$ant_t1 == "other" & cactus$ant_t == "other"], col = "black", alpha = 0.2)
legend("topleft",c("vacant","other","crem.","liom."), fill = c("pink","black","red","blue"))
plot(size_dummy_real, pred_crem[,1], type = "l", col = "black",main = "Previously Crem", ylim = c(0,1))
lines(size_dummy_real, pred_crem[,2], col = "red")
lines(size_dummy_real, pred_crem[,3], col = "blue")
points(cactus$logsize_t[cactus$ant_t1 == "crem" & cactus$ant_t == "crem"], (cactus$occ_YN[cactus$ant_t1 == "crem" & cactus$ant_t == "crem"]), col = "red", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "liom" & cactus$ant_t == "crem"], cactus$occ_YN[cactus$ant_t1 == "liom" & cactus$ant_t == "crem"], col = "blue", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "other" & cactus$ant_t == "crem"], cactus$occ_YN[cactus$ant_t1 == "other" & cactus$ant_t == "crem"], col = "black", alpha = 0.2)
plot(size_dummy_real, pred_liom[,1], type = "l", col = "black",main = "Previously Liom", ylim = c(0,1))
lines(size_dummy_real, pred_liom[,2], col = "red")
lines(size_dummy_real, pred_liom[,3], col = "blue")
points(cactus$logsize_t[cactus$ant_t1 == "crem" & cactus$ant_t == "liom"], (cactus$occ_YN[cactus$ant_t1 == "crem" & cactus$ant_t == "liom"]), col = "red", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "liom" & cactus$ant_t == "liom"], cactus$occ_YN[cactus$ant_t1 == "liom" & cactus$ant_t == "liom"], col = "blue", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "other" & cactus$ant_t == "liom"], cactus$occ_YN[cactus$ant_t1 == "other" & cactus$ant_t == "liom"], col = "black", alpha = 0.2)
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

####################################### LIOM & OTHER & VAC #####################################################
## Calculate the probabilities of being tended by each ant species
## Previously tended by none
Denominator_vac <- exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,1,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,1,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_vac<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_vac,
  #pr(other)
  exp(mean(multi_out$beta[,1,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_vac,
  #pr(liom)
  exp(mean(multi_out$beta[,1,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_vac)
sum(pred_vac[1,])

## Previously tended by Other
Denominator_other <- exp(mean(multi_out$beta[,3,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,3,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_other<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,3,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_crem,
  #pr(other)
  exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_crem,
  #pr(liom)
  exp(mean(multi_out$beta[,3,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_crem)
sum(pred_crem[1,])

## Previously tended by Liom
Denominator_liom <- exp(mean(multi_out$beta[,4,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,4,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_liom<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,4,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_liom,
  #pr(other)
  exp(mean(multi_out$beta[,4,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_liom,
  #pr(liom)
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_liom)
sum(pred_liom[1,])

                  ## vac -> vac       vac -> other    vac -> liom
pred_probs_vac <- cbind((pred_vac[,1]) , (pred_vac[,2]) , (pred_vac[,3]))
                  ## other-> vac        other -> other    other -> liom
pred_probs_other <- cbind((pred_other[,1]) , (pred_other[,2]) , (pred_other[,3]))
                  ## liom-> vac       liom -> other    liom -> liom
pred_probs_liom <- cbind((pred_liom[,1]) , (pred_liom[,2]) , (pred_liom[,3]))

## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_3_LOV_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4, byrow = TRUE), heights = c(0.6,1,1,1), widths = c(5))
plot.new()
text(0.5,0.5,"Ant States \n Other, Vacant, and Liom.",cex=2,font=2)
plot(size_dummy_real, pred_vac[,1], type = "l", col = "pink",main = "Previously Vacant", ylim = c(0,1))
lines(size_dummy_real, pred_vac[,2], col = "black")
lines(size_dummy_real, pred_vac[,3], col = "blue")
points(cactus$logsize_t[cactus$ant_t1 == "vacant" & cactus$ant_t == "vacant"], (cactus$occ_YN[cactus$ant_t1 == "vacant" & cactus$ant_t == "vacant"]), col = "pink", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "liom" & cactus$ant_t == "vacant"], cactus$occ_YN[cactus$ant_t1 == "liom" & cactus$ant_t == "vacant"], col = "blue", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "other" & cactus$ant_t == "vacant"], cactus$occ_YN[cactus$ant_t1 == "other" & cactus$ant_t == "vacant"], col = "black", alpha = 0.2)
legend("topleft",c("vacant","other","crem.","liom."), fill = c("pink","black","red","blue"))
plot(size_dummy_real, pred_other[,1], type = "l", col = "pink",main = "Previously Other", ylim = c(0,1))
lines(size_dummy_real, pred_other[,2], col = "black")
lines(size_dummy_real, pred_other[,3], col = "blue")
points(cactus$logsize_t[cactus$ant_t1 == "vacant" & cactus$ant_t == "other"], (cactus$occ_YN[cactus$ant_t1 == "vacant" & cactus$ant_t == "other"]), col = "pink", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "liom" & cactus$ant_t == "other"], cactus$occ_YN[cactus$ant_t1 == "liom" & cactus$ant_t == "other"], col = "blue", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "other" & cactus$ant_t == "other"], cactus$occ_YN[cactus$ant_t1 == "other" & cactus$ant_t == "other"], col = "black", alpha = 0.2)
plot(size_dummy_real, pred_liom[,1], type = "l", col = "pink",main = "Previously Liom", ylim = c(0,1))
lines(size_dummy_real, pred_liom[,2], col = "black")
lines(size_dummy_real, pred_liom[,3], col = "blue")
points(cactus$logsize_t[cactus$ant_t1 == "vacant" & cactus$ant_t == "liom"], (cactus$occ_YN[cactus$ant_t1 == "vacant" & cactus$ant_t == "liom"]), col = "pink", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "liom" & cactus$ant_t == "liom"], cactus$occ_YN[cactus$ant_t1 == "liom" & cactus$ant_t == "liom"], col = "blue", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "other" & cactus$ant_t == "liom"], cactus$occ_YN[cactus$ant_t1 == "other" & cactus$ant_t == "liom"], col = "black", alpha = 0.2)
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

####################################### OTHER & CREM & VAC #####################################################
## Calculate the probabilities of being tended by each ant species
## Previously tended by none
Denominator_vac <- exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,1,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,1,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))
pred_vac<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_vac,
  #pr(other)
  exp(mean(multi_out$beta[,1,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_vac,
  #pr(crem)
  exp(mean(multi_out$beta[,1,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_vac)
sum(pred_vac[1,])

## Previously tended by Other
Denominator_other <- exp(mean(multi_out$beta[,2,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,2,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,2,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))
pred_other<-cbind(
  #pr(vacant)
  exp((mean(multi_out$beta[,2,1])) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_other,
  #pr(other)
  exp((mean(multi_out$beta[,2,2])) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_other,
  #pr(crem)
  exp((mean(multi_out$beta[,2,3])) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_other)
sum(pred_other[1,])

## Previously tended by Crem
Denominator_crem <- exp(mean(multi_out$beta[,3,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))
pred_crem<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,3,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_crem,
  #pr(other)
  exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_crem,
  #pr(crem)
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_crem)
sum(pred_crem[1,])

                    ## vac -> vac       vac -> other    vac -> crem  
pred_probs_vac <- cbind((pred_vac[,1]) , (pred_vac[,2]) , (pred_vac[,3]))
                    ## other-> vac        other -> other    other -> crem    
pred_probs_other <- cbind((pred_other[,1]) , (pred_other[,2]) , (pred_other[,3]))
                    ## crem-> vac       crem -> other    crem -> crem    
pred_probs_crem <- cbind((pred_crem[,1]) , (pred_crem[,2]) , (pred_crem[,3]))

## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_3_COV_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4, byrow = TRUE), heights = c(0.6,1,1,1), widths = c(5))
plot.new()
text(0.5,0.5,"Ant States \n Crem., Vacant, and Liom.",cex=2,font=2)
plot(size_dummy_real, pred_vac[,1], type = "l", col = "pink",main = "Previously Vacant", ylim = c(0,1))
lines(size_dummy_real, pred_vac[,2], col = "black")
lines(size_dummy_real, pred_vac[,3], col = "red")
points(cactus$logsize_t[cactus$ant_t1 == "vacant" & cactus$ant_t == "vacant"], (cactus$occ_YN[cactus$ant_t1 == "vacant" & cactus$ant_t == "vacant"]), col = "pink", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "crem" & cactus$ant_t == "vacant"], cactus$occ_YN[cactus$ant_t1 == "crem" & cactus$ant_t == "vacant"], col = "red", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "other" & cactus$ant_t == "vacant"], cactus$occ_YN[cactus$ant_t1 == "other" & cactus$ant_t == "vacant"], col = "black", alpha = 0.2)
legend("topleft",c("vacant","other","crem.","liom."), fill = c("pink","black","red","blue"))
plot(size_dummy_real, pred_other[,1], type = "l", col = "pink",main = "Previously Other", ylim = c(0,1))
lines(size_dummy_real, pred_other[,2], col = "black")
lines(size_dummy_real, pred_other[,3], col = "red")
points(cactus$logsize_t[cactus$ant_t1 == "vacant" & cactus$ant_t == "other"], (cactus$occ_YN[cactus$ant_t1 == "vacant" & cactus$ant_t == "other"]), col = "pink", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "crem" & cactus$ant_t == "other"], cactus$occ_YN[cactus$ant_t1 == "crem" & cactus$ant_t == "other"], col = "red", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "other" & cactus$ant_t == "other"], cactus$occ_YN[cactus$ant_t1 == "other" & cactus$ant_t == "other"], col = "black", alpha = 0.2)
plot(size_dummy_real, pred_crem[,1], type = "l", col = "pink",main = "Previously Liom", ylim = c(0,1))
lines(size_dummy_real, pred_crem[,2], col = "black")
lines(size_dummy_real, pred_crem[,3], col = "red")
points(cactus$logsize_t[cactus$ant_t1 == "vacant" & cactus$ant_t == "crem"], (cactus$occ_YN[cactus$ant_t1 == "vacant" & cactus$ant_t == "crem"]), col = "pink", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "crem" & cactus$ant_t == "crem"], cactus$occ_YN[cactus$ant_t1 == "crem" & cactus$ant_t == "crem"], col = "red", alpha = 0.2)
points(cactus$logsize_t[cactus$ant_t1 == "other" & cactus$ant_t == "crem"], cactus$occ_YN[cactus$ant_t1 == "other" & cactus$ant_t == "crem"], col = "black", alpha = 0.2)
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

################################################################################################################
#### 2 ANT SPECIES TRANSITION RATES ############################################################################
################################################################################################################

####################################### LIOM & OTHER ###########################################################
## Calculate the probabilities of being tended by each ant species
## Previously tended by Other
Denominator_other <- exp(mean(multi_out$beta[,2,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,2,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_other<-cbind(
  #pr(other)
  exp((mean(multi_out$beta[,2,2])) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_other,
  #pr(liom)
  exp((mean(multi_out$beta[,2,4])) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_other)
sum(pred_other[1,])
## Previously tended by Liom
Denominator_liom <- exp(mean(multi_out$beta[,4,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_liom<-cbind(
  #pr(other)
  exp(mean(multi_out$beta[,4,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_liom,
  #pr(liom)
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_liom)
sum(pred_liom[1,])
## other-> vac        other -> other    other -> crem       other -> liom
pred_probs_other <- cbind((pred_other[,1]) , (pred_other[,2]) , (pred_other[,3]) , (pred_other[,4]))
## liom-> vac       liom -> other    liom -> crem       liom -> liom
pred_probs_liom <- cbind((pred_liom[,1]) , (pred_liom[,2]) , (pred_liom[,3]) , (pred_liom[,4]))

## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_Size_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,2,3,4,5),
              ncol = 2, nrow = 3, byrow = TRUE), heights = c(0.6,1.4,1.4), widths = c(3.9,3.9))
plot.new()
text(0.5,0.5,"Ant States",cex=2,font=2)
plot(size_dummy_real, pred_vac[,1], type = "l", col = "pink",main = "Previously Vacant", ylim = c(0,1))
lines(size_dummy_real, pred_vac[,2], col = "black")
lines(size_dummy_real, pred_vac[,3], col = "red")
lines(size_dummy_real, pred_vac[,4], col = "blue")
plot(size_dummy_real, pred_other[,1], type = "l", col = "pink",main = "Previously Other", ylim = c(0,1))
lines(size_dummy_real, pred_other[,2], col = "black")
lines(size_dummy_real, pred_other[,3], col = "red")
lines(size_dummy_real, pred_other[,4], col = "blue")
legend("topright",c("vacant","other","crem.","liom."), fill = c("pink","black","red","blue"))
plot(size_dummy_real, pred_crem[,1], type = "l", col = "pink",main = "Previously Crem", ylim = c(0,1))
lines(size_dummy_real, pred_crem[,2], col = "black")
lines(size_dummy_real, pred_crem[,3], col = "red")
lines(size_dummy_real, pred_crem[,4], col = "blue")
plot(size_dummy_real, pred_liom[,1], type = "l", col = "pink",main = "Previously Liom", ylim = c(0,1))
lines(size_dummy_real, pred_liom[,2], col = "black")
lines(size_dummy_real, pred_liom[,3], col = "red")
lines(size_dummy_real, pred_liom[,4], col = "blue")
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
####################################### LIOM & VAC #############################################################
## Calculate the probabilities of being tended by each ant species
## Previously tended by none
Denominator_vac <- exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,1,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,1,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,1,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_vac<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_vac,
  #pr(other)
  exp(mean(multi_out$beta[,1,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_vac,
  #pr(crem)
  exp(mean(multi_out$beta[,1,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_vac,
  #pr(liom)
  exp(mean(multi_out$beta[,1,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_vac)
sum(pred_vac[1,])
## Previously tended by Crem
Denominator_other <- exp(mean(multi_out$beta[,2,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,2,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,2,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,2,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_other<-cbind(
  #pr(vacant)
  exp((mean(multi_out$beta[,2,1])) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_other,
  #pr(other)
  exp((mean(multi_out$beta[,2,2])) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_other,
  #pr(crem)
  exp((mean(multi_out$beta[,2,3])) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_other,
  #pr(liom)
  exp((mean(multi_out$beta[,2,4])) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_other)
sum(pred_other[1,])
## Previously tended by Liom
Denominator_crem <- exp(mean(multi_out$beta[,3,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,3,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))

pred_crem<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,3,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_crem,
  #pr(other)
  exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_crem,
  #pr(crem)
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_crem,
  #pr(liom)
  exp(mean(multi_out$beta[,3,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_crem)
sum(pred_crem[1,])
## Previously tended by Other
Denominator_liom <- exp(mean(multi_out$beta[,4,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,4,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,4,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))

pred_liom<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,4,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_liom,
  #pr(other)
  exp(mean(multi_out$beta[,4,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_liom,
  #pr(crem)
  exp(mean(multi_out$beta[,4,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_liom,
  #pr(liom)
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_liom)
sum(pred_liom[1,])
## vac -> vac       vac -> other    vac -> crem       vac -> liom
pred_probs_vac <- cbind((pred_vac[,1]) , (pred_vac[,2]) , (pred_vac[,3]) , (pred_vac[,4]))
## other-> vac        other -> other    other -> crem       other -> liom
pred_probs_other <- cbind((pred_other[,1]) , (pred_other[,2]) , (pred_other[,3]) , (pred_other[,4]))
## crem-> vac       crem -> other    crem -> crem      crem -> liom
pred_probs_crem <- cbind((pred_crem[,1]) , (pred_crem[,2]) , (pred_crem[,3]) , (pred_crem[,4]))
## liom-> vac       liom -> other    liom -> crem       liom -> liom
pred_probs_liom <- cbind((pred_liom[,1]) , (pred_liom[,2]) , (pred_liom[,3]) , (pred_liom[,4]))

## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_Size_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,2,3,4,5),
              ncol = 2, nrow = 3, byrow = TRUE), heights = c(0.6,1.4,1.4), widths = c(3.9,3.9))
plot.new()
text(0.5,0.5,"Ant States",cex=2,font=2)
plot(size_dummy_real, pred_vac[,1], type = "l", col = "pink",main = "Previously Vacant", ylim = c(0,1))
lines(size_dummy_real, pred_vac[,2], col = "black")
lines(size_dummy_real, pred_vac[,3], col = "red")
lines(size_dummy_real, pred_vac[,4], col = "blue")
plot(size_dummy_real, pred_other[,1], type = "l", col = "pink",main = "Previously Other", ylim = c(0,1))
lines(size_dummy_real, pred_other[,2], col = "black")
lines(size_dummy_real, pred_other[,3], col = "red")
lines(size_dummy_real, pred_other[,4], col = "blue")
legend("topright",c("vacant","other","crem.","liom."), fill = c("pink","black","red","blue"))
plot(size_dummy_real, pred_crem[,1], type = "l", col = "pink",main = "Previously Crem", ylim = c(0,1))
lines(size_dummy_real, pred_crem[,2], col = "black")
lines(size_dummy_real, pred_crem[,3], col = "red")
lines(size_dummy_real, pred_crem[,4], col = "blue")
plot(size_dummy_real, pred_liom[,1], type = "l", col = "pink",main = "Previously Liom", ylim = c(0,1))
lines(size_dummy_real, pred_liom[,2], col = "black")
lines(size_dummy_real, pred_liom[,3], col = "red")
lines(size_dummy_real, pred_liom[,4], col = "blue")
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
####################################### LIOM & CREM ############################################################
## Calculate the probabilities of being tended by each ant species
## Previously tended by none
Denominator_vac <- exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,1,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,1,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,1,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_vac<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_vac,
  #pr(other)
  exp(mean(multi_out$beta[,1,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_vac,
  #pr(crem)
  exp(mean(multi_out$beta[,1,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_vac,
  #pr(liom)
  exp(mean(multi_out$beta[,1,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_vac)
sum(pred_vac[1,])
## Previously tended by Crem
Denominator_other <- exp(mean(multi_out$beta[,2,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,2,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,2,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,2,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_other<-cbind(
  #pr(vacant)
  exp((mean(multi_out$beta[,2,1])) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_other,
  #pr(other)
  exp((mean(multi_out$beta[,2,2])) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_other,
  #pr(crem)
  exp((mean(multi_out$beta[,2,3])) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_other,
  #pr(liom)
  exp((mean(multi_out$beta[,2,4])) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_other)
sum(pred_other[1,])
## Previously tended by Liom
Denominator_crem <- exp(mean(multi_out$beta[,3,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,3,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))

pred_crem<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,3,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_crem,
  #pr(other)
  exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_crem,
  #pr(crem)
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_crem,
  #pr(liom)
  exp(mean(multi_out$beta[,3,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_crem)
sum(pred_crem[1,])
## Previously tended by Other
Denominator_liom <- exp(mean(multi_out$beta[,4,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,4,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,4,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))

pred_liom<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,4,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_liom,
  #pr(other)
  exp(mean(multi_out$beta[,4,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_liom,
  #pr(crem)
  exp(mean(multi_out$beta[,4,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_liom,
  #pr(liom)
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_liom)
sum(pred_liom[1,])
## vac -> vac       vac -> other    vac -> crem       vac -> liom
pred_probs_vac <- cbind((pred_vac[,1]) , (pred_vac[,2]) , (pred_vac[,3]) , (pred_vac[,4]))
## other-> vac        other -> other    other -> crem       other -> liom
pred_probs_other <- cbind((pred_other[,1]) , (pred_other[,2]) , (pred_other[,3]) , (pred_other[,4]))
## crem-> vac       crem -> other    crem -> crem      crem -> liom
pred_probs_crem <- cbind((pred_crem[,1]) , (pred_crem[,2]) , (pred_crem[,3]) , (pred_crem[,4]))
## liom-> vac       liom -> other    liom -> crem       liom -> liom
pred_probs_liom <- cbind((pred_liom[,1]) , (pred_liom[,2]) , (pred_liom[,3]) , (pred_liom[,4]))

## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_Size_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,2,3,4,5),
              ncol = 2, nrow = 3, byrow = TRUE), heights = c(0.6,1.4,1.4), widths = c(3.9,3.9))
plot.new()
text(0.5,0.5,"Ant States",cex=2,font=2)
plot(size_dummy_real, pred_vac[,1], type = "l", col = "pink",main = "Previously Vacant", ylim = c(0,1))
lines(size_dummy_real, pred_vac[,2], col = "black")
lines(size_dummy_real, pred_vac[,3], col = "red")
lines(size_dummy_real, pred_vac[,4], col = "blue")
plot(size_dummy_real, pred_other[,1], type = "l", col = "pink",main = "Previously Other", ylim = c(0,1))
lines(size_dummy_real, pred_other[,2], col = "black")
lines(size_dummy_real, pred_other[,3], col = "red")
lines(size_dummy_real, pred_other[,4], col = "blue")
legend("topright",c("vacant","other","crem.","liom."), fill = c("pink","black","red","blue"))
plot(size_dummy_real, pred_crem[,1], type = "l", col = "pink",main = "Previously Crem", ylim = c(0,1))
lines(size_dummy_real, pred_crem[,2], col = "black")
lines(size_dummy_real, pred_crem[,3], col = "red")
lines(size_dummy_real, pred_crem[,4], col = "blue")
plot(size_dummy_real, pred_liom[,1], type = "l", col = "pink",main = "Previously Liom", ylim = c(0,1))
lines(size_dummy_real, pred_liom[,2], col = "black")
lines(size_dummy_real, pred_liom[,3], col = "red")
lines(size_dummy_real, pred_liom[,4], col = "blue")
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
####################################### OTHER & VAC ############################################################
## Calculate the probabilities of being tended by each ant species
## Previously tended by none
Denominator_vac <- exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,1,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,1,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,1,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_vac<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_vac,
  #pr(other)
  exp(mean(multi_out$beta[,1,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_vac,
  #pr(crem)
  exp(mean(multi_out$beta[,1,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_vac,
  #pr(liom)
  exp(mean(multi_out$beta[,1,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_vac)
sum(pred_vac[1,])
## Previously tended by Crem
Denominator_other <- exp(mean(multi_out$beta[,2,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,2,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,2,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,2,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_other<-cbind(
  #pr(vacant)
  exp((mean(multi_out$beta[,2,1])) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_other,
  #pr(other)
  exp((mean(multi_out$beta[,2,2])) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_other,
  #pr(crem)
  exp((mean(multi_out$beta[,2,3])) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_other,
  #pr(liom)
  exp((mean(multi_out$beta[,2,4])) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_other)
sum(pred_other[1,])
## Previously tended by Liom
Denominator_crem <- exp(mean(multi_out$beta[,3,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,3,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))

pred_crem<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,3,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_crem,
  #pr(other)
  exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_crem,
  #pr(crem)
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_crem,
  #pr(liom)
  exp(mean(multi_out$beta[,3,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_crem)
sum(pred_crem[1,])
## Previously tended by Other
Denominator_liom <- exp(mean(multi_out$beta[,4,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,4,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,4,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))

pred_liom<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,4,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_liom,
  #pr(other)
  exp(mean(multi_out$beta[,4,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_liom,
  #pr(crem)
  exp(mean(multi_out$beta[,4,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_liom,
  #pr(liom)
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_liom)
sum(pred_liom[1,])
## vac -> vac       vac -> other    vac -> crem       vac -> liom
pred_probs_vac <- cbind((pred_vac[,1]) , (pred_vac[,2]) , (pred_vac[,3]) , (pred_vac[,4]))
## other-> vac        other -> other    other -> crem       other -> liom
pred_probs_other <- cbind((pred_other[,1]) , (pred_other[,2]) , (pred_other[,3]) , (pred_other[,4]))
## crem-> vac       crem -> other    crem -> crem      crem -> liom
pred_probs_crem <- cbind((pred_crem[,1]) , (pred_crem[,2]) , (pred_crem[,3]) , (pred_crem[,4]))
## liom-> vac       liom -> other    liom -> crem       liom -> liom
pred_probs_liom <- cbind((pred_liom[,1]) , (pred_liom[,2]) , (pred_liom[,3]) , (pred_liom[,4]))

## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_Size_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,2,3,4,5),
              ncol = 2, nrow = 3, byrow = TRUE), heights = c(0.6,1.4,1.4), widths = c(3.9,3.9))
plot.new()
text(0.5,0.5,"Ant States",cex=2,font=2)
plot(size_dummy_real, pred_vac[,1], type = "l", col = "pink",main = "Previously Vacant", ylim = c(0,1))
lines(size_dummy_real, pred_vac[,2], col = "black")
lines(size_dummy_real, pred_vac[,3], col = "red")
lines(size_dummy_real, pred_vac[,4], col = "blue")
plot(size_dummy_real, pred_other[,1], type = "l", col = "pink",main = "Previously Other", ylim = c(0,1))
lines(size_dummy_real, pred_other[,2], col = "black")
lines(size_dummy_real, pred_other[,3], col = "red")
lines(size_dummy_real, pred_other[,4], col = "blue")
legend("topright",c("vacant","other","crem.","liom."), fill = c("pink","black","red","blue"))
plot(size_dummy_real, pred_crem[,1], type = "l", col = "pink",main = "Previously Crem", ylim = c(0,1))
lines(size_dummy_real, pred_crem[,2], col = "black")
lines(size_dummy_real, pred_crem[,3], col = "red")
lines(size_dummy_real, pred_crem[,4], col = "blue")
plot(size_dummy_real, pred_liom[,1], type = "l", col = "pink",main = "Previously Liom", ylim = c(0,1))
lines(size_dummy_real, pred_liom[,2], col = "black")
lines(size_dummy_real, pred_liom[,3], col = "red")
lines(size_dummy_real, pred_liom[,4], col = "blue")
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
####################################### OTHER & CREM ###########################################################
## Calculate the probabilities of being tended by each ant species
## Previously tended by none
Denominator_vac <- exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,1,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,1,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,1,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_vac<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_vac,
  #pr(other)
  exp(mean(multi_out$beta[,1,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_vac,
  #pr(crem)
  exp(mean(multi_out$beta[,1,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_vac,
  #pr(liom)
  exp(mean(multi_out$beta[,1,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_vac)
sum(pred_vac[1,])
## Previously tended by Crem
Denominator_other <- exp(mean(multi_out$beta[,2,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,2,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,2,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,2,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_other<-cbind(
  #pr(vacant)
  exp((mean(multi_out$beta[,2,1])) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_other,
  #pr(other)
  exp((mean(multi_out$beta[,2,2])) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_other,
  #pr(crem)
  exp((mean(multi_out$beta[,2,3])) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_other,
  #pr(liom)
  exp((mean(multi_out$beta[,2,4])) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_other)
sum(pred_other[1,])
## Previously tended by Liom
Denominator_crem <- exp(mean(multi_out$beta[,3,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,3,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))

pred_crem<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,3,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_crem,
  #pr(other)
  exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_crem,
  #pr(crem)
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_crem,
  #pr(liom)
  exp(mean(multi_out$beta[,3,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_crem)
sum(pred_crem[1,])
## Previously tended by Other
Denominator_liom <- exp(mean(multi_out$beta[,4,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,4,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,4,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))

pred_liom<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,4,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_liom,
  #pr(other)
  exp(mean(multi_out$beta[,4,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_liom,
  #pr(crem)
  exp(mean(multi_out$beta[,4,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_liom,
  #pr(liom)
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_liom)
sum(pred_liom[1,])
## vac -> vac       vac -> other    vac -> crem       vac -> liom
pred_probs_vac <- cbind((pred_vac[,1]) , (pred_vac[,2]) , (pred_vac[,3]) , (pred_vac[,4]))
## other-> vac        other -> other    other -> crem       other -> liom
pred_probs_other <- cbind((pred_other[,1]) , (pred_other[,2]) , (pred_other[,3]) , (pred_other[,4]))
## crem-> vac       crem -> other    crem -> crem      crem -> liom
pred_probs_crem <- cbind((pred_crem[,1]) , (pred_crem[,2]) , (pred_crem[,3]) , (pred_crem[,4]))
## liom-> vac       liom -> other    liom -> crem       liom -> liom
pred_probs_liom <- cbind((pred_liom[,1]) , (pred_liom[,2]) , (pred_liom[,3]) , (pred_liom[,4]))

## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_Size_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,2,3,4,5),
              ncol = 2, nrow = 3, byrow = TRUE), heights = c(0.6,1.4,1.4), widths = c(3.9,3.9))
plot.new()
text(0.5,0.5,"Ant States",cex=2,font=2)
plot(size_dummy_real, pred_vac[,1], type = "l", col = "pink",main = "Previously Vacant", ylim = c(0,1))
lines(size_dummy_real, pred_vac[,2], col = "black")
lines(size_dummy_real, pred_vac[,3], col = "red")
lines(size_dummy_real, pred_vac[,4], col = "blue")
plot(size_dummy_real, pred_other[,1], type = "l", col = "pink",main = "Previously Other", ylim = c(0,1))
lines(size_dummy_real, pred_other[,2], col = "black")
lines(size_dummy_real, pred_other[,3], col = "red")
lines(size_dummy_real, pred_other[,4], col = "blue")
legend("topright",c("vacant","other","crem.","liom."), fill = c("pink","black","red","blue"))
plot(size_dummy_real, pred_crem[,1], type = "l", col = "pink",main = "Previously Crem", ylim = c(0,1))
lines(size_dummy_real, pred_crem[,2], col = "black")
lines(size_dummy_real, pred_crem[,3], col = "red")
lines(size_dummy_real, pred_crem[,4], col = "blue")
plot(size_dummy_real, pred_liom[,1], type = "l", col = "pink",main = "Previously Liom", ylim = c(0,1))
lines(size_dummy_real, pred_liom[,2], col = "black")
lines(size_dummy_real, pred_liom[,3], col = "red")
lines(size_dummy_real, pred_liom[,4], col = "blue")
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
####################################### VAC & CREM #############################################################
## Calculate the probabilities of being tended by each ant species
## Previously tended by none
Denominator_vac <- exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,1,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,1,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,1,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_vac<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_vac,
  #pr(other)
  exp(mean(multi_out$beta[,1,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_vac,
  #pr(crem)
  exp(mean(multi_out$beta[,1,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_vac,
  #pr(liom)
  exp(mean(multi_out$beta[,1,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_vac)
sum(pred_vac[1,])
## Previously tended by Crem
Denominator_other <- exp(mean(multi_out$beta[,2,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,2,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,2,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,2,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))
pred_other<-cbind(
  #pr(vacant)
  exp((mean(multi_out$beta[,2,1])) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_other,
  #pr(other)
  exp((mean(multi_out$beta[,2,2])) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_other,
  #pr(crem)
  exp((mean(multi_out$beta[,2,3])) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_other,
  #pr(liom)
  exp((mean(multi_out$beta[,2,4])) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_other)
sum(pred_other[1,])
## Previously tended by Liom
Denominator_crem <- exp(mean(multi_out$beta[,3,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,3,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))

pred_crem<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,3,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_crem,
  #pr(other)
  exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_crem,
  #pr(crem)
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_crem,
  #pr(liom)
  exp(mean(multi_out$beta[,3,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_crem)
sum(pred_crem[1,])
## Previously tended by Other
Denominator_liom <- exp(mean(multi_out$beta[,4,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,4,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,4,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))

pred_liom<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,4,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_liom,
  #pr(other)
  exp(mean(multi_out$beta[,4,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_liom,
  #pr(crem)
  exp(mean(multi_out$beta[,4,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_liom,
  #pr(liom)
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_liom)
sum(pred_liom[1,])
## vac -> vac       vac -> other    vac -> crem       vac -> liom
pred_probs_vac <- cbind((pred_vac[,1]) , (pred_vac[,2]) , (pred_vac[,3]) , (pred_vac[,4]))
## other-> vac        other -> other    other -> crem       other -> liom
pred_probs_other <- cbind((pred_other[,1]) , (pred_other[,2]) , (pred_other[,3]) , (pred_other[,4]))
## crem-> vac       crem -> other    crem -> crem      crem -> liom
pred_probs_crem <- cbind((pred_crem[,1]) , (pred_crem[,2]) , (pred_crem[,3]) , (pred_crem[,4]))
## liom-> vac       liom -> other    liom -> crem       liom -> liom
pred_probs_liom <- cbind((pred_liom[,1]) , (pred_liom[,2]) , (pred_liom[,3]) , (pred_liom[,4]))

## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_Size_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,2,3,4,5),
              ncol = 2, nrow = 3, byrow = TRUE), heights = c(0.6,1.4,1.4), widths = c(3.9,3.9))
plot.new()
text(0.5,0.5,"Ant States",cex=2,font=2)
plot(size_dummy_real, pred_vac[,1], type = "l", col = "pink",main = "Previously Vacant", ylim = c(0,1))
lines(size_dummy_real, pred_vac[,2], col = "black")
lines(size_dummy_real, pred_vac[,3], col = "red")
lines(size_dummy_real, pred_vac[,4], col = "blue")
plot(size_dummy_real, pred_other[,1], type = "l", col = "pink",main = "Previously Other", ylim = c(0,1))
lines(size_dummy_real, pred_other[,2], col = "black")
lines(size_dummy_real, pred_other[,3], col = "red")
lines(size_dummy_real, pred_other[,4], col = "blue")
legend("topright",c("vacant","other","crem.","liom."), fill = c("pink","black","red","blue"))
plot(size_dummy_real, pred_crem[,1], type = "l", col = "pink",main = "Previously Crem", ylim = c(0,1))
lines(size_dummy_real, pred_crem[,2], col = "black")
lines(size_dummy_real, pred_crem[,3], col = "red")
lines(size_dummy_real, pred_crem[,4], col = "blue")
plot(size_dummy_real, pred_liom[,1], type = "l", col = "pink",main = "Previously Liom", ylim = c(0,1))
lines(size_dummy_real, pred_liom[,2], col = "black")
lines(size_dummy_real, pred_liom[,3], col = "red")
lines(size_dummy_real, pred_liom[,4], col = "blue")
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")






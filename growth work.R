setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

################################################################################
##### Null model -- no predictors ##############################################
################################################################################
##### Simulate growth data with a skewed distribution
a <- rsn(n = 1000,xi = 200, omega = 15, alpha = 20)
##### Now load the data s.t. stan may interpret it
stan_data <- list(y = a,              ## the response variable
                  N = length(a)       ## the number of observations
                  )
stan_model <- stan(file = "Data Analysis/STAN Models/stan_prac.stan", 
                   data = stan_data, warmup = 150, iter = 1000, chains = 3, 
                   cores = 3, thin = 1)
##### export the required variables so they may be used
## export
summary(stan_model)
stan_mu <- rstan::extract(stan_model, pars = c("beta0"))$beta0
stan_outputs <- rstan::extract(stan_model, pars = c("alpha","sigma"))
write.csv(stan_outputs, "stan_outputs.csv")
write.csv(stan_mu, "stan_mu.csv")
## format
beta <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/stan_mu.csv", header = TRUE,stringsAsFactors=T)
beta <- beta[,c(-1)]
beta <- as.matrix(beta)
others <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/stan_outputs.csv", header = TRUE,stringsAsFactors=T)
others <- others[,c(-1)]
others <- as.matrix(others)
y_sim <- matrix(NA, 500,length(a))
for(i in 1:500){
  y_sim[i,] <- rsn(n=length(a), xi = (beta[i,]), 
                   omega = (others[i,"sigma"]), 
                   alpha = others[i,"alpha"], dp = NULL)
}
##### visualize the outcomes of the model
## the convergence of the parameters
png("null_convergence_try3.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(stan_model, pars = c("beta0","sigma","alpha")) 
dev.off()
## converges fine
## the ability of the model to match the data
png("null_overlay_try3.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay(as.vector(a), y_sim)
dev.off()
## seems pretty good
##### check the estimates of each of the parameters
mean(beta)
## try one
## input xi is 20
## pretty close (19.773)
## try two
## input xi is 200
## pretty close (200.0814)
## try three
## input xi is 200
## pretty close (199.9757)
mean(others[,"sigma"])
## try one
## input sigma is 5
## pretty close (5.180)
## try two
## input sigma is 15
## pretty close (15.0691)
## try three
## input sigma is 15
## pretty close (15.31448)
mean(others[,"alpha"])
## try one
## input alpha is 2
## pretty close (2.058173)
## try two 
## input alpha is 200
## not that great (114.0239)
## try three 
## input alpha is 20
## not that great (19.0722)

#########################################################################
###### Now do it with real data #########################################
#########################################################################
growth_data_orig <- cactus[,c("Plot","Year_t","logsize_t","logsize_t1","ant_t")]
growth_data <- na.omit(growth_data_orig)
stan_data <- list(y = (growth_data$logsize_t1),              ## the response variable
                  N = nrow(growth_data),                     ## the number of observations
                  vol = (growth_data$logsize_t)             ## predictors volume
)
stan_model <- stan(file = "Data Analysis/STAN Models/stan_prac.stan", 
                   data = stan_data, warmup = 150, iter = 1000, chains = 3, 
                   cores = 3, thin = 1)
list(N = nrow(growth_data), ## number of observations
     vol = (growth_data$logsize_t), ## predictors volume
     y_grow = (growth_data$logsize_t1), ## response survival next year
     ant = as.integer(as.factor(growth_data$ant_t)),## predictors ants
     K = 4, ## number of ant states
     N_Year = max(as.integer(as.factor(growth_data$Year_t))), ## number of years
     N_Plot = max(as.integer(as.factor(growth_data$Plot))), ## number of plots
     plot = as.integer(as.factor(growth_data$Plot)), ## predictor plots
     year = as.integer(as.factor(growth_data$Year_t)) ## predictor years
)
summary(stan_model)
stan_mu <- rstan::extract(stan_model, pars = c("beta0"))$beta0
stan_outputs <- rstan::extract(stan_model, pars = c("alpha","sigma"))
stan_yrep <- rstan::extract(stan_model, pars = c("y_rep"))$y_rep
write.csv(stan_outputs, "stan_outputs.csv")
write.csv(stan_mu, "stan_mu.csv")
write.csv(stan_yrep, "stan_yrep.csv")
## format
beta <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/stan_mu.csv", header = TRUE,stringsAsFactors=T)
beta <- beta[,c(-1)]
beta <- as.matrix(beta)
others <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/stan_outputs.csv", header = TRUE,stringsAsFactors=T)
others <- others[,c(-1)]
others <- as.matrix(others)
y_rep <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/stan_yrep.csv", header = TRUE,stringsAsFactors=T)
a <- y_rep[1:500,1:length(y)]
y_sim <- matrix(NA, 500,length(y))
for(i in 1:500){
  y_sim[i,] <- rsn(n=length(y), xi = (beta[i,]*mean(stan_data$vol)), 
                   omega = (others[i,"sigma"]), 
                   alpha = others[i,"alpha"], dp = NULL)
}
##### visualize the outcomes of the model
## the convergence of the parameters
png("null_convergence_data2.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(stan_model, pars = c("beta0","sigma","alpha")) 
dev.off()
## converges fine
## the ability of the model to match the data
png("null_overlay_data2.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay(as.vector(y), y_rep)
dev.off()

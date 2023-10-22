################################################################################
## Growth model
################################################################################
## Visualize the outputs of the model -- trace plots to check conve4rgence, data moments to check fit
fit_grow_stud@model_pars
bayesplot::mcmc_trace(fit_grow_stud,pars=c("d_0","d_size","a_0","a_size"))
bayesplot::mcmc_trace(fit_grow_stud,pars=c("beta0[1]","beta0[2]","beta0[3]","beta0[4]"))
bayesplot::mcmc_trace(fit_grow_stud,pars=c("beta1[1]","beta1[2]","beta1[3]","beta1[4]"))
bayesplot::mcmc_trace(fit_grow_stud,pars=c("beta2[1]","beta2[2]","beta2[3]","beta2[4]"))
# Check the different quantile fits of the model to make sure not only the mean but also other quantiles fit well with the real data
q.fit<-matrix(NA,7,length(stan_data_grow_stud$vol))
q.fit[1,]<-predict(qgam(y~s(vol),qu=0.05,data=data.frame(y=stan_data_grow_stud$y,vol=stan_data_grow_stud$vol)))
q.fit[2,]<-predict(qgam(y~s(vol),qu=0.10,data=data.frame(y=stan_data_grow_stud$y,vol=stan_data_grow_stud$vol)))
q.fit[3,]<-predict(qgam(y~s(vol),qu=0.25,data=data.frame(y=stan_data_grow_stud$y,vol=stan_data_grow_stud$vol)))
q.fit[4,]<-predict(qgam(y~s(vol),qu=0.5,data=data.frame(y=stan_data_grow_stud$y,vol=stan_data_grow_stud$vol)))
q.fit[5,]<-predict(qgam(y~s(vol),qu=0.75,data=data.frame(y=stan_data_grow_stud$y,vol=stan_data_grow_stud$vol)))
q.fit[6,]<-predict(qgam(y~s(vol),qu=0.90,data=data.frame(y=stan_data_grow_stud$y,vol=stan_data_grow_stud$vol)))
q.fit[7,]<-predict(qgam(y~s(vol),qu=0.95,data=data.frame(y=stan_data_grow_stud$y,vol=stan_data_grow_stud$vol)))
obs_mean<-Q.mean(q.fit[3,],q.fit[4,],q.fit[5,])
obs_sd<-Q.sd(q.fit[3,],q.fit[5,])
obs_skew<-Q.skewness(q.fit[2,],q.fit[4,],q.fit[6,])
obs_kurt<-Q.kurtosis(q.fit[1,],q.fit[3,],q.fit[5,],q.fit[7,])
plot(stan_data_grow_stud$vol,stan_data_grow_stud$y,pch=".",col="red")
points(stan_data_grow_stud$vol,q.fit[1,],col="black",pch=".")
points(stan_data_grow_stud$vol,q.fit[2,],col="black",pch=".")
points(stan_data_grow_stud$vol,q.fit[3,],col="black",pch=".")
points(stan_data_grow_stud$vol,q.fit[4,],col="black",pch=".")
points(stan_data_grow_stud$vol,q.fit[5,],col="black",pch=".")
points(stan_data_grow_stud$vol,q.fit[6,],col="black",pch=".")
points(stan_data_grow_stud$vol,q.fit[7,],col="black",pch=".")
# simulate data to compare simulated data to the real data and check your estimates
grow_params <- rstan::extract(fit_grow_stud)
fit_grow_stud@model_pars
n_draws=25
grow_sim<-matrix(NA,n_draws,stan_data_grow_stud$N)
sim_mean<-sim_sd<-sim_skew<-sim_kurt<-matrix(NA,n_draws,stan_data_grow_stud$N)
for(i in 1:n_draws){
  for(n in 1:stan_data_grow_stud$N){
    grow_sim[i,n]<-rlst(n=1,
                        mu=grow_params$beta0[i,stan_data_grow_stud$ant[n]]+
                          grow_params$beta1[i,stan_data_grow_stud$ant[n]]*stan_data_grow_stud$vol[n]+
                          grow_params$beta2[i,stan_data_grow_stud$ant[n]]*stan_data_grow_stud$vol2[n]+
                          grow_params$u[i,stan_data_grow_stud$plot[n]]+
                          grow_params$w[i,stan_data_grow_stud$ant[n],stan_data_grow_stud$year[n]],
                        sigma=exp(grow_params$d_0[i]+grow_params$d_size[i]*stan_data_grow_stud$vol[n]),
                        df=grow_params$a_0[i]+grow_params$a_size[i]*stan_data_grow_stud$vol[n])
  }
  q.fit[1,]<-predict(qgam(y~s(vol),qu=0.05,data=data.frame(y=grow_sim[i,],vol=stan_data_grow_stud$vol)))
  q.fit[2,]<-predict(qgam(y~s(vol),qu=0.10,data=data.frame(y=grow_sim[i,],vol=stan_data_grow_stud$vol)))
  q.fit[3,]<-predict(qgam(y~s(vol),qu=0.25,data=data.frame(y=grow_sim[i,],vol=stan_data_grow_stud$vol)))
  q.fit[4,]<-predict(qgam(y~s(vol),qu=0.5,data=data.frame(y=grow_sim[i,],vol=stan_data_grow_stud$vol)))
  q.fit[5,]<-predict(qgam(y~s(vol),qu=0.75,data=data.frame(y=grow_sim[i,],vol=stan_data_grow_stud$vol)))
  q.fit[6,]<-predict(qgam(y~s(vol),qu=0.90,data=data.frame(y=grow_sim[i,],vol=stan_data_grow_stud$vol)))
  q.fit[7,]<-predict(qgam(y~s(vol),qu=0.95,data=data.frame(y=grow_sim[i,],vol=stan_data_grow_stud$vol)))
  sim_mean[i,]<-Q.mean(q.fit[3,],q.fit[4,],q.fit[5,])
  sim_sd[i,]<-Q.sd(q.fit[3,],q.fit[5,])
  sim_skew[i,]<-Q.skewness(q.fit[2,],q.fit[4,],q.fit[6,])
  sim_kurt[i,]<-Q.kurtosis(q.fit[1,],q.fit[3,],q.fit[5,],q.fit[7,])
  print(i/n_draws)
}
# plot the simulated data
plot(stan_data_grow_stud$vol,grow_sim[1,],pch=".",col="red")
points(stan_data_grow_stud$vol,stan_data_grow_stud$y,pch=".",col="black")
# plot overlays to check the fit in a second way
bayesplot::ppc_dens_overlay(stan_data_grow_stud$y, grow_sim)
# plot the mean fit compared to the observed mean
matplot(stan_data_grow_stud$vol,t(sim_mean),pch=".",col="gray")
points(stan_data_grow_stud$vol,obs_mean)
# plot the sd fit compared to the observed sd
matplot(stan_data_grow_stud$vol,t(sim_sd),pch=".",col="gray")
points(stan_data_grow_stud$vol,obs_sd)
# plot the skew fit compared to the observed skew
matplot(stan_data_grow_stud$vol,t(sim_skew),pch=".",col="gray")
points(stan_data_grow_stud$vol,obs_skew)
# plot the kurt fit compared to the observed kurt
matplot(stan_data_grow_stud$vol,t(sim_kurt),pch=".",col="gray")
points(stan_data_grow_stud$vol,obs_kurt)


################################################################################
## Survival Model
################################################################################
## Visualize the outputs of the model -- trace plots to check convergence, overlay plots to check the fit
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
# simulate data based on model fits
y <- stan_data_surv$y_surv
ant <- stan_data_surv$ant
surv_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/surv.params.csv", header = TRUE,stringsAsFactors=T)
surv_data <- surv_data[,c(-1)]
surv_data <- as.matrix(surv_data)
surv_mu <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/surv.mu.csv", header = TRUE,stringsAsFactors=T)
surv_mu <- surv_mu[,c(-1)]
surv_mu <- as.matrix(surv_mu)
y_sim <- matrix(NA, 1000,length(y))
for(i in 1:1000){
  y_sim[i,] <- rbinom(n=length(y), size=1, prob = invlogit(mean(surv_mu[i,])))
}
view(y_sim)
# Overlay Plots
png(file = "surv_post.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay_grouped(y, y_sim,group = ant)
dev.off()
# Convergence Plots
png(file = "surv_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_surv, pars=c("beta0","beta1")))
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")






################################################################################
## Flowering Model
################################################################################
# flow_outputs <- rstan::extract(fit_flow_trunc, pars = c("w","beta0","beta1","u","sigma_w","sigma_u"))
# flow_outputs <- as.data.frame(flow_outputs)
# ## pull 1000 random rows from the data frame and export it
# flow.params <- flow_outputs[draws,]
# write.csv(flow.params, "flow.params.csv")
# #### Phi
# flow_phi <- rstan::extract(fit_flow_trunc, pars = c("phi"))
# flow_phi <- as.data.frame(flow_phi)
# ## pull 1000 random rows from the data frame and export it
# flow.phi <- flow_phi[draws,]
# write.csv(flow.phi, "flow.phi.csv")
# #### Mu
# flow_mu <- rstan::extract(fit_flow_trunc, pars = c("mu"))
# flow_mu <- as.data.frame(flow_mu)
# ## pull 1000 random rows from the data frame and export it
# flow.mu <- flow_mu[draws,]
# write.csv(flow.mu, "flow.mu.csv")
# ## Check the posterior distributions
# y <- flower_data$TotFlowerbuds_t
# #flow_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/flow.params.csv", header = TRUE,stringsAsFactors=T)
# flow_data <- read.csv("flow.params.csv", header = TRUE,stringsAsFactors=T)
# # Create the y rep (needs to be done outside of STAN because of the 0 truncation)
# y_sim <- matrix(NA,1000,length(y))
# for(i in 1:1000){
#   for(j in 1:length(y)){
#     y_sim[i,j] <- sample(x=1:1000,size=1,replace=T,prob=dnbinom(1:1000, mu = exp(flow.mu[i,j]), size=flow.phi[i]) / (1 - dnbinom(0, mu = exp(flow.mu[i,j]), size=flow.phi[i])))
#   }
# }
# ## Plot the posterior distributions
# setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
# png("flow_post.png")
# bayesplot::color_scheme_set(scheme = "pink")
# bayesplot::ppc_dens_overlay(y, y_sim)
# dev.off()
# ## Convergence Plots
# png(file = "flow_conv.png")
# bayesplot::color_scheme_set(scheme = "pink")
# bayesplot::mcmc_trace(As.mcmc.list(fit_flow_trunc, pars=c("beta0", "beta1")))
# dev.off()
# setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
# 
# 
# 
# 
# 
# 
# 
# 

################################################################################
## Viability Model
################################################################################
 
# 
# 
# fit_viab <- stan(file = "Data Analysis/STAN Models/viab_code.stan", data = stan_data_viab, warmup = 1500, iter = 10000, chains = 3, cores = 3, thin = 1)
# fit_viab@model_pars
# draws<-sample(nrow(surv_outputs),1000)
# ## list all parameters
# ## pull all iterations for parameters and save as a data frame
# viab_outputs <- rstan::extract(fit_viab, pars = c("w","beta0","u","sigma_w","sigma_u"))
# viab_outputs <- as.data.frame(viab_outputs)
# ## pull 1000 random rows from the data frame and export it
# viab.params <- viab_outputs[draws,]
# write.csv(viab.params, "viab.params.csv")
# ## pull all iterations for parameters and save as a data frame
# viab_sigma <- rstan::extract(fit_viab, pars = c("sigma"))
# viab_sigma <- as.data.frame(viab_sigma)
# ## pull 1000 random rows from the data frame and export it
# viab.sigma <- viab_sigma[draws,]
# write.csv(viab.sigma, "viab.sigma.csv")
# ## pull all iterations for parameters and save as a data frame
# viab_mu <- rstan::extract(fit_viab, pars = c("mu"))
# viab_mu <- as.data.frame(viab_mu)
# ## pull 1000 random rows from the data frame and export it
# viab.mu <- viab_mu[draws,]
# write.csv(viab.mu, "viab.mu.csv")
# 
# ## Check the Posterior Distribution
# y <- viability_data$Goodbuds_t1
# #viab_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/viab.params.csv", header = TRUE,stringsAsFactors=T)
# viab_data <- read.csv("viab.params.csv", header = TRUE,stringsAsFactors=T)
# viab_data <- viab_data[,c(-1)]
# viab_data <- as.matrix(viab_data)
# #viab_mu <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/viab.mu.csv", header = TRUE,stringsAsFactors=T)
# viab_mu <- read.csv("viab.mu.csv", header = TRUE,stringsAsFactors=T)
# viab_mu <- viab_mu[,c(-1)]
# viab_mu <- as.matrix(viab_mu)
# y_sim <- matrix(NA,1000,length(y))
# for(i in 1:1000){
#   y_sim[i,] <- rbern(n = length(y), prob = invlogit(mean(viab_mu[i,])))
# }
# setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
# ## Overlay Plots
# png(file = "viab_post.png")
# bayesplot::color_scheme_set(scheme = "pink")
# bayesplot::ppc_dens_overlay(y, y_sim)
# dev.off()
# png(file = "viab_ant_post.png")
# bayesplot::color_scheme_set(scheme = "pink")
# bayesplot::ppc_dens_overlay_grouped(y, y_sim,group = as.integer(as.factor(viability_data$ant)))
# dev.off()
# ## Convergence Plots
# png(file = "viab_conv.png")
# bayesplot::color_scheme_set(scheme = "pink")
# bayesplot::mcmc_trace(As.mcmc.list(fit_viab, pars=c("beta0")))
# dev.off()
# setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

###### Check the significance of the differences between survival rates
## create data set where each column is an estimated survival rate
viab_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/viab.params.csv", header = TRUE,stringsAsFactors=T)
crem_est <- (invlogit(viab_out$beta0.3))
liom_est <- (invlogit(viab_out$beta0.4))
other_est <- (invlogit(viab_out$beta0.2))
vac_est <- (invlogit(viab_out$beta0.1))
estimates <- cbind(crem_est, liom_est, other_est, vac_est)
## crem and liom -- p = 2.2 e-16  ***
t.test(estimates[,1],estimates[,2], alternative = "two.sided")
## crem and other -- p = 0.1168
t.test(estimates[,1],estimates[,3], alternative = "two.sided")
## crem and vac -- p = 2.2e-16    ***
t.test(estimates[,1],estimates[,4], alternative = "two.sided")
## liom and other -- p = 2.2 e-16
t.test(estimates[,2],estimates[,3], alternative = "two.sided")
## liom and vac -- p = 2.2 e-16.  ***
t.test(estimates[,2],estimates[,4], alternative = "two.sided")
## other and vac -- p = 2.2 e-16. ***
t.test(estimates[,3],estimates[,4], alternative = "two.sided")

################################################################################
## Repro
################################################################################

## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/repro_mix_ant.stan")
# fit_repro <- stan(file = "Data Analysis/STAN Models/repro_code.stan", data = stan_data_repro, warmup = 1500, iter = 10000, chains = 3, cores = 3, thin = 1)
# fit_repro@model_pars
# ########## extract the parameters from the model and save a random selection of the iterations
# ## list all parameters
# ## pull all iterations for parameters and save as a data frame
# repro_outputs <- rstan::extract(fit_repro, pars = c("w","beta0","beta1","u","sigma_w","sigma_u"))
# repro_outputs <- as.data.frame(repro_outputs)
# ## pull 1000 random rows from the data frame and export it
# draws<-sample(nrow(repro_outputs),1000)
# repro.params <- repro_outputs[draws,]
# write.csv(repro.params, "repro.params.csv")
# ## Mu
# ## pull all iterations for parameters and save as a data frame
# repro_mu <- rstan::extract(fit_repro, pars = c("mu"))
# repro_mu <- as.data.frame(repro_mu)
# ## pull 1000 random rows from the data frame and export it
# repro.mu <- repro_mu[draws,]
# write.csv(repro.mu, "repro.mu.csv")
# ## Sigma
# ## pull all iterations for parameters and save as a data frame
# repro_sigma <- rstan::extract(fit_repro, pars = c("sigma"))
# repro_sigma <- as.data.frame(repro_sigma)
# ## pull 1000 random rows from the data frame and export it
# repro.sigma <- repro_sigma[draws,]
# write.csv(repro.sigma, "repro.sigma.csv")
# ## Check the Posteriors
# #repro_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/repro.params.csv", header = TRUE,stringsAsFactors=T)
# repro_data <- read.csv("repro.params.csv", header = TRUE,stringsAsFactors=T)
# repro_data <- repro_data[,c(-1)]
# repro_data <- as.matrix(repro_data)
# #repro_mu <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/repro.mu.csv", header = TRUE,stringsAsFactors=T)
# repro_mu <- read.csv("repro.mu.csv", header = TRUE,stringsAsFactors=T)
# repro_mu <- repro_mu[,c(-1)]
# repro_mu <- as.matrix(repro_mu)
# y <- as.numeric(reproductive_data$flower1_YN)
# y_sim <- matrix(NA,1000,length(y))
# for(i in 1:1000){
#   y_sim[i,] <- rbern(n = length(y), prob = invlogit(mean(repro_mu[i,])))
# }
# ## Overlay Plots
# setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
# png(file = "repro_post.png")
# bayesplot::ppc_dens_overlay(y, y_sim)
# dev.off()
# ## Convergence Plots
# png(file = "repro_conv.png")
# bayesplot::mcmc_trace(As.mcmc.list(fit_repro, pars=c("beta0","beta1","sigma_u","sigma_w")))
# dev.off()
# setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
# 


################################################################################
## Seed sper flower
################################################################################
## Run the model
# fit_seed <- stan(file = "Data Analysis/STAN Models/seed_code.stan", data = stan_data_seed, warmup = 1500, iter = 10000, chains = 3, cores = 3, thin = 1)
# fit_seed@model_pars
# ## pull all iterations for parameters and save as a data frame
# seed_outputs <- rstan::extract(fit_seed, pars = c("beta0"))
# seed_outputs <- as.data.frame(seed_outputs)
# ## pull 1000 random rows from the data frame and export it
# seed.params <- seed_outputs[draws,]
# write.csv(seed.params, "seed.params.csv")
# ## mu
# seed_mu <- rstan::extract(fit_seed, pars = c("mu"))
# seed_mu <- as.data.frame(seed_mu)
# ## pull 1000 random rows from the data frame and export it
# seed.mu <- seed_mu[draws,]
# write.csv(seed.mu, "seed.mu.csv")
# ## sigma
# seed_sigma <- rstan::extract(fit_seed, pars = c("sigma"))
# seed_sigma <- as.data.frame(seed_sigma)
# ## pull 1000 random rows from the data frame and export it
# seed.sigma <- seed_sigma[draws,]
# write.csv(seed.sigma, "seed.sigma.csv")
# ## phi
# seed_phi <- rstan::extract(fit_seed, pars = c("phi"))
# seed_phi <- as.data.frame(seed_phi)
# ## pull 1000 random rows from the data frame and export it
# seed.phi <- seed_phi[draws,]
# write.csv(seed.phi, "seed.phi.csv")
# ## Check the Posteriors
# setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
# seed_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/seed.params.csv", header = TRUE,stringsAsFactors=T)
# seed_data <- seed_data[,c(-1)]
# seed_data <- as.matrix(seed_data)
# seed_mu <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/seed.mu.csv", header = TRUE,stringsAsFactors=T)
# seed_mu <- seed_mu[,c(-1)]
# seed_mu <- as.matrix(seed_mu)
# seed_phi <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/seed.phi.csv", header = TRUE,stringsAsFactors=T)
# seed_phi <- seed_phi[,c(-1)]
# seed_phi <- as.matrix(seed_phi)
# y <- stan_data_seed$seed
# ant = stan_data_seed$ant
# y_sim <- matrix(NA,1000,length(y))
# for(i in 1:1000){
#   y_sim[i,] <- rnegbin(n = length(y), mu = seed_mu[i,], theta = seed_phi[i,])
# }
# ## Overlay Plots
# png(file = "seed_post.png")
# bayesplot::ppc_dens_overlay(y, y_sim)
# dev.off()
# png(file = "seed_ant_post.png")
# bayesplot::ppc_dens_overlay_grouped(y, y_sim,group = ant)
# dev.off()
# ## Convergence Plots
# png(file = "seed_conv.png")
# bayesplot::mcmc_trace(As.mcmc.list(fit_seed, pars=c("beta0")))
# dev.off()
# 
# ###### Check the significance of the differences between survival rates
# ## create data set where each column is an estimated survival rate
# seed_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/seed.params.csv", header = TRUE,stringsAsFactors=T)
# crem_est <- (exp(viab_out$beta0.1))
# liom_est <- (exp(viab_out$beta0.2))
# vac_est <- (exp(viab_out$beta0.3))
# estimates <- cbind(crem_est, liom_est, vac_est)
# ## crem and liom -- p = 2.2 e-16  *** 
# t.test(estimates[,1],estimates[,2], alternative = "two.sided")
# ## crem and vac -- p = 2.2e-16    ***
# t.test(estimates[,1],estimates[,3], alternative = "two.sided")
# ## liom and vac -- p = 0.02521 
# t.test(estimates[,2],estimates[,3], alternative = "two.sided")
# 
# setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")



################################################################################
## Precensus Survival
################################################################################
## Run the model
# fit_seed_surv <- stan(file = "Data Analysis/STAN Models/seed_surv_code.stan", data = stan_data_seed_surv, warmup = 1500, iter = 10000, chains = 3, cores = 3, thin = 1)
# fit_seed_surv@model_pars
# ## pull all iterations for parameters and save as a data frame
# seed_surv_outputs <- rstan::extract(fit_seed_surv, pars = c("beta0","w","sigma_w","mu","sigma"))
# seed_surv_outputs <- as.data.frame(seed_surv_outputs)
# ## pull 1000 random rows from the data frame and export it
# seed.surv.params <- seed_surv_outputs[draws,]
# write.csv(seed.surv.params, "seed.surv.params.csv")
# ## yrep
# seed_surv_yrep <- rstan::extract(fit_seed_surv, pars = c("y_rep"))
# seed_surv_yrep <- as.data.frame(seed_surv_yrep)
# ## pull 1000 random rows from the data frame and export it
# seed.surv.yrep <- seed_surv_yrep[draws,]
# write.csv(seed.surv.yrep, "seed.surv.yrep.csv")
# summary(fit_seed_surv)[1]
# ## Check the Posteriors
# setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
# y <- precensus.dat$survive0405
# seed_surv_mu <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/seed.surv.yrep.csv", header = TRUE,stringsAsFactors=T)
# seed_surv_mu <- seed_surv_mu[,c(-1)]
# seed_surv_mu <- as.matrix(seed_surv_mu)
# ## Overlay Plots
# png(file = "seed_surv_post.png")
# bayesplot::ppc_dens_overlay(y, seed_surv_mu)
# dev.off()
# ## Convergence Plots
# png(file = "seed_surv_conv.png")
# bayesplot::mcmc_trace(As.mcmc.list(fit_seed_surv, pars=c("beta0")))
# dev.off()
# setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
# 


################################################################################
## 
################################################################################


























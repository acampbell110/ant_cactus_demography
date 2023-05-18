#setwd("C:/Users/tm9/Dropbox/github/ant_cactus_demography")
#######################################################################################################
##
##                  The purpose of this file is to run each vital rate sub model separately,
##                          save the outputs, and check the posterior distributions 
##
#######################################################################################################
cactus <- read.csv("cholla_demography_20042021_cleaned.csv", header = TRUE,stringsAsFactors=T)
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

##############################################################################################
#### Growth Model -- What size will the cacti be next time step? #############################
##############################################################################################
# growth_data_orig <- cactus[,c("Plot","Year_t","logsize_t","logsize_t1","ant_t")]
# growth_data <- na.omit(growth_data_orig)
# ## Lose 2032 rows (due to plant death & recruit status)
# nrow(growth_data_orig)
# nrow(growth_data)
# # check that you are happy with the subsetting
# plot(growth_data$logsize_t, growth_data$logsize_t1)
# points((cactus$logsize_t), (cactus$logsize_t1), col = "red")
# levels(growth_data$ant_t)
# ## Create Stan Data for all ant states
# stan_data_grow <- list(N = nrow(growth_data), ## number of observations
#                        vol = (growth_data$logsize_t), ## predictors volume
#                        y_grow = (growth_data$logsize_t1), ## response survival next year
#                        ant = as.integer(as.factor(growth_data$ant_t)),## predictors ants
#                        K = 4, ## number of ant states
#                        N_Year = max(as.integer(as.factor(growth_data$Year_t))), ## number of years
#                        N_Plot = max(as.integer(as.factor(growth_data$Plot))), ## number of plots
#                        plot = as.integer(as.factor(growth_data$Plot)), ## predictor plots
#                        year = as.integer(as.factor(growth_data$Year_t)) ## predictor years
# )
# ########## growth model includes sd variance across size and year as a random effect
# fit_grow <- stan(file = "Data Analysis/STAN Models/grow.stan", data = stan_data_grow, warmup = 150, iter = 1000, chains = 3, cores = 3, thin = 1)
# grow_outputs <- rstan::extract(fit_grow, pars = c("w","beta0","beta1","u","d_0","d_size","sigma_w","sigma_u"))
# grow_yrep <- rstan::extract(fit_grow, pars = c("mu"))$mu
# grow_sigma <- rstan::extract(fit_grow, pars = c("sigma"))$sigma
# write.csv(grow_outputs, "grow_outputs.csv")
# write.csv(grow_yrep, "grow_yrep.csv")
# write.csv(grow_sigma, "grow_sigma.csv")
# summary(fit_grow)
# ## Check the posterior distributions
# setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
# #For overlay plots
# n_post_draws <- 100
# post_draws <- sample.int(dim(fit_grow)[1], n_post_draws)
# y <- stan_data_grow$y_grow
# ant <- stan_data_grow$ant
# grow_outputs <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow_outputs.csv", header = TRUE,stringsAsFactors=T)
# grow_yrep <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow_yrep.csv", header = TRUE,stringsAsFactors=T)
# grow_sigma <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow_sigma.csv", header = TRUE,stringsAsFactors=T)
# y_sim <- matrix(NA, n_post_draws,length(y))
# for(i in 1:n_post_draws){
#   y_sim[i,] <- rnorm(n=length(y), mean = mean(grow_yrep[i,]), sd = grow_sigma[i,])
# }
# samp100 <- sample(nrow(y_sim), 100)
# ## Overlay Plots
# png(file = "grow_post_full.png")
# bayesplot::ppc_dens_overlay_grouped(y, y_sim[samp100,], group = ant)
# dev.off()
# ## Convergence Plots
# png("grow_conv_full.png")
# bayesplot::mcmc_trace(As.mcmc.list(fit_grow, pars=c("beta0", "beta1")))
# dev.off()
# ## They all converge
# ## Histograms
# png("grow_hist_post_full.png")
# bayesplot::ppc_stat_grouped(y, y_sim[samp100,], stat = "mean",group = ant)
# dev.off()
# #### Skew, Kurtosis, ETC.
# png("grow_skew_kurt_full.png")
# size_moments_ppc(growth_data, 
#                  "logsize_t1", 
#                  y_sim, 
#                  n_bins = 10,
#                  "Growth")
# dev.off()
# setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
##############################################################################################
#### Skew Growth Model -- What size will the cacti be next time step? #############################
##############################################################################################
growth_data_orig <- cactus[,c("Plot","Year_t","logsize_t","logsize_t1","ant_t")]
growth_data <- na.omit(growth_data_orig)
## Lose 2032 rows (due to plant death & recruit status)
nrow(growth_data_orig)
nrow(growth_data)
# check that you are happy with the subsetting
plot(growth_data$logsize_t, growth_data$logsize_t1)
points((cactus$logsize_t), (cactus$logsize_t1), col = "red")
levels(growth_data$ant_t)
## Create Stan Data for all ant states
stan_data_grow_skew <- list(N = nrow(growth_data), ## number of observations
                       vol = (growth_data$logsize_t-mean(growth_data$logsize_t)), ## predictors volume (because of the - constant need to subrtract that back out when recreating the beta function)
                       y = (growth_data$logsize_t1), ## response survival next year
                       ant = as.integer(as.factor(growth_data$ant_t)),## predictors ants
                       K = 4, ## number of ant states
                       N_Year = max(as.integer(as.factor(growth_data$Year_t))), ## number of years
                       N_Plot = max(as.integer(as.factor(growth_data$Plot))), ## number of plots
                       plot = as.integer(as.factor(growth_data$Plot)), ## predictor plots
                       year = as.integer(as.factor(growth_data$Year_t)) ## predictor years
)
########## growth model includes sd variance across size and year as a random effect
fit_grow_skew <- stan(file = "Data Analysis/STAN Models/grow_skew.stan", data = stan_data_grow_skew, warmup = 1000, iter = 5000, chains = 3, cores = 3, thin = 2)
bayesplot::mcmc_trace(fit_grow_skew,pars=c("a_0","a_size","d_0","d_size","sigma_w","sigma_u"))

grow_outputs_skew <- rstan::extract(fit_grow_skew, pars = c("w","beta0","beta1","u","d_0","d_size","a_0","a_size","sigma_w","sigma_u"))
write.csv(grow_outputs_skew, "grow_outputs_skew.csv")
grow_xi_skew <- rstan::extract(fit_grow_skew, pars = c("xi"))
grow_omega_skew <- rstan::extract(fit_grow_skew, pars = c("omega"))
grow_alpha_skew <- rstan::extract(fit_grow_skew,pars = c("alpha"))
write.csv(grow_xi_skew, "grow_xi_skew.csv")
write.csv(grow_omega_skew, "grow_omega_skew.csv")
write.csv(grow_alpha_skew, "grow_alpha_skew.csv")
summary(fit_grow_skew)
## Check the posterior distributions
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
#For overlay plots
n_post_draws <- 100
post_draws <- sample.int(dim(fit_grow_skew)[1], n_post_draws)
y <- stan_data_grow_skew$y
ant <- stan_data_grow_skew$ant
outputs <- grow_outputs_skew
outputs <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow_outputs_skew.csv", header = TRUE,stringsAsFactors=T)
outputs <- outputs[,c(-1)]
outputs <- as.matrix(outputs)
xi <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow_xi_skew.csv", header = TRUE,stringsAsFactors=T)
#xi <- read.csv("/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow_xi_skew.csv", header = TRUE,stringsAsFactors=T)
xi <- xi[,c(-1)]
xi <- as.matrix(xi)
omega <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow_omega_skew.csv", header = TRUE,stringsAsFactors=T)
#omega <- read.csv("/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow_omega_skew.csv", header = TRUE,stringsAsFactors=T)
omega <- omega[,c(-1)]
omega <- as.matrix(omega)
alpha <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow_alpha_skew.csv", header = TRUE,stringsAsFactors=T)
#alpha <- read.csv("/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow_alpha_skew.csv", header = TRUE,stringsAsFactors=T)
alpha <- alpha[,c(-1)]
alpha <- as.matrix(alpha)
n_post_draws = 100
y_sim <- matrix(NA, n_post_draws,length(y))
for(i in 1:n_post_draws){
  y_sim[i,] <- rsn(n=length(y), xi = (xi[i,]), omega = (omega[i,]), alpha = alpha[i,])
}
View(y_sim)
samp100 <- sample(nrow(y_sim), 100)
## Overlay Plots
png(file = "grow_post.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay_grouped(y, y_sim[samp100,], group = ant)
dev.off()
## Convergence Plots
png("grow_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_grow_skew, pars=c("beta0", "beta1","a_0","a_size","d_0","d_size")))
dev.off()
## They all converge
## Histograms
png("grow_hist_post.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_stat_grouped(y, y_sim[samp100,], stat = "mean",group = ant)
dev.off()
#### Skew, Kurtosis, ETC.
png("grow_moments.png")
size_moments_ppc(growth_data, 
                 "logsize_t1", 
                 y_sim[samp100,], 
                 n_bins = 10,
                 "Growth")
dev.off()

Lkurtosis=function(x) log(kurtosis(x)); 
size_moments_ppc <- function(data,y_name,sim, n_bins, title = NA){
  require(tidyverse)
  require(patchwork)
  growth_data$logsize_t1 <- growth_data[["logsize_t1"]]
  bins <- growth_data %>%
    ungroup() %>% 
    arrange(logsize_t) %>% 
    mutate(size_bin = cut_number(logsize_t, 10)) %>% 
    group_by(size_bin)  %>% 
    dplyr::summarize(mean_t1 = mean(logsize_t1),
                     sd_t1 = sd(logsize_t1),
                     skew_t1 = skewness(logsize_t1),
                     kurt_t1 = Lkurtosis(logsize_t1),
                     bin_mean = mean(logsize_t),
                     bin_n = n())
  sim_moments <- bind_cols(enframe(growth_data$logsize_t), as_tibble(t(y_sim))) %>%
    rename(logsize_t = value) %>%
    arrange(logsize_t) %>%
    mutate(size_bin = cut_number(logsize_t, 10)) %>%
    pivot_longer(., cols = starts_with("V"), names_to = "post_draw", values_to = "y_sim") %>%
    group_by(size_bin, post_draw) %>%
    summarize( mean_sim = mean((y_sim)),
               sd_sim = sd((y_sim)),
               skew_sim = skewness((y_sim)),
               kurt_sim = Lkurtosis((y_sim)),
               bin_mean = mean(logsize_t),
               bin_n = n())
  sim_medians <- sim_moments %>%
    group_by(size_bin, bin_mean) %>%
    summarize(median_mean_sim = median(mean_sim),
              median_sd_sim = median(sd_sim),
              median_skew_sim = median(skew_sim),
              median_kurt_sim = median(kurt_sim))
  meanplot <-  ggplot(data = bins)+
    geom_point(data = sim_moments, aes(x = bin_mean, y = mean_sim), color = "pink") +
    geom_point(data = sim_medians, aes(x = bin_mean, y = median_mean_sim),shape = 1, color = "black") +
    geom_point(aes(x = bin_mean, y = mean_t1), shape = 1, color = "gray72") +
    theme_classic()
  sdplot <-  ggplot(data = bins)+
    geom_point(data = sim_moments, aes(x = bin_mean, y = sd_sim), color = "pink") +
    geom_point(data = sim_medians, aes(x = bin_mean, y = median_sd_sim),shape = 1, color = "black") +
    geom_point(aes(x = bin_mean, y = sd_t1), shape = 1, color = "gray72") + theme_classic()
  skewplot <-  ggplot(data = bins)+
    geom_point(data = sim_moments, aes(x = bin_mean, y = skew_sim), color = "pink") +
    geom_point(data = sim_medians, aes(x = bin_mean, y = median_skew_sim),shape = 1, color = "black") +
    geom_point(aes(x = bin_mean, y = skew_t1), shape = 1, color = "gray72") + theme_classic()
  kurtplot <- ggplot(data = bins)+
    geom_point(data = sim_moments, aes(x = bin_mean, y = kurt_sim), color = "pink") +
    geom_point(data = sim_medians, aes(x = bin_mean, y = median_kurt_sim),shape = 1, color = "black") +
    geom_point(aes(x = bin_mean, y = kurt_t1), shape = 1, color = "gray72") + theme_classic()
  size_ppc_plot <- meanplot+ sdplot+skewplot+ kurtplot+plot_annotation(title = title)
  return(size_ppc_plot)
}

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
#######################################################################################################
#### Survival Model -- What is the probability of surviving to the next time step?  ###################
#######################################################################################################
survival_data_orig <- subset(cactus, is.na(Survival_t1) == FALSE,c("Plot","Year_t","Survival_t1","ant_t","logsize_t"))
survival_data_orig <- cactus[,c("Plot","Year_t","Survival_t1","ant_t","logsize_t")]
survival_data <- na.omit(survival_data_orig)
survival_data <- subset(survival_data, survival_data$Survival_t1 != 2)
levels(survival_data$ant_t)
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
summary(fit_surv)[1]
surv_out <- rstan::extract(fit_surv, pars = c("w","beta0","beta1","u","sigma_u","sigma_w"))
surv_mu <- rstan::extract(fit_surv, pars = c("mu"))$mu
surv_sigma <- rstan::extract(fit_surv, pars = c("sigma"))$sigma
write.csv(surv_outputs, "surv_outputs.csv")
write.csv(surv_mu, "surv_mu.csv")
write.csv(surv_sigma, "surv_sigma.csv")

freq <- glm(survival_data$Survival_t1 ~ survival_data$logsize_t + (as.factor(survival_data$ant_t)), family = "binomial")
summary(freq)
## Compare Freq to Bayes
vac_freq <- (coef(freq)[1] + coef(freq)[2]*size_vac) ## Vacant
other_freq <- (coef(freq)[1] + coef(freq)[2]*size_other + coef(freq)[3]) ## Other
crem_freq <- (coef(freq)[1] + coef(freq)[2]*size_crem + coef(freq)[4]) ## Crem
liom_freq <- (coef(freq)[1] + coef(freq)[2]*size_liom + coef(freq)[5]) ## Liom
vac_bayes <- quantile(surv_out$beta0.1,0.5) + size_vac * quantile(surv_out$beta1.1,0.5)
other_bayes <- quantile(surv_out$beta0.2,0.5) + size_other * quantile(surv_out$beta1.2,0.5)
crem_bayes <- quantile(surv_out$beta0.3,0.5) + size_crem * quantile(surv_out$beta1.3,0.5)
liom_bayes <- quantile(surv_out$beta0.4,0.5) + size_liom * quantile(surv_out$beta1.4,0.5)
## Vacant -- very good
plot(size_vac, invlogit(vac_freq), col = vaccol, ylim = c(0,1))
lines(size_vac, invlogit(vac_bayes), col = vaccol, lwd = 3) 
## Other -- underestimating at small values
points(size_other, invlogit(other_freq), col = othercol, ylim = c(0,1))
lines(size_other, invlogit(other_bayes), col = othercol, lwd = 3)
## Crem -- underestimating at small values
points(size_crem, invlogit(crem_freq), col = cremcol, ylim = c(0,1))
lines(size_crem, invlogit(crem_bayes), col = cremcol, lwd = 3)
## Liom -- underestimating at small values
points(size_liom, invlogit(liom_freq), col = liomcol, ylim = c(0,1))
lines(size_liom, invlogit(liom_bayes), col = liomcol, lwd = 3)

## Visualize the posterior distributions
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
#overlay plot data
y <- survival_data$Survival_t1
surv_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/surv_outputs.csv", header = TRUE,stringsAsFactors=T)
surv_data <- surv_data[,c(-1)]
surv_data <- as.matrix(surv_data)
surv_mu <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/surv_mu.csv", header = TRUE,stringsAsFactors=T)
surv_mu <- surv_mu[,c(-1)]
surv_mu <- as.matrix(surv_mu)
y_sim <- matrix(NA, n_post_draws,length(y))
for(i in 1:n_post_draws){
  #y_sim[i,] <- rbinom(n=length(y), mean = surv_yrep[i,], sd = surv_sigma[i,])
  y_sim[i,] <- rbinom(n=length(y), size=1, prob = invlogit(mean(surv_mu[i,])))
}
samp100 <- sample(nrow(y_sim), 100)
## Overlay Plots
png(file = "surv_ant_post.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay_grouped(y, y_sim[samp100,],group = as.integer(as.factor(survival_data$ant_t)))
dev.off()
## Convergence Plots
png(file = "surv_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_surv, pars=c("beta0","beta1")))
dev.off()
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
flow_trunc_outputs <- rstan::extract(fit_flow_trunc, pars = c("beta0","beta1","u","w","sigma_u","sigma_w"))
write.csv(flow_trunc_outputs, "flow_trunc_outputs.csv")
flow_mu <- rstan::extract(fit_flow_trunc, pars = c("mu"))$mu
write.csv(flow_mu, "flow_mu.csv")
flow_phi <- rstan::extract(fit_flow_trunc, pars = c("phi"))$phi
write.csv(flow_phi, "flow_phi.csv")
summary(fit_flow_trunc)[1]
## Check the posterior distributions
y <- flower_data$TotFlowerbuds_t
flow_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/flow_trunc_outputs.csv", header = TRUE,stringsAsFactors=T)
# Create the y rep (needs to be done outside of STAN because of the 0 truncation)
n_post_draws <- 100
post_draws <- sample.int(dim(flow_mu)[1], n_post_draws)
y_sim <- matrix(NA,n_post_draws,length(y))
for(i in 1:n_post_draws){
  ## sample panicle data (zero-truncated NB)
  for(j in 1:length(y)){
    y_sim[i,j] <- sample(x=1:1000,size=1,replace=T,prob=dnbinom(1:1000, mu = exp(flow_mu[i,j]), size=flow_phi[i]) / (1 - dnbinom(0, mu = exp(flow_mu[i,j]), size=flow_phi[i])))
  }
}
## Plot the posterior distributions
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("flow_trunc_post.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay(y, y_sim[samp100,])
dev.off()
## Convergence Plots
png(file = "flow_trunc_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
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
levels(viability_data$ant_t)
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
viab_mu <- rstan::extract(fit_viab, pars = c("mu"))$mu
viab_outputs <- rstan::extract(fit_viab, pars = c("w","beta0","u","sigma_u","sigma_w"))
viab_sigma <- rstan::extract(fit_viab, pars = c("sigma"))$sigma
write.csv(viab_outputs, "viab_outputs.csv")
write.csv(viab_mu, "viab_mu.csv")
write.csv(viab_sigma, "viab_sigma.csv")
summary(fit_viab)[1]


freq <- glm(cbind(Goodbuds_t,ABFlowerbuds_t) ~ ant_t ,family="binomial",data=cactus)
summary(freq)
## Compare Freq to Bayes
vac_freq <- (coef(freq)[1]) ## Vacant
other_freq <- (coef(freq)[1] + coef(freq)[2]) ## Other
crem_freq <- (coef(freq)[1] + coef(freq)[3]) ## Crem
liom_freq <- (coef(freq)[1] + coef(freq)[4]) ## Liom
liom_bayes <- invlogit(mean(viab_out$beta0.4))
crem_bayes <- invlogit(mean(viab_out$beta0.3))
vac_bayes <- invlogit(mean(viab_out$beta0.1))
other_bayes <- invlogit(mean(viab_out$beta0.2))
viability_data$viab <- viability_data$Goodbuds_t1/viability_data$TotFlowerbuds_t1
vac_real <- viability_data$viab[viability_data$ant_t == "vacant"]
other_real <- viability_data$viab[viability_data$ant_t == "other"]
crem_real <- viability_data$viab[viability_data$ant_t == "crem"]
liom_real <- viability_data$viab[viability_data$ant_t == "liom"]
par(mfrow = c(1,3))
barplot(c(invlogit(vac_freq), invlogit(other_freq), invlogit(crem_freq), invlogit(liom_freq)), col = c(vaccol, othercol, cremcol, liomcol), ylim = c(0,1))
barplot(c(vac_bayes,other_bayes,crem_bayes,liom_bayes), col = c(vaccol, othercol, cremcol, liomcol))
barplot(c(mean(vac_real), mean(other_real), mean(crem_real), mean(liom_real)),col = c(vaccol,othercol,cremcol,liomcol))


## Check the Posterior Distribution
n_post_draws <- 100
post_draws <- sample.int(dim(viab_yrep)[1], n_post_draws)
y <- viability_data$Goodbuds_t1
y_sim <- matrix(NA,n_post_draws,length(y))
for(i in 1:n_post_draws){
    y_sim[i,] <- rbern(n = length(y), prob = invlogit(mean(viab_mu[i,])))
}
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
samp100 <- sample(nrow(y_sim), 100)
## Overlay Plots
png(file = "viab_post.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay(y, y_sim[samp100,])
dev.off()
png(file = "viab_ant_post.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay_grouped(y, y_sim[samp100,],group = as.integer(as.factor(viability_data$ant)))
dev.off()
## Convergence Plots
png(file = "viab_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
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

plot(reproductive_data$logsize_t1,reproductive_data$flower1_YN)
flow <-glm(flower1_YN~logsize_t1,data=reproductive_data,family="binomial")
lines(-5:15,exp(coef(flow)[1]+coef(flow)[2]*-5:15)/(1+exp(coef(flow)[1]+coef(flow)[2]*-5:15)))

## Run the Model
#Check if the model is written to the right place
#stanc("STAN Models/repro_mix_ant.stan")
fit_repro <- stan(file = "Data Analysis/STAN Models/repro_code.stan", data = stan_data_repro, warmup = 150, iter = 1000, chains = 3, cores = 3, thin = 1)
repro_outputs <- rstan::extract(fit_repro, pars = c("beta0","u","w","sigma_u","sigma_w","beta1"))
repro_mu <- rstan::extract(fit_repro, pars = c("mu"))$mu
repro_sigma <- rstan::extract(fit_repro, pars = c("sigma"))$sigma
write.csv(repro_outputs, "repro_outputs.csv")
write.csv(repro_mu, "repro_mu.csv")
write.csv(repro_sigma, "repro_sigma.csv")
summary(fit_repro)[1]
## Check the Posteriors
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
repro_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/repro_outputs.csv", header = TRUE,stringsAsFactors=T)
y <- as.numeric(reproductive_data$flower1_YN)
y_sim <- matrix(NA,n_post_draws,length(y))
for(i in 1:n_post_draws){
  y_sim[i,] <- rbern(n = length(y), prob = invlogit(mean(repro_mu[i,])))
}
samp100 <- sample(nrow(y_sim), 100)
## Overlay Plots
png(file = "repro_post.png")
bayesplot::ppc_dens_overlay(y, y_sim[samp100,])
dev.off()
## Convergence Plots
png(file = "repro_conv.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_repro, pars=c("beta0","beta1","sigma_u","sigma_w")))
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

seed_data <- seed
seed_data <- na.omit(seed_data)
seed_data$ant <- as.integer(as.factor(seed_data$ant_state))
seed_data <- subset(seed_data, seed_count > 0)
str(seed_data)
levels(as.factor(seed_data$ant_state))
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
seed_outputs <- rstan::extract(fit_seed, pars = c("beta0"))
seed_mu <- rstan::extract(fit_seed, pars = c("y_rep"))$y_rep
write.csv(seed_outputs, "seed_outputs.csv")
write.csv(seed_mu, "seed_mu.csv")
summary(fit_seed)[1]

## Compare to Real and Bayes
crem_bayes <- exp(quantile(seed_out$beta0.1,0.5))
liom_bayes <- quantile(seed_out$beta0.2,0.5)
vac_bayes <- quantile(seed_out$beta0.3,0.5)
crem_real <- mean(seed$seed_count[seed$ant_state == "Crem"])
liom_real <- mean(seed$seed_count[seed$ant_state == "Liom"])
vac_real <- mean(seed$seed_count[seed$ant_state == "Vacant"])
barplot(c(crem_real,liom_real,vac_real), col = c(cremcol, liomcol, vaccol))
barplot(c(exp(vac_bayes),exp(crem_bayes),exp(liom_bayes)), col = c(cremcol, liomcol, vaccol))

## Check the Posteriors
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
y <- seed_data$seed_count
samp100 <- sample(nrow(seed_mu), 500)
## Overlay Plots
png(file = "seed_post.png")
bayesplot::ppc_dens_overlay(y, seed_mu[samp100,])
dev.off()
png(file = "seed_ant_post.png")
bayesplot::ppc_dens_overlay_grouped(y, seed_mu[samp100,],group = as.integer(as.factor(seed_data$ant)))
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
fit_seed_surv <- stan(file = "Data Analysis/STAN Models/seed_surv_code.stan", data = stan_data_seed_surv, warmup = 150, iter = 1000, chains = 3, cores = 3, thin = 1)
seed_surv_outputs <- rstan::extract(fit_seed_surv, pars = c("beta0","w","sigma_w","sigma"))
seed_surv_mu <- rstan::extract(fit_seed_surv, pars = c("y_rep"))$y_rep
write.csv(seed_surv_outputs, "seed_surv_outputs.csv")
write.csv(seed_surv_mu, "seed_surv_mu.csv")
summary(fit_seed_surv)[1]
## Check the Posteriors
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
y <- precensus.dat$survive0405
samp100 <- sample(nrow(seed_surv_mu), 100)
## Overlay Plots
png(file = "seed_surv_post.png")
bayesplot::ppc_dens_overlay(y, seed_surv_mu[samp100,])
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
fit_germ1 <- stan(file = "Data Analysis/STAN Models/germ_code.stan", data = stan_data_germ1, warmup = 150, iter = 1000, chains = 3, cores = 3, thin = 1)
fit_germ2 <- stan(file = "Data Analysis/STAN Models/germ_code.stan", data = stan_data_germ2, warmup = 150, iter = 1000, chains = 3, cores = 3, thin = 1)
germ1_outputs <- rstan::extract(fit_germ1, pars = c("beta0","sigma","phi"))
germ2_outputs <- rstan::extract(fit_germ2, pars = c("beta0","sigma","phi"))
write.csv(germ1_outputs, "germ1_outputs.csv")
write.csv(germ2_outputs, "germ2_outputs.csv")
germ1_mu <- rstan::extract(fit_germ1, pars = c("y_rep"))$y_rep
germ2_mu <- rstan::extract(fit_germ2, pars = c("y_rep"))$y_rep
write.csv(germ1_mu, "germ1_mu.csv")
write.csv(germ2_mu, "germ2_mu.csv")
summary(fit_germ1)[1]
summary(fit_germ2)[1]

## Check the Posteriors
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
## Germ yr 1
y <- germ.dat$Seedlings04/germ.dat$Input
samp100 <- sample(nrow(germ1_mu), 100)
## Overlay Plots
png(file = "germ1_post.png")
bayesplot::ppc_dens_overlay(y, germ1_mu[samp100,])
dev.off()
## Convergence Plots
png(file = "germ1_conv.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_germ1, pars=c("beta0")))
dev.off()
## Germ yr 2
y <- germ.dat$Seedlings05/(germ.dat$Input - germ.dat$Seedlings04)
samp100 <- sample(nrow(germ2_mu), 100)
## Overlay Plots
png(file = "germ2_post.png")
bayesplot::ppc_dens_overlay(y, germ2_mu[samp100,])
dev.off()
## Convergence Plots
png(file = "germ2_conv.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_germ2, pars=c("beta0")))
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

#######################################################################################################
#### Recruits -- Size Distribution of  #########################################################################################
#######################################################################################################
seedling.dat_orig <- cactus[,c("logsize_t1","Recruit","Year_t")]
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
rec_outputs <- rstan::extract(fit_rec, pars = c("beta0","sigma"))
write.csv(rec_outputs,"rec_outputs.csv")
rec_mu <- rstan::extract(fit_rec,pars = c("y_rep"))$y_rep
write.csv(rec_mu, "rec_mu.csv")
## Check Posterior Dist
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
y <- seedling.dat$logsize_t1
samp100 <- sample(nrow(rec_mu), 100)
## Overlay Plots
png(file = "rec_post.png")
bayesplot::ppc_dens_overlay(y, rec_mu[samp100,])
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
multi_dat_real <- list(K = length(unique(cactus_real$ant_t1)), #number of possible ant species
                       N = dim(cactus_real)[1], #number of observations
                       D = 5, #number of predictors
                       P = 14, #number of random effect predictors
                       y = as.integer(as.factor(cactus_real$ant_t1)), #observations
                       x = model.matrix(~ 0 + (as.factor(ant_t)) + logsize_t, cactus_real), #design matrix
                       z = model.matrix(~0 + as.factor(Year_t), cactus_real),
                       N_Year = as.integer(length(unique(cactus_real$Year_t)))
)
## Run the model & save the results
fit_multi <- stan(file = "Data Analysis/STAN Models/multi_mixed.stan", 
                      data = multi_dat_real, warmup = 15, iter = 100, chains = 3)
fit_multi1 <- stan(file = "Data Analysis/STAN Models/multi_prac_tom_Km1.stan",
                   data = multi_dat_real, warmup = 150, iter = 1000, chains = 3)
multi_out <- rstan::extract(fit_multi, pars = c("beta","beta_raw","theta","theta_raw"))
write.csv(multi_out, "multi_outputs.csv")
multi_out1 <- rstan::extract(fit_multi1, pars = c("beta","beta_raw"))
write.csv(multi_out1, "multi_outputs1.csv")
## Simulate Multinomial Yreps
n_post_draws <- 100
post_draws <- sample.int(dim(flow_yrep)[1], n_post_draws)
y_sim <- matrix(NA,n_post_draws,length(y))
for(i in 1:n_post_draws){
  ## sample panicle data (zero-truncated NB)
  for(j in 1:length(y)){
    y_sim[i,j] <- sample(x=1:1000,size=1,replace=T,prob=dnbinom(1:1000, mu = exp(flow_yrep[i,j]), size=flow_phi[i]) / (1 - dnbinom(0, mu = exp(flow_yrep[i,j]), size=flow_phi[i])))
  }
}
## P
## plot the chains
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("multi_conv_beta.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_multi, pars=c("beta")))
dev.off()
png("multi_conv_theta.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_multi, pars=c("theta[1,1]")))
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

summary(fit_multi)
## this looks pretty good. All betas converge

########################################## ALL ANTS ############################################################################
## Calculate the probabilities of being tended by each ant species
size_dummy_real <- seq(min(cactus_real$logsize_t, na.rm = TRUE), max(cactus_real$logsize_t, na.rm = TRUE), by = 0.1)
## Previously tended by none
Denominator_vac <- exp(mean(multi_out1$beta[,1,1]) + size_dummy_real*mean(multi_out1$beta[,5,1])) + 
  exp(mean(multi_out1$beta[,1,2]) + size_dummy_real*mean(multi_out1$beta[,5,2])) + 
  exp(mean(multi_out1$beta[,1,3]) + size_dummy_real*mean(multi_out1$beta[,5,3])) + 
  exp(mean(multi_out1$beta[,1,4]) + size_dummy_real*mean(multi_out1$beta[,5,4]))
pred_vac<-cbind(
  #pr(vacant)
  exp(mean(multi_out1$beta[,1,1]) + size_dummy_real*mean(multi_out1$beta[,5,1]))/Denominator_vac,
  #pr(other)
  exp(mean(multi_out1$beta[,1,2]) + size_dummy_real*mean(multi_out1$beta[,5,2]))/Denominator_vac,
  #pr(crem)
  exp(mean(multi_out1$beta[,1,3]) + size_dummy_real*mean(multi_out1$beta[,5,3]))/Denominator_vac,
  #pr(liom)
  exp(mean(multi_out1$beta[,1,4]) + size_dummy_real*mean(multi_out1$beta[,5,4]))/Denominator_vac)
sum(pred_vac[1,])

## Previously tended by Other
Denominator_other <- exp(mean(multi_out1$beta[,2,1]) + size_dummy_real*mean(multi_out1$beta[,5,1])) + 
  exp(mean(multi_out1$beta[,2,2]) + size_dummy_real*mean(multi_out1$beta[,5,2])) + 
  exp(mean(multi_out1$beta[,2,3]) + size_dummy_real*mean(multi_out1$beta[,5,3])) + 
  exp(mean(multi_out1$beta[,2,4]) + size_dummy_real*mean(multi_out1$beta[,5,4]))
pred_other<-cbind(
  #pr(vacant)
  exp((mean(multi_out1$beta[,2,1])) + size_dummy_real*mean(multi_out1$beta[,5,1]))/Denominator_other,
  #pr(other)
  exp((mean(multi_out1$beta[,2,2])) + size_dummy_real*mean(multi_out1$beta[,5,2]))/Denominator_other,
  #pr(crem)
  exp((mean(multi_out1$beta[,2,3])) + size_dummy_real*mean(multi_out1$beta[,5,3]))/Denominator_other,
  #pr(liom)
  exp((mean(multi_out1$beta[,2,4])) + size_dummy_real*mean(multi_out1$beta[,5,4]))/Denominator_other)
sum(pred_other[1,])

## Previously tended by Crem
Denominator_crem <- exp(mean(multi_out1$beta[,3,1]) + size_dummy_real*mean(multi_out1$beta[,5,1])) + 
  exp(mean(multi_out1$beta[,3,2]) + size_dummy_real*mean(multi_out1$beta[,5,2])) + 
  exp(mean(multi_out1$beta[,3,3]) + size_dummy_real*mean(multi_out1$beta[,5,3])) + 
  exp(mean(multi_out1$beta[,3,4]) + size_dummy_real*mean(multi_out1$beta[,5,4]))
pred_crem<-cbind(
  #pr(vacant)
  exp(mean(multi_out1$beta[,3,1]) + size_dummy_real*mean(multi_out1$beta[,5,1]))/Denominator_crem,
  #pr(other)
  exp(mean(multi_out1$beta[,3,2]) + size_dummy_real*mean(multi_out1$beta[,5,2]))/Denominator_crem,
  #pr(crem)
  exp(mean(multi_out1$beta[,3,3]) + size_dummy_real*mean(multi_out1$beta[,5,3]))/Denominator_crem,
  #pr(liom)
  exp(mean(multi_out1$beta[,3,4]) + size_dummy_real*mean(multi_out1$beta[,5,4]))/Denominator_crem)
sum(pred_crem[1,])

## Previously tended by Liom
Denominator_liom <- exp(mean(multi_out1$beta[,4,1]) + size_dummy_real*mean(multi_out1$beta[,5,1])) + 
  exp(mean(multi_out1$beta[,4,2]) + size_dummy_real*mean(multi_out1$beta[,5,2])) + 
  exp(mean(multi_out1$beta[,4,3]) + size_dummy_real*mean(multi_out1$beta[,5,3])) + 
  exp(mean(multi_out1$beta[,4,4]) + size_dummy_real*mean(multi_out1$beta[,5,4]))
pred_liom<-cbind(
  #pr(vacant)
  exp(mean(multi_out1$beta[,4,1]) + size_dummy_real*mean(multi_out1$beta[,5,1]))/Denominator_liom,
  #pr(other)
  exp(mean(multi_out1$beta[,4,2]) + size_dummy_real*mean(multi_out1$beta[,5,2]))/Denominator_liom,
  #pr(crem)
  exp(mean(multi_out1$beta[,4,3]) + size_dummy_real*mean(multi_out1$beta[,5,3]))/Denominator_liom,
  #pr(liom)
  exp(mean(multi_out1$beta[,4,4]) + size_dummy_real*mean(multi_out1$beta[,5,4]))/Denominator_liom)
sum(pred_liom[1,])
                  ## vac -> vac       vac -> other    vac -> crem       vac -> liom
pred_probs_vac <- cbind((pred_vac[,1]) , (pred_vac[,2]) , (pred_vac[,3]) , (pred_vac[,4]))
                  ## other-> vac        other -> other    other -> crem       other -> liom
pred_probs_other <- cbind((pred_other[,1]) , (pred_other[,2]) , (pred_other[,3]) , (pred_other[,4]))
                  ## crem-> vac       crem -> other    crem -> crem      crem -> liom
pred_probs_crem <- cbind((pred_crem[,1]) , (pred_crem[,2]) , (pred_crem[,3]) , (pred_crem[,4]))
                  ## liom-> vac       liom -> other    liom -> crem       liom -> liom
pred_probs_liom <- cbind((pred_liom[,1]) , (pred_liom[,2]) , (pred_liom[,3]) , (pred_liom[,4]))

all_ant_multi <- cbind(pred_probs_vac, pred_probs_crem, pred_probs_other, pred_probs_liom)
colnames(all_ant_multi) <- c("vacvac","vaccrem","vacother","vacliom",
                             "cremvac","cremcrem","cremother","cremliom",
                             "othervac","othercrem","otherother","otherliom",
                             "liomvac","liomcrem","liomother","liomliom")
write.csv(all_ant_multi,"all_ant_multi.csv")

## Calculate the yrep counts vs the real counts
x <- c(1,2,3,4)
vac_prob <- c(mean(pred_vac[,1]),mean(pred_vac[,3]), mean(pred_vac[,4]), mean(pred_vac[,2]))
vac_multi_yrep <- rmultinom(x, length(cactus_real$ant_t1[cactus_real$ant_t == "vacant"]),vac_prob)
tab <- table(cactus_real$ant_t1, cactus_real$ant_t)
liom_prob <- c(mean(pred_liom[,1]),mean(pred_liom[,2]), mean(pred_liom[,3]), mean(pred_liom[,4]))
liom_multi_yrep <- rmultinom(x, length(cactus_real$ant_t1[cactus_real$ant_t == "liom"]),liom_prob)
other_prob <- c(mean(pred_other[,1]),mean(pred_other[,3]), mean(pred_other[,4]), mean(pred_other[,2]))
other_multi_yrep <- rmultinom(x, length(cactus_real$ant_t1[cactus_real$ant_t == "other"]),other_prob)
crem_prob <- c(mean(pred_crem[,1]),mean(pred_crem[,3]), mean(pred_crem[,4]), mean(pred_crem[,2]))
crem_multi_yrep <- rmultinom(x, length(cactus_real$ant_t1[cactus_real$ant_t == "crem"]),crem_prob)

#### Plot the simulated counts of the model compared to the real data
## Color Codes
## Retro bright
cremcol <- "#9239F6"
liomcol <- "#00A08A"
othercol <- "#FF0076"
vaccol <- "#F8B660"
## Prev Vacant
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("multi_yrep.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,2,3,4,5),
              ncol = 2, byrow = TRUE), heights = c(0.7,1.4,1.4), widths = c(3.9,3.9,3.9))
plot.new()
text(0.5,0.5,"Yrep for Multi Model",cex=2,font=2)
## Prev Vacant
plot(x,vac_multi_yrep, col = c(vaccol, cremcol, liomcol, othercol), xlab = "", ylab = "", main = "a)            Prev. Vac.", pch = 20, cex = 2)
points(c(1,4,2,3),tab[,1], col = c(vaccol, othercol, cremcol, liomcol), cex = 2)
## Prev Liom
plot(x,liom_multi_yrep, col = c(vaccol, cremcol, liomcol, othercol), xlab = "", ylab = "", main = "b)           Prev. Liom.", pch = 20, cex = 2, ylim = c(0,2000))
points(c(4,3,2,1),tab[,4], col = c(othercol, liomcol, cremcol, vaccol), cex = 2)
## Prev Crem
plot(x,crem_multi_yrep, col = c(vaccol, cremcol, liomcol, othercol), xlab = "", ylab = "", main = "c)           Prev. Crem.", pch = 20, cex = 2)
points(c(1,4,2,3),tab[,3], col = c(vaccol, othercol, cremcol, liomcol), cex = 2)
## Prev Other
plot(x,other_multi_yrep, col = c(vaccol, cremcol, liomcol, othercol), xlab = "", ylab = "", main = "d)          Prev. Other", pch = 20, cex = 2)
points(c(1,4,2,3),tab[,2], col = c(vaccol, othercol, cremcol, liomcol), cex = 2)
legend("topright",legend = c("Vacant","Crem.","Liom.","Other"), fill = c(vaccol, cremcol, liomcol, othercol))
mtext("Ant Year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Predicted Next Ant Count",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()



## Bin the data 
## prev crem
subset_crem <- filter(cactus_real, cactus_real$ant_t_relevel == "crem")
subset_crem$ant_t1_crem_YN <- 0
subset_crem$ant_t1_liom_YN <- 0
subset_crem$ant_t1_other_YN <- 0
subset_crem$ant_t1_vac_YN <- 0
for(i in 1:nrow(subset_crem)){
  if(subset_crem$ant_t1_relevel[i] == "crem"){subset_crem$ant_t1_crem_YN[i] = 1}
  if(subset_crem$ant_t1_relevel[i] == "liom"){subset_crem$ant_t1_liom_YN[i] = 1}
  if(subset_crem$ant_t1_relevel[i] == "other"){subset_crem$ant_t1_other_YN[i] = 1}
  if(subset_crem$ant_t1_relevel[i] == "vacant"){subset_crem$ant_t1_vac_YN[i] = 1}
}
## prev liom
subset_liom <- subset(cactus_real, cactus_real$ant_t_relevel == "liom")
subset_liom$ant_t1_crem_YN <- 0
subset_liom$ant_t1_liom_YN <- 0
subset_liom$ant_t1_other_YN <- 0
subset_liom$ant_t1_vac_YN <- 0
for(i in 1:nrow(subset_liom)){
  if(subset_liom$ant_t1_relevel[i] == "crem"){subset_liom$ant_t1_crem_YN[i] = 1}
  if(subset_liom$ant_t1_relevel[i] == "liom"){subset_liom$ant_t1_liom_YN[i] = 1}
  if(subset_liom$ant_t1_relevel[i] == "other"){subset_liom$ant_t1_other_YN[i] = 1}
  if(subset_liom$ant_t1_relevel[i] == "vacant"){subset_liom$ant_t1_vac_YN[i] = 1}
}
## prev other
subset_other <- subset(cactus_real, cactus_real$ant_t_relevel == "other")
subset_other$ant_t1_crem_YN <- 0
subset_other$ant_t1_liom_YN <- 0
subset_other$ant_t1_other_YN <- 0
subset_other$ant_t1_vac_YN <- 0
for(i in 1:nrow(subset_other)){
  if(subset_other$ant_t1_relevel[i] == "crem"){subset_other$ant_t1_crem_YN[i] = 1}
  if(subset_other$ant_t1_relevel[i] == "liom"){subset_other$ant_t1_liom_YN[i] = 1}
  if(subset_other$ant_t1_relevel[i] == "other"){subset_other$ant_t1_other_YN[i] = 1}
  if(subset_other$ant_t1_relevel[i] == "vacant"){subset_other$ant_t1_vac_YN[i] = 1}
}
## prev vac
subset_vac <- subset(cactus_real, cactus_real$ant_t_relevel == "vacant")
subset_vac$ant_t1_crem_YN <- 0
subset_vac$ant_t1_liom_YN <- 0
subset_vac$ant_t1_other_YN <- 0
subset_vac$ant_t1_vac_YN <- 0
for(i in 1:nrow(subset_vac)){
  if(subset_vac$ant_t1_relevel[i] == "crem"){subset_vac$ant_t1_crem_YN[i] = 1}
  if(subset_vac$ant_t1_relevel[i] == "liom"){subset_vac$ant_t1_liom_YN[i] = 1}
  if(subset_vac$ant_t1_relevel[i] == "other"){subset_vac$ant_t1_other_YN[i] = 1}
  if(subset_vac$ant_t1_relevel[i] == "vacant"){subset_vac$ant_t1_vac_YN[i] = 1}
}
#### Plot the simulated probs against the real probs
## Prev Vac
tab_vac<-table(subset_vac$ant_t1_vac_YN)
probs_vac <- tab_vac/nrow(subset_vac)
tab_liom<-table(subset_vac$ant_t1_liom_YN)
probs_liom <- tab_liom/nrow(subset_vac)
tab_crem<-table(subset_vac$ant_t1_crem_YN)
probs_crem <- tab_crem/nrow(subset_vac)
tab_other<-table(subset_vac$ant_t1_other_YN)
probs_other <- tab_other/nrow(subset_vac)
plot(x,c(probs_vac[2], probs_crem[2],probs_liom[2], probs_other[2]), ylim = c(0,1), col = c(vaccol, cremcol, liomcol, othercol,cex = 2), main = "Pre. Vac.", xlab = "Ant Sp", ylab = "Probability")
points(c(1,4,2,3),c(mean(pred_vac[,1]), mean(pred_vac[,2]),mean(pred_vac[,4]),mean(pred_vac[,3])), col = c(vaccol, othercol, cremcol, liomcol), pch = 20,cex = 1)
legend("topright",legend = c("vac - data", "vac - sim","crem - data", "crem - sim","liom - data","liom - sim","other - data","other - sim"), fill = c(vaccol,vaccol,cremcol, cremcol, liomcol, liomcol, othercol, othercol), pch = c(1,20,1,20,1,20,1,20))
## Prev Crem
tab_vac<-table(subset_crem$ant_t1_vac_YN)
probs_vac <- tab_vac/nrow(subset_crem)
tab_liom<-table(subset_crem$ant_t1_liom_YN)
probs_liom <- tab_liom/nrow(subset_crem)
tab_crem<-table(subset_crem$ant_t1_crem_YN)
probs_crem <- tab_crem/nrow(subset_crem)
tab_other<-table(subset_crem$ant_t1_other_YN)
probs_other <- tab_other/nrow(subset_crem)
plot(x,c(probs_vac[2], probs_crem[2],probs_liom[2], probs_other[2]), ylim = c(0,1), col = c(vaccol, cremcol, liomcol, othercol,cex = 2),main = "Pre. Crem.", xlab = "Ant Sp", ylab = "Probability")
points(c(1,4,2,3),c(mean(pred_crem[,1]), mean(pred_crem[,2]),mean(pred_crem[,4]),mean(pred_crem[,3])), col = c(vaccol, othercol, cremcol, liomcol), pch = 20,cex = 1)
legend("topright",legend = c("vac - data", "vac - sim","crem - data", "crem - sim","liom - data","liom - sim","other - data","other - sim"), fill = c(vaccol,vaccol,cremcol, cremcol, liomcol, liomcol, othercol, othercol), pch = c(1,20,1,20,1,20,1,20))
## Prev Liom
tab_vac<-table(subset_liom$ant_t1_vac_YN)
probs_vac <- tab_vac/nrow(subset_liom)
tab_liom<-table(subset_liom$ant_t1_liom_YN)
probs_liom <- tab_liom/nrow(subset_liom)
tab_crem<-table(subset_liom$ant_t1_crem_YN)
probs_crem <- tab_crem/nrow(subset_liom)
tab_other<-table(subset_liom$ant_t1_other_YN)
probs_other <- tab_other/nrow(subset_liom)
plot(x,c(probs_vac[2], probs_crem[2],probs_liom[2], probs_other[2]), ylim = c(0,1), col = c(vaccol, cremcol, liomcol, othercol,cex = 2),main = "Pre. Liom.", xlab = "Ant Sp", ylab = "Probability")
points(c(3,4,2,1),c(mean(pred_liom[,1]), mean(pred_liom[,2]),mean(pred_liom[,4]),mean(pred_liom[,3])), col = c(liomcol, othercol, cremcol, vaccol), pch = 20,cex = 1)
legend("topright",legend = c("vac - data", "vac - sim","crem - data", "crem - sim","liom - data","liom - sim","other - data","other - sim"), fill = c(vaccol,vaccol,cremcol, cremcol, liomcol, liomcol, othercol, othercol), pch = c(1,20,1,20,1,20,1,20))
## Prev Other
tab_vac<-table(subset_other$ant_t1_vac_YN)
probs_vac <- tab_vac/nrow(subset_other)
tab_liom<-table(subset_other$ant_t1_liom_YN)
probs_liom <- tab_liom/nrow(subset_other)
tab_crem<-table(subset_other$ant_t1_crem_YN)
probs_crem <- tab_crem/nrow(subset_other)
tab_other<-table(subset_other$ant_t1_other_YN)
probs_other <- tab_other/nrow(subset_other)
plot(x,c(probs_vac[2], probs_crem[2],probs_liom[2], probs_other[2]), ylim = c(0,1), col = c(vaccol, cremcol, liomcol, othercol,cex = 2),main = "Pre. Other", xlab = "Ant Sp", ylab = "Probability")
points(c(1,4,2,3),c(mean(pred_other[,1]), mean(pred_other[,2]),mean(pred_other[,4]),mean(pred_other[,3])), col = c(vaccol, othercol, cremcol, liomcol), pch = 20,cex = 1)
legend("topright",legend = c("vac - data", "vac - sim","crem - data", "crem - sim","liom - data","liom - sim","other - data","other - sim"), fill = c(vaccol,vaccol,cremcol, cremcol, liomcol, liomcol, othercol, othercol), pch = c(1,20,1,20,1,20,1,20))

multi_plot_crem <- subset_crem %>% 
  mutate(size_bin = cut_interval((logsize_t),25)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t),na.rm=T),
            ant_t1_crem = mean(ant_t1_crem_YN,na.rm=T),
            ant_t1_liom = mean(ant_t1_liom_YN, na.rm = T),
            ant_t1_other = mean(ant_t1_other_YN, na.rm = T),
            ant_t1_vac = mean(ant_t1_vac_YN, na.rm = T),
            N = length(logsize_t))
multi_plot_crem$N_mod <- log(multi_plot_crem$N)

multi_plot_liom <- subset_liom %>% 
  mutate(size_bin = cut_interval((logsize_t),25)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t),na.rm=T),
            ant_t1_crem = mean(ant_t1_crem_YN,na.rm=T),
            ant_t1_liom = mean(ant_t1_liom_YN, na.rm = T),
            ant_t1_other = mean(ant_t1_other_YN, na.rm = T),
            ant_t1_vac = mean(ant_t1_vac_YN, na.rm = T),
            N = length(logsize_t))
multi_plot_liom$N_mod <- log(multi_plot_liom$N)

multi_plot_other <- subset_other %>% 
  mutate(size_bin = cut_interval((logsize_t),25)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t),na.rm=T),
            ant_t1_crem = mean(ant_t1_crem_YN,na.rm=T),
            ant_t1_liom = mean(ant_t1_liom_YN, na.rm = T),
            ant_t1_other = mean(ant_t1_other_YN, na.rm = T),
            ant_t1_vac = mean(ant_t1_vac_YN, na.rm = T),
            N = length(logsize_t))
multi_plot_other$N_mod <- log(multi_plot_other$N)

multi_plot_vac <- subset_vac %>% 
  mutate(size_bin = cut_interval((logsize_t),25)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t),na.rm=T),
            ant_t1_crem = mean(ant_t1_crem_YN,na.rm=T),
            ant_t1_liom = mean(ant_t1_liom_YN, na.rm = T),
            ant_t1_other = mean(ant_t1_other_YN, na.rm = T),
            ant_t1_vac = mean(ant_t1_vac_YN, na.rm = T),
            N = length(logsize_t))
multi_plot_vac$N_mod <- log(multi_plot_vac$N)

## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_Size_Multi_title.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,1,2,3,4,5),
              ncol = 2, nrow = 3, byrow = TRUE), heights = c(1,1.4,1.4), widths = c(3.9,3.9))
plot.new()
text(0.5,0.5,"Large Plants are Most Likely \n to be Liom. Tended",cex=4,font=2)
## Prev Vac
plot(size_dummy_real, pred_vac[,1], type = "l", col = vaccol,main = "Previously Vacant", ylim = c(0,1), xlab = "", ylab = "",
     cex.main = 2)
lines(size_dummy_real, pred_vac[,2], col = othercol)
lines(size_dummy_real, pred_vac[,3], col = cremcol)
lines(size_dummy_real, pred_vac[,4], col = liomcol)
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_crem,pch=16,cex=multi_plot_vac$N_mod,col= alpha(cremcol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_liom,pch=16,cex=multi_plot_vac$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_other,pch=16,cex=multi_plot_vac$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_vac,pch=16,cex=multi_plot_vac$N_mod,col= alpha(vaccol, 0.4))
## Prev Other
plot(size_dummy_real, pred_other[,1], type = "l", col = vaccol,main = "Previously Other", ylim = c(0,1), xlab = "", ylab = "",
     cex.main = 2)
lines(size_dummy_real, pred_other[,2], col = othercol)
lines(size_dummy_real, pred_other[,3], col = cremcol)
lines(size_dummy_real, pred_other[,4], col = liomcol)
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_crem,pch=16,cex=multi_plot_other$N_mod,col= alpha(cremcol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_liom,pch=16,cex=multi_plot_other$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_other,pch=16,cex=multi_plot_other$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_vac,pch=16,cex=multi_plot_other$N_mod,col= alpha(vaccol, 0.4))
legend("topright",c("vacant","other","crem.","liom."), fill = c(vaccol,othercol,cremcol,liomcol), cex = 2)
## Prev Crem
plot(size_dummy_real, pred_crem[,1], type = "l", col = vaccol,main = "Previously Crem", ylim = c(0,1), xlab = "", ylab = "",
     cex.main = 2)
lines(size_dummy_real, pred_crem[,2], col = othercol)
lines(size_dummy_real, pred_crem[,3], col = cremcol)
lines(size_dummy_real, pred_crem[,4], col = liomcol)
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_crem,pch=16,cex=multi_plot_crem$N_mod,col= alpha(cremcol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_liom,pch=16,cex=multi_plot_crem$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_other,pch=16,cex=multi_plot_crem$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_vac,pch=16,cex=multi_plot_crem$N_mod,col= alpha(vaccol, 0.4))
## Prev Liom
plot(size_dummy_real, pred_liom[,1], type = "l", col = vaccol,main = "Previously Liom", ylim = c(0,1), xlab = "", ylab = "",
     cex.main = 2)
lines(size_dummy_real, pred_liom[,2], col = othercol)
lines(size_dummy_real, pred_liom[,3], col = cremcol)
lines(size_dummy_real, pred_liom[,4], col = liomcol)
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_crem,pch=16,cex=multi_plot_liom$N_mod,col= alpha(cremcol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_liom,pch=16,cex=multi_plot_liom$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_other,pch=16,cex=multi_plot_liom$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_vac,pch=16,cex=multi_plot_liom$N_mod,col= alpha(vaccol, 0.4))
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.5)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.5,las=0)
dev.off()

png("Ant_Size_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4),
              ncol = 2, nrow = 2, byrow = TRUE), heights = c(1.4,1.4), widths = c(3.9,3.9))
## Prev Vac
plot(size_dummy_real, pred_vac[,1], type = "l", col = vaccol,main = "a)              Prev. Vacant               ", ylim = c(0,1), xlab = "", ylab = "",
     cex.main = 1.5)
lines(size_dummy_real, pred_vac[,2], col = othercol)
lines(size_dummy_real, pred_vac[,3], col = cremcol)
lines(size_dummy_real, pred_vac[,4], col = liomcol)
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_crem,pch=16,cex=multi_plot_vac$N_mod,col= alpha(cremcol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_liom,pch=16,cex=multi_plot_vac$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_other,pch=16,cex=multi_plot_vac$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_vac,pch=16,cex=multi_plot_vac$N_mod,col= alpha(vaccol, 0.4))
## Prev Other
plot(size_dummy_real, pred_other[,1], type = "l", col = vaccol,main = "b)              Prev. Other                 ", ylim = c(0,1), xlab = "", ylab = "",
     cex.main = 1.5)
lines(size_dummy_real, pred_other[,2], col = othercol)
lines(size_dummy_real, pred_other[,3], col = cremcol)
lines(size_dummy_real, pred_other[,4], col = liomcol)
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_crem,pch=16,cex=multi_plot_other$N_mod,col= alpha(cremcol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_liom,pch=16,cex=multi_plot_other$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_other,pch=16,cex=multi_plot_other$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_vac,pch=16,cex=multi_plot_other$N_mod,col= alpha(vaccol, 0.4))
legend("topleft",c("vacant","other","crem.","liom."), fill = c(vaccol,othercol,cremcol,liomcol), cex = 1.5)
## Prev Crem
plot(size_dummy_real, pred_crem[,1], type = "l", col = vaccol,main = "c)              Prev. Crem.                ", ylim = c(0,1), xlab = "", ylab = "",
     cex.main = 1.5)
lines(size_dummy_real, pred_crem[,2], col = othercol)
lines(size_dummy_real, pred_crem[,3], col = cremcol)
lines(size_dummy_real, pred_crem[,4], col = liomcol)
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_crem,pch=16,cex=multi_plot_crem$N_mod,col= alpha(cremcol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_liom,pch=16,cex=multi_plot_crem$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_other,pch=16,cex=multi_plot_crem$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_vac,pch=16,cex=multi_plot_crem$N_mod,col= alpha(vaccol, 0.4))
## Prev Liom
plot(size_dummy_real, pred_liom[,1], type = "l", col = vaccol,main = "d)              Prev. Liom.                 ", ylim = c(0,1), xlab = "", ylab = "",
     cex.main = 1.5)
lines(size_dummy_real, pred_liom[,2], col = othercol)
lines(size_dummy_real, pred_liom[,3], col = cremcol)
lines(size_dummy_real, pred_liom[,4], col = liomcol)
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_crem,pch=16,cex=multi_plot_liom$N_mod,col= alpha(cremcol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_liom,pch=16,cex=multi_plot_liom$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_other,pch=16,cex=multi_plot_liom$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_vac,pch=16,cex=multi_plot_liom$N_mod,col= alpha(vaccol, 0.4))
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.5)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.5,las=0)
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

##################################### LIOM & VAC & OTHER ###############################################################
## Calculate the probabilities of being tended by each ant species
## Previously tended by none
Denominator_vac <- exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,1,4]) + size_dummy_real*mean(multi_out$beta[,5,4])) + 
  exp(mean(multi_out$beta[,1,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))
pred_vac<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_vac,
  #pr(liom)
  exp(mean(multi_out$beta[,1,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_vac,
  #pr(other)
  exp(mean(multi_out$beta[,1,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_vac)
sum(pred_vac[1,])

## Previously tended by Liom
Denominator_liom <- exp(mean(multi_out$beta[,4,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4])) + 
  exp(mean(multi_out$beta[,4,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))
pred_liom<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,4,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_liom,
  #pr(liom)
  exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_liom,
  #pr(other)
  exp(mean(multi_out$beta[,4,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_liom)
sum(pred_liom[1,])

## Previously tended by Other
Denominator_other <- exp(mean(multi_out$beta[,2,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,2,4]) + size_dummy_real*mean(multi_out$beta[,5,4])) + 
  exp(mean(multi_out$beta[,2,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))
pred_other<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,2,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_other,
  #pr(liom
  exp(mean(multi_out$beta[,2,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_other,
  #pr(other)
  exp(mean(multi_out$beta[,2,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_other)
sum(pred_other[1,])
                    ## vac -> vac      vac -> liom       vac -> other
pred_probs_vac <- cbind((pred_vac[,1]) , (pred_vac[,2]) , (pred_vac[,3]))
                    ## liom-> vac      liom -> liom      liom -> other
pred_probs_liom <- cbind((pred_liom[,1]) ,  (pred_liom[,2]) , (pred_liom[,3]))
                    ## other-> vac      other -> liom       other -> other
pred_probs_other <- cbind((pred_other[,1]) ,  (pred_other[,2]) , (pred_other[,3]))

lov_ant_multi <- cbind(pred_probs_vac, pred_probs_other, pred_probs_liom)
colnames(lov_ant_multi) <- c("vacvac","vacliom","vacother",
                             "othervac","otherliom","otherother",
                             "liomvac","liomliom","liomother")
write.csv(lov_ant_multi,"lov_ant_multi.csv")


## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_3_LVC_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4, byrow = TRUE), heights = c(0.6,1,1,1), widths = c(5))
plot.new()
text(0.5,0.5,"Ant States \n Vacant, Liom., and Other",cex=2,font=2)
plot(size_dummy_real, pred_vac[,1], type = "l", col = vaccol,main = "Previously Vacant", ylim = c(0,1))
lines(size_dummy_real, pred_vac[,2], col = liomcol)
lines(size_dummy_real, pred_vac[,3], col = othercol)
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_liom,pch=16,cex=multi_plot_vac$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_other,pch=16,cex=multi_plot_vac$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_vac,pch=16,cex=multi_plot_vac$N_mod,col= alpha(vaccol, 0.4))
legend("topleft",c("vacant","liom","other"), fill = c(vaccol,liomcol,othercol))
plot(size_dummy_real, pred_other[,1], type = "l", col = vaccol,main = "Previously Other", ylim = c(0,1))
lines(size_dummy_real, pred_other[,2], col = liomcol)
lines(size_dummy_real, pred_other[,3], col = othercol)
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_liom,pch=16,cex=multi_plot_other$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_other,pch=16,cex=multi_plot_other$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_vac,pch=16,cex=multi_plot_other$N_mod,col= alpha(vaccol, 0.4))
plot(size_dummy_real, pred_liom[,1], type = "l", col = vaccol,main = "Previously Liom", ylim = c(0,1))
lines(size_dummy_real, pred_liom[,2], col = liomcol)
lines(size_dummy_real, pred_liom[,3], col = othercol)
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_liom,pch=16,cex=multi_plot_liom$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_other,pch=16,cex=multi_plot_liom$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_vac,pch=16,cex=multi_plot_liom$N_mod,col= alpha(vaccol, 0.4))
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
Denominator_crem <- exp(mean(multi_out$beta[,2,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
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

loc_ant_multi <- cbind( pred_probs_crem, pred_probs_other, pred_probs_liom)
colnames(loc_ant_multi) <- c("cremother","cremcrem","cremliom",
                             "otherother","othercrem","otherliom",
                             "liomother","liomcrem","liomliom")
write.csv(loc_ant_multi,"loc_ant_multi.csv")



## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_3_LOC_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4, byrow = TRUE), heights = c(0.6,1,1,1), widths = c(5))
plot.new()
text(0.5,0.5,"Ant States \n Other, Crem., and Liom.",cex=2,font=2)
plot(size_dummy_real, pred_other[,1], type = "l", col = othercol,main = "Previously Other", ylim = c(0,1))
lines(size_dummy_real, pred_other[,2], col = cremcol)
lines(size_dummy_real, pred_other[,3], col = liomcol)
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_liom,pch=16,cex=multi_plot_other$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_other,pch=16,cex=multi_plot_other$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_crem,pch=16,cex=multi_plot_other$N_mod,col= alpha(cremcol, 0.4))
legend("topleft",c("other","crem.","liom."), fill = c(othercol,cremcol,liomcol))
plot(size_dummy_real, pred_crem[,1], type = "l", col = othercol,main = "Previously Crem", ylim = c(0,1))
lines(size_dummy_real, pred_crem[,2], col = cremcol)
lines(size_dummy_real, pred_crem[,3], col = liomcol)
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_liom,pch=16,cex=multi_plot_crem$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_other,pch=16,cex=multi_plot_crem$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_crem,pch=16,cex=multi_plot_crem$N_mod,col= alpha(cremcol, 0.4))
plot(size_dummy_real, pred_liom[,1], type = "l", col = othercol,main = "Previously Liom", ylim = c(0,1))
lines(size_dummy_real, pred_liom[,2], col = cremcol)
lines(size_dummy_real, pred_liom[,3], col = liomcol)
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_liom,pch=16,cex=multi_plot_liom$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_other,pch=16,cex=multi_plot_liom$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_crem,pch=16,cex=multi_plot_liom$N_mod,col= alpha(cremcol, 0.4))
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

####################################### LIOM & CREM & VAC #####################################################
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
  exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_crem,
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

                ## vac -> vac             vac -> crem    vac -> liom
pred_probs_vac <- cbind((pred_vac[,1]) , (pred_vac[,2]) , (pred_vac[,3]))
                ## crem-> vac               crem -> crem        crem -> liom
pred_probs_crem <- cbind((pred_crem[,1]) , (pred_crem[,2]) , (pred_crem[,3]))
                ## liom-> vac              liom -> crem    liom -> liom
pred_probs_liom <- cbind((pred_liom[,1]) , (pred_liom[,2]) , (pred_liom[,3]))


lvc_ant_multi <- cbind(pred_probs_vac, pred_probs_crem, pred_probs_liom)
colnames(lvc_ant_multi) <- c("vacvac","vaccrem","vacliom",
                             "cremvac","cremcrem","cremliom",
                             "liomvac","liomcrem","liomliom")
write.csv(lvc_ant_multi,"lvc_ant_multi.csv")



## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_3_LOV_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4, byrow = TRUE), heights = c(0.6,1,1,1), widths = c(5))
plot.new()
text(0.5,0.5,"Ant States \n Other, Vacant, and Liom.",cex=2,font=2)
plot(size_dummy_real, pred_vac[,1], type = "l", col = vaccol,main = "Previously Vacant", ylim = c(0,1))
lines(size_dummy_real, pred_vac[,2], col = cremcol)
lines(size_dummy_real, pred_vac[,3], col = liomcol)
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_liom,pch=16,cex=multi_plot_vac$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_vac,pch=16,cex=multi_plot_vac$N_mod,col= alpha(vaccol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_crem,pch=16,cex=multi_plot_vac$N_mod,col= alpha(cremcol, 0.4))
legend("topleft",c("vacant","crem.","liom."), fill = c(vaccol,cremcol,liomcol))
plot(size_dummy_real, pred_crem[,1], type = "l", col = vaccol,main = "Previously Crem", ylim = c(0,1))
lines(size_dummy_real, pred_crem[,2], col = cremcol)
lines(size_dummy_real, pred_crem[,3], col = liomcol)
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_liom,pch=16,cex=multi_plot_crem$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_vac,pch=16,cex=multi_plot_crem$N_mod,col= alpha(vaccol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_crem,pch=16,cex=multi_plot_crem$N_mod,col= alpha(cremcol, 0.4))
plot(size_dummy_real, pred_liom[,1], type = "l", col = vaccol,main = "Previously Liom", ylim = c(0,1))
lines(size_dummy_real, pred_liom[,2], col = cremcol)
lines(size_dummy_real, pred_liom[,3], col = liomcol)
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_liom,pch=16,cex=multi_plot_liom$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_vac,pch=16,cex=multi_plot_liom$N_mod,col= alpha(vaccol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_crem,pch=16,cex=multi_plot_liom$N_mod,col= alpha(cremcol, 0.4))
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

## Previously tended by Crem
Denominator_crem <- exp(mean(multi_out$beta[,3,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))
pred_crem<-cbind(
  #pr(vacant)
  exp((mean(multi_out$beta[,3,1])) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_crem,
  #pr(other)
  exp((mean(multi_out$beta[,3,2])) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_crem,
  #pr(crem)
  exp((mean(multi_out$beta[,3,3])) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_crem)
sum(pred_crem[1,])

## Previously tended by Crem
Denominator_other <- exp(mean(multi_out$beta[,2,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,2,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,2,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))
pred_other<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta[,2,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_other,
  #pr(other)
  exp(mean(multi_out$beta[,2,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_other,
  #pr(crem)
  exp(mean(multi_out$beta[,2,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_other)
sum(pred_other[1,])

                    ## vac -> vac       vac -> other    vac -> crem  
pred_probs_vac <- cbind((pred_vac[,1]) , (pred_vac[,2]) , (pred_vac[,3]))
                    ## oter-> vac        other -> other    other -> crem    
pred_probs_other <- cbind((pred_other[,1]) , (pred_other[,2]) , (pred_other[,3]))
                    ## crem-> vac       crem -> other    crem -> crem    
pred_probs_crem <- cbind((pred_crem[,1]) , (pred_crem[,2]) , (pred_crem[,3]))

cov_ant_multi <- cbind(pred_probs_vac, pred_probs_crem, pred_probs_other)
colnames(cov_ant_multi) <- c("vacvac","vacother","vaccrem",
                             "cremvac","cremother","cremcrem",
                             "othervac","otherother","othercrem")
write.csv(cov_ant_multi,"cov_ant_multi.csv")



## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_3_COV_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4, byrow = TRUE), heights = c(0.6,1,1,1), widths = c(5))
plot.new()
text(0.5,0.5,"Ant States \n Crem., Vacant, and Liom.",cex=2,font=2)
plot(size_dummy_real, pred_vac[,1], type = "l", col = vaccol,main = "Previously Vacant", ylim = c(0,1))
lines(size_dummy_real, pred_vac[,2], col = othercol)
lines(size_dummy_real, pred_vac[,3], col = cremcol)
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_other,pch=16,cex=multi_plot_vac$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_vac,pch=16,cex=multi_plot_vac$N_mod,col= alpha(vaccol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_crem,pch=16,cex=multi_plot_vac$N_mod,col= alpha(cremcol, 0.4))
legend("topleft",c("vacant","other","crem."), fill = c(vaccol,othercol,cremcol))
plot(size_dummy_real, pred_other[,1], type = "l", col = vaccol,main = "Previously Other", ylim = c(0,1))
lines(size_dummy_real, pred_other[,2], col = othercol)
lines(size_dummy_real, pred_other[,3], col = cremcol)
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_other,pch=16,cex=multi_plot_other$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_vac,pch=16,cex=multi_plot_other$N_mod,col= alpha(vaccol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_crem,pch=16,cex=multi_plot_other$N_mod,col= alpha(cremcol, 0.4))
plot(size_dummy_real, pred_crem[,1], type = "l", col = vaccol,main = "Previously Crem", ylim = c(0,1))
lines(size_dummy_real, pred_crem[,2], col = othercol)
lines(size_dummy_real, pred_crem[,3], col = cremcol)
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_other,pch=16,cex=multi_plot_crem$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_vac,pch=16,cex=multi_plot_crem$N_mod,col= alpha(vaccol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_crem,pch=16,cex=multi_plot_crem$N_mod,col= alpha(cremcol, 0.4))
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

################################################################################################################
#### 2 ANT SPECIES TRANSITION RATES ############################################################################
################################################################################################################

####################################### CREM & OTHER ###########################################################
## Calculate the probabilities of being tended by each ant species
## Previously tended by Other
Denominator_other <- exp(mean(multi_out$beta[,2,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,2,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))
pred_other<-cbind(
  #pr(crem)
  exp((mean(multi_out$beta[,2,3])) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_other,
  #pr(other)
  exp((mean(multi_out$beta[,2,2])) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_other)
sum(pred_other[1,])
## Previously tended by Crem
Denominator_crem <- exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3])) + 
  exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))
pred_crem<-cbind(
  #pr(crem)
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_crem,
  #pr(other)
  exp(mean(multi_out$beta[,3,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_crem)
sum(pred_crem[1,])
                ## other-> crem        other -> other 
pred_probs_other <- cbind((pred_other[,1]) , (pred_other[,2]))
                ## crem-> crem         crem -> other     
pred_probs_crem <- cbind((pred_crem[,1]) , (pred_crem[,2]))


oc_ant_multi <- cbind(pred_probs_crem, pred_probs_other)
colnames(oc_ant_multi) <- c("cremcrem","cremother",
                            "othercrem","otherother")
write.csv(oc_ant_multi,"oc_ant_multi.csv")



## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_Size_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3),
              ncol = 1, nrow = 3, byrow = TRUE), heights = c(0.6,1.4,1.4), widths = c(3.9))
plot.new()
text(0.5,0.5,"Ant States",cex=2,font=2)
## Prev Crem
plot(size_dummy_real, pred_crem[,1], type = "l", col = cremcol,main = "Previously Crem", ylim = c(0,1))
lines(size_dummy_real, pred_crem[,2], col = othercol)
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_other,pch=16,cex=multi_plot_crem$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_crem,pch=16,cex=multi_plot_crem$N_mod,col= alpha(cremcol, 0.4))
## Prev Other
plot(size_dummy_real, pred_other[,1], type = "l", col = cremcol,main = "Previously Other", ylim = c(0,1))
lines(size_dummy_real, pred_other[,2], col = othercol)
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_other,pch=16,cex=multi_plot_other$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_crem,pch=16,cex=multi_plot_other$N_mod,col= alpha(cremcol, 0.4))
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
####################################### LIOM & VAC #############################################################
## Calculate the probabilities of being tended by each ant species
## Previously tended by Liom
Denominator_liom <- exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4])) + 
  exp(mean(multi_out$beta[,4,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))
pred_liom<-cbind(
  #pr(liom)
  exp((mean(multi_out$beta[,4,4])) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_liom,
  #pr(vac)
  exp((mean(multi_out$beta[,4,1])) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_liom)
sum(pred_liom[1,])
## Previously tended by none
Denominator_vac <- exp(mean(multi_out$beta[,1,4]) + size_dummy_real*mean(multi_out$beta[,5,4])) + 
  exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))
pred_vac<-cbind(
  #pr(liom)
  exp(mean(multi_out$beta[,1,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_vac,
  #pr(vac)
  exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_vac)
sum(pred_vac[1,])
                  ## liom-> liom        liom -> vac 
pred_probs_liom <- cbind((pred_liom[,1]) , (pred_liom[,2]))
                  ## vac-> liom         vac -> vac     
pred_probs_vac <- cbind((pred_vac[,1]) , (pred_vac[,2]))

lv_ant_multi <- cbind(pred_probs_vac, pred_probs_liom)
colnames(lv_ant_multi) <- c("vacliom","vacvac",
                            "liomliom","liomvac")
write.csv(lv_ant_multi,"lv_ant_multi.csv")



## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_Size_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3),
              ncol = 1, nrow = 3, byrow = TRUE), heights = c(0.6,1.4,1.4), widths = c(3.9))
plot.new()
text(0.5,0.5,"Ant States",cex=2,font=2)
## Prev Liom
plot(size_dummy_real, pred_liom[,1], type = "l", col = liomcol,main = "Previously Liom", ylim = c(0,1))
lines(size_dummy_real, pred_liom[,2], col = vaccol)
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_liom,pch=16,cex=multi_plot_liom$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_vac,pch=16,cex=multi_plot_liom$N_mod,col= alpha(vaccol, 0.4))
## Prev Vac
plot(size_dummy_real, pred_vac[,1], type = "l", col = liomcol,main = "Previously Vacant", ylim = c(0,1))
lines(size_dummy_real, pred_vac[,2], col = vaccol)
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_liom,pch=16,cex=multi_plot_vac$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_vac,pch=16,cex=multi_plot_vac$N_mod,col= alpha(vaccol, 0.4))
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
####################################### LIOM & CREM ############################################################
## Calculate the probabilities of being tended by each ant species
## Previously tended by none
Denominator_liom <- exp(mean(multi_out$beta[,4,4]) + size_dummy_real*mean(multi_out$beta[,5,4])) + 
  exp(mean(multi_out$beta[,4,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))
pred_liom<-cbind(
  #pr(liom)
  exp((mean(multi_out$beta[,4,4])) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_liom,
  #pr(crem)
  exp((mean(multi_out$beta[,4,3])) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_liom)
sum(pred_liom[1,])
## Previously tended by none
Denominator_crem <- exp(mean(multi_out$beta[,3,4]) + size_dummy_real*mean(multi_out$beta[,5,4])) + 
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))
pred_crem<-cbind(
  #pr(liom)
  exp(mean(multi_out$beta[,3,4]) + size_dummy_real*mean(multi_out$beta[,5,4]))/Denominator_crem,
  #pr(crem)
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_crem)
sum(pred_crem[1,])
                ## liom-> liom        liom -> crem 
pred_probs_liom <- cbind((pred_liom[,1]) , (pred_liom[,2]))
                ## crem-> liom         crem -> crem     
pred_probs_crem <- cbind((pred_crem[,1]) , (pred_crem[,2]))

lc_ant_multi <- cbind(pred_probs_crem,pred_probs_liom)
colnames(lc_ant_multi) <- c("cremliom","cremcrem",
                            "liomliom","cremliom")
write.csv(lc_ant_multi,"lc_ant_multi.csv")



## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_Size_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3),
              ncol = 1, nrow = 3, byrow = TRUE), heights = c(0.6,1.4,1.4), widths = c(3.9))
plot.new()
text(0.5,0.5,"Ant States",cex=2,font=2)
## Prev Liom
plot(size_dummy_real, pred_liom[,1], type = "l", col = liomcol,main = "Previously Liom", ylim = c(0,1))
lines(size_dummy_real, pred_liom[,2], col = cremcol)
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_liom,pch=16,cex=multi_plot_liom$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_crem,pch=16,cex=multi_plot_liom$N_mod,col= alpha(cremcol, 0.4))
## Prev Vac
plot(size_dummy_real, pred_crem[,1], type = "l", col = liomcol,main = "Previously Crem", ylim = c(0,1))
lines(size_dummy_real, pred_crem[,2], col = cremcol)
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_liom,pch=16,cex=multi_plot_crem$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_crem,pch=16,cex=multi_plot_crem$N_mod,col= alpha(cremcol, 0.4))
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
####################################### OTHER & VAC ############################################################
## Calculate the probabilities of being tended by each ant species
## Previously tended by none
Denominator_other <- exp(mean(multi_out$beta[,2,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,2,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))
pred_other<-cbind(
  #pr(other)
  exp((mean(multi_out$beta[,2,2])) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_other,
  #pr(vac)
  exp((mean(multi_out$beta[,2,1])) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_other)
sum(pred_other[1,])
## Previously tended by none
Denominator_vac <- exp(mean(multi_out$beta[,1,2]) + size_dummy_real*mean(multi_out$beta[,5,2])) + 
  exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))
pred_vac<-cbind(
  #pr(other)
  exp(mean(multi_out$beta[,1,2]) + size_dummy_real*mean(multi_out$beta[,5,2]))/Denominator_vac,
  #pr(vac)
  exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_vac)
sum(pred_vac[1,])
                    ## other-> other        other -> vac 
pred_probs_other <- cbind((pred_other[,1]) , (pred_other[,2]))
                    ## vac-> other         vac -> vac
pred_probs_vac <- cbind((pred_vac[,1]) , (pred_vac[,2]))

ov_ant_multi <- cbind(pred_probs_vac, pred_probs_other)
colnames(ov_ant_multi) <- c("vacother","vacvac",
                            "otherother","othervac")
write.csv(ov_ant_multi,"ov_ant_multi.csv")



## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_Size_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3),
              ncol = 1, nrow = 3, byrow = TRUE), heights = c(0.6,1.4,1.4), widths = c(3.9))
plot.new()
text(0.5,0.5,"Ant States",cex=2,font=2)
## Prev other
plot(size_dummy_real, pred_other[,1], type = "l", col = othercol,main = "Previously Other", ylim = c(0,1))
lines(size_dummy_real, pred_other[,2], col = vaccol)
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_other,pch=16,cex=multi_plot_other$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_vac,pch=16,cex=multi_plot_other$N_mod,col= alpha(vaccol, 0.4))
## Prev Vac
plot(size_dummy_real, pred_vac[,1], type = "l", col = othercol,main = "Previously Vacant", ylim = c(0,1))
lines(size_dummy_real, pred_vac[,2], col = vaccol)
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_other,pch=16,cex=multi_plot_vac$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_vac,pch=16,cex=multi_plot_vac$N_mod,col= alpha(vaccol, 0.4))
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
####################################### OTHER & LIOM ###########################################################
## Calculate the probabilities of being tended by each ant species
## Previously tended by none
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
                  ## other-> other        other -> liom 
pred_probs_other <- cbind((pred_other[,1]) , (pred_other[,2]))
                  ## liom-> other         liom -> liom
pred_probs_liom <- cbind((pred_liom[,1]) , (pred_liom[,2]))

ol_ant_multi <- cbind(pred_probs_other, pred_probs_liom)
colnames(ol_ant_multi) <- c( "otherother","otherliom",
                             "liomother","liomliom")
write.csv(ol_ant_multi,"ol_ant_multi.csv")



## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_Size_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3),
              ncol = 1, nrow = 3, byrow = TRUE), heights = c(0.6,1.4,1.4), widths = c(3.9))
plot.new()
text(0.5,0.5,"Ant States",cex=2,font=2)
## Prev other
plot(size_dummy_real, pred_other[,1], type = "l", col = othercol,main = "Previously Other", ylim = c(0,1))
lines(size_dummy_real, pred_other[,2], col = liomcol)
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_other,pch=16,cex=multi_plot_other$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_liom,pch=16,cex=multi_plot_other$N_mod,col= alpha(liomcol, 0.4))
## Prev Vac
plot(size_dummy_real, pred_liom[,1], type = "l", col = othercol,main = "Previously Liom", ylim = c(0,1))
lines(size_dummy_real, pred_liom[,2], col = liomcol)
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_other,pch=16,cex=multi_plot_liom$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_liom,pch=16,cex=multi_plot_liom$N_mod,col= alpha(liomcol, 0.4))
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
####################################### VAC & CREM #############################################################
## Calculate the probabilities of being tended by each ant species
## Previously tended by none
Denominator_vac <- exp(mean(multi_out$beta[,1,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,1,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))
pred_vac<-cbind(
  #pr(vac)
  exp((mean(multi_out$beta[,1,1])) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_vac,
  #pr(crem)
  exp((mean(multi_out$beta[,1,3])) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_vac)
sum(pred_vac[1,])
## Previously tended by Crem
Denominator_crem <- exp(mean(multi_out$beta[,3,1]) + size_dummy_real*mean(multi_out$beta[,5,1])) + 
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))
pred_liom<-cbind(
  #pr(vac)
  exp(mean(multi_out$beta[,3,1]) + size_dummy_real*mean(multi_out$beta[,5,1]))/Denominator_crem,
  #pr(crem)
  exp(mean(multi_out$beta[,3,3]) + size_dummy_real*mean(multi_out$beta[,5,3]))/Denominator_crem)
sum(pred_crem[1,])
                  ## vac-> vac        vac -> crem 
pred_probs_vac <- cbind((pred_vac[,1]) , (pred_vac[,2]))
                  ## crem-> vac         crem -> crem
pred_probs_crem <- cbind((pred_crem[,1]) , (pred_crem[,2]))

cv_ant_multi <- cbind(pred_probs_vac, pred_probs_crem)
colnames(cv_ant_multi) <- c("vacvac","vaccrem",
                            "cremvac","cremcrem")
write.csv(cv_ant_multi,"cv_ant_multi.csv")



## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_Size_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3),
              ncol = 1, nrow = 3, byrow = TRUE), heights = c(0.6,1.4,1.4), widths = c(3.9))
plot.new()
text(0.5,0.5,"Ant States",cex=2,font=2)
## Prev vac
plot(size_dummy_real, pred_vac[,1], type = "l", col = vaccol,main = "Previously Vac", ylim = c(0,1))
lines(size_dummy_real, pred_vac[,2], col = cremcol)
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_vac,pch=16,cex=multi_plot_vac$N_mod,col= alpha(vaccol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_crem,pch=16,cex=multi_plot_vac$N_mod,col= alpha(cremcol, 0.4))
## Prev crem
plot(size_dummy_real, pred_crem[,1], type = "l", col = vaccol,main = "Previously Crem", ylim = c(0,1))
lines(size_dummy_real, pred_crem[,2], col = cremcol)
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_vac,pch=16,cex=multi_plot_crem$N_mod,col= alpha(vaccol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_crem,pch=16,cex=multi_plot_crem$N_mod,col= alpha(cremcol, 0.4))
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")



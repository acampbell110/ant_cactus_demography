#######################################################################################################
##
##                  The purpose of this file is to run each vital rate sub model separately,
##                          save the outputs, and check the posterior distributions 
##
#######################################################################################################

## First read the data in 
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
#setwd("C:/Users/tm9/Dropbox/github/ant_cactus_demography")
#setwd("/Users/Labuser/Documents/GitHub/ant_cactus_demography")
cactus <- read.csv("cholla_demography_20042021_cleaned.csv", header = TRUE,stringsAsFactors=T)



levels(cactus$ant_t)
levels(cactus$ant_t1)


##############################################################################################
##
##   Skew Growth Model -- What size will the cacti be next time step?
##
##############################################################################################
## Pull all necessary variables together, remove NAs, and put them into a list so they are
## ready to feed into the stan model
growth_data_orig <- cactus[,c("Plot","Year_t","logsize_t","logsize_t1","ant_t")]
growth_data <- na.omit(growth_data_orig)
## Lose 2032 rows (due to plant death & recruit status)
nrow(growth_data_orig)
nrow(growth_data)
# check that you are happy with the subsetting by plotting the original and cleaned data
plot(growth_data$logsize_t, growth_data$logsize_t1)
points((cactus$logsize_t), (cactus$logsize_t1), col = "red")
## Make a list of all necessary variables so they are properly formatted to feed into the stan model
stan_data_grow_skew <- list(N = nrow(growth_data),                                ## number of observations
                            vol = (growth_data$logsize_t), ## predictor volume year t
                            y = (growth_data$logsize_t1),                              ## response volume next year
                            ant = as.integer(as.factor(growth_data$ant_t)),            ## predictor ant state
                            K = 4,                                                     ## number of ant states
                            N_Year = max(as.integer(as.factor(growth_data$Year_t))),   ## number of years
                            N_Plot = max(as.integer(as.factor(growth_data$Plot))),     ## number of plots
                            plot = as.integer(as.factor(growth_data$Plot)),            ## predictor plots
                            year = as.integer(as.factor(growth_data$Year_t))           ## predictor years
)
########## growth model with a skew normal distribution -- fixed effects: previous size and ant state, ##############
########## random effects: plot and year, size variation is included for both the omega and alpha estimates #########
fit_grow_skew <- stan(file = "Data Analysis/STAN Models/grow_skew.stan", data = stan_data_grow_skew, 
                      warmup = 3000, iter = 20000, chains = 3, cores = 3, thin = 2)

########## extract the parameters from the model and save a random selection of the iterations
## list all parameters
fit_grow_skew@model_pars
## pull all iterations for parameters and save as a data frame
grow_outputs <- rstan::extract(fit_grow_skew, pars = c("w","beta0","beta1","u","d_0","d_size","a_0","a_size","sigma_w","sigma_u"))
grow_outputs <- as.data.frame(grow_outputs)
## pull 1000 random rows from the data frame and export it
draws<-sample(nrow(grow_outputs),1000)
grow.params <- grow_outputs[draws,]
write.csv(grow.params, "grow.params.csv")
########## Xi
## pull all iterations for parameters and save as a data frame
grow_xi <- rstan::extract(fit_grow_skew, pars = c("xi"))
grow_xi <- as.data.frame(grow_xi)
## pull 1000 random rows from the data frame and export it
grow.xi <- grow_xi[draws,]
write.csv(grow.xi, "grow.xi.csv")
########## Omega
grow_omega <- rstan::extract(fit_grow_skew, pars = c("omega"))
grow_omega <- as.data.frame(grow_omega)
## pull 1000 random rows from the data frame and export it
grow.omega <- grow_omega[draws,]
write.csv(grow.omega, "grow.omega.csv")
########## Alpha
grow_alpha <- rstan::extract(fit_grow_skew, pars = c("xi"))
grow_alpha <- as.data.frame(grow_alpha)
## pull 1000 random rows from the data frame and export it
grow.alpha <- grow_alpha[draws,]
write.csv(grow.alpha, "grow.alpha.csv")
########## Y reps
grow_yrep <- rstan::extract(fit_grow_skew, pars = c("y_rep"))
grow_yrep <- as.data.frame(grow_yrep)
## pull 1000 random rows from the data frame and export it
grow.yrep <- grow_yrep[draws,]
write.csv(grow.yrep, "grow.yrep.csv")
########### Check the posterior distribution of the model and the convergence of each of the parameters
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
setwd("/Users/Labuser/Documents/GitHub/ant_cactus_demography/Figures")
#For overlay plots
y <- stan_data_grow_skew$y
ant <- stan_data_grow_skew$ant
## Read in the data and format it properly to use to simulate data
## remove the first column which is the iteration id and format as a matrix.
outputs <- read.csv("/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow.params.csv", header = TRUE,stringsAsFactors=T)
#outputs <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow.params.csv", header = TRUE,stringsAsFactors=T)
outputs <- outputs[,c(-1)]
outs <- as.matrix(outputs)
########## xi
xi <- read.csv("/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow.xi.csv", header = TRUE,stringsAsFactors=T)
#xi <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow.xi.csv", header = TRUE,stringsAsFactors=T)
xi <- xi[,c(-1)]
xi <- as.matrix(xi)
########## omega
#omega <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow.omega.csv", header = TRUE,stringsAsFactors=T)
omega <- read.csv("/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow.omega.csv", header = TRUE,stringsAsFactors=T)
omega <- omega[,c(-1)]
omega <- as.matrix(omega)
########## alpha
#alpha <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow.alpha.csv", header = TRUE,stringsAsFactors=T)
alpha <- read.csv("/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow.alpha.csv", header = TRUE,stringsAsFactors=T)
alpha <- alpha[,c(-1)]
alpha <- as.matrix(alpha)
########## y_rep
y_rep <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow.yrep.csv", header = TRUE,stringsAsFactors=T)
#y_rep <- read.csv("/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow.yrep.csv", header = TRUE,stringsAsFactors=T)
y_rep <- y_rep[,c(-1)]
y_rep <- as.matrix(y_rep)
## Simulate data using the model outputs
yc = stan_data_grow_skew$vol[stan_data_grow_skew$ant==1]
yl = stan_data_grow_skew$vol[stan_data_grow_skew$ant==2]
yo = stan_data_grow_skew$vol[stan_data_grow_skew$ant==3]
yv = stan_data_grow_skew$vol[stan_data_grow_skew$ant==4]
y_sim_c <- matrix(NA, 100,length(yc))
y_sim_l <- matrix(NA, 100,length(yl))
y_sim_o <- matrix(NA, 100,length(yo))
y_sim_v <- matrix(NA, 100,length(yv))
y_sim <- matrix(NA,100,length(y))
dim(y_sim_c) ## Each row is an iteration, each column is a data point
# for(i in 1:1000){
#   y_sim[i,] <- rsn(n=length(y), xi = (xi[i,]), omega = (omega[i,]), alpha = alpha[i,])
# }
for(i in 1:100){
  #for(j in 1:length(size_dummy)){
    y_sim_c[i,] <- rsn(n=length(yc), xi=outputs$beta0.1[i]+outputs$beta1.1[i]*yc,
                     omega=exp(outputs$d_0[i]+outputs$d_size[i]*yc),
                     alpha=outputs$a_0[i]+outputs$a_size[i]*yc)
    y_sim_l[i,] <- rsn(n=length(yl), xi=outputs$beta0.2[i]+outputs$beta1.2[i]*yl,
                        omega=exp(outputs$d_0[i]+outputs$d_size[i]*yl),
                        alpha=outputs$a_0[i]+outputs$a_size[i]*yl)
    y_sim_o[i,] <- rsn(n=length(yo), xi=outputs$beta0.3[i]+outputs$beta1.3[i]*yo,
                        omega=exp(outputs$d_0[i]+outputs$d_size[i]*yo),
                        alpha=outputs$a_0[i]+outputs$a_size[i]*yo)
    y_sim_v[i,] <- rsn(n=length(yv), xi=outputs$beta0.4[i]+outputs$beta1.4[i]*yv,
                        omega=exp(outputs$d_0[i]+outputs$d_size[i]*yv),
                        alpha=outputs$a_0[i]+outputs$a_size[i]*yv)
    y_sim[i,] <- rsn(n=length(y), xi = (xi[i,]), omega = (omega[i,]), alpha = alpha[i,])
  #}
}


## Plot the simulated data over the real data (separated by ant partners)
## This looks pretty good
png(file = "grow_post_ysim.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay_grouped(y, y_sim) + xlim(-50,50)
dev.off()
png("grow_post_hand.png")
par(mfrow=c(2,2))
## Crem
plot(density(y_sim_c[1,]))
for(i in 1:100){
  lines(density(y_sim_c[i,]), col = "pink")
}
lines(density(stan_data_grow_skew$y[stan_data_grow_skew$ant==1]), col = "maroon", lwd = 3)
## Liom
plot(density(y_sim_l[1,]))
for(i in 1:100){
  lines(density(y_sim_l[i,]), col = "pink")
}
lines(density(stan_data_grow_skew$y[stan_data_grow_skew$ant==2]), col = "maroon", lwd = 3)
## Other
plot(density(y_sim_o[1,]))
for(i in 1:100){
  lines(density(y_sim_o[i,]), col = "pink")
}
lines(density(stan_data_grow_skew$y[stan_data_grow_skew$ant==3]), col = "maroon", lwd = 3)
## Vacant
plot(density(y_sim_v[1,]))
for(i in 1:100){
  lines(density(y_sim_v[i,]), col = "pink")
}
lines(density(stan_data_grow_skew$y[stan_data_grow_skew$ant==4]), col = "maroon", lwd = 3)
dev.off()

## Plot the convergence of all chains for parameters
## They all converge
png("grow_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_grow_skew, pars=c("beta0", "beta1","a_0","a_size","d_0","d_size")))
dev.off()

#### Check the mean, sd, skew, and kurtosis fits of the model across sizes
y = stan_data_grow_skew$y[stan_data_grow_skew$ant == 4]
x = stan_data_grow_skew$vol[stan_data_grow_skew$ant == 4]
## All data together
#y <- stan_data_grow_skew$y
#x <- stan_data_grow_skew$vol
data <- data.frame(x=x,y=y)
n_bins<-10
require(tidyverse)
require(patchwork)
bins <- data %>%
  ungroup() %>% 
  arrange(x) %>% 
  mutate(size_bin = cut_number(x, n_bins)) %>% 
  group_by(size_bin)  %>% 
  dplyr::summarize(mean_t1 = mean(y),
                   sd_t1 = sd(y),
                   skew_t1 = skewness(y),
                   kurt_t1 = Lkurtosis(y),
                   bin_mean = mean(x),
                   bin_n = n())
sim_moments <- bind_cols(enframe(data$x), as_tibble(t(y_sim_v))) %>%
  rename(x = value) %>%
  arrange(x) %>%
  mutate(size_bin = cut_number(x, n_bins)) %>%
  pivot_longer(., cols = starts_with("V"), names_to = "post_draw", values_to = "sim") %>%
  group_by(size_bin, post_draw) %>%
  summarize( mean_sim = mean((sim)),
             sd_sim = sd((sim)),
             skew_sim = skewness((sim)),
             kurt_sim = Lkurtosis((sim)),
             bin_mean = mean(x),
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
size_ppc_plot <- meanplot+ sdplot+skewplot+ kurtplot
size_ppc_plot

png("grow_moments_vac.png")
size_ppc_plot
dev.off()


###### Check the significance of the differences between survival rates
## create data set where each column is an estimated survival rate
grow_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow.params.csv", header = TRUE,stringsAsFactors=T)
crem_est <- invlogit(mean(surv_out$beta0.3) + mean(surv_out$beta1.3)*size_dummy)
liom_est <- invlogit(mean(surv_out$beta0.4) + mean(surv_out$beta1.4)*size_dummy)
other_est <- invlogit(mean(surv_out$beta0.2) + mean(surv_out$beta1.2)*size_dummy)
vac_est <- invlogit(mean(surv_out$beta0.1) + mean(surv_out$beta1.1)*size_dummy)
estimates <- cbind(crem_est, liom_est, other_est, vac_est)
## crem and liom -- p = 0.9135
t.test(estimates[,1],estimates[,2], alternative = "two.sided")
## crem and other -- p = 0.4864
t.test(estimates[,1],estimates[,3], alternative = "two.sided")
## crem and vac -- p = 4.488e-14. ***
t.test(estimates[,1],estimates[,4], alternative = "two.sided")
## liom and other -- p = 0.3899
t.test(estimates[,2],estimates[,3], alternative = "two.sided")
## liom and vac -- p = 2.2e-16.   ***
t.test(estimates[,2],estimates[,4], alternative = "two.sided")
## other and vac -- p = 2.908e-14 ***
t.test(estimates[,3],estimates[,4], alternative = "two.sided")


#######################################################################################################
#######################################################################################################
##
##  Survival Model -- What is the probability of surviving to the next time step?   
##
#######################################################################################################
#######################################################################################################
## Pull all necessary variables together, remove NAs, and put them into a list so they are
## ready to feed into the stan model
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
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
fit_surv <- stan(file = "Data Analysis/STAN Models/surv_code.stan", data = stan_data_surv, warmup = 1500, iter = 10000, chains = 3, cores = 3, thin = 1)
########## extract the parameters from the model and save a random selection of the iterations
## list all parameters
fit_surv@model_pars
## pull all iterations for parameters and save as a data frame
surv_outputs <- rstan::extract(fit_surv, pars = c("w","beta0","beta1","u","sigma_w","sigma_u"))
surv_outputs <- as.data.frame(surv_outputs)
## pull 1000 random rows from the data frame and export it
draws<-sample(nrow(surv_outputs),1000)
surv.params <- surv_outputs[draws,]
write.csv(surv.params, "surv.params.csv")
## mu
surv_mu <- rstan::extract(fit_surv, pars = c("mu"))
surv_mu <- as.data.frame(surv_mu)
## pull 1000 random rows from the data frame and export it
surv.mu <- surv_mu[draws,]
write.csv(surv.mu, "surv.mu.csv")
## sigma
surv_sigma <- rstan::extract(fit_surv, pars = c("sigma"))
surv_sigma <- as.data.frame(surv_sigma)
## pull 1000 random rows from the data frame and export it
surv.sigma <- surv_sigma[draws,]
write.csv(surv.sigma, "surv.sigma.csv")

## Visualize the posterior distributions
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
#overlay plot data
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
  #y_sim[i,] <- rbinom(n=length(y), mean = surv_yrep[i,], sd = surv_sigma[i,])
  y_sim[i,] <- rbinom(n=length(y), size=1, prob = invlogit(mean(surv_mu[i,])))
}
view(y_sim)
## Overlay Plots
png(file = "surv_post.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay_grouped(y, y_sim,group = ant)
dev.off()
## Convergence Plots
png(file = "surv_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_surv, pars=c("beta0","beta1")))
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

###### Check the significance of the differences between survival rates
## create data set where each column is an estimated survival rate
surv_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/surv.params.csv", header = TRUE,stringsAsFactors=T)
crem_est <- invlogit(mean(surv_out$beta0.1) + mean(surv_out$beta1.1)*size_dummy)
liom_est <- invlogit(mean(surv_out$beta0.2) + mean(surv_out$beta1.2)*size_dummy)
other_est <- invlogit(mean(surv_out$beta0.3) + mean(surv_out$beta1.3)*size_dummy)
vac_est <- invlogit(mean(surv_out$beta0.4) + mean(surv_out$beta1.4)*size_dummy)
estimates <- cbind(crem_est, liom_est, other_est, vac_est)
## crem and liom -- p = 2.908 e-14  *** 
t.test(estimates[,1],estimates[,2], alternative = "two.sided")
## crem and other -- p = 4.488 e-14 ***
t.test(estimates[,1],estimates[,3], alternative = "two.sided")
## crem and vac -- p = 2.2e-16      ***
t.test(estimates[,1],estimates[,4], alternative = "two.sided")
## liom and other -- p = 0.4864 
t.test(estimates[,2],estimates[,3], alternative = "two.sided")
## liom and vac -- p = 0.3899
t.test(estimates[,2],estimates[,4], alternative = "two.sided")
## other and vac -- p = 0.9135
t.test(estimates[,3],estimates[,4], alternative = "two.sided")

######################################################################################################
######################################################################################################
####
#### Flowering Model -- What are the total number of fruits produced in the next time step? ##########
####
######################################################################################################
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
fit_flow_trunc <- stan(file = "Data Analysis/STAN Models/flower_trunc_code.stan", data = stan_data_flow_trunc, warmup = 1500, iter = 10000, chains = 3, cores = 3, thin = 1)
fit_flow_trunc@model_pars
draws<-sample(nrow(surv_outputs),1000)
########## extract the parameters from the model and save a random selection of the iterations
## list all parameters
## pull all iterations for parameters and save as a data frame
flow_outputs <- rstan::extract(fit_flow_trunc, pars = c("w","beta0","beta1","u","sigma_w","sigma_u"))
flow_outputs <- as.data.frame(flow_outputs)
## pull 1000 random rows from the data frame and export it
flow.params <- flow_outputs[draws,]
write.csv(flow.params, "flow.params.csv")
#### Phi
flow_phi <- rstan::extract(fit_flow_trunc, pars = c("phi"))
flow_phi <- as.data.frame(flow_phi)
## pull 1000 random rows from the data frame and export it
flow.phi <- flow_phi[draws,]
write.csv(flow.phi, "flow.phi.csv")
#### Mu
flow_mu <- rstan::extract(fit_flow_trunc, pars = c("mu"))
flow_mu <- as.data.frame(flow_mu)
## pull 1000 random rows from the data frame and export it
flow.mu <- flow_mu[draws,]
write.csv(flow.mu, "flow.mu.csv")
## Check the posterior distributions
y <- flower_data$TotFlowerbuds_t
#flow_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/flow.params.csv", header = TRUE,stringsAsFactors=T)
flow_data <- read.csv("flow.params.csv", header = TRUE,stringsAsFactors=T)
# Create the y rep (needs to be done outside of STAN because of the 0 truncation)
y_sim <- matrix(NA,1000,length(y))
for(i in 1:1000){
  for(j in 1:length(y)){
    y_sim[i,j] <- sample(x=1:1000,size=1,replace=T,prob=dnbinom(1:1000, mu = exp(flow.mu[i,j]), size=flow.phi[i]) / (1 - dnbinom(0, mu = exp(flow.mu[i,j]), size=flow.phi[i])))
  }
}
## Plot the posterior distributions
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("flow_post.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay(y, y_sim)
dev.off()
## Convergence Plots
png(file = "flow_conv.png")
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
unique(viability_data_orig$Year_t)
## Lose __ Rows of data
view(viability_data_orig)
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
fit_viab <- stan(file = "Data Analysis/STAN Models/viab_code.stan", data = stan_data_viab, warmup = 1500, iter = 10000, chains = 3, cores = 3, thin = 1)
fit_viab@model_pars
draws<-sample(nrow(surv_outputs),1000)
## list all parameters
## pull all iterations for parameters and save as a data frame
viab_outputs <- rstan::extract(fit_viab, pars = c("w","beta0","u","sigma_w","sigma_u"))
viab_outputs <- as.data.frame(viab_outputs)
## pull 1000 random rows from the data frame and export it
viab.params <- viab_outputs[draws,]
write.csv(viab.params, "viab.params.csv")
## pull all iterations for parameters and save as a data frame
viab_sigma <- rstan::extract(fit_viab, pars = c("sigma"))
viab_sigma <- as.data.frame(viab_sigma)
## pull 1000 random rows from the data frame and export it
viab.sigma <- viab_sigma[draws,]
write.csv(viab.sigma, "viab.sigma.csv")
## pull all iterations for parameters and save as a data frame
viab_mu <- rstan::extract(fit_viab, pars = c("mu"))
viab_mu <- as.data.frame(viab_mu)
## pull 1000 random rows from the data frame and export it
viab.mu <- viab_mu[draws,]
write.csv(viab.mu, "viab.mu.csv")

## Check the Posterior Distribution
y <- viability_data$Goodbuds_t1
#viab_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/viab.params.csv", header = TRUE,stringsAsFactors=T)
viab_data <- read.csv("viab.params.csv", header = TRUE,stringsAsFactors=T)
viab_data <- viab_data[,c(-1)]
viab_data <- as.matrix(viab_data)
#viab_mu <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/viab.mu.csv", header = TRUE,stringsAsFactors=T)
viab_mu <- read.csv("viab.mu.csv", header = TRUE,stringsAsFactors=T)
viab_mu <- viab_mu[,c(-1)]
viab_mu <- as.matrix(viab_mu)
y_sim <- matrix(NA,1000,length(y))
for(i in 1:1000){
  y_sim[i,] <- rbern(n = length(y), prob = invlogit(mean(viab_mu[i,])))
}
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
## Overlay Plots
png(file = "viab_post.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay(y, y_sim)
dev.off()
png(file = "viab_ant_post.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay_grouped(y, y_sim,group = as.integer(as.factor(viability_data$ant)))
dev.off()
## Convergence Plots
png(file = "viab_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_viab, pars=c("beta0")))
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

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
fit_repro <- stan(file = "Data Analysis/STAN Models/repro_code.stan", data = stan_data_repro, warmup = 1500, iter = 10000, chains = 3, cores = 3, thin = 1)
fit_repro@model_pars
########## extract the parameters from the model and save a random selection of the iterations
## list all parameters
## pull all iterations for parameters and save as a data frame
repro_outputs <- rstan::extract(fit_repro, pars = c("w","beta0","beta1","u","sigma_w","sigma_u"))
repro_outputs <- as.data.frame(repro_outputs)
## pull 1000 random rows from the data frame and export it
draws<-sample(nrow(repro_outputs),1000)
repro.params <- repro_outputs[draws,]
write.csv(repro.params, "repro.params.csv")
## Mu
## pull all iterations for parameters and save as a data frame
repro_mu <- rstan::extract(fit_repro, pars = c("mu"))
repro_mu <- as.data.frame(repro_mu)
## pull 1000 random rows from the data frame and export it
repro.mu <- repro_mu[draws,]
write.csv(repro.mu, "repro.mu.csv")
## Sigma
## pull all iterations for parameters and save as a data frame
repro_sigma <- rstan::extract(fit_repro, pars = c("sigma"))
repro_sigma <- as.data.frame(repro_sigma)
## pull 1000 random rows from the data frame and export it
repro.sigma <- repro_sigma[draws,]
write.csv(repro.sigma, "repro.sigma.csv")
## Check the Posteriors
#repro_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/repro.params.csv", header = TRUE,stringsAsFactors=T)
repro_data <- read.csv("repro.params.csv", header = TRUE,stringsAsFactors=T)
repro_data <- repro_data[,c(-1)]
repro_data <- as.matrix(repro_data)
#repro_mu <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/repro.mu.csv", header = TRUE,stringsAsFactors=T)
repro_mu <- read.csv("repro.mu.csv", header = TRUE,stringsAsFactors=T)
repro_mu <- repro_mu[,c(-1)]
repro_mu <- as.matrix(repro_mu)
y <- as.numeric(reproductive_data$flower1_YN)
y_sim <- matrix(NA,1000,length(y))
for(i in 1:1000){
  y_sim[i,] <- rbern(n = length(y), prob = invlogit(mean(repro_mu[i,])))
}
## Overlay Plots
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png(file = "repro_post.png")
bayesplot::ppc_dens_overlay(y, y_sim)
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
fit_seed <- stan(file = "Data Analysis/STAN Models/seed_code.stan", data = stan_data_seed, warmup = 1500, iter = 10000, chains = 3, cores = 3, thin = 1)
fit_seed@model_pars
## pull all iterations for parameters and save as a data frame
seed_outputs <- rstan::extract(fit_seed, pars = c("beta0"))
seed_outputs <- as.data.frame(seed_outputs)
## pull 1000 random rows from the data frame and export it
seed.params <- seed_outputs[draws,]
write.csv(seed.params, "seed.params.csv")
## mu
seed_mu <- rstan::extract(fit_seed, pars = c("mu"))
seed_mu <- as.data.frame(seed_mu)
## pull 1000 random rows from the data frame and export it
seed.mu <- seed_mu[draws,]
write.csv(seed.mu, "seed.mu.csv")
## sigma
seed_sigma <- rstan::extract(fit_seed, pars = c("sigma"))
seed_sigma <- as.data.frame(seed_sigma)
## pull 1000 random rows from the data frame and export it
seed.sigma <- seed_sigma[draws,]
write.csv(seed.sigma, "seed.sigma.csv")
## phi
seed_phi <- rstan::extract(fit_seed, pars = c("phi"))
seed_phi <- as.data.frame(seed_phi)
## pull 1000 random rows from the data frame and export it
seed.phi <- seed_phi[draws,]
write.csv(seed.phi, "seed.phi.csv")
## Check the Posteriors
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
seed_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/seed.params.csv", header = TRUE,stringsAsFactors=T)
seed_data <- seed_data[,c(-1)]
seed_data <- as.matrix(seed_data)
seed_mu <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/seed.mu.csv", header = TRUE,stringsAsFactors=T)
seed_mu <- seed_mu[,c(-1)]
seed_mu <- as.matrix(seed_mu)
seed_phi <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/seed.phi.csv", header = TRUE,stringsAsFactors=T)
seed_phi <- seed_phi[,c(-1)]
seed_phi <- as.matrix(seed_phi)
y <- stan_data_seed$seed
ant = stan_data_seed$ant
y_sim <- matrix(NA,1000,length(y))
for(i in 1:1000){
  y_sim[i,] <- rnegbin(n = length(y), mu = seed_mu[i,], theta = seed_phi[i,])
}
## Overlay Plots
png(file = "seed_post.png")
bayesplot::ppc_dens_overlay(y, y_sim)
dev.off()
png(file = "seed_ant_post.png")
bayesplot::ppc_dens_overlay_grouped(y, y_sim,group = ant)
dev.off()
## Convergence Plots
png(file = "seed_conv.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_seed, pars=c("beta0")))
dev.off()

###### Check the significance of the differences between survival rates
## create data set where each column is an estimated survival rate
seed_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/seed.params.csv", header = TRUE,stringsAsFactors=T)
crem_est <- (exp(viab_out$beta0.1))
liom_est <- (exp(viab_out$beta0.2))
vac_est <- (exp(viab_out$beta0.3))
estimates <- cbind(crem_est, liom_est, vac_est)
## crem and liom -- p = 2.2 e-16  *** 
t.test(estimates[,1],estimates[,2], alternative = "two.sided")
## crem and vac -- p = 2.2e-16    ***
t.test(estimates[,1],estimates[,3], alternative = "two.sided")
## liom and vac -- p = 0.02521 
t.test(estimates[,2],estimates[,3], alternative = "two.sided")

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
fit_seed_surv <- stan(file = "Data Analysis/STAN Models/seed_surv_code.stan", data = stan_data_seed_surv, warmup = 1500, iter = 10000, chains = 3, cores = 3, thin = 1)
fit_seed_surv@model_pars
## pull all iterations for parameters and save as a data frame
seed_surv_outputs <- rstan::extract(fit_seed_surv, pars = c("beta0","w","sigma_w","mu","sigma"))
seed_surv_outputs <- as.data.frame(seed_surv_outputs)
## pull 1000 random rows from the data frame and export it
seed.surv.params <- seed_surv_outputs[draws,]
write.csv(seed.surv.params, "seed.surv.params.csv")
## yrep
seed_surv_yrep <- rstan::extract(fit_seed_surv, pars = c("y_rep"))
seed_surv_yrep <- as.data.frame(seed_surv_yrep)
## pull 1000 random rows from the data frame and export it
seed.surv.yrep <- seed_surv_yrep[draws,]
write.csv(seed.surv.yrep, "seed.surv.yrep.csv")
summary(fit_seed_surv)[1]
## Check the Posteriors
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
y <- precensus.dat$survive0405
seed_surv_mu <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/seed.surv.yrep.csv", header = TRUE,stringsAsFactors=T)
seed_surv_mu <- seed_surv_mu[,c(-1)]
seed_surv_mu <- as.matrix(seed_surv_mu)
## Overlay Plots
png(file = "seed_surv_post.png")
bayesplot::ppc_dens_overlay(y, seed_surv_mu)
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
fit_germ1 <- stan(file = "Data Analysis/STAN Models/germ_code.stan", data = stan_data_germ1, warmup = 1500, iter = 10000, chains = 3, cores = 3, thin = 1)
fit_germ2 <- stan(file = "Data Analysis/STAN Models/germ_code.stan", data = stan_data_germ2, warmup = 1500, iter = 10000, chains = 3, cores = 3, thin = 1)
fit_germ1@model_pars
fit_germ2@model_pars
## pull all iterations for parameters and save as a data frame
## germ 1 params
germ1_outputs <- rstan::extract(fit_germ1, pars = c("beta0","mu","sigma"))
germ1_outputs <- as.data.frame(germ1_outputs)
## pull 1000 random rows from the data frame and export it
germ1.params <- germ1_outputs[draws,]
write.csv(germ1.params, "germ1.params.csv")
## germ 2 params
germ2_outputs <- rstan::extract(fit_germ2, pars = c("beta0","mu","sigma"))
germ2_outputs <- as.data.frame(germ2_outputs)
## pull 1000 random rows from the data frame and export it
germ2.params <- germ2_outputs[draws,]
write.csv(germ2.params, "germ2.params.csv")
## germ 1 yrep
germ1_yrep <- rstan::extract(fit_germ1, pars = c("y_rep"))
germ1_yrep <- as.data.frame(germ1_yrep)
## pull 1000 random rows from the data frame and export it
germ1.yrep <- germ1_yrep[draws,]
write.csv(germ1.yrep, "germ1.yrep.csv")
## germ 2 yrep
germ2_yrep <- rstan::extract(fit_germ2, pars = c("y_rep"))
germ2_yrep <- as.data.frame(germ2_yrep)
## pull 1000 random rows from the data frame and export it
germ2.yrep <- germ2_yrep[draws,]
write.csv(germ2.yrep, "germ2.yrep.csv")
## Check the Posteriors
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
## Germ yr 1
y <- germ.dat$Seedlings04/germ.dat$Input
germ1_mu <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/germ1.yrep.csv", header = TRUE,stringsAsFactors=T)
germ1_mu <- germ1_mu[,c(-1)]
germ1_mu <- as.matrix(germ1_mu)
## Overlay Plots
png(file = "germ1_post.png")
bayesplot::ppc_dens_overlay(y, germ1_mu)
dev.off()
## Convergence Plots
png(file = "germ1_conv.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_germ1, pars=c("beta0")))
dev.off()
## Germ yr 2
y <- germ.dat$Seedlings05/(germ.dat$Input - germ.dat$Seedlings04)
germ2_mu <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/germ2.yrep.csv", header = TRUE,stringsAsFactors=T)
germ2_mu <- germ2_mu[,c(-1)]
germ2_mu <- as.matrix(germ2_mu)## Overlay Plots## Overlay Plots
png(file = "germ2_post.png")
bayesplot::ppc_dens_overlay(y, germ2_mu)
dev.off()
## Convergence Plots
png(file = "germ2_conv.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_germ2, pars=c("beta0")))
dev.off()

###### Check the significance of the differences between germination rates
## create data set where each column is an estimated survival rate
germ1_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/germ1.params.csv", header = TRUE,stringsAsFactors=T)
germ2_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/germ2.params.csv", header = TRUE,stringsAsFactors=T)
germ1_est <- (invlogit(germ1_out$beta0))
germ2_est <- (invlogit(germ2_out$beta0))
estimates <- cbind(germ1_est, germ2_est)
## crem and liom -- p = 2.2 e-16  *** 
t.test(estimates[,1],estimates[,2], alternative = "two.sided")

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
fit_rec <- stan(file = "Data Analysis/STAN Models/rec_code.stan",data = stan_data_rec, warmup = 1500, iter = 10000, chains = 3, cores = 3, thin = 1)
fit_rec@model_pars
## pull all iterations for parameters and save as a data frame
## recruitment params
rec_outputs <- rstan::extract(fit_rec, pars = c("beta0","mu","sigma"))
rec_outputs <- as.data.frame(rec_outputs)
## pull 1000 random rows from the data frame and export it
rec.params <- rec_outputs[draws,]
write.csv(rec.params, "rec.params.csv")
## recruitment yrep
rec_yrep <- rstan::extract(fit_rec, pars = c("y_rep"))
rec_yrep <- as.data.frame(rec_yrep)
## pull 1000 random rows from the data frame and export it
rec.yrep <- rec_yrep[draws,]
write.csv(rec.yrep, "rec.yrep.csv")
## Check Posterior Dist
y <- seedling.dat$logsize_t1
#rec_mu <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/rec.yrep.csv", header = TRUE,stringsAsFactors=T)
rec_mu <- read.csv("rec.yrep.csv", header = TRUE,stringsAsFactors=T)
rec_mu <- rec_mu[,c(-1)]
rec_mu <- as.matrix(rec_mu)
## Overlay Plots
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png(file = "rec_post.png")
bayesplot::ppc_dens_overlay(y, rec_mu)
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
                       P = 15, #number of random effect predictors
                       y = as.integer(as.factor(cactus_real$ant_t1)), #observations
                       x = model.matrix(~ 0 + (as.factor(ant_t)) + logsize_t, cactus_real), #design matrix
                       z = model.matrix(~0 + as.factor(Year_t), cactus_real),
                       N_Year = as.integer(length(unique(cactus_real$Year_t)))
)
## Run the model & save the results
fit_multi <- stan(file = "Data Analysis/STAN Models/multi_mixed.stan", 
                  data = multi_dat_real, warmup = 1500, iter = 10000, chains = 3)
fit_multi@model_pars
## pull all iterations for parameters and save as a data frame
## recruitment params
multi_outputs <- rstan::extract(fit_multi, pars = c("beta","beta_raw","theta","theta_raw","sigma_w"))
multi_outputs <- as.data.frame(multi_outputs)
## pull 1000 random rows from the data frame and export it
multi.params <- multi_outputs[draws,]
write.csv(multi.params, "multi.params.csv")
## P
## plot the chains
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("multi_conv_beta2021.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_multi, pars=c("beta")))
dev.off()
png("multi_conv_theta2021.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_multi, pars=c("theta[1,1]")))
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

summary(fit_multi)
## this looks pretty good. All betas converge

########################################## ALL ANTS ############################################################################
## Calculate the probabilities of being tended by each ant species
levels(cactus$ant_t)
multi_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/multi.params.csv", header = TRUE,stringsAsFactors=T)
size_dummy_real <- seq(min(cactus_real$logsize_t, na.rm = TRUE), max(cactus_real$logsize_t, na.rm = TRUE), by = 0.1)
## Previously tended by crem
Denominator_crem <- exp(mean(multi_out$beta.1.1) + size_dummy_real*mean(multi_out$beta.5.1)) + 
  exp(mean(multi_out$beta.1.2) + size_dummy_real*mean(multi_out$beta.5.2)) + 
  exp(mean(multi_out$beta.1.3) + size_dummy_real*mean(multi_out$beta.5.3)) + 
  exp(mean(multi_out$beta.1.4) + size_dummy_real*mean(multi_out$beta.5.4))
pred_crem<-cbind(
  #pr(crem)
  exp(mean(multi_out$beta.1.1) + size_dummy_real*mean(multi_out$beta.5.1))/Denominator_crem,
  #pr(liom)
  exp(mean(multi_out$beta.1.2) + size_dummy_real*mean(multi_out$beta.5.2))/Denominator_crem,
  #pr(other)
  exp(mean(multi_out$beta.1.3) + size_dummy_real*mean(multi_out$beta.5.3))/Denominator_crem,
  #pr(vac)
  exp(mean(multi_out$beta.1.4) + size_dummy_real*mean(multi_out$beta.5.4))/Denominator_crem)
sum(pred_crem[1,])

## Previously tended by Liom
Denominator_liom <- exp(mean(multi_out$beta.2.1) + size_dummy_real*mean(multi_out$beta.5.1)) + 
  exp(mean(multi_out$beta.2.2) + size_dummy_real*mean(multi_out$beta.5.2)) + 
  exp(mean(multi_out$beta.2.3) + size_dummy_real*mean(multi_out$beta.5.3)) + 
  exp(mean(multi_out$beta.2.4) + size_dummy_real*mean(multi_out$beta.5.4))
pred_liom<-cbind(
  #pr(crem)
  exp(mean(multi_out$beta.2.1) + size_dummy_real*mean(multi_out$beta.5.1))/Denominator_liom,
  #pr(liom)
  exp(mean(multi_out$beta.2.2) + size_dummy_real*mean(multi_out$beta.5.2))/Denominator_liom,
  #pr(other)
  exp(mean(multi_out$beta.2.3) + size_dummy_real*mean(multi_out$beta.5.3))/Denominator_liom,
  #pr(vac)
  exp(mean(multi_out$beta.2.4) + size_dummy_real*mean(multi_out$beta.5.4))/Denominator_liom)
sum(pred_liom[1,])

## Previously tended by other
Denominator_other <- exp(mean(multi_out$beta.3.1) + size_dummy_real*mean(multi_out$beta.5.1)) + 
  exp(mean(multi_out$beta.3.2) + size_dummy_real*mean(multi_out$beta.5.2)) + 
  exp(mean(multi_out$beta.3.3) + size_dummy_real*mean(multi_out$beta.5.3)) + 
  exp(mean(multi_out$beta.3.4) + size_dummy_real*mean(multi_out$beta.5.4))
pred_other<-cbind(
  #pr(crem)
  exp(mean(multi_out$beta.3.1) + size_dummy_real*mean(multi_out$beta.5.1))/Denominator_other,
  #pr(liom)
  exp(mean(multi_out$beta.3.2) + size_dummy_real*mean(multi_out$beta.5.2))/Denominator_other,
  #pr(other)
  exp(mean(multi_out$beta.3.3) + size_dummy_real*mean(multi_out$beta.5.3))/Denominator_other,
  #pr(vac)
  exp(mean(multi_out$beta.3.4) + size_dummy_real*mean(multi_out$beta.5.4))/Denominator_other)
sum(pred_other[1,])

## Previously tended by vac
Denominator_vac <- exp(mean(multi_out$beta.4.1) + size_dummy_real*mean(multi_out$beta.5.1)) + 
  exp(mean(multi_out$beta.4.2) + size_dummy_real*mean(multi_out$beta.5.2)) + 
  exp(mean(multi_out$beta.4.3) + size_dummy_real*mean(multi_out$beta.5.3)) + 
  exp(mean(multi_out$beta.4.4) + size_dummy_real*mean(multi_out$beta.5.4))
pred_vac<-cbind(
  #pr(crem)
  exp(mean(multi_out$beta.4.1) + size_dummy_real*mean(multi_out$beta.5.1))/Denominator_vac,
  #pr(liom)
  exp(mean(multi_out$beta.4.2) + size_dummy_real*mean(multi_out$beta.5.2))/Denominator_vac,
  #pr(other)
  exp(mean(multi_out$beta.4.3) + size_dummy_real*mean(multi_out$beta.5.3))/Denominator_vac,
  #pr(vac)
  exp(mean(multi_out$beta.4.4) + size_dummy_real*mean(multi_out$beta.5.4))/Denominator_vac)
sum(pred_vac[1,])
## vac -> crem       vac -> liom    vac -> other       vac -> vac
pred_probs_vac <- cbind((pred_vac[,1]) , (pred_vac[,2]) , (pred_vac[,3]) , (pred_vac[,4]))
## other-> crem        other -> liom    other -> other       other -> vac
pred_probs_other <- cbind((pred_other[,1]) , (pred_other[,2]) , (pred_other[,3]) , (pred_other[,4]))
## crem-> crem       crem -> liom    crem -> other      crem -> vac
pred_probs_crem <- cbind((pred_crem[,1]) , (pred_crem[,2]) , (pred_crem[,3]) , (pred_crem[,4]))
## liom-> crem       liom -> liom    liom -> other       liom -> vac
pred_probs_liom <- cbind((pred_liom[,1]) , (pred_liom[,2]) , (pred_liom[,3]) , (pred_liom[,4]))

all_ant_multi <- cbind(pred_probs_vac, pred_probs_crem, pred_probs_other, pred_probs_liom)
colnames(all_ant_multi) <- c("vaccrem","vacliom","vacother","vacvac",
                             "cremcrem","cremliom","cremother","cremvac",
                             "othercrem","otherliom","otherother","othervac",
                             "liomcrem","liomliom","liomother","liomvac")
write.csv(all_ant_multi,"all_ant_multi.csv")


###### Check the significance of the differences between germination rates
## create data set where each column is an estimated survival rate
multi_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/multi.params.csv", header = TRUE,stringsAsFactors=T)
vc_est <- all_ant_multi[,1]
vl_est <- all_ant_multi[,2]
vo_est <- all_ant_multi[,3]
vv_est <- all_ant_multi[,4]
cc_est <- all_ant_multi[,5]
cl_est <- all_ant_multi[,6]
co_est <- all_ant_multi[,7]
cv_est <- all_ant_multi[,8]
oc_est <- all_ant_multi[,9]
ol_est <- all_ant_multi[,10]
oo_est <- all_ant_multi[,11]
ov_est <- all_ant_multi[,12]
lc_est <- all_ant_multi[,13]
ll_est <- all_ant_multi[,14]
lo_est <- all_ant_multi[,15]
lv_est <- all_ant_multi[,16]

estimates <- cbind(vv_est, vc_est, vo_est, vl_est,
                   cv_est, cc_est, co_est, cl_est,
                   ov_est, oc_est, oo_est, ol_est, 
                   lv_est, lc_est, lo_est, ll_est)

#### previously vacant probabilities ########################
## vv and vc -- p = 2.2e-16.   *** >
t.test(estimates[,1],estimates[,2], alternative = "two.sided")
## vv and vo -- p = 2.2e-16    *** >
t.test(estimates[,1],estimates[,3], alternative = "two.sided")
## vv and vl -- p = 2.2e-16.   *** >
t.test(estimates[,1],estimates[,4], alternative = "two.sided")
## vc and vo -- p = 0.00227.   *** >
t.test(estimates[,2],estimates[,3], alternative = "two.sided")
## vc and vl -- p = 3.828e-14. *** <
t.test(estimates[,2],estimates[,4], alternative = "two.sided")
## vo and vl -- p = 2.2e-16.   *** <
t.test(estimates[,3],estimates[,4], alternative = "two.sided")

#### previously crem probabilities ############################
## cv and cc -- p = 2.2e-16.   *** >
t.test(estimates[,5],estimates[,6], alternative = "two.sided")
## cv and co -- p = 2.2e-16.   *** >
t.test(estimates[,5],estimates[,7], alternative = "two.sided")
## cv and cl -- p = 2.2e-16.   *** >
t.test(estimates[,5],estimates[,8], alternative = "two.sided")
## cc and co -- p = 3.797e-9.  *** >
t.test(estimates[,6],estimates[,7], alternative = "two.sided")
## cc and cl -- p = 2.2e-16.   *** >
t.test(estimates[,6],estimates[,8], alternative = "two.sided")
## co and cl -- p = 3.797e-9.  *** <
t.test(estimates[,7],estimates[,8], alternative = "two.sided")

#### previously other probabilities ###########################
## ov and oc -- p = 2.2e-16   *** >
t.test(estimates[,9],estimates[,10], alternative = "two.sided")
## ov and oo -- p = 2.2e-16.  *** >
t.test(estimates[,9],estimates[,11], alternative = "two.sided")
## ov and ol -- p = 2.2e-16.  *** >
t.test(estimates[,9],estimates[,12], alternative = "two.sided")
## oc and oo -- p = 0.954.        <
t.test(estimates[,10],estimates[,11], alternative = "two.sided")
## oc and ol -- p = 6.256e-13 *** <
t.test(estimates[,10],estimates[,12], alternative = "two.sided")
## oo and ol -- p = 6.309e-12 *** <
t.test(estimates[,11],estimates[,12], alternative = "two.sided")

#### previously liom probabilities ############################
## lv and lc -- p = 2.2e-16.  *** >
t.test(estimates[,13],estimates[,14], alternative = "two.sided")
## lv and lo -- p = 2.2e-16   *** >
t.test(estimates[,13],estimates[,15], alternative = "two.sided")
## lv and ll -- p = 2.2e-16.  *** >
t.test(estimates[,13],estimates[,16], alternative = "two.sided")
## lc and lo -- p = 0.007925  *** >
t.test(estimates[,14],estimates[,15], alternative = "two.sided")
## lc and ll -- p = 2.2e-16.  *** <
t.test(estimates[,14],estimates[,16], alternative = "two.sided")
## lo and ll -- p = 2.2e-16.  *** <
t.test(estimates[,15],estimates[,16], alternative = "two.sided")


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
      plot(size_dummy_real, pred_vac[,1], type = "l", col = cremcol,main = "Previously Vacant", ylim = c(0,1), xlab = "", ylab = "",
           cex.main = 2)
      lines(size_dummy_real, pred_vac[,2], col = liomcol)
      lines(size_dummy_real, pred_vac[,3], col = othercol)
      lines(size_dummy_real, pred_vac[,4], col = vaccol)
      points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_crem,pch=16,cex=multi_plot_vac$N_mod,col= alpha(cremcol, 0.4))
      points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_liom,pch=16,cex=multi_plot_vac$N_mod,col= alpha(liomcol, 0.4))
      points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_other,pch=16,cex=multi_plot_vac$N_mod,col= alpha(othercol, 0.4))
      points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_vac,pch=16,cex=multi_plot_vac$N_mod,col= alpha(vaccol, 0.4))
      ## Prev Other
      plot(size_dummy_real, pred_other[,1], type = "l", col = cremcol,main = "Previously Other", ylim = c(0,1), xlab = "", ylab = "",
           cex.main = 2)
      lines(size_dummy_real, pred_other[,2], col = liomcol)
      lines(size_dummy_real, pred_other[,3], col = othercol)
      lines(size_dummy_real, pred_other[,4], col = vaccol)
      points(multi_plot_other$mean_size,multi_plot_other$ant_t1_crem,pch=16,cex=multi_plot_other$N_mod,col= alpha(cremcol, 0.4))
      points(multi_plot_other$mean_size,multi_plot_other$ant_t1_liom,pch=16,cex=multi_plot_other$N_mod,col= alpha(liomcol, 0.4))
      points(multi_plot_other$mean_size,multi_plot_other$ant_t1_other,pch=16,cex=multi_plot_other$N_mod,col= alpha(othercol, 0.4))
      points(multi_plot_other$mean_size,multi_plot_other$ant_t1_vac,pch=16,cex=multi_plot_other$N_mod,col= alpha(vaccol, 0.4))
      legend("topright",c("vacant","other","crem.","liom."), fill = c(vaccol,othercol,cremcol,liomcol), cex = 2)
      ## Prev Crem
      plot(size_dummy_real, pred_crem[,1], type = "l", col = cremcol,main = "Previously Crem", ylim = c(0,1), xlab = "", ylab = "",
           cex.main = 2)
      lines(size_dummy_real, pred_crem[,2], col = liomcol)
      lines(size_dummy_real, pred_crem[,3], col = othercol)
      lines(size_dummy_real, pred_crem[,4], col = vaccol)
      points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_crem,pch=16,cex=multi_plot_crem$N_mod,col= alpha(cremcol, 0.4))
      points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_liom,pch=16,cex=multi_plot_crem$N_mod,col= alpha(liomcol, 0.4))
      points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_other,pch=16,cex=multi_plot_crem$N_mod,col= alpha(othercol, 0.4))
      points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_vac,pch=16,cex=multi_plot_crem$N_mod,col= alpha(vaccol, 0.4))
      ## Prev Liom
      plot(size_dummy_real, pred_liom[,1], type = "l", col = cremcol,main = "Previously Liom", ylim = c(0,1), xlab = "", ylab = "",
           cex.main = 2)
      lines(size_dummy_real, pred_liom[,2], col = liomcol)
      lines(size_dummy_real, pred_liom[,3], col = othercol)
      lines(size_dummy_real, pred_liom[,4], col = vaccol)
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
      plot(size_dummy_real, pred_vac[,4], type = "l", col = vaccol,main = "a)              Prev. Vacant               ", ylim = c(0,1), xlab = "", ylab = "",
           cex.main = 1.5)
      lines(size_dummy_real, pred_vac[,3], col = othercol)
      lines(size_dummy_real, pred_vac[,1], col = cremcol)
      lines(size_dummy_real, pred_vac[,2], col = liomcol)
      points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_crem,pch=16,cex=multi_plot_vac$N_mod,col= alpha(cremcol, 0.4))
      points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_liom,pch=16,cex=multi_plot_vac$N_mod,col= alpha(liomcol, 0.4))
      points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_other,pch=16,cex=multi_plot_vac$N_mod,col= alpha(othercol, 0.4))
      points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_vac,pch=16,cex=multi_plot_vac$N_mod,col= alpha(vaccol, 0.4))
      ## Prev Other
      plot(size_dummy_real, pred_other[,4], type = "l", col = vaccol,main = "b)              Prev. Other                 ", ylim = c(0,1), xlab = "", ylab = "",
           cex.main = 1.5)
      lines(size_dummy_real, pred_other[,3], col = othercol)
      lines(size_dummy_real, pred_other[,1], col = cremcol)
      lines(size_dummy_real, pred_other[,2], col = liomcol)
      points(multi_plot_other$mean_size,multi_plot_other$ant_t1_crem,pch=16,cex=multi_plot_other$N_mod,col= alpha(cremcol, 0.4))
      points(multi_plot_other$mean_size,multi_plot_other$ant_t1_liom,pch=16,cex=multi_plot_other$N_mod,col= alpha(liomcol, 0.4))
      points(multi_plot_other$mean_size,multi_plot_other$ant_t1_other,pch=16,cex=multi_plot_other$N_mod,col= alpha(othercol, 0.4))
      points(multi_plot_other$mean_size,multi_plot_other$ant_t1_vac,pch=16,cex=multi_plot_other$N_mod,col= alpha(vaccol, 0.4))
      legend("topleft",c("vacant","other","crem.","liom."), fill = c(vaccol,othercol,cremcol,liomcol), cex = 1.5)
      ## Prev Crem
      plot(size_dummy_real, pred_crem[,4], type = "l", col = vaccol,main = "c)              Prev. Crem.                ", ylim = c(0,1), xlab = "", ylab = "",
           cex.main = 1.5)
      lines(size_dummy_real, pred_crem[,3], col = othercol)
      lines(size_dummy_real, pred_crem[,1], col = cremcol)
      lines(size_dummy_real, pred_crem[,2], col = liomcol)
      points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_crem,pch=16,cex=multi_plot_crem$N_mod,col= alpha(cremcol, 0.4))
      points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_liom,pch=16,cex=multi_plot_crem$N_mod,col= alpha(liomcol, 0.4))
      points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_other,pch=16,cex=multi_plot_crem$N_mod,col= alpha(othercol, 0.4))
      points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_vac,pch=16,cex=multi_plot_crem$N_mod,col= alpha(vaccol, 0.4))
      ## Prev Liom
      plot(size_dummy_real, pred_liom[,4], type = "l", col = vaccol,main = "d)              Prev. Liom.                 ", ylim = c(0,1), xlab = "", ylab = "",
           cex.main = 1.5)
      lines(size_dummy_real, pred_liom[,3], col = othercol)
      lines(size_dummy_real, pred_liom[,1], col = cremcol)
      lines(size_dummy_real, pred_liom[,2], col = liomcol)
      points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_crem,pch=16,cex=multi_plot_liom$N_mod,col= alpha(cremcol, 0.4))
      points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_liom,pch=16,cex=multi_plot_liom$N_mod,col= alpha(liomcol, 0.4))
      points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_other,pch=16,cex=multi_plot_liom$N_mod,col= alpha(othercol, 0.4))
      points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_vac,pch=16,cex=multi_plot_liom$N_mod,col= alpha(vaccol, 0.4))
      mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.5)
      mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.5,las=0)
      dev.off()
      setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
      
      #######################################################################################################################
      #### 3 ANTS TRANSITION PROBABILITIES ##################################################################################
      #######################################################################################################################
      
      ##################################### LIOM & VAC & OTHER ###############################################################
      ## Calculate the probabilities of being tended by each ant species
      ## liom = 2, other = 3, vac = 4
      ## Previously tended by vac
      Denominator_vac <- exp(mean(multi_out$beta.4.4) + size_dummy_real*mean(multi_out$beta.5.4)) + 
        exp(mean(multi_out$beta.4.2) + size_dummy_real*mean(multi_out$beta.5.2)) + 
        exp(mean(multi_out$beta.4.3) + size_dummy_real*mean(multi_out$beta.5.3))
      pred_vac<-cbind(
        #pr(vacant)
        exp(mean(multi_out$beta.4.4) + size_dummy_real*mean(multi_out$beta.5.4))/Denominator_vac,
        #pr(liom)
        exp(mean(multi_out$beta.4.2) + size_dummy_real*mean(multi_out$beta.5.2))/Denominator_vac,
        #pr(other)
        exp(mean(multi_out$beta.4.3) + size_dummy_real*mean(multi_out$beta.5.3))/Denominator_vac)
      sum(pred_vac[1,])
      
      ## Previously tended by Liom
      Denominator_liom <- exp(mean(multi_out$beta.2.4) + size_dummy_real*mean(multi_out$beta.2.4)) + 
        exp(mean(multi_out$beta.2.2) + size_dummy_real*mean(multi_out$beta.5.2)) + 
        exp(mean(multi_out$beta.2.3) + size_dummy_real*mean(multi_out$beta.5.3))
      pred_liom<-cbind(
        #pr(vacant)
        exp(mean(multi_out$beta.2.4) + size_dummy_real*mean(multi_out$beta.5.4))/Denominator_liom,
        #pr(liom)
        exp(mean(multi_out$beta.2.2) + size_dummy_real*mean(multi_out$beta.5.2))/Denominator_liom,
        #pr(other)
        exp(mean(multi_out$beta.2.3) + size_dummy_real*mean(multi_out$beta.5.3))/Denominator_liom)
      sum(pred_liom[1,])
      
      ## Previously tended by Other
      Denominator_other <- exp(mean(multi_out$beta.3.4) + size_dummy_real*mean(multi_out$beta.5.4)) + 
        exp(mean(multi_out$beta.3.2) + size_dummy_real*mean(multi_out$beta.5.2)) + 
        exp(mean(multi_out$beta.3.3) + size_dummy_real*mean(multi_out$beta.5.3))
      pred_other<-cbind(
        #pr(vacant)
        exp(mean(multi_out$beta.3.4) + size_dummy_real*mean(multi_out$beta.5.4))/Denominator_other,
        #pr(liom
        exp(mean(multi_out$beta.3.2) + size_dummy_real*mean(multi_out$beta.5.2))/Denominator_other,
        #pr(other)
        exp(mean(multi_out$beta.3.3) + size_dummy_real*mean(multi_out$beta.5.3))/Denominator_other)
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
      Denominator_other <- exp(mean(multi_out$beta.3.3) + size_dummy_real*mean(multi_out$beta.5.3)) + 
        exp(mean(multi_out$beta.3.1) + size_dummy_real*mean(multi_out$beta.5.1)) + 
        exp(mean(multi_out$beta.3.2) + size_dummy_real*mean(multi_out$beta.5.2))
      pred_other<-cbind(
        #pr(other)
        exp((mean(multi_out$beta.3.3)) + size_dummy_real*mean(multi_out$beta.5.3))/Denominator_other,
        #pr(crem)
        exp((mean(multi_out$beta.3.1)) + size_dummy_real*mean(multi_out$beta.5.1))/Denominator_other,
        #pr(liom)
        exp((mean(multi_out$beta.3.2)) + size_dummy_real*mean(multi_out$beta.5.2))/Denominator_other)
      sum(pred_other[1,])
      
      ## Previously tended by Crem
      Denominator_crem <- exp(mean(multi_out$beta.1.1) + size_dummy_real*mean(multi_out$beta.5.1)) + 
        exp(mean(multi_out$beta.1.3) + size_dummy_real*mean(multi_out$beta.5.3)) + 
        exp(mean(multi_out$beta.1.2) + size_dummy_real*mean(multi_out$beta.5.2))
      pred_crem<-cbind(
        #pr(crem)
        exp(mean(multi_out$beta.1.1) + size_dummy_real*mean(multi_out$beta.5.1))/Denominator_crem,
        #pr(other)
        exp(mean(multi_out$beta.1.3) + size_dummy_real*mean(multi_out$beta.5.3))/Denominator_crem,
        #pr(liom)
        exp(mean(multi_out$beta.1.2) + size_dummy_real*mean(multi_out$beta.5.2))/Denominator_crem)
      sum(pred_crem[1,])
      
      ## Previously tended by Liom
      Denominator_liom <- exp(mean(multi_out$beta.2.3) + size_dummy_real*mean(multi_out$beta.5.3)) + 
        exp(mean(multi_out$beta.2.1) + size_dummy_real*mean(multi_out$beta.5.3)) + 
        exp(mean(multi_out$beta.2.2) + size_dummy_real*mean(multi_out$beta.5.2))
      pred_liom<-cbind(
        #pr(other)
        exp(mean(multi_out$beta.2.3) + size_dummy_real*mean(multi_out$beta.5.3))/Denominator_liom,
        #pr(crem)
        exp(mean(multi_out$beta.2.2) + size_dummy_real*mean(multi_out$beta.5.2))/Denominator_liom,
        #pr(liom)
        exp(mean(multi_out$beta.2.1) + size_dummy_real*mean(multi_out$beta.5.1))/Denominator_liom)
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
      Denominator_vac <- exp(mean(multi_out$beta.4.4) + size_dummy_real*mean(multi_out$beta.5.4)) + 
        exp(mean(multi_out$beta.4.1) + size_dummy_real*mean(multi_out$beta.5.1)) + 
        exp(mean(multi_out$beta.4.2) + size_dummy_real*mean(multi_out$beta.5.2))
      pred_vac<-cbind(
        #pr(vacant)
        exp(mean(multi_out$beta.4.4) + size_dummy_real*mean(multi_out$beta.5.4))/Denominator_vac,
        #pr(crem)
        exp(mean(multi_out$beta.4.1) + size_dummy_real*mean(multi_out$beta.5.1))/Denominator_vac,
        #pr(liom)
        exp(mean(multi_out$beta.4.2) + size_dummy_real*mean(multi_out$beta.5.2))/Denominator_vac)
      sum(pred_vac[1,])
      
      ## Previously tended by Crem
      Denominator_crem <- exp(mean(multi_out$beta.1.4) + size_dummy_real*mean(multi_out$beta.5.4)) + 
        exp(mean(multi_out$beta.1.1) + size_dummy_real*mean(multi_out$beta.5.1)) + 
        exp(mean(multi_out$beta.1.2) + size_dummy_real*mean(multi_out$beta.5.2))
      pred_crem<-cbind(
        #pr(vacant)
        exp(mean(multi_out$beta.1.4) + size_dummy_real*mean(multi_out$beta.5.4))/Denominator_crem,
        #pr(crem)
        exp(mean(multi_out$beta.1.1) + size_dummy_real*mean(multi_out$beta.5.1))/Denominator_crem,
        #pr(liom)
        exp(mean(multi_out$beta.1.2) + size_dummy_real*mean(multi_out$beta.5.2))/Denominator_crem)
      sum(pred_crem[1,])
      
      ## Previously tended by Liom
      Denominator_liom <- exp(mean(multi_out$beta.2.4) + size_dummy_real*mean(multi_out$beta.5.4)) + 
        exp(mean(multi_out$beta.2.1) + size_dummy_real*mean(multi_out$beta.5.1)) + 
        exp(mean(multi_out$beta.2.2) + size_dummy_real*mean(multi_out$beta.5.2))
      pred_liom<-cbind(
        #pr(vacant)
        exp(mean(multi_out$beta.2.4) + size_dummy_real*mean(multi_out$beta.5.4))/Denominator_liom,
        #pr(crem)
        exp(mean(multi_out$beta.2.1) + size_dummy_real*mean(multi_out$beta.5.1))/Denominator_liom,
        #pr(liom)
        exp(mean(multi_out$beta.2.2) + size_dummy_real*mean(multi_out$beta.5.2))/Denominator_liom)
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
      Denominator_vac <- exp(mean(multi_out$beta.4.4) + size_dummy_real*mean(multi_out$beta.5.4)) + 
        exp(mean(multi_out$beta.4.3) + size_dummy_real*mean(multi_out$beta.5.3)) + 
        exp(mean(multi_out$beta.4.1) + size_dummy_real*mean(multi_out$beta.5.1))
      pred_vac<-cbind(
        #pr(vacant)
        exp(mean(multi_out$beta.4.4) + size_dummy_real*mean(multi_out$beta.5.4))/Denominator_vac,
        #pr(other)
        exp(mean(multi_out$beta.4.3) + size_dummy_real*mean(multi_out$beta.5.3))/Denominator_vac,
        #pr(crem)
        exp(mean(multi_out$beta.4.1) + size_dummy_real*mean(multi_out$beta.5.1))/Denominator_vac)
      sum(pred_vac[1,])
      
      ## Previously tended by Crem
      Denominator_crem <- exp(mean(multi_out$beta.1.4) + size_dummy_real*mean(multi_out$beta.5.4)) + 
        exp(mean(multi_out$beta.1.3) + size_dummy_real*mean(multi_out$beta.5.3)) + 
        exp(mean(multi_out$beta.1.1) + size_dummy_real*mean(multi_out$beta.5.1))
      pred_crem<-cbind(
        #pr(vacant)
        exp((mean(multi_out$beta.1.4)) + size_dummy_real*mean(multi_out$beta.5.4))/Denominator_crem,
        #pr(other)
        exp((mean(multi_out$beta.1.3)) + size_dummy_real*mean(multi_out$beta.5.3))/Denominator_crem,
        #pr(crem)
        exp((mean(multi_out$beta.1.1)) + size_dummy_real*mean(multi_out$beta.5.1))/Denominator_crem)
      sum(pred_crem[1,])
      
      ## Previously tended by Crem
      Denominator_other <- exp(mean(multi_out$beta.1.4) + size_dummy_real*mean(multi_out$beta.5.4)) + 
        exp(mean(multi_out$beta.1.3) + size_dummy_real*mean(multi_out$beta.5.3)) + 
        exp(mean(multi_out$beta.1.1) + size_dummy_real*mean(multi_out$beta.5.1))
      pred_other<-cbind(
        #pr(vacant)
        exp(mean(multi_out$beta.1.4) + size_dummy_real*mean(multi_out$beta.5.4))/Denominator_other,
        #pr(other)
        exp(mean(multi_out$beta.1.3) + size_dummy_real*mean(multi_out$beta.5.3))/Denominator_other,
        #pr(crem)
        exp(mean(multi_out$beta.1.1) + size_dummy_real*mean(multi_out$beta.5.1))/Denominator_other)
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
      Denominator_other <- exp(mean(multi_out$beta.3.1) + size_dummy_real*mean(multi_out$beta.5.1)) + 
        exp(mean(multi_out$beta.3.3) + size_dummy_real*mean(multi_out$beta.5.3))
      pred_other<-cbind(
        #pr(crem)
        exp((mean(multi_out$beta.3.1)) + size_dummy_real*mean(multi_out$beta.5.1))/Denominator_other,
        #pr(other)
        exp((mean(multi_out$beta.3.3)) + size_dummy_real*mean(multi_out$beta.5.3))/Denominator_other)
      sum(pred_other[1,])
      ## Previously tended by Crem
      Denominator_crem <- exp(mean(multi_out$beta.1.1) + size_dummy_real*mean(multi_out$beta.5.1)) + 
        exp(mean(multi_out$beta.1.3) + size_dummy_real*mean(multi_out$beta.5.3))
      pred_crem<-cbind(
        #pr(crem)
        exp(mean(multi_out$beta.1.1) + size_dummy_real*mean(multi_out$beta.5.1))/Denominator_crem,
        #pr(other)
        exp(mean(multi_out$beta.1.3) + size_dummy_real*mean(multi_out$beta.5.3))/Denominator_crem)
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
      Denominator_liom <- exp(mean(multi_out$beta.2.2) + size_dummy_real*mean(multi_out$beta.5.2)) + 
        exp(mean(multi_out$beta.2.4) + size_dummy_real*mean(multi_out$beta.5.4))
      pred_liom<-cbind(
        #pr(liom)
        exp((mean(multi_out$beta.2.2)) + size_dummy_real*mean(multi_out$beta.5.2))/Denominator_liom,
        #pr(vac)
        exp((mean(multi_out$beta.2.4)) + size_dummy_real*mean(multi_out$beta.5.4))/Denominator_liom)
      sum(pred_liom[1,])
      ## Previously tended by none
      Denominator_vac <- exp(mean(multi_out$beta.4.2) + size_dummy_real*mean(multi_out$beta.5.2)) + 
        exp(mean(multi_out$beta.4.4) + size_dummy_real*mean(multi_out$beta.5.4))
      pred_vac<-cbind(
        #pr(liom)
        exp(mean(multi_out$beta.4.2) + size_dummy_real*mean(multi_out$beta.5.2))/Denominator_vac,
        #pr(vac)
        exp(mean(multi_out$beta.4.4) + size_dummy_real*mean(multi_out$beta.5.4))/Denominator_vac)
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
      Denominator_liom <- exp(mean(multi_out$beta.2.2) + size_dummy_real*mean(multi_out$beta.5.2)) + 
        exp(mean(multi_out$beta.2.1) + size_dummy_real*mean(multi_out$beta.5.1))
      pred_liom<-cbind(
        #pr(liom)
        exp((mean(multi_out$beta.2.2)) + size_dummy_real*mean(multi_out$beta.5.2))/Denominator_liom,
        #pr(crem)
        exp((mean(multi_out$beta.2.1)) + size_dummy_real*mean(multi_out$beta.5.1))/Denominator_liom)
      sum(pred_liom[1,])
      ## Previously tended by none
      Denominator_crem <- exp(mean(multi_out$beta.1.2) + size_dummy_real*mean(multi_out$beta.5.2)) + 
        exp(mean(multi_out$beta.1.1) + size_dummy_real*mean(multi_out$beta.5.1))
      pred_crem<-cbind(
        #pr(liom)
        exp(mean(multi_out$beta.1.2) + size_dummy_real*mean(multi_out$beta.5.2))/Denominator_crem,
        #pr(crem)
        exp(mean(multi_out$beta.1.1) + size_dummy_real*mean(multi_out$beta.5.1))/Denominator_crem)
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
      Denominator_other <- exp(mean(multi_out$beta.3.3) + size_dummy_real*mean(multi_out$beta.5.3)) + 
        exp(mean(multi_out$beta.3.4) + size_dummy_real*mean(multi_out$beta.5.4))
      pred_other<-cbind(
        #pr(other)
        exp((mean(multi_out$beta.3.3)) + size_dummy_real*mean(multi_out$beta.5.3))/Denominator_other,
        #pr(vac)
        exp((mean(multi_out$beta.3.4)) + size_dummy_real*mean(multi_out$beta.5.4))/Denominator_other)
      sum(pred_other[1,])
      ## Previously tended by none
      Denominator_vac <- exp(mean(multi_out$beta.4.3) + size_dummy_real*mean(multi_out$beta.5.3)) + 
        exp(mean(multi_out$beta.4.4) + size_dummy_real*mean(multi_out$beta.5.4))
      pred_vac<-cbind(
        #pr(other)
        exp(mean(multi_out$beta.4.3) + size_dummy_real*mean(multi_out$beta.5.3))/Denominator_vac,
        #pr(vac)
        exp(mean(multi_out$beta.4.4) + size_dummy_real*mean(multi_out$beta.5.4))/Denominator_vac)
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
      
      

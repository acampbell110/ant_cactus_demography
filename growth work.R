setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

################################################################################
##### Null model -- no predictors ##############################################
################################################################################
##### Simulate growth data with a skewed distribution
vol <- rnorm(n = 1000, mean = 0, sd = 3)
a <- rsn(n = 1000,xi = (3 + 5*vol), omega = 15, alpha = 20)
##### Now load the data s.t. stan may interpret it
stan_data <- list(y = a,              ## the response variable
                  N = length(a),      ## the number of observations
                  vol = vol           ## size data
                  )
stan_null_model <- stan(file = "Data Analysis/STAN Models/stan_prac.stan", 
                   data = stan_data, warmup = 150, iter = 1000, chains = 3, 
                   cores = 3, thin = 1)
##### export the required variables so they may be used
## export
summary(stan_null_model)
stan_mu <- rstan::extract(stan_null_model, pars = c("beta0"))$beta0
stan_outputs <- rstan::extract(stan_null_model, pars = c("alpha","sigma","beta0","beta1"))
write.csv(stan_outputs, "stan_outputs.csv")
write.csv(stan_mu, "stan_mu.csv")
y_rep <- rstan::extract(stan_null_model, pars = "y_rep")[["y_rep"]]
samp100 <- sample(nrow(y_rep), 100)
## format
others <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/stan_outputs.csv", header = TRUE,stringsAsFactors=T)
others <- others[,c(-1)]
others <- as.matrix(others)
y_sim <- matrix(NA, 1000,length(a))
for(i in 1:1000){
  y_sim[i,] <- rsn(n=length(a), xi = (others[i,"beta0"] + others[i,"beta1"] * vol), 
                   omega = others[i,"sigma"], 
                   alpha = others[i,"alpha"], dp = NULL)
}
##### visualize the outcomes of the model
## the convergence of the parameters
png("null_convergence_try4.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(stan_null_model, pars = c("beta0","beta1","sigma","alpha")) 
dev.off()
## converges fine
## the ability of the model to match the data
png("null_overlay_ysim_try4.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay(as.vector(a), y_sim)
dev.off()
## seems pretty good
##### check the estimates of each of the parameters
mean(others[,"beta1"])
## try one
## input xi is 20
## pretty close (19.773)
## try two
## input xi is 200
## pretty close (200.0814)
## try three
## input xi is 200
## pretty close (199.9757)
## try four
## input xi is 3 + 5*vol
## pretty close (2.886862 + 5.017264 * vol)
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
## try three
## input sigma is 15
## pretty close (14.57359)
mean(others[,"alpha"])
## try one
## input alpha is 2
## pretty close (2.058173)
## try two 
## input alpha is 200
## not that great (114.0239)
## try three 
## input alpha is 20
## pretty good (19.0722)
## try four 
## input alpha is 20
## not that great (18.5701)

#########################################################################
###### Now do it with real data #########################################
#########################################################################
growth_data_orig <- cactus[,c("Plot","Year_t","logsize_t","logsize_t1","ant_t")]
growth_data <- na.omit(growth_data_orig)
stan_data <- list(y = (growth_data$logsize_t1),                            ## the response variable
                  N = nrow(growth_data),                                   ## the number of observations
                  vol = (growth_data$logsize_t)-mean(growth_data$logsize_t),                           ## predictors volume
                  ant = as.integer(as.factor(growth_data$ant_t)),          ## predictors ants
                  K = 4,                                                   ## number of ant states
                  N_Year = max(as.integer(as.factor(growth_data$Year_t))), ## number of years
                  N_Plot = max(as.integer(as.factor(growth_data$Plot))),   ## number of plots
                  plot = as.integer(as.factor(growth_data$Plot)),          ## predictor plots
                  year = as.integer(as.factor(growth_data$Year_t))         ## predictor years
)
stan_model <- stan(file = "Data Analysis/STAN Models/grow_skew.stan", 
                   data = stan_data, warmup = 150, iter = 1000, chains = 3, 
                   cores = 3, thin = 1)

summary(stan_model)
stan_outputs <- rstan::extract(stan_model, pars = c("beta0","beta1","u","w","d_0","d_size","a_0","a_size"))
stan_yrep <- rstan::extract(stan_model, pars = c("y_rep"))$y_rep
write.csv(stan_outputs, "stan_outputs.csv")
write.csv(stan_yrep, "stan_yrep.csv")
## format
y_rep <- rstan::extract(stan_model, pars = "y_rep")[["y_rep"]]
samp100 <- sample(nrow(y_rep), 100)
##### visualize the outcomes of the model
## the convergence of the parameters
png("convergence_data7.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(stan_model, pars = c("a_0","a_size","d_0","d_size")) 
dev.off()
## converges fine
## the ability of the model to match the data
png("overlay_data7.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay(as.vector(y), y_rep[,])
dev.off()
## Overlay separated by ant partner
png("overlay_group_data7.png")
bayesplot::ppc_dens_overlay_grouped(y, y_rep[samp100,],group = ant)
dev.off()
## Moments plot with mean sd skew and kurtosis
png("moments_data7.png")
size_moments_ppc(growth_data, 
                 "logsize_t1", 
                 y_rep[samp100,], 
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
  sim_moments <- bind_cols(enframe(growth_data$logsize_t), as_tibble(t(y_rep))) %>%
    rename(logsize_t = value) %>%
    arrange(logsize_t) %>%
    mutate(size_bin = cut_number(logsize_t, 10)) %>%
    pivot_longer(., cols = starts_with("V"), names_to = "post_draw", values_to = "y_rep") %>%
    group_by(size_bin, post_draw) %>%
    summarize( mean_sim = mean((y_rep)),
               sd_sim = sd((y_rep)),
               skew_sim = skewness((y_rep)),
               kurt_sim = Lkurtosis((y_rep)),
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

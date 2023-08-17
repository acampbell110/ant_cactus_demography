n_sim<-1000
x<-runif(n_sim,-4,16)
beta_xi0<-4
beta_xi1<-2.6
beta_omega0<- -0.5
beta_omega1<- 0.2
beta_alpha0<- -5
beta_alpha1<- -2

y<-rsn(n=length(x),
       xi = beta_xi0+beta_xi1*x,
       omega = exp(beta_omega0 +beta_omega1*x),
       alpha = beta_alpha0+beta_alpha1*x)

hist(y)
plot(x,y)
lines(x,beta_xi0+beta_xi1*x,col="red")

stan_data_skew_sim <- list(N = n_sim,  
                            x = x,
                            y = y)
########## growth model with a skew normal distribution -- fixed effects: previous size and ant state, ##############
########## random effects: plot and year, size variation is included for both the omega and alpha estimates #########
# fit_grow_skew <- stan(file = "Data Analysis/STAN Models/grow_skew_toy_sim.stan", data = stan_data_skew_sim, 
#                       warmup = 1000, iter = 5000, chains = 3, cores = 3, thin = 2)
# bayesplot::mcmc_trace(fit_grow_skew,pars=c("beta0","beta1","d_0","d_size","a_0","a_size"))
# 
# sim_params<-rstan::extract(fit_grow_skew,c("beta0","beta1","d_0","d_size","a_0","a_size"))
# samples<-sample.int(100,n=3000,replace=F)
# 
y_sim<-matrix(NA,100,n_sim)
for(i in 1:100){
  y_sim[i,]<-rsn(n=length(x),
         xi = sim_params$beta0[i]+sim_params$beta1[i]*x,
         omega = exp(sim_params$d_0[i] +sim_params$d_size[i]*x),
         alpha = sim_params$a_0[i]+sim_params$a_size[i]*x)
}
n_bins<-10
sim_dat<-data.frame(x=x,y=y)
  require(tidyverse)
  require(patchwork)
  bins <- sim_dat %>%
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
  sim_moments <- bind_cols(enframe(sim_dat$x), as_tibble(t(y_sim))) %>%
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


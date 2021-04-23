setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Data Analysis")
## Fruit Data Set Linear Models

## Proportion of seeds aborted based on the ant state
#view raw proportions ############################################################
fruit1 %>% 
  mutate(viab_rate = number.unblackened/(number.unblackened+number.blackened)) %>% 
  group_by(ant_state) %>% 
  summarise(mean_viab = mean(viab_rate,na.rm=T),
            n_viab = n()) 
fruit1$viab_rate <- fruit1$number.unblackened/(fruit1$number.unblackened+fruit1$number.blackened)

abort_mod1 <- glm(cbind(number.unblackened,number.blackened) ~ 1,family="binomial",data=fruit1)
abort_mod2 <- glm(cbind(number.unblackened,number.blackened) ~ ant_state,family="binomial",data=fruit1) 
abort_mod3 <- glm(cbind(number.unblackened,number.blackened) ~ ant_state + fruit_number,family="binomial",data=fruit1)
abort_mod4 <- glm(cbind(number.unblackened,number.blackened) ~ ant_state * fruit_number,family="binomial",data=fruit1) ## Chosen model
AICtab(abort_mod1,abort_mod2,abort_mod3,abort_mod4)

summary(abort_mod4)

ggplot(data = fruit1) +
  aes(x = fruit_number, y = number.unblackened/(number.unblackened+number.blackened), color = ant_state) + 
  geom_smooth()

## Production of seeds  ############################################################
seeds_mod1 <- glm(seed_count ~ 1, family = "poisson", data = fruit1)
seeds_mod2 <- glm(seed_count ~ ant_state, family = "poisson", data = fruit1)
AICtab(seeds_mod1, seeds_mod2,seeds_mod3,seeds_mod4)

summary(seeds_mod4)

ggplot(data = fruit1) +
  aes(x = fruit_number, y = seed_count, color = ant_state) + 
  geom_smooth()
## IPM Section
seed_prod <- fruit1[,c("seed_count","ant_state","fruit_number")]
seed_prod <- na.omit(seed_prod)

stan_data_seed <- list(N_obs = nrow(seed_prod), # Number of observations
                      seed = seed_prod$seed_count, # Number of seeds
                      ant = seed_prod$ant_state, # Ant state
                      fruit = seed_prod$fruit_number, # Fruit number
                      N_ant = 4
                      )  
fit_seed_ant <- stan(file = "STAN Models/seed_prod.stan", data = stan_data_seed, warmup = 5, iter = 10, chains = 1, cores = 2, thin = 1)
viab_outputs <- rstan::extract(fit_viab_mix_ant, pars = c("beta0","beta1","y_rep"))
write.csv(viab_outputs, "viab_outputs.csv")


write("// Stan model for simple total flowers regression

data {
  int <lower = 1> N_obs; // number of observations
  int <lower = 2> N_ant; // number of observations
  int <lower = 1, upper = N_ant> ant[N_obs]; // the list of ant species 
  int <lower = 0> seed[N_obs]; // survival in year t1
}
parameters {
  real < lower = 0> phi;
  vector[N_ant] beta0; //intercept, unique to ant sp
  real < lower = 0 > sigma; // Error SD
}
transformed parameters{
  vector[N_obs] mu; //linear predictor for the mean
  for(i in 1:N_obs){
   	mu[i] = beta0[ant[i]];
  }
}
model {
  beta0 ~ normal(0,100); // intercept distribution
  for(i in 1:N_obs){
    seed[i] ~ neg_binomial_2(inv_logit(mu[i]), phi);
  }
}
generated quantities {
  int<lower = 0> y_rep[N_flower] = neg_binomial_2_rng(inv_logit(mu), phi);
  real<lower = 0> mean_y_rep = mean(to_vector(y_rep));
  real<lower = 0> sd_y_rep = sd(to_vector(y_rep));
} 

","STAN Models/seed_prod_stan.stan")

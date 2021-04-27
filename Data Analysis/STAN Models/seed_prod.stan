// Stan model for simple total flowers regression

data {
  int <lower = 1> N_obs; // number of observations
  int <lower = 2> N_ant; // number of observations
  int <lower = 1> N_Plant_ID; // number of plant observations
  int <lower = 1, upper = N_ant> ant[N_obs]; // the list of ant species 
  int <lower = 1, upper = N_Plant_ID> plant[N_obs]; // the list of plant IDs
  int <lower = 0> seed[N_obs]; // survival in year t1
}
parameters {
  real < lower = 0> phi;
  real < lower = 0 > sigma_v; // plant ID SD
  vector[N_ant] beta0; //intercept, unique to ant sp
  vector[N_Plant_ID] v; // the random effects of plant ID
  real < lower = 0 > sigma; // Error SD
}
transformed parameters{
  vector[N_obs] mu; //linear predictor for the mean
  for(i in 1:N_obs){
   	mu[i] = beta0[ant[i]] + v[plant[i]];
  }
}
model {
  v ~ normal(0, sigma_v); // plot random effects
  beta0 ~ normal(0,100); // intercept distribution
  for(i in 1:N_obs){
    seed[i] ~ neg_binomial_2(inv_logit(mu[i]), phi);
  }
}
generated quantities {
  int<lower = 0> y_rep[N_obs] = neg_binomial_2_rng(inv_logit(mu), phi);
  real<lower = 0> mean_y_rep = mean(to_vector(y_rep));
  real<lower = 0> sd_y_rep = sd(to_vector(y_rep));
} 



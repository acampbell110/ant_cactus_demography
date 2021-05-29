// Stan model for simple total flowers regression

data {
  int <lower = 1> N_seed; // number of observations
  int <lower = 2> N_ant_seed; // number of observations
  int <lower = 1> N_Plant_ID; // number of plant observations
  int <lower = 1, upper = N_ant_seed> ant_seed[N_seed]; // the list of ant species 
  int <lower = 1, upper = N_Plant_ID> plant_seed[N_seed]; // the list of plant IDs
  int <lower = 0> seed[N_seed]; // survival in year t1
}
parameters {
  real < lower = 0> phi;
  real < lower = 0 > sigma_v; // plant ID SD
  vector[N_ant_seed] beta0; //intercept, unique to ant sp
  vector[N_Plant_ID] v; // the random effects of plant ID
  real < lower = 0 > sigma; // Error SD
}
transformed parameters{
  vector[N_seed] mu; //linear predictor for the mean
  for(i in 1:N_seed){
   	mu[i] = beta0[ant_seed[i]] + v[plant_seed[i]];
  }
}
model {
  v ~ normal(0, sigma_v); // plot random effects
  beta0 ~ normal(0,100); // intercept distribution
  for(i in 1:N_seed){
    seed[i] ~ neg_binomial_2(exp(mu[i]), phi);
  }
}
generated quantities {
  int<lower = 0> y_rep[N_seed] = neg_binomial_2_rng(exp(mu), phi);
  real<lower = 0> mean_y_rep = mean(to_vector(y_rep));
  real<lower = 0> sd_y_rep = sd(to_vector(y_rep));
} 



data {
  int < lower = 1 > N_obs; // Sample size
  int <lower = 0, upper = 1> success[N_obs]; // Ant code 0 = vacant, 1 = crem (therefore a prob will give you the prob of being crem and 1 - prob = prob of being vac)
  int<lower=1> trials[N_obs]; // number of trials
  vector[N_obs] vol_ant;	//size_t
  int <lower = 1> N_ant; // number of ant states
  int <lower = 1, upper = N_ant> prev_ant[N_obs]; // the list of ant species 
}
parameters {
  vector[N_ant] beta0; //intercept, unique to ant sp
  vector[N_ant] beta1; //slope, unique to ant sp
  real < lower = 0 > sigma; // Error SD
}

transformed parameters{
  vector[N_obs] mu; //linear predictor for the mean
  for(i in 1:N_obs){
    mu[i] = beta0[prev_ant[i]] + vol_ant[i] * beta1[prev_ant[i]];
  }
}
model {
//Priors
 beta0 ~ normal(0,100); // intercept distribution
 beta1 ~ normal(0,100); // intercept distribution
 //Model
 for(i in 1:N_obs){
 success[i] ~ bernoulli_logit(mu[i]);
 }
}
generated quantities {
  int<lower = 0> y_rep[N_obs] = bernoulli_logit_rng(mu);
  real<lower = 0> mean_y_rep = mean(to_vector(y_rep));
  real<lower = 0> sd_y_rep = sd(to_vector(y_rep));
}


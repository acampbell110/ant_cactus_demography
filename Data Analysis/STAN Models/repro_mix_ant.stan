// Stan model for simple total flowers regression

data {
  int <lower = 1> N_flower; // number of observations
  int <lower = 1> N_ant; // number of ant states
  int <lower = 1, upper = N_ant> ant_flower[N_flower]; // the list of ant species 
  vector[N_flower] vol1_flower;	//size_t
  int <lower = 0, upper = 1> y_repro[N_flower]; // survival in year t1
  int<lower=1> N_Year; //number of plots
  int<lower=1> N_Plot; //number of years
  int<lower=1, upper=N_Plot> plot_flower[N_flower]; // plot
  int<lower=1, upper=N_Year> year_flower[N_flower]; // year
}
parameters {
  vector[N_ant] beta0; //intercept, unique to ant sp
  vector[N_ant] beta1; //slope, unique to ant sp
  vector[N_Plot] u; //subject intercepts
  vector[N_Year] w; //item intercepts
  real < lower = 0 > sigma; // Error SD
  real < lower = 0 > sigma_u; // plot SD
  real < lower = 0 > sigma_w; // year SD
}
transformed parameters{
  vector[N_flower] mu; //linear predictor for the mean
  for(i in 1:N_flower){
    mu[i] = beta0[ant_flower[i]] + beta1[ant_flower[i]] * vol1_flower[i] + u[plot_flower[i]] + w[year_flower[i]];
  }
}
model {
 // Priors
  u ~ normal(0, sigma_u); // plot random effects
  w ~ normal(0, sigma_w); // year random effects
  beta0 ~ normal(0,100); // intercept distribution
  beta1 ~ normal(0,100); // slope distribution
 // Model
 for(i in 1:N_flower){
  y_repro[i] ~ bernoulli_logit(mu[i]);
 }
}
generated quantities {
  int<lower = 0> y_rep[N_flower] = bernoulli_logit_rng(mu);
  real<lower = 0> mean_y_rep = mean(to_vector(y_rep));
  real<lower = 0> sd_y_rep = sd(to_vector(y_rep));
}


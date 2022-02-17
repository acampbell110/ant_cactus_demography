// Stan model for simple ant survival regression

data {
  int <lower = 1> N_binom; // number of observations
  int <lower = 1> N_ant; // number of ant states
  int <lower = 1, upper = N_ant> ant_binom[N_binom]; // the list of ant species 
  vector[N_binom] vol_binom;	//size_t
  int <lower = 0, upper = 1> y_binom[N_binom]; // survival in year t1
  //int<lower=1> N_Year_binom; //number of plots
  //int<lower=1> N_Plot_binom; //number of years
  // int<lower=1, upper=N_Plot_binom> plot_binom[N_binom]; // plot
  // int<lower=1, upper=N_Year_binom> year_binom[N_binom]; // year
}
parameters {
  vector[N_ant] beta0; //intercept, unique to ant sp
  vector[N_ant] beta1; //slope, unique to ant sp
 // vector[N_Plot_binom] u; //subject intercepts
 // vector[N_Year_binom] w; //item intercepts
 // real < lower = 0 > sigma; // Error SD
 // real < lower = 0 > sigma_u; // plot SD
 // real < lower = 0 > sigma_w; // year SD
}

transformed parameters{
  vector[N_binom] mu; //linear predictor for the mean
  for(i in 1:N_binom){
    mu[i] = beta0[ant_binom[i]] + beta1[ant_binom[i]] * vol_binom[i];
  }
}
model {
//Priors
 // u ~ normal(0, sigma_u); // plot random effects
 // w ~ normal(0, sigma_w); // year random effects
 beta0 ~ normal(0,10); // intercept distribution
 beta1 ~ normal(0,10); // slope distribution
 //Model
 for(i in 1:N_binom){
 y_binom[i] ~ bernoulli_logit(mu[i]);
 }
}


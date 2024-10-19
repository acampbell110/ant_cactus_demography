// Stan model for logit regression of flowering Y/N

data {
  int <lower = 1> N; // number of observations
  vector[N] vol;	//size_t
  int <lower = 0, upper = 1> y_repro[N]; // reproducing in year t
  int<lower=1> N_Year; //number of plots
  int<lower=1> N_Plot; //number of years
  int<lower=1, upper=N_Plot> plot[N]; // plot
  int<lower=1, upper=N_Year> year[N]; // year
}
parameters {
  real beta0; //intercept
  real beta1; //size slope
  vector[N_Plot] u; //plot intercepts
  vector[N_Year] w; //year intercepts
  real < lower = 0 > sigma_u; // plot SD
  real < lower = 0 > sigma_w; // year SD
}
transformed parameters{
  vector[N] mu; //linear predictor for the mean
  for(i in 1:N){
    mu[i] = beta0 + beta1 * vol[i] + u[plot[i]] + w[year[i]];
  }
}
model {
 // Priors
  u ~ normal(0, sigma_u); // plot random effects
  w ~ normal(0, sigma_w); // year random effects
  beta0 ~ normal(0,10); // intercept distribution
  beta1 ~ normal(0,10); // slope distribution
 // Model
 for(i in 1:N){
  y_repro[i] ~ bernoulli_logit(mu[i]);
 }
}


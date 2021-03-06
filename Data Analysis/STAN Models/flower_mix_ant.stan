// Stan model for simple total flowers regression

data {
 int <lower = 1> N_flower; // number of observations
  int <lower = 1> N_ant; // number of ant states
  int <lower = 1, upper = N_ant> ant_flower[N_flower]; // the list of ant species 
  vector[N_flower] vol_flower;	//size_t
  int <lower = 0> y_flow[N_flower]; // survival in year t1
  int<lower=1> N_Year; //number of plots
  int<lower=1> N_Plot; //number of years
  int<lower=1, upper=N_Plot> plot_flower[N_flower]; // plot
  int<lower=1, upper=N_Year> year_flower[N_flower]; // year
}
parameters {
	real < lower = 0> phi;
  real beta0; //intercept
  real beta1; //slope
  vector[N_Plot] u; //subject intercepts
  vector[N_Year] w; //item intercepts
  real < lower = 0 > sigma; // Error SD
  real < lower = 0 > sigma_u; // plot SD
  real < lower = 0 > sigma_w; // year SD
}
transformed parameters{
  vector[N_flower] mu; //linear predictor for the mean
  for(i in 1:N_flower){
    mu[i] = beta0 + beta1 * vol_flower[i] + u[plot_flower[i]] + w[year_flower[i]];
  }
}
model {
	phi ~ cauchy(0,3);
 // Model
  u ~ normal(0, sigma_u); // plot random effects
  w ~ normal(0, sigma_w); // year random effects
  beta0 ~ normal(0,100); // intercept distribution
  beta1 ~ normal(0,100); // slope distribution
  for(i in 1:N_flower){
    y_flow[i] ~ neg_binomial_2(mu[i], phi);
  }
}
generated quantities {
  int<lower = 0> y_rep[N_flower] = neg_binomial_2_rng(mu, phi);
  real<lower = 0> mean_y_rep = mean(to_vector(y_rep));
  real<lower = 0> sd_y_rep = sd(to_vector(y_rep));
} 


// Stan model for simple total flowers regression

data {
  int <lower = 1> N_repro; // number of observations
  vector[N_repro] vol1_repro;	//size_t
  int <lower = 0, upper = 1> y_repro[N_repro]; // survival in year t1
  int<lower=1> N_Year_repro; //number of plots
  int<lower=1> N_Plot_repro; //number of years
  int<lower=1, upper=N_Plot_repro> plot_repro[N_repro]; // plot
  int<lower=1, upper=N_Year_repro> year_repro[N_repro]; // year
}
parameters {
  real beta0; //intercept, unique to ant sp
  real beta1; //slope, unique to ant sp
  vector[N_Plot_repro] u; //subject intercepts
  vector[N_Year_repro] w; //item intercepts
  real < lower = 0 > sigma; // Error SD
  real < lower = 0 > sigma_u; // plot SD
  real < lower = 0 > sigma_w; // year SD
}
transformed parameters{
  vector[N_repro] mu; //linear predictor for the mean
  for(i in 1:N_repro){
    mu[i] = beta0 + beta1 * vol1_repro[i] + u[plot_repro[i]] + w[year_repro[i]];
  }
}
model {
 // Priors
  u ~ normal(0, sigma_u); // plot random effects
  w ~ normal(0, sigma_w); // year random effects
  beta0 ~ normal(0,100); // intercept distribution
  beta1 ~ normal(0,100); // slope distribution
 // Model
 for(i in 1:N_repro){
  y_repro[i] ~ bernoulli_logit(mu[i]);
 }
}
generated quantities {
  int<lower = 0> y_rep[N_repro] = bernoulli_logit_rng(mu);
  real<lower = 0> mean_y_rep = mean(to_vector(y_rep));
  real<lower = 0> sd_y_rep = sd(to_vector(y_rep));
}


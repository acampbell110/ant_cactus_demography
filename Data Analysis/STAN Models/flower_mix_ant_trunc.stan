// Stan model for simple total flowers regression

data {
 int <lower = 1> N_flower; // number of observations
 int lower_limit;
  vector[N_flower] vol_flower;	//size_t
  int <lower = lower_limit> y_flow[N_flower]; // number of flowers produced in year t (natural number)
  int<lower=1> N_Year_flower; //number of plots
  int<lower=1> N_Plot_flower; //number of years
  int<lower=1, upper=N_Plot_flower> plot_flower[N_flower]; // plot
  int<lower=1, upper=N_Year_flower> year_flower[N_flower]; // year
}
parameters {
real < lower = 0> phi;
  real beta0; //intercept
  real beta1; //slope
  vector[N_Plot_flower] u; //subject intercepts
  vector[N_Year_flower] w; //item intercepts
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
  // Model
  u ~ normal(0, sigma_u); // plot random effects
  w ~ normal(0, sigma_w); // year random effects
  beta0 ~ normal(0,100); // intercept distribution
  beta1 ~ normal(0,100); // slope distribution
  for(i in 1:N_flower){
    y_flow[i] ~ neg_binomial_2(exp(mu[i]), phi);
    target += - log1m(neg_binomial_2_log_lpmf(0 | mu[i], phi)); // manually adjusting computation of likelihood
  }
}
generated quantities {
  int<lower = 0> y_rep[N_flower] = neg_binomial_2_rng(inv_logit(mu), phi);
  real<lower = 0> mean_y_rep = mean(to_vector(y_rep));
  real<lower = 0> sd_y_rep = sd(to_vector(y_rep));
} 


// Stan model for simple total flowers regression

data {
 int <lower = 1> N; // number of observations
 int lower_limit;
  vector[N] vol;	//size_t
  int <lower = lower_limit> y_flow[N]; // number of flowers produced in year t (natural number)
  int<lower=1> N_Year; //number of plots
  int<lower=1> N_Plot; //number of years
  int<lower=1, upper=N_Plot> plot[N]; // plot
  int<lower=1, upper=N_Year> year[N]; // year
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
  vector[N] mu; //linear predictor for the mean
  for(i in 1:N){
   	mu[i] = beta0 + beta1 * vol[i] + u[plot[i]] + w[year[i]];
  }
}
model {
  // Model
  u ~ normal(0, sigma_u); // plot random effects
  w ~ normal(0, sigma_w); // year random effects
  beta0 ~ normal(0,100); // intercept distribution
  beta1 ~ normal(0,100); // slope distribution
  for(i in 1:N){
    y_flow[i] ~ neg_binomial_2(exp(mu[i]), phi);
    target += - log1m(neg_binomial_2_log_lpmf(0 | mu[i], phi)); // manually adjusting computation of likelihood
  }
}


data {
    int<lower=2> N_ant; // of alternatives/outcomes
    int<lower=1> N; // of observations
    int <lower=0,upper=N_ant> y[N];
    int <lower = 1, upper = N_ant> ant[N]; // the list of ant species 
    vector[N] vol;
    int<lower=1> N_Year; //number of plots
    int<lower=1> N_Plot; //number of years
    int<lower=1, upper=N_Plot> plot[N]; // plot
    int<lower=1, upper=N_Year> year[N]; // year
}
transformed data {
  real<lower = 0> mean_y = mean(to_vector(y));
  real<lower = 0> sd_y = sd(to_vector(y));
}
parameters {
  vector[N_ant] beta0; //intercept, unique to ant sp
  real beta1; //slope, unique to ant sp
  vector[N_Plot] u; //subject intercepts
  vector[N_Year] w; //item intercepts
  real < lower = 0 > sigma; // Error SD
  real < lower = 0 > sigma_u; // plot SD
  real < lower = 0 > sigma_w; // year SD

}
transformed parameters{
  vector[N] mu; //linear predictor for the mean
  for(i in 1:N){
    mu[i] = beta0[ant[i]] + beta1 * vol[i] + u[plot[i]] + w[year[i]];
  }
}
model {
  // Model
  u ~ normal(0, sigma_u); // plot random effects
  w ~ normal(0, sigma_w); // year random effects
  beta0 ~ normal(0,100); // intercept distribution
  beta1 ~ normal(0,100); // slope distribution
    y ~ categorical_logit(mu);
}
generated quantities {
  int<lower = 0> y_rep[N] = categorical_logit_rng(mu);
  real<lower = 0> mean_y_rep = mean(to_vector(y_rep));
  real<lower = 0> sd_y_rep = sd(to_vector(y_rep));
}


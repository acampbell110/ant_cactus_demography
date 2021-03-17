
data {
    int<lower=2> N_ant; // of alternatives/outcomes
    int<lower=1> N_data; // of observations
    int <lower = 1, upper = N_ant> ant_data[N_data]; // the list of ant species 
    int <lower = 1, upper = N_ant> ant1_data[N_data]; // the list of ant species 
    vector[N_data] vol_data;
    int<lower=1> N_Year; //number of plots
    int<lower=1> N_Plot; //number of years
    int<lower=1, upper=N_Plot> plot_data[N_data]; // plot
    int<lower=1, upper=N_Year> year_data[N_data]; // year
}
parameters {
  vector[N_ant] beta0; //intercept, unique to ant sp
  vector[N_Plot] u; //subject intercepts
  vector[N_Year] w; //item intercepts
  real < lower = 0 > sigma; // Error SD
  real < lower = 0 > sigma_u; // plot SD
  real < lower = 0 > sigma_w; // year SD
}
transformed parameters{
  vector[N_data] mu; //linear predictor for the mean
  for(i in 1:N_data){
    mu[i] = ant_data[i];
  }
}
model {
  // Model
  u ~ normal(0, sigma_u); // plot random effects
  w ~ normal(0, sigma_w); // year random effects
  beta0 ~ normal(0,100); // intercept distribution
  ant1_data ~ categorical_logit(mu);
}
generated quantities {
  vector<lower = 0,upper = N_ant>[N_data] y_rep = categorical_logit_rng(mu);
}


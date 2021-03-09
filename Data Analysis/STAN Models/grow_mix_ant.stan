// Stan model for simple growth regression
data {
  int <lower = 1> N_data; // number of observations
  int <lower = 1> N_ant; // number of ant states
  int <lower = 1, upper = N_ant> ant_data[N_data]; // the list of ant species 
  vector[N_data] vol_data;	//size_t
  vector[N_data] y_grow; // size_t1
  int<lower=1> N_Year; //number of plots
  int<lower=1> N_Plot; //number of years
  int<lower=1, upper=N_Plot> plot_data[N_data]; // plot
  int<lower=1, upper=N_Year> year_data[N_data]; // year
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
  vector[N_data] mu; //linear predictor for the mean
  for(i in 1:N_data){
    mu[i] = beta0[ant_data[i]] + beta1[ant_data[i]] * vol_data[i] + u[plot_data[i]] + w[year_data[i]];
  }
}
model {
  u ~ normal(0, sigma_u); // plot random effects
  w ~ normal(0, sigma_w); // year random effects
  beta0 ~ normal(0,100); // intercept distribution
  beta1 ~ normal(0,100); // slope distribution
  for(i in 1:N_data){
    y_grow[i] ~ normal(mu[i], sigma);
  }
}
generated quantities{
  real  y_rep[N_data] = normal_rng(mu, sigma);
  real  mean_y_rep = mean(to_vector(y_rep));
  real  sd_y_rep = sd(to_vector(y_rep));
}


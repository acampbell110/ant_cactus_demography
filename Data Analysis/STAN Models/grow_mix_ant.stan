// Stan model for simple growth regression
data {
  int <lower = 1> N_grow; // number of observations
  int <lower = 1> N_ant; // number of ant states
  int <lower = 1, upper = N_ant> ant_grow[N_grow]; // the list of ant species 
  vector[N_grow] vol_grow;	//size_t
  vector[N_grow] y_grow; // size_t1
  int<lower=1> N_Year_grow; //number of plots
  int<lower=1> N_Plot_grow; //number of years
  int<lower=1, upper=N_Plot_grow> plot_grow[N_grow]; // plot
  int<lower=1, upper=N_Year_grow> year_grow[N_grow]; // year
}
parameters {
  vector[N_ant] beta0; //intercept, unique to ant sp
  vector[N_ant] beta1; //slope, unique to ant sp
  vector[N_Plot_grow] u; //subject intercepts
  vector[N_Year_grow] w; //item intercepts
  real < lower = 0 > sigma; // Error SD
  real < lower = 0 > sigma_u; // plot SD
  real < lower = 0 > sigma_w; // year SD
}
transformed parameters{
  vector[N_grow] mu; //linear predictor for the mean
  for(i in 1:N_grow){
    mu[i] = beta0[ant_grow[i]] + beta1[ant_grow[i]] * vol_grow[i] + u[plot_grow[i]] + w[year_grow[i]];
  }
}
model {
  u ~ normal(0, sigma_u); // plot random effects
  w ~ normal(0, sigma_w); // year random effects
  beta0 ~ normal(0,100); // intercept distribution
  beta1 ~ normal(0,100); // slope distribution
  for(i in 1:N_grow){
    y_grow[i] ~ normal(mu[i], sigma);
  }
}
generated quantities{
  real  y_rep[N_grow] = normal_rng(mu, sigma);
  real  mean_y_rep = mean(to_vector(y_rep));
  real  sd_y_rep = sd(to_vector(y_rep));
}


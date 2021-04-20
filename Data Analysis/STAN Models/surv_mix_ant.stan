// Stan model for simple ant survival regression

data {
  int <lower = 1> N_surv; // number of observations
  int <lower = 1> N_ant; // number of ant states
  int <lower = 1, upper = N_ant> ant_surv[N_surv]; // the list of ant species 
  vector[N_surv] vol_surv;	//size_t
  int <lower = 0, upper = 1> y_surv[N_surv]; // survival in year t1
  int<lower=1> N_Year_surv; //number of plots
  int<lower=1> N_Plot_surv; //number of years
  int<lower=1, upper=N_Plot_surv> plot_surv[N_surv]; // plot
  int<lower=1, upper=N_Year_surv> year_surv[N_surv]; // year
}
parameters {
  vector[N_ant] beta0; //intercept, unique to ant sp
  vector[N_ant] beta1; //slope, unique to ant sp
  vector[N_Plot_surv] u; //subject intercepts
  vector[N_Year_surv] w; //item intercepts
  real < lower = 0 > sigma; // Error SD
  real < lower = 0 > sigma_u; // plot SD
  real < lower = 0 > sigma_w; // year SD
}

transformed parameters{
  vector[N_surv] mu; //linear predictor for the mean
  for(i in 1:N_surv){
    mu[i] = beta0[ant_surv[i]] + beta1[ant_surv[i]] * vol_surv[i] + u[plot_surv[i]] + w[year_surv[i]];
  }
}
model {
//Priors
 u ~ normal(0, sigma_u); // plot random effects
 w ~ normal(0, sigma_w); // year random effects
 beta0 ~ normal(0,100); // intercept distribution
 beta1 ~ normal(0,100); // slope distribution
 //Model
 for(i in 1:N_surv){
 y_surv[i] ~ bernoulli_logit(mu[i]);
 }
}
generated quantities {
  int<lower = 0> y_rep[N_surv] = bernoulli_logit_rng(mu);
  real<lower = 0> mean_y_rep = mean(to_vector(y_rep));
  real<lower = 0> sd_y_rep = sd(to_vector(y_rep));
}


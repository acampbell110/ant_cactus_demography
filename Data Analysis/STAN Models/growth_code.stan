/// Stan model for simple ant growth regression

data {
  int <lower = 1> N; // number of observations
  int <lower = 1> K; // number of ant states
  int <lower = 1, upper = K> ant[N]; // the list of ant species 
  vector[N] vol;	//size_t
  vector[N] y_grow; // survival in year t1
  int<lower=1> N_Year; //number of plots
  int<lower=1> N_Plot; //number of years
  int<lower=1, upper=N_Plot> plot[N]; // plot
  int<lower=1, upper=N_Year> year[N]; // year
}
parameters {
  vector[K] beta0; //ant beta
  vector[K] beta1; //interaction beta
  vector[N_Plot] u; //subject intercepts
  vector[N_Year] w; //item intercepts
  real < lower = 0 > sigma; // Error SD
  real < lower = 0 > sigma_u; // plot SD
  real < lower = 0 > sigma_w; // year SD
}

transformed parameters{
  vector[N] mu; //linear predictor for the mean
  for(i in 1:N){
    mu[i] = beta0[ant[i]] + beta1[ant[i]] * vol[i] + u[plot[i]] + w[year[i]];
  }
}
model {
//Priors
 u ~ normal(0, sigma_u); // plot random effects
 w ~ normal(0, sigma_w); // year random effects
 beta0 ~ normal(0,100); // ant beta
 beta1 ~ normal(0,100); // size & ant beta
 //Model
 for(i in 1:N){
 y_grow[i] ~ normal(mu[i],sigma);
 }
}
generated quantities {
  int<lower = 0> y_rep[N] = bernoulli_logit_rng(mu);
  real<lower = 0> mean_y_rep = mean(to_vector(y_rep));
  real<lower = 0> sd_y_rep = sd(to_vector(y_rep));
}



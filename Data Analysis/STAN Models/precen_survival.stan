// Stan model for simple pre-census survival regression

data {
  int <lower = 1> N_ss; // number of observations
  vector[N_ss] vol_ss;	//size_t
  int <lower = 0, upper = 1> y_ss[N_ss]; // survival in year t1
}
parameters {
  real beta0; //intercept, unique to ant sp
  real beta1; //slope, unique to ant sp
  real < lower = 0 > sigma; // Error SD
}

transformed parameters{
  vector[N_ss] mu; //linear predictor for the mean
  for(i in 1:N_ss){
    mu[i] = beta0 + beta1 * vol_ss[i];
  }
}
model {
//Priors
 beta0 ~ normal(0,100); // intercept distribution
 beta1 ~ normal(0,100); // slope distribution
 //Model
 for(i in 1:N_ss){
 y_ss[i] ~ bernoulli_logit(mu[i]);
 }
}
generated quantities {
  int<lower = 0> y_rep[N_ss] = bernoulli_logit_rng(mu);
  real<lower = 0> mean_y_rep = mean(to_vector(y_rep));
  real<lower = 0> sd_y_rep = sd(to_vector(y_rep));
}

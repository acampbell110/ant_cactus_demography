/// Stan model for simple ant survival regression

data {
  int <lower = 1> N; // number of observations
  int <lower = 0, upper = 1> y[N]; // survival in year t1
}
parameters {
  real beta0; //intercept, unique to ant sp
}

transformed parameters{
  vector[N] mu; //linear predictor for the mean
  for(i in 1:N){
    mu[i] = beta0;
  }
}
model {
//Priors
 beta0 ~ normal(0,100); // intercept distribution
 //Model
 for(i in 1:N){
 y[i] ~ bernoulli_logit(mu[i]);
 }
}
generated quantities {
  int<lower = 0> y_rep[N] = bernoulli_logit_rng(mu);
  real<lower = 0> mean_y_rep = mean(to_vector(y_rep));
  real<lower = 0> sd_y_rep = sd(to_vector(y_rep));
}



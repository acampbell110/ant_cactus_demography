data {
  int <lower = 1> N_fruit; // number of observations
  real <lower = 0, upper = 1> fr_prop[N_fruit]; // survival in year t1
  int<lower=1> on_ground[N_fruit];
  int<lower=1> on_plant[N_fruit];
}
parameters {
  vector [N_fruit] beta0; //intercept, unique to ant sp
  real < lower = 0 > sigma; // Error SD
}

transformed parameters{
  vector[N_fruit] mu; //linear predictor for the mean
  for(i in 1:N_fruit){
    mu[i] = beta0[i];
  }
}
model {
//Priors
 beta0 ~ normal(0,100); // intercept distribution
 //Model
 on_ground ~ binomial_logit(on_plant,mu);
}
generated quantities {
  int<lower = 0> y_rep[N_fruit] = bernoulli_logit_rng(mu);
  real<lower = 0> mean_y_rep = mean(to_vector(y_rep));
  real<lower = 0> sd_y_rep = sd(to_vector(y_rep));
}


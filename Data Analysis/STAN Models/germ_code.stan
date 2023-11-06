// Stan model for simple total flowers regression

data {
  int <lower = 1> N; // number of observations
  int y_germ[N]; // number of seeds germinated
  int<lower=1> trials[N]; // number of seeds put out
}
parameters {
  real beta0; //intercept
  real < lower = 0 > sigma; // Error SD
}
transformed parameters{
  vector[N] mu; //linear predictor for the mean
  for(i in 1:N){
   	mu[i] = beta0;
  }
  
}
model {
  // Model
  beta0 ~ normal(0,100); // intercept distribution
  
  for(i in 1:N){
    y_germ[i] ~ binomial_logit(trials[i], (mu[i]));
  }
}
generated quantities {
  int<lower = 0> y_rep[N] = binomial_rng(trials, inv_logit(mu));
  real<lower = 0> mean_y_rep = mean(to_vector(y_rep));
  real<lower = 0> sd_y_rep = sd(to_vector(y_rep));
} 


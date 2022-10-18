
data{
  int <lower = 1> N; // number of observations
  int <lower = 2> K; // number of observations
  int <lower = 1, upper = K> ant[N]; // the list of ant species 
  int <lower = 0> seed[N]; // 
}
parameters{
  real < lower = 0> phi;
  vector[K] beta0; //intercept, unique to ant sp
  real < lower = 0 > sigma; // Error SD
}
transformed parameters{
  vector[N] mu; //linear predictor for the mean
  for(i in 1:N){
   	mu[i] = beta0[ant[i]];
  };
}
model{
  beta0 ~ normal(0,100); // intercept distribution
  for(i in 1:N){
    seed[i] ~ neg_binomial_2(exp(mu[i]), phi);
  };
}
generated quantities{
  int<lower = 0> y_rep[N] = neg_binomial_2_rng(exp(mu), phi);
  real<lower = 0> mean_y_rep = mean(to_vector(y_rep));
  real<lower = 0> sd_y_rep = sd(to_vector(y_rep));
}
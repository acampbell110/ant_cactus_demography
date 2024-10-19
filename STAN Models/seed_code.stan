
data{
  int <lower = 1> N; // number of observations
  int <lower = 1> N_plants; // number of plants
  int <lower = 1> K; // number of ant states
  int <lower = 1, upper = K> ant[N]; // the list of ant species 
  int <lower = 1, upper = N_plants> plant[N];
  int <lower = 0> seed[N]; // 
}
parameters{
  real < lower = 0> phi;
  real < lower = 0> sigma_plant;
  vector[K] beta0; //intercept, unique to ant sp
  vector[N_plants] alpha0; //random intercept for plant id
}
transformed parameters{
  vector[N] mu; //linear predictor for the mean
  for(i in 1:N){
   	mu[i] = alpha0[plant[i]] + beta0[ant[i]];
  }
}
model{
  beta0 ~ normal(0,100); // intercept distribution
  alpha0 ~ normal(0,sigma_plant);
  phi ~ gamma(0.01, 0.01);
  seed ~ neg_binomial_2(exp(mu), phi);
}
generated quantities{
  int<lower = 0> y_rep[N] = neg_binomial_2_rng(exp(mu), phi);
  real<lower = 0> mean_y_rep = mean(to_vector(y_rep));
  real<lower = 0> sd_y_rep = sd(to_vector(y_rep));
}



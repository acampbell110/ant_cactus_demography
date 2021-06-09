
data{
  int <lower = 1> N_seed; // number of observations
  int <lower = 2> N_ant_seed; // number of observations
  int <lower = 1, upper = N_ant_seed> ant_seed[N_seed]; // the list of ant species 
  int <lower = 0> seed[N_seed]; // survival in year t1
}
parameters{
  real < lower = 0> phi_seed;
  vector[N_ant_seed] beta0_seed; //intercept, unique to ant sp
  real < lower = 0 > sigma_seed; // Error SD
}
transformed parameters{
  vector[N_seed] mu_seed; //linear predictor for the mean
  for(i in 1:N_seed){
   	mu_seed[i] = beta0_seed[ant_seed[i]];
  };
}
model{
  beta0_seed ~ normal(0,100); // intercept distribution
  for(i in 1:N_seed){
    seed[i] ~ neg_binomial_2(exp(mu_seed[i]), phi_seed);
  };
}
generated quantities{
  int<lower = 0> y_rep_seed[N_seed] = neg_binomial_2_rng(exp(mu_seed), phi_seed);
  real<lower = 0> mean_y_rep_seed = mean(to_vector(y_rep_seed));
  real<lower = 0> sd_y_rep_seed = sd(to_vector(y_rep_seed));
}
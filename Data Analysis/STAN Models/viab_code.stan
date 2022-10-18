data {
  int < lower = 1 > N; // Sample size
  int <lower = 0> good[N]; // Number of viable seeds
  int <lower = 1> K; // number of ant states
  int <lower = 1, upper = K> ant[N]; // the list of ant species 
  int<lower=1> tot[N]; // number of trials
  int<lower=0> abort[N];
  int<lower=1> N_Year; //number of plots
  int<lower=1> N_Plot; //number of years
  int<lower=1, upper=N_Plot> plot[N]; // plot
  int<lower=1, upper=N_Year> year[N]; // year
}
parameters {
  matrix[K,N_Year] w; 
  vector[K] beta0; // intercept of probability of viability for each bud
  vector[N_Plot] u; //subject intercepts
  real < lower = 0 > sigma; // Error SD
  real < lower = 0 > sigma_u; // plot SD
  real < lower = 0 > sigma_w; // year SD
}
transformed parameters{
  vector[N] mu; //linear predictor for the mean
    // Prediction for seed viability
  for(i in 1:N){
    mu[i] = beta0[ant[i]] + u[plot[i]] + w[ant[i],year[i]];
  }
}
model {
  //Priors
 u ~ normal(0, sigma_u); // plot random effects
 for(i in 1:K){
   w[i,] ~ normal(0,sigma_w);
 } 
 sigma ~ normal(0,10);
 beta0 ~ normal(0,10); // intercept distribution

  good ~ binomial_logit(tot, mu);
}
// generated quantities {
//   int<lower = 0> y_rep[N] = binomial_rng(tot , inv_logit(mu));
//   real<lower = 0> mean_y_rep = mean(to_vector(y_rep));
//   real<lower = 0> sd_y_rep = sd(to_vector(y_rep));
// }

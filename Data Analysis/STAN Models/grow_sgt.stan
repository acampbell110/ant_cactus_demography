functions {
  #include skew_generalized_t.stanfunctions
}
data {
  int <lower = 1> N;                   // number of observations
  int <lower = 1> K;                   // number of ant states
  vector[N] y;                         // survival in year t1
  vector[N] vol;	                     //size in year t
  //vector[N] vol2;
  int <lower = 1, upper = K> ant[N];   // the list of ant species 
  int<lower=1> N_Year;                 //number of plots
  int<lower=1> N_Plot;                 //number of years
  int<lower=1, upper=N_Plot> plot[N];  // plot
  int<lower=1, upper=N_Year> year[N];  // year
}
parameters {
  matrix[K,N_Year] w;         // year random effects
  vector[K] beta0;            // ant beta
  vector[K] beta1;            // interaction size and ant beta
  vector[N_Plot] u;           // Plot random effects
  real < lower = 0 > sigma_w; // plot SD
  real < lower = 0 > sigma_u; // plot SD
  real d_0;                   // Error intercept
  real d_size;                // Error size
  real<lower=-1, upper=1> lambda;
  real<lower=0> q;
  real<lower=2.0/q> p;
}
transformed parameters{
  vector[N] mu;               // linear predictor for the mean
  vector[N] sigma;            // transformed predictor for the sd

  for(i in 1:N){
    mu[i] = beta0[ant[i]] + beta1[ant[i]] * vol[i] + u[plot[i]] + w[ant[i],year[i]];
    sigma[i] = exp(d_0 + d_size * vol[i]);
  }
}
model {
//Priors
  u ~ normal(0, sigma_u);         // plot random effects
  for(i in 1:K){
    w[i,] ~ normal(0,sigma_w);    // year random effects
  }
  beta0 ~ normal(0,10);          // ant beta
  beta1 ~ normal(0,10);          // size & ant beta
  d_0 ~ normal(0, 10);           // intercept sd 
  d_size ~ normal(0, 10);        // size sd
  lambda ~ std_normal();
  p ~ normal(2, 1);
  q ~ normal(7, 2);
  for(i in 1:N){  
  y[i] ~ skew_generalized_t(mu[i], sigma[i], lambda, p, q);
  }
}

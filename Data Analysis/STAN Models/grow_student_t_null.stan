data {
  int <lower = 1> N;                   // number of observations
  int <lower = 1> K;                   // number of ant states
  vector[N] y;                         // survival in year t1
  vector[N] vol;	                     //size in year t
  vector[N] vol2;
  int <lower = 1, upper = K> ant[N];   // the list of ant species 
  int<lower=1> N_Year;                 //number of plots
  int<lower=1> N_Plot;                 //number of years
  int<lower=1, upper=N_Plot> plot[N];  // plot
  int<lower=1, upper=N_Year> year[N];  // year
}
parameters {
  vector[N_Year] w;         // year random effects
  vector[K] beta0;            // ant beta
  vector[K] beta1;            // interaction size and ant beta
  vector[K] beta2;            // interaction size and ant beta
  vector[N_Plot] u;           // Plot random effects
  real < lower = 0 > sigma_w; // plot SD
  real < lower = 0 > sigma_u; // plot SD
  real d_0;                   // Error intercept
  real d_size;                // Error size
  real a_0;                   // Error intercept
  real a_size;                // Error size
}
transformed parameters{
  vector[N] nu;               // transformed predictor for df
  vector[N] mu;               // linear predictor for the mean
  vector[N] sigma;            // transformed predictor for the sd

  for(i in 1:N){
    nu[i] = exp(a_0 + a_size * vol[i]);
    mu[i] = beta0[ant[i]] + beta1[ant[i]] * vol[i] + beta2[ant[i]] * vol2[i] + u[plot[i]] + w[year[i]];
    sigma[i] = exp(d_0 + d_size * vol[i]);
  }
}
model {
//Priors
  u ~ normal(0, sigma_u);         // plot random effects
  w ~ normal(0,sigma_w);    // year random effects
  beta0 ~ normal(0,10);          // ant beta
  beta1 ~ normal(0,10);          // size & ant beta
  beta2 ~ normal(0,10);          // size & ant beta
  d_0 ~ normal(0, 10);           // intercept sd 
  d_size ~ normal(0, 10);        // size sd
  a_0 ~ normal(0, 10);           // intercept sd 
  a_size ~ normal(0, 10);        // size sd
//sampling  
  y ~ student_t(nu, mu, sigma);
}
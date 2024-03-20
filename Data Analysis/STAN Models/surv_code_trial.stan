/// Stan model for simple ant survival regression

data {
  int <lower = 1> N; // number of observations
  int <lower = 1> K; // number of ant states
  int <lower = 1, upper = K> ant[N]; // the list of ant species 
  vector[N] vol;	//size_t
  int <lower = 0, upper = 1> y_surv[N]; // survival in year t1
  int<lower=1> N_Year; //number of plots
  int<lower=1> N_Plot; //number of years
  int<lower=1, upper=N_Plot> plot[N]; // plot
  int<lower=1, upper=N_Year> year[N]; // year
}
parameters {
  matrix[K,N_Year] w; 
  vector[K] beta0; //intercept, unique to ant sp
  vector[K] beta1; //slope, unique to ant sp
  vector[N_Plot] u; //subject intercepts
  real < lower = 0 > sigma; // Error SD
  real < lower = 0 > sigma_u; // plot SD
  real < lower = 0 > sigma_w; // year SD
}

transformed parameters{
  real mu [N] ; //linear predictor for the mean
  for(i in 1:N){
    mu[i] = beta0[ant[i]] + beta1[ant[i]] * vol[i] + u[plot[i]] + w[ant[i],year[i]];
  }
}

model {
//Priors
 u ~ normal(0, sigma_u); // plot random effects
 for(i in 1:K){
   w[i,] ~ normal(0,sigma_w);
 } 
 sigma ~ normal(0,1);
 beta0 ~ normal(0,sigma); // intercept distribution
 beta1 ~ normal(0,sigma); // slope distribution
 // sampling
 y_surv ~ bernoulli_logit(mu);
}
 generated quantities {
   vertor[N] log_lik;
   for(i in 1:N){
     log_lik[i]=bernoulli_logit_lpmf(y_surv[i] | mu[i]);
   }
 }






/// Stan model for simple ant growth regression
data {
  int <lower = 1> N;                   // number of observations
  vector[N] y;                         // survival in year t1
  vector[N] vol;	                     //size in year t
}

parameters {
  real beta0;                          // ant beta
  real beta1;                          // interaction size and ant beta
  real sigma;                          // transformed predictor for the sd
  real alpha;                          // predictor for the skew
  }

transformed parameters{
  vector[N] mu;                       // linear predictor for the mean
  for(i in 1:N){
    mu[i] = beta0 + beta1 * vol[i];
  }
}
model {
//Priors
  //Model
  for(i in 1:N){
    y[i] ~ skew_normal(mu[i],sigma, alpha);
  }
}
generated quantities {
  real y_rep[N] = skew_normal_rng(mu,sigma,alpha);
}







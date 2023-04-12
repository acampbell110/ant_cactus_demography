/// Stan model for simple ant growth regression
data {
  int <lower = 1> N; // number of observations
  vector[N] y;      // survival in year t1
  vector[N] vol;	//size in year t
}

parameters {
  real beta0; // intercept beta
  real beta1; // volume beta
  real alpha; // alpha
  real sigma; // omega
}

transformed parameters{
  vector[N] mu; //linear predictor for the mean
  for(i in 1:N){
    mu[i] = beta0 + beta1 * vol[i];
  }
}
model {
//Priors
 beta0 ~ normal(0,100); // ant beta
 //Model
 for(i in 1:N){
 y[i] ~ skew_normal(mu[i],sigma, alpha);
 }
}
generated quantities {
  real y_rep[N] = skew_normal_rng(mu,sigma,alpha);
  real mean_y_rep = mean(to_vector(y_rep));
  real<lower = 0> sd_y_rep = sd(to_vector(y_rep));
}







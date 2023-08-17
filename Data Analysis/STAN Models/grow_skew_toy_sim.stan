/// Stan model for simple ant growth regression
data {
  int <lower = 1> N;                   // number of observations
  vector[N] y;                         // survival in year t1
  vector[N] x;	                     //size in year t
}

parameters {
  real beta0;            // ant beta
  real beta1;            // interaction size and ant beta
  real d_0;                   // Error intercept
  real d_size;                // Error size
  real a_0;                   // skew intercept
  real a_size;                // skew size
  //real alpha;
  //real < lower = 0 > omega;
  }

transformed parameters{
  vector[N] xi;               // linear predictor for the mean
  vector[N] omega;            // transformed predictor for the sd
  vector[N] alpha;            // predictor for the skew

  for(i in 1:N){
    xi[i] = beta0 + beta1 * x[i];
    omega[i] = exp(d_0 + d_size * x[i]);
    alpha[i] = a_0 + a_size * x[i];
  }
}
model {
//Priors
  beta0 ~ normal(0,100);          // ant beta
  beta1 ~ normal(0,100);          // size & ant beta
  d_0 ~ normal(0, 100);           // intercept sd 
  d_size ~ normal(0, 100);        // size sd
  a_0 ~ normal(0, 100);           // intercept skew 
  a_size ~ normal(0, 100);        // size skew
  //beta0 ~ normal(0,100);          // ant beta
  //sigma ~ gamma(.001,.001);
  //Model
  //for(i in 1:N){
    y ~ skew_normal(xi,omega,alpha);
  //}
}
generated quantities {
  real y_rep[N] = skew_normal_rng(xi,omega,alpha);
}


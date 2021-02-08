// Stan model for simple linear regression

data {
 int < lower = 1 > N; // Sample size
 int < lower = 1 > K; // Number of Predictors (Ant Species)
 real < lower = 0 > vol[N]; // volume
 int < lower = 0 > ant[K]; // ants
 int <lower=0,upper=1> y[N]; // outcome vector
}

parameters {
  real alpha;
  vector[K] beta; // coefficients for predictors
}

transformed parameters{
 real mu[N];
 for(i in 1:N){
 mu[i] = beta[1] + beta[2]*vol[i] + beta[3]*ant[i]; 
 }
}

model {
//Priors
 alpha ~ normal(0,100);
 beta ~ normal(0,100);
 //Model
 y ~ bernoulli_logit(mu);
}


// Stan model for simple linear regression

data {
  int<lower=0> N;
  vector[N] x;
  int<lower=0,upper=1> y[N];
}
parameters {
  real alpha;
  real beta;
}
model {
 // Priors
 alpha ~ normal(0,100);
 beta ~ normal(0,100); 
 // Model
 y ~ bernoulli_logit(alpha + beta * x);
}


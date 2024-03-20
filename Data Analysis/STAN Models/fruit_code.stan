data {
  int<lower=1> N; // Sample size
  int<lower=0>good[N]; // Number of viable seeds
  int<lower=1> tot[N]; // number of trials
}
parameters {
  real beta0; // intercept of probability of viability for each bud
  real < lower = 0 > sigma; // Error SD
}
transformed parameters{
  real mu[N]; //linear predictor for the mean
    // Prediction for seed viability
  for(i in 1:N){
    mu[i] = beta0;
  }
}
model {
  //Priors
 sigma ~ normal(0,10);
 beta0 ~ normal(0,10); // intercept distribution

  good ~ binomial_logit(tot, mu);

}


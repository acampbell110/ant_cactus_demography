data {
  int<lower=1> N; // Sample size
  int<lower=0>good[N]; // Number of fruits escaping predation
  int<lower=1> tot[N]; // total number of fruits on plant
}
parameters {
  real beta0; // intercept of probability of viability for each bud
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
 beta0 ~ normal(0,10); // intercept distribution

  good ~ binomial_logit(tot, mu);

}


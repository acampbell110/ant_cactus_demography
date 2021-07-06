// https://mc-stan.org/docs/2_20/stan-users-guide/multi-logit-section.html
// This has no intercept. This also has no reference level.

data {
int K; //alternatives for Non Cont Predictor and Y
int N; //number of trials
int D; //number of predictors (1)
int <lower = 1, upper = K> Y[N]; // Outcome
real X[N]; // Continuous Predictor
int <lower = 1, upper = K> Z[N]; // Non Continuous Predictor
}
parameters {
vector[K] beta1; //slope param
}
transformed parameters {
  vector[N] mu; //linear predictor for the mean
  for(i in 1:N){
    mu[i] = beta1[Z[i]] * X[i];
  }
}
model {
    Y ~ categorical_logit(mu);
    
}




data {
    int<lower=2> J; // of alternatives/outcomes (ant state) -- 4
    //int<lower=2> G; // of alternatives/outcomes (volume) -- inf
    int<lower=1> N; // of observations utilities (number of data points)
    int<lower=1> K; // of covariates (volume & ant state)
    int<lower=0,upper=J> Y[N];
    matrix[J,K] X[N];
}

parameters {
    vector[J-1] alpha_raw; // unconstrained UPC intercepts
    vector[J] beta1;
}

transformed parameters{
  vector[J] alpha; 
  alpha = append_row(-sum(alpha_raw), alpha_raw); // sum to zero constraint
}

model {
    for (i in 1:N)
        Y[i] ~ categorical_logit(alpha + X[i]*beta1);
}
generated quantities{
  int Y_rep[N];
  for (i in 1:N)
         Y_rep[i]= categorical_logit_rng(alpha + X[i]*beta1);
}

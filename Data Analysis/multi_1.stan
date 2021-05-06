data {
    int<lower=2> J; // of alternatives/outcomes
    int<lower=1> N; // of observations
    int<lower=1> K; // of covariates
    int<lower=0,upper=J> Y[N];
    matrix[J,K] X[N];
}

parameters {
    vector[K] beta;  // attribute effects 
}

model {
    for (i in 1:N)
        Y[i] ~ categorical_logit(X[i]*beta); // then our y here will be the log(prob ij/prob ij*) where j != j* and j* is the baseline/ref level
}
generated quantities{
    
}


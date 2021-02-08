// Stan model for simple growth regression

data {
 int < lower = 1 > N; // Sample size
 vector[N] x; // Predictor
 vector[N] y; // Outcome
}

parameters {
 real alpha; // Intercept
 real beta; // Slope (regression coefficients)
 real < lower = 0 > sigma; // Error SD
}

model {
// Priors
alpha ~ normal(0,100);
beta ~ normal(0,100); 
//sigma ~ exponential(0,0);
 y ~ normal(alpha + x * beta , sigma); // The Model
}

generated quantities {
} // The posterior predictive distribution

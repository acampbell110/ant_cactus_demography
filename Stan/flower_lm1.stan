// Stan model for simple linear regression

data {
 int N;
 real x[N];
 int y[N];
}
parameters {
 real<lower = 0> lambda;
}
model {
// Priors
 lambda ~ cauchy(0, 1);
 //lambda ~ gamma(0, 0);
 // Model
 y ~ poisson(lambda);
}


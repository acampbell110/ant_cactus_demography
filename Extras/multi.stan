// https://mc-stan.org/docs/2_20/stan-users-guide/multi-logit-section.html
// This has no intercept. This also has no reference level.

data {
  int K; // Number of alternatives for Non cont outcome
  int N; // Number of observations
  int D; // Number of covariates (how many input variables)
  int <lower = 1, upper = K> y[N]; // Output non continuoius variable
  matrix[N, D] x; // Input continuous variable (aka all variables in this case)
}
parameters {
  matrix[D, K] beta; // Intercept parameters (unique to each variable (each row = dif variable) and each alternative (each column = dif alt))
}
model {
  matrix[N, K] x_beta = x * beta; // the equation = the matrix of all input variables * matrix of all parameters (aka beta1[k]*x1 + beta2[k]*x2...)

  to_vector(beta) ~ normal(0, 5); // make this a vector of priors

  for (n in 1:N)
    y[n] ~ categorical_logit(x_beta[n]'); // take the real inputs and spit out the estimated y's to estimate the betas
}

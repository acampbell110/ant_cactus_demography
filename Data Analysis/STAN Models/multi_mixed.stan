data {
  int<lower=2> K; // Number of ant outcomes possible (4)
  int<lower=0> N; // Number of data points
  int<lower=1> D; // Number of fixed effect predictor variables
  int<lower=1> P; // Number of random effect predictor variables
  int<lower=1> N_Year; // Number of Years
  int<lower=1,upper=K> y[N]; // Outcomes
  matrix[N, D] x; // All fixed effect predictor variables as a matrix
  matrix[N, P] z; // All random effect predictor variables
}

transformed data {
  vector[D] zeros_f = rep_vector(0, D);
  vector[P] zeros_r = rep_vector(0, P);
}

parameters {
  matrix[D, K - 1] beta_raw; // fixed effect parameters
  matrix[P, K - 1] theta_raw; // random effect parameters
  vector[K] sigma_w; // Year SD
}

transformed parameters {
  matrix[D, K] beta = append_col(beta_raw, zeros_f); // fixed effect transformed parameters
  matrix[P, K] theta = append_col(theta_raw,zeros_r); // random effect transformed parameters
}

model {
    matrix[N, K] x_beta = (x * beta); // fixed effects
    matrix[N, K] z_theta = (z * theta); // random effects
  // Priors
  //for(i in 1:(K-1)){
  // theta_raw[,i] ~ normal(0,sigma_w[i]);
 //}
  //to_vector(beta) ~ normal(0, 5); 

  for (m in 1:N) {
    y[m] ~ categorical_logit(x_beta[m]');

  }
}



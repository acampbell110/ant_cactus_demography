// Stan model for simple growth regression
data {
  int <lower = 1> K; //number of possible ant states 
  int <lower = 1> N; // number of observations
  int <lower = 1> D; // number of predictors
  vector[N] y_grow; // size_t1
  matrix[N, D] x;
}
transformed data {
  vector[D] zeros = rep_vector(0, D);
}

parameters {
  matrix[D, K - 1] beta_raw;
  real < lower = 0 > sigma; // Error SD
}

transformed parameters {
  matrix[D, K] beta = append_col(beta_raw, zeros);
}

model {
  matrix[N, K] x_beta = x * beta;
  to_vector(beta) ~ normal(0, 5);
  
  // Likelihood:
  for(n in 1:N)
    target += normal_lpdf(y_grow[n] | x_beta[n]', sigma);
}
//generated quantities{
//  real  y_rep[N] = normal_rng(x_beta', sigma);
//  real  mean_y_rep = mean(to_vector(y_rep));
//  real  sd_y_rep = sd(to_vector(y_rep));
//}


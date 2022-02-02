data {
  int<lower=2> K;
  int<lower=0> N;
  int<lower=1> D;
  int<lower=1, upper=K> y[N];
  matrix[N, D] x;
}

transformed data {
  vector[D] zeros = rep_vector(0, D);
}

parameters {
  matrix[D, K - 1] beta_raw;
}

transformed parameters {
  matrix[D, K] beta = append_col(beta_raw, zeros);
}

model {
  matrix[N, K] x_beta = x * beta;

  to_vector(beta) ~ normal(0, 5);

  for (n in 1:N) {
    y[n] ~ categorical_logit(x_beta[n]');

  }
}
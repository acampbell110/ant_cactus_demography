data {
  int<lower=0> N;
  vector[N] y;
}

parameters {
  real xi;
  real<lower=0> omega;
  real alpha;
}

model {
  y ~ skew_normal(xi, omega, alpha);
}


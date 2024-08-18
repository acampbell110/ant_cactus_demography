// Stan model for simple growth regression
data {
  int <lower = 1> N; // number of observations
  vector[N] y_rec; // size_t1
}
parameters {
  real beta0; //mean size
  real < lower = 0 > sigma; // Error SD
}
model {
  beta0 ~ normal(0,10); 
  y_rec ~ normal(beta0, sigma);
}
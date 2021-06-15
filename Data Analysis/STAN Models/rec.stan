// Stan model for simple growth regression
data {
  int <lower = 1> N_rec; // number of observations
  vector[N_rec] y_rec; // size_t1
}
parameters {
  real beta0; //intercept, unique to ant sp
  real < lower = 0 > sigma; // Error SD
}
transformed parameters{
  vector[N_rec] mu; //linear predictor for the mean
  for(i in 1:N_rec){
    mu[i] = beta0;
  }
}
model {
  beta0 ~ normal(0,100); // intercept distribution
  for(i in 1:N_rec){
    y_rec[i] ~ normal(mu[i], sigma);
  }
}
generated quantities{
  real  y_rep[N_rec] = normal_rng(mu, sigma);
  real  mean_y_rep = mean(to_vector(y_rep));
  real  sd_y_rep = sd(to_vector(y_rep));
}

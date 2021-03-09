// Stan model for simple total flowers regression
data {
 int < lower = 1 > N; // Sample size
 int <lower = 0> good[N]; // Number of viable seeds
 int <lower = 1> N_ant; // number of ant states
 int <lower = 1, upper = N_ant> ant[N]; // the list of ant species 
 int<lower=0> tot[N]; // number of trials
 int<lower=0> abort[N];
 vector[N] vol;	//size_t
 int<lower=1> N_Year; //number of plots
  int<lower=1> N_Plot; //number of years
  int<lower=1, upper=N_Plot> plot[N]; // plot
  int<lower=1, upper=N_Year> year[N]; // year
}
transformed data {
  real<lower = 0> mean_y = mean(to_vector(good));
  real<lower = 0> sd_y = sd(to_vector(good));
}
parameters {
  real theta[N]; // probability of viability for each bud
  //
  real<lower=0,upper=1> v0;
  vector[N_Plot] u; //subject intercepts
  vector[N_Year] w; //item intercepts
  real < lower = 0 > sigma; // Error SD
  real < lower = 0 > sigma_u; // plot SD
  real < lower = 0 > sigma_w; // year SD
}
transformed parameters{
  real<lower=0,upper=1> predV[N]; // proportion viable for each plant?
  // Prediction for seed viability
  for(i in 1:N){
    predV[i] = logit(theta[ant[i]] + u[ant[i]] + w[ant[i]]);
  }
}
model {
 // Priors
  v0  ~ beta(10,1);  // intercept viability model
  theta ~ normal(0,100); // intercept distribution
  //Model Statements
  u ~ normal(0, sigma_u); // plot random effects
  w ~ normal(0, sigma_w); // year random effects
  
  good ~ binomial(tot, predV);
}
generated quantities {
  int<lower = 0> y_rep[N] = binomial_rng(tot,predV);
  real<lower = 0> mean_y_rep = mean(to_vector(y_rep));
  real<lower = 0> sd_y_rep = sd(to_vector(y_rep));
}


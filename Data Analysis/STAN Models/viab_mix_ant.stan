// Stan model for simple total flowers regression
data {
 int < lower = 1 > N_viab; // Sample size
 int <lower = 0> good_viab[N_viab]; // Number of viable seeds
 int <lower = 1> N_ant; // number of ant states
 int <lower = 1, upper = N_ant> ant_viab[N_viab]; // the list of ant species 
 int<lower=1> tot_viab[N_viab]; // number of trials
 int<lower=0> abort_viab[N_viab];
 vector[N_viab] vol_viab;	//size_t
 int<lower=1> N_Year_viab; //number of plots
  int<lower=1> N_Plot_viab; //number of years
  int<lower=1, upper=N_Plot_viab> plot_viab[N_viab]; // plot
  int<lower=1, upper=N_Year_viab> year_viab[N_viab]; // year
}
parameters {
  real beta0[N_ant]; // intercept of probability of viability for each bud
  real beta1[N_ant]; // slope of probability of viability for each bud

  //
  vector[N_Plot_viab] u; //subject intercepts
  vector[N_Year_viab] w; //item intercepts
  real < lower = 0 > sigma; // Error SD
  real < lower = 0 > sigma_u; // plot SD
  real < lower = 0 > sigma_w; // year SD
}
transformed parameters{
  real mu[N_viab]; // proportion viable for each plant?
  // Prediction for seed viability
  for(i in 1:N_viab){
    mu[i] = beta0[ant_viab[i]] + beta1[ant_viab[i]] * vol_viab[i] + u[plot_viab[i]] + w[year_viab[i]];
  }
}
model {
 // Priors
  beta0 ~ normal(0,100); // intercept distribution
  beta1 ~ normal(0,100); // intercept distribution
  //Model Statements
  u ~ normal(0, sigma_u); // plot random effects
  w ~ normal(0, sigma_w); // year random effects
  
  good_viab ~ binomial_logit(tot_viab, mu);
}
generated quantities {
  int<lower = 0> y_rep[N_viab] = binomial_rng(tot_viab , inv_logit(mu));
  real<lower = 0> mean_y_rep = mean(to_vector(y_rep));
  real<lower = 0> sd_y_rep = sd(to_vector(y_rep));
}


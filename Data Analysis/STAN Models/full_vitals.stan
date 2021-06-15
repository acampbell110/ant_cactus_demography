data {
  //All 
  int <lower = 1> N_ant; // number of ant states
  //Growth Predictors
  int <lower = 1> N_grow; // number of observations
  int <lower = 1, upper = N_ant> ant_grow[N_grow]; // the list of ant species 
  vector[N_grow] vol_grow;	//size_t
  vector[N_grow] y_grow; // size_t1
  int<lower=1> N_Year_grow; //number of plots
  int<lower=1> N_Plot_grow; //number of years
  int<lower=1, upper=N_Plot_grow> plot_grow[N_grow]; // plot
  int<lower=1, upper=N_Year_grow> year_grow[N_grow]; // year  
  //Survival Predictors
  int <lower = 1> N_surv; // number of observations
  int <lower = 1, upper = N_ant> ant_surv[N_surv]; // the list of ant species 
  vector[N_surv] vol_surv;	//size_t
  int <lower = 0, upper = 1> y_surv[N_surv]; // survival in year t1
  int<lower=1> N_Year_surv; //number of plots
  int<lower=1> N_Plot_surv; //number of years
  int<lower=1, upper=N_Plot_surv> plot_surv[N_surv]; // plot
  int<lower=1, upper=N_Year_surv> year_surv[N_surv]; // year
  //Flower Bud Predictions
  int <lower = 1> N_flower; // number of observations
  vector[N_flower] vol_flower;	//size_t
  int <lower = 1> y_flow[N_flower]; // number of flowers produced in year t (natural number)
  int<lower=1> N_Year_flower; //number of plots
  int<lower=1> N_Plot_flower; //number of years
  int<lower=1, upper=N_Plot_flower> plot_flower[N_flower]; // plot
  int<lower=1, upper=N_Year_flower> year_flower[N_flower]; // year
  //Repro Predictors
  int <lower = 1> N_repro; // number of observations
  vector[N_repro] vol1_repro;	//size_t
  int <lower = 0, upper = 1> y_repro[N_repro]; // survival in year t1
  int<lower=1> N_Year_repro; //number of plots
  int<lower=1> N_Plot_repro; //number of years
  int<lower=1, upper=N_Plot_repro> plot_repro[N_repro]; // plot
  int<lower=1, upper=N_Year_repro> year_repro[N_repro]; // year
  //Viability Predictors
  int < lower = 1 > N_viab; // Sample size
  int <lower = 0> good_viab[N_viab]; // Number of viable seeds
  int <lower = 1, upper = N_ant> ant_viab[N_viab]; // the list of ant species 
  int<lower=1> tot_viab[N_viab]; // number of trials
  int<lower=0> abort_viab[N_viab];
  int<lower=1> N_Year_viab; //number of plots
  int<lower=1> N_Plot_viab; //number of years
  int<lower=1, upper=N_Plot_viab> plot_viab[N_viab]; // plot
  int<lower=1, upper=N_Year_viab> year_viab[N_viab]; // year
  //Seed Prod Predictors
  int <lower = 1> N_seed; // number of observations
  int <lower = 2> N_ant_seed; // number of observations
  int <lower = 1, upper = N_ant_seed> ant_seed[N_seed]; // the list of ant species 
  int <lower = 0> seed[N_seed]; // survival in year t1
  //Seed Survival Predictors
  int <lower = 1> N_seed_s; // number of observations
  real <lower = 0, upper = 1> fr_prop[N_seed_s]; // survival in year t1
  int<lower=1> on_ground[N_seed_s];
  int<lower=1> on_plant[N_seed_s];
  //Germ yr 1 Predictors
  int <lower = 1> N_germ; // number of observations
  int y_germ1[N_germ]; // number of flowers produced in year t (natural number)
  int<lower=1> trials_germ1[N_germ]; // plot
  //Germ 2
  int y_germ2[N_germ]; // number of flowers produced in year t (natural number)
  int<lower=1> trials_germ2[N_germ]; // plot
  //Precensus
  int <lower = 1> N_precen; // number of observations
  vector[N_precen] vol_precen;	//size_t
  int <lower = 0, upper = 1> y_precen[N_precen]; // survival in year t1
  //Recruit
  int <lower = 1> N_rec; // number of observations
  vector[N_rec] y_rec; // size_t1
}
parameters {
  //Growth Predictors
  vector[N_ant] beta0_g; //intercept, unique to ant sp
  vector[N_ant] beta1_g; //slope, unique to ant sp
  vector[N_Plot_grow] u_g; //subject intercepts
  vector[N_Year_grow] w_g; //item intercepts
  real < lower = 0 > sigma_g; // Error SD
  real < lower = 0 > sigma_u_g; // plot SD
  real < lower = 0 > sigma_w_g; // year SD
  //Survival Predictors
  vector[N_ant] beta0_s; //intercept, unique to ant sp
  vector[N_ant] beta1_s; //slope, unique to ant sp
  vector[N_Plot_surv] u_s; //subject intercepts
  vector[N_Year_surv] w_s; //item intercepts
  real < lower = 0 > sigma_s; // Error SD
  real < lower = 0 > sigma_u_s; // plot SD
  real < lower = 0 > sigma_w_s; // year SD
  //Flowerbud Predictors 
  real < lower = 0> phi_f;
  real beta0_f; //intercept
  real beta1_f; //slope
  vector[N_Plot_flower] u_f; //subject intercepts
  vector[N_Year_flower] w_f; //item intercepts
  real < lower = 0 > sigma_f; // Error SD
  real < lower = 0 > sigma_u_f; // plot SD
  real < lower = 0 > sigma_w_f; // year SD
  //Repro Predictors
  real beta0_r; //intercept, unique to ant sp
  real beta1_r; //slope, unique to ant sp
  vector[N_Plot_repro] u_r; //subject intercepts
  vector[N_Year_repro] w_r; //item intercepts
  real < lower = 0 > sigma_r; // Error SD
  real < lower = 0 > sigma_u_r; // plot SD
  real < lower = 0 > sigma_w_r; // year SD
  //Viability Predictors
  real beta0_v[N_ant]; // intercept of probability of viability for each bud
  vector[N_Plot_viab] u_v; //subject intercepts
  vector[N_Year_viab] w_v; //item intercepts
  real < lower = 0 > sigma_v; // Error SD
  real < lower = 0 > sigma_u_v; // plot SD
  real < lower = 0 > sigma_w_v; // year SD
  //Seed Prod Predictors
  real < lower = 0> phi_seed;
  vector[N_ant_seed] beta0_seed; //intercept, unique to ant sp
  real < lower = 0 > sigma_seed; // Error SD
  //Seed Survival Predictors
  vector [N_seed_s] beta0_seed_s; //intercept, unique to ant sp
  real < lower = 0 > sigma_seed_s; // Error SD
  //Germ 1
  real < lower = 1> phi_germ1;
  real beta0_germ1; //intercept
  real beta1_germ1;
  real < lower = 0 > sigma_germ1; // Error SD
  //Germ 2
  real < lower = 1> phi_germ2;
  real beta0_germ2; //intercept
  real beta1_germ2;
  real < lower = 0 > sigma_germ2; // Error SD
  //Precensus
  real beta0_precen; //intercept, unique to ant sp
  real beta1_precen; //slope, unique to ant sp
  real < lower = 0 > sigma_precen; // Error SD
  // Recruit
  real beta0_rec; //intercept, unique to ant sp
  real < lower = 0 > sigma_rec; // Error SD
}
transformed parameters{
  //Mus
  vector[N_grow] mu_g; //linear predictor for the mean
  vector[N_surv] mu_s; //linear predictor for the mean
  vector[N_flower] mu_f; //linear predictor for the mean
  vector[N_repro] mu_r; //linear predictor for the mean
  real mu_v[N_viab]; // proportion viable for each plant?
  vector[N_seed] mu_seed; //linear predictor for the mean
  vector[N_seed_s] mu_seed_s; //linear predictor for the mean
  vector[N_germ] mu_germ1; //linear predictor for the mean
  vector[N_germ] mu_germ2; //linear predictor for the mean
  vector[N_precen] mu_precen; //linear predictor for the mean
  vector[N_rec] mu_rec; //linear predictor for the mean
  // Mu equations
  for(i in 1:N_grow){
    mu_g[i] = beta0_g[ant_grow[i]] + beta1_g[ant_grow[i]] * vol_grow[i] + u_g[plot_grow[i]] + w_g[year_grow[i]];
  };
  for(i in 1:N_surv){
    mu_s[i] = beta0_s[ant_surv[i]] + beta1_s[ant_surv[i]] * vol_surv[i] + u_s[plot_surv[i]] + w_s[year_surv[i]];
  };
  for(i in 1:N_flower){
   	mu_f[i] = beta0_f + beta1_f * vol_flower[i] + u_f[plot_flower[i]] + w_f[year_flower[i]];
  };
  for(i in 1:N_repro){
    mu_r[i] = beta0_r + beta1_r * vol1_repro[i] + u_r[plot_repro[i]] + w_r[year_repro[i]];
  };
  for(i in 1:N_viab){
    mu_v[i] = beta0_v[ant_viab[i]] + u_v[plot_viab[i]] + w_v[year_viab[i]];
  };
  for(i in 1:N_seed){
   	mu_seed[i] = beta0_seed[ant_seed[i]];
  };
  for(i in 1:N_seed_s){
    mu_seed_s[i] = beta0_seed_s[i];
  }
  for(i in 1:N_germ){
   	mu_germ1[i] = beta0_germ1 + beta1_germ1 * trials_germ1[i];
  }
  for(i in 1:N_germ){
   	mu_germ2[i] = beta0_germ2 + beta1_germ2 * trials_germ2[i];
  }
  for(i in 1:N_precen){
    mu_precen[i] = beta0_precen + beta1_precen * vol_precen[i];
  }
  for(i in 1:N_rec){
    mu_rec[i] = beta0_rec;
  }
}
model {
  //Growth Predictors
  u_g ~ normal(0, sigma_u_g); // plot random effects
  w_g ~ normal(0, sigma_w_g); // year random effects
  beta0_g ~ normal(0,100); // intercept distribution
  beta1_g ~ normal(0,100); // slope distribution
  for(i in 1:N_grow){
    y_grow[i] ~ normal(mu_g[i], sigma_g);
  };
  //Survival Predictors
  u_s ~ normal(0, sigma_u_s); // plot random effects
  w_s ~ normal(0, sigma_w_s); // year random effects
  beta0_s ~ normal(0,100); // intercept distribution
  beta1_s ~ normal(0,100); // slope distribution
  for(i in 1:N_surv){
    y_surv[i] ~ bernoulli_logit(mu_s[i]);
  };
  // Flowerbud Predictors
  u_f ~ normal(0, sigma_u_f); // plot random effects
  w_f ~ normal(0, sigma_w_f); // year random effects
  beta0_f ~ normal(0,100); // intercept distribution
  beta1_f ~ normal(0,100); // slope distribution
  for(i in 1:N_flower){
    y_flow[i] ~ neg_binomial_2(exp(mu_f[i]), phi_f);
  };
  //Repro Predictors
  u_r ~ normal(0, sigma_u_r); // plot random effects
  w_r ~ normal(0, sigma_w_r); // year random effects
  beta0_r ~ normal(0,100); // intercept distribution
  beta1_r ~ normal(0,100); // slope distribution
 for(i in 1:N_repro){
  y_repro[i] ~ bernoulli_logit(mu_r[i]);
 };
  //Viability Predictors
  beta0_v ~ normal(0,100); // intercept distribution
  u_v ~ normal(0, sigma_u_v); // plot random effects
  w_v ~ normal(0, sigma_w_v); // year random effects
  good_viab ~ binomial_logit(tot_viab, mu_v);
  //Seed Predictors
  beta0_seed ~ normal(0,100); // intercept distribution
  for(i in 1:N_seed){
    seed[i] ~ neg_binomial_2(exp(mu_seed[i]), phi_seed);
  };
  //Seed Survival Predictors
  beta0_seed_s ~ normal(0,100); // intercept distribution
  //Model
   on_ground ~ binomial_logit(on_plant,mu_seed_s);
  //Germ 1
  beta0_germ1 ~ normal(0,100); // intercept distribution
  for(i in 1:N_germ){
    y_germ1[i] ~ binomial_logit(trials_germ1[i], (mu_germ1[i]));
  }
  //Germ 2
  beta0_germ2 ~ normal(0,100); // intercept distribution
  for(i in 1:N_germ){
    y_germ2[i] ~ binomial_logit(trials_germ2[i], (mu_germ2[i]));
  }
  //Precensus
  beta0_precen ~ normal(0,100); // intercept distribution
  beta1_precen ~ normal(0,100); // slope distribution
  for(i in 1:N_precen){
  y_precen[i] ~ bernoulli_logit(mu_precen[i]);
 }
 //Recruit
   beta0_rec ~ normal(0,100); // intercept distribution
  for(i in 1:N_rec){
    y_rec[i] ~ normal(mu_rec[i], sigma_rec);
  }
}




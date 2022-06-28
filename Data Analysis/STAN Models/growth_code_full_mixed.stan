/// Stan model for simple ant growth regression
data {
  int <lower = 1> N;                          // number of observations
  int <lower = 1> K;                          // number of ant states
  int <lower = 1, upper = K> ant[N];          // the list of ant species 
  vector[N] vol;	                            //size_t
  vector[N] y_grow;                           // survival in year t1
  // Random Effects
  int<lower=1> N_Year;                        //number of plots
  int<lower=1> N_Plot;                        //number of years
  int<lower=1, upper=N_Plot> plot[N];         // plot
  int<lower=1, upper=N_Year> year[N];         // year
}

parameters {
  // intercept and slope params
  vector[K] beta0;                           //ant beta
  vector[K] beta1;                           //interaction beta
  // sd variation
  real d_0;                             // Error intercept
  real d_size;                          // Error size
  // random effect params
  vector[N_Plot] u;                          //subject intercepts
  vector[N_Year] w;                          //item intercepts
  matrix[K,N_Year] beta_yr;
  real < lower = 0 > sigma_u;                // plot SD
  real < lower = 0 > sigma_w;                // year SD
}

transformed parameters{
  vector[N] mu;                             //linear predictor for the mean
  matrix[N,N_Year] mu_year;
  vector[N] sigma;                          // linear predictor for the sd
  // Predictor Equations
  //for(i in 1:N){
  //  mu[i] = beta0[ant[i]] + beta1[ant[i]] * vol[i] + u[plot[i]] + w[year[ant[i]]];
  //  sigma[i] = exp(d_0 + d_size * vol[i]);
  //}
  for(a in 1:K){
    for(b in 1:N_Year){
      beta_yr[a,b] = 
    }
  }
  for(i in 1:N){ ## Cycle through each individual
    for(j in 1:N_Year){ ## Cycle through each year -> Expect a year output for every ant species
      mu_year[i,j] = beta0[ant[i]] + beta1[ant[i]]*vol[i] + u[plot[i]];
    }
  }
}



model {
//Priors
 u ~ normal(0, sigma_u);                   // plot random effects
 w ~ normal(0, sigma_w);                   // year random effects
 beta0 ~ normal(0,100);                    // mu intercept 
 beta1 ~ normal(0,100);                    // mu slope
 d_0 ~ normal(0, 100);                     // sigma intercept
 d_size ~ normal(0, 100);                  // sigma slope
 //Model
 for(i in 1:N){
 y_grow[i] ~ normal(mu[i],sigma[i]);
 }
}






setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Data Analysis")

## Cat model
write("data {
  int<lower = 1> N_data;
  int<lower = 1, upper = 5> w_ans[N_data];
}
parameters {
  simplex[5] theta;
}
model {
  target += dirichlet_lpdf(theta | rep_vector(2, 5));
  for(n in 1:N_data)
    target += categorical_lpmf(w_ans[n] | theta);
}
generated quantities{
  int pred_w_ans[N_data];
  for(n in 1:N_data)
    pred_w_ans[n] = categorical_rng(theta);
}",
"STAN Models/multi_prac2.stan")

N_data <- 100 
ans_cat <-rcat(N_data, prob = as.matrix(true_theta))

data_cat <-  list(N_data = N_data,
                  w_ans = ans_cat)
str(data_cat)
fit_cat <- stan(file = "STAN Models/multi_prac2.stan", 
               data = data_cat, warmup = 50, iter = 100, chains = 3, cores = 2, thin = 1)
          

## Cat model
write("data {
  int<lower = 1> N_data;
  int<lower = 1, upper = 5> ant_data[N_data]; // list of ant species (year t)
  int<lower = 1, upper = 5> ant1_data[N_data]; // the list of ant species (year t+1)

}
parameters {
  simplex[5] theta; // this is the mu
}
model {
  for(i in 1:N_data)
    //mu = theta[]
    target += dirichlet_lpdf(theta | rep_vector(2, 5));
  for(n in 1:N_data)
    target += categorical_lpmf(ant1_data[n] | theta);
}
generated quantities{
  int pred_ant[N_data];
  for(n in 1:N_data)
    pred_ant[n] = categorical_rng(theta);
}",
"STAN Models/multi_prac2.stan")
stanc("STAN Models/multi_prac2.stan")

ant_cat <-rcat(N_data, prob = as.matrix(true_theta))
ant_data 

data_cat <-  list(N_data = N_data,
                  ant_data = ant_data)
str(data_cat)
fit_cat <- stan(file = "STAN Models/multi_prac2.stan", 
                data = data_cat, warmup = 50, iter = 100, chains = 3, cores = 2, thin = 1)

y <- ant_data
yrep_multi <- rstan::extract(fit_cat, pars = "pred_w_ans")[["pred_w_ans"]]
samp100 <- sample(nrow(yrep_multi), 50)
bayesplot::ppc_dens_overlay(y, yrep_multi[samp100,])




## Alternate Method
library(car)
head(Prestige)
write("data{
  int N; 
  int Ntype;
  int type[N]; // type will be coded as an integer from 1 to 4
  vector[N] vol;
  vector[N] new_type;
} 
parameters{
  real m_new_type;
  real b_vol;
  vector[Ntype] u_type;
  real<lower=0> sigma;
}
transformed parameters{
  vector[Ntype] m_type;
  m_type = m_new_type + u_type;
}
model{
  // uniform on m_new_type, b_vol sigma
  u_type ~ normal(0,100);  // proper prior on deviations
  // -- a proper Bayesian hierarchical model for
  // type would use a hyperparameter instead of 100
  // and the hyperparameter would help determine
  // appropriate amount of pooling between types
  new_type ~ normal(
    m_new_type + 
      u_type[type] +     // note how this works using array indexing
    // -- a key technique for hierarchical modeling
    b_vol * vol,
    sigma);
}
generated quantities {
  real y_rep[N] = normal_rng(m_new_type + u_type[type] +  b_vol * vol,sigma);
}
","STAN Models/multi_prac3.stan")

dat <- list( N = N_data,
             Ntype = length(unique(data$ant)),
             type = as.numeric(as.factor(data$ant)), # to ensure integers from 1 to 3
             vol = vol_data,
             new_type = data$ant1)

multi_stan <- stan("STAN Models/multi_prac3.stan", data = dat, cores = 2, chains = 1, thin = 1, iter = 100)

y <- data$ant1
yrep_multi <- rstan::extract(prestige.stanfit, pars = "y_rep")[["y_rep"]]
samp100 <- sample(nrow(yrep_multi), 5)

write("data{
  int N; 
  int Ntype;
  int type[N]; // type will be coded as an integer from 1 to 3
  vector[N] women;
  vector[N] prestige;
} 
parameters{
  real m_prestige;
  real b_women;
  vector[Ntype] u_type;
  real<lower=0> sigma;
}
transformed parameters{
  vector[Ntype] m_type;
  m_type = m_prestige + u_type;
}
model{
  // uniform on m_prestige, b_women sigma
  u_type ~ normal(0,100);  // proper prior on deviations
  // -- a proper Bayesian hierarchical model for
  // type would use a hyperparameter instead of 100
  // and the hyperparameter would help determine
  // appropriate amount of pooling between types
  prestige ~ normal(
    m_prestige + 
      u_type[type] +     // note how this works using array indexing
    // -- a key technique for hierarchical modeling
    b_women * women,
    sigma);
}
","STAN Models/multi_prac3.stan")


###
b <- data[,c("volume_t","ant")]
x <- as.matrix(b)
K <- N_ant
N <- N_data
D <- 2
y <- ant1_data
Stan_Data <- list(K = K,
                  N = N,
                  D = D,
                  y = y,
                  x = x)
write("data {
  int K;
  int N;
  int D;
  int y[N];
  matrix[N, D] x;
}
parameters {
  matrix[D, K] beta;
}
model {
  matrix[N, K] x_beta = x * beta;
  
  to_vector(beta) ~ normal(0, 2);
  
  for (n in 1:N)
    y[n] ~ categorical_logit(x_beta[n]);
}",
"STAN Models/multi_prac4.stan")

fitted <- stan(file = "STAN Models/multi_prac4.stan", 
                data = data_cat, warmup = 50, iter = 100, chains = 3, cores = 2, thin = 1)

stanc("STAN Models/multi_prac4.stan"
)

compiled_model <- stan_model("categorical_model.stan")

sim_out <- sampling(compiled_model, data = list(N = 1000, 
                                                P = 5, 
                                                # Y should be real but fake data if we're simulating
                                                # new Y
                                                y = sample(1:2, 1000, replace = T), 
                                                run_estimation = 0,
                                                prior_sd = 100))

library(dplyr)

fake_data_matrix  <- sim_out %>% 
  as.data.frame %>% 
  select(contains("y_sim"))

summary_tbl <- apply(fake_data_matrix[1:5,], 1, summary)

write("data {
  int N;
  int P; // number of categories to be estimated
  int y[N]; // outcomes
  int<lower = 0, upper = 1> run_estimation; // a switch to evaluate the likelihood
  real<lower = 0> prior_sd; // standard deviation of the prior on theta
}
parameters {
  vector[P-1] theta_raw;
}
transformed parameters {
  vector[P] theta;
  theta[1] = 0.0;
  theta[2:P] = theta_raw;
}
model {
  // prior
  theta_raw ~ normal(0, prior_sd);
  
  // likelihood, which we only evaluate conditionally
  if(run_estimation==1){
    y ~ categorical(softmax(theta));
  }
}
generated quantities {
  vector[N] y_sim;
  for(i in 1:N) {
    y_sim[i] = categorical_rng(softmax(theta));
  }
}",
"STAN Models/multi_prac5.stan")
dat <- list(N = N_data,
            P = 4,
            y = data$ant1,
            run_estimation = 1,
            prior_sd = 100)

fitted <- stan(file = "STAN Models/multi_prac5.stan", 
               data = dat, warmup = 50, iter = 100, chains = 3, cores = 2, thin = 1)

y <- data$ant1
yrep_multi <- rstan::extract(fitted, pars = "y_sim")[["y_sim"]]
samp100 <- sample(nrow(yrep_multi), 50)
bayesplot::ppc_dens_overlay(y, yrep_multi[samp100,])

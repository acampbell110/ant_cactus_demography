setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

##############################################################################################################
##http://eleafeit.com/post/mnl_identification_stan/
generate_mnl_data <- function(N=1000, K=1, J=4 ,beta=c(1), alpha=c(1,0,-1, 0)){
  if(length(beta) != K) stop ("incorrect number of parameters")
  Y <- rep(NA, N)
  X <- list(NULL) 
  Fac <- list(NULL)
  Mat <- list(NULL)
  for (i in 1:N) {
    X[[i]] <- matrix(rnorm(J*K), ncol=K)
    Fac[[i]] <- matrix(sample(1:4,4), ncol = 1)
    Mat[[i]] <- cbind(X[[i]],Fac[[i]])
    Y[i] <- sample(x=J, size=1, prob=exp(alpha+X[[i]]%*%beta))
  }
  list(N=N, J=J, K=K, Y=Y, X=X, beta=beta, alpha=alpha, Mat = Mat, Fac = Fac)
}
d0 <- generate_mnl_data()
## The attributes are presence of ant state 1,2,3,4, alternates are the volume of the cacti
## The attributes are ant state and volume, the alternates for ant state are 1,2,3,4, the attributes for volume are inf
write("
data {
    int<lower=2> J; // of alternatives/outcomes (ant state) -- 4
    //int<lower=2> G; // of alternatives/outcomes (volume) -- inf
    int<lower=1> N; // of observations utilities (number of data points)
    int<lower=1> K; // of covariates (volume & ant state)
    int<lower=0,upper=J> Y[N];
    matrix[J,K] X[N];
    int<lower = 0, upper = J> Mat[N];
}

parameters {
    vector[J-1] alpha_raw; // unconstrained UPC intercepts
    vector[J] beta1;
}

transformed parameters{
  vector[J] alpha; 
  alpha = append_row(-sum(alpha_raw), alpha_raw); // sum to zero constraint
}

model {
    for (i in 1:N)
        Y[i] ~ categorical_logit(alpha + X[i]*beta1);
}
generated quantities{
  int Y_rep[N];
  for (i in 1:N)
         Y_rep[i]= categorical_logit_rng(alpha + X[i]*beta1);
}","Data Analysis/STAN Models/multi_1.stan")

d2 <- generate_mnl_data(N=1000, K=2, J=4 ,beta=c(1, -2), alpha=c(1,0,-1, 0))
p6 <- stan("Data Analysis/STAN Models/multi_1.stan", data=d2, iter=1000, chains=2, seed=19730715)
shifted_alpha = d2$alpha - (1/d2$J)*sum(d2$alpha)
bayesplot::mcmc_recover_hist(As.mcmc.list(p6, pars="alpha"), true=shifted_alpha)
bayesplot::mcmc_recover_hist(As.mcmc.list(p6, pars="beta"), true=d2$beta)
bayesplot::mcmc_trace(As.mcmc.list(p6, pars="alpha"))
bayesplot::mcmc_trace(As.mcmc.list(p6, pars="beta"))

y <- d0$Y
yrep_multi <- rstan::extract(p6, pars = "Y_rep")[["Y_rep"]]
samp100 <- sample(nrow(yrep_multi), 500)

bayesplot::ppc_dens_overlay_grouped(y, yrep_multi[samp100,], x = d0$X, group = d0$beta)
png("Figures/multi_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_multi[samp100,])
dev.off()


generate_mnl_data <- function(N=1000, K=2, J=4 ,beta=c(1,0), alpha=c(1,0,-1, 0)){
  if(length(beta) != K) stop ("incorrect number of parameters")
  Y <- rep(NA, N)
  X <- list(NULL) 
  for (i in 1:N) {
    X[[i]] <- matrix(rnorm(J*K), ncol=K)
    Y[i] <- sample(x=J, size=1, prob=exp(alpha+X[[i]]%*%beta))
  }
  list(N=N, J=J, K=K, Y=Y, X=X, beta=beta, alpha=alpha)
}
d0 <- generate_mnl_data()
## The attributes are ant state and volume, the alternates for ant state are 1,2,3,4, the attributes for volume are inf
write("
data {
    int<lower=2> J; // of alternatives/outcomes (ant state) -- 4
    //int<lower=2> G; // of alternatives/outcomes (volume) -- inf
    int<lower=1> N; // of observations utilities (number of data points)
    int<lower=1> K; // of covariates (volume & ant state)
    int<lower=0,upper=J> Y[N];
    real cont[N];
    matrix[J,K] X[N];
}

parameters {
    vector[J-1] alpha_raw; // unconstrained UPC intercepts
    vector[K] beta;
}

transformed parameters{
  vector[J] alpha; 
  alpha = append_row(-sum(alpha_raw), alpha_raw); // sum to zero constraint
}

model {
    for (i in 1:N)
        Y[i] ~ categorical_logit(alpha + X[i]*beta);
}","Data Analysis/STAN Models/multi_1.stan")

d2 <- generate_mnl_data(N=1000, K=4, J=1 ,beta=c(1,2,3,4), alpha=c(1,0,-1, 0))
p6 <- stan("STAN Models/multi_1.stan", data=d2, iter=10, chains=2, seed=19730715)
shifted_alpha = d2$alpha - (1/d2$J)*sum(d2$alpha)
bayesplot::mcmc_recover_hist(As.mcmc.list(p6, pars="alpha"), true=shifted_alpha)
bayesplot::mcmc_recover_hist(As.mcmc.list(p6, pars="beta"), true=d2$beta)
bayesplot::mcmc_trace(As.mcmc.list(p6, pars="alpha"))
bayesplot::mcmc_trace(As.mcmc.list(p6, pars="beta"))












#############################################################################
## https://vasishth.github.io/bayescogsci/book/modeling-multiple-categorical-responses.html
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
"Data Analysis/STAN Models/multi_prac2.stan")

N_data <- 100 
ans_cat <-rcat(N_data, prob = as.matrix(true_theta))

data_cat <-  list(N_data = N_data,
                  w_ans = ans_cat)
str(data_cat)
fit_cat <- stan(file = "STAN Models/multi_prac2.stan", 
               data = data_cat, warmup = 50, iter = 100, chains = 3, cores = 2, thin = 1)

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




#############################################################################
## http://blackwell.math.yorku.ca/MATH6635/files/Stan_first_examples.html#categorical-predictors--
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
","Data Analysis/STAN Models/multi_prac3.stan")

dat <- list( N = N_data,
             Ntype = length(unique(data$ant)),
             type = as.numeric(as.factor(data$ant)), # to ensure integers from 1 to 3
             vol = vol_data,
             new_type = data$ant1)

multi_stan <- stan("STAN Models/multi_prac3.stan", data = dat, cores = 2, chains = 1, thin = 1, iter = 100)


################################################################################
## https://medium.com/swlh/multi-logistic-regression-with-probabilistic-programming-db9a24467c0d
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

stanc("Data Analysis/STAN Models/multi_prac4.stan"
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

##############################################################################################
## https://khakieconomics.github.io/2017/04/30/An-easy-way-to-simulate-fake-data-in-stan.html
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
"Data Analysis/STAN Models/multi_prac5.stan")
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


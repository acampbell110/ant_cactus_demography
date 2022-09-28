// saved as mixed_conditional_plus_mnp_logit.stan
data {
  int<lower=0> N; // number of rows
  int<lower=1> D; // number of covariates that vary by choice
  int K; // number of choices
  int<lower=1, upper=K> y[N];
  // Variables I don't understand
  int T; // number of inidvidual-choice sets/task combinations
  int I; // number of Individuals
  int P2; // number of covariates that vary by individual
  
  vector<lower = 0, upper = 1>[N] choice; // binary indicator for choice
  matrix[N, D] X; // choice attributes
  matrix[I, P2] X2; // individual attributes
  
  int task[T]; // index for tasks
  int task_individual[T]; // index for individual
  int start[T]; // the starting observation for each task
  int end[T]; // the ending observation for each task
}
parameters {
  vector[D] beta; // hypermeans of the part-worths
  matrix[K-1, P2] Gamma_raw; // coefficient matrix on individual attributes
  vector<lower = 0>[D] tau; // diagonal of the part-worth covariance matrix
  matrix[I, D] z; // individual random effects (unscaled)
  cholesky_factor_corr[D] L_Omega; // the cholesky factor of the correlation matrix of tastes/part-worths
}
transformed parameters {
  // here we use the reparameterization discussed on slide 30
  matrix[I, D] beta_individual = rep_matrix(beta', I) + z*diag_pre_multiply(tau, L_Omega);
  matrix[K, P2] Gamma; // we set the final row to zero for identification
  Gamma[1:(K-1), :] = Gamma_raw;
  Gamma[K,:] = rep_row_vector(0.0, P2);
}
model {
  // create a temporary holding vector
  vector[N] log_prob;
  
  // priors on the parameters
  tau ~ normal(0, .5);
  beta ~ normal(0, .5);
  to_vector(z) ~ normal(0, 1);
  L_Omega ~ lkj_corr_cholesky(4);
  to_vector(Gamma_raw) ~ normal(0, 1);
  
  // log probabilities of each choice in the dataset
  for(t in 1:T) {
    vector[K] utilities; // tmp vector holding the utilities for the task/individual combination
    // add utility from product attributes with individual part-worths/marginal utilities
    utilities = X[start[t]:end[t]]*beta_individual[task_individual[t]]';
    // Add the utilty from the individual attributes for each choice
    utilities += Gamma * X2[task_individual[t]]';  
    
    log_prob[start[t]:end[t]] = log(softmax(utilities));
  }
  
  // use the likelihood derivation on slide 29
  target += log_prob' * choice;
}
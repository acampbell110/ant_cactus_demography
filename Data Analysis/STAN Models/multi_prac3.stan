data{
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


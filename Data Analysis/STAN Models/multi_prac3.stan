data{
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


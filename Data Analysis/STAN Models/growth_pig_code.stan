functions {
  real sgt_log(real x, real mu, real s, real l, real p, real q) {
    // Skewed generalised t
        // Accounts for: Skew (s), (l), (p), (q)
    //int N;
    real lz1;
    real lz2;
    real v;
    real m;
    real r;
    real out;
    //N = dims(x)[1];
    lz1=lbeta(1.0/p,q);
    lz2=lbeta(2.0/p,q-1.0/p);
    v=q^(-1.0/p)*((3*l^2+1)*exp(lbeta(3.0/p,q-2.0/p)-lz1)-4*l^2*exp(lz2-lz1)^2)^(-0.5);
    m=2*v*s*l*q^(1.0/p)*exp(lz2-lz1);
    out=0;
    //for (n in 1:N) {
      r=x-mu+m;
      if (r<0)
      	     //out=out+log(p)-log(2*v*s*q^(1.0/p)*exp(lz1)*(fabs(r)^p /(q*(v*s)^p*(l*(-1)+1)^p)+1)^(1.0/p+q));
      	     out=log(p)-log(2*v*s*q^(1.0/p)*exp(lz1)*(fabs(r)^p /(q*(v*s)^p*(l*(-1)+1)^p)+1)^(1.0/p+q));
      else
      	     //out=out+log(p)-log(2*v*s*q^(1.0/p)*exp(lz1)*(fabs(r)^p /(q*(v*s)^p*(l*(1)+1)^p)+1)^(1.0/p+q));
      	     out=log(p)-log(2*v*s*q^(1.0/p)*exp(lz1)*(fabs(r)^p /(q*(v*s)^p*(l*(1)+1)^p)+1)^(1.0/p+q));
    //}
    return out;
  }
}
data {
  int<lower=1> N;
  real vol[N];
  real delta_size[N];
  int<lower=1> N_plot;
  int<lower=1> N_year;
  int<lower=1, upper=N_plot> plot[N];
  int<lower=1, upper=N_year> year[N];
}
parameters {
  //real mu; 
  //real<lower=0> sigma; 
  real b_0;
  real b_size;
  real<lower=0> sigma_plot;
  real<lower=0> sigma_year;
  real plot_rfx[N_plot];
  real year_rfx[N_year];
  real d_0;
  real d_size;
  real<lower=-0.99,upper=0.99> l; 
  real<lower=0.01> p; 
  real<lower=2/p> q; 
}
transformed parameters{
  real cholla_pred[N];
  real<lower=0> cholla_sd[N];
  for(i in 1:N){
    cholla_pred[i] = b_0 + b_size * vol[i] + plot_rfx[plot[i]] + year_rfx[year[i]];
    cholla_sd[i] = exp(d_0 + d_size * vol[i]);
  }
}
model {
  b_0 ~ normal(0, 100);    
  b_size ~ normal(0, 100); 
  d_0 ~ normal(0, 100);    
  d_size ~ normal(0, 100); 
  //sigma ~ inv_gamma(0.001, 0.001);
  sigma_plot ~ inv_gamma(0.001, 0.001);
  for(i in 1:N_plot){
    plot_rfx[i]~normal(0,sigma_plot);
  }
  sigma_year ~ inv_gamma(0.001, 0.001);
  for(i in 1:N_year){
    year_rfx[i]~normal(0,sigma_year);
  }
  
  l ~ uniform(-0.99,0.99);
  p ~ inv_gamma(0.001, 0.001);
  q ~ inv_gamma(0.001, 0.001);
  
  for(i in 1:N){
  delta_size[i] ~ sgt(cholla_pred[i], cholla_sd[i], l, p, q);
  }
}


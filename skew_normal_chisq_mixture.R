#see https://discourse.mc-stan.org/t/skewed-distribution/1383/15

library(rstan)
scode <- "
data {
  real<lower=1> nu;
  real alpha;
}
transformed data {
  real sqrt_nu = sqrt(nu);
}
parameters {
  real<lower=0> V;
  real Z;
}
transformed parameters {
  real T = Z * sqrt(V) * sqrt_nu;
}
model {
  V ~ inv_chi_square(nu); 
  Z ~ skew_normal(0, 1, alpha);
 }
"

foo_data <- list(nu = 4, alpha = 1)

foo <- stan(model_code = scode, data = foo_data, chains = 1, iter = 4000, control=list(adapt_delta=0.8))
T <- extract(foo)$T

###############################################
library(extraDistr)
x<-runif(1000,0,100)
beta0<--2
beta1<-0.8
y<-rlst(n=1000,df = 6,mu=beta0+beta1*x,sigma = 8)
hist(y)
plot(x,y)

toycode <- "
data {
  int <lower = 1> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real alpha;
  real beta0;
  real beta1;
  real<lower=0> tausq;
  real<lower=1> nu;
}
transformed parameters {
  vector[N] xi;
  vector[N] omega;

  for(i in 1:N){
  xi[i] = beta0 + beta1 * x[i];
  }
}
model {
  beta0 ~ normal(0,5);
  beta1 ~ normal(0,5);
  alpha ~ normal(0,5);

  omega ~ scaled_inv_chi_square(nu, tausq);
  y ~ skew_normal(xi, omega, alpha);
 }
"
toy_data <- list(x = x, y = y, N = length(y))
toy_fit <- stan(model_code = toycode, data = toy_data, chains = 1, iter = 400, control=list(adapt_delta=0.99))

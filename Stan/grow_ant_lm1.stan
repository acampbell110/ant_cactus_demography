// Stan model for simple linear regression

data {
 int < lower = 1 > N; // Sample size
 int < lower = 1 > K; // Number of Predictors (Ant Species)
 real < lower = 0 > vol[N]; // volume
 int < lower = 0 > ant[K]; // ants
 real < lower = 0 > y[N]; // outcome vector
}

parameters {
 real alpha; // Intercept
 vector[K] beta; // coefficients for predictors
 real < lower = 0 > sigma; // Error SD
}

transformed parameters{
real mu[N];
for(i in 1:N){
mu[i] = beta[1] + beta[2]*vol[i] + beta[3]*ant[i]; 
             }
}

model {
// Priors
alpha ~ normal(0,100);
beta ~ normal(0,100); 
//sigma ~ exponential(0,0);
 y ~  normal(mu, sigma);   // The Model
}

generated quantities {
} // The posterior predictive distribution

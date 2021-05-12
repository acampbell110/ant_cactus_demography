data {
int K; //alternatives
int N; //number of trials
int D; //number of predictors (1)
int<lower=1> N_Year; //number of plots
int<lower=1> N_Plot; //number of years
int<lower=1, upper=N_Plot> plot[N]; // plot
int<lower=1, upper=N_Year> year[N]; // year
int y[N];
matrix[N, D] x;
}
parameters {
matrix[D, K] beta;
vector[N_Plot] u; 
vector[N_Year] w; 
real < lower = 0 > sigma_u; // plot SD
real < lower = 0 > sigma_w; // year SD
}
model {
	matrix[N, K] x_beta = x * beta;
for(i in 1:N){
	x_beta[i,K] = x_beta[i,K] + u[plot[i]] + w[year[i]];
}
u ~ normal(0, sigma_u); // plot random effects
w ~ normal(0, sigma_w); // year random effects
to_vector(beta) ~ normal(0, 5);
for (n in 1:N)
y[n] ~ categorical_logit(x_beta[n]');
}

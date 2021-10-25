// https://mc-stan.org/docs/2_20/stan-users-guide/multi-logit-section.html
// http://eleafeit.com/post/mnl_identification_stan/ MLN Fix-one-to-zzero ex
// This includes an intercept which uses a reference/baseline variable

data {
int K; //alternatives
int N; //number of trials
int D; //number of predictors (1)
//int<lower=1> N_Year; //number of plots
//int<lower=1> N_Plot; //number of years
//int<lower=1, upper=N_Plot> plot[N]; // plot
//int<lower=1, upper=N_Year> year[N]; // year
int Y[N];
matrix[N, D] X;
}
parameters {
vector[K-1] alpha_raw;
matrix[D, K] beta1;
//vector[N_Plot] u; 
//vector[N_Year] w; 
//real < lower = 0 > sigma_u; // plot SD
//real < lower = 0 > sigma_w; // year SD
}
transformed parameters {
    vector[K] alpha; 
    alpha = append_row(0, alpha_raw); 
}
model {
	matrix[N, K] x_beta = X * beta1;
for(i in 1:N){
	x_beta[i,K] = x_beta[i,K];// + u[plot[i]] + w[year[i]];
}
//u ~ normal(0, sigma_u); // plot random effects
//w ~ normal(0, sigma_w); // year random effects
to_vector(beta1) ~ normal(0, 5);
for (n in 1:N)
Y[n] ~ categorical_logit(alpha + x_beta[n]');
}


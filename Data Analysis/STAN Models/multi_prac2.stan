data {
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
}

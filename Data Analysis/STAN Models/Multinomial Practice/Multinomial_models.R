setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Data Analysis/STAN Models/Multinomial Practice")

################## Fake Data Practice
N <- 1000 ## Number of Observations
K <- 4 ## Number of Alternatives
D <- 1 ## Covariates
X <- matrix(rep(NA, D*1000), ncol = D) ## Variables
Y <- rep(NA, 1000)
beta <- c(1,2,3,4)
for(i in 1:N){
  X[i,1] <- rnorm(1, mean = 20, sd = 3)
  #X[i,2] <- sample(4,1,replace = TRUE, prob = exp(beta))
  Y[i] <- sample(4,1,replace = TRUE, prob = exp(beta))
}
stan_data <- list(N = N, X = X, Y = Y, D = D, K = K)

mod_data <- stan(file = "multi_3.stan",data = stan_data, warmup = 5, iter = 10, chains = 3, cores = 2, thin = 1)
mod_data_outputs <- rstan::extract(mod_data, pars = c("beta1","alpha"))
write.csv(mod_data_outputs, "mod_data_outputs.csv")
mod_data_outputs <- read.csv("mod_data_outputs.csv")


summary(mod_data)
plot(mod_data, pars = c("beta1","alpha"))


#################### Include Ant State as a predictor
multi_data3 <- cactus[,c("ant_t1","volume_t","Year_t","Plot", "ant_t")]
multi_data3 <- na.omit(multi_data3)
multi_data3$ant_stat <- as.numeric(as.factor(multi_data3$ant_t1))
multi_data3$ant_last <- as.numeric(as.factor(multi_data3$ant_t))
multi_data3$Plot <- as.integer(as.factor(multi_data3$Plot))
multi_data3$Year_t <- as.integer(as.factor(multi_data3$Year_t))
x_mat <- matrix(log(multi_data3$volume_t))
x_mat <- cbind(x_mat, multi_data3$ant_last)
stan_data_mod3 <- list(D = 2,
                       K = 4,
                       N = nrow(multi_data3),
                       x = x_mat,
                       ant = multi_data3$ant_last,
                       y = multi_data3$ant_stat, 
                       N_Plot = max(unique(multi_data3$Plot)),
                       N_Year = max(unique(multi_data3$Year_t)),
                       plot = multi_data3$Plot,
                       year = multi_data3$Year_t,
                       N_ants = 4)
mod3_data <- stan(file = "multi_3.stan",data = stan_data_mod3, warmup = 5, iter = 10, chains = 3, cores = 2, thin = 1)
mod3_data_outputs <- rstan::extract(mod3_data, pars = c("beta1","alpha"))
write.csv(mod3_data_outputs, "mod3_data_outputs.csv")
mod3_data_outputs <- read.csv("mod3_data_outputs.csv")


summary(mod3_data)
plot(mod3_data, pars = c("beta1","alpha"))
x_dum <- seq(min(log(multi_data3$volume_t)), max(log(multi_data3$volume_t)), by = 1)
plot(density(mod3_data_outputs$beta1.1.1*x_dum + mod3_data_outputs$beta1.2.1*z_dum1 + mod3_data_outputs$alpha.1))



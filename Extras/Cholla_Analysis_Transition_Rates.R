######################################################################################################
#### Binomial transitions between vacant and ant #####################################################
######################################################################################################
cactus <- read.csv("cholla_demography_20042019_cleaned.csv", header = TRUE,stringsAsFactors=T)
## 2 ant species
binom_ant <- cactus[,c("occ_t", "occ_t1", "logsize_t")]
binom_ant <- na.omit(binom_ant)
nrow(binom_ant)
# check that you're happy with the subsetting
plot(((as.numeric(as.factor(binom_ant$occ_t1)))-1), ylim = c(0,1))
points(((as.numeric(as.factor(cactus$occ_t1)))-1), col = "red")

## Create Stan Data
stan_data_binom <- list(N = nrow(binom_ant), ## number of observations
                        vol = binom_ant$logsize_t, ## predictors volume
                        y_binom = as.integer(as.factor(binom_ant$occ_t1))-1, ## response survival next year
                        ant = as.integer(as.factor(binom_ant$occ_t)), ## ant predictors
                        K = 2 ## number of ant states
)

## Run the model 
fit_binom <- stan(file = "Data Analysis/STAN Models/binom_code.stan", data = stan_data_binom, warmup = 150, iter = 1000, chains = 3, cores = 3, thin = 1)
binom_outputs <- rstan::extract(fit_binom, pars = c("beta0","beta1"))
write.csv(binom_outputs,"binom_outputs.csv")
binom_yrep <- rstan::extract(fit_binom,pars = c("y_rep"))$y_rep


## Check Posterior Distributions
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
y <- as.numeric(as.factor(binom_ant$occ_t1))
binom_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/ant_outputs_occ.csv", header = TRUE,stringsAsFactors=T)
yrep_binom <- binom_yrep
samp100 <- sample(nrow(yrep_binom), 1000)
## Overlay Plots
png(file = "binom_post.png")
bayesplot::ppc_dens_overlay(y, yrep_binom[samp100,])
dev.off()
png(file = "binom_ant_post.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_binom[samp100,], group = as.numeric(as.factor(binom_ant$occ_t1)))
dev.off()
## Convergence Plots
png("binom_conv.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_binom, pars=c("beta0", "beta1")))
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

#######################################################################################################
#### Multinomial Three Ants -- Probability of being occupied by each ant species #################################
#######################################################################################################
clv <- subset(cactus, ant_t != "other" & ant_t1 != "other" )
clv$ant_t1_relevel <- droplevels(clv$ant_t1_relevel)
clv$ant_t_relevel <- droplevels(clv$ant_t_relevel)
levels(clv$ant_t_relevel)
clv <- clv[,c("ant_t_relevel","ant_t1_relevel","logsize_t", "ant_t", "ant_t1","Year_t","Plot")]
clv <- na.omit(clv)
summary(clv$ant_t_relevel)
table(clv$ant_t1_relevel,clv$ant_t_relevel)/nrow(clv)
## Note that there are few liometopum that were not previously liometopum, making this a kinda weird subset. 
## This explains the somewhat odd results. 
##  t: 1 -- vac, 2 -- crem, 3 -- liom
##  t1: 1 -- liom, 2 -- vac, 3 -- crem
multi_dat_real <- list(K = length(unique(clv$ant_t1_relevel)), #number of possible ant species
                       N = nrow(clv), #number of observations
                       y = as.integer(as.factor(clv$ant_t1_relevel)), #observations
                       x = model.matrix(~ 0 + (as.factor(ant_t_relevel)) + logsize_t + as.factor(Year_t) + Plot, clv),
                       D = dim(model.matrix(~ 0 + (as.factor(ant_t_relevel)) + logsize_t + as.factor(Year_t) + Plot, clv))[2] #number of predictors
) #design matrix
## Run the model & save the results
fit_multi_clv <- stan(file = "Data Analysis/STAN Models/multi_prac_tom_Km1.stan", 
                      data = multi_dat_real, warmup = 150, iter = 1000, chains = 3) 

multi_clv_output <- rstan::extract(fit_multi_clv, pars = c("beta"))
write.csv(multi_clv_output,"multi_clv_outputs.csv")
summary(fit_multi_clv)
## Check the Posterior Dist
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
y <- clv$ant_t1_relevel
clv_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/multi_clv_outputs.csv", header = TRUE,stringsAsFactors=T)

## Convergence Plots
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[1,1]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[1,2]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[1,3]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[2,1]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[2,2]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[2,3]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[3,1]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[3,2]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[3,3]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[4,1]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[4,2]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[4,3]")),ISB = F, exact = T)
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

#######################################################################################################
#### Multinomial Three Ants -- Probability of being occupied by each ant species #################################
#######################################################################################################
cov <- subset(cactus, ant_t != "liom" & ant_t1 != "liom" )
cov$ant_t1_relevel <- droplevels(cov$ant_t1_relevel)
cov$ant_t_relevel <- droplevels(cov$ant_t_relevel)
levels(cov$ant_t_relevel)
cov <- cov[,c("ant_t_relevel","ant_t1_relevel","logsize_t", "ant_t", "ant_t1","Year_t","Plot")]
cov <- na.omit(cov)
summary(cov$ant_t_relevel)
table(cov$ant_t1_relevel,cov$ant_t_relevel)
## Note that there are few liometopum that were not previously liometopum, making this a kinda weird subset. 
## This explains the somewhat odd results. 
##  t: 1 -- vac, 2 -- other, 3 -- crem
##  t1: 1 -- crem, 2 -- vac, 3 -- other
multi_dat_real <- list(K = length(unique(cov$ant_t1_relevel)), #number of possible ant species
                       N = nrow(cov), #number of observations
                       y = as.integer(as.factor(cov$ant_t1_relevel)), #observations
                       x = model.matrix(~ 0 + (as.factor(ant_t_relevel)) + logsize_t + as.factor(Year_t) + Plot, cov),
                       D = dim(model.matrix(~ 0 + (as.factor(ant_t_relevel)) + logsize_t + as.factor(Year_t) + Plot, cov))[2] #number of predictors
) #design matrix
## Run the model & save the results
fit_multi_cov <- stan(file = "Data Analysis/STAN Models/multi_prac_tom_Km1.stan", 
                      data = multi_dat_real, warmup = 150, iter = 1000, chains = 3) 

multi_cov_output <- rstan::extract(fit_multi_cov, pars = c("beta"))
write.csv(multi_cov_output,"multi_cov_outputs.csv")
summary(fit_multi_cov)
## Check the Posterior Dist
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
y <- clv$ant_t1_relevel
clv_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/multi_clv_outputs.csv", header = TRUE,stringsAsFactors=T)

## Convergence Plots
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[1,1]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[1,2]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[1,3]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[2,1]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[2,2]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[2,3]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[3,1]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[3,2]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[3,3]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[4,1]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[4,2]")),ISB = F, exact = T)
bayesplot::mcmc_trace(As.mcmc.list(fit_multi_clv, pars=c("beta[4,3]")),ISB = F, exact = T)
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")






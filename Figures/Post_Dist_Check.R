setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
source("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Cholla_Analysis_Vital_Rates.R")
#### Y_rep checks

## Growth ################################################################################################
#For overlay plots
y <- growth_data$logsize_t1
grow_out_all <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow_outputs_all.csv", header = TRUE,stringsAsFactors=T)
yrep_grow <- grow_yrep_all#read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow_yrep_all.csv", header = TRUE,stringsAsFactors=T)-all
samp100 <- sample(nrow(yrep_grow), 500)
## Overlay Plots
png(file = "grow_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_grow[samp100,])
dev.off()
png(file = "grow_ant_post1.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_grow[samp100,], group = ant_grow)
dev.off()
## Convergence Plots
png("grow_conv2.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_grow_all, pars=c("beta0", "beta1")))
dev.off()

## Survival ################################################################################################
#overlay plot data
y <- survival_data$Survival_t1
surv_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/surv_outputs.csv", header = TRUE,stringsAsFactors=T)
yrep_surv <- surv_yrep_all
samp100 <- sample(nrow(yrep_surv), 500)
## Overlay Plots
png(file = "surv_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_surv[samp100,])
dev.off()
png(file = "surv_ant_post1.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_surv[samp100,],group = as.integer(as.factor(survival_data$ant_t)))
dev.off()
## Convergence Plots
png(file = "surv_conv1.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_surv_all, pars=c("beta0", "beta1")))
dev.off()

## Reproductive State ################################################################################################
repro_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/repro_outputs.csv", header = TRUE,stringsAsFactors=T)
y <- as.numeric(y_repro)
yrep_repro <- repro_yrep
samp100 <- sample(nrow(yrep_repro), 500)
## Overlay Plots
png(file = "repro_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_repro[samp100,])
dev.off()
## Convergence Plots
png(file = "repro_conv1.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_repro_mix_ant, pars=c("beta0", "beta1")))
dev.off()


## Flower Numbers Truncated ################################################################################################
y_trunc <- y_flow
# Create the y rep (needs to be done outside of STAN because of the 0 truncation)
y_flow_trunc_sim <- matrix(NA,1500,length(flower_data$TotFlowerbuds_t))
for(i in 1:1500){
  print(i)
  for(j in 1:length(flower_data$TotFlowerbuds_t)){
    y_flow_trunc_sim[i,j] <- sample(x=1:1000,size=1,replace=T,prob=dnbinom(1:1000, mu = exp(flow_y_rep_all[i,j]), size=flow_phi_all[i]) / (1 - dnbinom(0, mu = exp(flow_y_rep_all[i,j]), size=flow_phi_all[i])))
  }
}
samp100 <- sample(nrow(y_flow_trunc_sim), 500)
## Overlay Plots
png(file = "flow_post_trunc1.png")
bayesplot::ppc_dens_overlay(y_trunc, y_flow_trunc_sim[samp100,])
dev.off()
## Convergence Plots
png(file = "flow_conv_trunc1")
bayesplot::mcmc_trace(As.mcmc.list(fit_flow_trunc_all, pars=c("beta0", "beta1")))
dev.off()

## Viability of Flowers ################################################################################################
y <- good_viab
yrep_viab <- viab_yrep_all
samp100 <- sample(nrow(yrep_viab), 500)
## Overlay Plots
png(file = "viab_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_viab[samp100,])
dev.off()
png(file = "viab_ant_post1.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_viab[samp100,],group = ant_viab)
dev.off()
## Convergence Plots
png(file = "viab_conv1")
bayesplot::mcmc_trace(As.mcmc.list(fit_viab_all, pars=c("beta0")))
dev.off()


## Fruit Survival ################################################################################################
y <- fruit.surv$Fr.on.grnd.not.chewed/fruit.surv$Fr.on.plant
yrep_fruit_surv <- fruit_surv_yrep
samp100 <- sample(nrow(yrep_fruit_surv), 500)
## Overlay Plots
png(file = "fruit_surv_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_fruit_surv[samp100,])
dev.off()
## Convergence Plots
png(file = "fruit_surv_conv1")
bayesplot::mcmc_trace(As.mcmc.list(fit_fruit_surv, pars=c("beta0")))
dev.off()


## Seeds per Fruit ################################################################################################
y <- seed_data$seed_count
yrep_seed <- seed_yrep
samp100 <- sample(nrow(yrep_seed), 500)
## Overlay Plots
png(file = "seed_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_seed[samp100,])
dev.off()
png(file = "seed_ant_post1.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_seed[samp100,],group = as.integer(as.factor(seed_data$ant)))
dev.off()
## Convergence Plots
png(file = "seed_conv1")
bayesplot::mcmc_trace(As.mcmc.list(fit_seed, pars=c("beta0")))
dev.off()


## Seed Survival ################################################################################################
y <- precensus.dat$survive0405
yrep_seed_surv <- seed_surv_yrep
samp100 <- sample(nrow(yrep_seed_surv), 500)
## Overlay Plots
png(file = "seed_surv_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_seed_surv[samp100,])
dev.off()
## Convergence Plots
png(file = "seed_surv_conv1")
bayesplot::mcmc_trace(As.mcmc.list(fit_seed_surv, pars=c("beta0","beta1")))
dev.off()


## Germination Rate ################################################################################################
## Germ yr 1
y <- germ.dat$Seedlings04/germ.dat$Input
yrep_germ1 <- germ1_yrep
samp100 <- sample(nrow(yrep_germ1), 500)
## Overlay Plots
png(file = "germ1_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_germ1[samp100,])
dev.off()
## Convergence Plots
png(file = "germ1_conv1")
bayesplot::mcmc_trace(As.mcmc.list(fit_germ1, pars=c("beta0","beta1")))
dev.off()
## Germ yr 2
y <- germ.dat$Seedlings05/(germ.dat$Input - germ.dat$Seedlings04)
yrep_germ2 <- germ2_yrep
samp100 <- sample(nrow(yrep_germ2), 500)
## Overlay Plots
png(file = "germ2_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_germ2[samp100,])
dev.off()
## Convergence Plots
png(file = "germ2_conv1")
bayesplot::mcmc_trace(As.mcmc.list(fit_germ2, pars=c("beta0","beta1")))
dev.off()


## Recruit ################################################################################################
y <- seedling.dat$volume_t
yrep_rec <- rec_yrep
samp100 <- sample(nrow(yrep_rec), 500)
## Overlay Plots
png(file = "rec_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_rec[samp100,])
dev.off()
## Convergence Plots
png(file = "rec_conv1")
bayesplot::mcmc_trace(As.mcmc.list(fit_rec, pars=c("beta0")))
dev.off()

## Binomial Transition Model ##############################################################################
y <- stan_data_ant_occ$success
occ_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/ant_outputs_occ.csv", header = TRUE,stringsAsFactors=T)
yrep_occ <- occ_yrep
samp100 <- sample(nrow(yrep_occ), 1000)
## Overlay Plots
png(file = "occ_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_occ[samp100,])
dev.off()
png(file = "occ_ant_post1.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_occ[samp100,], group = stan_data_ant_occ$prev_ant)
dev.off()
## Convergence Plots
png("occ_conv2.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_ant_occ, pars=c("beta0", "beta1")))
dev.off()

## Multinomial Transition Model ##############################################################################


setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")

#### Y_rep checks

## Growth ################################################################################################
#For overlay plots
y <- y_grow
grow_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow_outputs.csv", header = TRUE,stringsAsFactors=T)
yrep_grow <- grow_yrep
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
bayesplot::mcmc_trace(As.mcmc.list(fit_grow_mix_ant, pars=c("beta0", "beta1")))
title()
dev.off()

## Survival ################################################################################################
#overlay plot data
y <- y_surv
surv_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/surv_outputs.csv", header = TRUE,stringsAsFactors=T)
yrep_surv <- surv_yrep
samp100 <- sample(nrow(yrep_surv), 500)
## Overlay Plots
png(file = "surv_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_surv[samp100,])
dev.off()
png(file = "surv_ant_post1.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_surv[samp100,],group = ant_surv)
dev.off()
## Convergence Plots
png(file = "surv_conv1.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_surv_mix_ant, pars=c("beta0", "beta1")))
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


## Flower Numbers ################################################################################################
y <- y_flow
yrep_flow <- flow_yrep
samp100 <- sample(nrow(yrep_flow), 500)
## Overlay Plots
png(file = "flow_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_flow[samp100,])
dev.off()
## Convergence Plots
png(file = "flow_conv1")
bayesplot::mcmc_trace(As.mcmc.list(fit_flow_mix_ant, pars=c("beta0", "beta1")))
dev.off()


## Flower Numbers Truncated ################################################################################################
y_trunc <- y_flow
# Create the y rep (needs to be done outside of STAN because of the 0 truncation)
y_flow_trunc_sim <- matrix(NA,1500,length(flower_data$TotFlowerbuds_t))
for(i in 1:1500){
  print(i)
  for(j in 1:length(flower_data$TotFlowerbuds_t)){
    y_flow_trunc_sim[i,j] <- sample(x=1:1000,size=1,replace=T,prob=dnbinom(1:1000, mu = exp(flow_pred[i,j]), size=phi_flow[i]) / (1 - dnbinom(0, mu = exp(flow_pred[i,j]), size=phi_flow[i])))
  }
}
yrep_flow_trunc <- y_flow_trunc_sim
samp100 <- sample(nrow(yrep_flow_trunc), 500)
## Overlay Plots
png(file = "flow_post_trunc1.png")
bayesplot::ppc_dens_overlay(y_trunc, yrep_flow_trunc[samp100,])
dev.off()
## Convergence Plots
png(file = "flow_conv_trunc1")
bayesplot::mcmc_trace(As.mcmc.list(fit_flow_mix_ant_trunc, pars=c("beta0", "beta1")))
dev.off()

## Viability of Flowers ################################################################################################
y <- good_viab
yrep_viab <- viab_yrep
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
bayesplot::mcmc_trace(As.mcmc.list(fit_viab_mix_ant, pars=c("beta0")))
dev.off()


## Number of Fruit ################################################################################################

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
y <- germ.dat$Seedlings04
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
y <- germ.dat$Seedlings05
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


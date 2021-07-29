setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Data Analysis")

#### Occupied vs Unoccupied ##########################################################################
ant.dat2 <- cactus
ant.dat2$occ_t <- 2
ant.dat2$occ_t1 <- 1
ant.dat2$occ_t1[ant.dat2$ant_t1 == "vacant"] <- 0 
ant.dat2$occ_t[ant.dat2$ant_t == "vacant"] <- 1
table_occ <- table(ant.dat2$Year_t, ant.dat2$ant_t)
ant.dat2$sum[ant.dat2$Year_t == 2004] <- sum(table_occ[1,])
ant.dat2$sum[ant.dat2$Year_t == 2005] <- sum(table_occ[2,])
ant.dat2$sum[ant.dat2$Year_t == 2006] <- sum(table_occ[3,])
ant.dat2$sum[ant.dat2$Year_t == 2007] <- sum(table_occ[4,])
ant.dat2$sum[ant.dat2$Year_t == 2009] <- sum(table_occ[5,])
ant.dat2$sum[ant.dat2$Year_t == 2010] <- sum(table_occ[6,])
ant.dat2$sum[ant.dat2$Year_t == 2011] <- sum(table_occ[7,])
ant.dat2$sum[ant.dat2$Year_t == 2012] <- sum(table_occ[8,])
ant.dat2$sum[ant.dat2$Year_t == 2013] <- sum(table_occ[9,])
ant.dat2$sum[ant.dat2$Year_t == 2014] <- sum(table_occ[10,])
ant.dat2$sum[ant.dat2$Year_t == 2015] <- sum(table_occ[11,])
ant.dat2$sum[ant.dat2$Year_t == 2016] <- sum(table_occ[12,])
ant.dat2$sum[ant.dat2$Year_t == 2017] <- sum(table_occ[13,])
ant.dat2$sum[ant.dat2$Year_t == 2018] <- sum(table_occ[14,])
ant.dat2$sum[ant.dat2$Year_t == 2019] <- sum(table_occ[15,])

ant.dat2 <- na.omit(ant.dat2)
View(ant.dat2)

stan_data_ant_occ <- list(N_obs = nrow(ant.dat2),
                           success = ant.dat2$occ_t1,
                           N_ant = 2,
                           prev_ant = ant.dat2$occ_t,
                           N_year = length(unique(ant.dat2$Year_t)),
                           year = as.factor(as.integer(ant.dat2$Year_t)),
                           trials = ant.dat2$sum,
                           vol_ant = ant.dat2$volume_t
)

fit_ant_occ <- stan("STAN Models/Multinomial Practice/crem_vac_prac.stan",data = stan_data_ant_occ, warmup = 500, iter = 3000, chains = 3, cores = 2, thin = 1)
ant_outputs_occ <- rstan::extract(fit_ant_crem, pars = c("beta0","beta1","sigma")
)
write.csv(ant_outputs_occ, "ant_outputs_occ.csv")
occ_yrep <- rstan::extract(fit_ant_occ, pars = c("y_rep"))$y_rep

summary(fit_ant_occ)
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
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






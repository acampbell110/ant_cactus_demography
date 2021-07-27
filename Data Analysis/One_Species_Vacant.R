setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Data Analysis")

#### Occupied vs Unoccupied ##########################################################################
ant.dat2 <- cactus
ant.dat2$occ_t <- 2
table_occ <- table(ant.dat2$Year_t, ant.dat2$ant_t)
ant.dat2$occ_t[ant.dat2$ant_t == "vacant"] <- 1
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
ant.dat2$ant <- 1
ant.dat2$ant[ant.dat2$ant_t == "crem"] <- 2
ant.dat2$ant1 <- 0
ant.dat2$ant1[ant.dat2$ant_t1 == "crem"] <- 1
ant.dat2 <- na.omit(ant.dat2)
View(ant.dat2)

stan_data_ant_occ <- list(N_obs = nrow(ant.dat2),
                           success = ant.dat2$ant1,
                           N_ant = 2,
                           prev_ant = ant.dat2$ant,
                           N_year = length(unique(ant.dat2$Year_t)),
                           year = as.factor(as.integer(ant.dat2$Year_t)),
                           trials = ant.dat2$sum,
                           vol_ant = ant.dat2$volume_t
)

fit_ant_occ <- stan("STAN Models/Multinomial Practice/crem_vac_prac.stan",data = stan_data_ant_occ, warmup = 500, iter = 3000, chains = 3, cores = 2, thin = 1)
ant_outputs_crem <- rstan::extract(fit_ant_crem, pars = c("beta0","beta1","sigma")
)
write.csv(ant_outputs_crem, "ant_outputs_crem.csv")
crem_yrep <- rstan::extract(fit_ant_crem, pars = c("y_rep"))$y_rep

summary(fit_ant_crem)
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
y <- stan_data_ant_crem$success
crem_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/ant_outputs_crem.csv", header = TRUE,stringsAsFactors=T)
yrep_crem <- crem_yrep
samp100 <- sample(nrow(yrep_crem), 500)
## Overlay Plots
png(file = "crem_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_crem[samp100,])
dev.off()
png(file = "crem_ant_post1.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_crem[samp100,], group = stan_data_ant_crem$prev_ant)
dev.off()
## Convergence Plots
png("crem_conv2.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_ant_crem, pars=c("beta0", "beta1")))
dev.off()




#### Crem ############################################################################################
ant.dat<-as.data.frame(table(cactus$ant_t ,cactus$Year_t))
ant.dat <- subset(ant.dat, Var1 == "crem" | Var1 == "vacant")
ant.dat2 <- subset(cactus, cactus$ant_t == "crem" | cactus$ant_t == "vacant")
ant.dat2 <- subset(ant.dat2, ant.dat2$ant_t1 == "crem" | ant.dat2$ant_t1 == "vacant")
ant.dat2$sum[ant.dat2$Year_t == 2004] <- sum(ant.dat$Freq[ant.dat$Var2 == 2004])
ant.dat2$sum[ant.dat2$Year_t == 2005] <- sum(ant.dat$Freq[ant.dat$Var2 == 2005])
ant.dat2$sum[ant.dat2$Year_t == 2006] <- sum(ant.dat$Freq[ant.dat$Var2 == 2006])
ant.dat2$sum[ant.dat2$Year_t == 2007] <- sum(ant.dat$Freq[ant.dat$Var2 == 2007])
ant.dat2$sum[ant.dat2$Year_t == 2009] <- sum(ant.dat$Freq[ant.dat$Var2 == 2009])
ant.dat2$sum[ant.dat2$Year_t == 2010] <- sum(ant.dat$Freq[ant.dat$Var2 == 2010])
ant.dat2$sum[ant.dat2$Year_t == 2011] <- sum(ant.dat$Freq[ant.dat$Var2 == 2011])
ant.dat2$sum[ant.dat2$Year_t == 2012] <- sum(ant.dat$Freq[ant.dat$Var2 == 2012])
ant.dat2$sum[ant.dat2$Year_t == 2013] <- sum(ant.dat$Freq[ant.dat$Var2 == 2013])
ant.dat2$sum[ant.dat2$Year_t == 2014] <- sum(ant.dat$Freq[ant.dat$Var2 == 2014])
ant.dat2$sum[ant.dat2$Year_t == 2015] <- sum(ant.dat$Freq[ant.dat$Var2 == 2015])
ant.dat2$sum[ant.dat2$Year_t == 2016] <- sum(ant.dat$Freq[ant.dat$Var2 == 2016])
ant.dat2$sum[ant.dat2$Year_t == 2017] <- sum(ant.dat$Freq[ant.dat$Var2 == 2017])
ant.dat2$sum[ant.dat2$Year_t == 2018] <- sum(ant.dat$Freq[ant.dat$Var2 == 2018])
ant.dat2$sum[ant.dat2$Year_t == 2019] <- sum(ant.dat$Freq[ant.dat$Var2 == 2019])
ant.dat2$ant <- 1
ant.dat2$ant[ant.dat2$ant_t == "crem"] <- 2
ant.dat2$ant1 <- 0
ant.dat2$ant1[ant.dat2$ant_t1 == "crem"] <- 1
ant.dat2 <- na.omit(ant.dat2)
View(ant.dat2)

stan_data_ant_crem <- list(N_obs = nrow(ant.dat2),
                      success = ant.dat2$ant1,
                      N_ant = 2,
                      prev_ant = ant.dat2$ant,
                      N_year = length(unique(ant.dat2$Year_t)),
                      year = as.factor(as.integer(ant.dat2$Year_t)),
                      trials = ant.dat2$sum,
                      vol_ant = ant.dat2$volume_t
                      )

fit_ant_crem <- stan("STAN Models/Multinomial Practice/crem_vac_prac.stan",data = stan_data_ant_crem, warmup = 500, iter = 3000, chains = 3, cores = 2, thin = 1)
ant_outputs_crem <- rstan::extract(fit_ant_crem, pars = c("beta0","beta1","sigma")
)
write.csv(ant_outputs_crem, "ant_outputs_crem.csv")
crem_yrep <- rstan::extract(fit_ant_crem, pars = c("y_rep"))$y_rep

summary(fit_ant_crem)
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
y <- stan_data_ant_crem$success
crem_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/ant_outputs_crem.csv", header = TRUE,stringsAsFactors=T)
yrep_crem <- crem_yrep
samp100 <- sample(nrow(yrep_crem), 500)
## Overlay Plots
png(file = "crem_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_crem[samp100,])
dev.off()
png(file = "crem_ant_post1.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_crem[samp100,], group = stan_data_ant_crem$prev_ant)
dev.off()
## Convergence Plots
png("crem_conv2.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_ant_crem, pars=c("beta0", "beta1")))
dev.off()



#### Other ############################################################################################
ant.dat<-as.data.frame(table(cactus$ant_t ,cactus$Year_t))
ant.dat <- subset(ant.dat, Var1 == "other" | Var1 == "vacant")
ant.dat2 <- subset(cactus, cactus$ant_t == "other" | cactus$ant_t == "vacant")
ant.dat2 <- subset(ant.dat2, ant.dat2$ant_t1 == "other" | ant.dat2$ant_t1 == "vacant")
ant.dat2$sum[ant.dat2$Year_t == 2004] <- sum(ant.dat$Freq[ant.dat$Var2 == 2004])
ant.dat2$sum[ant.dat2$Year_t == 2005] <- sum(ant.dat$Freq[ant.dat$Var2 == 2005])
ant.dat2$sum[ant.dat2$Year_t == 2006] <- sum(ant.dat$Freq[ant.dat$Var2 == 2006])
ant.dat2$sum[ant.dat2$Year_t == 2007] <- sum(ant.dat$Freq[ant.dat$Var2 == 2007])
ant.dat2$sum[ant.dat2$Year_t == 2009] <- sum(ant.dat$Freq[ant.dat$Var2 == 2009])
ant.dat2$sum[ant.dat2$Year_t == 2010] <- sum(ant.dat$Freq[ant.dat$Var2 == 2010])
ant.dat2$sum[ant.dat2$Year_t == 2011] <- sum(ant.dat$Freq[ant.dat$Var2 == 2011])
ant.dat2$sum[ant.dat2$Year_t == 2012] <- sum(ant.dat$Freq[ant.dat$Var2 == 2012])
ant.dat2$sum[ant.dat2$Year_t == 2013] <- sum(ant.dat$Freq[ant.dat$Var2 == 2013])
ant.dat2$sum[ant.dat2$Year_t == 2014] <- sum(ant.dat$Freq[ant.dat$Var2 == 2014])
ant.dat2$sum[ant.dat2$Year_t == 2015] <- sum(ant.dat$Freq[ant.dat$Var2 == 2015])
ant.dat2$sum[ant.dat2$Year_t == 2016] <- sum(ant.dat$Freq[ant.dat$Var2 == 2016])
ant.dat2$sum[ant.dat2$Year_t == 2017] <- sum(ant.dat$Freq[ant.dat$Var2 == 2017])
ant.dat2$sum[ant.dat2$Year_t == 2018] <- sum(ant.dat$Freq[ant.dat$Var2 == 2018])
ant.dat2$sum[ant.dat2$Year_t == 2019] <- sum(ant.dat$Freq[ant.dat$Var2 == 2019])

ant.dat2$ant <- 1
ant.dat2$ant[ant.dat2$ant_t == "other"] <- 2
ant.dat2$ant1 <- 0
ant.dat2$ant1[ant.dat2$ant_t1 == "other"] <- 1
ant.dat2 <- na.omit(ant.dat2)
View(ant.dat2)


stan_data_ant_other <- list(N_obs = nrow(ant.dat2),
                      success = ant.dat2$ant1,
                      N_ant = 2,
                      prev_ant = ant.dat2$ant,
                      N_year = length(unique(ant.dat2$Year_t)),
                      year = as.factor(as.integer(ant.dat2$Year_t)),
                      trials = ant.dat2$sum,
                      vol_ant = ant.dat2$volume_t
)

fit_ant_other <- stan("STAN Models/Multinomial Practice/crem_vac_prac.stan",data = stan_data_ant_other, warmup = 500, iter = 3000, chains = 3, cores = 2, thin = 1)
ant_outputs_other <- rstan::extract(fit_ant_other, pars = c("beta0","beta1","sigma")
)
write.csv(ant_outputs_other, "ant_outputs_other.csv")
other_yrep <- rstan::extract(fit_ant_other, pars = c("y_rep"))$y_rep
summary(fit_ant_other)

y <- stan_data_ant_other$success
other_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/ant_outputs_other.csv", header = TRUE,stringsAsFactors=T)
yrep_other <- other_yrep
samp100 <- sample(nrow(yrep_other), 500)
## Overlay Plots
png(file = "other_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_other[samp100,])
dev.off()
png(file = "other_ant_post1.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_other[samp100,], group = stan_data_ant_other$prev_ant)
dev.off()
## Convergence Plots
png("other_conv2.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_ant_other, pars=c("beta0", "beta1")))
dev.off()

#### Liom ############################################################################################
ant.dat<-as.data.frame(table(cactus$ant_t ,cactus$Year_t))
ant.dat <- subset(ant.dat, Var1 == "liom" | Var1 == "vacant")
ant.dat2 <- subset(cactus, cactus$ant_t == "liom" | cactus$ant_t == "vacant")
ant.dat2 <- subset(ant.dat2, ant.dat2$ant_t1 == "liom" | ant.dat2$ant_t1 == "vacant")
ant.dat2$sum[ant.dat2$Year_t == 2004] <- sum(ant.dat$Freq[ant.dat$Var2 == 2004])
ant.dat2$sum[ant.dat2$Year_t == 2005] <- sum(ant.dat$Freq[ant.dat$Var2 == 2005])
ant.dat2$sum[ant.dat2$Year_t == 2006] <- sum(ant.dat$Freq[ant.dat$Var2 == 2006])
ant.dat2$sum[ant.dat2$Year_t == 2007] <- sum(ant.dat$Freq[ant.dat$Var2 == 2007])
ant.dat2$sum[ant.dat2$Year_t == 2009] <- sum(ant.dat$Freq[ant.dat$Var2 == 2009])
ant.dat2$sum[ant.dat2$Year_t == 2010] <- sum(ant.dat$Freq[ant.dat$Var2 == 2010])
ant.dat2$sum[ant.dat2$Year_t == 2011] <- sum(ant.dat$Freq[ant.dat$Var2 == 2011])
ant.dat2$sum[ant.dat2$Year_t == 2012] <- sum(ant.dat$Freq[ant.dat$Var2 == 2012])
ant.dat2$sum[ant.dat2$Year_t == 2013] <- sum(ant.dat$Freq[ant.dat$Var2 == 2013])
ant.dat2$sum[ant.dat2$Year_t == 2014] <- sum(ant.dat$Freq[ant.dat$Var2 == 2014])
ant.dat2$sum[ant.dat2$Year_t == 2015] <- sum(ant.dat$Freq[ant.dat$Var2 == 2015])
ant.dat2$sum[ant.dat2$Year_t == 2016] <- sum(ant.dat$Freq[ant.dat$Var2 == 2016])
ant.dat2$sum[ant.dat2$Year_t == 2017] <- sum(ant.dat$Freq[ant.dat$Var2 == 2017])
ant.dat2$sum[ant.dat2$Year_t == 2018] <- sum(ant.dat$Freq[ant.dat$Var2 == 2018])
ant.dat2$sum[ant.dat2$Year_t == 2019] <- sum(ant.dat$Freq[ant.dat$Var2 == 2019])

ant.dat2$ant <- 1
ant.dat2$ant[ant.dat2$ant_t == "liom"] <- 2
ant.dat2$ant1 <- 0
ant.dat2$ant1[ant.dat2$ant_t1 == "liom"] <- 1
ant.dat2 <- na.omit(ant.dat2)
View(ant.dat2)


stan_data_ant_liom <- list(N_obs = nrow(ant.dat2),
                      success = ant.dat2$ant1,
                      N_ant = 2,
                      prev_ant = ant.dat2$ant,
                      N_year = length(unique(ant.dat2$Year_t)),
                      year = as.factor(as.integer(ant.dat2$Year_t)),
                      trials = ant.dat2$sum,
                      vol_ant = ant.dat2$volume_t
)

fit_ant_liom <- stan("STAN Models/Multinomial Practice/crem_vac_prac.stan",data = stan_data_ant_liom, warmup = 500, iter = 3000, chains = 3, cores = 2, thin = 1)
ant_outputs_liom <- rstan::extract(fit_ant_liom, pars = c("beta0","beta1","sigma")
)
write.csv(ant_outputs_liom, "ant_outputs_liom.csv")
liom_yrep <- rstan::extract(fit_ant_liom, pars = c("y_rep"))$y_rep
summary(fit_ant_liom)

y <- stan_data_ant_liom$success
liom_data <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/ant_outputs_liom.csv", header = TRUE,stringsAsFactors=T)
yrep_liom <- liom_yrep
samp100 <- sample(nrow(yrep_liom), 500)
## Overlay Plots
png(file = "liom_post1.png")
bayesplot::ppc_dens_overlay(y, yrep_liom[samp100,])
dev.off()
png(file = "liom_ant_post1.png")
bayesplot::ppc_dens_overlay_grouped(y, yrep_liom[samp100,], group = stan_data_ant_liom$prev_ant)
dev.off()
## Convergence Plots
png("liom_conv2.png")
bayesplot::mcmc_trace(As.mcmc.list(fit_ant_liom, pars=c("beta0", "beta1")))
dev.off()

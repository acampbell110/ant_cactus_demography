setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Data Analysis")


ant.dat<-as.data.frame(table(cactus$ant_t ,cactus$Year_t))
ant.dat <- subset(ant.dat, Var1 == "crem" | Var1 == "vacant")
ant.dat2 <- subset(cactus, cactus$ant_t == "crem" | cactus$ant_t == "vacant")
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

ant.dat2$Freq[ant.dat2$Year_t == 2004 & ant.dat2$ant_t == "crem"] <- (ant.dat$Freq[ant.dat$Var2 == 2004 & ant.dat$Var1 == "crem"])
ant.dat2$Freq[ant.dat2$Year_t == 2004 & ant.dat2$ant_t == "vacant"] <- (ant.dat$Freq[ant.dat$Var2 == 2004 & ant.dat$Var1 == "vacant"])
ant.dat2$Freq[ant.dat2$Year_t == 2005 & ant.dat2$ant_t == "crem"] <- (ant.dat$Freq[ant.dat$Var2 == 2005 & ant.dat$Var1 == "crem"])
ant.dat2$Freq[ant.dat2$Year_t == 2005 & ant.dat2$ant_t == "vacant"] <- (ant.dat$Freq[ant.dat$Var2 == 2005 & ant.dat$Var1 == "vacant"])
ant.dat2$Freq[ant.dat2$Year_t == 2006 & ant.dat2$ant_t == "crem"] <- (ant.dat$Freq[ant.dat$Var2 == 2006 & ant.dat$Var1 == "crem"])
ant.dat2$Freq[ant.dat2$Year_t == 2006 & ant.dat2$ant_t == "vacant"] <- (ant.dat$Freq[ant.dat$Var2 == 2006 & ant.dat$Var1 == "vacant"])
ant.dat2$Freq[ant.dat2$Year_t == 2007 & ant.dat2$ant_t == "crem"] <- (ant.dat$Freq[ant.dat$Var2 == 2007 & ant.dat$Var1 == "crem"])
ant.dat2$Freq[ant.dat2$Year_t == 2007 & ant.dat2$ant_t == "vacant"] <- (ant.dat$Freq[ant.dat$Var2 == 2007 & ant.dat$Var1 == "vacant"])
ant.dat2$Freq[ant.dat2$Year_t == 2009 & ant.dat2$ant_t == "crem"] <- (ant.dat$Freq[ant.dat$Var2 == 2009 & ant.dat$Var1 == "crem"])
ant.dat2$Freq[ant.dat2$Year_t == 2009 & ant.dat2$ant_t == "vacant"] <- (ant.dat$Freq[ant.dat$Var2 == 2009 & ant.dat$Var1 == "vacant"])
ant.dat2$Freq[ant.dat2$Year_t == 2010 & ant.dat2$ant_t == "crem"] <- (ant.dat$Freq[ant.dat$Var2 == 2010 & ant.dat$Var1 == "crem"])
ant.dat2$Freq[ant.dat2$Year_t == 2010 & ant.dat2$ant_t == "vacant"] <- (ant.dat$Freq[ant.dat$Var2 == 2010 & ant.dat$Var1 == "vacant"])
ant.dat2$Freq[ant.dat2$Year_t == 2011 & ant.dat2$ant_t == "crem"] <- (ant.dat$Freq[ant.dat$Var2 == 2011 & ant.dat$Var1 == "crem"])
ant.dat2$Freq[ant.dat2$Year_t == 2011 & ant.dat2$ant_t == "vacant"] <- (ant.dat$Freq[ant.dat$Var2 == 2011 & ant.dat$Var1 == "vacant"])
ant.dat2$Freq[ant.dat2$Year_t == 2012 & ant.dat2$ant_t == "crem"] <- (ant.dat$Freq[ant.dat$Var2 == 2012 & ant.dat$Var1 == "crem"])
ant.dat2$Freq[ant.dat2$Year_t == 2012 & ant.dat2$ant_t == "vacant"] <- (ant.dat$Freq[ant.dat$Var2 == 2012 & ant.dat$Var1 == "vacant"])
ant.dat2$Freq[ant.dat2$Year_t == 2013 & ant.dat2$ant_t == "crem"] <- (ant.dat$Freq[ant.dat$Var2 == 2013 & ant.dat$Var1 == "crem"])
ant.dat2$Freq[ant.dat2$Year_t == 2013 & ant.dat2$ant_t == "vacant"] <- (ant.dat$Freq[ant.dat$Var2 == 2013 & ant.dat$Var1 == "vacant"])
ant.dat2$Freq[ant.dat2$Year_t == 2014 & ant.dat2$ant_t == "crem"] <- (ant.dat$Freq[ant.dat$Var2 == 2014 & ant.dat$Var1 == "crem"])
ant.dat2$Freq[ant.dat2$Year_t == 2014 & ant.dat2$ant_t == "vacant"] <- (ant.dat$Freq[ant.dat$Var2 == 2014 & ant.dat$Var1 == "vacant"])
ant.dat2$Freq[ant.dat2$Year_t == 2015 & ant.dat2$ant_t == "crem"] <- (ant.dat$Freq[ant.dat$Var2 == 2015 & ant.dat$Var1 == "crem"])
ant.dat2$Freq[ant.dat2$Year_t == 2015 & ant.dat2$ant_t == "vacant"] <- (ant.dat$Freq[ant.dat$Var2 == 2015 & ant.dat$Var1 == "vacant"])
ant.dat2$Freq[ant.dat2$Year_t == 2016 & ant.dat2$ant_t == "crem"] <- (ant.dat$Freq[ant.dat$Var2 == 2016 & ant.dat$Var1 == "crem"])
ant.dat2$Freq[ant.dat2$Year_t == 2016 & ant.dat2$ant_t == "vacant"] <- (ant.dat$Freq[ant.dat$Var2 == 2016 & ant.dat$Var1 == "vacant"])
ant.dat2$Freq[ant.dat2$Year_t == 2017 & ant.dat2$ant_t == "crem"] <- (ant.dat$Freq[ant.dat$Var2 == 2017 & ant.dat$Var1 == "crem"])
ant.dat2$Freq[ant.dat2$Year_t == 2017 & ant.dat2$ant_t == "vacant"] <- (ant.dat$Freq[ant.dat$Var2 == 2017 & ant.dat$Var1 == "vacant"])
ant.dat2$Freq[ant.dat2$Year_t == 2018 & ant.dat2$ant_t == "crem"] <- (ant.dat$Freq[ant.dat$Var2 == 2018 & ant.dat$Var1 == "crem"])
ant.dat2$Freq[ant.dat2$Year_t == 2018 & ant.dat2$ant_t == "vacant"] <- (ant.dat$Freq[ant.dat$Var2 == 2018 & ant.dat$Var1 == "vacant"])
ant.dat2$Freq[ant.dat2$Year_t == 2019 & ant.dat2$ant_t == "crem"] <- (ant.dat$Freq[ant.dat$Var2 == 2019 & ant.dat$Var1 == "crem"])
ant.dat2$Freq[ant.dat2$Year_t == 2019 & ant.dat2$ant_t == "vacant"] <- (ant.dat$Freq[ant.dat$Var2 == 2019 & ant.dat$Var1 == "vacant"])
ant.dat2 <- na.omit(ant.dat2)
ant.dat2$ant <- as.integer(as.factor(ant.dat2$ant1))
ant.dat2$ant1 <- 0
ant.dat2$ant1[ant.dat2$ant_t1 == "crem"] <- 1


stan_data_ant <- list(N_obs = nrow(ant.dat2),
                      success = ant.dat2$ant1,
                      N_ant = 2,
                      prev_ant = ant.dat2$ant,
                      N_year = length(unique(ant.dat2$Year_t)),
                      year = as.factor(as.integer(ant.dat2$Year_t)),
                      trials = ant.dat2$sum,
                      vol_ant = ant.dat2$volume_t
                      )

fit_ant <- stan("STAN Models/Multinomial Practice/crem_vac_prac.stan",data = stan_data_ant, warmup = 500, iter = 1000, chains = 3, cores = 2, thin = 1)
ant_outputs <- rstan::extract(fit_ant, pars = c("beta0","beta1","sigma")
)
write.csv(ant_outputs, "ant_outputs.csv")

stan_data <- list(
  #### Growth Variables
  N_grow = N_grow, ## number of observations
  vol_grow = vol_grow, ## predictors volume
  y_grow = y_grow, ## response volume next year
  ant_grow = ant_grow,## predictors ants
  N_ant = N_ant, ## number of ant states
  N_Year_grow = N_Year_grow, ## number of years
  N_Plot_grow = N_Plot_grow, ## number of plots
  plot_grow = plot_grow, ## predictor plots
  year_grow = year_grow, ## predictor years
  #### Survival Variables
  N_surv = N_surv, ## number of observations
  vol_surv = vol_surv, ## predictors volume
  y_surv = y_surv, ## response volume next year
  ant_surv = ant_surv,## predictors ants
  N_ant = N_ant, ## number of ant states
  N_Year_surv = N_Year_surv, ## number of years
  N_Plot_surv = N_Plot_surv, ## number of plots
  plot_surv = plot_surv, ## predictor plots
  year_surv = year_surv, ## predictor years
  #### Flowering Variables
  N_flower = N_flower, ## number of observations
  lower_limit = 1, ## we want the 0s to be removed
  vol_flower = vol_flower, ## predictors volume
  y_flow = y_flow, ## response volume next year
  N_Year_flower = N_Year_flower, ## number of years
  N_Plot_flower = N_Plot_flower, ## number of plots
  plot_flower = plot_flower, ## predictor plots
  year_flower = year_flower, ## predictor years
  #### Viability Variables
  N_viab = N_viab, ## number of observations
  good_viab = good_viab,
  abort_viab = abort_viab, ## aborted buds data
  tot_viab = tot_viab, ## number of trials
  ant_viab = ant_viab,## predictors ants
  N_ant = N_ant, ## number of ant states
  N_Year_viab = N_Year_viab, ## number of years
  N_Plot_viab = N_Plot_viab, ## number of plots
  plot_viab = plot_viab, ## predictor plots
  year_viab = year_viab, ## predictor years
  #### Reproductive State Variables
  N_repro = N_repro, ## number of observations
  vol1_repro = vol1_repro, ## predictors volume
  y_repro = y_repro, ## response volume next year
  N_Year_repro = N_Year_repro, ## number of years
  N_Plot_repro = N_Plot_repro, ## number of plots
  plot_repro = plot_repro, ## predictor plots
  year_repro = year_repro, ## predictor years
  #### Seed Prod Variables
  N_seed = nrow(seed_data),
  N_ant_seed = 3,
  ant_seed = seed_data$ant,
  seed = seed_data$seed_count,
  #### Seed Surv Variables
  on_ground = fruit.surv$Fr.on.grnd.not.chewed,
  on_plant = fruit.surv$Fr.on.plant,
  fr_prop = fruit.surv$Fr.on.grnd.not.chewed/fruit.surv$Fr.on.plant,
  N_seed_s = nrow(fruit.surv),
  #### Germ 1 Variables
  N_germ = nrow(germ.dat),
  y_germ1 = as.integer(germ.dat$Seedlings04),
  trials_germ1 = germ.dat$Input,
  #### Germ 2 Variables
  N_germ = nrow(germ.dat),
  y_germ2 = germ.dat$Seedlings05,
  trials_germ2 = germ.dat$Input-germ.dat$Seedlings04,
  #### Precensus Surv Variables
  N_precen = nrow(precensus.dat),
  N_Plant_precen = length(unique(precensus.dat$plant.ID)),
  N_Transect_precen = length(unique(precensus.dat$Transect)),
  vol_precen = precensus.dat$Log.size,
  plant_precen = as.integer(as.factor(precensus.dat$plant.ID)),
  transect_precen = as.integer(as.factor(precensus.dat$Transect)),
  y_precen = precensus.dat$survive0405,
  #### Recruit Variables
  N_rec = length(seedlings$volume_t),
  y_rec = (seedlings$volume_t),
  #### Ants Variables
  N_obs = nrow(ant.dat2),
  success = ant.dat2$ant,
  N_ant = 2,
  ant = ant.dat2$ant,
  N_year = length(unique(ant.dat2$Year_t)),
  year = as.factor(as.integer(ant.dat2$Year_t)),
  trials = ant.dat2$sum,
  vol_ant = ant.dat2$volume_t
  
)

fit_full_one <- stan(file = "STAN Models/full_vitals_one.stan", data = stan_data, warmup = 500, iter = 1000, chains = 3, cores = 2, thin = 1)
full_one_outputs <- rstan::extract(fit_full_one, pars = c("beta0_g","beta1_g","u_g","w_g","sigma_g","sigma_u_g","sigma_w_g", 
                                                  "beta0_s","beta1_s","u_s","w_s","sigma_s","sigma_u_s","sigma_w_s",
                                                  "beta0_f","beta1_f","u_f","w_f","sigma_f","sigma_u_f","sigma_w_f", "phi_f",
                                                  "beta0_r","beta1_r","u_r","w_r","sigma_r","sigma_u_r","sigma_w_r",
                                                  "beta0_v","u_v","w_v","sigma_v","sigma_u_v","sigma_w_v",
                                                  "beta0_seed","sigma_seed","phi_seed",
                                                  "beta0_seed_s","sigma_seed_s",
                                                  "beta0_germ1","beta1_germ1","sigma_germ1","phi_germ1",
                                                  "beta0_germ2","beta1_germ2","sigma_germ2","phi_germ2",
                                                  "beta0_precen","beta1_precen","sigma_precen",
                                                  "beta0_rec","sigma_rec",
                                                  "beta0_ant","beta1_ant","sigma_ant"
)
)
write.csv(full_one_outputs, "params_one_outputs.csv")

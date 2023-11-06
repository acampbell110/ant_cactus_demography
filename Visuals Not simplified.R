setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
source( "/Users/alicampbell/Documents/GitHub/ant_cactus_demography/02_cholla_ant_IPM_vital_rates.R")
size_dummy <- seq(min(cactus$logsize_t, na.rm = T), max(cactus$logsize_t, na.rm = TRUE), by = 0.1)

################################################################################
## Growth model
################################################################################
################################################################################
## Read in the model fits as an RDS file and extract the saved parameters
fit_grow_stud<-readRDS("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_grow_student_t.rds")
grow_out <- rstan::extract(fit_grow_stud)
# fit_grow_stud@model_pars
## Visualize the outputs of the model -- trace plots to check conve4rgence, data moments to check fit
png("grow_conv.png")
bayesplot::mcmc_trace(fit_grow_stud,pars=c("d_0","d_size","a_0","a_size",
                                           "beta0[1]","beta0[2]","beta0[3]","beta0[4]",
                                           "beta1[1]","beta1[2]","beta1[3]","beta1[4]",
                                           "beta2[1]","beta2[2]","beta2[3]","beta2[4]"))
dev.off()
# # Check the different quantile fits of the model to make sure not only the mean but also other quantiles fit well with the real data
# # real data moments
# q.fit<-matrix(NA,7,length(stan_data_grow_stud$vol))
# q.fit[1,]<-predict(qgam(y~s(vol),qu=0.05,data=data.frame(y=stan_data_grow_stud$y,vol=stan_data_grow_stud$vol)))
# q.fit[2,]<-predict(qgam(y~s(vol),qu=0.10,data=data.frame(y=stan_data_grow_stud$y,vol=stan_data_grow_stud$vol)))
# q.fit[3,]<-predict(qgam(y~s(vol),qu=0.25,data=data.frame(y=stan_data_grow_stud$y,vol=stan_data_grow_stud$vol)))
# q.fit[4,]<-predict(qgam(y~s(vol),qu=0.5,data=data.frame(y=stan_data_grow_stud$y,vol=stan_data_grow_stud$vol)))
# q.fit[5,]<-predict(qgam(y~s(vol),qu=0.75,data=data.frame(y=stan_data_grow_stud$y,vol=stan_data_grow_stud$vol)))
# q.fit[6,]<-predict(qgam(y~s(vol),qu=0.90,data=data.frame(y=stan_data_grow_stud$y,vol=stan_data_grow_stud$vol)))
# q.fit[7,]<-predict(qgam(y~s(vol),qu=0.95,data=data.frame(y=stan_data_grow_stud$y,vol=stan_data_grow_stud$vol)))
# obs_mean<-Q.mean(q.fit[3,],q.fit[4,],q.fit[5,])
# obs_sd<-Q.sd(q.fit[3,],q.fit[5,])
# obs_skew<-Q.skewness(q.fit[2,],q.fit[4,],q.fit[6,])
# obs_kurt<-Q.kurtosis(q.fit[1,],q.fit[3,],q.fit[5,],q.fit[7,])
# # simulate data 
# n_draws=25
# grow_sim<-matrix(NA,n_draws,stan_data_grow_stud$N)
# sim_mean<-sim_sd<-sim_skew<-sim_kurt<-matrix(NA,n_draws,stan_data_grow_stud$N)
# for(i in 1:n_draws){
#   for(n in 1:stan_data_grow_stud$N){
#     grow_sim[i,n]<-rlst(n=1,mu=grow_out$beta0[i,stan_data_grow_stud$ant[n]]+
#                                grow_out$beta1[i,stan_data_grow_stud$ant[n]]*stan_data_grow_stud$vol[n]+
#                                grow_out$beta2[i,stan_data_grow_stud$ant[n]]*stan_data_grow_stud$vol2[n]+
#                                grow_out$u[i,stan_data_grow_stud$plot[n]],#+
#                                #grow_out$w[i,stan_data_grow_stud$ant[n],stan_data_grow_stud$year[n]],
#                            sigma=exp(grow_out$d_0[i]+grow_out$d_size[i]*stan_data_grow_stud$vol[n]),
#                            df=grow_out$a_0[i]+grow_out$a_size[i]*stan_data_grow_stud$vol[n])
#   }
#   q.fit[1,]<-predict(qgam(y~s(vol),qu=0.05,data=data.frame(y=grow_sim[i,],vol=stan_data_grow_stud$vol)))
#   q.fit[2,]<-predict(qgam(y~s(vol),qu=0.10,data=data.frame(y=grow_sim[i,],vol=stan_data_grow_stud$vol)))
#   q.fit[3,]<-predict(qgam(y~s(vol),qu=0.25,data=data.frame(y=grow_sim[i,],vol=stan_data_grow_stud$vol)))
#   q.fit[4,]<-predict(qgam(y~s(vol),qu=0.5,data=data.frame(y=grow_sim[i,],vol=stan_data_grow_stud$vol)))
#   q.fit[5,]<-predict(qgam(y~s(vol),qu=0.75,data=data.frame(y=grow_sim[i,],vol=stan_data_grow_stud$vol)))
#   q.fit[6,]<-predict(qgam(y~s(vol),qu=0.90,data=data.frame(y=grow_sim[i,],vol=stan_data_grow_stud$vol)))
#   q.fit[7,]<-predict(qgam(y~s(vol),qu=0.95,data=data.frame(y=grow_sim[i,],vol=stan_data_grow_stud$vol)))
#   sim_mean[i,]<-Q.mean(q.fit[3,],q.fit[4,],q.fit[5,]) 
#   sim_sd[i,]<-Q.sd(q.fit[3,],q.fit[5,])  
#   sim_skew[i,]<-Q.skewness(q.fit[2,],q.fit[4,],q.fit[6,])
#   sim_kurt[i,]<-Q.kurtosis(q.fit[1,],q.fit[3,],q.fit[5,],q.fit[7,])
#   print(i/n_draws)  
# }
# plot(stan_data_grow_stud$vol,grow_sim[1,],pch=".",col="red")
# points(stan_data_grow_stud$vol,stan_data_grow_stud$y,pch=".",col="black")
# bayesplot::ppc_dens_overlay(stan_data_grow_stud$y, grow_sim)
# matplot(stan_data_grow_stud$vol,t(sim_mean),pch=".",col="gray")
# points(stan_data_grow_stud$vol,obs_mean)
# matplot(stan_data_grow_stud$vol,t(sim_sd),pch=".",col="gray")
# points(stan_data_grow_stud$vol,obs_sd)
# matplot(stan_data_grow_stud$vol,t(sim_skew),pch=".",col="gray")
# points(stan_data_grow_stud$vol,obs_skew)
# matplot(stan_data_grow_stud$vol,t(sim_kurt),pch=".",col="gray")
# points(stan_data_grow_stud$vol,obs_kurt)



## Format the original data
y_subset <- growth_data[,c("logsize_t1","ant_t", "logsize_t")]
y_crem_subset_grow <- subset(y_subset, ant_t == "crem")
y_liom_subset_grow <- subset(y_subset, ant_t == "liom")
y_vac_subset_grow <- subset(y_subset, ant_t == "vacant")
y_other_subset_grow <- subset(y_subset, ant_t == "other")
## Size dummies for each subset
size_crem <- seq(min(y_crem_subset_grow$logsize_t, na.rm = TRUE), max(y_crem_subset_grow$logsize_t, na.rm = TRUE), by = 0.1)
size_liom <- seq(min(y_liom_subset_grow$logsize_t, na.rm = TRUE), max(y_liom_subset_grow$logsize_t, na.rm = TRUE), by = 0.1)
size_other <- seq(min(y_other_subset_grow$logsize_t, na.rm = TRUE), max(y_other_subset_grow$logsize_t, na.rm = TRUE), by = 0.1)
size_vac <- seq(min(y_vac_subset_grow$logsize_t, na.rm = TRUE), max(y_vac_subset_grow$logsize_t, na.rm = TRUE), by = 0.1)
y_other_mean_grow <- quantile(grow_out$beta0[,3],0.5) + (size_dummy) * quantile(grow_out$beta1[,3],0.5) + (size_dummy)^2 * quantile(grow_out$beta2[,3],0.5)
# Crem
y_crem_mean_grow <- quantile(grow_out$beta0[,1],0.5) + (size_dummy) * quantile(grow_out$beta1[,1],0.5) + (size_dummy)^2 * quantile(grow_out$beta2[,1],0.5)
# Liom
y_liom_mean_grow <- quantile(grow_out$beta0[,2],0.5) + (size_dummy) * quantile(grow_out$beta1[,2],0.5) + (size_dummy)^2 * quantile(grow_out$beta2[,2],0.5)
# Vac
y_vac_mean_grow <-  quantile(grow_out$beta0[,4],0.5) + (size_dummy) * quantile(grow_out$beta1[,4],0.5) + (size_dummy)^2 * quantile(grow_out$beta2[,4],0.5)

## Create a contour plot which shows the full fit of the growth model rather than just the mean
x <- seq(min(cactus$logsize_t, na.rm = T),max(cactus$logsize_t,na.rm = T), length = 25); # three columns
y <- seq(min(cactus$logsize_t1, na.rm = T),max(cactus$logsize_t1,na.rm = T), length = 25); # five rows
other <- outer (
  y,     # First dimension:  the columns (y)
  x,     # Second dimension: the rows    (x)
  function (x, y)   dlst(y,mu=quantile(grow_out$beta0[,3],0.5) + quantile(grow_out$beta1[,3],0.5)*x + quantile(grow_out$beta2[,3],0.5)*x^2, 
                         sigma = exp(quantile(grow_out$d_0,0.5) + x * quantile(grow_out$d_size,0.5)), 
                         df = exp(quantile(grow_out$a_0,0.5) + x * quantile(grow_out$a_size,0.5)))
);
vacant <- outer (
  y,     # First dimension:  the columns (y)
  x,     # Second dimension: the rows    (x)
  function (x, y)   dlst(y,mu=quantile(grow_out$beta0[,4],0.5) + quantile(grow_out$beta1[,4],0.5)*x + quantile(grow_out$beta2[,4],0.5)*x^2, 
                         sigma = exp(quantile(grow_out$d_0,0.5) + x * quantile(grow_out$d_size,0.5)), 
                         df = exp(quantile(grow_out$a_0,0.5) + x * quantile(grow_out$a_size,0.5)))
);
liom <- outer (
  y,     # First dimension:  the columns (y)
  x,     # Second dimension: the rows    (x)
  function (x, y)   dlst(y,mu=quantile(grow_out$beta0[,2],0.5) + quantile(grow_out$beta1[,2],0.5)*x + quantile(grow_out$beta2[,2],0.5)*x^2, 
                         sigma = exp(quantile(grow_out$d_0,0.5) + x * quantile(grow_out$d_size,0.5)), 
                         df = exp(quantile(grow_out$a_0,0.5) + x * quantile(grow_out$a_size,0.5)))
);
crem <- outer (
  y,     # First dimension:  the columns (y)
  x,     # Second dimension: the rows    (x)
  function (x, y)   dlst(y,mu=quantile(grow_out$beta0[,1],0.5) + quantile(grow_out$beta1[,1],0.5)*x + quantile(grow_out$beta2[,1],0.5)*x^2, 
                         sigma = exp(quantile(grow_out$d_0,0.5) + x * quantile(grow_out$d_size,0.5)), 
                         df = exp(quantile(grow_out$a_0,0.5) + x * quantile(grow_out$a_size,0.5)))
);
## Plot the countour lines of the studetn t growth model with the mean fit of the model and the real data
png("grow.png")
par(mar=c(3,3,3,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4,5,5),
              ncol = 3, byrow = TRUE), heights = c(1.4,1.4), widths = c(3.9,3.9,3.9))
# Crem
contour(x,y,crem, nlevels = 20,  xlim = c(2,10), ylim = c(0,10), 
        main = "a)       Crem.               ", cex.main = 2,lwd=1.5,col="black") 
points(y_crem_subset_grow$logsize_t, y_crem_subset_grow$logsize_t1,col=alpha(cremcol,0.5),pch=16,cex=0.75)
lines(size_dummy, y_crem_mean_grow, col = cremcol, lwd = 4)
# Liom
contour(x,y,liom, nlevels = 20, col = "black", xlim = c(2,10), ylim = c(0,10), 
        main = "b)      Liom.                ", cex.main = 2, lwd = 1.5) 
points(y_liom_subset_grow$logsize_t, y_liom_subset_grow$logsize_t1,col=alpha(liomcol,0.5),pch=16,cex=0.75)
lines(size_dummy, y_liom_mean_grow, col = liomcol, lwd = 4)
# Other
contour(x,y,other, nlevels = 20, col = "black", xlim = c(2,10), ylim = c(0,10), 
        main = "c)       Other                ", cex.main = 2, lwd = 1.5) 
points(y_other_subset_grow$logsize_t, y_other_subset_grow$logsize_t1,col=alpha(othercol,0.5),pch=16,cex=0.75)
lines(size_dummy, y_other_mean_grow, col = othercol, lwd = 4)
# Vacant
contour(x,y,vacant, nlevels = 20, col = "black", xlim = c(2,10), ylim = c(0,10), 
        main = "d)      Vacant                ", cex.main = 2, lwd = 1.5) 
points(y_vac_subset_grow$logsize_t, y_vac_subset_grow$logsize_t1,col=alpha(vaccol,0.5),pch=16,cex=0.75)
lines(size_dummy, y_vac_mean_grow, col = vaccol, lwd = 4)
# All together
plot(size_dummy, y_crem_mean_grow, type = "l", col = cremcol, lwd = 3, xlim = c(-4,6), ylim = c(-4,6), 
     main = "e)                      All Ants                           ", cex.main = 2) 
lines(size_dummy, y_liom_mean_grow, col = liomcol, lwd = 3)
lines(size_dummy, y_other_mean_grow, col = othercol, lwd = 3)
lines(size_dummy, y_vac_mean_grow, col = vaccol, lwd = 3)
lines(size_dummy, size_dummy, col = "grey", lty = 2)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c(othercol,cremcol,liomcol,vaccol), pch = 16)
mtext("Log(Volume Year t)",side=1,line=0,outer=TRUE,cex=2)
mtext("Log(Volume Year t+1)",side=2,line=0,outer=TRUE,cex=2)
dev.off()


################################################################################
## Survival Model
################################################################################
## Read in the model fits
fit_surv<-readRDS("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_surv.rds")
surv_out <- rstan::extract(fit_surv)
names(surv_out)
## Visualize the outputs of the model -- trace plots to check convergence, overlay plots to check the fit
# simulate data based on model fits
y <- stan_data_surv$y_surv
ant <- stan_data_surv$ant
n_draws = 100
surv_sim <- matrix(NA, n_draws,stan_data_surv$N)
for(i in 1:n_draws){
    for(n in 1:stan_data_surv$N){
      surv_sim[i,n]<-rbern(n=1,prob=invlogit(surv_out$beta0[i,stan_data_surv$ant[n]]+surv_out$beta1[i,stan_data_surv$ant[n]]*stan_data_surv$vol[n]))
    }
  }
# Overlay Plots
png(file = "surv_post_fit.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay_grouped(y,surv_sim,group = ant)
dev.off()
# Convergence Plots
png(file = "surv_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_surv, pars=c("beta0")))
dev.off()
## Format the original data
y_subset <- survival_data[,c("logsize_t","ant_t", "Survival_t1")]
## Create subsets for each ant species
y_crem_subset_surv <- subset(survival_data, ant_t == "crem")
y_liom_subset_surv <- subset(survival_data, ant_t == "liom")
y_vac_subset_surv <- subset(survival_data, ant_t == "vacant")
y_other_subset_surv <- subset(survival_data, ant_t == "other")
#Size Dummies for every ant
size_crem = seq(min((y_crem_subset_surv$logsize_t), na.rm = TRUE), max ((y_crem_subset_surv$logsize_t), na.rm = TRUE), by = 0.1)
size_other = seq(min((y_other_subset_surv$logsize_t), na.rm = TRUE), max ((y_other_subset_surv$logsize_t), na.rm = TRUE), by = 0.1)
size_liom = seq(min((y_liom_subset_surv$logsize_t), na.rm = TRUE), max ((y_liom_subset_surv$logsize_t), na.rm = TRUE), by = 0.1)
size_vac = seq(min((y_vac_subset_surv$logsize_t), na.rm = TRUE), max ((y_vac_subset_surv$logsize_t), na.rm = TRUE), by = 0.1)
## Formulas -- mean, 95% and 5% percentiles
percentiles <- function(ant,percent){
  if(ant == "crem"){a <- 1}
  if(ant == "liom"){a <- 2}
  if(ant == "other"){a <- 3}
  if(ant == "vacant"){a <- 4}
  #y_surv <- quantile(surv_out$beta0[,a],percent) + size_dummy * quantile(surv_out$beta1[,a],percent)
  y_surv <- mean(surv_out$beta0[,a]) + size_dummy * mean(surv_out$beta1[,a])
  return(y_surv)
}
mean(invlogit(percentiles("vacant",0.95)))
## Bin the size data
# Crem
surv_plot_crem <- y_crem_subset_surv %>% 
  mutate(size_bin = cut_interval((logsize_t),10)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t),na.rm=T),
            surv = mean(Survival_t1,na.rm=T),
            N = length(logsize_t))
surv_plot_crem$N_mod <- log(surv_plot_crem$N)
# Liom
surv_plot_liom <- y_liom_subset_surv %>% 
  mutate(size_bin = cut_interval((logsize_t),10)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t),na.rm=T),
            surv = mean(Survival_t1,na.rm=T),
            N = length(logsize_t))
surv_plot_liom$N_mod <- log(surv_plot_liom$N)
# Other
surv_plot_other <- y_other_subset_surv %>% 
  mutate(size_bin = cut_interval((logsize_t),10)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t),na.rm=T),
            surv = mean(Survival_t1,na.rm=T),
            N = length(logsize_t))
surv_plot_other$N_mod <- log(surv_plot_other$N)
# Vac
surv_plot_vac <- y_vac_subset_surv %>% 
  mutate(size_bin = cut_interval((logsize_t),10)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t),na.rm=T),
            surv = mean(Survival_t1,na.rm=T),
            N = length(logsize_t))
surv_plot_vac$N_mod <- log(surv_plot_vac$N)
## Plot the survival rates of cacti across size with different ant partners
png("survival_plot.png")
par(mar=c(3,3,3,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4,5,5),ncol = 3, byrow = TRUE), heights = c(1.5,1.5), widths = c(3.9,3.9,3.9))
# Crem
plot(x = size_dummy  ,y = invlogit(percentiles("crem",0.5)), type = "l", col = cremcol, lwd = 4, ylim = c(0.6,1), xlim = c(1,15),cex.main = 2, main = "a)           Crem.         ")
points(surv_plot_crem$mean_size,surv_plot_crem$surv,pch=16,cex=surv_plot_crem$N_mod,col= alpha(cremcol, 0.4))
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(percentiles("crem",0.95)), rev(invlogit(percentiles("crem",0.05)))), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Liom
plot(x = size_dummy  ,y = invlogit(percentiles("liom",0.5)), type = "l", col = liomcol, lwd = 4, ylim = c(0.6,1), xlim = c(1,15),cex.main = 2, main = "a)           Liom.         ")
points(surv_plot_liom$mean_size,surv_plot_liom$surv,pch=16,cex=surv_plot_liom$N_mod,col= alpha(liomcol, 0.4))
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(percentiles("liom",0.95)), rev(invlogit(percentiles("liom",0.05)))), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Other
plot(x = size_dummy  ,y = invlogit(percentiles("other",0.5)), type = "l", col = othercol, lwd = 4, ylim = c(0.6,1), xlim = c(1,15),cex.main = 2, main = "a)           Other         ")
points(surv_plot_other$mean_size,surv_plot_other$surv,pch=16,cex=surv_plot_other$N_mod,col= alpha(othercol, 0.4))
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(percentiles("other",0.95)), rev(invlogit(percentiles("other",0.05)))), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# Vacant
plot(x = size_dummy  ,y = invlogit(percentiles("vacant",0.5)), type = "l", col = vaccol, lwd = 4, ylim = c(0.6,1), xlim = c(1,15),cex.main = 2, main = "a)           Vac.         ")
points(surv_plot_vac$mean_size,surv_plot_vac$surv,pch=16,cex=surv_plot_vac$N_mod,col= alpha(vaccol, 0.4))
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(percentiles("vacant",0.95)), rev(invlogit(percentiles("vacant",0.05)))), col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
# All together
plot(x = size_dummy, y = invlogit(percentiles("other",0.5)), type = "l", col = othercol, lwd = 2, ylim = c(0.2,1), lty = 2, xlim = c(0.2,15), cex.main = 2, main = "e)                          All Ants                          ")
lines(x = size_dummy, y = invlogit(percentiles("crem",0.5)), col = cremcol,lwd = 2, lty = 2)
lines(x = size_dummy, y = invlogit(percentiles("liom",0.5)), col = liomcol, lwd = 2, lty = 2)
lines(x = size_dummy, y = invlogit(percentiles("vacant",0.5)), col = vaccol, lwd = 2, lty = 2)
lines(x = size_other, y = invlogit(quantile(surv_out$beta0[,3],.5) + size_other * quantile(surv_out$beta1[,3],.5)), col = othercol, lwd = 3)
lines(x = size_crem, y = invlogit(quantile(surv_out$beta0[,1],.5) + size_crem * quantile(surv_out$beta1[,1],.5)), col = cremcol, lwd = 3)
lines(x = size_liom, y = invlogit(quantile(surv_out$beta0[,2],.5) + size_liom * quantile(surv_out$beta1[,2],.5)), col = liomcol, lwd = 3)
lines(x = size_vac, y = invlogit(quantile(surv_out$beta0[,4],.5) + size_vac * quantile(surv_out$beta1[,4],.5)), col = vaccol, lwd = 3)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c(othercol,cremcol,liomcol,vaccol), pch = 16,
       cex = 2)
mtext("Log(Volume)",side=1,line=0,outer=TRUE,cex=2)
mtext("Probability of Survival",side=2,line=0,outer=TRUE,cex=2,las=0)
dev.off()
################################################################################
## Flowering Model
################################################################################
## Read in the model fits
fit_flow<-readRDS("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_flow.rds")
flow_out <- rstan::extract(fit_flow)
names(flow_out)
## Visualize the outputs of the model -- trace plots to check convergence, overlay plots to check the fit
# simulate data based on model fits
y <- stan_data_flow_trunc$y_flow
n_draws = 1000
flow_sim <- matrix(NA, n_draws,stan_data_flow_trunc$N)
for(i in 1:n_draws){
  for(n in 1:stan_data_flow_trunc$N){
    flow_sim[i,n] <- sample(x=1:n_draws,size=1,replace=T,prob=dnbinom(1:n_draws, mu = exp(flow_out$beta0[i] + flow_out$beta1[i]*stan_data_surv$vol[n]), size=flow_out$phi[i]) / (1 - dnbinom(0, mu = exp(flow_out$beta0[i] + flow_out$beta1[i]*stan_data_surv$vol[n]), size=flow_out$phi[i])))
  }
}
## Plot the posterior distributions
png("flow_post.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay(y, flow_sim)
dev.off()
## Convergence Plots
png(file = "flow_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_flow, pars=c("beta0", "beta1","phi")))
dev.off()
## Formulas -- mean, 95% and 5% percentiles
percentiles <- function(percent){
  y_flow <- quantile(flow_out$beta0,percent) + size_dummy * quantile(flow_out$beta1,percent)
  return(y_flow)
}
## Bin the data
flow_plot <- flower_data %>% 
  mutate(size_bin = cut_interval((logsize_t),10)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t),na.rm=T),
            tot = mean(TotFlowerbuds_t,na.rm=T),
            N = length(logsize_t),
            N_mod = log(N))
## Plot the mean estimate of how many flowers are produced based on the size of the plant alongside the real data and teh estimation errors
png("flow.png")
par(mar=c(4,4,1,1))
plot(x = size_dummy  ,y = exp(percentiles(.5)), type = "l", col = "chartreuse4", lwd = 4, ylim = c(0,100), xlab = " ", ylab = " ")
points(flow_plot$mean_size,flow_plot$tot,pch=16,cex=flow_plot$N_mod,col= alpha("chartreuse4", 0.4))
lines(x = size_dummy, y = exp(percentiles(.05)), type = "l", col = "darkgrey", lty = 2, lwd = 2)
lines(x = size_dummy, y = exp(percentiles(.95)), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c(size_dummy,rev(size_dummy)),c(exp(percentiles(.95)), rev(exp(percentiles(.05)))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
mtext("Log(Volume)",side=1,line=-1.5,outer=TRUE,cex=1.7)
mtext("Total Number of Flowers Produced",side=2,line=-1.5,outer=TRUE,cex=1.7,las=0)
dev.off()


################################################################################
## Viability Model
################################################################################
## Read in the model fits
fit_viab<-readRDS("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_viab.rds")
viab_out <- rstan::extract(fit_viab)
## Visualize the outputs of the model -- trace plots to check convergence, overlay plots to check the fit
# simulate data based on model fits
y <- stan_data_viab$tot
ant <- stan_data_viab$ant
n_draws = 1000
viab_sim <- matrix(NA, n_draws,stan_data_viab$N)
for(i in 1:n_draws){
  for(n in 1:stan_data_viab$N){
     viab_sim[i,n] <- rbern(n = 1, prob = invlogit(viab_out$beta0[i,stan_data_viab$ant[n]]))
  }
}
# Plot the posterior distributions
png("viab_post.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay_grouped(y, viab_sim, group = ant)
dev.off()
# Convergence Plots
png(file = "viab_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_viab, pars=c("beta0")))
dev.off()
## Format the original data for figures
# calculate the viability rates of the real data
for(i in 1:nrow(viability_data)){
  viability_data$viab[i] <- viability_data$Goodbuds_t1[i]/viability_data$TotFlowerbuds_t1[i]
}
# subset the data by ant partner
other_subset <- subset(viability_data, ant_t == "other")
crem_subset <- subset(viability_data, ant_t == "crem")
liom_subset <- subset(viability_data, ant_t == "liom")
vac_subset <- subset(viability_data, ant_t == "vacant")
## Plot the histograms of the actual data alongside the mean estimates of viability rates by ant state
png("viab_hist.png")
par(mar=c(5,6,3,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(1,1,1,1)) 
# crem
hist(crem_subset$viab, xlim = c(0,1), prob = TRUE, ylim = c(0,12), col = cremcol, cex.main = 2,xlab = "",ylab = "",main = "a)                                               Crem.                                                 ")
lines(density(invlogit(viab_out$beta0[,1])), lwd = 3, col = cremcol)
# liom
hist(liom_subset$viab, xlim = c(0,1), prob = TRUE, ylim = c(0,12), col = liomcol, cex.main = 2, xlab = "",ylab = "",main = "b)                                               Liom.                                                 ")
lines(density(invlogit(viab_out$beta0[,2])), lwd = 3, col = liomcol)
# other
hist(other_subset$viab, xlim = c(0,1), prob = TRUE, ylim = c(0,12), col = othercol, cex.main = 2, xlab = "",ylab = "",main = "c)                                              Other                                                  ")
lines(density(invlogit(viab_out$beta0[,3])), lwd = 3, col = othercol)
# vacant
hist(vac_subset$viab, xlim = c(0,1), prob = TRUE, ylim = c(0,12), col = vaccol, cex.main = 2, xlab = "",ylab = "",main = "d)                                           Vacant                                               ")
lines(density(invlogit(viab_out$beta0[,4])), lwd = 3, col = vaccol) 
mtext("Proportion of Flowerbuds Viable",side=1,line=-1.5,outer=TRUE,cex=2)
mtext("Density",side=2,line=-2,outer=TRUE,cex=2,las=0)
dev.off()

################################################################################
## Repro
################################################################################
## Read in the model fits
fit_repro<-readRDS("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_repro.rds")
repro_out <- rstan::extract(fit_repro)
## Visualize the outputs of the model -- trace plots to check convergence, overlay plots to check the fit
# simulate data based on model fits
y <- as.numeric(stan_data_repro$y_repro)
n_draws = 1000
repro_sim <- matrix(NA, n_draws,stan_data_repro$N)
for(i in 1:n_draws){
  for(n in 1:stan_data_repro$N){
    repro_sim[i,n] <- rbern(n = 1, prob = invlogit(repro_out$beta0[i] + repro_out$beta1[i] * stan_data_repro$vol[n]))
  }
}
# Plot the posterior distributions
png("repro_post.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay(y, repro_sim)
dev.off()
# Convergence Plots
png(file = "repro_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_repro, pars=c("beta0","beta1")))
dev.off()
## Format the original data for the figures
# Formulas -- mean, 95% and 5% percentiles
percentiles <- function(percent){
  y_repro <- quantile(repro_out$beta0,percent) + size_dummy * quantile(repro_out$beta1,percent)
  return(y_repro)
}
# Create a subset which includes the necessary data
## Panel Plot showing the probability of reproducing across sizes with the error
png("repro_panel.png")
plot(x = (size_dummy)  ,y = invlogit(percentiles(.5)), type = "l", col = "chartreuse4",ylim = c(0,1),  lwd = 4,xlab = "Log(Volume)",ylab = "Reproduction Rate")
points(x = stan_data_repro$vol, y =as.numeric(stan_data_repro$y_repro))
lines(x = (size_dummy)  ,y = invlogit(percentiles(.5)), type = "l", col = "chartreuse4", lwd = 4)
lines(x = (size_dummy), y = invlogit(percentiles(.05)), type = "l", col = "darkgrey", lty = 2, lwd = 2)
lines(x = (size_dummy), y = invlogit(percentiles(.95)), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c((size_dummy),rev((size_dummy))),c(invlogit(percentiles(.95)), rev(invlogit(percentiles(.05)))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
dev.off()

################################################################################
## Seeds per flower
################################################################################
## Read in the model fits
fit_surv<-readRDS("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_surv.rds")
surv_out <- rstan::extract(fit_surv)
names(surv_out)
## Visualize the outputs of the model -- trace plots to check convergence, overlay plots to check the fit
# simulate data based on model fits
y <- stan_data_surv$y_surv
ant <- stan_data_surv$ant
n_draws = 100
surv_sim <- matrix(NA, n_draws,stan_data_surv$N)
for(i in 1:n_draws){
  for(n in 1:stan_data_surv$N){
    surv_sim[i,n]<-rbern(n=1,prob=invlogit(surv_out$beta0[i,stan_data_surv$ant[n]]+surv_out$beta1[i,stan_data_surv$ant[n]]*stan_data_surv$vol[n]))
  }
}
# Overlay Plots
png(file = "surv_post_fit.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay_grouped(y,surv_sim,group = ant)
dev.off()
# Convergence Plots
png(file = "surv_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_surv, pars=c("beta0")))
dev.off()
## Format teh original data for 
subset_crem <- subset(seed_data, seed_data$ant_state == "Crem")
summary(subset_crem$seed_count)
subset_liom <- subset(seed_data, seed_data$ant_state == "Liom")
subset_vac <- subset(seed_data, seed_data$ant_state == "Vacant")
## Formulas
y_crem <- seed_out$beta0.1
summary(exp(y_crem))
y_vac <- seed_out$beta0.3
y_liom <- seed_out$beta0.2
y_crem_l <- quantile(seed_out$beta0.1,0.05)
y_vac_l <- quantile(seed_out$beta0.3,0.05)
y_liom_l <- quantile(seed_out$beta0.2,0.05)
y_crem_h <- quantile(seed_out$beta0.1,0.95)
y_vac_h <- quantile(seed_out$beta0.3,0.95)
y_liom_h <- quantile(seed_out$beta0.2,0.95)

seeds <- as.data.frame(matrix(nrow = 3 * nrow(seed_out)))
for(i in 1:nrow(seed_out)){
  seeds$beta[i] <- seed_out$beta0.1[i]
  seeds$ant[i] <- "crem"
  seeds$low[i] <- y_crem_l
  seeds$high[i] <- y_crem_h
}
for(i in 1:nrow(seed_out)){
  seeds$beta[nrow(seed_out) + i] <- seed_out$beta0.2[i]
  seeds$ant[nrow(seed_out) + i] <- "liom"
  seeds$low[i] <- y_liom_l
  seeds$high[i] <- y_liom_h
}
for(i in 1:nrow(seed_out)){
  seeds$beta[nrow(seed_out) + i] <- seed_out$beta0.3[i]
  seeds$ant[nrow(seed_out) + i] <- "vacant"
  seeds$low[i] <- y_vac_l
  seeds$high[i] <- y_vac_h
}
unique(seeds$ant)
boxplot(seed$seed_count~seed$ant_state)

png("seed_prod_hist.png")
par(mar=c(5,5,0,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(.6,1,1,1))
plot.new()
text(0.5,0.25,"Seeds Produced by Ants",cex=2,font=2)
## Crem
hist(subset_crem$seed_count, freq = F, xlab = "", ylab = "", main = "", ylim = c(0,0.05))
lines(density(exp(y_crem)), type = "l", col = cremcol, lwd = 2)
legend("topleft", legend = c("Crem.","Liom.","Vacant"), col = c(cremcol,liomcol,vaccol), pch = 16)
## Liom
hist(subset_liom$seed_count, freq = F, xlab = "", ylab = "", main = "", ylim = c(0,0.05))
lines(density(exp(y_liom)), type = "l", col = liomcol, lwd = 2)
## Vac
hist(subset_vac$seed_count, freq = F, xlab = "", ylab = "", main = "", ylim = c(0,0.06))
lines(density(exp(y_vac)), type = "l", col = vaccol, lwd = 2)
mtext("Number of Seeds Produced",side=1,line=-2,outer=TRUE,cex=1.3)
mtext("Density",side=2,line=-3,outer=TRUE,cex=1.3,las=0)
dev.off()



################################################################################
## Precensus Survival
################################################################################
## Run the model
# fit_seed_surv <- stan(file = "Data Analysis/STAN Models/seed_surv_code.stan", data = stan_data_seed_surv, warmup = 1500, iter = 10000, chains = 3, cores = 3, thin = 1)
# fit_seed_surv@model_pars
# ## pull all iterations for parameters and save as a data frame
# seed_surv_outputs <- rstan::extract(fit_seed_surv, pars = c("beta0","w","sigma_w","mu","sigma"))
# seed_surv_outputs <- as.data.frame(seed_surv_outputs)
# ## pull 1000 random rows from the data frame and export it
# seed.surv.params <- seed_surv_outputs[draws,]
# write.csv(seed.surv.params, "seed.surv.params.csv")
# ## yrep
# seed_surv_yrep <- rstan::extract(fit_seed_surv, pars = c("y_rep"))
# seed_surv_yrep <- as.data.frame(seed_surv_yrep)
# ## pull 1000 random rows from the data frame and export it
# seed.surv.yrep <- seed_surv_yrep[draws,]
# write.csv(seed.surv.yrep, "seed.surv.yrep.csv")
# summary(fit_seed_surv)[1]
# ## Check the Posteriors
# setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
# y <- precensus.dat$survive0405
# seed_surv_mu <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/seed.surv.yrep.csv", header = TRUE,stringsAsFactors=T)
# seed_surv_mu <- seed_surv_mu[,c(-1)]
# seed_surv_mu <- as.matrix(seed_surv_mu)
# ## Overlay Plots
# png(file = "seed_surv_post.png")
# bayesplot::ppc_dens_overlay(y, seed_surv_mu)
# dev.off()
# ## Convergence Plots
# png(file = "seed_surv_conv.png")
# bayesplot::mcmc_trace(As.mcmc.list(fit_seed_surv, pars=c("beta0")))
# dev.off()
# setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
# 


################################################################################
## Germination Models
################################################################################
## Run a model 
# fit_germ1 <- stan(file = "Data Analysis/STAN Models/germ_code.stan", data = stan_data_germ1, warmup = 1500, iter = 10000, chains = 3, cores = 3, thin = 1)
# fit_germ2 <- stan(file = "Data Analysis/STAN Models/germ_code.stan", data = stan_data_germ2, warmup = 1500, iter = 10000, chains = 3, cores = 3, thin = 1)
# fit_germ1@model_pars
# fit_germ2@model_pars
# ## pull all iterations for parameters and save as a data frame
# ## germ 1 params
# germ1_outputs <- rstan::extract(fit_germ1, pars = c("beta0","mu","sigma"))
# germ1_outputs <- as.data.frame(germ1_outputs)
# ## pull 1000 random rows from the data frame and export it
# germ1.params <- germ1_outputs[draws,]
# write.csv(germ1.params, "germ1.params.csv")
# ## germ 2 params
# germ2_outputs <- rstan::extract(fit_germ2, pars = c("beta0","mu","sigma"))
# germ2_outputs <- as.data.frame(germ2_outputs)
# ## pull 1000 random rows from the data frame and export it
# germ2.params <- germ2_outputs[draws,]
# write.csv(germ2.params, "germ2.params.csv")
# ## germ 1 yrep
# germ1_yrep <- rstan::extract(fit_germ1, pars = c("y_rep"))
# germ1_yrep <- as.data.frame(germ1_yrep)
# ## pull 1000 random rows from the data frame and export it
# germ1.yrep <- germ1_yrep[draws,]
# write.csv(germ1.yrep, "germ1.yrep.csv")
# ## germ 2 yrep
# germ2_yrep <- rstan::extract(fit_germ2, pars = c("y_rep"))
# germ2_yrep <- as.data.frame(germ2_yrep)
# ## pull 1000 random rows from the data frame and export it
# germ2.yrep <- germ2_yrep[draws,]
# write.csv(germ2.yrep, "germ2.yrep.csv")
# ## Check the Posteriors
# setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
# ## Germ yr 1
# y <- germ.dat$Seedlings04/germ.dat$Input
# germ1_mu <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/germ1.yrep.csv", header = TRUE,stringsAsFactors=T)
# germ1_mu <- germ1_mu[,c(-1)]
# germ1_mu <- as.matrix(germ1_mu)
# ## Overlay Plots
# png(file = "germ1_post.png")
# bayesplot::ppc_dens_overlay(y, germ1_mu)
# dev.off()
# ## Convergence Plots
# png(file = "germ1_conv.png")
# bayesplot::mcmc_trace(As.mcmc.list(fit_germ1, pars=c("beta0")))
# dev.off()
# ## Germ yr 2
# y <- germ.dat$Seedlings05/(germ.dat$Input - germ.dat$Seedlings04)
# germ2_mu <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/germ2.yrep.csv", header = TRUE,stringsAsFactors=T)
# germ2_mu <- germ2_mu[,c(-1)]
# germ2_mu <- as.matrix(germ2_mu)## Overlay Plots## Overlay Plots
# png(file = "germ2_post.png")
# bayesplot::ppc_dens_overlay(y, germ2_mu)
# dev.off()
# ## Convergence Plots
# png(file = "germ2_conv.png")
# bayesplot::mcmc_trace(As.mcmc.list(fit_germ2, pars=c("beta0")))
# dev.off()

###### Check the significance of the differences between germination rates
## create data set where each column is an estimated survival rate
# germ1_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/germ1.params.csv", header = TRUE,stringsAsFactors=T)
# germ2_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/germ2.params.csv", header = TRUE,stringsAsFactors=T)
# germ1_est <- (invlogit(germ1_out$beta0))
# germ2_est <- (invlogit(germ2_out$beta0))
# estimates <- cbind(germ1_est, germ2_est)
# ## crem and liom -- p = 2.2 e-16  *** 
# t.test(estimates[,1],estimates[,2], alternative = "two.sided")
# 
# setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
# 
# 
# 
################################################################################
## Recuits Model
################################################################################
# fit_rec <- stan(file = "Data Analysis/STAN Models/rec_code.stan",data = stan_data_rec, warmup = 1500, iter = 10000, chains = 3, cores = 3, thin = 1)
# fit_rec@model_pars
# ## pull all iterations for parameters and save as a data frame
# ## recruitment params
# rec_outputs <- rstan::extract(fit_rec, pars = c("beta0","mu","sigma"))
# rec_outputs <- as.data.frame(rec_outputs)
# ## pull 1000 random rows from the data frame and export it
# rec.params <- rec_outputs[draws,]
# write.csv(rec.params, "rec.params.csv")
# ## recruitment yrep
# rec_yrep <- rstan::extract(fit_rec, pars = c("y_rep"))
# rec_yrep <- as.data.frame(rec_yrep)
# ## pull 1000 random rows from the data frame and export it
# rec.yrep <- rec_yrep[draws,]
# write.csv(rec.yrep, "rec.yrep.csv")
# ## Check Posterior Dist
# y <- seedling.dat$logsize_t1
# #rec_mu <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/rec.yrep.csv", header = TRUE,stringsAsFactors=T)
# rec_mu <- read.csv("rec.yrep.csv", header = TRUE,stringsAsFactors=T)
# rec_mu <- rec_mu[,c(-1)]
# rec_mu <- as.matrix(rec_mu)
# ## Overlay Plots
# setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
# png(file = "rec_post.png")
# bayesplot::ppc_dens_overlay(y, rec_mu)
# dev.off()
# ## Convergence Plots
# png(file = "rec_conv.png")
# bayesplot::mcmc_trace(As.mcmc.list(fit_rec, pars=c("beta0")))
# dev.off()
# setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")




################################################################################
## 
################################################################################
























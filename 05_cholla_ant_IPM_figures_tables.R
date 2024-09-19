################################################################################
##  This file visualizes the outputs of the Bayesian Models created in the sourced code
################################################################################
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
source( "/Users/alicampbell/Documents/GitHub/ant_cactus_demography/03_cholla_ant_IPM_params_functions.R")

## Color Codes
## Retro bright
cremcol <- "#9239F6"
liomcol <- "#00A08A"
othercol <- "#FF0076"
vaccol <- "#F8B660"
vcol <- "#ad90ec"
lcol <- "#084f98"
ccol <- "#e9a67a"
ocol <- "#93022f"
lccol <- "#5dc9cf"
locol <- "#cf3545"
cocol <- "#ab59c8"
acol <- "#5d906b"
cols <- c(vcol, ccol, lcol, ocol, lccol, locol, cocol, acol)
                        
str(cactus)
##### Size variable used in most visualizations
size_dummy <- seq(min(cactus$logsize_t, na.rm = T), max(cactus$logsize_t, na.rm = TRUE), by = 0.1)
################################################################################
## Growth Model Visuals
################################################################################
# Visualize the outputs of the model -- trace plots to check conve4rgence, data moments to check fit
png("Manuscript/Figures/grow_conv_skew.png")
bayesplot::mcmc_trace(fit_grow_skew,pars=c("d_0",
                                           "d_size",
                                           "a_0",
                                           "a_size",
                                           "beta0[1]","beta0[2]","beta0[3]","beta0[4]",
                                           "beta1[1]","beta1[2]","beta1[3]","beta1[4]"))
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
## Predicted sizes for each partner condition
omega <- exp(mean(params$grow_sig0) + size_dummy*mean(params$grow_sig1))
alpha <- exp(mean(params$grow_alp0) + size_dummy*mean(params$grow_alp1))
# omega_crem <- exp(mean(params$grow_sig01) + size_dummy * mean(params$grow_sig11))
# omega_liom <- exp(mean(params$grow_sig02) + size_dummy * mean(params$grow_sig12))
# omega_other <- exp(mean(params$grow_sig03) + size_dummy * mean(params$grow_sig13))
# omega_vac <- exp(mean(params$grow_sig04) + size_dummy * mean(params$grow_sig14))
# alpha_crem <- mean(params$grow_alp01) + size_dummy * mean(params$grow_alp11)
# alpha_liom <- mean(params$grow_alp02) + size_dummy * mean(params$grow_alp12)
# alpha_other <- mean(params$grow_alp03) + size_dummy * mean(params$grow_alp13)
# alpha_vac <- mean(params$grow_alp04) + size_dummy * mean(params$grow_alp14)
# Other
y_other_mean_grow <- mean(params$grow_beta03) + (size_dummy) * mean(params$grow_beta13 ) + (size_dummy)^2 * mean(params$grow_beta23)
# Crem
y_crem_mean_grow <- mean(params$grow_beta01) + (size_dummy) * mean(params$grow_beta11) + (size_dummy)^2 * mean(params$grow_beta21)
# Liom
y_liom_mean_grow <- mean(params$grow_beta02) + (size_dummy) * mean(params$grow_beta12) + (size_dummy)^2 * mean(params$grow_beta22)
# Vac
y_vac_mean_grow <-  mean(params$grow_beta04) + (size_dummy) * mean(params$grow_beta14) + (size_dummy)^2 * mean(params$grow_beta24)

other_mean <- y_other_mean_grow + omega * (alpha/(sqrt(1 + alpha^2)) * (sqrt(2/pi)))
crem_mean <- y_crem_mean_grow + omega * (alpha/(sqrt(1 + alpha^2)) * (sqrt(2/pi)))
liom_mean <- y_liom_mean_grow + omega * (alpha/(sqrt(1 + alpha^2)) * (sqrt(2/pi)))
vac_mean <- y_vac_mean_grow + omega * (alpha/(sqrt(1 + alpha^2)) * (sqrt(2/pi)))

## Create a contour plot which shows the full fit of the growth model rather than just the mean
x <- seq(min(cactus$logsize_t, na.rm = T),max(cactus$logsize_t,na.rm = T), length = 25); # three columns
y <- seq(min(cactus$logsize_t1, na.rm = T),max(cactus$logsize_t1,na.rm = T), length = 25); # five rows
# Student T Kernels
# other <- outer (
#   y,     # First dimension:  the columns (y)
#   x,     # Second dimension: the rows    (x)
#   function (x, y)   dlst(y,mu=quantile(grow.params$beta0[,3],0.5) + quantile(grow.params$beta1[,3],0.5)*x + quantile(grow.params$beta2[,3],0.5)*x^2,
#                          sigma = exp(quantile(grow.params$d_0,0.5) + x * quantile(grow.params$d_size,0.5)),
#                          df = exp(quantile(grow.params$a_0,0.5) + x * quantile(grow.params$a_size,0.5)))
# );
# vacant <- outer (
#   y,     # First dimension:  the columns (y)
#   x,     # Second dimension: the rows    (x)
#   function (x, y)   dlst(y,mu=quantile(grow.params$beta0[,4],0.5) + quantile(grow.params$beta1[,4],0.5)*x + quantile(grow.params$beta2[,4],0.5)*x^2,
#                          sigma = exp(quantile(grow.params$d_0,0.5) + x * quantile(grow.params$d_size,0.5)),
#                          df = exp(quantile(grow.params$a_0,0.5) + x * quantile(grow.params$a_size,0.5)))
# );
# liom <- outer (
#   y,     # First dimension:  the columns (y)
#   x,     # Second dimension: the rows    (x)
#   function (x, y)   dlst(y,mu=quantile(grow.params$beta0[,2],0.5) + quantile(grow.params$beta1[,2],0.5)*x + quantile(grow.params$beta2[,2],0.5)*x^2,
#                          sigma = exp(quantile(grow.params$d_0,0.5) + x * quantile(grow.params$d_size,0.5)),
#                          df = exp(quantile(grow.params$a_0,0.5) + x * quantile(grow.params$a_size,0.5)))
# );
# crem <- outer (
#   y,     # First dimension:  the columns (y)
#   x,     # Second dimension: the rows    (x)
#   function (x, y)   dlst(y,mu=quantile(grow.params$beta0[,1],0.5) + quantile(grow.params$beta1[,1],0.5)*x + quantile(grow.params$beta2[,1],0.5)*x^2,
#                          sigma = exp(quantile(grow.params$d_0,0.5) + x * quantile(grow.params$d_size,0.5)),
#                          df = exp(quantile(grow.params$a_0,0.5) + x * quantile(grow.params$a_size,0.5)))
# );
## Skew Kernels
other <- outer (
  y,     # First dimension:  the columns (y)
  x,     # Second dimension: the rows    (x)
  function (x, y)   dsn(y,xi=mean(params$grow_beta03) + mean(params$grow_beta13)*x + mean(params$grow_beta23)*x^2,
                         omega = exp(mean(params$grow_sig0) + x * mean(params$grow_sig1)),
                         alpha = (mean(params$grow_alp0) + x * mean(params$grow_alp1)))
);
vacant <- outer (
  y,     # First dimension:  the columns (y)
  x,     # Second dimension: the rows    (x)
  function (x, y)   dsn(y,xi=mean(params$grow_beta04) + mean(params$grow_beta14)*x + mean(params$grow_beta24)*x^2,
                        omega = exp(mean(params$grow_sig0) + x * mean(params$grow_sig1)),
                        alpha = (mean(params$grow_alp0) + x * mean(params$grow_alp1)))
);
liom <- outer (
  y,     # First dimension:  the columns (y)
  x,     # Second dimension: the rows    (x)
  function (x, y)   dsn(y,xi=mean(params$grow_beta02) + mean(params$grow_beta12)*x + mean(params$grow_beta22)*x^2,
                        omega = exp(mean(params$grow_sig0) + x * mean(params$grow_sig1)),
                        alpha = (mean(params$grow_alp0) + x * mean(params$grow_alp1)))
);
crem <- outer (
  y,     # First dimension:  the columns (y)
  x,     # Second dimension: the rows    (x)
  function (x, y)   dsn(y,xi=mean(params$grow_beta01) + mean(params$grow_beta11)*x + mean(params$grow_beta21)*x^2,
                        omega = exp(mean(params$grow_sig0) + x * mean(params$grow_sig1)),
                        alpha = (mean(params$grow_alp0) + x * mean(params$grow_alp1)))
);
# ## Skew Kernels -- No Ant
# y_grow <-  quantile(grow.params$beta0,0.5) + (size_dummy) * quantile(grow.params$beta1,0.5)# + (size_dummy)^2 * quantile(grow.params$beta2[,4],0.5)
# grow <- outer (
#   y,     # First dimension:  the columns (y)
#   x,     # Second dimension: the rows    (x)
#   function (x, y)   dsn(y,xi=quantile(grow.params$beta0,0.5) + quantile(grow.params$beta1,0.5)*x,
#                         omega = exp(quantile(grow.params$d_0,0.5) + x * quantile(grow.params$d_size,0.5)),
#                         alpha = (quantile(grow.params$a_0,0.5) + x * quantile(grow.params$a_size,0.5)))
# );
# png("Figures/grow_skew_no_ant.png")
# contour(x,y,grow,nlevels = 15)
# points(y_subset$logsize_t,y_subset$logsize_t1, col = alpha("green",0.5), pch = 16, cex = 0.5)
# lines(size_dummy, y_grow, col = "green", lwd = 4)
# lines(size_dummy,size_dummy, lty = 2, col = "grey")
# dev.off()

##plot showing future size distribution conditional on initial size
##small size is the 5th percentile of tended plants
initsmall<-quantile(cactus$logsize_t[cactus$ant_t!="vacant"],probs=0.01,na.rm=T)
##large size is the 95th percentile of tended plants
initlarge<-quantile(cactus$logsize_t[cactus$ant_t!="vacant"],probs=0.99,na.rm=T)

## Plot the countour lines of the studetn t growth model with the mean fit of the model and the real data
png("Manuscript/Figures/grow_contour_v1.png")
par(mar=c(3,5,3,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4,5,5),
              ncol = 3, byrow = TRUE), heights = c(1.4,1.4), widths = c(3.9,3.9,3.9))
# Crem
contour(x,y,crem, nlevels = 20,  xlim = c(-5,15), ylim = c(-2,15),
        main = "a)       Crem.               ", cex.main = 2,lwd=1.5,col="black")
points(y_crem_subset_grow$logsize_t, y_crem_subset_grow$logsize_t1,col=alpha(cremcol,0.5),pch=16,cex=0.75)
lines(size_dummy, crem_mean, col = cremcol, lwd = 2)
# Liom
contour(x,y,liom, nlevels = 20, col = "black", xlim = c(-5,15), ylim = c(-2,15),
        main = "b)      Liom.                ", cex.main = 2, lwd = 1.5)
points(y_liom_subset_grow$logsize_t, y_liom_subset_grow$logsize_t1,col=alpha(liomcol,0.5),pch=16,cex=0.75)
lines(size_dummy, liom_mean, col = liomcol, lwd = 2)
# Other
contour(x,y,other, nlevels = 30, col = "black", xlim = c(-5,15), ylim = c(-2,15),
        main = "c)       Other                ", cex.main = 2, lwd = 1.5)
points(y_other_subset_grow$logsize_t, y_other_subset_grow$logsize_t1,col=alpha(othercol,0.5),pch=16,cex=0.75)
lines(size_dummy,other_mean, type = "l", col = othercol,lwd = 2)
# Vacant
contour(x,y,vacant, nlevels = 20, col = "black", xlim = c(-5,15), ylim = c(-2,15),
        main = "d)      Vacant                ", cex.main = 2, lwd = 1.5)
points(y_vac_subset_grow$logsize_t, y_vac_subset_grow$logsize_t1,col=alpha(vaccol,0.5),pch=16,cex=0.75)
lines(size_dummy, vac_mean, col = vaccol, lwd = 2)
# All together

plot(size_dummy,dsn(x=size_dummy,
                    xi=mean(params$grow_beta03) + mean(params$grow_beta13)*initlarge + mean(params$grow_beta23)*initlarge^2,
                    omega = exp(mean(params$grow_sig0) + initlarge * mean(params$grow_sig1)),
                    alpha = (mean(params$grow_alp0) + initlarge * mean(params$grow_alp1))),type="l",col=alpha(othercol,0.75),lwd=3,
     xlim=c(0,15),ylab="Probability of size next year",main= "e)                      All Ants                           ", cex.main = 2)
lines(size_dummy,dsn(x=size_dummy,
                     xi=mean(params$grow_beta04) + mean(params$grow_beta14)*initlarge + mean(params$grow_beta24)*initlarge^2,
                     omega = exp(mean(params$grow_sig0) + initlarge * mean(params$grow_sig1)),
                     alpha = (mean(params$grow_alp0) + initlarge * mean(params$grow_alp1))),type="l",col=alpha(vaccol,0.75),lwd=3)
lines(size_dummy,dsn(x=size_dummy,
                     xi=mean(params$grow_beta02) + mean(params$grow_beta12)*initlarge + mean(params$grow_beta22)*initlarge^2,
                     omega = exp(mean(params$grow_sig0) + initlarge * mean(params$grow_sig1)),
                     alpha = (mean(params$grow_alp0) + initlarge * mean(params$grow_alp1))),type="l",col=alpha(liomcol,0.75),lwd=3)
lines(size_dummy,dsn(x=size_dummy,
                     xi=mean(params$grow_beta01) + mean(params$grow_beta11)*initlarge + mean(params$grow_beta21)*initlarge^2,
                     omega = exp(mean(params$grow_sig0) + initlarge * mean(params$grow_sig1)),
                     alpha = (mean(params$grow_alp0) + initlarge * mean(params$grow_alp1))),type="l",col=alpha(cremcol,0.75),lwd=3)
abline(v=initlarge,lty=3)

lines(size_dummy,dsn(x=size_dummy,
                     xi=mean(params$grow_beta03) + mean(params$grow_beta13)*initsmall + mean(params$grow_beta23)*initsmall^2,
                     omega = exp(mean(params$grow_sig0) + initsmall * mean(params$grow_sig1)),
                     alpha = (mean(params$grow_alp0) + initsmall * mean(params$grow_alp1))),type="l",col=alpha(othercol,0.75),lwd=3)
lines(size_dummy,dsn(x=size_dummy,
                     xi=mean(params$grow_beta04) + mean(params$grow_beta14)*initsmall + mean(params$grow_beta24)*initsmall^2,
                     omega = exp(mean(params$grow_sig0) + initsmall * mean(params$grow_sig1)),
                     alpha = (mean(params$grow_alp0) + initsmall * mean(params$grow_alp1))),type="l",col=alpha(vaccol,0.75),lwd=3)
lines(size_dummy,dsn(x=size_dummy,
                     xi=mean(params$grow_beta02) + mean(params$grow_beta12)*initsmall + mean(params$grow_beta22)*initsmall^2,
                     omega = exp(mean(params$grow_sig0) + initsmall * mean(params$grow_sig1)),
                     alpha = (mean(params$grow_alp0) + initsmall * mean(params$grow_alp1))),type="l",col=alpha(liomcol,0.75),lwd=3)
lines(size_dummy,dsn(x=size_dummy,
                     xi=mean(params$grow_beta01) + mean(params$grow_beta11)*initsmall + mean(params$grow_beta21)*initsmall^2,
                     omega = exp(mean(params$grow_sig0) + initsmall * mean(params$grow_sig1)),
                     alpha = (mean(params$grow_alp0) + initsmall * mean(params$grow_alp1))),type="l",col=alpha(cremcol,0.75),lwd=3)
abline(v=initsmall,lty=3)
legend("topleft", legend = c("Other","Crem.","Liom.","Vacant"), col = c(othercol,cremcol,liomcol,vaccol), lwd = 2)

mtext("Log(Volume Year t)",side=1,line=0,outer=TRUE,cex=2)
mtext("Log(Volume Year t+1)",side=2,line=0,outer=TRUE,cex=2)
dev.off()

png("Manuscript/Figures/grow_contour_v2.png")
par(mar=c(3,3,3,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4,5,5),
              ncol = 3, byrow = TRUE), heights = c(1.4,1.4), widths = c(3.9,3.9,3.9))
# Crem
contour(x,y,crem, nlevels = 20,  xlim = c(-5,15), ylim = c(-2,15),
        main = "a)       Crem.               ", cex.main = 2,lwd=1.5,col="black")
points(y_crem_subset_grow$logsize_t, y_crem_subset_grow$logsize_t1,col=alpha(cremcol,0.5),pch=16,cex=0.75)
lines(size_dummy, crem_mean, col = cremcol, lwd = 2)
# Liom
contour(x,y,liom, nlevels = 20, col = "black", xlim = c(-5,15), ylim = c(-2,15),
        main = "b)      Liom.                ", cex.main = 2, lwd = 1.5)
points(y_liom_subset_grow$logsize_t, y_liom_subset_grow$logsize_t1,col=alpha(liomcol,0.5),pch=16,cex=0.75)
lines(size_dummy, liom_mean, col = liomcol, lwd = 2)
# Other
contour(x,y,other, nlevels = 30, col = "black", xlim = c(-5,15), ylim = c(-2,15),
        main = "c)       Other                ", cex.main = 2, lwd = 1.5)
points(y_other_subset_grow$logsize_t, y_other_subset_grow$logsize_t1,col=alpha(othercol,0.5),pch=16,cex=0.75)
lines(size_dummy,other_mean, type = "l", col = othercol,lwd = 2)
# Vacant
contour(x,y,vacant, nlevels = 20, col = "black", xlim = c(-5,15), ylim = c(-2,15),
        main = "d)      Vacant                ", cex.main = 2, lwd = 1.5)
points(y_vac_subset_grow$logsize_t, y_vac_subset_grow$logsize_t1,col=alpha(vaccol,0.5),pch=16,cex=0.75)
lines(size_dummy, vac_mean, col = vaccol, lwd = 2)
# All together
plot(size_dummy, crem_mean, type = "l", col = cremcol, lwd = 3, xlim = c(-5,15), ylim = c(-4,15),
     main = "e)                      All Ants                           ", cex.main = 2)
lines(size_dummy, liom_mean, col = liomcol, lwd = 3)
lines(size_dummy, other_mean, col = othercol, lwd = 3)
lines(size_dummy, vac_mean, col = vaccol, lwd = 3)
lines(size_dummy, size_dummy, col = "grey", lty = 2)
legend("topleft", legend = c("Crem.","Liom.","Other","Vacant"), col = c(cremcol,liomcol,othercol,vaccol), lwd = 2, cex=1)

mtext("Log(Volume Year t)",side=1,line=0,outer=TRUE,cex=2)
mtext("Log(Volume Year t+1)",side=2,line=0,outer=TRUE,cex=2)
dev.off()

################################################################################
## Variance of random effects by year, ant, and model
################################################################################
## Show the correlation between ant and year -- from growth model random effects
g_crem <- colMeans((grow_rfx1))
g_liom <- colMeans((grow_rfx2))
g_other <- colMeans((grow_rfx3))
g_vac <- colMeans((grow_rfx4))
years_seq <- 2004:2023
## Show the correlation between ant and year -- from survival model random effects
s_crem <- colMeans((surv_rfx1))
s_liom <- colMeans((surv_rfx2))
s_other <- colMeans((surv_rfx3))
s_vac <- colMeans((surv_rfx4))
## Show the correlation between ant and year -- from growth model random effects
v_crem <- colMeans((viab_rfx1))
v_liom <- colMeans((viab_rfx2))
v_other <- colMeans((viab_rfx3))
v_vac <- colMeans((viab_rfx4))

pdf("Manuscript/Figures/year_ant_timeseries.pdf",width=10,height=4)
par(mfrow=c(1,3),mar=c(4,2,2,1),oma=c(2,2,0,0))
## Growth Ant EEffects
plot(years_seq,g_liom,col = liomcol, cex.main = 2,type = "b",lwd = 4, pch = 16,cex = 2,
     main = "a)                               ",
     ylim = c(-2.5,2.5), xlab = " ",ylab = " ",cex.lab = 2)
lines(years_seq, g_crem, type = "b", col = cremcol, lwd = 4, pch = 16, cex = 2)
lines(years_seq, g_vac, type = "b", col = vaccol, lwd = 4, pch = 16, cex = 2)
lines(years_seq, g_other, type = "b", col = othercol, lwd = 4, pch = 16, cex = 2)
legend("topleft",legend = c("Liom.","Crem.","Other","Vacant"),fill = c(liomcol,cremcol,othercol,vaccol),cex=1.4)
## Survival ant effects
plot(years_seq,s_liom,col = liomcol, cex.main = 2,type = "b",lwd = 4, pch = 16,cex = 2,
     main = "b)                                 ",
     ylim = c(-2.5,2.5), xlab = "",ylab = " ",cex.lab = 1.5)
lines(years_seq, s_crem, type = "b", col = cremcol, lwd = 4, pch = 16, cex = 2)
lines(years_seq, s_other, type = "b", col = othercol, lwd = 4, pch = 16, cex = 2)
lines(years_seq, s_vac, type = "b", col = vaccol, lwd = 4, pch = 16, cex = 2)
## Viability Ant Effects
plot(years_seq,v_liom,col = liomcol, cex.main = 2,type = "b",lwd = 4, pch = 16,cex = 2,
     main = "c)                                ",
     ylim = c(-2.5,2.5), xlab = " ",ylab = " ",cex.lab = 1.5)
lines(years_seq, v_crem, type = "b", col = cremcol, lwd = 4, pch = 16, cex = 2)
lines(years_seq, v_other, type = "b", col = othercol, lwd = 4, pch = 16, cex = 2)
lines(years_seq, v_vac, type = "b", col = vaccol, lwd = 4, pch = 16, cex = 2)
mtext("Year",side=1,line=0,outer=TRUE,cex=1.4)
mtext("Year-specific deviation",side=2,line=0,outer=TRUE,cex=1.4,las=0)
dev.off()


##### CREM FIGS 
# Corr between crem and crem
ccg <- cor.test(grow_rfx1,grow_rfx1)
ccs <- cor.test(surv_rfx1,surv_rfx1)
ccv <- cor.test(viab_rfx1,viab_rfx1)
# Corr between crem and liom
clg <- cor.test(grow_rfx1,grow_rfx2)
cls <- cor.test(surv_rfx1,surv_rfx2)
clv <- cor.test(viab_rfx1,viab_rfx2)
# Corr between liom and liom
llg <- cor.test(grow_rfx2,grow_rfx2)
lls <- cor.test(surv_rfx2,surv_rfx2)
llv <- cor.test(viab_rfx2,viab_rfx2)
# Corr between crem and other
cog <- cor.test(grow_rfx1,grow_rfx3)
cos <- cor.test(surv_rfx1,surv_rfx3)
cov <- cor.test(viab_rfx1,viab_rfx3)
# Corr between liom and other
log <- cor.test(grow_rfx2,grow_rfx3)
los <- cor.test(surv_rfx2,surv_rfx3)
lov <- cor.test(viab_rfx2,viab_rfx3)
# Corr between other and other
oog <- cor.test(grow_rfx3,grow_rfx3)
oos <- cor.test(surv_rfx3,surv_rfx3)
oov <- cor.test(viab_rfx3,viab_rfx3)
# Corr between crem and vacant
cvg <- cor.test(grow_rfx1,grow_rfx4)
cvs <- cor.test(surv_rfx1,surv_rfx4)
cvv <- cor.test(viab_rfx1,viab_rfx4)
# Corr between liom and vacant
lvg <- cor.test(grow_rfx2,grow_rfx4)
lvs <- cor.test(surv_rfx2,surv_rfx4)
lvv <- cor.test(viab_rfx2,viab_rfx4)
# Corr between other and vacant
ovg <- cor.test(grow_rfx3,grow_rfx4)
ovs <- cor.test(surv_rfx3,surv_rfx4)
ovv <- cor.test(viab_rfx3,viab_rfx4)
# Corr between vacant and vacant
vvg <- cor.test(grow_rfx4,grow_rfx4)
vvs <- cor.test(surv_rfx4,surv_rfx4)
vvv <- cor.test(viab_rfx4,viab_rfx4)

# create matrix 1 = crem, 2 = liom, 3 = other, 4 = vac
a <- matrix(NA, nrow = 4, ncol = 4)
#crem col
a[1,1] <- ccg$estimate
a[1,2] <- clg$estimate
a[1,3] <- cog$estimate
a[1,4] <- cvg$estimate
#liom col
a[2,1] <- clg$estimate
a[2,2] <- llg$estimate
a[2,3] <- log$estimate
a[2,4] <- lvg$estimate
#other col
a[3,1] <- cog$estimate
a[3,2] <- log$estimate
a[3,3] <- oog$estimate
a[3,4] <- ovg$estimate
#vac col
a[4,1] <- cvg$estimate
a[4,2] <- lvg$estimate
a[4,3] <- ovg$estimate
a[4,4] <- vvg$estimate
colnames(a) <- c("Crem.","Liom.","Other","Vacant")
rownames(a) <- c("Crem.","Liom.","Other","Vacant")
png("Manuscript/Figures/corr_GRFX.png")
corrplot(a, method = "shade", col.lim = c(0,1), addCoef.col = T)
dev.off()


# create matrix 1 = crem, 2 = liom, 3 = other, 4 = vac
b <- matrix(NA, nrow = 4, ncol = 4)
#crem col
b[1,1] <- ccs$estimate
b[1,2] <- cls$estimate
b[1,3] <- cos$estimate
b[1,4] <- cvs$estimate
#liom col
b[2,1] <- cls$estimate
b[2,2] <- lls$estimate
b[2,3] <- los$estimate
b[2,4] <- lvs$estimate
#other col
b[3,1] <- cos$estimate
b[3,2] <- los$estimate
b[3,3] <- oos$estimate
b[3,4] <- ovs$estimate
#vac col
b[4,1] <- cvs$estimate
b[4,2] <- lvs$estimate
b[4,3] <- ovs$estimate
b[4,4] <- vvs$estimate
colnames(b) <- c("Crem.","Liom.","Other","Vacant")
rownames(b) <- c("Crem.","Liom.","Other","Vacant")
png("Manuscript/Figures/corr_SRFX.png")
corrplot(b, method = "shade", col.lim = c(0,1), addCoef.col = T)
dev.off()

# create matrix 1 = crem, 2 = liom, 3 = other, 4 = vac
c <- matrix(NA, nrow = 4, ncol = 4)
#crem col
c[1,1] <- ccv$estimate
c[1,2] <- clv$estimate
c[1,3] <- cov$estimate
c[1,4] <- cvv$estimate
#liom col
c[2,1] <- clv$estimate
c[2,2] <- llv$estimate
c[2,3] <- lov$estimate
c[2,4] <- lvv$estimate
#other col
c[3,1] <- cov$estimate
c[3,2] <- lov$estimate
c[3,3] <- oov$estimate
c[3,4] <- ovv$estimate
#vac col
c[4,1] <- cvv$estimate
c[4,2] <- lvv$estimate
c[4,3] <- ovv$estimate
c[4,4] <- vvv$estimate
colnames(c) <- c("Crem.","Liom.","Other","Vacant")
rownames(c) <- c("Crem.","Liom.","Other","Vacant")
png("Manuscript/Figures/corr_VRFX.png")
corrplot(c, method = "shade", col.lim = c(0,1), addCoef.col = T)
dev.off()

mean(a)
mean(b)
mean(c)
colMeans(a)
colMeans(b)
colMeans(c)

png("Manuscript/Figures/corr_RFX.png")
par(mar=c(0,1,0,10),oma=c(0,2,0,2))
layout(matrix(c(1,2,3),ncol = 3, byrow = TRUE), heights = c(1), widths = c(3.9,3.9,3.9))
corrplot(a, method = "shade", col.lim = c(0,1), addCoef.col = T)
corrplot(b, method = "shade", col.lim = c(0,1), addCoef.col = T)
corrplot(c, method = "shade", col.lim = c(0,1), addCoef.col = T)
mtext("   Growth          Survival          Viability",side=3,line=-14,outer=TRUE,cex=2)
dev.off()
################################################################################
## Survival Model Visuals
################################################################################
## Visualize the outputs of the model -- trace plots to check convergence, overlay plots to check the fit
# simulate data based on model fits
y <- stan_data_surv$y_surv
ant <- stan_data_surv$ant
n_draws = 100
surv_sim <- matrix(NA, n_draws,stan_data_surv$N)
for(i in 1:n_draws){
  for(n in 1:stan_data_surv$N){
    surv_sim[i,n]<-rbern(n=1,prob=invlogit(surv.params$beta0[i,stan_data_surv$ant[n]]+surv.params$beta1[i,stan_data_surv$ant[n]]*stan_data_surv$vol[n]))
  }
}
# Overlay Plots
png(file = "Manuscript/Figures/surv_post.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay_grouped(y,surv_sim,group = ant)
dev.off()
# Convergence Plots
png(file = "Manuscript/Figures/surv_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_surv, pars=c("beta0","beta1")))
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
  y_surv <- quantile(surv.params$beta0[,a],percent) + size_dummy * quantile(surv.params$beta1[,a],percent)
  return(y_surv)
}
mean(invlogit(percentiles("vacant",0.05)))
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
png("Manuscript/Figures/survival_plot.png")
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
lines(x = size_other, y = invlogit(quantile(surv.params$beta0[,3],.5) + size_other * quantile(surv.params$beta1[,3],.5)), col = othercol, lwd = 3)
lines(x = size_crem, y = invlogit(quantile(surv.params$beta0[,1],.5) + size_crem * quantile(surv.params$beta1[,1],.5)), col = cremcol, lwd = 3)
lines(x = size_liom, y = invlogit(quantile(surv.params$beta0[,2],.5) + size_liom * quantile(surv.params$beta1[,2],.5)), col = liomcol, lwd = 3)
lines(x = size_vac, y = invlogit(quantile(surv.params$beta0[,4],.5) + size_vac * quantile(surv.params$beta1[,4],.5)), col = vaccol, lwd = 3)
legend("bottomright", legend = c("Other","Crem.","Liom.","Vacant"), col = c(othercol,cremcol,liomcol,vaccol), pch = 16,
       cex = 2)
mtext("Log(Volume)",side=1,line=0,outer=TRUE,cex=2)
mtext("Probability of Survival",side=2,line=0,outer=TRUE,cex=2,las=0)
dev.off()


################################################################################
## Number of Flowers Model Visuals
################################################################################
## Visualize the outputs of the model -- trace plots to check convergence, overlay plots to check the fit
# simulate data based on model fits
y <- stan_data_flow_trunc$y_flow
n_draws = 1000
flow_sim <- matrix(NA, n_draws,stan_data_flow_trunc$N)
for(i in 1:n_draws){
  for(n in 1:stan_data_flow_trunc$N){
    flow_sim[i,n] <- sample(x=1:n_draws,size=1,replace=T,prob=dnbinom(1:n_draws, mu = exp(flow.params$beta0[i] + flow.params$beta1[i]*stan_data_surv$vol[n]), size=flow.params$phi[i]) / (1 - dnbinom(0, mu = exp(flow.params$beta0[i] + flow.params$beta1[i]*stan_data_surv$vol[n]), size=flow.params$phi[i])))
  }
}
## Plot the posterior distributions
png("Manuscript/Figures/flow_post.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay(y, flow_sim)
dev.off()
## Convergence Plots
png(file = "Manuscript/Figures/flow_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_flow, pars=c("beta0", "beta1","phi")))
dev.off()


## Formulas -- mean, 95% and 5% percentiles
percentiles <- function(percent){
  y_flow <- quantile(flow.params$beta0,percent) + size_dummy * quantile(flow.params$beta1,percent)
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
png("Manuscript/Figures/flow.png")
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
## Viability of Flowerbuds Model Visuals
################################################################################
## Visualize the outputs of the model -- trace plots to check convergence, overlay plots to check the fit
# simulate data based on model fits
y <- stan_data_viab$tot
ant <- stan_data_viab$ant
n_draws = 1000
viab_sim <- matrix(NA, n_draws,stan_data_viab$N)
for(i in 1:n_draws){
  for(n in 1:stan_data_viab$N){
    viab_sim[i,n] <- rbern(n = 1, prob = invlogit(viab.params$beta0[i,stan_data_viab$ant[n]]))
  }
}
# Plot the posterior distributions
png("Manuscript/Figures/viab_post.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay_grouped(y, viab_sim, group = ant)
dev.off()
# Convergence Plots
png(file = "Manuscript/Figures/viab_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_viab, pars=c("beta0")))
dev.off()


## Format the original data for figures
# calculate the viability rates of the real data
viability_data$viab <- viability_data$Goodbuds_t/viability_data$TotFlowerbuds_t
# subset the data by ant partner
other_subset <- subset(viability_data, ant_t == "other")
crem_subset <- subset(viability_data, ant_t == "crem")
liom_subset <- subset(viability_data, ant_t == "liom")
vac_subset <- subset(viability_data, ant_t == "vacant")
## Plot the histograms of the actual data alongside the mean estimates of viability rates by ant state
png("Figures/viab_hist.png")
par(mar=c(5,6,3,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(1,1,1,1)) 
# crem
hist(crem_subset$viab, xlim = c(0,1), prob = TRUE, ylim = c(0,12), col = cremcol, cex.main = 2,xlab = "",ylab = "",main = "a)                                               Crem.                                                 ")
lines(density(invlogit(viab.params$beta0[,1])), lwd = 3, col = cremcol)
# liom
hist(liom_subset$viab, xlim = c(0,1), prob = TRUE, ylim = c(0,12), col = liomcol, cex.main = 2, xlab = "",ylab = "",main = "b)                                               Liom.                                                 ")
lines(density(invlogit(viab.params$beta0[,2])), lwd = 3, col = liomcol)
# other
hist(other_subset$viab, xlim = c(0,1), prob = TRUE, ylim = c(0,12), col = othercol, cex.main = 2, xlab = "",ylab = "",main = "c)                                              Other                                                  ")
lines(density(invlogit(viab.params$beta0[,3])), lwd = 3, col = othercol)
# vacant
hist(vac_subset$viab, xlim = c(0,1), prob = TRUE, ylim = c(0,12), col = vaccol, cex.main = 2, xlab = "",ylab = "",main = "d)                                           Vacant                                               ")
lines(density(invlogit(viab.params$beta0[,4])), lwd = 3, col = vaccol) 
mtext("Proportion of Flowerbuds Viable",side=1,line=-1.5,outer=TRUE,cex=2)
mtext("Density",side=2,line=-2,outer=TRUE,cex=2,las=0)
dev.off()

viab_out<-rstan::extract(readRDS(paste0(mcmc_dir,"fit_viab.rds")))
alpha_val<-0.15
png("Manuscript/Figures/viab.png")
plot(1:4,c(1,1,1,1),ylim=c(0,1),type="n",axes=F,xlab="",ylab="",cex.lab=1.4,xlim=c(1,4.25))
points(jitter(rep(1,nrow(crem_subset))),jitter(crem_subset$viab),
       cex=0.5+(crem_subset$TotFlowerbuds_t1/max(viability_data$TotFlowerbuds_t1))*4,
       col=alpha(cremcol,alpha_val),pch=16)
lines(rep(1.25,2),quantile(invlogit(viab_out$beta0[,1]),probs=c(0.025,.975)),
      lwd=3,col=cremcol)
points(1.25,mean(invlogit(viab_out$beta0[,1])),col=cremcol,pch=15,cex=1.5)
points(jitter(rep(2,nrow(liom_subset))),jitter(liom_subset$viab),
       cex=0.5+(liom_subset$TotFlowerbuds_t1/max(viability_data$TotFlowerbuds_t1))*4,
       col=alpha(liomcol,alpha_val),pch=16)
lines(rep(2.25,2),quantile(invlogit(viab_out$beta0[,2]),probs=c(0.025,.975)),
      lwd=3,col=liomcol)
points(2.25,mean(invlogit(viab_out$beta0[,2])),col=liomcol,pch=15,cex=1.5)
points(jitter(rep(3,nrow(other_subset))),jitter(other_subset$viab),
       cex=0.5+(other_subset$TotFlowerbuds_t1/max(viability_data$TotFlowerbuds_t1))*4,
       col=alpha(othercol,alpha_val),pch=16)
lines(rep(3.25,2),quantile(invlogit(viab_out$beta0[,3]),probs=c(0.025,.975)),
      lwd=3,col=othercol)
points(3.25,mean(invlogit(viab_out$beta0[,3])),col=othercol,pch=15,cex=1.5)
points(jitter(rep(4,nrow(vac_subset))),jitter(vac_subset$viab),
       cex=0.5+(vac_subset$TotFlowerbuds_t1/max(viability_data$TotFlowerbuds_t1))*4,
       col=alpha(vaccol,alpha_val),pch=16)
lines(rep(4.25,2),quantile(invlogit(viab_out$beta0[,4]),probs=c(0.025,.975)),
      lwd=3,col=vaccol)
points(4.25,mean(invlogit(viab_out$beta0[,4])),col=vaccol,pch=15,cex=1.5)
axis(1,at=1:4,labels=c(expression(italic("C.opuntiae")),expression(italic("L.apiculatum")),"Other","Vacant"))
mtext("Log(Volume)",side=1,line=-1.5,outer=TRUE,cex=1.7)
mtext("Flowerbud Viability",side=2,line=-1.5,outer=TRUE,cex=1.7,las=0)
box()
dev.off()
## min buds is 1, max is 264

png("Figures/Viab_v2.png")
plot(1:4,c(1,1,1,1),ylim=c(0,1),type="n",axes=F,xlab="Ant state",ylab="",cex.lab=1.4,xlim=c(1,4.25))
points(jitter(rep(1,nrow(crem_subset))),jitter(crem_subset$viab),
       cex=0.5+(crem_subset$TotFlowerbuds_t1/max(viability_data$TotFlowerbuds_t1))*4,
       col=alpha(cremcol,alpha_val),pch=16)
lines(rep(1.25,2),quantile(invlogit(viab_out$beta0[,1]),probs=c(0.025,.975)),
      lwd=3,col=cremcol)
points(1.25,mean(invlogit(viab_out$beta0[,1])),col=cremcol,pch=15,cex=1.5)
points(jitter(rep(2,nrow(liom_subset))),jitter(liom_subset$viab),
       cex=0.5+(liom_subset$TotFlowerbuds_t1/max(viability_data$TotFlowerbuds_t1))*4,
       col=alpha(liomcol,alpha_val),pch=16)
lines(rep(2.25,2),quantile(invlogit(viab_out$beta0[,2]),probs=c(0.025,.975)),
      lwd=3,col=liomcol)
points(2.25,mean(invlogit(viab_out$beta0[,2])),col=liomcol,pch=15,cex=1.5)
points(jitter(rep(3,nrow(other_subset))),jitter(other_subset$viab),
       cex=0.5+(other_subset$TotFlowerbuds_t1/max(viability_data$TotFlowerbuds_t1))*4,
       col=alpha(othercol,alpha_val),pch=16)
lines(rep(3.25,2),quantile(invlogit(viab_out$beta0[,3]),probs=c(0.025,.975)),
      lwd=3,col=othercol)
points(3.25,mean(invlogit(viab_out$beta0[,3])),col=othercol,pch=15,cex=1.5)
points(jitter(rep(4,nrow(vac_subset))),jitter(vac_subset$viab),
       cex=0.5+(vac_subset$TotFlowerbuds_t1/max(viability_data$TotFlowerbuds_t1))*4,
       col=alpha(vaccol,alpha_val),pch=16)
lines(rep(4.25,2),quantile(invlogit(viab_out$beta0[,4]),probs=c(0.025,.975)),
      lwd=3,col=vaccol)
points(4.25,mean(invlogit(viab_out$beta0[,4])),col=vaccol,pch=15,cex=1.5)
axis(1,at=1:4,labels=c(expression(italic("C.opuntiae")),expression(italic("L.apiculatum")),"Other","Vacant"))
mtext("Flowerbud viability", side = 2, line = 1, cex=1.7)
box()
dev.off()

################################################################################
## Seed number visuals
################################################################################
png("Manuscript/Figures/Seeds_Per_Fruit.png")
plot(density(exp(seed.params$beta0[,3])),lwd=3,col=vaccol,
     xlab="Mean seeds per fruit",main=" ",xlim=c(50,250))
lines(density(exp(seed.params$beta0[,2])),lwd=3,col=liomcol)
lines(density(exp(seed.params$beta0[,1])),lwd=3,col=cremcol)
legend("topright",legend = c("Vacant","Crem.","Liom."), fill = c(vaccol, cremcol, liomcol))
dev.off()
################################################################################
## Probability of Reproducing Model Visuals
################################################################################
## Visualize the outputs of the model -- trace plots to check convergence, overlay plots to check the fit
# simulate data based on model fits
y <- as.numeric(stan_data_repro$y_repro)
repro_sim <- matrix(NA, n_draws,stan_data_repro$N)
for(i in 1:n_draws){
  for(n in 1:stan_data_repro$N){
    repro_sim[i,n] <- rbern(n = 1, prob = invlogit(repro.params$beta0[i] + repro.params$beta1[i] * stan_data_repro$vol[n]))
  }
}
# Plot the posterior distributions
png("Manuscript/Figures/repro_post.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay(y, repro_sim)
dev.off()
# Convergence Plots
png(file = "Manuscript/Figures/repro_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_repro, pars=c("beta0","beta1")))
dev.off()


## Format the original data for the figures
# Formulas -- mean, 95% and 5% percentiles
percentiles <- function(percent){
  y_repro <- quantile(repro.params$beta0,percent) + size_dummy * quantile(repro.params$beta1,percent)
  return(y_repro)
}
# Create a subset which includes the necessary data
## Panel Plot showing the probability of reproducing across sizes with the error
png("Manuscript/Figures/repro_panel.png")
plot(x = (size_dummy)  ,y = invlogit(percentiles(.5)), type = "l", col = "chartreuse4",ylim = c(0,1),  lwd = 4,xlab = "",ylab = "")
points(x = stan_data_repro$vol, y =as.numeric(stan_data_repro$y_repro))
lines(x = (size_dummy)  ,y = invlogit(percentiles(.5)), type = "l", col = "chartreuse4", lwd = 4)
lines(x = (size_dummy), y = invlogit(percentiles(.05)), type = "l", col = "darkgrey", lty = 2, lwd = 2)
lines(x = (size_dummy), y = invlogit(percentiles(.95)), type = "l", col = "darkgrey", lty = 2, lwd = 2)
polygon(c((size_dummy),rev((size_dummy))),c(invlogit(percentiles(.95)), rev(invlogit(percentiles(.05)))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
mtext("Log(Volume)",side=1,line=-2,outer=TRUE,cex=1.7)
mtext("Reproduction Rate",side=2,line=-1.5,outer=TRUE,cex=1.7,las=0)
dev.off()



################################################################################
## Seeds Produced Model Visuals
################################################################################
## Visualize the outputs of the model -- trace plots to check convergence, overlay plots to check the fit
# simulate data based on model fits
y <- stan_data_seed$seed
ant = stan_data_seed$ant
seed_sim <- matrix(NA,n_draws,stan_data_seed$N)
for(i in 1:n_draws){
  for(n in 1:stan_data_seed$N){
    seed_sim[i,n] <- rnegbin(n = 1, mu = seed.params$beta0[i,stan_data_seed$ant[n]], theta = seed.params$phi[i])
  }
}
# Overlay Plots
png(file = "Manuscript/Figures/seed_post.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay_grouped(y,seed_sim,group = ant)
dev.off()
# Convergence Plots
png(file = "Manuscript/Figures/seed_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_seed, pars=c("beta0")))
dev.off()


## Format the original data for 
subset_crem <- subset(seed_data, seed_data$ant_state == "Crem")
subset_liom <- subset(seed_data, seed_data$ant_state == "Liom")
subset_vac <- subset(seed_data, seed_data$ant_state == "Vacant")
## Plot the number of seeds produced
crem <- exp(params$seed_beta01)
liom <- exp(params$seed_beta02)
vac <- exp(params$seed_beta03)
png("Manuscript/Figures/num_seeds.png")
boxplot(cbind(crem,liom,vac), col = c(cremcol,liomcol,vaccol), ylab = "", xtext = c("crem","liom","vac"))
mtext("Ant Partner",side=1,line=-2,outer=TRUE,cex=1.7)
mtext("Number of Seeds Per Fruit",side=2,line=-1.5,outer=TRUE,cex=1.7,las=0)
dev.off()
################################################################################
## Fruit Survival Model Visuals
################################################################################
## Visualize the outputs of the model -- trace plots to check convergence, overlay plots to check the fit
# simulate data based on model fits
y <- stan_data_fruit$tot
n_draws = 1000
fruit_sim <- matrix(NA, n_draws,stan_data_fruit$N)
for(i in 1:n_draws){
  for(n in 1:stan_data_fruit$N){
    fruit_sim[i,n] <- rbern(n = 1, prob = invlogit(fruit.params$beta0[i]))
  }
}
# Plot the posterior distributions
png("Manuscript/Figures/fruit_post.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::ppc_dens_overlay(y, fruit_sim)
dev.off()
# Convergence Plots
png(file = "Manuscript/Figures/fruit_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_fruit, pars=c("beta0")))
dev.off()

## Format the original data for figures
# calculate the viability rates of the real data
fruit_data$viab <- fruit_data$Fr.on.plant/(fruit_data$Fr.on.grnd + fruit_data$Fr.on.plant)
## Plot the histograms of the actual data alongside the mean estimates of viability rates by ant state
png("Manuscript/Figures/fruit_hist.png")
hist(fruit_data$viab, xlim = c(0.5,1), prob = TRUE, ylim = c(0,30), col = vaccol, cex.main = 2, xlab = "",ylab = "",main = "")
lines(density(invlogit(fruit.params$beta0)), lwd = 3, col = vaccol) 
mtext("Proportion of Surviving Fruit",side=1,line=-1.5,outer=TRUE,cex=2)
mtext("Density",side=2,line=-2,outer=TRUE,cex=2,las=0)
dev.off()


################################################################################
## Pre-Census Survival of Seedlings Model Visuals
################################################################################
## Visualize the outputs of the model -- trace plots to check convergence, overlay plots to check the fit
# Convergence Plots
png(file = "Manuscript/Figures/seed_surv_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_seed_surv, pars=c("beta0")))
dev.off()


## Formulas
y_surv = pre.seed.params$beta0
y_low_surv = quantile(pre.seed.params$beta0,0.05) 
y_high_surv = quantile(pre.seed.params$beta0,0.95)
## Plot the precensus survival estimated by the model with the precensus survival of real data
png("Manuscript/Figures/seed_surv.png")
plot(density(invlogit(y_surv)), col = "chartreuse4",lwd = 2, xlab = "", ylab = "",main = "")
abline(v = mean(precensus.dat$survive0405), lty = 2)
legend("topright",legend = c("Predicted Pre-census Survival","Real Pre-census Survival"), col = c("chartreuse4","black"), pch = 16)
mtext("Pre-census Survival Probability",side=1,line=-1.5,outer=TRUE,cex=2)
mtext("Density",side=2,line=-2,outer=TRUE,cex=2,las=0)
dev.off()



################################################################################
## Germination Model Visuals
################################################################################
## Visualize the outputs of the model -- trace plots to check convergence, overlay plots to check the fit
# Convergence Plots
png(file = "Manuscript/Figures/germ1_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_germ1, pars=c("beta0")))
dev.off()
png(file = "Manuscript/Figures/germ2_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_germ2, pars=c("beta0")))
dev.off()


## Format the original data
y_germ1 <- stan_data_germ$y_germ[stan_data_germ$year==1]/stan_data_germ$trials[stan_data_germ$year==1]
y_germ2 <- stan_data_germ$y_germ[stan_data_germ$year==2]/stan_data_germ$trials[stan_data_germ$year==2]
germ <- cbind(y_germ1,y_germ2)
colnames(germ) <- c("Year 1","Year 2")
png("Manuscript/Figures/germination.png")
boxplot((germ), col = "chartreuse4", names.arg = c("Yr 1","Yr 2"),
        xlab = "", ylab = "", main = "")
mtext("Year in Seedbank",side=1,line=-1.5,outer=TRUE,cex=2)
mtext("Probability of Germinating",side=2,line=-2,outer=TRUE,cex=2,las=0)
dev.off()


################################################################################
## Recruitment Size Distribution Model Visuals
################################################################################
## Visualize the outputs of the model -- trace plots to check convergence, overlay plots to check the fit
# Convergence Plots
png(file = "Manuscript/Figures/rec_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_rec, pars=c("beta0")))
dev.off()


## Plot the Recruit size distribution
png("Manuscript/Figures/rec_size.png")
boxplot(rec.params$beta0, col = "chartreuse4", ylab = "", main = "", xlab = "")
mtext("Log(Volume)",side=2,line=-2,outer=TRUE,cex=2,las=0)
dev.off()


################################################################################
## Ant Partner Transitions Model Visuals
################################################################################
## Visualize the outputs of the model -- trace plots to check convergence, overlay plots to check the fit
# Convergence Plots
png(file = "Manuscript/Figures/multi_conv.png")
bayesplot::color_scheme_set(scheme = "pink")
bayesplot::mcmc_trace(As.mcmc.list(fit_multi, pars=c("beta")))
dev.off()

## Format the original data 
# prev crem
subset_crem <- filter(cactus_real, cactus_real$ant_t == "crem")
subset_crem$ant_t1_crem_YN <- 0
subset_crem$ant_t1_liom_YN <- 0
subset_crem$ant_t1_other_YN <- 0
subset_crem$ant_t1_vac_YN <- 0
for(i in 1:nrow(subset_crem)){
  if(subset_crem$ant_t1[i] == "crem"){subset_crem$ant_t1_crem_YN[i] = 1}
  if(subset_crem$ant_t1[i] == "liom"){subset_crem$ant_t1_liom_YN[i] = 1}
  if(subset_crem$ant_t1[i] == "other"){subset_crem$ant_t1_other_YN[i] = 1}
  if(subset_crem$ant_t1[i] == "vacant"){subset_crem$ant_t1_vac_YN[i] = 1}
}
# prev liom
subset_liom <- subset(cactus_real, cactus_real$ant_t == "liom")
subset_liom$ant_t1_crem_YN <- 0
subset_liom$ant_t1_liom_YN <- 0
subset_liom$ant_t1_other_YN <- 0
subset_liom$ant_t1_vac_YN <- 0
for(i in 1:nrow(subset_liom)){
  if(subset_liom$ant_t1[i] == "crem"){subset_liom$ant_t1_crem_YN[i] = 1}
  if(subset_liom$ant_t1[i] == "liom"){subset_liom$ant_t1_liom_YN[i] = 1}
  if(subset_liom$ant_t1[i] == "other"){subset_liom$ant_t1_other_YN[i] = 1}
  if(subset_liom$ant_t1[i] == "vacant"){subset_liom$ant_t1_vac_YN[i] = 1}
}
# prev other
subset_other <- subset(cactus_real, cactus_real$ant_t == "other")
subset_other$ant_t1_crem_YN <- 0
subset_other$ant_t1_liom_YN <- 0
subset_other$ant_t1_other_YN <- 0
subset_other$ant_t1_vac_YN <- 0
for(i in 1:nrow(subset_other)){
  if(subset_other$ant_t1[i] == "crem"){subset_other$ant_t1_crem_YN[i] = 1}
  if(subset_other$ant_t1[i] == "liom"){subset_other$ant_t1_liom_YN[i] = 1}
  if(subset_other$ant_t1[i] == "other"){subset_other$ant_t1_other_YN[i] = 1}
  if(subset_other$ant_t1[i] == "vacant"){subset_other$ant_t1_vac_YN[i] = 1}
}
# prev vac
subset_vac <- subset(cactus_real, cactus_real$ant_t == "vacant")
subset_vac$ant_t1_crem_YN <- 0
subset_vac$ant_t1_liom_YN <- 0
subset_vac$ant_t1_other_YN <- 0
subset_vac$ant_t1_vac_YN <- 0
for(i in 1:nrow(subset_vac)){
  if(subset_vac$ant_t1[i] == "crem"){subset_vac$ant_t1_crem_YN[i] = 1}
  if(subset_vac$ant_t1[i] == "liom"){subset_vac$ant_t1_liom_YN[i] = 1}
  if(subset_vac$ant_t1[i] == "other"){subset_vac$ant_t1_other_YN[i] = 1}
  if(subset_vac$ant_t1[i] == "vacant"){subset_vac$ant_t1_vac_YN[i] = 1}
}
# bin the data
multi_plot_crem <- subset_crem %>%
  mutate(size_bin = cut_interval((logsize_t),25)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t),na.rm=T),
            ant_t1_crem = mean(ant_t1_crem_YN,na.rm=T),
            ant_t1_liom = mean(ant_t1_liom_YN, na.rm = T),
            ant_t1_other = mean(ant_t1_other_YN, na.rm = T),
            ant_t1_vac = mean(ant_t1_vac_YN, na.rm = T),
            N = length(logsize_t))
multi_plot_crem$N_mod <- log(multi_plot_crem$N)
multi_plot_liom <- subset_liom %>%
  mutate(size_bin = cut_interval((logsize_t),25)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t),na.rm=T),
            ant_t1_crem = mean(ant_t1_crem_YN,na.rm=T),
            ant_t1_liom = mean(ant_t1_liom_YN, na.rm = T),
            ant_t1_other = mean(ant_t1_other_YN, na.rm = T),
            ant_t1_vac = mean(ant_t1_vac_YN, na.rm = T),
            N = length(logsize_t))
multi_plot_liom$N_mod <- log(multi_plot_liom$N)
multi_plot_other <- subset_other %>%
  mutate(size_bin = cut_interval((logsize_t),25)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t),na.rm=T),
            ant_t1_crem = mean(ant_t1_crem_YN,na.rm=T),
            ant_t1_liom = mean(ant_t1_liom_YN, na.rm = T),
            ant_t1_other = mean(ant_t1_other_YN, na.rm = T),
            ant_t1_vac = mean(ant_t1_vac_YN, na.rm = T),
            N = length(logsize_t))
multi_plot_other$N_mod <- log(multi_plot_other$N)
multi_plot_vac <- subset_vac %>%
  mutate(size_bin = cut_interval((logsize_t),25)) %>%
  group_by(size_bin) %>%
  summarise(mean_size = mean((logsize_t),na.rm=T),
            ant_t1_crem = mean(ant_t1_crem_YN,na.rm=T),
            ant_t1_liom = mean(ant_t1_liom_YN, na.rm = T),
            ant_t1_other = mean(ant_t1_other_YN, na.rm = T),
            ant_t1_vac = mean(ant_t1_vac_YN, na.rm = T),
            N = length(logsize_t))
multi_plot_vac$N_mod <- log(multi_plot_vac$N)
#### ALL ANTS ------------------------------------------------------------------
size_dummy <- seq(5.01, max(cactus_real$logsize_t, na.rm = T), length = 100)
#size_dummy <- seq(min(cactus_real$logsize_t, na.rm = T),5.01, length = 100)

# Previously tended by crem
Denominator_crem <- exp(mean(params$multi_betacc) + size_dummy*mean(params$multi_betac)) + 
  exp(mean(params$multi_betacl) + size_dummy*mean(params$multi_betal)) + 
  exp(mean(params$multi_betaco) + size_dummy*mean(params$multi_betao)) + 
  exp(mean(params$multi_betacv) + size_dummy*mean(params$multi_betav))
pred_crem<-cbind(
  #pr(crem)
  exp(mean(params$multi_betacc) + size_dummy*mean(params$multi_betac))/Denominator_crem,
  #pr(liom)
  exp(mean(params$multi_betacl) + size_dummy*mean(params$multi_betal))/Denominator_crem,
  #pr(other)
  exp(mean(params$multi_betaco) + size_dummy*mean(params$multi_betao))/Denominator_crem,
  #pr(vac)
  exp(mean(params$multi_betacv) + size_dummy*mean(params$multi_betav))/Denominator_crem)
sum(pred_crem[1,])
# Previously tended by Liom
Denominator_liom <- exp(mean(params$multi_betalc) + size_dummy*mean(params$multi_betac)) + 
  exp(mean(params$multi_betall) + size_dummy*mean(params$multi_betal)) + 
  exp(mean(params$multi_betalo) + size_dummy*mean(params$multi_betao)) + 
  exp(mean(params$multi_betalv) + size_dummy*mean(params$multi_betav))
pred_liom<-cbind(
  #pr(crem)
  exp(mean(params$multi_betalc) + size_dummy*mean(params$multi_betac))/Denominator_liom,
  #pr(liom)
  exp(mean(params$multi_betall) + size_dummy*mean(params$multi_betal))/Denominator_liom,
  #pr(other)
  exp(mean(params$multi_betalo) + size_dummy*mean(params$multi_betao))/Denominator_liom,
  #pr(vac)
  exp(mean(params$multi_betalv) + size_dummy*mean(params$multi_betav))/Denominator_liom)
sum(pred_liom[1,])
# Previously tended by other
Denominator_other <- exp(mean(params$multi_betaoc) + size_dummy*mean(params$multi_betac)) + 
  exp(mean(params$multi_betaol) + size_dummy*mean(params$multi_betal)) + 
  exp(mean(params$multi_betaoo) + size_dummy*mean(params$multi_betao)) + 
  exp(mean(params$multi_betaov) + size_dummy*mean(params$multi_betav))
pred_other<-cbind(
  #pr(crem)
  exp(mean(params$multi_betaoc) + size_dummy*mean(params$multi_betac))/Denominator_other,
  #pr(liom)
  exp(mean(params$multi_betaol) + size_dummy*mean(params$multi_betal))/Denominator_other,
  #pr(other)
  exp(mean(params$multi_betaoo) + size_dummy*mean(params$multi_betao))/Denominator_other,
  #pr(vac)
  exp(mean(params$multi_betaov) + size_dummy*mean(params$multi_betav))/Denominator_other)
sum(pred_other[1,])
# Previously tended by vac
Denominator_vac <- exp(mean(params$multi_betavc) + size_dummy*mean(params$multi_betac)) + 
  exp(mean(params$multi_betavl) + size_dummy*mean(params$multi_betal)) + 
  exp(mean(params$multi_betavo) + size_dummy*mean(params$multi_betao)) + 
  exp(mean(params$multi_betavv) + size_dummy*mean(params$multi_betav))
pred_vac<-cbind(
  #pr(crem)
  exp(mean(params$multi_betavc) + size_dummy*mean(params$multi_betac))/Denominator_vac,
  #pr(liom)
  exp(mean(params$multi_betavl) + size_dummy*mean(params$multi_betal))/Denominator_vac,
  #pr(other)
  exp(mean(params$multi_betavo) + size_dummy*mean(params$multi_betao))/Denominator_vac,
  #pr(vac)
  exp(mean(params$multi_betavv) + size_dummy*mean(params$multi_betav))/Denominator_vac)
sum(pred_vac[1,])
## Plot the probabilities of your next ant partner based on previous partner and size -- includes model estimates and real data
png("Manuscript/Figures/multi_ants_size.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4),
              ncol = 2, nrow = 2, byrow = TRUE), heights = c(1.4,1.4), widths = c(3.9,3.9))
# Prev Vac
plot(size_dummy, pred_vac[,4], type = "l", col = vaccol,main = "a)              Prev. Vacant               ", ylim = c(0,1), xlab = "", ylab = "",
     cex.main = 1.5)
lines(size_dummy, pred_vac[,3], col = othercol)
lines(size_dummy, pred_vac[,1], col = cremcol)
lines(size_dummy, pred_vac[,2], col = liomcol)
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_crem,pch=16,cex=multi_plot_vac$N_mod,col= alpha(cremcol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_liom,pch=16,cex=multi_plot_vac$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_other,pch=16,cex=multi_plot_vac$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_vac,pch=16,cex=multi_plot_vac$N_mod,col= alpha(vaccol, 0.4))
# Prev Other
plot(size_dummy, pred_other[,4], type = "l", col = vaccol,main = "b)              Prev. Other                 ", ylim = c(0,1), xlab = "", ylab = "",
     cex.main = 1.5)
lines(size_dummy, pred_other[,3], col = othercol)
lines(size_dummy, pred_other[,1], col = cremcol)
lines(size_dummy, pred_other[,2], col = liomcol)
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_crem,pch=16,cex=multi_plot_other$N_mod,col= alpha(cremcol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_liom,pch=16,cex=multi_plot_other$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_other,pch=16,cex=multi_plot_other$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_vac,pch=16,cex=multi_plot_other$N_mod,col= alpha(vaccol, 0.4))
# Prev Crem
plot(size_dummy, pred_crem[,4], type = "l", col = vaccol,main = "c)              Prev. Crem.                ", ylim = c(0,1), xlab = "", ylab = "",
     cex.main = 1.5)
lines(size_dummy, pred_crem[,3], col = othercol)
lines(size_dummy, pred_crem[,1], col = cremcol)
lines(size_dummy, pred_crem[,2], col = liomcol)
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_crem,pch=16,cex=multi_plot_crem$N_mod,col= alpha(cremcol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_liom,pch=16,cex=multi_plot_crem$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_other,pch=16,cex=multi_plot_crem$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_vac,pch=16,cex=multi_plot_crem$N_mod,col= alpha(vaccol, 0.4))
legend("topright",c("vacant","other","crem.","liom."), fill = c(vaccol,othercol,cremcol,liomcol), cex = 1.5)
# Prev Liom
plot(size_dummy, pred_liom[,4], type = "l", col = vaccol,main = "d)              Prev. Liom.                 ", ylim = c(0,1), xlab = "", ylab = "",
     cex.main = 1.5)
lines(size_dummy, pred_liom[,3], col = othercol)
lines(size_dummy, pred_liom[,1], col = cremcol)
lines(size_dummy, pred_liom[,2], col = liomcol)
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_crem,pch=16,cex=multi_plot_liom$N_mod,col= alpha(cremcol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_liom,pch=16,cex=multi_plot_liom$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_other,pch=16,cex=multi_plot_liom$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_vac,pch=16,cex=multi_plot_liom$N_mod,col= alpha(vaccol, 0.4))
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.5)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.5,las=0)
dev.off()


## Two states 

png("Figures/Comp_Excl_1.png")
# Liom Vac
plot(size_dummy, transition.1.comp(size_dummy,"crem","vacant",params = params_mean,scenario = "cremvac"), 
     col = vaccol, type = "l", lwd = 2, main = "Prev. Crem.", ylab = "Probability of Next Partner", xlab = "Log(Volume)", ylim = c(0,1))
legend("topright",legend = c("Vac.","Crem."), fill = c(vaccol,cremcol))
dev.off()
png("Figures/Comp_Excl_2.png")
# Liom Vac
plot(size_dummy, transition.1.comp(size_dummy,"crem","vacant",params = params_mean,scenario = "cremvac"), 
     col = vaccol, type = "l", lwd = 2, main = "Prev. Crem.", ylab = "Probability of Next Partner", xlab = "Log(Volume)", ylim = c(0,1))
lines(size_dummy,transition.1.comp(size_dummy,"crem","crem",params = params_mean,scenario = "cremvac"), col = cremcol, lwd = 2)
legend("topright",legend = c("Vac.","Crem."), fill = c(vaccol,cremcol))
dev.off()
png("Figures/Two_State_Comp_Excl_Transitions.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2),
              ncol = 2, nrow = 1, byrow = TRUE), heights = c(1.4), widths = c(3.9,3.9))
# Liom Vac
plot(size_dummy, transition.1.comp(size_dummy,"liom","vacant",params = params_mean,scenario = "liomvac"), 
     col = vaccol, type = "l", lwd = 2, main = "Prev. Liom.", ylab = "Probability of Next Partner", xlab = "Log(Volume)", ylim = c(0,1))
lines(size_dummy,transition.1.comp(size_dummy,"liom","liom",params = params_mean,scenario = "liomvac"), col = liomcol, lwd = 2)
legend("topright",legend = c("Vac.","Liom."), fill = c(vaccol,liomcol))
plot(size_dummy, transition.1.comp(size_dummy,"vacant","vacant",params = params_mean,scenario = "liomvac"), col = vaccol, type = "l", lwd = 2, main = "Prev. Vac.", ylab = "Probability of Next Partner", xlab = "Log(Volume)", ylim = c(0,1))
lines(size_dummy,transition.1.comp(size_dummy,"vacant","liom",params = params_mean,scenario = "liomvac"), col = liomcol, lwd = 2)
dev.off()

## Three states 
png("Figures/Three_State_Comp_Excl_Transitions.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3),
              ncol = 3, nrow = 1, byrow = TRUE), heights = c(1.4), widths = c(3.9,3.9,3.9))
# Liom Vac
plot(size_dummy, transition.2.comp(size_dummy,"liom","vacant",params = params_mean,scenario = "liomcremvac"), 
     col = vaccol, type = "l", lwd = 2, main = "Prev. Liom.", ylab = "Probability of Next Partner", xlab = "Log(Volume)", ylim = c(0,1))
lines(size_dummy,transition.2.comp(size_dummy,"liom","liom",params = params_mean,scenario = "liomcremvac"), col = liomcol, lwd = 2)
lines(size_dummy,transition.2.comp(size_dummy,"liom","crem",params = params_mean,scenario = "liomcremvac"), col = cremcol, lwd = 2)
legend("topright",legend = c("Vac.","Liom.","Crem."), fill = c(vaccol,liomcol,cremcol))
plot(size_dummy, transition.2.comp(size_dummy,"vacant","vacant",params = params_mean,scenario = "liomcremvac"), col = vaccol, type = "l", lwd = 2, main = "Prev. Vac.", ylab = "Probability of Next Partner", xlab = "Log(Volume)", ylim = c(0,1))
lines(size_dummy,transition.2.comp(size_dummy,"vacant","liom",params = params_mean,scenario = "liomcremvac"), col = liomcol, lwd = 2)
lines(size_dummy,transition.2.comp(size_dummy,"vacant","crem",params = params_mean,scenario = "liomcremvac"), col = cremcol, lwd = 2)
plot(size_dummy, transition.2.comp(size_dummy,"crem","vacant",params = params_mean,scenario = "liomcremvac"), col = vaccol, type = "l", lwd = 2, main = "Prev. Crem.", ylab = "Probability of Next Partner", xlab = "Log(Volume)", ylim = c(0,1))
lines(size_dummy,transition.2.comp(size_dummy,"crem","liom",params = params_mean,scenario = "liomcremvac"), col = liomcol, lwd = 2)
lines(size_dummy,transition.2.comp(size_dummy,"crem","crem",params = params_mean,scenario = "liomcremvac"), col = cremcol, lwd = 2)
dev.off()


##### Frequency Based
png("Figures/Comp_Excl_1.png")
# Liom Vac
plot(size_dummy, transition.1.comp(size_dummy,"vacant","crem",params = params_mean,scenario = "cremvac"), 
     col = vaccol, type = "l", lwd = 2, main = "Prev. Crem.", ylab = "Probability of Next Partner", xlab = "Log(Volume)", ylim = c(0,1))
legend("topright",legend = c("Vac.","Crem."), fill = c(vaccol,cremcol))
dev.off()
png("Figures/Comp_Excl_2.png")
# Liom Vac
plot(size_dummy, transition.1.comp(size_dummy,"crem","vacant",params = params_mean,scenario = "cremvac"), 
     col = vaccol, type = "l", lwd = 2, main = "Prev. Crem.", ylab = "Probability of Next Partner", xlab = "Log(Volume)", ylim = c(0,1))
lines(size_dummy,transition.1.comp(size_dummy,"crem","crem",params = params_mean,scenario = "cremvac"), col = cremcol, lwd = 2)
legend("topright",legend = c("Vac.","Crem."), fill = c(vaccol,cremcol))
dev.off()
png("Figures/Two_State_Freq_Excl_Transitions.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4,5,6),
              ncol = 2, nrow = 3, byrow = TRUE), heights = c(1.4,1.4,1.4), widths = c(3.9,3.9))
# Liom Vac
plot(size_dummy, transition.1.freq(size_dummy,"liom","vacant",params = params_mean,scenario = "liomvac"), 
     col = vaccol, type = "l", lwd = 2, main = "Prev. Liom.", ylab = "Probability of Next Partner", xlab = "Log(Volume)", ylim = c(0,1))
lines(size_dummy,transition.1.freq(size_dummy,"liom","liom",params = params_mean,scenario = "liomvac"), col = liomcol, lwd = 2)
legend("topright",legend = c("Vac.","Liom."), fill = c(vaccol,liomcol))
plot(size_dummy, transition.1.freq(size_dummy,"vacant","vacant",params = params_mean,scenario = "liomvac"), col = vaccol, type = "l", lwd = 2, main = "Prev. Vac.", ylab = "Probability of Next Partner", xlab = "Log(Volume)", ylim = c(0,1))
lines(size_dummy,transition.1.freq(size_dummy,"vacant","liom",params = params_mean,scenario = "liomvac"), col = liomcol, lwd = 2)

# Crem Vac
plot(size_dummy, transition.1.freq(size_dummy,"crem","vacant",params = params_mean,scenario = "cremvac"), 
     col = vaccol, type = "l", lwd = 2, main = "Prev. Crem.", ylab = "Probability of Next Partner", xlab = "Log(Volume)", ylim = c(0,1))
lines(size_dummy,transition.1.freq(size_dummy,"crem","crem",params = params_mean,scenario = "cremvac"), col = cremcol, lwd = 2)
legend("topright",legend = c("Vac.","Crem."), fill = c(vaccol,cremcol))
plot(size_dummy, transition.1.freq(size_dummy,"vacant","vacant",params = params_mean,scenario = "cremvac"), col = vaccol, type = "l", lwd = 2, main = "Prev. Vac.", ylab = "Probability of Next Partner", xlab = "Log(Volume)", ylim = c(0,1))
lines(size_dummy,transition.1.freq(size_dummy,"vacant","crem",params = params_mean,scenario = "cremvac"), col = cremcol, lwd = 2)

# Other Vac
plot(size_dummy, transition.1.freq(size_dummy,"other","vacant",params = params_mean,scenario = "othervac"), 
     col = vaccol, type = "l", lwd = 2, main = "Prev. Other", ylab = "Probability of Next Partner", xlab = "Log(Volume)", ylim = c(0,1))
lines(size_dummy,transition.1.freq(size_dummy,"other","other",params = params_mean,scenario = "othervac"), col = othercol, lwd = 2)
legend("topright",legend = c("Vac.","Other"), fill = c(vaccol,othercol))
plot(size_dummy, transition.1.freq(size_dummy,"vacant","vacant",params = params_mean,scenario = "othervac"), col = vaccol, type = "l", lwd = 2, main = "Prev. Vac.", ylab = "Probability of Next Partner", xlab = "Log(Volume)", ylim = c(0,1))
lines(size_dummy,transition.1.freq(size_dummy,"vacant","other",params = params_mean,scenario = "othervac"), col = othercol, lwd = 2)
dev.off()

## Three states 
png("Figures/Three_State_Freq_Excl_Transitions.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3),
              ncol = 3, nrow = 1, byrow = TRUE), heights = c(1.4), widths = c(3.9,3.9,3.9))
# Liom Vac
plot(size_dummy, transition.2.freq(size_dummy,"liom","vacant",params = params_mean,scenario = "liomcremvac"), 
     col = vaccol, type = "l", lwd = 2, main = "Prev. Liom.", ylab = "Probability of Next Partner", xlab = "Log(Volume)", ylim = c(0,1))
lines(size_dummy,transition.2.freq(size_dummy,"liom","liom",params = params_mean,scenario = "liomcremvac"), col = liomcol, lwd = 2)
lines(size_dummy,transition.2.freq(size_dummy,"liom","crem",params = params_mean,scenario = "liomcremvac"), col = cremcol, lwd = 2)
legend("topright",legend = c("Vac.","Liom.","Crem."), fill = c(vaccol,liomcol,cremcol))
plot(size_dummy, transition.2.freq(size_dummy,"vacant","vacant",params = params_mean,scenario = "liomcremvac"), col = vaccol, type = "l", lwd = 2, main = "Prev. Vac.", ylab = "Probability of Next Partner", xlab = "Log(Volume)", ylim = c(0,1))
lines(size_dummy,transition.2.freq(size_dummy,"vacant","liom",params = params_mean,scenario = "liomcremvac"), col = liomcol, lwd = 2)
lines(size_dummy,transition.2.freq(size_dummy,"vacant","crem",params = params_mean,scenario = "liomcremvac"), col = cremcol, lwd = 2)
plot(size_dummy, transition.2.freq(size_dummy,"crem","vacant",params = params_mean,scenario = "liomcremvac"), col = vaccol, type = "l", lwd = 2, main = "Prev. Crem.", ylab = "Probability of Next Partner", xlab = "Log(Volume)", ylim = c(0,1))
lines(size_dummy,transition.2.freq(size_dummy,"crem","liom",params = params_mean,scenario = "liomcremvac"), col = liomcol, lwd = 2)
lines(size_dummy,transition.2.freq(size_dummy,"crem","crem",params = params_mean,scenario = "liomcremvac"), col = cremcol, lwd = 2)
dev.off()



################################################################################
## Herbivory Visuals
################################################################################
## Set the damage = 1 if there is evidence of herbivory
# create a herbivory dataset
herbivory <- cactus[,c("ant_t1","NP_adult","NP_juv","CV","WVL","Damage","MA")]
# make sure all the herbivores are read in as numbers
summary(herbivory$ant_t1)
summary(herbivory$NP_adult)
summary(herbivory$NP_juv)
summary(herbivory$CV)
summary(herbivory$WVL)
summary(herbivory$Damage)
summary(herbivory$MA)
# pull out rows which have data
for(i in 1:nrow(herbivory)){
  if(is.na(herbivory$NP_adult[i]) == F | is.na(herbivory$NP_juv[i]) == F | is.na(herbivory$CV[i]) == F | is.na(herbivory$WVL[i]) ==F | is.na(herbivory$Damage[i]) ==F | is.na(herbivory$MA[i]) == F){herbivory$herb_data_all[i] <- 1}
  if(is.na(herbivory$NP_adult[i]) == F | is.na(herbivory$NP_juv[i]) == F | is.na(herbivory$CV[i]) == F | is.na(herbivory$WVL[i]) ==F |  is.na(herbivory$MA[i]) == F){herbivory$herb_data[i] <- 1}
}
herb_all <- subset(herbivory, herbivory$herb_data_all == 1)
herb_some <- subset(herbivory, herbivory$herb_data == 1)
# replace NA with 0
herb_all[is.na(herb_all)] <- 0
herb_some[is.na(herb_some)] <- 0
# Make a variable where all signs of damage are considered and one where only spotter herbivores are considered
herb_all$herb <- 0
for(i in 1:nrow(herb_all)){
  if(herb_all$NP_adult[i] > 0 | herb_all$NP_juv[i] > 0 | herb_all$CV[i] > 0 | herb_all$WVL[i] > 0 | herb_all$Damage[i] > 0 | herb_all$MA[i] > 0){herb_all$herb[i] <- 1}
}
herb_some$herb <- 0
for(i in 1:nrow(herb_some)){
  if(herb_some$NP_adult[i] > 0 | herb_some$NP_juv[i] > 0 | herb_some$CV[i] > 0 | herb_some$WVL[i] > 0 |  herb_some$MA[i] > 0){herb_some$herb[i] <- 1}
}

summary(herb_all$herb)
summary(herb_some$herb)
## Subset by ant
crem_herb_all <- subset(herb_all, herb_all$ant_t1 == "crem")
liom_herb_all <- subset(herb_all, herb_all$ant_t1 == "liom")
other_herb_all <- subset(herb_all, herb_all$ant_t1 == "other")
vac_herb_all <- subset(herb_all, herb_all$ant_t1 == "vacant")
## Calculate the proportion of plants in each subset which 
crem_damage <- sum(crem_herb_all$herb, na.rm = T)/nrow(crem_herb_all)
liom_damage <- sum(liom_herb_all$herb, na.rm = T)/nrow(liom_herb_all)
other_damage <- sum(other_herb_all$herb, na.rm = T)/nrow(other_herb_all)
vac_damage <- sum(vac_herb_all$herb, na.rm = T)/nrow(vac_herb_all)
## Visualize the proportion of plants which have herbivory damage
png("Figures/herb_all.png")
barplot(height = c(crem_damage,liom_damage,other_damage,vac_damage), names.arg = c("Crem.","Liom.","Other","Vac."), col = c(cremcol,liomcol,othercol,vaccol))
dev.off()

## Subset by ant
crem_herb_some <- subset(herb_some, herb_some$ant_t1 == "crem")
liom_herb_some <- subset(herb_some, herb_some$ant_t1 == "liom")
other_herb_some <- subset(herb_some, herb_some$ant_t1 == "other")
vac_herb_some <- subset(herb_some, herb_some$ant_t1 == "vacant")
## Calculate the proportion of plants in each subset which 
crem_damage <- sum(crem_herb_some$herb, na.rm = T)/nrow(crem_herb_some)
liom_damage <- sum(liom_herb_some$herb, na.rm = T)/nrow(liom_herb_some)
other_damage <- sum(other_herb_some$herb, na.rm = T)/nrow(other_herb_some)
vac_damage <- sum(vac_herb_some$herb, na.rm = T)/nrow(vac_herb_some)
## Visualize the proportion of plants which have herbivory damage
png("Figures/herb_some.png")
barplot(height = c(crem_damage,liom_damage,other_damage,vac_damage), names.arg = c("Crem.","Liom.","Other","Vac."), col = c(cremcol,liomcol,othercol,vaccol))
dev.off()





#######################################################################################################
#### Hypotheses #######################################################################################
#######################################################################################################
barplot(c(0.1014,0.06296,0.1265,0.09043), col = c(othercol, cremcol, liomcol, vaccol), names.arg = c("Other","Crem.","Liom.","Vacant"),
        ylab = "Herbivory Prob.", main = "Proportion of Plants with Evidence of Herbivory")
#### Sampling Effect
heights <- c(0.9,1.2,1.2)
png("Sampling_Effect.png")
barplot(heights, col = c("Chartreuse4","Pink", "Purple"),names.arg = c("A","B","A & B"), ylab = "Lambda (Fitness)", main = "Sampling Effect")
dev.off()

#### Complementarity
heights <- c(0.9,1.2,1.6)
png("Complementarity.png")
barplot(heights, col = c("Chartreuse4","Pink", "Purple"),names.arg = c("A","B","A & B"), ylab = "Lambda (Fitness)", main = "Complementarity")
dev.off()

#### Portfolio Effect
yr <- seq(2000,2010,by = 1)
yr1 <- c(1,1.5,.2,0,.5,.7,.6,1.2,1.3,1.8,1)
yr2 <- c(1,.2,1.2,1.7,1.5,1.1,.9,.5,.5,.6,0.8)
png("Portfolio_Effect.png")
plot(x = yr, y = yr1, type = "l", col = "Chartreuse4", xlab = "Year", ylab = "Lambda (Fitness)", main = "Portfolio Effect", lwd = 2)
lines(x = yr, y = yr2, col = "Pink", lwd = 2)
legend("bottomright", legend = c("A","B"), fill = c("Chartreuse4","Pink"))
dev.off()

########################################################################################################
#### Timeseries Visuals ################################################################################
########################################################################################################
##Number of Cacti by ant partners
ant_freq_ts <- cactus %>% 
  select(Year_t,ant_t) %>% 
  drop_na() %>% 
  group_by(Year_t,ant_t) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n))
png("Manuscript/Figures/Timeseries.png")
plot(ant_freq_ts$Year_t[ant_freq_ts$ant_t == "crem"], ant_freq_ts$freq[ant_freq_ts$ant_t == "crem"], 
     type = "b",pch = 20, cex = 3,col = cremcol, ylim =c(0,1),
     xlab = "", ylab = "")
lines(ant_freq_ts$Year_t[ant_freq_ts$ant_t == "liom"], ant_freq_ts$freq[ant_freq_ts$ant_t == "liom"], 
      type = "b",pch = 20, cex = 3,col = liomcol)
lines(ant_freq_ts$Year_t[ant_freq_ts$ant_t == "vacant"], ant_freq_ts$freq[ant_freq_ts$ant_t == "vacant"], 
      type = "b",pch = 20, cex = 3,col = vaccol)
lines(ant_freq_ts$Year_t[ant_freq_ts$ant_t == "other"], ant_freq_ts$freq[ant_freq_ts$ant_t == "other"], 
      type = "b",pch = 20, cex = 3,col = othercol)
legend("topright", legend = c("Liom.","Crem.","Other","Vacant"), fill = c(liomcol,cremcol,othercol,vaccol),
       cex = 1.5)
mtext("Year",side=1,line=-1.5,outer=TRUE,cex=1.5)
mtext("Frequency of Ant Species",side=2,line=-1.5,outer=TRUE,cex=1.5,las=0)
dev.off()



######################################################################################################
######################################################################################################
####                PULL IN THE DETERMINISTIC AND STOCHASTIC DISTRIBUTIONS                        ####
######################################################################################################
######################################################################################################
## Read in lambda estimates
## Competitive Exclusion
lams_comp_stoch <- read.csv("lambda_comp.csv")
lams_comp_stoch <- lams_comp_stoch[,-c(1)]
## Competitive Exclusion Null Stochastic
lams_comp_stoch_null <- read.csv("lambda_sync_comp.csv")
lams_comp_stoch_null <- lams_comp_stoch_null[,-c(1)]
## Frequency Based
lams_freq_stoch <- read.csv("lambda_freq.csv")
lams_freq_stoch <- lams_freq_stoch[,-c(1)]
## Frequency Based Null
lams_freq_stoch_null <- read.csv("lambda_sync_freq.csv")
lams_freq_stoch_null <- lams_freq_stoch_null[,-c(1)]
## Equal Probability
lams_equal_stoch <- read.csv("lambda_equal.csv")
lams_equal_stoch <- lams_equal_stoch[,-c(1)]
## Equal Probability Null Stochastic
lams_equal_stoch_null <- read.csv("lambda_sync_equal.csv")
lams_equal_stoch_null <- lams_equal_stoch_null[,-c(1)]
scenario_abv <- c("V","C","L","O","LC","LO","OC","LOC")

######################################################################################################
######################################################################################################
####                     VISUALIZE EACH OF THE POSTERIOR DISTRIBUTIONS                            ####
######################################################################################################
######################################################################################################
# Set the working directory to the figures folder
scenario_abv <- c("V","CV","LV","OV","LCV","LOV","OCV","LOCV")
## Make plot for poster & other things -- research statement
# png("Manuscript/Figures/Lambdas_C_Simple.png")
# plot(x  = c(1,3,3,3,5,5,5,7), y = colMeans(lams_comp_stoch), pch = 19, cex = 3,col = cols,
#      xlim = c(0,8), ylim = c(0.962,0.995),xaxt = "n",cex.lab = 2,
#      xlab = "Number of Ant Partners", ylab = "Mean Fitness Estimate",
#      main = "")
# axis(side=1,at=c(1,3,5,7),labels=c("0","1","2","3"))
# dev.off()

###### Manuscript Visual

## Plot the means for the competitive exclusion model
png("Manuscript/Figures/Lambdas_Comp_Full.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2,3),
              ncol = 3, nrow = 1), heights = c(1,1,1))
plot(x  = c(1,10,11,9,20,21,19,30), y = colMeans(lams_comp_stoch), pch = 19, cex = 3,col = cols,
     xlim = c(0,31), ylim = c(0.962,0.995),xaxt = "n",cex.lab = 2,
     xlab = " ", ylab = " ",
     main = "Non-Synchronous")
axis(side=1,at=c(1,10,20,31),labels=c("0","1","2","3"))
mtext("Mean Lambda Value", side = 2, cex = 2.2, line = 2)

plot(x  = c(1,10,11,9,20,21,19,30), y = colMeans(lams_comp_stoch_null), pch = 19, cex = 3,col = cols,
     xlim = c(0,31), ylim = c(0.962,0.995),xaxt = "n",cex.lab = 2,
     xlab = " ", ylab = " ",
     main = "Synchronous")
axis(side=1,at=c(1,10,20,31),labels=c("0","1","2","3"))
legend("bottomright", legend = scenario_abv, fill = cols)
mtext("Number of Ant Partners", side = 1, cex = 2.2, line = 3, adj = 1.25)

no_part <- colMeans(lams_comp_stoch)[1]
one_part <- mean(colMeans(lams_comp_stoch)[2:4])
two_part <- mean(colMeans(lams_comp_stoch)[5:7])
three_part <- colMeans(lams_comp_stoch)[8]
no_part_sync <- colMeans(lams_comp_stoch_null)[1]
one_part_sync <- mean(colMeans(lams_comp_stoch_null)[2:4])
two_part_sync <- mean(colMeans(lams_comp_stoch_null)[5:7])
three_part_sync <- colMeans(lams_comp_stoch_null)[8]
plot(c(1,2,3,4),c(no_part, one_part, two_part, three_part), pch = 16, cex = 3, 
     xlim = c(0,5), ylim = c(0.94, 1), xaxt = "n", cex.lab = 2, 
     xlab = "Number of Partners", ylab = "Mean Lambda Value", main = "")
axis(1, at = c(1,2,3,4), labels = c("0","1","2","3"))
points(c(1,2,3,4),c(no_part_sync,one_part_sync,two_part_sync,three_part_sync),pch = 18, cex = 3, col = "red")
legend("bottomright",legend = c("synchronous","non-synchronous"), fill = c("black","red"))
dev.off()

png("Manuscript/Figures/Lambdas_Comp_lines.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2),
              ncol = 2, nrow = 1), heights = c(1,1,1,1))
plot(x  = c(1,10,11,9,20,21,19,30), y = colMeans(lams_comp_stoch), pch = 19, cex = 3,col = cols,
     xlim = c(0,31), ylim = c(0.9005,1.025),xaxt = "n",cex.lab = 2,
     xlab = " ", ylab = " ",
     main = "Non-Synchronous")
arrows(x0 = c(1,10,11,9,20,21,19,30), 
       y0 = c(min(lams_comp_stoch[,1]),min(lams_comp_stoch[,2]),min(lams_comp_stoch[,3]),min(lams_comp_stoch[,4]),min(lams_comp_stoch[,5]),min(lams_comp_stoch[,6]),min(lams_comp_stoch[,7]),min(lams_comp_stoch[,8])),
       x1 = c(1,10,11,9,20,21,19,30), 
       y1 = c(max(lams_comp_stoch[,1]),max(lams_comp_stoch[,2]),max(lams_comp_stoch[,3]),max(lams_comp_stoch[,4]),max(lams_comp_stoch[,5]),max(lams_comp_stoch[,6]),max(lams_comp_stoch[,7]),max(lams_comp_stoch[,8])),
       angle = 90, code = 3, length = 0.05, col = cols, lwd = 5)
points(x  = c(1,10,11,9,20,21,19,30), y = colMeans(lams_comp_stoch), pch = 19, cex = 3,col = cols)
axis(side=1,at=c(1,10,20,31),labels=c("0","1","2","3"))
mtext("Mean Lambda Value", side = 2, cex = 2.2, line = 2)


## Plot the means for the competitive exclusion model
png("Manuscript/Figures/Lambdas_Comp.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2),
              ncol = 2, nrow = 1), heights = c(1,1,1,1))
plot(x  = c(1,10,11,9,20,21,19,30), y = colMeans(lams_comp_stoch), pch = 19, cex = 3,col = cols,
     xlim = c(0,31), ylim = c(0.962,0.995),xaxt = "n",cex.lab = 2,
     xlab = " ", ylab = " ",
     main = "Non-Synchronous")
axis(side=1,at=c(1,10,20,31),labels=c("0","1","2","3"))
mtext("Mean Lambda Value", side = 2, cex = 2.2, line = 2)

plot(x  = c(1,10,11,9,20,21,19,30), y = colMeans(lams_comp_stoch_null), pch = 19, cex = 3,col = cols,
     xlim = c(0,31), ylim = c(0.962,0.995),xaxt = "n",cex.lab = 2,
     xlab = " ", ylab = " ",
     main = "Synchronous")
axis(side=1,at=c(1,10,20,31),labels=c("0","1","2","3"))
legend("bottomright", legend = scenario_abv, fill = cols)
mtext("Number of Ant Partners", side = 1, cex = 2.2, line = 3, adj = 1.25)
dev.off()

png("Manuscript/Figures/Lambdas_Comp_lines.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2),
              ncol = 2, nrow = 1), heights = c(1,1,1,1))
plot(x  = c(1,10,11,9,20,21,19,30), y = colMeans(lams_comp_stoch), pch = 19, cex = 3,col = cols,
     xlim = c(0,31), ylim = c(0.9005,1.025),xaxt = "n",cex.lab = 2,
     xlab = " ", ylab = " ",
     main = "Non-Synchronous")
arrows(x0 = c(1,10,11,9,20,21,19,30), 
       y0 = c(min(lams_comp_stoch[,1]),min(lams_comp_stoch[,2]),min(lams_comp_stoch[,3]),min(lams_comp_stoch[,4]),min(lams_comp_stoch[,5]),min(lams_comp_stoch[,6]),min(lams_comp_stoch[,7]),min(lams_comp_stoch[,8])),
       x1 = c(1,10,11,9,20,21,19,30), 
       y1 = c(max(lams_comp_stoch[,1]),max(lams_comp_stoch[,2]),max(lams_comp_stoch[,3]),max(lams_comp_stoch[,4]),max(lams_comp_stoch[,5]),max(lams_comp_stoch[,6]),max(lams_comp_stoch[,7]),max(lams_comp_stoch[,8])),
       angle = 90, code = 3, length = 0.05, col = cols, lwd = 5)
points(x  = c(1,10,11,9,20,21,19,30), y = colMeans(lams_comp_stoch), pch = 19, cex = 3,col = cols)
axis(side=1,at=c(1,10,20,31),labels=c("0","1","2","3"))
mtext("Mean Lambda Value", side = 2, cex = 2.2, line = 2)

plot(x  = c(1,10,11,9,20,21,19,30), y = colMeans(lams_comp_stoch_null), pch = 19, cex = 3,col = cols,
     xlim = c(0,31), ylim = c(0.9005,1.025),xaxt = "n",cex.lab = 2,
     xlab = " ", ylab = " ",
     main = "Synchronous")
arrows(x0 = c(1,10,11,9,20,21,19,30), 
       y0 = c(min(lams_comp_stoch_null[,1]),min(lams_comp_stoch_null[,2]),min(lams_comp_stoch_null[,3]),min(lams_comp_stoch_null[,4]),min(lams_comp_stoch_null[,5]),min(lams_comp_stoch_null[,6]),min(lams_comp_stoch_null[,7]),min(lams_comp_stoch_null[,8])),
       x1 = c(1,10,11,9,20,21,19,30), 
       y1 = c(max(lams_comp_stoch_null[,1]),max(lams_comp_stoch_null[,2]),max(lams_comp_stoch_null[,3]),max(lams_comp_stoch_null[,4]),max(lams_comp_stoch_null[,5]),max(lams_comp_stoch_null[,6]),max(lams_comp_stoch_null[,7]),max(lams_comp_stoch_null[,8])),
       angle = 90, code = 3, length = 0.05, col = cols, lwd = 5)
points(x  = c(1,10,11,9,20,21,19,30), y = colMeans(lams_comp_stoch), pch = 19, cex = 3,col = cols)
axis(side=1,at=c(1,10,20,31),labels=c("0","1","2","3"))
legend("bottomright", legend = scenario_abv, fill = cols)
mtext("Number of Ant Partners", side = 1, cex = 2.2, line = 3, adj = 1.25)
dev.off()


## Plot the means for the equal opportunity model
png("Manuscript/Figures/Lambdas_Equal.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2),
              ncol = 2, nrow = 1), heights = c(1,1,1,1))
plot(x  = c(1,10,11,9,20,21,19,30), y = colMeans(lams_equal_stoch), pch = 19, cex = 3,col = cols,
     xlim = c(0,31), ylim = c(0.962,0.995),xaxt = "n",cex.lab = 2,
     xlab = " ", ylab = " ",
     main = "Non-Synchronous")
axis(side=1,at=c(1,10,20,30),labels=c("0","1","2","3"))
mtext("Mean Lambda Value", side = 2, cex = 2.2, line = 2)
plot(x  = c(1,10,11,9,20,21,19,30), y = colMeans(lams_equal_stoch_null), pch = 19, cex = 3,col = cols,
     xlim = c(0,31), ylim = c(0.962,0.995),xaxt = "n",cex.lab = 2,
     xlab = " ", ylab = " ",
     main = "Synchronous")
axis(side=1,at=c(1,10,20,30),labels=c("0","1","2","3"))
legend("bottomright", legend = scenario_abv, fill = cols)
mtext("Number of Ant Partners", side = 1, cex = 2.2, line = 3, adj = 1.25)
dev.off()

png("Manuscript/Figures/Lambdas_Equal_lines.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2),
              ncol = 2, nrow = 1), heights = c(1,1,1,1))
plot(x  = c(1,10,11,9,20,21,19,30), y = colMeans(lams_equal_stoch), pch = 19, cex = 3,col = cols,
     xlim = c(0,31), ylim = c(0.9005,1.025),xaxt = "n",cex.lab = 2,
     xlab = " ", ylab = " ",
     main = "Non-Synchronous")
arrows(x0 = c(1,10,11,9,20,21,19,30), 
       y0 = c(min(lams_equal_stoch[,1]),min(lams_equal_stoch[,2]),min(lams_equal_stoch[,3]),min(lams_equal_stoch[,4]),min(lams_equal_stoch[,5]),min(lams_equal_stoch[,6]),min(lams_equal_stoch[,7]),min(lams_equal_stoch[,8])),
       x1 = c(1,10,11,9,20,21,19,30), 
       y1 = c(max(lams_equal_stoch[,1]),max(lams_equal_stoch[,2]),max(lams_equal_stoch[,3]),max(lams_equal_stoch[,4]),max(lams_equal_stoch[,5]),max(lams_equal_stoch[,6]),max(lams_equal_stoch[,7]),max(lams_equal_stoch[,8])),
       angle = 90, code = 3, length = 0.05, col = cols, lwd = 5)
axis(side=1,at=c(1,10,20,30),labels=c("0","1","2","3"))
mtext("Mean Lambda Value", side = 2, cex = 2.2, line = 2)

plot(x  = c(1,10,11,9,20,21,19,30), y = colMeans(lams_equal_stoch_null), pch = 19, cex = 3,col = cols,
     xlim = c(0,31), ylim = c(0.9005,1.025),xaxt = "n",cex.lab = 2,
     xlab = " ", ylab = " ",
     main = "Synchronous")
arrows(x0 = c(1,10,11,9,20,21,19,30), 
       y0 = c(min(lams_equal_stoch_null[,1]),min(lams_equal_stoch_null[,2]),min(lams_equal_stoch_null[,3]),min(lams_equal_stoch_null[,4]),min(lams_equal_stoch_null[,5]),min(lams_equal_stoch_null[,6]),min(lams_equal_stoch_null[,7]),min(lams_equal_stoch_null[,8])),
       x1 = c(1,10,11,9,20,21,19,30), 
       y1 = c(max(lams_equal_stoch_null[,1]),max(lams_equal_stoch_null[,2]),max(lams_equal_stoch_null[,3]),max(lams_equal_stoch_null[,4]),max(lams_equal_stoch_null[,5]),max(lams_equal_stoch_null[,6]),max(lams_equal_stoch_null[,7]),max(lams_equal_stoch_null[,8])),
       angle = 90, code = 3, length = 0.05, col = cols, lwd = 5)
axis(side=1,at=c(1,10,20,30),labels=c("0","1","2","3"))
legend("bottomright", legend = scenario_abv, fill = cols)
mtext("Number of Ant Partners", side = 1, cex = 2.2, line = 3, adj = 1.25)
dev.off()

## Plot the means for the Frequency based model
png("Manuscript/Figures/Lambdas_Freq.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2),
              ncol = 2, nrow = 1), heights = c(1,1,1,1))
plot(x  = c(1,10,11,9,20,21,19,30), y = colMeans(lams_freq_stoch), pch = 19, cex = 3,col = cols,
     xlim = c(0,31), ylim = c(0.942,0.992),xaxt = "n",cex.lab = 2,
     xlab = " ", ylab = " ",
     main = "Non-Synchronous")
axis(side=1,at=c(1,10,20,30),labels=c("0","1","2","3"))
legend("bottomright", legend = scenario_abv, fill = cols)
mtext("Mean Lambda Value", side = 2, cex = 2.2, line = 2)
plot(x  = c(1,10,11,9,20,21,19,30), y = colMeans(lams_freq_stoch_null), pch = 19, cex = 3,col = cols,
     xlim = c(0,31), ylim = c(0.942,0.992),xaxt = "n",cex.lab = 2,
     xlab = " ", ylab = " ",
     main = "Synchronous")
axis(side=1,at=c(1,10,20,30),labels=c("0","1","2","3"))
mtext("Number of Ant Partners", side = 1, cex = 2.2, line = 3, adj = 1.25)
dev.off()

png("Manuscript/Figures/Lambdas_Freq_lines.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2),
              ncol = 2, nrow = 1), heights = c(1,1,1,1))
plot(x  = c(1,10,11,9,20,21,19,30), y = colMeans(lams_freq_stoch), pch = 19, cex = 3,col = cols,
     xlim = c(0,31), ylim = c(0.9005,1.025),xaxt = "n",cex.lab = 2,
     xlab = " ", ylab = " ",
     main = "Non-Synchronous")
arrows(x0 = c(1,10,11,9,20,21,19,30), 
       y0 = c(min(lams_freq_stoch[,1]),min(lams_freq_stoch[,2]),min(lams_freq_stoch[,3]),min(lams_freq_stoch[,4]),min(lams_freq_stoch[,5]),min(lams_freq_stoch[,6]),min(lams_freq_stoch[,7]),min(lams_freq_stoch[,8])),
       x1 = c(1,10,11,9,20,21,19,30), 
       y1 = c(max(lams_freq_stoch[,1]),max(lams_freq_stoch[,2]),max(lams_freq_stoch[,3]),max(lams_freq_stoch[,4]),max(lams_freq_stoch[,5]),max(lams_freq_stoch[,6]),max(lams_freq_stoch[,7]),max(lams_freq_stoch[,8])),
       angle = 90, code = 3, length = 0.05, col = cols, lwd = 5)
axis(side=1,at=c(1,10,20,30),labels=c("0","1","2","3"))
mtext("Mean Lambda Value", side = 2, cex = 2.2, line = 2)

plot(x  = c(1,10,11,9,20,21,19,30), y = colMeans(lams_freq_stoch_null), pch = 19, cex = 3,col = cols,
     xlim = c(0,31), ylim = c(0.9005,1.025),xaxt = "n",cex.lab = 2,
     xlab = " ", ylab = " ",
     main = "Synchronous")
arrows(x0 = c(1,10,11,9,20,21,19,30), 
       y0 = c(min(lams_freq_stoch_null[,1]),min(lams_freq_stoch_null[,2]),min(lams_freq_stoch_null[,3]),min(lams_freq_stoch_null[,4]),min(lams_freq_stoch_null[,5]),min(lams_freq_stoch_null[,6]),min(lams_freq_stoch_null[,7]),min(lams_freq_stoch_null[,8])),
       x1 = c(1,10,11,9,20,21,19,30), 
       y1 = c(max(lams_freq_stoch_null[,1]),max(lams_freq_stoch_null[,2]),max(lams_freq_stoch_null[,3]),max(lams_freq_stoch_null[,4]),max(lams_freq_stoch_null[,5]),max(lams_freq_stoch_null[,6]),max(lams_freq_stoch_null[,7]),max(lams_freq_stoch_null[,8])),
       angle = 90, code = 3, length = 0.05, col = cols, lwd = 5)
axis(side=1,at=c(1,10,20,30),labels=c("0","1","2","3"))
legend("bottomright", legend = scenario_abv, fill = cols)
mtext("Number of Ant Partners", side = 1, cex = 2.2, line = 3, adj = 1.25)
dev.off()

# plot the lambda values against the number of partners
no_part <- colMeans(lams_comp_stoch)[1]
one_part <- mean(colMeans(lams_comp_stoch)[2:4])
two_part <- mean(colMeans(lams_comp_stoch)[5:7])
three_part <- colMeans(lams_comp_stoch)[8]
no_part_sync <- colMeans(lams_comp_stoch_null)[1]
one_part_sync <- mean(colMeans(lams_comp_stoch_null)[2:4])
two_part_sync <- mean(colMeans(lams_comp_stoch_null)[5:7])
three_part_sync <- colMeans(lams_comp_stoch_null)[8]
png("Manuscript/Figures/comp_num_partners.png")
plot(c(1,2,3,4),c(no_part, one_part, two_part, three_part), pch = 16, cex = 3, 
     xlim = c(0,5), ylim = c(0.94, 1), xaxt = "n", cex.lab = 2, 
     xlab = "Number of Partners", ylab = "Mean Lambda Value", main = "")
axis(1, at = c(1,2,3,4), labels = c("0","1","2","3"))
points(c(1,2,3,4),c(no_part_sync,one_part_sync,two_part_sync,three_part_sync),pch = 18, cex = 3, col = "red")
legend("bottomright",legend = c("synchronous","non-synchronous"), fill = c("black","red"))
dev.off()


# plot the lambda values against the number of partners
no_part <- colMeans(lams_freq_stoch)[1]
one_part <- mean(colMeans(lams_freq_stoch)[2:4])
two_part <- mean(colMeans(lams_freq_stoch)[5:7])
three_part <- colMeans(lams_freq_stoch)[8]
no_part_sync <- colMeans(lams_freq_stoch_null)[1]
one_part_sync <- mean(colMeans(lams_freq_stoch_null)[2:4])
two_part_sync <- mean(colMeans(lams_freq_stoch_null)[5:7])
three_part_sync <- colMeans(lams_freq_stoch_null)[8]
png("Manuscript/Figures/freq_num_partners.png")
plot(c(1,2,3,4),c(no_part, one_part, two_part, three_part), pch = 16, cex = 3, 
     xlim = c(0,5), ylim = c(0.95,1.01), xaxt = "n", cex.lab = 2, 
     xlab = "Number of Partners", ylab = "Mean Lambda Value", main = "")
axis(1, at = c(1,2,3,4), labels = c("0","1","2","3"))
points(c(1,2,3,4),c(no_part_sync,one_part_sync,two_part_sync,three_part_sync),pch = 18, cex = 3, col = "red")
legend("bottomright",legend = c("synchronous","non-synchronous"), fill = c("black","red"))
dev.off()

no_part <- colMeans(lams_equal_stoch)[1]
one_part <- mean(colMeans(lams_equal_stoch)[2:4])
two_part <- mean(colMeans(lams_equal_stoch)[5:7])
three_part <- colMeans(lams_equal_stoch)[8]
no_part_sync <- colMeans(lams_equal_stoch_null)[1]
one_part_sync <- mean(colMeans(lams_equal_stoch_null)[2:4])
two_part_sync <- mean(colMeans(lams_equal_stoch_null)[5:7])
three_part_sync <- colMeans(lams_equal_stoch_null)[8]
png("Manuscript/Figures/equal_num_partners.png")
plot(c(1,2,3,4),c(no_part, one_part, two_part, three_part), pch = 16, cex = 3, 
     xlim = c(0,5), ylim = c(0.94, 1), xaxt = "n", cex.lab = 2, 
     xlab = "Number of Partners", ylab = "Mean Lambda Value", main = "")
axis(1, at = c(1,2,3,4), labels = c("0","1","2","3"))
points(c(1,2,3,4),c(no_part_sync,one_part_sync,two_part_sync,three_part_sync),pch = 18, cex = 3, col = "red")
legend("bottomright",legend = c("synchronous","non-synchronous"), fill = c("black","red"))
dev.off()

## Plot the distributions of the stochastic lambdas competitive exclusion
png("Manuscript/Figures/comp_conv_NS.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(1,1,1,1))
plot(density(lams_comp_stoch[,1]), col = vcol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "a)                                                                                                       ",ylim = c(0,140), xlim = c(0.95,1.01))
abline(v = mean(lams_comp_stoch[,1]),col = vcol, lty = 2, lwd =3)
legend("topright", legend = c("Vacant"), fill = c(vcol), cex = 1.5)
plot(density(lams_comp_stoch[,2]), col = ccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "b)                                                                                                       ",ylim = c(0,140), xlim = c(0.95,1.01))
abline(v = mean(lams_comp_stoch[,2]),col = ccol, lty = 2, lwd =3)
lines(density(lams_comp_stoch[,3]), col = lcol, lwd =3)
abline(v = mean(lams_comp_stoch[,3]),col = lcol, lty = 2, lwd =3)
lines(density(lams_comp_stoch[,4]), col = ocol, lwd =3)
abline(v = mean(lams_comp_stoch[,4]),col = ocol, lty = 2, lwd =3)
legend("topright", legend = c("C. opun","L. apic", "Other"), fill = c(ccol,lcol,ocol), cex = 1.5)
plot(density(lams_comp_stoch[,5]), col = lccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "c)                                                                                                       ",ylim = c(0,140), xlim = c(0.95,1.01))
abline(v = mean(lams_comp_stoch[,5]),col = lccol, lty = 2, lwd =3)
lines(density(lams_comp_stoch[,6]), col = locol, lwd =3)
abline(v = mean(lams_comp_stoch[,6]),col = locol, lty = 2, lwd =3)
lines(density(lams_comp_stoch[,7]), col = cocol, lwd =3)
abline(v = mean(lams_comp_stoch[,7]),col = cocol, lty = 2, lwd =3)
legend("topright", legend = c("C. opun and L. apic","L. apic and Other", "C. opun and Other"), fill = c(lccol,locol,cocol), cex = 1.5)
plot(density(lams_comp_stoch[,8]), col = acol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "d)                                                                                                       ",ylim = c(0,140), xlim = c(0.95,1.01))
abline(v = mean(lams_comp_stoch[,8]),col = acol, lty = 2, lwd =3)
mtext("Lambda",side=1,line=-2,outer=TRUE,cex=1.3)
mtext("Density",side=2,line=-2,outer=TRUE,cex=1.3,las=0)
legend("topright",legend = c("All Ants"),fill = c(acol),
       cex = 1.5)
dev.off()

## Plot the distributions of the stochastic null lambdas competitive exclusion
png("Manuscript/Figures/comp_conv_S.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(1,1,1,1))
plot(density(lams_comp_stoch_null[,1]), col = vcol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "a)                                                                                                       ",ylim = c(0,100), xlim = c(0.92,1.01))
abline(v = mean(lams_comp_stoch_null[,1]),col = vcol, lty = 2, lwd =3)
legend("topright", legend = c("Vacant"), fill = c(vcol), cex = 1.5)
plot(density(lams_comp_stoch_null[,2]), col = ccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "b)                                                                                                       ",ylim = c(0,100), xlim = c(0.92,1.01))
abline(v = mean(lams_comp_stoch_null[,2]),col = ccol, lty = 2, lwd =3)
lines(density(lams_comp_stoch_null[,3]), col = lcol, lwd =3)
abline(v = mean(lams_comp_stoch_null[,3]),col = lcol, lty = 2, lwd =3)
lines(density(lams_comp_stoch_null[,4]), col = ocol, lwd =3)
abline(v = mean(lams_comp_stoch_null[,4]),col = ocol, lty = 2, lwd =3)
legend("topright", legend = c("Crematogaster","Liometopum", "Other"), fill = c(ccol,lcol,ocol), cex = 1.5)
plot(density(lams_comp_stoch_null[,5]), col = lccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "c)                                                                                                       ",ylim = c(0,100), xlim = c(0.92,1.01))
abline(v = mean(lams_comp_stoch_null[,5]),col = lccol, lty = 2, lwd =3)
lines(density(lams_comp_stoch_null[,6]), col = locol, lwd =3)
abline(v = mean(lams_comp_stoch_null[,6]),col = locol, lty = 2, lwd =3)
lines(density(lams_comp_stoch_null[,7]), col = cocol, lwd =3)
abline(v = mean(lams_comp_stoch_null[,7]),col = cocol, lty = 2, lwd =3)
legend("topright", legend = c("Crematogaster and Liometopum","Liometopum and Other", "Crematogaster and Other"), fill = c(lccol,locol,cocol), cex = 1.5)
plot(density(lams_comp_stoch_null[,8]), col = acol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "d)                                                                                                       ",ylim = c(0,100), xlim = c(0.92,1.01))
abline(v = mean(lams_comp_stoch_null[,8]),col = acol, lty = 2, lwd =3)
mtext("Lambda",side=1,line=-2,outer=TRUE,cex=1.3)
mtext("Density",side=2,line=-2,outer=TRUE,cex=1.3,las=0)
legend("topright",legend = c("All Ants"),fill = c(acol),
       cex = 1.5)
dev.off()

## Plot the distributions of the stochastic lambdas frequency based
png("Manuscript/Figures/freq_conv_NS.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(1,1,1,1))
plot(density(lams_freq_stoch[,1]), col = vcol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "a)                                                                                                       ",ylim = c(0,140), xlim = c(0.92,1.01))
abline(v = mean(lams_freq_stoch[,1]),col = vcol, lty = 2, lwd =3)
legend("topright", legend = c("Vacant"), fill = c(vcol), cex = 1.5)
plot(density(lams_freq_stoch[,2]), col = ccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "b)                                                                                                       ",ylim = c(0,140), xlim = c(0.92,1.01))
abline(v = mean(lams_freq_stoch[,2]),col = ccol, lty = 2, lwd =3)
lines(density(lams_freq_stoch[,3]), col = lcol, lwd =3)
abline(v = mean(lams_freq_stoch[,3]),col = lcol, lty = 2, lwd =3)
lines(density(lams_freq_stoch[,4]), col = ocol, lwd =3)
abline(v = mean(lams_freq_stoch[,4]),col = ocol, lty = 2, lwd =3)
legend("topright", legend = c("C. opun","L. apic", "Other"), fill = c(ccol,lcol,ocol), cex = 1.5)
plot(density(lams_freq_stoch[,5]), col = lccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "c)                                                                                                       ",ylim = c(0,140), xlim = c(0.92,1.01))
abline(v = mean(lams_freq_stoch[,5]),col = lccol, lty = 2, lwd =3)
lines(density(lams_freq_stoch[,6]), col = locol, lwd =3)
abline(v = mean(lams_freq_stoch[,6]),col = locol, lty = 2, lwd =3)
lines(density(lams_freq_stoch[,7]), col = cocol, lwd =3)
abline(v = mean(lams_freq_stoch[,7]),col = cocol, lty = 2, lwd =3)
legend("topright", legend = c("C. opun and L. apic","L. apic and Other", "C. opun and Other"), fill = c(lccol,locol,cocol), cex = 1.5)
plot(density(lams_freq_stoch[,8]), col = acol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "d)                                                                                                       ",ylim = c(0,140), xlim = c(0.92,1.01))
abline(v = mean(lams_freq_stoch[,8]),col = acol, lty = 2, lwd =3)
mtext("Lambda",side=1,line=-2,outer=TRUE,cex=1.3)
mtext("Density",side=2,line=-2,outer=TRUE,cex=1.3,las=0)
legend("topright",legend = c("All Ants"),fill = c(acol),
       cex = 1.5)
dev.off()

## Plot the distributions of the stochastic null lambdas frequency based
png("Manuscript/Figures/freq_conv_S.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(1,1,1,1))
plot(density(lams_freq_stoch_null[,1]), col = vcol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "a)                                                                                                       ",ylim = c(0,100), xlim = c(0.92,1.01))
abline(v = mean(lams_freq_stoch_null[,1]),col = vcol, lty = 2, lwd =3)
legend("topright", legend = c("Vacant"), fill = c(vcol), cex = 1.5)
plot(density(lams_freq_stoch_null[,2]), col = ccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "b)                                                                                                       ",ylim = c(0,100), xlim = c(0.92,1.01))
abline(v = mean(lams_freq_stoch_null[,2]),col = ccol, lty = 2, lwd =3)
lines(density(lams_freq_stoch_null[,3]), col = lcol, lwd =3)
abline(v = mean(lams_freq_stoch_null[,3]),col = lcol, lty = 2, lwd =3)
lines(density(lams_freq_stoch_null[,4]), col = ocol, lwd =3)
abline(v = mean(lams_freq_stoch_null[,4]),col = ocol, lty = 2, lwd =3)
legend("topright", legend = c("Crematogaster","Liometopum", "Other"), fill = c(ccol,lcol,ocol), cex = 1.5)
plot(density(lams_freq_stoch_null[,5]), col = lccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "c)                                                                                                       ",ylim = c(0,100), xlim = c(0.92,1.01))
abline(v = mean(lams_freq_stoch_null[,5]),col = lccol, lty = 2, lwd =3)
lines(density(lams_freq_stoch_null[,6]), col = locol, lwd =3)
abline(v = mean(lams_freq_stoch_null[,6]),col = locol, lty = 2, lwd =3)
lines(density(lams_freq_stoch_null[,7]), col = cocol, lwd =3)
abline(v = mean(lams_freq_stoch_null[,7]),col = cocol, lty = 2, lwd =3)
legend("topright", legend = c("Crematogaster and Liometopum","Liometopum and Other", "Crematogaster and Other"), fill = c(lccol,locol,cocol), cex = 1.5)
plot(density(lams_freq_stoch_null[,8]), col = acol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "d)                                                                                                       ",ylim = c(0,100), xlim = c(0.92,1.01))
abline(v = mean(lams_freq_stoch_null[,8]),col = acol, lty = 2, lwd =3)
mtext("Lambda",side=1,line=-2,outer=TRUE,cex=1.3)
mtext("Density",side=2,line=-2,outer=TRUE,cex=1.3,las=0)
legend("topright",legend = c("All Ants"),fill = c(acol),
       cex = 1.5)
dev.off()

## Plot the distributions of the stochastic lambdas equal likelihood
png("Manuscript/Figures/equal_conv_NS.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(1,1,1,1))
plot(density(lams_equal_stoch[,1]), col = vcol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "a)                                                                                                       ",ylim = c(0,140), xlim = c(0.92,1.01))
abline(v = mean(lams_equal_stoch[,1]),col = vcol, lty = 2, lwd =3)
legend("topright", legend = c("Vacant"), fill = c(vcol), cex = 1.5)
plot(density(lams_equal_stoch[,2]), col = ccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "b)                                                                                                       ",ylim = c(0,140), xlim = c(0.92,1.01))
abline(v = mean(lams_equal_stoch[,2]),col = ccol, lty = 2, lwd =3)
lines(density(lams_equal_stoch[,3]), col = lcol, lwd =3)
abline(v = mean(lams_equal_stoch[,3]),col = lcol, lty = 2, lwd =3)
lines(density(lams_equal_stoch[,4]), col = ocol, lwd =3)
abline(v = mean(lams_equal_stoch[,4]),col = ocol, lty = 2, lwd =3)
legend("topright", legend = c("C. opun","L. apic", "Other"), fill = c(ccol,lcol,ocol), cex = 1.5)
plot(density(lams_equal_stoch[,5]), col = lccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "c)                                                                                                       ",ylim = c(0,140), xlim = c(0.92,1.01))
abline(v = mean(lams_equal_stoch[,5]),col = lccol, lty = 2, lwd =3)
lines(density(lams_equal_stoch[,6]), col = locol, lwd =3)
abline(v = mean(lams_equal_stoch[,6]),col = locol, lty = 2, lwd =3)
lines(density(lams_equal_stoch[,7]), col = cocol, lwd =3)
abline(v = mean(lams_equal_stoch[,7]),col = cocol, lty = 2, lwd =3)
legend("topright", legend = c("C. opun and L. apic","L. apic and Other", "C. opun and Other"), fill = c(lccol,locol,cocol), cex = 1.5)
plot(density(lams_equal_stoch[,8]), col = acol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "d)                                                                                                       ",ylim = c(0,140), xlim = c(0.92,1.01))
abline(v = mean(lams_equal_stoch[,8]),col = acol, lty = 2, lwd =3)
mtext("Lambda",side=1,line=-2,outer=TRUE,cex=1.3)
mtext("Density",side=2,line=-2,outer=TRUE,cex=1.3,las=0)
legend("topright",legend = c("All Ants"),fill = c(acol),
       cex = 1.5)
dev.off()

## Plot the distributions of the stochastic null lambdas equal likelihood
png("Manuscript/Figures/equal_conv_S.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(1,1,1,1))
plot(density(lams_equal_stoch_null[,1]), col = vcol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "a)                                                                                                       ",ylim = c(0,100), xlim = c(0.92,1.01))
abline(v = mean(lams_equal_stoch_null[,1]),col = vcol, lty = 2, lwd =3)
legend("topright", legend = c("Vacant"), fill = c(vcol), cex = 1.5)
plot(density(lams_equal_stoch_null[,2]), col = ccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "b)                                                                                                       ",ylim = c(0,100), xlim = c(0.92,1.01))
abline(v = mean(lams_equal_stoch_null[,2]),col = ccol, lty = 2, lwd =3)
lines(density(lams_equal_stoch_null[,3]), col = lcol, lwd =3)
abline(v = mean(lams_equal_stoch_null[,3]),col = lcol, lty = 2, lwd =3)
lines(density(lams_equal_stoch_null[,4]), col = ocol, lwd =3)
abline(v = mean(lams_equal_stoch_null[,4]),col = ocol, lty = 2, lwd =3)
legend("topright", legend = c("Crematogaster","Liometopum", "Other"), fill = c(ccol,lcol,ocol), cex = 1.5)
plot(density(lams_equal_stoch_null[,5]), col = lccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "c)                                                                                                       ",ylim = c(0,100), xlim = c(0.92,1.01))
abline(v = mean(lams_equal_stoch_null[,5]),col = lccol, lty = 2, lwd =3)
lines(density(lams_equal_stoch_null[,6]), col = locol, lwd =3)
abline(v = mean(lams_equal_stoch_null[,6]),col = locol, lty = 2, lwd =3)
lines(density(lams_equal_stoch_null[,7]), col = cocol, lwd =3)
abline(v = mean(lams_equal_stoch_null[,7]),col = cocol, lty = 2, lwd =3)
legend("topright", legend = c("Crematogaster and Liometopum","Liometopum and Other", "Crematogaster and Other"), fill = c(lccol,locol,cocol), cex = 1.5)
plot(density(lams_equal_stoch_null[,8]), col = acol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "d)                                                                                                       ",ylim = c(0,100), xlim = c(0.92,1.01))
abline(v = mean(lams_equal_stoch_null[,8]),col = acol, lty = 2, lwd =3)
mtext("Lambda",side=1,line=-2,outer=TRUE,cex=1.3)
mtext("Density",side=2,line=-2,outer=TRUE,cex=1.3,las=0)
legend("topright",legend = c("All Ants"),fill = c(acol),
       cex = 1.5)
dev.off()


######################################################################################################
######################################################################################################
####                      COMPARE EACH OF THE POSTERIOR DISTRIBUTIONS                             ####
######################################################################################################
######################################################################################################

############################### Competitive Exclusion ##########################
# Compare the stochastic posterior distributions to vacancy
# Calculate the difference in the between the posterior distributions of lambda
colnames(lams_comp_stoch) <- scenario
all_vac <- lams_comp_stoch$all - lams_comp_stoch$none
cl_vac <- lams_comp_stoch$liomcremvac - lams_comp_stoch$none
lo_vac <- lams_comp_stoch$liomvacother - lams_comp_stoch$none
co_vac <- lams_comp_stoch$othercremvac - lams_comp_stoch$none
c_vac <- lams_comp_stoch$cremvac - lams_comp_stoch$none
l_vac <- lams_comp_stoch$liomvac - lams_comp_stoch$none
o_vac <- lams_comp_stoch$othervac - lams_comp_stoch$none
vac_vac <- lams_comp_stoch$none - lams_comp_stoch$none
#plot them
par(mar=c(1,1,1,1))
layout(matrix(c(1,2,3,4,5,6,7),
              ncol = 1, nrow = 7), heights = c(1,1,1,1,1,1,1))
## All
plot(density(all_vac), col = acol, xlim = c(-0.025,0.04))
abline(v = 0, col = acol, lty = 2)
## Crem and Liom
plot(density(cl_vac), col = lccol, xlim = c(-0.025,0.04))
abline(v = 0, col = lccol, lty = 2)
## Other and Liom
plot(density(lo_vac), col = locol, xlim = c(-0.025,0.04))
abline(v = 0, col = locol, lty = 2)
## Other and Crem
plot(density(co_vac), col = cocol, xlim = c(-0.025,0.04))
abline(v = 0, col = cocol, lty = 2)
## Crem
plot(density(c_vac), col = ccol, xlim = c(-0.025,0.04))
abline(v = 0, col = ccol, lty = 2)
## Liom
plot(density(l_vac), col = lcol, xlim = c(-0.025,0.04))
abline(v = 0, col = lcol, lty = 2)
## Other
plot(density(o_vac), col = ocol, xlim = c(-0.025,0.04))
abline(v = 0, col = ocol, lty = 2)
#calculate what proportion of each is > 0
#aka what proportion of lambda estimations are greater than when vacant
proportions <- vector()
proportions[1] <- 0
proportions[2] <- length(subset(c_vac, c_vac>0))/100
proportions[3] <- length(subset(l_vac, l_vac>0))/100
proportions[4] <- length(subset(o_vac, o_vac>0))/100
proportions[5] <- length(subset(cl_vac, cl_vac>0))/100
proportions[6] <- length(subset(lo_vac, lo_vac>0))/100
proportions[7] <- length(subset(co_vac, co_vac>0))/100
proportions[8] <- length(subset(all_vac, all_vac>0))/100
proportions
prop <- as.data.frame(matrix(rep(NA,8), ncol = 8))
colnames(prop) <- scenario_abv
prop[1,] <- proportions
prop

## Compare the stochastic posterior distributions to scenarios with liom
none_l <- lams_comp_stoch$liomvac - lams_comp_stoch$none
crem_l <- lams_comp_stoch$liomvac - lams_comp_stoch$cremvac
other_l <- lams_comp_stoch$liomvac - lams_comp_stoch$othervac
co_l <- lams_comp_stoch$liomvac - lams_comp_stoch$othercremvac
proportions <- vector()
proportions[1] <- length(subset(none_l, none_l>=0))/100
proportions[2] <- length(subset(crem_l, crem_l>=0))/100
proportions[3] <- length(subset(other_l, other_l>=0))/100
proportions

################################ Frequency Based ###############################
# Compare the stochastic posterior distributions to vacancy
# Calculate the difference in the between the posterior distributions of lambda
all_vac <- lams_freq_stoch$all - lams_freq_stoch$none
cl_vac <- lams_freq_stoch$liomcremvac - lams_freq_stoch$none
lo_vac <- lams_freq_stoch$liomvacother - lams_freq_stoch$none
co_vac <- lams_freq_stoch$othercremvac - lams_freq_stoch$none
c_vac <- lams_freq_stoch$cremvac - lams_freq_stoch$none
l_vac <- lams_freq_stoch$liomvac - lams_freq_stoch$none
o_vac <- lams_freq_stoch$othervac - lams_freq_stoch$none
vac_vac <- lams_freq_stoch$none - lams_freq_stoch$none
#plot them
par(mar=c(1,1,1,1))
layout(matrix(c(1,2,3,4,5,6,7),
              ncol = 1, nrow = 7), heights = c(1,1,1,1,1,1,1))
## All
plot(density(all_vac), col = acol, xlim = c(-0.025,0.04))
abline(v = 0, col = acol, lty = 2)
## Crem and Liom
plot(density(cl_vac), col = lccol, xlim = c(-0.025,0.04))
abline(v = 0, col = lccol, lty = 2)
## Other and Liom
plot(density(lo_vac), col = locol, xlim = c(-0.025,0.04))
abline(v = 0, col = locol, lty = 2)
## Other and Crem
plot(density(co_vac), col = cocol, xlim = c(-0.025,0.04))
abline(v = 0, col = cocol, lty = 2)
## Crem
plot(density(c_vac), col = ccol, xlim = c(-0.025,0.04))
abline(v = 0, col = ccol, lty = 2)
## Liom
plot(density(l_vac), col = lcol, xlim = c(-0.025,0.04))
abline(v = 0, col = lcol, lty = 2)
## Other
plot(density(o_vac), col = ocol, xlim = c(-0.025,0.04))
abline(v = 0, col = ocol, lty = 2)
#calculate what proportion of each is > 0
#aka what proportion of lambda estimations are greater than when vacant
proportions <- vector()
proportions[1] <- 0
proportions[2] <- length(subset(c_vac, c_vac>0))/100
proportions[3] <- length(subset(l_vac, l_vac>0))/100
proportions[4] <- length(subset(o_vac, o_vac>0))/100
proportions[5] <- length(subset(cl_vac, cl_vac>0))/100
proportions[6] <- length(subset(lo_vac, lo_vac>0))/100
proportions[7] <- length(subset(co_vac, co_vac>0))/100
proportions[8] <- length(subset(all_vac, all_vac>0))/100
proportions
prop <- as.data.frame(matrix(rep(NA,8), ncol = 8))
colnames(prop) <- scenario
prop[1,] <- proportions
prop

## Compare the stochastic posterior distributions to scenarios with liom
none_l <- lams_freq_stoch$liomvac - lams_freq_stoch$none
crem_l <- lams_freq_stoch$liomvac - lams_freq_stoch$cremvac
other_l <- lams_freq_stoch$liomvac - lams_freq_stoch$othervac
co_l <- lams_freq_stoch$liomvac - lams_freq_stoch$othercremvac
proportions <- vector()
proportions[1] <- length(subset(none_l, none_l>=0))/100
proportions[2] <- length(subset(crem_l, crem_l>=0))/100
proportions[3] <- length(subset(other_l, other_l>=0))/100
proportions


############################### Equal Opportunity ##############################
# Compare the stochastic posterior distributions to vacancy
# Calculate the difference in the between the posterior distributions of lambda
all_vac <- lams_equal_stoch$all - lams_equal_stoch$none
cl_vac <- lams_equal_stoch$liomcremvac - lams_equal_stoch$none
lo_vac <- lams_equal_stoch$liomvacother - lams_equal_stoch$none
co_vac <- lams_equal_stoch$othercremvac - lams_equal_stoch$none
c_vac <- lams_equal_stoch$cremvac - lams_equal_stoch$none
l_vac <- lams_equal_stoch$liomvac - lams_equal_stoch$none
o_vac <- lams_equal_stoch$othervac - lams_equal_stoch$none
vac_vac <- lams_equal_stoch$none - lams_equal_stoch$none
#plot them
par(mar=c(1,1,1,1))
layout(matrix(c(1,2,3,4,5,6,7),
              ncol = 1, nrow = 7), heights = c(1,1,1,1,1,1,1))
## All
plot(density(all_vac), col = acol, xlim = c(-0.025,0.04))
abline(v = 0, col = acol, lty = 2)
## Crem and Liom
plot(density(cl_vac), col = lccol, xlim = c(-0.025,0.04))
abline(v = 0, col = lccol, lty = 2)
## Other and Liom
plot(density(lo_vac), col = locol, xlim = c(-0.025,0.04))
abline(v = 0, col = locol, lty = 2)
## Other and Crem
plot(density(co_vac), col = cocol, xlim = c(-0.025,0.04))
abline(v = 0, col = cocol, lty = 2)
## Crem
plot(density(c_vac), col = ccol, xlim = c(-0.025,0.04))
abline(v = 0, col = ccol, lty = 2)
## Liom
plot(density(l_vac), col = lcol, xlim = c(-0.025,0.04))
abline(v = 0, col = lcol, lty = 2)
## Other
plot(density(o_vac), col = ocol, xlim = c(-0.025,0.04))
abline(v = 0, col = ocol, lty = 2)
#calculate what proportion of each is > 0
#aka what proportion of lambda estimations are greater than when vacant
proportions <- vector()
proportions[1] <- 0
proportions[2] <- length(subset(c_vac, c_vac>0))/100
proportions[3] <- length(subset(l_vac, l_vac>0))/100
proportions[4] <- length(subset(o_vac, o_vac>0))/100
proportions[5] <- length(subset(cl_vac, cl_vac>0))/100
proportions[6] <- length(subset(lo_vac, lo_vac>0))/100
proportions[7] <- length(subset(co_vac, co_vac>0))/100
proportions[8] <- length(subset(all_vac, all_vac>0))/100
proportions
prop <- as.data.frame(matrix(rep(NA,8), ncol = 8))
colnames(prop) <- scenario_abv
prop[1,] <- proportions
prop

## Compare the stochastic posterior distributions to scenarios with liom
none_l <- lams_equal_stoch$liomvac - lams_equal_stoch$none
crem_l <- lams_equal_stoch$liomvac - lams_equal_stoch$cremvac
other_l <- lams_equal_stoch$liomvac - lams_equal_stoch$othervac
co_l <- lams_equal_stoch$liomvac - lams_equal_stoch$othercremvac
proportions <- vector()
proportions[1] <- length(subset(none_l, none_l>=0))/100
proportions[2] <- length(subset(crem_l, crem_l>=0))/100
proportions[3] <- length(subset(other_l, other_l>=0))/100
proportions





########################################### STOCHASTIC AND DETERMINISTIC #############################
# Compare the deterministic difference distributions to the stochastic difference distributions
# to determine if portfolio effect is at play
colnames(lams_comp_stoch_null) <- scenario
all_vac_stoch_null <- lams_comp_stoch$all - lams_comp_stoch_null$none
all_vac_stoch <- lams_comp_stoch$all - lams_comp_stoch$none
# Plot the boost offered by the real ant scenario based on stochastic and deterministic lambda estimates
png("Manuscript/Figures/portfolio_effect.png")
plot(density(all_vac_stoch_null), lwd = 3, col = "chartreuse4", ylim = c(0,30), xlab = "Effect of Partner Presence on Fitness", main = "", cex.lab = 2)
lines(density(all_vac_stoch), lwd = 3, col = "violet")
abline(v = 0, lty = 2, lwd = 3)
legend("topright",legend = c("Synchronicity Possible","Synchronicity Excluded"), fill = c("violet","chartreuse4"))
dev.off()
# check the mean density 
mean(all_vac_stoch_null>0)
mean(all_vac_stoch>0)
# there appears to be a stronger fitness effect when the ants can fluctuate independently -- not a very strong portfolio effect

# What proprotion of the difference in these is >0
# 0% confident that there is a fitness boost from partner diversity
length(subset((all_vac_stoch-all_vac_stoch_null), (all_vac_stoch-all_vac_stoch_null)>0))/100

## Check which mean has a larger difference -- the answer is null has a greater difference (no portfolio effect)
mean(lams_comp_stoch$all) - mean(lams_comp_stoch$none)
mean(lams_comp_stoch_null$all) - mean(lams_comp_stoch_null$none)

#################################################################################################
#################################################################################################
######## VISUALIZE THE MEAN LAMBDA BY NUMBER OF ANT PARTNERS RATHER THAN IDENTITY ###############
#################################################################################################
#################################################################################################
## No partners mean
zero_part <- vac_mean
## One partners mean
one_part <- mean(c(liom_mean,crem_mean,other_mean))
## Two partners mean
two_part <- mean(c(liomcrem_mean,liomother_mean,othercrem_mean))
## All partners mean
all_part <- (all_mean)

zero_part <- 0.9643225
## One partners mean
one_part <- mean(c(0.9767861,0.9587724,0.9531965))
## Two partners mean
two_part <- mean(c(0.9627127,0.9755172,0.9748269))
## All partners mean
all_part <- (0.9745587)

## Plot these means
png("Figures/Lambda_Num_Partners_freq.png")
plot(x = c(0,1,2,3), y = c(zero_part,one_part,two_part,all_part), xlab = "Number of Partners", ylab = "Mean Fitness", pch = 20, cex = 2.5,cex.lab = 1.8)
dev.off()

################################################################################
## VISUALIZE THE STABLE STAGE DISTRIBUTIONS
################################################################################


                        
                        
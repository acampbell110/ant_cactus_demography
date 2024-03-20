setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
source( "/Users/alicampbell/Documents/GitHub/ant_cactus_demography/03_cholla_ant_IPM_params_functions.R")
size_dummy <- seq(min(cactus$logsize_t, na.rm = T), max(cactus$logsize_t, na.rm = TRUE), by = 0.1)

################################################################################
## Growth model
################################################################################
################################################################################
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




################################################################################
## Survival Model
################################################################################


################################################################################
## Flowering Model
################################################################################



################################################################################
## Viability Model
################################################################################

## min viab rate of plants
(mean(invlogit(viab.params$beta0[,1])))
(mean(invlogit(viab.params$beta0[,2])))
(mean(invlogit(viab.params$beta0[,3])))
(mean(invlogit(viab.params$beta0[,4])))

## Confidence intervals
c <- ((invlogit(viab.params$beta0[,1])))
l <- ((invlogit(viab.params$beta0[,2])))
o <- ((invlogit(viab.params$beta0[,3])))
v <- ((invlogit(viab.params$beta0[,4])))
#Liom
c_v <- l-v
c_l <- l-c
c_o <- l-o
length(subset(c_v, c_v>0))/7500
length(subset(c_l, c_l>0))/7500
length(subset(c_o, c_o>0))/7500
#Vacant
c_v <- l-v
c_l <- c-v
c_o <- o-v
length(subset(c_v, c_v>0))/7500
length(subset(c_l, c_l>0))/7500
length(subset(c_o, c_o>0))/7500


################################################################################
## Repro
################################################################################


## Mean flowers produced
small <- seq(min(cactus$logsize_t, na.rm = T), 2.3, length = 205)
y_other <- invlogit(quantile(repro.params$beta0,.5) + small * quantile(repro.params$beta1,.5))
mean(y_other)
small <- seq(2.3, 5.01, length = 205)
y_other <- invlogit(quantile(repro.params$beta0,.5) + small * quantile(repro.params$beta1,.5))
mean(y_other)
small <- seq(5.01, max(cactus$logsize_t, na.rm = T), length = 205)
y_other <- invlogit(quantile(repro.params$beta0,.5) + small * quantile(repro.params$beta1,.5))
mean(y_other)

################################################################################
## Seeds per flower
################################################################################


## Min and max per ant 1 = crem, 2 = liom, 3 = vac
max(exp(seed.params$beta0[,1]))
max(exp(seed.params$beta0[,2]))
max(exp(seed.params$beta0[,3]))

## Confidence intervals
c <- exp(seed.params$beta0[,1])
l <- exp(seed.params$beta0[,2])
v <- exp(seed.params$beta0[,3])
#Liom
c_v <- v-c
c_l <- v-l
a <- c-l
length(subset(c_v, c_v>0))/7500
length(subset(c_l, c_l>0))/7500
length(subset(a, a>0))/7500

#Vacant
c_v <- l-v
c_l <- c-v
c_o <- o-v
length(subset(c_v, c_v>0))/7500
length(subset(c_l, c_l>0))/7500
length(subset(c_o, c_o>0))/7500

################################################################################
## Precensus Survival
################################################################################

mean(invlogit(y_surv))
min(invlogit(y_surv))
max(invlogit(y_surv))

################################################################################
## Germination Models
################################################################################


## Min max and mean
min(invlogit(y_germ1))
max(invlogit(y_germ1))
mean(invlogit(y_germ1))
min(invlogit(y_germ2))
max(invlogit(y_germ2))
mean(invlogit(y_germ2))

################################################################################
## Recuits Model
################################################################################


## Mean min max
exp(mean(rec.params$beta0))
exp(min(rec.params$beta0))
exp(max(rec.params$beta0))

################################################################################
## Multinomial Model
################################################################################



## Previously vacant
min(pred_vac[,4])
max(pred_vac[,4])
min(pred_vac[,2])
max(pred_vac[,2])
min(pred_vac[,1])
max(pred_vac[,1])
min(pred_vac[,3])
max(pred_vac[,3])
## Previously Crem
min(pred_crem[,4])
max(pred_crem[,4])
min(pred_crem[,2])
max(pred_crem[,2])
min(pred_crem[,1])
max(pred_crem[,1])
min(pred_crem[,3])
max(pred_crem[,3])
## Previously Other
min(pred_other[,4])
max(pred_other[,4])
min(pred_other[,2])
max(pred_other[,2])
min(pred_other[,1])
max(pred_other[,1])
min(pred_other[,3])
max(pred_other[,3])
## Previously Liom
min(pred_liom[,4])
max(pred_liom[,4])
min(pred_liom[,2])
max(pred_liom[,2])
min(pred_liom[,1])
max(pred_liom[,1])
min(pred_liom[,3])
max(pred_liom[,3])

## Small and med plants probability of being tended vs vacant
mean(pred_crem[,4])
mean(pred_liom[,4])
mean(pred_other[,4])
mean(pred_vac[,4])

## Probs of being liom as large plant (prev vac)
vl <- pred_vac[,2] - pred_vac[,4]
ll <- pred_vac[,2] - pred_vac[,1]
ol <- pred_vac[,2] - pred_vac[,3]
length(subset(vl,vl>0))/100
length(subset(ll,ll>0))/100
length(subset(ol,ol>0))/100
## Probs of being liom as large plant (prev liom)
vl <- pred_liom[,2] - pred_liom[,4]
ll <- pred_liom[,2] - pred_liom[,1]
ol <- pred_liom[,2] - pred_liom[,3]
length(subset(vl,vl>0))/100
length(subset(ll,ll>0))/100
length(subset(ol,ol>0))/100
## Probs of being liom as large plant (prev other)
vl <- pred_other[,2] - pred_other[,4]
ll <- pred_other[,2] - pred_other[,1]
ol <- pred_other[,2] - pred_other[,3]
length(subset(vl,vl>0))/100
length(subset(ll,ll>0))/100
length(subset(ol,ol>0))/100
## Probs of being crem as large plant (prev crem)
vl <- pred_crem[,1] - pred_crem[,4]
ll <- pred_crem[,1] - pred_crem[,2]
ol <- pred_crem[,1] - pred_crem[,3]
length(subset(vl,vl>0))/100
length(subset(ll,ll>0))/100
length(subset(ol,ol>0))/100





#### 3 ANTS --------------------------------------------------------------------
#### LIOM VAC CREM
## Calculate the probabilities of being tended by each ant species
# Previously tended by vac
Denominator_vac <- exp(mean(multi.params$beta[draws,4,4]) + size_dummy*mean(multi.params$beta[draws,5,4])) +
  exp(mean(multi.params$beta[draws,4,2]) + size_dummy*mean(multi.params$beta[draws,5,2])) + 
  exp(mean(multi.params$beta[draws,4,3]) + size_dummy*mean(multi.params$beta[draws,5,3]))
pred_vac<-cbind(
  #pr(vacant)
  exp(mean(multi.params$beta[draws,4,4]) + size_dummy*mean(multi.params$beta[draws,5,4]))/Denominator_vac,
  #pr(liom)
  exp(mean(multi.params$beta[draws,4,2]) + size_dummy*mean(multi.params$beta[draws,5,2]))/Denominator_vac,
  #pr(other)
  exp(mean(multi.params$beta[draws,4,3]) + size_dummy*mean(multi.params$beta[draws,5,3]))/Denominator_vac)
sum(pred_vac[1,])
# Previously tended by Liom
Denominator_liom <- exp(mean(multi.params$beta[draws,2,4]) + size_dummy*mean(multi.params$beta[draws,5,4])) + exp(mean(multi.params$beta[draws,2,2]) + size_dummy*mean(multi.params$beta[draws,5,2])) + 
  exp(mean(multi.params$beta[draws,2,3]) + size_dummy*mean(multi.params$beta[draws,5,3]))
pred_liom<-cbind(
  #pr(vacant)
  exp(mean(multi.params$beta[draws,2,4]) + size_dummy*mean(multi.params$beta[draws,5,4]))/Denominator_liom,
  #pr(liom)
  exp(mean(multi.params$beta[draws,2,2]) + size_dummy*mean(multi.params$beta[draws,5,2]))/Denominator_liom,
  #pr(other)
  exp(mean(multi.params$beta[draws,2,3]) + size_dummy*mean(multi.params$beta[draws,5,3]))/Denominator_liom)
sum(pred_liom[1,])
# Previously tended by Other
Denominator_other <- exp(mean(multi.params$beta[draws,3,4]) + size_dummy*mean(multi.params$beta[draws,5,4])) + exp(mean(multi.params$beta[draws,3,2]) + size_dummy*mean(multi.params$beta[draws,5,2])) + 
  exp(mean(multi.params$beta[draws,3,3]) + size_dummy*mean(multi.params$beta[draws,5,3]))
pred_other<-cbind(
  #pr(vacant)
  exp(mean(multi.params$beta[draws,3,4]) + size_dummy*mean(multi.params$beta[draws,5,4]))/Denominator_other,
  #pr(liom
  exp(mean(multi.params$beta[draws,3,2]) + size_dummy*mean(multi.params$beta[draws,5,2]))/Denominator_other,
  #pr(other)
  exp(mean(multi.params$beta[draws,3,3]) + size_dummy*mean(multi.params$beta[draws,5,3]))/Denominator_other)
sum(pred_other[1,])
## Plot the probabilities of next ant partner based on previous partner and size -- include model fits and real data
png("Ant_3_LVC_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4, byrow = TRUE), heights = c(0.6,1,1,1), widths = c(5))
plot.new()
text(0.5,0.5,"Ant States \n Vacant, Liom., and Other",cex=2,font=2)
plot(size_dummy, pred_vac[,1], type = "l", col = vaccol,main = "Previously Vacant", ylim = c(0,1))
lines(size_dummy, pred_vac[,2], col = liomcol)
lines(size_dummy, pred_vac[,3], col = othercol)
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_liom,pch=16,cex=multi_plot_vac$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_other,pch=16,cex=multi_plot_vac$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_vac,pch=16,cex=multi_plot_vac$N_mod,col= alpha(vaccol, 0.4))
legend("topleft",c("vacant","liom","other"), fill = c(vaccol,liomcol,othercol))
plot(size_dummy, pred_other[,1], type = "l", col = vaccol,main = "Previously Other", ylim = c(0,1))
lines(size_dummy, pred_other[,2], col = liomcol)
lines(size_dummy, pred_other[,3], col = othercol)
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_liom,pch=16,cex=multi_plot_other$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_other,pch=16,cex=multi_plot_other$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_vac,pch=16,cex=multi_plot_other$N_mod,col= alpha(vaccol, 0.4))
plot(size_dummy, pred_liom[,1], type = "l", col = vaccol,main = "Previously Liom", ylim = c(0,1))
lines(size_dummy, pred_liom[,2], col = liomcol)
lines(size_dummy, pred_liom[,3], col = othercol)
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_liom,pch=16,cex=multi_plot_liom$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_other,pch=16,cex=multi_plot_liom$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_vac,pch=16,cex=multi_plot_liom$N_mod,col= alpha(vaccol, 0.4))
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
#### LIOM OTHER CREM
## Calculate the probabilities of being tended by each ant species
# Previously tended by Other
Denominator_other <- exp(mean(multi.params$beta[draws,3,3]) + size_dummy*mean(multi.params$beta[draws,5,3])) + exp(mean(multi.params$beta[draws,3,1]) + size_dummy*mean(multi.params$beta[draws,5,1])) + 
  exp(mean(multi.params$beta[draws,3,2]) + size_dummy*mean(multi.params$beta[draws,5,2]))
pred_other<-cbind(
  #pr(other)
  exp((mean(multi.params$beta[draws,3,3])) + size_dummy*mean(multi.params$beta[draws,5,3]))/Denominator_other,
  #pr(crem)
  exp((mean(multi.params$beta[draws,3,1])) + size_dummy*mean(multi.params$beta[draws,5,1]))/Denominator_other,
  #pr(liom)
  exp((mean(multi.params$beta[draws,3,2])) + size_dummy*mean(multi.params$beta[draws,5,2]))/Denominator_other)
sum(pred_other[1,])
# Previously tended by Crem
Denominator_crem <- exp(mean(multi.params$beta[draws,1,1]) + size_dummy*mean(multi.params$beta[draws,5,1])) + exp(mean(multi.params$beta[draws,1,3]) + size_dummy*mean(multi.params$beta[draws,5,3])) + 
  exp(mean(multi.params$beta[draws,1,2]) + size_dummy*mean(multi.params$beta[draws,5,2]))
pred_crem<-cbind(
  #pr(crem)
  exp(mean(multi.params$beta[draws,1,1]) + size_dummy*mean(multi.params$beta[draws,5,1]))/Denominator_crem,
  #pr(other)
  exp(mean(multi.params$beta[draws,1,3]) + size_dummy*mean(multi.params$beta[draws,5,3]))/Denominator_crem,
  #pr(liom)
  exp(mean(multi.params$beta[draws,1,2]) + size_dummy*mean(multi.params$beta[draws,5,2]))/Denominator_crem)
sum(pred_crem[1,])
# Previously tended by Liom
Denominator_liom <- exp(mean(multi.params$beta[draws,2,3]) + size_dummy*mean(multi.params$beta[draws,5,3])) + 
  exp(mean(multi.params$beta[draws,2,1]) + size_dummy*mean(multi.params$beta[draws,5,1])) + 
  exp(mean(multi.params$beta[draws,2,2]) + size_dummy*mean(multi.params$beta[draws,5,2]))
pred_liom<-cbind(
  #pr(other)
  exp(mean(multi.params$beta[draws,2,3]) + size_dummy*mean(multi.params$beta[draws,5,3]))/Denominator_liom,
  #pr(crem)
  exp(mean(multi.params$beta[draws,2,2]) + size_dummy*mean(multi.params$beta[draws,5,2]))/Denominator_liom,
  #pr(liom)
  exp(mean(multi.params$beta[draws,2,1]) + size_dummy*mean(multi.params$beta[draws,5,1]))/Denominator_liom)
sum(pred_liom[1,])
## Plot the probabilities of next partner based on previous partner and size -- show model fits and real data
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_3_LOC_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4, byrow = TRUE), heights = c(0.6,1,1,1), widths = c(5))
plot.new()
text(0.5,0.5,"Ant States \n Other, Crem., and Liom.",cex=2,font=2)
plot(size_dummy, pred_other[,1], type = "l", col = othercol,main = "Previously Other", ylim = c(0,1))
lines(size_dummy, pred_other[,2], col = cremcol)
lines(size_dummy, pred_other[,3], col = liomcol)
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_liom,pch=16,cex=multi_plot_other$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_other,pch=16,cex=multi_plot_other$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_crem,pch=16,cex=multi_plot_other$N_mod,col= alpha(cremcol, 0.4))
legend("topleft",c("other","crem.","liom."), fill = c(othercol,cremcol,liomcol))
plot(size_dummy, pred_crem[,1], type = "l", col = othercol,main = "Previously Crem", ylim = c(0,1))
lines(size_dummy, pred_crem[,2], col = cremcol)
lines(size_dummy, pred_crem[,3], col = liomcol)
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_liom,pch=16,cex=multi_plot_crem$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_other,pch=16,cex=multi_plot_crem$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_crem,pch=16,cex=multi_plot_crem$N_mod,col= alpha(cremcol, 0.4))
plot(size_dummy, pred_liom[,1], type = "l", col = othercol,main = "Previously Liom", ylim = c(0,1))
lines(size_dummy, pred_liom[,2], col = cremcol)
lines(size_dummy, pred_liom[,3], col = liomcol)
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_liom,pch=16,cex=multi_plot_liom$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_other,pch=16,cex=multi_plot_liom$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_crem,pch=16,cex=multi_plot_liom$N_mod,col= alpha(cremcol, 0.4))
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
#### LIOM & CREM & VAC
## Calculate the probabilities of being tended by each ant species
# Previously tended by none
Denominator_vac <- exp(mean(multi.params$beta[draws,4,4]) + size_dummy*mean(multi.params$beta[draws,5,4])) +  exp(mean(multi.params$beta[draws,4,1]) + size_dummy*mean(multi.params$beta[draws,5,1])) + 
  exp(mean(multi.params$beta[draws,4,2]) + size_dummy*mean(multi.params$beta[draws,5,2]))
pred_vac<-cbind(
  #pr(vacant)
  exp(mean(multi.params$beta[draws,4,4]) + size_dummy*mean(multi.params$beta[draws,5,4]))/Denominator_vac,
  #pr(crem)
  exp(mean(multi.params$beta[draws,4,1]) + size_dummy*mean(multi.params$beta[draws,5,1]))/Denominator_vac,
  #pr(liom)
  exp(mean(multi.params$beta[draws,4,2]) + size_dummy*mean(multi.params$beta[draws,5,2]))/Denominator_vac)
sum(pred_vac[1,])
# Previously tended by Crem
Denominator_crem <- exp(mean(multi.params$beta[draws,1,4]) + size_dummy*mean(multi.params$beta[draws,5,4])) + exp(mean(multi.params$beta[draws,1,1]) + size_dummy*mean(multi.params$beta[draws,5,1])) + 
  exp(mean(multi.params$beta[draws,1,2]) + size_dummy*mean(multi.params$beta[draws,5,2]))
pred_crem<-cbind(
  #pr(vacant)
  exp(mean(multi_out$beta.1.4) + size_dummy*mean(multi_out$beta.5.4))/Denominator_crem,
  #pr(crem)
  exp(mean(multi_out$beta.1.1) + size_dummy*mean(multi_out$beta.5.1))/Denominator_crem,
  #pr(liom)
  exp(mean(multi_out$beta.1.2) + size_dummy*mean(multi_out$beta.5.2))/Denominator_crem)
sum(pred_crem[1,])
# Previously tended by Liom
Denominator_liom <- exp(mean(multi.params$beta[draws,2,4]) + size_dummy*mean(multi.params$beta[draws,5,4])) + 
  exp(mean(multi.params$beta[draws,2,1]) + size_dummy*mean(multi.params$beta[draws,5,1])) + 
  exp(mean(multi.params$beta[draws,2,2]) + size_dummy*mean(multi.params$beta[draws,5,2]))
pred_liom<-cbind(
  #pr(vacant)
  exp(mean(multi.params$beta[draws,2,4]) + size_dummy*mean(multi.params$beta[draws,5,4]))/Denominator_liom,
  #pr(crem)
  exp(mean(multi.params$beta[draws,2,1]) + size_dummy*mean(multi.params$beta[draws,5,1]))/Denominator_liom,
  #pr(liom)
  exp(mean(multi.params$beta[draws,2,2]) + size_dummy*mean(multi.params$beta[draws,5,2]))/Denominator_liom)
sum(pred_liom[1,])
## Plot the probabilities 
png("Ant_3_LOV_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4, byrow = TRUE), heights = c(0.6,1,1,1), widths = c(5))
plot.new()
text(0.5,0.5,"Ant States \n Other, Vacant, and Liom.",cex=2,font=2)
plot(size_dummy, pred_vac[,1], type = "l", col = vaccol,main = "Previously Vacant", ylim = c(0,1))
lines(size_dummy, pred_vac[,2], col = cremcol)
lines(size_dummy, pred_vac[,3], col = liomcol)
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_liom,pch=16,cex=multi_plot_vac$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_vac,pch=16,cex=multi_plot_vac$N_mod,col= alpha(vaccol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_crem,pch=16,cex=multi_plot_vac$N_mod,col= alpha(cremcol, 0.4))
legend("topleft",c("vacant","crem.","liom."), fill = c(vaccol,cremcol,liomcol))
plot(size_dummy, pred_crem[,1], type = "l", col = vaccol,main = "Previously Crem", ylim = c(0,1))
lines(size_dummy, pred_crem[,2], col = cremcol)
lines(size_dummy, pred_crem[,3], col = liomcol)
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_liom,pch=16,cex=multi_plot_crem$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_vac,pch=16,cex=multi_plot_crem$N_mod,col= alpha(vaccol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_crem,pch=16,cex=multi_plot_crem$N_mod,col= alpha(cremcol, 0.4))
plot(size_dummy, pred_liom[,1], type = "l", col = vaccol,main = "Previously Liom", ylim = c(0,1))
lines(size_dummy, pred_liom[,2], col = cremcol)
lines(size_dummy, pred_liom[,3], col = liomcol)
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_liom,pch=16,cex=multi_plot_liom$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_vac,pch=16,cex=multi_plot_liom$N_mod,col= alpha(vaccol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_crem,pch=16,cex=multi_plot_liom$N_mod,col= alpha(cremcol, 0.4))
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
#### OTHER & CREM & VAC
## Calculate the probabilities of being tended by each ant species
# Previously tended by none
Denominator_vac <- exp(mean(multi.params$beta[draws,4,4]) + size_dummy*mean(multi.params$beta[draws,5,4])) +
  exp(mean(multi.params$beta[draws,4,3]) + size_dummy*mean(multi.params$beta[draws,5,3])) + 
  exp(mean(multi.params$beta[draws,4,1]) + size_dummy*mean(multi.params$beta[draws,5,1]))
pred_vac<-cbind(
  #pr(vacant)
  exp(mean(multi.params$beta[draws,4,4]) + size_dummy*mean(multi.params$beta[draws,5,4]))/Denominator_vac,
  #pr(other)
  exp(mean(multi.params$beta[draws,4,3]) + size_dummy*mean(multi.params$beta[draws,5,3]))/Denominator_vac,
  #pr(crem)
  exp(mean(multi.params$beta[draws,4,1]) + size_dummy*mean(multi.params$beta[draws,5,1]))/Denominator_vac)
sum(pred_vac[1,])
## Previously tended by Crem
Denominator_crem <- exp(mean(multi.params$beta[draws,1,4]) + size_dummy*mean(multi.params$beta[draws,5,4])) + exp(mean(multi.params$beta[draws,1,3]) + size_dummy*mean(multi.params$beta[draws,5,3])) + 
  exp(mean(multi.params$beta[draws,1,1]) + size_dummy*mean(multi.params$beta[draws,5,1]))
pred_crem<-cbind(
  #pr(vacant)
  exp((mean(multi.params$beta[draws,1,4])) + size_dummy*mean(multi.params$beta[draws,5,4]))/Denominator_crem,
  #pr(other)
  exp((mean(multi.params$beta[draws,1,3])) + size_dummy*mean(multi.params$beta[draws,5,3]))/Denominator_crem,
  #pr(crem)
  exp((mean(multi.params$beta[draws,1,1])) + size_dummy*mean(multi.params$beta[draws,5,1]))/Denominator_crem)
sum(pred_crem[1,])
## Previously tended by Crem
Denominator_other <- exp(mean(multi.params$beta[draws,1,4]) + size_dummy*mean(multi.params$beta[draws,5,4])) + exp(mean(multi.params$beta[draws,1,3]) + size_dummy*mean(multi.params$beta[draws,5,3])) + 
  exp(mean(multi.params$beta[draws,1,1]) + size_dummy*mean(multi.params$beta[draws,5,1]))
pred_other<-cbind(
  #pr(vacant)
  exp(mean(multi.params$beta[draws,1,4]) + size_dummy*mean(multi.params$beta[draws,5,4]))/Denominator_other,
  #pr(other)
  exp(mean(multi.params$beta[draws,1,3]) + size_dummy*mean(multi.params$beta[draws,5,3]))/Denominator_other,
  #pr(crem)
  exp(mean(multi.params$beta[draws,1,1]) + size_dummy*mean(multi.params$beta[draws,5,1]))/Denominator_other)
sum(pred_other[1,])
## Plot the probabilities
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("Ant_3_COV_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4, byrow = TRUE), heights = c(0.6,1,1,1), widths = c(5))
plot.new()
text(0.5,0.5,"Ant States \n Crem., Vacant, and Liom.",cex=2,font=2)
plot(size_dummy, pred_vac[,1], type = "l", col = vaccol,main = "Previously Vacant", ylim = c(0,1))
lines(size_dummy, pred_vac[,2], col = othercol)
lines(size_dummy, pred_vac[,3], col = cremcol)
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_other,pch=16,cex=multi_plot_vac$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_vac,pch=16,cex=multi_plot_vac$N_mod,col= alpha(vaccol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_crem,pch=16,cex=multi_plot_vac$N_mod,col= alpha(cremcol, 0.4))
legend("topleft",c("vacant","other","crem."), fill = c(vaccol,othercol,cremcol))
plot(size_dummy, pred_other[,1], type = "l", col = vaccol,main = "Previously Other", ylim = c(0,1))
lines(size_dummy, pred_other[,2], col = othercol)
lines(size_dummy, pred_other[,3], col = cremcol)
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_other,pch=16,cex=multi_plot_other$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_vac,pch=16,cex=multi_plot_other$N_mod,col= alpha(vaccol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_crem,pch=16,cex=multi_plot_other$N_mod,col= alpha(cremcol, 0.4))
plot(size_dummy, pred_crem[,1], type = "l", col = vaccol,main = "Previously Crem", ylim = c(0,1))
lines(size_dummy, pred_crem[,2], col = othercol)
lines(size_dummy, pred_crem[,3], col = cremcol)
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_other,pch=16,cex=multi_plot_crem$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_vac,pch=16,cex=multi_plot_crem$N_mod,col= alpha(vaccol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_crem,pch=16,cex=multi_plot_crem$N_mod,col= alpha(cremcol, 0.4))
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
#### 2 ANTS --------------------------------------------------------------------
#### CREM AND OTHER
## Calculate probabilities of next ant partner
# Previously tended by other
Denominator_other <- exp(mean(multi.params$beta[draws,3,1]) + size_dummy*mean(multi.params$beta[draws,5,1])) + exp(mean(multi.params$beta[draws,3,3]) + size_dummy*mean(multi.params$beta[draws,5,3]))
pred_other<-cbind(
  #pr(crem)
  exp((mean(multi.params$beta[draws,3,1])) + size_dummy*mean(multi.params$beta[draws,5,1]))/Denominator_other,
  #pr(other)
  exp((mean(multi.params$beta[draws,3,3])) + size_dummy*mean(multi.params$beta[draws,5,3]))/Denominator_other)
sum(pred_other[1,])
# Previously tended by Crem
Denominator_crem <- exp(mean(multi.params$beta[draws,1,1]) + size_dummy*mean(multi.params$beta[draws,5,1])) + exp(mean(multi.params$beta[draws,1,3]) + size_dummy*mean(multi.params$beta[draws,5,3]))
pred_crem<-cbind(
  #pr(crem)
  exp(mean(multi.params$beta[draws,1,1]) + size_dummy*mean(multi.params$beta[draws,5,1]))/Denominator_crem,
  #pr(other)
  exp(mean(multi.params$beta[draws,1,3]) + size_dummy*mean(multi.params$beta[draws,5,3]))/Denominator_crem)
sum(pred_crem[1,])
## Plot the probabilities of next ant partner based on previous ant and size -- includes real data and model fits
png("Ant_2_CO_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3),
              ncol = 1, nrow = 3, byrow = TRUE), heights = c(0.6,1.4,1.4), widths = c(3.9))
plot.new()
text(0.5,0.5,"Ant States",cex=2,font=2)
# Prev Crem
plot(size_dummy, pred_crem[,1], type = "l", col = cremcol,main = "Previously Crem", ylim = c(0,1))
lines(size_dummy, pred_crem[,2], col = othercol)
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_other,pch=16,cex=multi_plot_crem$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_crem,pch=16,cex=multi_plot_crem$N_mod,col= alpha(cremcol, 0.4))
# Prev Other
plot(size_dummy, pred_other[,1], type = "l", col = cremcol,main = "Previously Other", ylim = c(0,1))
lines(size_dummy, pred_other[,2], col = othercol)
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_other,pch=16,cex=multi_plot_other$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_crem,pch=16,cex=multi_plot_other$N_mod,col= alpha(cremcol, 0.4))
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
#### LIOM & VAC
## Calculate the probabilities of being tended by each ant species
# Previously tended by Liom
Denominator_liom <- exp(mean(multi.params$beta[draws,2,2]) + size_dummy*mean(multi.params$beta[draws,5,2])) + 
  exp(mean(multi.params$beta[draws,2,4]) + size_dummy*mean(multi.params$beta[draws,5,4]))
pred_liom<-cbind(
  #pr(liom)
  exp((mean(multi.params$beta[draws,2,2])) + size_dummy*mean(multi.params$beta[draws,5,2]))/Denominator_liom,
  #pr(vac)
  exp((mean(multi.params$beta[draws,2,4])) + size_dummy*mean(multi.params$beta[draws,5,4]))/Denominator_liom)
sum(pred_liom[1,])
## Previously tended by none
Denominator_vac <- exp(mean(multi.params$beta[draws,4,2]) + size_dummy*mean(multi.params$beta[draws,5,2])) + 
  exp(mean(multi.params$beta[draws,4,4]) + size_dummy*mean(multi.params$beta[draws,5,4]))
pred_vac<-cbind(
  #pr(liom)
  exp(mean(multi.params$beta[draws,4,2]) + size_dummy*mean(multi.params$beta[draws,5,2]))/Denominator_vac,
  #pr(vac)
  exp(mean(multi.params$beta[draws,4,4]) + size_dummy*mean(multi.params$beta[draws,5,4]))/Denominator_vac)
sum(pred_vac[1,])
## Plot the probabilities of the next ant partner based on previous partner and size -- Include real data and model fits
png("Ant_2_LV_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3),
              ncol = 1, nrow = 3, byrow = TRUE), heights = c(0.6,1.4,1.4), widths = c(3.9))
plot.new()
text(0.5,0.5,"Ant States",cex=2,font=2)
## Prev Liom
plot(size_dummy, pred_liom[,1], type = "l", col = liomcol,main = "Previously Liom", ylim = c(0,1))
lines(size_dummy, pred_liom[,2], col = vaccol)
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_liom,pch=16,cex=multi_plot_liom$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_vac,pch=16,cex=multi_plot_liom$N_mod,col= alpha(vaccol, 0.4))
## Prev Vac
plot(size_dummy, pred_vac[,1], type = "l", col = liomcol,main = "Previously Vacant", ylim = c(0,1))
lines(size_dummy, pred_vac[,2], col = vaccol)
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_liom,pch=16,cex=multi_plot_vac$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_vac,pch=16,cex=multi_plot_vac$N_mod,col= alpha(vaccol, 0.4))
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
#### LIOM & CREM
## Calculate the probabilities of being tended by each ant species
# Previously tended by none
Denominator_liom <- exp(mean(multi.params$beta[draws,2,2]) + size_dummy*mean(multi.params$beta[draws,5,2])) + 
  exp(mean(multi.params$beta[draws,2,1]) + size_dummy*mean(multi.params$beta[draws,5,1]))
pred_liom<-cbind(
  #pr(liom)
  exp((mean(multi.params$beta[draws,2,2])) + size_dummy*mean(multi.params$beta[draws,5,2]))/Denominator_liom,
  #pr(crem)
  exp((mean(multi.params$beta[draws,2,1])) + size_dummy*mean(multi.params$beta[draws,5,1]))/Denominator_liom)
sum(pred_liom[1,])
# Previously tended by none
Denominator_crem <- exp(mean(multi.params$beta[draws,1,2]) + size_dummy*mean(multi.params$beta[draws,5,2])) + 
  exp(mean(multi.params$beta[draws,1,1]) + size_dummy*mean(multi.params$beta[draws,5,1]))
pred_crem<-cbind(
  #pr(liom)
  exp(mean(multi.params$beta[draws,1,2]) + size_dummy*mean(multi.params$beta[draws,5,2]))/Denominator_crem,
  #pr(crem)
  exp(mean(multi.params$beta[draws,1,1]) + size_dummy*mean(multi.params$beta[draws,5,1]))/Denominator_crem)
sum(pred_crem[1,])
## Plot the probabilities
png("Ant_2_LC_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3),
              ncol = 1, nrow = 3, byrow = TRUE), heights = c(0.6,1.4,1.4), widths = c(3.9))
plot.new()
text(0.5,0.5,"Ant States",cex=2,font=2)
## Prev Liom
plot(size_dummy, pred_liom[,1], type = "l", col = liomcol,main = "Previously Liom", ylim = c(0,1))
lines(size_dummy, pred_liom[,2], col = cremcol)
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_liom,pch=16,cex=multi_plot_liom$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_crem,pch=16,cex=multi_plot_liom$N_mod,col= alpha(cremcol, 0.4))
## Prev Vac
plot(size_dummy, pred_crem[,1], type = "l", col = liomcol,main = "Previously Crem", ylim = c(0,1))
lines(size_dummy, pred_crem[,2], col = cremcol)
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_liom,pch=16,cex=multi_plot_crem$N_mod,col= alpha(liomcol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_crem,pch=16,cex=multi_plot_crem$N_mod,col= alpha(cremcol, 0.4))
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
#### OTHER & VAC
## Calculate the probabilities of being tended by each ant species
# Previously tended by none
Denominator_other <- exp(mean(multi.params$beta[draws,3,3]) + size_dummy*mean(multi.params$beta[draws,5,3])) + 
  exp(mean(multi.params$beta[draws,3,4]) + size_dummy*mean(multi.params$beta[draws,5,4]))
pred_other<-cbind(
  #pr(other)
  exp((mean(multi.params$beta[draws,3,3])) + size_dummy*mean(multi.params$beta[draws,5,3]))/Denominator_other,
  #pr(vac)
  exp((mean(multi.params$beta[draws,3,4])) + size_dummy*mean(multi.params$beta[draws,5,4]))/Denominator_other)
sum(pred_other[1,])
# Previously tended by none
Denominator_vac <- exp(mean(multi.params$beta[draws,4,3]) + size_dummy*mean(multi.params$beta[draws,5,3])) + 
  exp(mean(multi.params$beta[draws,4,4]) + size_dummy*mean(multi.params$beta[draws,5,4]))
pred_vac<-cbind(
  #pr(other)
  exp(mean(multi.params$beta[draws,4,3]) + size_dummy*mean(multi.params$beta[draws,5,3]))/Denominator_vac,
  #pr(vac)
  exp(mean(multi.params$beta[draws,4,4]) + size_dummy*mean(multi.params$beta[draws,5,4]))/Denominator_vac)
sum(pred_vac[1,])
## Plot the probabilities of next ant partners based on previous partners and size -- include real data and model fits
png("Ant_2_OV_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3),
              ncol = 1, nrow = 3, byrow = TRUE), heights = c(0.6,1.4,1.4), widths = c(3.9))
plot.new()
text(0.5,0.5,"Ant States",cex=2,font=2)
## Prev other
plot(size_dummy, pred_other[,1], type = "l", col = othercol,main = "Previously Other", ylim = c(0,1))
lines(size_dummy, pred_other[,2], col = vaccol)
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_other,pch=16,cex=multi_plot_other$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_vac,pch=16,cex=multi_plot_other$N_mod,col= alpha(vaccol, 0.4))
## Prev Vac
plot(size_dummy, pred_vac[,1], type = "l", col = othercol,main = "Previously Vacant", ylim = c(0,1))
lines(size_dummy, pred_vac[,2], col = vaccol)
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_other,pch=16,cex=multi_plot_vac$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_vac,pch=16,cex=multi_plot_vac$N_mod,col= alpha(vaccol, 0.4))
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
#### OTHER & LIOM
## Calculate the probabilities of being tended by each ant species
# Previously tended by none
Denominator_other <- exp(mean(multi.params$beta[,2,2]) + size_dummy*mean(multi.params$beta[,5,2])) + 
  exp(mean(multi.params$beta[,2,4]) + size_dummy*mean(multi.params$beta[,5,4]))
pred_other<-cbind(
  #pr(other)
  exp((mean(multi.params$beta[,2,2])) + size_dummy*mean(multi.params$beta[,5,2]))/Denominator_other,
  #pr(liom)
  exp((mean(multi.params$beta[,2,4])) + size_dummy*mean(multi.params$beta[,5,4]))/Denominator_other)
sum(pred_other[1,])
## Previously tended by Liom
Denominator_liom <- exp(mean(multi.params$beta[,4,2]) + size_dummy*mean(multi.params$beta[,5,2])) + 
  exp(mean(multi.params$beta[,4,4]) + size_dummy*mean(multi.params$beta[,5,4]))
pred_liom<-cbind(
  #pr(other)
  exp(mean(multi.params$beta[,4,2]) + size_dummy*mean(multi.params$beta[,5,2]))/Denominator_liom,
  #pr(liom)
  exp(mean(multi.params$beta[,4,4]) + size_dummy*mean(multi.params$beta[,5,4]))/Denominator_liom)
sum(pred_liom[1,])
## Plot the probabilities
png("Ant_2_OL_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3),
              ncol = 1, nrow = 3, byrow = TRUE), heights = c(0.6,1.4,1.4), widths = c(3.9))
plot.new()
text(0.5,0.5,"Ant States",cex=2,font=2)
## Prev other
plot(size_dummy, pred_other[,1], type = "l", col = othercol,main = "Previously Other", ylim = c(0,1))
lines(size_dummy, pred_other[,2], col = liomcol)
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_other,pch=16,cex=multi_plot_other$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_other$mean_size,multi_plot_other$ant_t1_liom,pch=16,cex=multi_plot_other$N_mod,col= alpha(liomcol, 0.4))
## Prev Vac
plot(size_dummy, pred_liom[,1], type = "l", col = othercol,main = "Previously Liom", ylim = c(0,1))
lines(size_dummy, pred_liom[,2], col = liomcol)
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_other,pch=16,cex=multi_plot_liom$N_mod,col= alpha(othercol, 0.4))
points(multi_plot_liom$mean_size,multi_plot_liom$ant_t1_liom,pch=16,cex=multi_plot_liom$N_mod,col= alpha(liomcol, 0.4))
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()
#### VAC & CREM
## Calculate the probabilities of being tended by each ant species
# Previously tended by none
Denominator_vac <- exp(mean(multi.params$beta[,1,1]) + size_dummy*mean(multi.params$beta[,5,1])) + 
  exp(mean(multi.params$beta[,1,3]) + size_dummy*mean(multi.params$beta[,5,3]))
pred_vac<-cbind(
  #pr(vac)
  exp((mean(multi.params$beta[,1,1])) + size_dummy*mean(multi.params$beta[,5,1]))/Denominator_vac,
  #pr(crem)
  exp((mean(multi.params$beta[,1,3])) + size_dummy*mean(multi.params$beta[,5,3]))/Denominator_vac)
sum(pred_vac[1,])
## Previously tended by Crem
Denominator_crem <- exp(mean(multi.params$beta[,3,1]) + size_dummy*mean(multi.params$beta[,5,1])) + 
  exp(mean(multi.params$beta[,3,3]) + size_dummy*mean(multi.params$beta[,5,3]))
pred_liom<-cbind(
  #pr(vac)
  exp(mean(multi.params$beta[,3,1]) + size_dummy*mean(multi.params$beta[,5,1]))/Denominator_crem,
  #pr(crem)
  exp(mean(multi.params$beta[,3,3]) + size_dummy*mean(multi.params$beta[,5,3]))/Denominator_crem)
sum(pred_crem[1,])
## Plot the probabilities
png("Ant_2_CV_Multi.png")
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3),
              ncol = 1, nrow = 3, byrow = TRUE), heights = c(0.6,1.4,1.4), widths = c(3.9))
plot.new()
text(0.5,0.5,"Ant States",cex=2,font=2)
## Prev vac
plot(size_dummy, pred_vac[,1], type = "l", col = vaccol,main = "Previously Vac", ylim = c(0,1))
lines(size_dummy, pred_vac[,2], col = cremcol)
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_vac,pch=16,cex=multi_plot_vac$N_mod,col= alpha(vaccol, 0.4))
points(multi_plot_vac$mean_size,multi_plot_vac$ant_t1_crem,pch=16,cex=multi_plot_vac$N_mod,col= alpha(cremcol, 0.4))
## Prev crem
plot(size_dummy, pred_crem[,1], type = "l", col = vaccol,main = "Previously Crem", ylim = c(0,1))
lines(size_dummy, pred_crem[,2], col = cremcol)
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_vac,pch=16,cex=multi_plot_crem$N_mod,col= alpha(vaccol, 0.4))
points(multi_plot_crem$mean_size,multi_plot_crem$ant_t1_crem,pch=16,cex=multi_plot_crem$N_mod,col= alpha(cremcol, 0.4))
mtext("Log(Volume) year t",side=1,line=0,outer=TRUE,cex=1.1)
mtext("Probability of Next Ant Partner",side=2,line=0,outer=TRUE,cex=1.1,las=0)
dev.off()

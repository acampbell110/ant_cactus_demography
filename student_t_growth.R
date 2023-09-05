#######################################################################################################
##
##                  The purpose of this file is to run each vital rate sub model separately,
##                          save the outputs, and check the posterior distributions 
##                  TM: the header here should make it clear which scripts need to be run before
##                      running this one (for example, there are packages needed here but none are loaded)
#######################################################################################################
library(rstan)
library(bayesplot)
library(quantreg)
library(qgam)
library(stevemisc)
## quantile-based moments
Q.mean<-function(q.25,q.50,q.75){(q.25+q.50+q.75)/3}
Q.sd<-function(q.25,q.75){(q.75-q.25)/1.35}
Q.skewness<-function(q.10,q.50,q.90){(q.10 + q.90 - 2*q.50)/(q.90 - q.10)}
Q.kurtosis<-function(q.05,q.25,q.75,q.95){
  qN = qnorm(c(0.05,0.25,0.75,0.95))
  KG = (qN[4]-qN[1])/(qN[3]-qN[2])
  return(((q.95-q.05)/(q.75-q.25))/KG - 1)
}

## First read the data in 
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
#setwd("C:/Users/tm9/Dropbox/github/ant_cactus_demography")
#setwd("/Users/Labuser/Documents/GitHub/ant_cactus_demography")
cactus <- read.csv("cholla_demography_20042023_cleaned.csv", header = TRUE,stringsAsFactors=T)
## re-assign the seedling plots ("HT1B1" etc) to transects 1-3
levels(cactus$Plot)<-c(levels(cactus$Plot),"T4")
cactus$Plot[cactus$Plot=="HT1B1"]<-"T1"
cactus$Plot[cactus$Plot=="HT2B3"]<-"T2"
cactus$Plot[cactus$Plot=="HT3B1" | cactus$Plot=="HT3B2" | cactus$Plot=="HT3B3"]<-"T3"
cactus$Plot[cactus$Plot=="HT4B1" | cactus$Plot=="HT4B2"]<-"T4"

##############################################################################################
##
##   Skew Growth Model -- What size will the cacti be next time step?
##
##############################################################################################
## Pull all necessary variables together, remove NAs, and put them into a list so they are
## ready to feed into the stan model
growth_data_orig <- cactus[,c("Plot","Year_t","logsize_t","logsize_t1","ant_t")]
growth_data <- na.omit(growth_data_orig)


## Lose 2032 rows (due to plant death & recruit status)
nrow(growth_data_orig)
nrow(growth_data)
# check that you are happy with the subsetting by plotting the original and cleaned data
plot(growth_data$logsize_t, growth_data$logsize_t1)
points((cactus$logsize_t), (cactus$logsize_t1), col = "red")
## Make a list of all necessary variables so they are properly formatted to feed into the stan model
stan_data_grow_skew <- list(N = nrow(growth_data),                                ## number of observations
                            vol = (growth_data$logsize_t), ## predictor volume year t
                            vol2 = (growth_data$logsize_t)^2,
                            y = (growth_data$logsize_t1),                              ## response volume next year
                            ant = as.integer(as.factor(growth_data$ant_t)),            ## predictor ant state
                            K = 4,                                                     ## number of ant states
                            N_Year = max(as.integer(as.factor(growth_data$Year_t))),   ## number of years
                            N_Plot = max(as.integer(growth_data$Plot)),     ## number of plots
                            plot = as.integer(growth_data$Plot),            ## predictor plots
                            year = as.integer(as.factor(growth_data$Year_t))           ## predictor years
)
########## growth model with a skew normal distribution -- fixed effects: previous size and ant state, ##############
########## random effects: plot and year, size variation is included for both the omega and alpha estimates #########
grow_st_model <- stan_model("Data Analysis/STAN Models/grow_student_t.stan")
fit_grow_st<-sampling(grow_st_model,data = stan_data_grow_skew,chains=3,
                        control = list(adapt_delta=0.99,stepsize=0.1),
                        iter=10000,cores=3,thin=2,
                        pars = c("u","w",          # plot and year random effects
                                  "beta0","beta1","beta2", #location coefficients
                                 "d_0","d_size", #scale coefficiences
                                 "a_0","a_size"), #shape coefficients
                        save_warmup=F)
saveRDS(fit_grow_st, "C:/Users/tm9/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_grow_student_t.rds")
#for control parameters see:
#https://github.com/stan-dev/stan/issues/1504#issuecomment-114685444
#https://mc-stan.org/rstanarm/reference/adapt_delta.html
fit_grow_skew<-readRDS("C:/Users/tm9/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/fit_grow_skew.rds")

mcmc_trace(fit_grow_st,pars=c("d_0","d_size","a_0","a_size"))
mcmc_trace(fit_grow_st,pars=c("beta0[1]","beta0[2]","beta0[3]","beta0[4]"))
mcmc_trace(fit_grow_st,pars=c("beta1[1]","beta1[2]","beta1[3]","beta1[4]"))
mcmc_trace(fit_grow_st,pars=c("beta2[1]","beta2[2]","beta2[3]","beta2[4]"))
## happy with convergence

## real data moments
q.fit<-matrix(NA,7,length(stan_data_grow_skew$vol))
q.fit[1,]<-predict(qgam(y~s(vol),qu=0.05,data=data.frame(y=stan_data_grow_skew$y,vol=stan_data_grow_skew$vol)))
q.fit[2,]<-predict(qgam(y~s(vol),qu=0.10,data=data.frame(y=stan_data_grow_skew$y,vol=stan_data_grow_skew$vol)))
q.fit[3,]<-predict(qgam(y~s(vol),qu=0.25,data=data.frame(y=stan_data_grow_skew$y,vol=stan_data_grow_skew$vol)))
q.fit[4,]<-predict(qgam(y~s(vol),qu=0.5,data=data.frame(y=stan_data_grow_skew$y,vol=stan_data_grow_skew$vol)))
q.fit[5,]<-predict(qgam(y~s(vol),qu=0.75,data=data.frame(y=stan_data_grow_skew$y,vol=stan_data_grow_skew$vol)))
q.fit[6,]<-predict(qgam(y~s(vol),qu=0.90,data=data.frame(y=stan_data_grow_skew$y,vol=stan_data_grow_skew$vol)))
q.fit[7,]<-predict(qgam(y~s(vol),qu=0.95,data=data.frame(y=stan_data_grow_skew$y,vol=stan_data_grow_skew$vol)))

obs_mean<-Q.mean(q.fit[3,],q.fit[4,],q.fit[5,])
obs_sd<-Q.sd(q.fit[3,],q.fit[5,])
obs_skew<-Q.skewness(q.fit[2,],q.fit[4,],q.fit[6,])
obs_kurt<-Q.kurtosis(q.fit[1,],q.fit[3,],q.fit[5,],q.fit[7,])

plot(stan_data_grow_skew$vol,stan_data_grow_skew$y,pch=".",col="red")
points(stan_data_grow_skew$vol,q.fit[1,],col="black",pch=".")
points(stan_data_grow_skew$vol,q.fit[2,],col="black",pch=".")
points(stan_data_grow_skew$vol,q.fit[3,],col="black",pch=".")
points(stan_data_grow_skew$vol,q.fit[4,],col="black",pch=".")
points(stan_data_grow_skew$vol,q.fit[5,],col="black",pch=".")
points(stan_data_grow_skew$vol,q.fit[6,],col="black",pch=".")
points(stan_data_grow_skew$vol,q.fit[7,],col="black",pch=".")

## simulate data 
## pull params
grow_params <- rstan::extract(fit_grow_st,permuted=FALSE)
hist(grow_params[,3,"w[2,5]"])

## one simulated dataset
n_draws<-25
draws=sample.int(n=dim(grow_params)[1],size=n_draws,replace = F)
grow_sim<-matrix(NA,n_draws,stan_data_grow_skew$N)
sim_mean<-sim_sd<-sim_skew<-sim_kurt<-matrix(NA,n_draws,stan_data_grow_skew$N)
for(i in 1:n_draws){
for(n in 1:stan_data_grow_skew$N){
grow_sim[i,n]<-rst(n=1,
                   df=exp(grow_params[draws[i],3,"a_0"] + grow_params[draws[i],3,"a_size"] * stan_data_grow_skew$vol[n]),
                   mu=grow_params[draws[i],3,paste0("beta0[",stan_data_grow_skew$ant[n],"]")]+
                      grow_params[draws[i],3,paste0("beta1[",stan_data_grow_skew$ant[n],"]")]*stan_data_grow_skew$vol[n]+
                      grow_params[draws[i],3,paste0("beta2[",stan_data_grow_skew$ant[n],"]")]*stan_data_grow_skew$vol2[n]+
                      grow_params[draws[i],3,paste0("u[",stan_data_grow_skew$plot[n],"]")]+
                      grow_params[draws[i],3,paste0("w[",stan_data_grow_skew$ant[n],",",stan_data_grow_skew$year[n],"]")],
                  sigma=exp(grow_params[draws[i],3,"d_0"] + grow_params[draws[i],3,"d_size"] * stan_data_grow_skew$vol[n]))
}
  q.fit[1,]<-predict(qgam(y~s(vol),qu=0.05,data=data.frame(y=grow_sim[i,],vol=stan_data_grow_skew$vol)))
  q.fit[2,]<-predict(qgam(y~s(vol),qu=0.10,data=data.frame(y=grow_sim[i,],vol=stan_data_grow_skew$vol)))
  q.fit[3,]<-predict(qgam(y~s(vol),qu=0.25,data=data.frame(y=grow_sim[i,],vol=stan_data_grow_skew$vol)))
  q.fit[4,]<-predict(qgam(y~s(vol),qu=0.5,data=data.frame(y=grow_sim[i,],vol=stan_data_grow_skew$vol)))
  q.fit[5,]<-predict(qgam(y~s(vol),qu=0.75,data=data.frame(y=grow_sim[i,],vol=stan_data_grow_skew$vol)))
  q.fit[6,]<-predict(qgam(y~s(vol),qu=0.90,data=data.frame(y=grow_sim[i,],vol=stan_data_grow_skew$vol)))
  q.fit[7,]<-predict(qgam(y~s(vol),qu=0.95,data=data.frame(y=grow_sim[i,],vol=stan_data_grow_skew$vol)))

  sim_mean[i,]<-Q.mean(q.fit[3,],q.fit[4,],q.fit[5,]) 
  sim_sd[i,]<-Q.sd(q.fit[3,],q.fit[5,])  
  sim_skew[i,]<-Q.skewness(q.fit[2,],q.fit[4,],q.fit[6,])
  sim_kurt[i,]<-Q.kurtosis(q.fit[1,],q.fit[3,],q.fit[5,],q.fit[7,])

print(i/n_draws)  
}

plot(stan_data_grow_skew$vol,grow_sim[1,],pch=".",col="red")
points(stan_data_grow_skew$vol,stan_data_grow_skew$y,pch=".",col="black")


bayesplot::ppc_dens_overlay(stan_data_grow_skew$y, grow_sim)

matplot(stan_data_grow_skew$vol,t(sim_mean),pch=".",col="gray")
points(stan_data_grow_skew$vol,obs_mean)

matplot(stan_data_grow_skew$vol,t(sim_sd),pch=".",col="gray")
points(stan_data_grow_skew$vol,obs_sd)

matplot(stan_data_grow_skew$vol,t(sim_skew),pch=".",col="gray")
points(stan_data_grow_skew$vol,obs_skew)

matplot(stan_data_grow_skew$vol,t(sim_kurt),pch=".",col="gray")
points(stan_data_grow_skew$vol,obs_kurt)




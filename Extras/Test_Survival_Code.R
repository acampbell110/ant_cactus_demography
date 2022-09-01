##########################################
## Check the means of different variables against each other
## Load the outputs for the visuals
surv_out <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/surv_outputs.csv", header = TRUE,stringsAsFactors=T)
## Load the outputs for the parameters
mcmc_dir <- "/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/"
surv.params <- read.csv(paste0(mcmc_dir,"surv_outputs.csv"), header = TRUE,stringsAsFactors=T)    
#### These go to the same location and pull the same file
##########################################
## Now check the means of all of the different important columns
## Vacant
c(mean(surv_out$beta0.1),mean(surv.params$beta0.1))
c(mean(surv_out$beta1.1),mean(surv.params$beta1.1))
## Other
c(mean(surv_out$beta0.2),mean(surv.params$beta0.2))
c(mean(surv_out$beta1.2),mean(surv.params$beta1.2))
## Crem
c(mean(surv_out$beta0.3),mean(surv.params$beta0.3))
c(mean(surv_out$beta1.3),mean(surv.params$beta1.3))
## Liom
c(mean(surv_out$beta0.4),mean(surv.params$beta0.4))
c(mean(surv_out$beta1.4),mean(surv.params$beta1.4))
#### These all match.
##########################################
## Now pull the random sample from the params ones and check that it still matches
Ndraws<-min(100,nrow(grow.params))
draws<-sample(nrow(grow.params),Ndraws)
surv.params<-surv.params[draws,]
surv_out<-surv_out[draws,]
## Vacant
c(mean(surv_out$beta0.1),mean(surv.params$beta0.1))
c(mean(surv_out$beta1.1),mean(surv.params$beta1.1))
## Other
c(mean(surv_out$beta0.2),mean(surv.params$beta0.2))
c(mean(surv_out$beta1.2),mean(surv.params$beta1.2))
## Crem
c(mean(surv_out$beta0.3),mean(surv.params$beta0.3))
c(mean(surv_out$beta1.3),mean(surv.params$beta1.3))
## Liom
c(mean(surv_out$beta0.4),mean(surv.params$beta0.4))
c(mean(surv_out$beta1.4),mean(surv.params$beta1.4))
#### These all match.
##########################################
## Now store the surv params under a different name (in the params dataframe) and 
## check that they still match
params <- data.frame(matrix(NA,nrow=Ndraws,ncol=1))
params<-params[,-1]
## Vacant
params$surv_beta01<-surv.params$beta0.1     	  ## surv intercept
params$surv_beta11<-surv.params$beta1.1				## surv slope
## Other
params$surv_beta02<-surv.params$beta0.2     	  ## surv intercept
params$surv_beta12<-surv.params$beta1.2				## surv slope
## Crem
params$surv_beta03<-surv.params$beta0.3     	  ## surv intercept
params$surv_beta13<-surv.params$beta1.3				## surv slope
## Liom
params$surv_beta04<-surv.params$beta0.4     	  ## surv intercept
params$surv_beta14<-surv.params$beta1.4				## surv slope
## Vacant
c(mean(surv_out$beta0.1),mean(params$surv_beta01))
c(mean(surv_out$beta1.1),mean(params$surv_beta11))
## Other
c(mean(surv_out$beta0.2),mean(params$surv_beta02))
c(mean(surv_out$beta1.2),mean(params$surv_beta12))
## Crem
c(mean(surv_out$beta0.3),mean(params$surv_beta03))
c(mean(surv_out$beta1.3),mean(params$surv_beta13))
## Liom
c(mean(surv_out$beta0.4),mean(params$surv_beta04))
c(mean(surv_out$beta1.4),mean(params$surv_beta14))
#### These all match.
############################################
## Now check the interquartile ranges of these and be sure that they match
## Vacant
quantile(surv_out$beta0.1)
quantile(params$surv_beta01)
quantile(surv_out$beta1.1)
quantile(params$surv_beta11)
## Other
quantile(surv_out$beta0.2)
quantile(params$surv_beta02)
quantile(surv_out$beta1.2)
quantile(params$surv_beta12)
## Crem
quantile(surv_out$beta0.3)
quantile(params$surv_beta03)
quantile(surv_out$beta1.3)
quantile(params$surv_beta13)
## Liom
quantile(surv_out$beta0.4)
quantile(params$surv_beta04)
quantile(surv_out$beta1.4)
quantile(params$surv_beta14)
## These all match.
#######################################
## Now make the equations for each of the lines
## Params Code
s_crem = invlogit(mean(params$surv_beta03) + mean(params$surv_beta13)*size_dummy)
s_vac = invlogit(mean(params$surv_beta01) + mean(params$surv_beta11)*size_dummy)
s_other = invlogit(mean(params$surv_beta02) + mean(params$surv_beta12)*size_dummy)
s_liom = invlogit(mean(params$surv_beta04) + mean(params$surv_beta14)*size_dummy)
## Figure Code
crem_extr = invlogit(mean(surv_out$beta0.3) + size_dummy*mean(surv_out$beta1.3))
vac_extr = invlogit(mean(surv_out$beta0.1) + size_dummy*mean(surv_out$beta1.1))
other_extr = invlogit(mean(surv_out$beta0.2) + size_dummy*mean(surv_out$beta1.2))
liom_extr = invlogit(mean(surv_out$beta0.4) + size_dummy*mean(surv_out$beta1.4))
## Plot these to compare
## Params Plot
par(mar=c(2,2,1,1),oma=c(2,2,0,0))
layout(matrix(c(1,2),
              ncol = 2, byrow = TRUE), heights = 1, widths = c(2,2))
plot(size_dummy,s_crem,col = cremcol,type = "l")
lines(size_dummy,s_vac,col = vaccol)
lines(size_dummy,s_other,col = othercol)
lines(size_dummy,s_liom,col = liomcol)
## Figure Plot
plot(size_dummy,crem_extr,col = cremcol,type = "l")
lines(size_dummy,vac_extr,col = vaccol)
lines(size_dummy,other_extr,col = othercol)
lines(size_dummy,liom_extr,col = liomcol)
## These plots match
#######################################
## Spot check values along those lines
size_dummy = 15
## Params Code
s_crem = invlogit(mean(params$surv_beta03) + mean(params$surv_beta13)*size_dummy)
s_vac = invlogit(mean(params$surv_beta01) + mean(params$surv_beta11)*size_dummy)
s_other = invlogit(mean(params$surv_beta02) + mean(params$surv_beta12)*size_dummy)
s_liom = invlogit(mean(params$surv_beta04) + mean(params$surv_beta14)*size_dummy)
## Figure Code
crem_extr = invlogit(mean(surv_out$beta0.3) + size_dummy*mean(surv_out$beta1.3))
vac_extr = invlogit(mean(surv_out$beta0.1) + size_dummy*mean(surv_out$beta1.1))
other_extr = invlogit(mean(surv_out$beta0.2) + size_dummy*mean(surv_out$beta1.2))
liom_extr = invlogit(mean(surv_out$beta0.4) + size_dummy*mean(surv_out$beta1.4))
## Compare
c(s_crem,crem_extr)
c(s_vac,vac_extr)
c(s_other,other_extr)
c(s_liom,liom_extr)
## x = -5, they match, vac>crem>other>liom, matches figures
## x = -3, they match, vac>crem>other>liom, matches figures
## x = -1, they match, crem>vac>other>liom, matches figures
## x = 1, they match, crem>vac>other>liom, matches figures
## x = 3, they match, crem>liom>other>vac, matches figures
## x = 5, they match, crem>liom>other>vac, matches figures
## x = 7, they match, crem>liom>other>vac, matches figures
## x = 10, they match, liom>crem>other>vac, matches figures
## x = 15, they match, liom>crem>other>vac, matches figures
#######################################
## Now check from the survival function compared to the figure equations
## Params Code
sx<-function(x,i,params){
  #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  xb=pmin(pmax(x,cholla_min),cholla_max)
  #Transform the ant specific parameters to the probability of survival
  s_crem = invlogit(mean(params$surv_beta03) + mean(params$surv_beta13)*xb)
  s_vac = invlogit(mean(params$surv_beta01) + mean(params$surv_beta11)*xb)
  s_other = invlogit(mean(params$surv_beta02) + mean(params$surv_beta12)*xb)
  s_liom = invlogit(mean(params$surv_beta04) + mean(params$surv_beta14)*xb)
  #Return the survival probabilities
  if(i == "crem"){ return(s_crem)}
  if(i == "liom"){ return(s_liom)}
  if(i == "other"){ return(s_other)}
  if(i == "vacant"){ return(s_vac)}
}
size_dummy = -3
## Figure Code
crem_extr = invlogit(mean(surv_out$beta0.3) + size_dummy*mean(surv_out$beta1.3))
vac_extr = invlogit(mean(surv_out$beta0.1) + size_dummy*mean(surv_out$beta1.1))
other_extr = invlogit(mean(surv_out$beta0.2) + size_dummy*mean(surv_out$beta1.2))
liom_extr = invlogit(mean(surv_out$beta0.4) + size_dummy*mean(surv_out$beta1.4))
c(sx(size_dummy,"crem",params), crem_extr) 
c(sx(size_dummy,"vacant",params),vac_extr)
c(sx(size_dummy,"other",params),other_extr)
c(sx(size_dummy,"liom",params),liom_extr)
## x = -5, slight difference because this may be beyond the cutoff -- caught by the extensions
## x = -3, they match
## x = -1, 
## x = 1, 
## x = 3, 
## x = 5, 
## x = 7, 
## x = 10, 
## x = 15, 
#### This is the stage where things are getting all mixed up!






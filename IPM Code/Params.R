## -------- set working directory ---------------------- ##

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code")



cholla_min<- min((cactus$logsize_t), na.rm = TRUE)  ## minsize 
cholla_max<- max((cactus$logsize_t), na.rm = TRUE)  ## maxsize 

Nplots <- length(unique(cactus$Plot))
Nyears <- length(unique(cactus$Year_t))
iter <- 1000
matsize<-200
floor.extend=0.9*cholla_min
ceiling.extend=1.1*cholla_max
lower<- cholla_min - floor.extend
upper<- cholla_max + ceiling.extend

n<-matsize
L<-lower; U<-upper
h<-(U-L)/n                   #Bin size
b<-L+c(0:n)*h;               #Lower boundaries of bins 
y<-0.5*(b[1:n]+b[2:(n+1)]);  #Bin midpoints


## -------- read in MCMC output ---------------------- ##

##These files contain all draws from the posterior distributions of all parameters
grow.params <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/grow_outputs.csv", header = TRUE,stringsAsFactors=T)    
surv.params <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/surv_outputs.csv", header = TRUE,stringsAsFactors=T)    
flow.params <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/flow_trunc_outputs.csv", header = TRUE,stringsAsFactors=T)    
viab.params <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/viab_outputs.csv", header = TRUE,stringsAsFactors=T)    
repro.params <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/repro_outputs.csv", header = TRUE,stringsAsFactors=T)    
seed.params <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/seed_outputs.csv", header = TRUE,stringsAsFactors=T)    
pre.seed.params <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/seed_surv_outputs.csv", header = TRUE,stringsAsFactors=T)    
germ1.params <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/germ1_outputs.csv", header = TRUE,stringsAsFactors=T)    
germ2.params <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/germ2_outputs.csv", header = TRUE,stringsAsFactors=T)    
rec.params <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/rec_outputs.csv", header = TRUE,stringsAsFactors=T)    
Ndraws<-min(100,nrow(grow.params))

## Pull the random draws from all posterior distributions
grow.params<-grow.params[1:Ndraws,]
surv.params<-surv.params[1:Ndraws,]
flow.params<-flow.params[1:Ndraws,]
viab.params<-viab.params[1:Ndraws,]
repro.params<-repro.params[1:Ndraws,]
seed.params<-seed.params[1:Ndraws,]
pre.seed.params<-pre.seed.params[1:Ndraws,]
germ1.params<-germ1.params[1:Ndraws,]
germ2.params<-germ2.params[1:Ndraws,]
rec.params<-rec.params[1:Ndraws,]

##This file contains random draws from the posterior distributions of the transition models 
multi.params <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/multi_outputs.csv", header = TRUE,stringsAsFactors=T)
## Pull the random draws from all posterior distributions
multi.params<-multi.params[1:Ndraws,]

## 'cholla' is a matrix where rows are vital rate coefficients and columns are posterior draws
## below, we will loop over columns, sending each set of coefficients into the stochastic IPM
params <- data.frame(matrix(NA,nrow=Ndraws,ncol=1))
params<-params[,-1]
##----------------------Growth Parameters----------------## 
## Check the names of the parameters
#head(grow.params)
#### No specific ant
params$grow_sig<-grow.params$sigma			    ## growth error
params$grow_sig_u<-grow.params$sigma_u
params$grow_sig_w<-grow.params$sigma_w
####Ant 1 (crem)
params$grow_beta01<-grow.params$beta0.1     	  ## growth intercept
params$grow_beta11<-grow.params$beta1.1				## growth slope
####Ant 2 (liom)
params$grow_beta02<-grow.params$beta0.2     	  ## growth intercept
params$grow_beta12<-grow.params$beta1.2				## growth slope
####Ant 3 (Other)
params$grow_beta03<-grow.params$beta0.3     	  ## growth intercept
params$grow_beta13<-grow.params$beta1.3				## growth slope
####Ant 4 (Vacant)
params$grow_beta04<-grow.params$beta0.4     	## growth intercept
params$grow_beta14<-grow.params$beta1.4				## growth slope

##-----------------------Survival Parameters-----------------## 
## Check the names of the parameters
#head(surv.params)
params$surv_sig<-surv.params$sigma
params$surv_sig_u<-surv.params$sigma_u
params$surv_sig_w<-surv.params$sigma_w
####Ant 1 (crem)
params$surv_beta01<-surv.params$beta0.1     	  ## surv intercept
params$surv_beta11<-surv.params$beta1.1				## surv slope
####Ant 2 (liom)
params$surv_beta02<-surv.params$beta0.2     	  ## surv intercept
params$surv_beta12<-surv.params$beta1.2				## surv slope
####Ant 3 (Other)
params$surv_beta03<-surv.params$beta0.3     	  ## surv intercept
params$surv_beta13<-surv.params$beta1.3				## surv slope
####Ant 4 (Vacant)
params$surv_beta04<-surv.params$beta0.4     	  ## surv intercept
params$surv_beta14<-surv.params$beta1.4				## surv slope

##-----------------------Flowering/Fecundity Parameters-----------------## 
## Check the names of the parameters
#head(flow.params)
params$flow_phi<-flow.params$phi     	      ## flow phi
params$flow_sig<-flow.params$sigma				  ## flow sigma
params$flow_sig_u<-flow.params$sigma_u        ## flow sigma u
params$flow_sig_w<-flow.params$sigma_w        ## flow sigma w

params$flow_beta0<-flow.params$beta0          ## flow intercept
params$flow_beta1<-flow.params$beta1          ## flow slopes

##-----------------------Reproductive State Parameters-----------------## 
## Check the names of the parameters
#head(repro.params)
params$repro_beta0<-repro.params$beta0      ## repro intercept
params$repro_beta1<-repro.params$beta1      ## repro slope

##-----------------------Viability Parameters-----------------## 
## Check the names of the parameters
#head(viab.params)
params$viab_sig<-viab.params$sigma
params$viab_sig_u<-viab.params$sigma_u
params$viab_sig_w<-viab.params$sigma_w
####Ant 1 (crem)
params$viab_beta01<-viab.params$beta0.1     	  ## surv intercept
####Ant 2 (liom)
params$viab_beta02<-viab.params$beta0.2     	  ## surv intercept
####Ant 3 (other)
params$viab_beta03<-viab.params$beta0.3     	  ## surv intercept
####Ant 4 (Vacant)
params$viab_beta04<-viab.params$beta0.4     	  ## surv intercept

##-----------------------Seeds Prod Parameters-----------------## 
## Check the names of the parameters
#head(seed.params)
params$seed_phi<-seed.params$phi          ## seed phi
params$seed_sig<-seed.params$sigma        ## seed sigma
####Ant 1 (Crem)
params$seed_beta01<-seed.params$beta0.1      ## seed intercept 
####Ant 2 (Liom)
params$seed_beta02<-seed.params$beta0.2      ## seed intercept
####Ant 3 (Vac)
params$seed_beta03<-seed.params$beta0.3      ## seed intercept

##-----------------------Seeds Precensus Surv Parameters-----------------## 
## Check the names of the parameters
#head(pre.seed.params)
params$preseed_sig<-pre.seed.params$sigma
params$preseed_sig_w<-pre.seed.params$sigma_w
params$preseed_beta0<-pre.seed.params$beta0

##-----------------------Germ1 Parameters-----------------## Ant 1 (crem)
## Check the names of the parameters
#head(germ1.params)
params$germ1_phi<-germ1.params$phi
params$germ1_sig<-germ1.params$sigma
params$germ1_beta0<-germ1.params$beta0

##-----------------------Germ2 Parameters-----------------## Ant 1 (crem)
## Check the names of the parameters
#head(germ2.params)
params$germ2_phi<-germ2.params$phi
params$germ2_sig<-germ2.params$sigma
params$germ2_beta0<-germ2.params$beta0

##------------------------- Recruitment-------------------##
## Check the names of the parameters
#head(rec.params)
params$rec_beta0<-rec.params$beta0        ## Rec intercept
params$rec_sig<-rec.params$sigma         ## Rec error

##-------------------------Transition Parameters-------------------##
cholla.multi<-matrix(NA,nrow=20,ncol=Ndraws) 
## Check the names of the parameters
#head(multi.params)

params$multi_beta11<-multi.params$beta.1.1
params$multi_beta12<-multi.params$beta.1.2
params$multi_beta13<-multi.params$beta.1.3
params$multi_beta14<-multi.params$beta.1.4
params$multi_beta21<-multi.params$beta.2.1
params$multi_beta22<-multi.params$beta.2.2
params$multi_beta23<-multi.params$beta.2.3
params$multi_beta24<-multi.params$beta.2.4
params$multi_beta31<-multi.params$beta.3.1
params$multi_beta32<-multi.params$beta.3.2
params$multi_beta33<-multi.params$beta.3.3
params$multi_beta34<-multi.params$beta.3.4
params$multi_beta41<-multi.params$beta.4.1
params$multi_beta42<-multi.params$beta.4.2
params$multi_beta43<-multi.params$beta.4.3
params$multi_beta44<-multi.params$beta.4.4
params$multi_beta51<-multi.params$beta.5.1
params$multi_beta52<-multi.params$beta.5.2
params$multi_beta53<-multi.params$beta.5.3
params$multi_beta54<-multi.params$beta.5.4

## Calculate the probabilities of being tended by each ant species
size_dummy_real <- seq(min((cactus_real$logsize_t)), max((cactus_real$logsize_t)), by=0.1)
## Previously tended by none
d_vac <- exp(mean(params$multi_beta11) + size_dummy_real*mean(params$multi_beta51)) + 
  exp(mean(params$multi_beta12) + size_dummy_real*mean(params$multi_beta52)) + 
  exp(mean(params$multi_beta13) + size_dummy_real*mean(params$multi_beta53)) + 
  exp(mean(params$multi_beta14) + size_dummy_real*mean(params$multi_beta54))
pred_vac<-cbind(
  ##pr(vac)
  exp(mean(params$multi_beta11) + size_dummy_real*mean(params$multi_beta51))/d_vac,
  ##pr(other)
  exp(mean(params$multi_beta12) + size_dummy_real*mean(params$multi_beta52))/d_vac,
  ##pr(crem)
  exp(mean(params$multi_beta13) + size_dummy_real*mean(params$multi_beta53))/d_vac,
  ##pr(liom)
  exp(mean(params$multi_beta14) + size_dummy_real*mean(params$multi_beta54))/d_vac
)
a<-vector()
for(i in 1:Ndraws){
  a[i]<-sum(pred_vac[i,])
}
a
## Previously tended by Other
d_other <- exp(mean(params$multi_beta21) + size_dummy_real*mean(params$multi_beta51)) + 
  exp(mean(params$multi_beta22) + size_dummy_real*mean(params$multi_beta52)) + 
  exp(mean(params$multi_beta23) + size_dummy_real*mean(params$multi_beta53)) + 
  exp(mean(params$multi_beta24) + size_dummy_real*mean(params$multi_beta54))
pred_other<-cbind(
  ##pr(vac)
  exp(mean(params$multi_beta21) + size_dummy_real*mean(params$multi_beta51))/d_other,
  ##pr(other)
  exp(mean(params$multi_beta22) + size_dummy_real*mean(params$multi_beta52))/d_other,
  ##pr(crem)
  exp(mean(params$multi_beta23) + size_dummy_real*mean(params$multi_beta53))/d_other,
  ##pr(liom)
  exp(mean(params$multi_beta24) + size_dummy_real*mean(params$multi_beta54))/d_other
)
for(i in 1:Ndraws){
  a[i]<-sum(pred_other[i,])
  print(a[i])
}
## Previously tended by other
d_crem <- exp(mean(params$multi_beta31) + size_dummy_real*mean(params$multi_beta51)) + 
  exp(mean(params$multi_beta32) + size_dummy_real*mean(params$multi_beta52)) + 
  exp(mean(params$multi_beta33) + size_dummy_real*mean(params$multi_beta53)) + 
  exp(mean(params$multi_beta34) + size_dummy_real*mean(params$multi_beta54))
pred_crem<-cbind(
  ##pr(vac)
  exp(mean(params$multi_beta31) + size_dummy_real*mean(params$multi_beta51))/d_crem,
  ##pr(other)
  exp(mean(params$multi_beta32) + size_dummy_real*mean(params$multi_beta52))/d_crem,
  ##pr(crem)
  exp(mean(params$multi_beta33) + size_dummy_real*mean(params$multi_beta53))/d_crem,
  ##pr(liom)
  exp(mean(params$multi_beta34) + size_dummy_real*mean(params$multi_beta54))/d_crem
)
for(i in 1:Ndraws){
  a[i]<-sum(pred_crem[i,])
  print(a[i])
}
## Previously tended by Liom
d_liom <- exp(mean(params$multi_beta41) + size_dummy_real*mean(params$multi_beta51)) + 
  exp(mean(params$multi_beta42) + size_dummy_real*mean(params$multi_beta52)) + 
  exp(mean(params$multi_beta43) + size_dummy_real*mean(params$multi_beta53)) + 
  exp(mean(params$multi_beta44) + size_dummy_real*mean(params$multi_beta54))
pred_liom<-cbind(
  ##pr(vac)
  exp(mean(params$multi_beta41) + size_dummy_real*mean(params$multi_beta51))/d_liom,
  ##pr(other)
  exp(mean(params$multi_beta42) + size_dummy_real*mean(params$multi_beta52))/d_liom,
  ##pr(crem)
  exp(mean(params$multi_beta43) + size_dummy_real*mean(params$multi_beta53))/d_liom,
  ##pr(liom)
  exp(mean(params$multi_beta44) + size_dummy_real*mean(params$multi_beta54))/d_liom
)
for(i in 1:Ndraws){
  a[i]<-sum(pred_liom[i,])
  print(a[i])
}
                      ## vac -> vac       vac -> other    vac -> crem       vac -> liom
pred_probs_vac <- cbind((pred_vac[,1]) , (pred_vac[,2]) , (pred_vac[,3]) , (pred_vac[,4]))
                      ## other-> vac        other -> other    other -> crem       other -> liom
pred_probs_other <- cbind((pred_other[,1]) , (pred_other[,2]) , (pred_other[,3]) , (pred_other[,4]))
                      ## crem-> vac       crem -> other    crem -> crem      crem -> liom
pred_probs_crem <- cbind((pred_crem[,1]) , (pred_crem[,2]) , (pred_crem[,3]) , (pred_crem[,4]))
                      ## liom-> vac       liom -> other    liom -> crem       liom -> liom
pred_probs_liom <- cbind((pred_liom[,1]) , (pred_liom[,2]) , (pred_liom[,3]) , (pred_liom[,4]))

# params$multi_beta11<-pred_probs_vac[,1] ## vac -> vac
# params$multi_beta12<-pred_probs_vac[,2] ## vac -> other
# params$multi_beta13<-pred_probs_vac[,3] ## vac -> crem
# params$multi_beta14<-pred_probs_vac[,4] ## vac -> liom
# params$multi_beta21<-pred_probs_other[,1] ## other -> vac
# params$multi_beta22<-pred_probs_other[,2] ## other -> other
# params$multi_beta23<-pred_probs_other[,3] ## other -> crem
# params$multi_beta24<-pred_probs_other[,4] ## other -> liom
# params$multi_beta31<-pred_probs_crem[,1] ## crem -> vac
# params$multi_beta32<-pred_probs_crem[,2] ## crem -> other
# params$multi_beta33<-pred_probs_crem[,3] ## crem -> crem
# params$multi_beta34<-pred_probs_crem[,4] ## crem -> liom
# params$multi_beta41<-pred_probs_liom[,1] ## liom -> vac
# params$multi_beta42<-pred_probs_liom[,2] ## liom -> other
# params$multi_beta43<-pred_probs_liom[,3] ## liom -> crem
# params$multi_beta44<-pred_probs_liom[,4] ## liom -> liom
# 
# ############### 2 ant combos
# ##total proportion that becomes liom by size
# pred_vac[,4] + pred_other[,4] + pred_crem[,4] + pred_liom[,4]
# 
# ## total proportion that becomes vac by size
# pred_vac[,1] + pred_other[,1] + pred_crem[,1] + pred_liom[,1]
# 
# ## total proportion that becomes crem by size
# pred_vac[,3] + pred_other[,3] + pred_crem[,3] + pred_liom[,3]
# 
# ## total proportion that becomes other by size
# pred_vac[,2] + pred_other[,2] + pred_crem[,2] + pred_liom[,2]
# 
# ## total proportion that goes from vacant by size
# rowSums(pred_probs_vac)
# 


  
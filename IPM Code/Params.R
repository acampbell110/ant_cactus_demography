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
####Ant 1 (vacant)
params$grow_beta01<-grow.params$beta0.1     	  ## growth intercept
params$grow_beta11<-grow.params$beta1.1				## growth slope
####Ant 2 (other)
params$grow_beta02<-grow.params$beta0.2     	  ## growth intercept
params$grow_beta12<-grow.params$beta1.2				## growth slope
####Ant 3 (crem)
params$grow_beta03<-grow.params$beta0.3     	  ## growth intercept
params$grow_beta13<-grow.params$beta1.3				## growth slope
####Ant 4 (liom)
params$grow_beta04<-grow.params$beta0.4     	## growth intercept
params$grow_beta14<-grow.params$beta1.4				## growth slope

##-----------------------Survival Parameters-----------------## 
## Check the names of the parameters
#head(surv.params)
params$surv_sig<-surv.params$sigma
params$surv_sig_u<-surv.params$sigma_u
params$surv_sig_w<-surv.params$sigma_w
####Ant 1 (vacant)
params$surv_beta01<-surv.params$beta0.1     	  ## surv intercept
params$surv_beta11<-surv.params$beta1.1				## surv slope
####Ant 2 (other)
params$surv_beta02<-surv.params$beta0.2     	  ## surv intercept
params$surv_beta12<-surv.params$beta1.2				## surv slope
####Ant 3 (crem)
params$surv_beta03<-surv.params$beta0.3     	  ## surv intercept
params$surv_beta13<-surv.params$beta1.3				## surv slope
####Ant 4 (liom)
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
####Ant 1 (vacant)
params$viab_beta01<-viab.params$beta0.4     	  ## surv intercept
####Ant 2 (other)
params$viab_beta02<-viab.params$beta0.3     	  ## surv intercept
####Ant 3 (crem)
params$viab_beta03<-viab.params$beta0.1     	  ## surv intercept
####Ant 4 (liom)
params$viab_beta04<-viab.params$beta0.2     	  ## surv intercept

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
multi.params <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/multi_outputs.csv", header = TRUE,stringsAsFactors=T)
## Pull the random draws from all posterior distributions
multi.params<-multi.params[1:Ndraws,]
##########################################
## Prev Vac
params$multi_betavv <- multi.params$beta.1.1
params$multi_betavo <- multi.params$beta.1.2
params$multi_betavc <- multi.params$beta.1.3
params$multi_betavl <- multi.params$beta.1.4
params$multi_betav <- multi.params$beta.5.1
## Prev Other
params$multi_betaov <- multi.params$beta.2.1
params$multi_betaoo <- multi.params$beta.2.2
params$multi_betaoc <- multi.params$beta.2.3
params$multi_betaol <- multi.params$beta.2.4
params$multi_betao <- multi.params$beta.5.2
## Prev Crem
params$multi_betacv <- multi.params$beta.3.1
params$multi_betaco <- multi.params$beta.3.2
params$multi_betacc <- multi.params$beta.3.3
params$multi_betacl <- multi.params$beta.3.4
params$multi_betac <- multi.params$beta.5.3
## Vac
params$multi_betalv <- multi.params$beta.4.1
params$multi_betalo <- multi.params$beta.4.2
params$multi_betalc <- multi.params$beta.4.3
params$multi_betall <- multi.params$beta.4.4
params$multi_betal <- multi.params$beta.5.4



  
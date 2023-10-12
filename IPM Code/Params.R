## -------- set working directory ---------------------- ##
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")


## load in the cleaned data product created by Create_Clean_Data_Script.R
cactus<-read.csv("cholla_demography_20042021_cleaned.csv")
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code")

cholla_min<- min((cactus$logsize_t), na.rm = TRUE)  ## minsize 
cholla_max<- max((cactus$logsize_t), na.rm = TRUE)  ## maxsize 

Nplots <- length(unique(cactus$Plot))
Nyears <- length(unique(cactus$Year_t))
iter <- 1000
matsize<-200
## This code was creating strange size boundaries
#floor.extend=0.9*cholla_min
#ceiling.extend=1.1*cholla_max
lower<- cholla_min
upper<- cholla_max


## -------- read in MCMC output ---------------------- ##
#Ali
mcmc_dir <- "/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/"
#Tom
#mcmc_dir <- "C:/Users/tm9/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/"
#Lab
#mcmc_dir <- "/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/"

##These files contain all draws from the posterior distributions of all parameters
fit_grow_stud<-readRDS(paste0(mcmc_dir,"fit_grow_student_t.rds"))
grow.params <- rstan::extract(fit_grow_stud)
draws <- sample(7500,1000,replace=F)
surv.params <- read.csv(paste0(mcmc_dir,"surv.params.csv"), header = TRUE,stringsAsFactors=T)
flow.params <- read.csv(paste0(mcmc_dir,"flow.params.csv"), header = TRUE,stringsAsFactors=T)
flow.phi <- read.csv(paste0(mcmc_dir,"flow.phi.csv"), header = TRUE,stringsAsFactors=T)
viab.params <- read.csv(paste0(mcmc_dir,"viab.params.csv"), header = TRUE,stringsAsFactors=T)
repro.params <- read.csv(paste0(mcmc_dir,"repro.params.csv"), header = TRUE,stringsAsFactors=T)
seed.params <- read.csv(paste0(mcmc_dir,"seed.params.csv"), header = TRUE,stringsAsFactors=T)
pre.seed.params <- read.csv(paste0(mcmc_dir,"seed.surv.params.csv"), header = TRUE,stringsAsFactors=T)
germ1.params <- read.csv(paste0(mcmc_dir,"germ1.params.csv"), header = TRUE,stringsAsFactors=T)
germ2.params <- read.csv(paste0(mcmc_dir,"germ2.params.csv"), header = TRUE,stringsAsFactors=T)
rec.params <- read.csv(paste0(mcmc_dir,"rec.params.csv"), header = TRUE,stringsAsFactors=T)


##This file contains random draws from the posterior distributions of the transition models 
multi.params <- read.csv(paste0(mcmc_dir,"multi.params.csv"), header = TRUE,stringsAsFactors=T)

## 'cholla' is a matrix where rows are vital rate coefficients and columns are posterior draws
## below, we will loop over columns, sending each set of coefficients into the stochastic IPM
params <- data.frame(matrix(NA,nrow=1000,ncol=1))
params<-params[,-1]
##----------------------Growth Parameters----------------## 
## Check the names of the parameters
#head(grow.params)
#### No specific ant
params$grow_sig0 <- grow.params$d_0[draws]           ## growth error intercept
params$grow_sig1 <- grow.params$d_size[draws]        ## ## growth error size
params$grow_alp0 <- grow.params$a_0[draws]
params$grow_alp1 <- grow.params$a_size[draws]
params$grow_sig_u<-grow.params$sigma_u[draws]
params$grow_sig_w<-grow.params$sigma_w[draws]
####Ant 1 (vacant)
params$grow_beta04<-grow.params$beta0[draws,4]     	  ## growth intercept
params$grow_beta14<-grow.params$beta1[draws,4]				## growth slope
params$grow_beta24<-grow.params$beta2[draws,4]				## growth slope
####Ant 2 (other)
params$grow_beta03<-grow.params$beta0[draws,3]     	  ## growth intercept
params$grow_beta13<-grow.params$beta1[draws,3]				## growth slope
params$grow_beta23<-grow.params$beta2[draws,3]				## growth slope
####Ant 3 (crem)
params$grow_beta01<-grow.params$beta0[draws,1]     	  ## growth intercept
params$grow_beta11<-grow.params$beta1[draws,1]				## growth slope
params$grow_beta21<-grow.params$beta2[draws,1]				## growth slope
####Ant 4 (liom)
params$grow_beta02<-grow.params$beta0[draws,2]     	## growth intercept
params$grow_beta12<-grow.params$beta1[draws,2]				## growth slope
params$grow_beta22<-grow.params$beta2[draws,2]				## growth slope
#### --- Year Random Effects --- ####

####Ant 1 (prev crem)
grow_rfx1 <- cbind(grow.params$w[draws,1,1],grow.params$w[draws,1,2],grow.params$w[draws,1,3],rep(0,100),rep(0,100),
                   grow.params$w[draws,1,4],grow.params$w[draws,1,5],grow.params$w[draws,1,6],grow.params$w[draws,1,7],
                   grow.params$w[draws,1,8],grow.params$w[draws,1,9],grow.params$w[draws,1,10],grow.params$w[draws,1,11],
                   grow.params$w[draws,1,12],grow.params$w[draws,1,13],grow.params$w[draws,1,14],grow.params$w[draws,1,15],
                   rep(0,100),rep(0,100))
####Ant 2 (prev liom)
grow_rfx2 <- cbind(grow.params$w[draws,2,1],grow.params$w[draws,2,2],grow.params$w[draws,2,3],rep(0,100),rep(0,100),
                   grow.params$w[draws,2,4],grow.params$w[draws,2,5],grow.params$w[draws,2,6],grow.params$w[draws,2,7],
                   grow.params$w[draws,2,8],grow.params$w[draws,2,9],grow.params$w[draws,2,10],grow.params$w[draws,2,11],
                   grow.params$w[draws,2,12],grow.params$w[draws,2,13],grow.params$w[draws,2,14],grow.params$w[draws,2,15],
                   rep(0,100),rep(0,100))
####Ant 3 (prev other)
grow_rfx3 <- cbind(grow.params$w[draws,3,1],grow.params$w[draws,3,2],grow.params$w[draws,3,3],rep(0,100),rep(0,100),
                   grow.params$w[draws,3,4],grow.params$w[draws,3,5],grow.params$w[draws,3,6],grow.params$w[draws,3,7],
                   grow.params$w[draws,3,8],grow.params$w[draws,3,9],grow.params$w[draws,3,10],grow.params$w[draws,3,11],
                   grow.params$w[draws,3,12],grow.params$w[draws,3,13],grow.params$w[draws,3,14],grow.params$w[draws,3,15],
                   rep(0,100),rep(0,100))
####Ant 4 (prev vac)
grow_rfx4 <- cbind(grow.params$w[draws,4,1],grow.params$w[draws,4,2],grow.params$w[draws,4,3],rep(0,100),rep(0,100),
                   grow.params$w[draws,4,4],grow.params$w[draws,4,5],grow.params$w[draws,4,6],grow.params$w[draws,4,7],
                   grow.params$w[draws,4,8],grow.params$w[draws,4,9],grow.params$w[draws,4,10],grow.params$w[draws,4,11],
                   grow.params$w[draws,4,12],grow.params$w[draws,4,13],grow.params$w[draws,4,14],grow.params$w[draws,4,15],
                   rep(0,100),rep(0,100))

##-----------------------Survival Parameters-----------------## 
## Check the names of the parameters
#head(surv.params)
params$surv_sig_u<-surv.params$sigma_u        ## surv sigma u
params$surv_sig_w<-surv.params$sigma_w        ## surv sigma w
####Ant 1 (vacant)
params$surv_beta04<-surv.params$beta0.4     	  ## surv intercept
params$surv_beta14<-surv.params$beta1.4				## surv slope
####Ant 2 (other)
params$surv_beta03<-surv.params$beta0.3     	  ## surv intercept
params$surv_beta13<-surv.params$beta1.3				## surv slope
####Ant 3 (crem)
params$surv_beta01<-surv.params$beta0.1     	  ## surv intercept
params$surv_beta11<-surv.params$beta1.1				## surv slope
####Ant 4 (liom)
params$surv_beta02<-surv.params$beta0.2     	  ## surv intercept
params$surv_beta12<-surv.params$beta1.2				##surv slope
#### --- Year Random Effects --- ####
####Ant 1 (prev crem)
surv_rfx1 <- cbind(surv.params$w.1.1,surv.params$w.1.2,surv.params$w.1.3,rep(0,100),rep(0,100),
                   surv.params$w.1.4,surv.params$w.1.5,surv.params$w.1.6,surv.params$w.1.7,
                   surv.params$w.1.8,surv.params$w.1.9,surv.params$w.1.10,surv.params$w.1.11,
                   surv.params$w.1.12,surv.params$w.1.13,surv.params$w.1.14,surv.params$w.1.15,
                   surv.params$w.1.16,rep(0,100))
####Ant 2 (prev liom)
surv_rfx2 <- cbind(surv.params$w.2.1,surv.params$w.2.2,surv.params$w.2.3,rep(0,100),rep(0,100),
                   surv.params$w.2.4,surv.params$w.2.5,surv.params$w.2.6,surv.params$w.2.7,
                   surv.params$w.2.8,surv.params$w.2.9,surv.params$w.2.10,surv.params$w.2.11,
                   surv.params$w.2.12,surv.params$w.2.13,surv.params$w.2.14,surv.params$w.2.15,
                   surv.params$w.2.16,rep(0,100))
####Ant 3 (prev other)
surv_rfx3 <- cbind(surv.params$w.3.1,surv.params$w.3.2,surv.params$w.3.3,rep(0,100),rep(0,100),
                   surv.params$w.3.4,surv.params$w.3.5,surv.params$w.3.6,surv.params$w.3.7,
                   surv.params$w.3.8,surv.params$w.3.9,surv.params$w.3.10,surv.params$w.3.11,
                   surv.params$w.3.12,surv.params$w.3.13,surv.params$w.3.14,surv.params$w.3.15,
                   surv.params$w.3.16,rep(0,100))
####Ant 4 (prev vac)
surv_rfx4 <- cbind(surv.params$w.4.1,surv.params$w.4.2,surv.params$w.4.3,rep(0,100),rep(0,100),
                   surv.params$w.4.4,surv.params$w.4.5,surv.params$w.4.6,surv.params$w.4.7,
                   surv.params$w.4.8,surv.params$w.4.9,surv.params$w.4.10,surv.params$w.4.11,
                   surv.params$w.4.12,surv.params$w.4.13,surv.params$w.4.14,surv.params$w.4.15,
                   surv.params$w.4.16,rep(0,100))

##-----------------------Flowering/Fecundity Parameters-----------------## 
## Check the names of the parameters
#head(flow.params)
params$flow_phi<-flow.phi$x   	      ## flow phi
params$flow_sig_u<-flow.params$sigma_u        ## flow sigma u
params$flow_sig_w<-flow.params$sigma_w        ## flow sigma w

params$flow_beta0<-flow.params$beta0          ## flow intercept
params$flow_beta1<-flow.params$beta1          ## flow slopes
#### --- Year Random Effects --- ####
flow_rfx <- cbind(flow.params$w.1,flow.params$w.2,flow.params$w.3,flow.params$w.4,rep(0,100),
                  rep(0,100),rep(0,100),rep(0,100),rep(0,100),flow.params$w.5,flow.params$w.6,
                  flow.params$w.7,flow.params$w.8,flow.params$w.9,flow.params$w.10,flow.params$w.11,
                  flow.params$w.12,flow.params$w.13,flow.params$w.14)

##-----------------------Reproductive State Parameters-----------------## 
## Check the names of the parameters
#head(repro.params)
params$repro_beta0<-repro.params$beta0      ## repro intercept
params$repro_beta1<-repro.params$beta1      ## repro slope
params$repro_sig_u<-repro.params$sigma_u    ## repro sigma u
params$repro_sig_w<-repro.params$sigma_w    ## repro sigma w
#### --- Year Random Effects --- ####


repro_rfx <- cbind(repro.params$w.1,repro.params$w.2,repro.params$w.3,repro.params$w.4,
                   rep(0,100),rep(0,100),rep(0,100),rep(0,100),repro.params$w.5,repro.params$w.6,
                   repro.params$w.7,repro.params$w.8,repro.params$w.9,repro.params$w.10,repro.params$w.11,
                   repro.params$w.12,repro.params$w.11,repro.params$w.13,repro.params$w.14)
##-----------------------Viability Parameters-----------------## 
## Check the names of the parameters
#head(viab.params) 
params$viab_sig<-viab.params$sigma              ## viab sigma
params$viab_sig_u<-viab.params$sigma_u          ## viab sigma u
params$viab_sig_w<-viab.params$sigma_w          ## viab sigma w
####Ant 4 (vacant)
params$viab_beta04<-viab.params$beta0.4     	  ## viab intercept
####Ant 3 (other)
params$viab_beta03<-viab.params$beta0.3     	  ## viab intercept
####Ant 1 (crem)
params$viab_beta01<-viab.params$beta0.1     	  ## viab intercept
####Ant 2 (liom)
params$viab_beta02<-viab.params$beta0.2     	  ## viab intercept
#### --- Year Random Effects --- ####
####Ant 1 (prev crem)
viab_rfx1 <- cbind(rep(0,100),viab.params$w.1.1,viab.params$w.1.2,viab.params$w.1.3,rep(0,100),
                   rep(0,100),rep(0,100),rep(0,100),rep(0,100),viab.params$w.1.4,
                   viab.params$w.1.5,viab.params$w.1.6, viab.params$w.1.7,viab.params$w.1.8,
                   viab.params$w.1.9,viab.params$w.1.10,viab.params$w.1.11,viab.params$w.1.12,
                   viab.params$w.1.13)
####Ant 2 (prev liom)
viab_rfx2 <- cbind(rep(0,100),viab.params$w.2.1,viab.params$w.2.2,viab.params$w.2.3,rep(0,100),
                   rep(0,100),rep(0,100),rep(0,100),rep(0,100),viab.params$w.2.4,
                   viab.params$w.2.5,viab.params$w.2.6, viab.params$w.2.7,viab.params$w.2.8,
                   viab.params$w.2.9,viab.params$w.2.10,viab.params$w.2.11,viab.params$w.2.12,
                   viab.params$w.2.13)
####Ant 3 (prev other)
viab_rfx3 <- cbind(rep(0,100),viab.params$w.3.1,viab.params$w.3.2,viab.params$w.3.3,rep(0,100),
                   rep(0,100),rep(0,100),rep(0,100),rep(0,100),viab.params$w.3.4,
                   viab.params$w.3.5,viab.params$w.3.6, viab.params$w.3.7,viab.params$w.3.8,
                   viab.params$w.3.9,viab.params$w.3.10,viab.params$w.3.11,viab.params$w.3.12,
                   viab.params$w.3.13)
####Ant 2 (prev vac)
viab_rfx4 <- cbind(rep(0,100),viab.params$w.4.1,viab.params$w.4.2,viab.params$w.4.3,rep(0,100),
                   rep(0,100),rep(0,100),rep(0,100),rep(0,100),viab.params$w.4.4,
                   viab.params$w.4.5,viab.params$w.4.6, viab.params$w.4.7,viab.params$w.4.8,
                   viab.params$w.4.9,viab.params$w.4.10,viab.params$w.4.11,viab.params$w.4.12,
                   viab.params$w.4.13)

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
params$preseed_sig<-pre.seed.params$sigma         ## pre seed sigma
params$preseed_sig_w<-pre.seed.params$sigma_w       ## pre seed sigma w
params$preseed_beta0<-pre.seed.params$beta0       ## pre seed intercept

##-----------------------Germ1 Parameters-----------------## Ant 1 (crem)
## Check the names of the parameters
#head(germ1.params)
params$germ1_phi<-germ1.params$phi            ## germ 1 phi
params$germ1_sig<-germ1.params$sigma            ## germ 1 sigma
params$germ1_beta0<-germ1.params$beta0        ## germ 1 intercept

##-----------------------Germ2 Parameters-----------------## Ant 1 (crem)
## Check the names of the parameters
#head(germ2.params)
params$germ2_phi<-germ2.params$phi            ## germ 2 phi
params$germ2_sig<-germ2.params$sigma        ## germ 2 sigma
params$germ2_beta0<-germ2.params$beta0        ## germ 2 intercept

##------------------------- Recruitment-------------------##
## Check the names of the parameters
#head(rec.params)
params$rec_beta0<-rec.params$beta0        ## Rec intercept
params$rec_sig<-rec.params$sigma         ## Rec error

##-------------------------Transition Parameters-------------------##

## Prev Vac
params$multi_betavv <- multi.params$beta.4.4 ## intercept for vacant to vacant  
params$multi_betavo <- multi.params$beta.4.3 ## intercept for vacant to other
params$multi_betavc <- multi.params$beta.4.1 ## intercept for vacant to crem
params$multi_betavl <- multi.params$beta.4.2 ## intercept for vacant to liom
params$multi_betav <- multi.params$beta.5.4 ## Size specific vacant slope
## Prev Other
params$multi_betaov <- multi.params$beta.3.4
params$multi_betaoo <- multi.params$beta.3.3
params$multi_betaoc <- multi.params$beta.3.1
params$multi_betaol <- multi.params$beta.3.2
params$multi_betao <- multi.params$beta.5.3
## Prev Crem
params$multi_betacv <- multi.params$beta.1.4
params$multi_betaco <- multi.params$beta.1.3
params$multi_betacc <- multi.params$beta.1.2
params$multi_betacl <- multi.params$beta.1.1
params$multi_betac <- multi.params$beta.5.1
## Prev Liom
params$multi_betalv <- multi.params$beta.2.4
params$multi_betalo <- multi.params$beta.2.3
params$multi_betalc <- multi.params$beta.2.1
params$multi_betall <- multi.params$beta.2.2
params$multi_betal <- multi.params$beta.5.2

multi_rfx1 <- cbind(rep(0,100),rep(0,100),rep(0,100),rep(0,100),
                    rep(0,100),rep(0,100),rep(0,100),rep(0,100),
                    rep(0,100),rep(0,100),rep(0,100),rep(0,100),
                    rep(0,100),rep(0,100),rep(0,100),rep(0,100),
                    rep(0,100))
 
  




######################## Tests nothing else ----
## Crem Random Effects
c <- rbind(grow_rfx1,surv_rfx1,viab_rfx1)
colMeans(c)
mean(c)
# 0.001281196 -- slightly positive overall mean
## Liom Random Effects
l <- rbind(grow_rfx2,surv_rfx2,viab_rfx2)
colMeans(l)
mean(l)
# -0.002160359 -- slightly negative overall mean. 2x the magnitude of crem
## Other Random Effects
o <- rbind(grow_rfx3,surv_rfx3,viab_rfx3)
colMeans(o)
mean(o)
# -0.002048737 -- slightly negative overall mean. Very close to liom, but slightly smaller magnitude
## Vacant Random Effects
v <- rbind(grow_rfx4,surv_rfx4,viab_rfx4)
colMeans(v)
mean(v)
# -0.0008365305 -- slightly negative overall mean. Smallest magnitude by far








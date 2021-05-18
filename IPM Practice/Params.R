## -------- set working directory ---------------------- ##

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Practice")

## -------- read in MCMC output ---------------------- ##

##This file contains random draws from the joint posterior distribution of all parameters
post.params<-read.csv("params_outputs.csv")

## Number of draws to take from the joint posterior distribution of the parameters. 
## Cannot be greater than the number of draws provided in the .csv file, which is 500.
Ndraws<-min(100,nrow(post.params))
post.params<-post.params[1:Ndraws,]

## -------- load IPM source functions ---------------------- ##

source("ElderdMiller_chollaIPM_SOURCE.R")
source("Prac.R")

## -------- Set up IPM parameter vector ---------------------- ##

## 'cholla' is a matrix where rows are vital rate coefficients and columns are posterior draws
## below, we will loop over columns, sending each set of coefficients into the stochastic IPM
cholla<-matrix(NA,nrow=100,ncol=Ndraws) 
##----------------------Growth Parameters----------------## Ant 1 (crem)

cholla[1,]<-post.params$beta0_g.1      	  ## growth intercept
cholla[2,]<-post.params$beta1_g.1				  ## growth slope
cholla[3,]<-post.params$u_g.1				      ## growth plotfx
cholla[4,]<-post.params$w_g.1					    ## growth yrfx
cholla[5,]<-post.params$sigma_g			      ## growth error
cholla[6,]<-post.params$sigma_u_g  	      ## growth plotfx error
cholla[7,]<-post.params$sigma_w_g         ## growth yrfx error

##-----------------------Survival Parameters-----------------## Ant 1 (crem)
cholla[11,]<-post.params$beta0_s.1      	## surv intercept
cholla[12,]<-post.params$beta1_s.1				## surv slope
cholla[13,]<-post.params$u_s.1			    	## surv plotfx
cholla[14,]<-post.params$w_s.1					  ## surv yrfx
cholla[15,]<-post.params$sigma_s			    ## surv error
cholla[16,]<-post.params$sigma_u_s  	    ## surv plotfx error
cholla[17,]<-post.params$sigma_w_s        ## surv yrfx error

##-----------------------Flowering/Fecundity Parameters-----------------## Ant 1 (crem)
cholla[21,]<-post.params$beta0_f      	  ## flower intercept
cholla[22,]<-post.params$beta1_f				  ## flower slope
cholla[23,]<-post.params$u_f.1			      ## flower plotfx
cholla[24,]<-post.params$w_f.1					  ## flower yrfx
cholla[25,]<-post.params$sigma_f			    ## flower error
cholla[26,]<-post.params$sigma_u_f  	    ## flower plotfx error
cholla[27,]<-post.params$sigma_w_f        ## flower yrfx error
cholla[28,]<-post.params$phi_f            ## flower dispersion parameter

##-----------------------Reproductive State Parameters-----------------## Ant 1 (crem)
cholla[31,]<-post.params$beta0_r      	  ## repro intercept
cholla[32,]<-post.params$beta1_r				  ## repro slope
cholla[33,]<-post.params$u_r.1				    ## repro plotfx
cholla[34,]<-post.params$w_r.1					  ## repro yrfx
cholla[35,]<-post.params$sigma_r			    ## repro error
cholla[36,]<-post.params$sigma_u_r  	    ## repro plotfx error
cholla[37,]<-post.params$sigma_w_r        ## repro yrfx error

##-----------------------Viability Parameters-----------------## Ant 1 (crem)
cholla[41,]<-post.params$beta0_v.1      	## viab coeff 
cholla[42,]<-post.params$u_v.1				    ## viab plotfx
cholla[43,]<-post.params$w_v.1					  ## viab yrfx
cholla[44,]<-post.params$sigma_v			    ## viab error
cholla[45,]<-post.params$sigma_u_v  	    ## viab plotfx error
cholla[46,]<-post.params$sigma_w_v        ## viab yrfx error

##-----------------------Seeds Parameters-----------------## Ant 1 (crem)
cholla[51,]<-post.params$beta0_seed.1     ## seed intercept
cholla[52,]<-post.params$v_seed.1			  	## seed plotfx
cholla[53,]<-post.params$sigma_seed			  ## seed error
cholla[54,]<-post.params$sigma_v_seed  	  ## seed plotfx error
cholla[55,]<-post.params$phi_seed         ## seed dispersion parameter

## Params 61-70: Misc params (bounds of continuous size domain, in units of log(cm^3)). Hard coded from Miller's data. 
cholla[61,]<- min(log(cactus$volume_t))  ## minsize 
cholla[62,]<- max(log(cactus$volume_t))  ## maxsize 

## -------- set working directory ---------------------- ##

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Practice")

## -------- read in MCMC output ---------------------- ##

##This file contains random draws from the joint posterior distribution of all parameters
post.params <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/params_outputs.csv", header = TRUE,stringsAsFactors=T)          

## Number of draws to take from the joint posterior distribution of the parameters. 
## Cannot be greater than the number of draws provided in the .csv file, which is 500.
Ndraws<-min(100,nrow(post.params))
post.params<-post.params[1:Ndraws,]
Nplots <- length(unique(cactus$Plot))
Nyears <- length(unique(cactus$Year_t))
iter <- 1000
matsize<-200
floor.extend=1
ceiling.extend=4
lower<- cholla[101] - floor.extend
upper<- cholla[102] + ceiling.extend

## -------- load IPM source functions ---------------------- ##

source("Prac.R")

## -------- Set up IPM parameter vector ---------------------- ##

## 'cholla' is a matrix where rows are vital rate coefficients and columns are posterior draws
## below, we will loop over columns, sending each set of coefficients into the stochastic IPM
cholla<-matrix(NA,nrow=105,ncol=Ndraws) 

##----------------------Growth Parameters----------------## Ant 1 (crem)

cholla[1,]<-post.params$beta0_g.1      	  ## growth intercept
cholla[2,]<-post.params$beta1_g.1				  ## growth slope
cholla[3,]<-post.params$sigma_g			      ## growth error
cholla[4,]<-post.params$sigma_u_g  	      ## growth plotfx error
cholla[5,]<-post.params$sigma_w_g         ## growth yrfx error

##-----------------------Survival Parameters-----------------## Ant 1 (crem)
cholla[11,]<-post.params$beta0_s.1      	## surv intercept
cholla[12,]<-post.params$beta1_s.1				## surv slope
cholla[13,]<-post.params$sigma_s			    ## surv error
cholla[14,]<-post.params$sigma_u_s  	    ## surv plotfx error
cholla[15,]<-post.params$sigma_w_s        ## surv yrfx error

##-----------------------Flowering/Fecundity Parameters-----------------## Ant 1 (crem)
cholla[21,]<-post.params$beta0_f      	  ## flower intercept
cholla[22,]<-post.params$beta1_f				  ## flower slope
cholla[23,]<-post.params$sigma_f			    ## flower error
cholla[24,]<-post.params$sigma_u_f  	    ## flower plotfx error
cholla[25,]<-post.params$sigma_w_f        ## flower yrfx error
cholla[26,]<-post.params$phi_f            ## flower dispersion parameter

##-----------------------Reproductive State Parameters-----------------## Ant 1 (crem)
cholla[31,]<-post.params$beta0_r      	  ## repro intercept
cholla[32,]<-post.params$beta1_r				  ## repro slope
cholla[33,]<-post.params$sigma_r			    ## repro error
cholla[34,]<-post.params$sigma_u_r  	    ## repro plotfx error
cholla[35,]<-post.params$sigma_w_r        ## repro yrfx error

##-----------------------Viability Parameters-----------------## Ant 1 (crem)
cholla[41,]<-post.params$beta0_v.1      	## viab coeff 
cholla[42,]<-post.params$sigma_v			    ## viab error
cholla[43,]<-post.params$sigma_u_v  	    ## viab plotfx error
cholla[44,]<-post.params$sigma_w_v        ## viab yrfx error

##-----------------------Seeds Prod Parameters-----------------## Ant 1 (crem)
cholla[51,]<-post.params$beta0_seed.1     ## seed intercept
cholla[52,]<-post.params$sigma_seed			  ## seed error
cholla[53,]<-post.params$phi_seed         ## seed dispersion parameter

##-----------------------Seeds Surv Parameters-----------------## Ant 1 (crem)
cholla[61,]<-post.params$beta0_seed_s.1     ## seed intercept
cholla[62,]<-post.params$sigma_seed_s			  ## seed error
cholla[63,]<-post.params$phi_seed_s         ## seed dispersion parameter

##-----------------------Germ1 Parameters-----------------## Ant 1 (crem)
cholla[71,]<-post.params$beta0_germ1        ## germ intercept
cholla[72,]<-post.params$beta1_germ1        ## germ slope
cholla[73,]<-post.params$sigma_germ1        ## germ error
cholla[74,]<-post.params$phi_germ1          ## germ dispersion parameter

##-----------------------Germ2 Parameters-----------------## Ant 1 (crem)
cholla[81,]<-post.params$beta0_germ2        ## germ intercept
cholla[82,]<-post.params$beta1_germ2        ## germ slope
cholla[83,]<-post.params$sigma_germ2        ## germ error
cholla[84,]<-post.params$phi_germ2          ## germ dispersion parameter

##-----------------------Precensus Surv Parameters-----------------## Ant 1 (crem)
cholla[91,]<-post.params$beta0_precen       ## precen intercept
cholla[92,]<-post.params$beta1_precen       ## precen slope
cholla[93,]<-post.params$sigma_precen       ## precen error


## Params 61-70: Misc params (bounds of continuous size domain, in units of log(cm^3)). Hard coded from Miller's data. 
cholla[101,]<- min(log(cactus$volume_t), na.rm = TRUE)  ## minsize 
cholla[102,]<- max(log(cactus$volume_t), na.rm = TRUE)  ## maxsize 

for(i in 1:Ndraws) {
  
  ## sample a sequence of random deviates representing plot-to-plot variance in each of the four main vital rates
  yrfx <- matrix(0,5,Nplots)
  yrfx[1,] <- rnorm(n=Nplots,mean=0,sd=cholla[6,i]) # Growth
  yrfx[2,] <- rnorm(n=Nplots,mean=0,sd=cholla[15,i]) # Survival 
  yrfx[3,] <- rnorm(n=Nplots,mean=0,sd=cholla[25,i]) # Repro 
  yrfx[4,] <- rnorm(n=Nplots,mean=0,sd=cholla[35,i]) # Flowers
  yrfx[5,] <- rnorm(n=Nplots,mean=0,sd=cholla[44,i]) # Viability
  ## sample a sequence of random deviates representing year-to-year variance in each of the four main vital rates, individually
  plotfx <- matrix(0,5,iter)  
  plotfx[1,] <- rnorm(n=iter,mean=0,sd=cholla[5,i]) # Growth
  plotfx[2,] <- rnorm(n=iter,mean=0,sd=cholla[14,i]) # Survival 
  plotfx[3,] <- rnorm(n=iter,mean=0,sd=cholla[24,i]) # Probability of flowering 
  plotfx[4,] <- rnorm(n=iter,mean=0,sd=cholla[34,i]) # Fertility
  plotfx[5,] <- rnorm(n=iter,mean=0,sd=cholla[43,i]) # Viability
}
  

##### #Run the matrix once with quantile values for the betas
gxy(4, 5,cholla)

sx(4, cholla)

pxy(4, 5, cholla)

fx(4,cholla) ## Not Working

bigmatrix(cholla,lower,upper,matsize)

lambda.fun(cholla,iter,matsize,extra.grid = 2,floor.extend = 1, ceiling.extend = 4)
lambda


  
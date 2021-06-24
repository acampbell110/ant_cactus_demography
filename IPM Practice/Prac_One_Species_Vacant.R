setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Practice")
#########################################################################################################
##            This will be an IPM which only includes one ant state -- Crem.
#########################################################################################################

## ----------- Miscellany...we'll need an inverse logit functions ------------- ##
invlogit<-function(x){exp(x)/(1+exp(x))}

## ----------- Vital rate functions. Parameter indices are hard-coded and must correspond to rows of MCMC matrix ------------- ##
#GROWTH FROM SIZE X TO Y
gxy<-function(x,y,params,i){
  xb=pmin(pmax(x,params[94]),params[95]) #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  g_crem = dnorm(y,mean=params[1] + params[2]*xb,sd=params[3])
  g_vac = dnorm(y,mean=params[301] + params[302]*xb,sd = params[3])
  ifelse(i == "crem", return(g_crem),return(g_vac))
}

#SURVIVAL AT SIZE X.
sx<-function(x,params,i){
  xb=pmin(pmax(x,params[94]),params[95])
  s_crem = invlogit(params[11] + params[12]*xb)
  s_vac = invlogit(params[311] + params[312]*xb)
  ifelse(i == "crem", return(s_crem),return(s_vac))
}

#SURVIVAL*GROWTH
pxy<-function(x,y,params,i){
  xb=pmin(pmax(x,params[94]),params[95])
  return(sx(xb,params,i)*gxy(xb,y,params,i))
}

#PRODUCTION OF 1-YO SEEDS IN THE SEED BANK FROM X-SIZED MOMS
fx<-function(x,params,i){
  xb=pmin(pmax(x,params[94]),params[95])        ## X dummy variable
  p.flow<-invlogit(params[31] + params[32]*xb)    ## Probability of Reproducing
  nflow<-exp(params[21] + params[22]*xb)          ## Number of FLowers produced
  flow.surv_crem<-invlogit(params[41]) ## Proportion of Flowers survive to fruit
  flow.surv_vac<-invlogit(params[341]) ## Proportion of Flowers survive to fruit
  seeds.per.fruit<-params[51]                     ## Number of Seeds per Fruit
  seed.survival<-invlogit(params[61])^2           ## Seed per Fruit Survival ---------I measured 6-month seed survival; annual survival is its square
  ifelse(i == "crem",
         (p.flow*nflow*flow.surv_crem*seeds.per.fruit*seed.survival),
         (p.flow*nflow*flow.surv_vac*seeds.per.fruit*seed.survival)
         )
}

#
recruits<-function(y,params){
  yb=pmin(pmax(y,params[94]),params[95])
  dnorm(yb, (params[96]),params[97])
}

beta<-function(vac_rec){
  ifelse(vac_rec == TRUE, return(0), return(1))
}

#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE
transition.x<-function(i,j,x){
  xb=pmin(pmax(x,params[94]),params[95])        ## X dummy variable
  ifelse(i == "crem",
         ifelse(j =="crem",
                (invlogit(params[100] + params[101]*xb)),
                (1 - invlogit(params[100] + params[101]*xb))
         ),
         ifelse(j == "vac",
                (invlogit(params[98] + params[99]*xb)),
                (1 - invlogit(params[98] + params[99]*xb))
         )
  )
}

#GROWTH*SURVIVAL*ANT PROBABILITIES
ptxy<-function(x,y,params,i,j){
  xb=pmin(pmax(x,params[94]),params[95])
  return(sx(xb,params,i)*gxy(xb,y,params,i)*transition.x(i,j,xb))
}

bigmatrix<-function(params,lower,upper,matsize){  
  ###################################################################################################
  ## returns the full IPM kernel (to be used in stochastic simulation), the F and T kernels, and meshpoints in the units of size
  ## params,yrfx,plotfx, and mwye get passed to the vital rate functions
  ## f.eps is fertility overdispersion. defaults to zero (see lambda.fun())
  ## lower and upper are the integration limits
  ## matsize is the dimension of the approximating matrix (it gets an additional 2 rows and columns for the seed banks)
  ###################################################################################################
  
  n<-matsize
  L<-lower; U<-upper
  h<-(U-L)/n                   #Bin size
  b<-L+c(0:n)*h;               #Lower boundaries of bins 
  y<-0.5*(b[1:n]+b[2:(n+1)]);  #Bin midpoints
  
  # Fertility matrix
  Fmat<-matrix(0,(2*n+2),(2*n+2))
  
  # Banked seeds go in top row
  fec1_v<-fx(y,params=params,"vac")
  fec1_c<-fx(y,params=params,"crem")
  Fmat[1,3:(n+2)]<-fec1_c
  Fmat[1,(n+3):(2*n+2)]<-fec1_v
  
  # Growth/survival transition matrix
  Tmat<-matrix(0,(2*n+2),(2*n+2))
  
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  Tmat[2,1]<-1-invlogit(params[71])
  
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  beta1_c<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
  beta1_v<- invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
  Tmat[3:(n+2),1]<-beta1_v
  Tmat[3:(n+2),2]<-beta1_c

  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  #Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)  
  beta2_c<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
  beta2_v<- invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
  Tmat[(n+3):(2*n+2),1]<-beta2_v
  Tmat[(n+3):(2*n+2),2]<-beta2_c
  # Growth/survival transitions among cts sizes
  vac_vac<-t(outer(y,y,ptxy,params=params,"vac","vac"))*h
  vac_crem<-t(outer(y,y,ptxy,params=params,"vac","crem"))*h
  crem_vac<-t(outer(y,y,ptxy,params=params,"crem","vac"))*h
  crem_crem<-t(outer(y,y,ptxy,params=params,"crem","crem"))*h
  Tmat[3:(n+2),3:(n+2)]<- vac_vac## Top left 
  Tmat[3:(n+2),(n+3):(2*n+2)]<-crem_vac ## Top Right 
  Tmat[(n+3):(2*n+2),3:(n+2)]<- crem_crem## Bottom Right 
  Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- vac_crem## Bottom Right 
  
  # Put it all together
  IPMmat<-Fmat+Tmat     
  
  return(list(IPMmat=IPMmat,Fmat=Fmat,Tmat=Tmat,meshpts=y))
}
#image(y,y,t(Tmat),main='fecundity kernel')
#image(y,y,t(Fmat),main='fecundity kernel')


## ----------------- Function that simulates population dynamics and returns lambdaS ------------- ############

lambda.fun<-function(params,iter,
                     matsize,extra.grid=2,floor.extend=1,ceiling.extend=4,i,j){
  ############################################################################################
  ## This function returns the population growth rate for a given set of parameters
  ## Defaults to lambdaS (stochastic=T) but can give deterministic lambda based on vital rate means
  ## extra.grid adds the discrete seed stages to the cts kernel
  ## floor extend and ceiling.extend correct eviction (see Williams, Miller, Ellner (2012), Ecology)
  ## corr==T (default) includes vital rate correlations. corr==F sets mwye to zero and therefore turns correlations off
  ############################################################################################
  
  ## IPM bounds
  lower<- params[94] - floor.extend
  upper<- params[95] + ceiling.extend
  
  lambda<-Re(eigen(bigmatrix(params,lower,upper,matsize,i,j)$IPMmat)$values[1])
  return(lambda)
  
}
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Practice")
#########################################################################################################
##            This will be an IPM which only includes one ant state -- Crem.
#########################################################################################################

## ----------- Miscellany...we'll need an inverse logit functions ------------- ##
invlogit<-function(x){exp(x)/(1+exp(x))}

## ----------- Vital rate functions. Parameter indices are hard-coded and must correspond to rows of MCMC matrix ------------- ##
#GROWTH FROM SIZE X TO Y
gxy<-function(x,y,params,yrfx,plotfx){
  xb=pmin(pmax(x,params[61]),params[62]) #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  return(dnorm(y,mean=params[1] + params[2]*xb  + yrfx[1] + plotfx[1],sd=params[4]))
}

#SURVIVAL AT SIZE X.
sx<-function(x,params,yrfx,plotfx){
  xb=pmin(pmax(x,params[61]),params[62])
  return(invlogit(params[11] + params[12]*xb   + yrfx[2] + plotfx[2]))
}

#SURVIVAL*GROWTH
pxy<-function(x,y,params,yrfx,plotfx){
  xb=pmin(pmax(x,params[61]),params[62])
  sx(xb,params,yrfx,plotfx)*gxy(xb,y,params,yrfx,plotfx)
}

#PRODUCTION OF 1-YO SEEDS IN THE SEED BANK FROM X-SIZED MOMS
fx<-function(x,params,yrfx,plotfx,f.eps){
  xb=pmin(pmax(x,params[61]),params[62])
  p.flow<-invlogit(params[21] + params[22]*xb + yrfx[3] + plotfx[3]) 
  nfruits<-exp(params[31] + params[32]*xb + yrfx[4] + plotfx[4] + f.eps)   
  seeds.per.fruit<-params[41]
  seed.survival<-invlogit(params[42])^2  ## I measured 6-month seed survival; annual survival is its square
  return(p.flow*nfruits*seeds.per.fruit*seed.survival)  
}

bigmatrix<-function(params,yrfx,plotfx,mwye,f.eps,lower,upper,matsize){  
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
  Fmat<-matrix(0,(n+2),(n+2))
  
  # Banked seeds go in top row
  Fmat[1,3:(n+2)]<-fx(y,params=params,yrfx=yrfx,plotfx=plotfx,f.eps=f.eps)
  
  # Growth/survival transition matrix
  Tmat<-matrix(0,(n+2),(n+2))
  
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  Tmat[2,1]<-1-invlogit(params[43])
  
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),1]<-invlogit(params[43])*recruits(y,params)*h*invlogit(params[45])   
  
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),2]<-invlogit(params[44])*recruits(y,params)*h*invlogit(params[45])  
  
  # Growth/survival transitions among cts sizes
  Tmat[3:(n+2),3:(n+2)]<-t(outer(y,y,pxy,params=params,yrfx=yrfx,plotfx=plotfx))*h
  
  # Put it all together
  IPMmat<-Fmat+Tmat     
  
  return(list(IPMmat=IPMmat,Fmat=Fmat,Tmat=Tmat,meshpts=y))
}
image(y,y,t(Tmat),main='fecundity kernel')
image(y,y,t(Fmat),main='fecundity kernel')


## ----------------- Function that simulates population dynamics and returns lambdaS ------------- ############

lambda.fun<-function(parameters,yrfx,plotfx,f.eps=0,iter,
                     matsize,extra.grid=2,floor.extend=1,ceiling.extend=4,stochastic=T,corr=T){
  ############################################################################################
  ## This function returns the population growth rate for a given set of parameters
  ## Defaults to lambdaS (stochastic=T) but can give deterministic lambda based on vital rate means
  ## extra.grid adds the discrete seed stages to the cts kernel
  ## floor extend and ceiling.extend correct eviction (see Williams, Miller, Ellner (2012), Ecology)
  ## corr==T (default) includes vital rate correlations. corr==F sets mwye to zero and therefore turns correlations off
  ############################################################################################
  
  ## IPM bounds
  lower<- parameters[61] - floor.extend
  upper<- parameters[62] + ceiling.extend
  
  if(stochastic==F){
    yrfx <- matrix(0,1,4)
    mwye <- 0
    lambda<-Re(eigen(bigmatrix(params=parameters,yrfx=yrfx,plotfx=plotfx,f.eps=f.eps,lower=lower,upper=upper,matsize=matsize)$IPMmat)$values[1])
    return(lambda)
  }
  }
  
  
  
  

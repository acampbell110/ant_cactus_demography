setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Practice")
#########################################################################################################
##            This will be an IPM which only includes one ant state -- Crem.
#########################################################################################################

## ----------- Miscellany...we'll need an inverse logit functions ------------- ##
invlogit<-function(x){exp(x)/(1+exp(x))}

## ----------- Vital rate functions. Parameter indices are hard-coded and must correspond to rows of MCMC matrix ------------- ##
#GROWTH FROM SIZE X TO Y
gxy<-function(x,y,params){
  xb=pmin(pmax(x,params[94]),params[95]) #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  return(dnorm(y,mean=params[1,] + params[2,]*xb,sd=params[3,]))
}

#SURVIVAL AT SIZE X.
sx<-function(x,params){
  xb=pmin(pmax(x,params[94]),params[95])
  return(invlogit(params[11,] + params[12,]*xb))
}

#SURVIVAL*GROWTH
pxy<-function(x,y,params){
  xb=pmin(pmax(x,params[94]),params[95])
  sx(xb,params)*gxy(xb,y,params)
}

#PRODUCTION OF 1-YO SEEDS IN THE SEED BANK FROM X-SIZED MOMS
fx<-function(x,params){
  xb=pmin(pmax(x,params[94]),params[95])        ## X dummy variable
  p.flow<-invlogit(params[31,] + params[32,]*xb)    ## Probability of Reproducing
  nflow<-exp(params[21,] + params[22,]*xb)          ## Number of FLowers produced
  flow.surv<-invlogit(params[41,]) ## Proportion of Flowers survive to fruit
  seeds.per.fruit<-params[51,]                     ## Number of Seeds per Fruit
  seed.survival<-invlogit(params[61,])^2           ## Seed per Fruit Survival ---------I measured 6-month seed survival; annual survival is its square
  return(p.flow*nflow*flow.surv*seeds.per.fruit*seed.survival)  
}

recruits<-function(y,params){
  yb=pmin(pmax(y,params[94]),params[95])
  dnorm(yb, log(cholla[96,]),cholla[97,])
}

beta<-function(vac_rec){
  ifelse(vac_rec == TRUE, return(0), return(1))
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
  y_list = rep(y,100)
  xb=pmin(pmax(y,params[94]),params[95])
  
  
  FMaster <- list(Fmat)
  TMaster <- list(Tmat)
  IPMMaster <- list(IPMmat)
  
  for(a in 1:100){
    #fx(x,params)[a]
    Fmat<-matrix(0,(n+2),(n+2))
    Fmat[1,3:(n+2)]<-fx(y,params)[a]
    FMaster[[a]] <- Fmat
  }
  for(a in 1:100){
    #fx(x,params)[a]
    Tmat<-matrix(0,(n+2),(n+2))
    Tmat[2,1]<-1-invlogit(params[71,a])
    Tmat[3:(n+2),1]<-invlogit(params[71,a])*recruits(y,params)[a]*h*invlogit(params[91,a] + params[92,a] * xb)
    Tmat[3:(n+2),1]<-invlogit(params[81,a])*recruits(y,params)[a]*h*invlogit(params[91,a] + params[92,a] * xb)
    Tmat[3:(n+2),3:(n+2)]<-t(outer(y,y,pxy,params))*h
    TMaster[[a]] <- Tmat
  }
  for(a in 1:100){
    IPMMaster[[a]] <- TMaster[[a]] + FMaster[[a]]
  }
  
  ################################################ Mean Lambda
  
  # Fertility matrix
  Fmat<-matrix(0,(n+2),(n+2))
  
  # Banked seeds go in top row
  Fmat[1,3:(n+2)]<-fx(y,params)
  
  # Growth/survival transition matrix
  Tmat<-matrix(0,(n+2),(n+2))
  
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  Tmat[2,1]<-1-invlogit(params[71])
  
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)   
  
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)  
  
  # Growth/survival transitions among cts sizes
  Tmat[3:(n+2),3:(n+2)]<-t(outer(y,y,pxy,params=params))*h
  
  # Put it all together
  IPMmat<-Fmat+Tmat     
  
  
  ############# Need to create a different matrix for every run I am considering (need a list of matricies?)
  
  
  
  return(list(IPMmat=IPMmat,Fmat=Fmat,Tmat=Tmat,meshpts=y, IPMMaster=IPMMaster, FMaster=FMaster, TMaster=TMaster))
}

#image(t(Tmat),main='transition kernel')
#image(t(Fmat),main='fecundity kernel')
#image(t(IPMmat), main='full kernel')



## ----------------- Function that simulates population dynamics and returns lambdaS ------------- ############
  mat <- bigmatrix(params,lower=lower,upper=upper,matsize=matsize)$IPMMaster[[2]]
  
  lambda <- vector()
  for(i in 1:100){
    mat <- bigmatrix(params,lower=lower,upper=upper,matsize=matsize)$IPMMaster[[i]]
    eig <- eigen(mat)
    lambda[i]<-Re(eig$values[1])
    
    lambda(mat)
  }
  
  
  plot(density(lambda))

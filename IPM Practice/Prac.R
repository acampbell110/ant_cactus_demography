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

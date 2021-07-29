setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Practice")
#########################################################################################################
##            This will be an IPM which only includes one ant state -- Crem.
#########################################################################################################

## ----------- Miscellany...we'll need an inverse logit functions ------------- ##
invlogit<-function(x){exp(x)/(1+exp(x))}

## ----------- Vital rate functions. Parameter indices are hard-coded and must correspond to rows of MCMC matrix ------------- ##
#GROWTH FROM SIZE X TO Y
gxy<-function(x,y,params){
  xb=pmin(pmax(x,cholla_min),cholla_max) #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  g_occ = dnorm(y,mean=params[1] + params[2]*xb,sd=params[3])
  g_vac = dnorm(y,mean=params[301] + params[302]*xb,sd=params[3])
  #g_other = dnorm(y,mean=params[201] + params[202]*xb,sd=params[3])
  #g_liom = dnorm(y,mean=params[101] + params[102]*xb,sd=params[3])
  return(list(g_occ=g_occ, g_vac=g_vac))
}
g <- list()
for(i in 1:10){
  params = cholla[,i]
  g[[i]]<-gxy(x,y,params)
}

#SURVIVAL AT SIZE X.
sx<-function(x,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  s_occ = invlogit(params[11] + params[12]*xb)
  s_vac = invlogit(params[311] + params[312]*xb)
  #s_other = invlogit(params[211] + params[212]*xb)
  #s_liom = invlogit(params[111] + params[112]*xb)
  return(list(s_occ=s_occ, s_vac=s_vac))
}
s <- list()
for(i in 1:10){
  params = cholla[,i]
  s[[i]]<-sx(x,params)
}

#SURVIVAL*GROWTH
pxy<-function(x,y,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  p_occ = sx(x,params)$s_occ * gxy(x,y,params)$g_occ
  p_vac = sx(x,params)$s_vac * gxy(x,y,params)$g_vac
  #p_other = sx(x,params)$s_other * gxy(x,y,params)$g_other
  #p_liom = sx(x,params)$s_liom * gxy(x,y,params)$g_liom
  return(list(p_occ=p_occ, p_vac=p_vac))
}
p <- list()
for(i in 1:10){
  params = cholla[,i]
  p[[i]]<-pxy(x,y,params)
}

#PRODUCTION OF 1-YO SEEDS IN THE SEED BANK FROM X-SIZED MOMS
fx<-function(x,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  p.flow<-invlogit(params[31] + params[32]*xb)    ## Probability of Reproducing
  nflow<-exp(params[21] + params[22]*xb)          ## Number of FLowers produced
  flow.surv_occ<-invlogit(params[41]) ## Proportion of Flowers survive to fruit
  flow.surv_vac<-invlogit(params[341]) ## Proportion of Flowers survive to fruit
  #flow.surv_other<-invlogit(params[241]) ## Proportion of Flowers survive to fruit
  #flow.surv_liom<-invlogit(params[141]) ## Proportion of Flowers survive to fruit
  seeds.per.fruit<-params[51]                     ## Number of Seeds per Fruit
  seed.survival<-invlogit(params[61])^2           ## Seed per Fruit Survival ---------I measured 6-month seed survival; annual survival is its square
  f_occ = p.flow*nflow*flow.surv_occ*seeds.per.fruit*seed.survival
  f_vac = p.flow*nflow*flow.surv_vac*seeds.per.fruit*seed.survival
  #f_other = p.flow*nflow*flow.surv_other*seeds.per.fruit*seed.survival
  #f_liom = p.flow*nflow*flow.surv_liom*seeds.per.fruit*seed.survival
  return(list(f_occ=f_occ, f_vac=f_vac))
}
f <- list()
for(i in 1:10){
  params = cholla[,i]
  f[[i]]<-fx(x,params)
}

#
recruits<-function(y,params){
  yb=pmin(pmax(x,cholla_min),cholla_max)
  dnorm(yb, (params[96]),params[97])
}

beta<-function(vac_rec){
  ifelse(vac_rec == TRUE, return(0), return(1))
}

#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE
transition.x<-function(x, i, j){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  occ_occ = (invlogit(params[100] + params[101]*xb))
  occ_vac = (1 - invlogit(params[100] + params[101]*xb))
  vac_occ = (invlogit(params[98] + params[99]*xb))
  vac_vac = (1 - invlogit(params[98] + params[99]*xb))
  
  #return(list(crem_crem=crem_crem, vac_vac=vac_vac, other_other=other_other, liom_liom=liom_liom,
  #            crem_vac=crem_vac, other_vac=other_vac, liom_vac=liom_vac,
  #            vac_crem=vac_crem, vac_other=vac_other, vac_liom=vac_liom))
  
  if(i == "occ" & j == "occ") return(occ_occ)
  if(i == "occ" & j == "vac") return(occ_vac)
  if(i == "vac" & j == "vac") return(vac_vac)
  if(i == "vac" & j == "occ") return(vac_occ)
}
transition.x(4, "occ","vac")


#GROWTH*SURVIVAL*ANT PROBABILITIES
ptxy<-function(x,y,params,i, j){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  vac_vac = sx(xb,params)$s_vac*gxy(xb,y,params)$g_vac*transition.x(xb,i,j) #i=vac
  vac_occ = sx(xb,params)$s_vac*gxy(xb,y,params)$g_vac*transition.x(xb,i,j) #i=vac
  occ_occ = sx(xb,params)$s_occ*gxy(xb,y,params)$g_occ*transition.x(xb,i,j) #i=crem
  occ_vac = sx(xb,params)$s_occ*gxy(xb,y,params)$g_occ*transition.x(xb,i,j)#i=crem
  if(i == "occ" & j == "occ") return(occ_occ)
  if(i == "occ" & j == "vac") return(occ_vac)
  if(i == "vac" & j == "vac") return(vac_vac)
  if(i == "vac" & j == "occ") return(vac_occ)
}
ptxy(4,5,cholla,i = "vac",j = "occ")

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
  
  ###################################################################################################
  ##                      Occ
  ###################################################################################################
  # Fertility matrix
  Fmat<-matrix(0,(2*n+2),(2*n+2))
  
  # Banked seeds go in top row
  fec1_v<-fx(y,params=params)$f_vac
  fec1_o<-fx(y,params=params)$f_occ
  Fmat[1,3:(n+2)]<-fec1_o
  Fmat[1,(n+3):(2*n+2)]<-fec1_v
  
  # Growth/survival transition matrix
  Tmat<-matrix(0,(2*n+2),(2*n+2))
  
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  Tmat[2,1]<-1-invlogit(params[71])
  
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  beta1_o<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
  beta1_v<- invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
  Tmat[3:(n+2),1]<-beta1_v
  Tmat[3:(n+2),2]<-beta1_o

  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  #Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)  
  beta2_o<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
  beta2_v<- invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
  Tmat[(n+3):(2*n+2),1]<-beta2_v
  Tmat[(n+3):(2*n+2),2]<-beta2_o
  # Growth/survival transitions among cts sizes
  vac_vac<-t(outer(y,y,ptxy,params=params,"vac","vac"))*h
  vac_occ<-t(outer(y,y,ptxy,params=params,"vac","occ"))*h
  occ_vac<-t(outer(y,y,ptxy,params=params,"occ","vac"))*h
  occ_occ<-t(outer(y,y,ptxy,params=params,"occ","occ"))*h
  Tmat[3:(n+2),3:(n+2)]<- vac_vac## Top left 
  Tmat[3:(n+2),(n+3):(2*n+2)]<-occ_vac ## Top Right 
  Tmat[(n+3):(2*n+2),3:(n+2)]<- occ_occ## Bottom Right 
  Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- vac_occ## Bottom Right 
  
  # Put it all together
  IPMmat<-Fmat+Tmat  
  
  return(list(IPMmat=IPMmat))
}
#image(y,y,t(Tmat),main='fecundity kernel')
#image(y,y,t(Fmat),main='fecundity kernel')

bigmatrix(cholla,lower,upper,matsize)$IPMmat

## ----------------- Function that simulates population dynamics and returns lambdaS ------------- ############
lambda.fun(cholla,iter,matsize,extra.grid = 2,floor.extend = 1,ceiling.extend = 4)

big_list <- list()
lambda <- vector()
stable <- list()

for(i in 1:100){
  params = cholla[,i]
  big_list[[i]]<-bigmatrix(cholla, lower, upper, matsize)$IPMmat
  mat <- big_list[[i]]
  eig <- eigen(mat)
  lambda[i]<-Re(eig$values[1])
  
  stable[[i]] <- stable.stage(mat)
}

hist(lambda)

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")

#### Lambda dist
png("two_species_models.png")
plot(density(lambda), col = "black")
dev.off()



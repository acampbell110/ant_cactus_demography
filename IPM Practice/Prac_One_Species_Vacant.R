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
  g_crem = dnorm(y,mean=params[1] + params[2]*xb,sd=params[3])
  g_vac = dnorm(y,mean=params[301] + params[302]*xb,sd=params[3])
  g_other = dnorm(y,mean=params[201] + params[202]*xb,sd=params[3])
  g_liom = dnorm(y,mean=params[101] + params[102]*xb,sd=params[3])
  return(list(g_crem=g_crem, g_vac=g_vac, g_other=g_other, g_liom=g_liom))
}
g <- list()
for(i in 1:10){
  params = cholla[,i]
  g[[i]]<-gxy(x,y,params)
}

#SURVIVAL AT SIZE X.
sx<-function(x,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  s_crem = invlogit(params[11] + params[12]*xb)
  s_vac = invlogit(params[311] + params[312]*xb)
  s_other = invlogit(params[211] + params[212]*xb)
  s_liom = invlogit(params[111] + params[112]*xb)
  return(list(s_crem=s_crem, s_vac=s_vac, s_other=s_other, s_liom=s_liom))
}
s <- list()
for(i in 1:10){
  params = cholla[,i]
  s[[i]]<-sx(x,params)
}

#SURVIVAL*GROWTH
pxy<-function(x,y,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  p_crem = sx(x,params)$s_crem * gxy(x,y,params)$g_crem
  p_vac = sx(x,params)$s_vac * gxy(x,y,params)$g_vac
  p_other = sx(x,params)$s_other * gxy(x,y,params)$g_other
  p_liom = sx(x,params)$s_liom * gxy(x,y,params)$g_liom
  return(list(p_crem=p_crem, p_vac=p_vac, p_other=p_other, p_liom=p_liom))
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
  flow.surv_crem<-invlogit(params[41]) ## Proportion of Flowers survive to fruit
  flow.surv_vac<-invlogit(params[341]) ## Proportion of Flowers survive to fruit
  flow.surv_other<-invlogit(params[241]) ## Proportion of Flowers survive to fruit
  flow.surv_liom<-invlogit(params[141]) ## Proportion of Flowers survive to fruit
  seeds.per.fruit<-params[51]                     ## Number of Seeds per Fruit
  seed.survival<-invlogit(params[61])^2           ## Seed per Fruit Survival ---------I measured 6-month seed survival; annual survival is its square
  f_crem = p.flow*nflow*flow.surv_crem*seeds.per.fruit*seed.survival
  f_vac = p.flow*nflow*flow.surv_vac*seeds.per.fruit*seed.survival
  f_other = p.flow*nflow*flow.surv_other*seeds.per.fruit*seed.survival
  f_liom = p.flow*nflow*flow.surv_liom*seeds.per.fruit*seed.survival
  return(list(f_crem=f_crem, f_vac=f_vac, f_other=f_other, f_liom=f_liom))
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
  crem_crem = (invlogit(params[100] + params[101]*xb))
  crem_vac = (1 - invlogit(params[100] + params[101]*xb))
  vac_crem = (invlogit(params[98] + params[99]*xb))
  vac_vac = (1 - invlogit(params[98] + params[99]*xb))
  other_other = (invlogit(params[2100] + params[2101]*xb))
  other_vac = (1 - invlogit(params[100] + params[101]*xb))
  vac_other = (invlogit(params[298] + params[299]*xb))
  liom_liom = (invlogit(params[1100] + params[1101]*xb))
  liom_vac = (1 - invlogit(params[1100] + params[1101]*xb))
  vac_liom = (invlogit(params[198] + params[199]*xb))
  
  #return(list(crem_crem=crem_crem, vac_vac=vac_vac, other_other=other_other, liom_liom=liom_liom,
  #            crem_vac=crem_vac, other_vac=other_vac, liom_vac=liom_vac,
  #            vac_crem=vac_crem, vac_other=vac_other, vac_liom=vac_liom))
  
  if(i == "crem" & j == "crem") return(crem_crem)
  if(i == "crem" & j == "vac") return(crem_vac)
  if(i == "other" & j == "other") return(other_other)
  if(i == "other" & j == "vac") return(other_vac)
  if(i == "liom" & j == "liom") return(liom_liom)
  if(i == "liom" & j == "vac") return(liom_vac)
  if(i == "vac" & j == "vac") return(vac_vac)
  if(i == "vac" & j == "crem") return(vac_crem)
  if(i == "vac" & j == "other") return(vac_other)
  if(i == "vac" & j == "liom") return(vac_liom)
}
transition.x(4, "crem","vac")


#GROWTH*SURVIVAL*ANT PROBABILITIES
ptxy<-function(x,y,params,i, j){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  vac_vac = sx(xb,params)$s_vac*gxy(xb,y,params)$g_vac*transition.x(xb,i,j) #i=vac
  vac_crem = sx(xb,params)$s_vac*gxy(xb,y,params)$g_vac*transition.x(xb,i,j) #i=vac
  vac_other = sx(xb,params)$s_vac*gxy(xb,y,params)$g_vac*transition.x(xb,i,j) #i=vac
  vac_liom = sx(xb,params)$s_vac*gxy(xb,y,params)$g_vac*transition.x(xb,i,j) #i=vac
  crem_crem = sx(xb,params)$s_crem*gxy(xb,y,params)$g_crem*transition.x(xb,i,j) #i=crem
  crem_vac = sx(xb,params)$s_crem*gxy(xb,y,params)$g_crem*transition.x(xb,i,j)#i=crem
  other_other = sx(xb,params)$s_other*gxy(xb,y,params)$g_other*transition.x(xb,i,j)#i=other
  other_vac = sx(xb,params)$s_other*gxy(xb,y,params)$g_other*transition.x(xb,i,j) #i=other
  liom_liom = sx(xb,params)$s_liom*gxy(xb,y,params)$g_liom*transition.x(xb,i,j)#i=liom
  liom_vac = sx(xb,params)$s_liom*gxy(xb,y,params)$g_liom*transition.x(xb,i,j)#i=liom
  #return(list(crem_crem=crem_crem, vac_vac=vac_vac, other_other=other_other, liom_liom=liom_liom,
  #            crem_vac=crem_vac, other_vac=other_vac, liom_vac=liom_vac,
  #            vac_crem=vac_crem, vac_other=vac_other, vac_liom=vac_liom))
  if(i == "crem" & j == "crem") return(crem_crem)
  if(i == "crem" & j == "vac") return(crem_vac)
  if(i == "other" & j == "other") return(other_other)
  if(i == "other" & j == "vac") return(other_vac)
  if(i == "liom" & j == "liom") return(liom_liom)
  if(i == "liom" & j == "vac") return(liom_vac)
  if(i == "vac" & j == "vac") return(vac_vac)
  if(i == "vac" & j == "crem") return(vac_crem)
  if(i == "vac" & j == "other") return(vac_other)
  if(i == "vac" & j == "liom") return(vac_liom)
}
ptxy(4,5,cholla,i = "vac",j = "other")

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
  ##                      CREM
  ###################################################################################################
  # Fertility matrix
  Fmat_c<-matrix(0,(2*n+2),(2*n+2))
  
  # Banked seeds go in top row
  fec1_v<-fx(y,params=params)$f_vac
  fec1_c<-fx(y,params=params)$f_crem
  Fmat_c[1,3:(n+2)]<-fec1_c
  Fmat_c[1,(n+3):(2*n+2)]<-fec1_v
  
  # Growth/survival transition matrix
  Tmat_c<-matrix(0,(2*n+2),(2*n+2))
  
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  Tmat_c[2,1]<-1-invlogit(params[71])
  
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  beta1_c<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
  beta1_v<- invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
  Tmat_c[3:(n+2),1]<-beta1_v
  Tmat_c[3:(n+2),2]<-beta1_c

  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  #Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)  
  beta2_c<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
  beta2_v<- invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
  Tmat_c[(n+3):(2*n+2),1]<-beta2_v
  Tmat_c[(n+3):(2*n+2),2]<-beta2_c
  # Growth/survival transitions among cts sizes
  vac_vac<-t(outer(y,y,ptxy,params=params,"vac","vac"))*h
  vac_crem<-t(outer(y,y,ptxy,params=params,"vac","crem"))*h
  crem_vac<-t(outer(y,y,ptxy,params=params,"crem","vac"))*h
  crem_crem<-t(outer(y,y,ptxy,params=params,"crem","crem"))*h
  Tmat_c[3:(n+2),3:(n+2)]<- vac_vac## Top left 
  Tmat_c[3:(n+2),(n+3):(2*n+2)]<-crem_vac ## Top Right 
  Tmat_c[(n+3):(2*n+2),3:(n+2)]<- crem_crem## Bottom Right 
  Tmat_c[(n+3):(2*n+2),(n+3):(2*n+2)]<- vac_crem## Bottom Right 
  
  # Put it all together
  IPMmat_c<-Fmat_c+Tmat_c  
  
  ###################################################################################################
  ##                      LIOM
  ###################################################################################################
  # Fertility matrix
  Fmat_l<-matrix(0,(2*n+2),(2*n+2))
  
  # Banked seeds go in top row
  fec1_v<-fx(y,params=params)$f_vac
  fec1_l<-fx(y,params=params)$f_liom
  Fmat_l[1,3:(n+2)]<-fec1_l
  Fmat_l[1,(n+3):(2*n+2)]<-fec1_v
  
  # Growth/survival transition matrix
  Tmat_l<-matrix(0,(2*n+2),(2*n+2))
  
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  Tmat_l[2,1]<-1-invlogit(params[71])
  
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  beta1_l<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
  beta1_v<- invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
  Tmat_l[3:(n+2),1]<-beta1_v
  Tmat_l[3:(n+2),2]<-beta1_l
  
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  #Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)  
  beta2_l<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
  beta2_v<- invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
  Tmat_l[(n+3):(2*n+2),1]<-beta2_v
  Tmat_l[(n+3):(2*n+2),2]<-beta2_l
  # Growth/survival transitions among cts sizes
  vac_vac<-t(outer(y,y,ptxy,params=params,"vac","vac"))*h
  vac_liom<-t(outer(y,y,ptxy,params=params,"vac","liom"))*h
  liom_vac<-t(outer(y,y,ptxy,params=params,"liom","vac"))*h
  liom_liom<-t(outer(y,y,ptxy,params=params,"liom","liom"))*h
  Tmat_l[3:(n+2),3:(n+2)]<- vac_vac## Top left 
  Tmat_l[3:(n+2),(n+3):(2*n+2)]<-liom_vac ## Top Right 
  Tmat_l[(n+3):(2*n+2),3:(n+2)]<- liom_liom## Bottom Right 
  Tmat_l[(n+3):(2*n+2),(n+3):(2*n+2)]<- vac_liom## Bottom Right 
  
  # Put it all together
  IPMmat_l<-Fmat_l+Tmat_l    
  
  
  ###################################################################################################
  ##                      OTHER
  ###################################################################################################
  # Fertility matrix
  Fmat_o<-matrix(0,(2*n+2),(2*n+2))
  
  # Banked seeds go in top row
  fec1_v<-fx(y,params=params)$f_vac
  fec1_o<-fx(y,params=params)$f_other
  Fmat_o[1,3:(n+2)]<-fec1_o
  Fmat_o[1,(n+3):(2*n+2)]<-fec1_v
  
  # Growth/survival transition matrix
  Tmat_o<-matrix(0,(2*n+2),(2*n+2))
  
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  Tmat_o[2,1]<-1-invlogit(params[71])
  
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  beta1_o<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
  beta1_v<- invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
  Tmat_o[3:(n+2),1]<-beta1_v
  Tmat_o[3:(n+2),2]<-beta1_o
  
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  #Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)  
  beta2_o<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
  beta2_v<- invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
  Tmat_o[(n+3):(2*n+2),1]<-beta2_v
  Tmat_o[(n+3):(2*n+2),2]<-beta2_o
  # Growth/survival transitions among cts sizes
  vac_vac<-t(outer(y,y,ptxy,params=params,"vac","vac"))*h
  vac_other<-t(outer(y,y,ptxy,params=params,"vac","other"))*h
  other_vac<-t(outer(y,y,ptxy,params=params,"other","vac"))*h
  other_other<-t(outer(y,y,ptxy,params=params,"other","other"))*h
  Tmat_o[3:(n+2),3:(n+2)]<- vac_vac## Top left 
  Tmat_o[3:(n+2),(n+3):(2*n+2)]<-other_vac ## Top Right 
  Tmat_o[(n+3):(2*n+2),3:(n+2)]<- other_other## Bottom Right 
  Tmat_o[(n+3):(2*n+2),(n+3):(2*n+2)]<- vac_other## Bottom Right 
  
  # Put it all together
  IPMmat_o<-Fmat_o+Tmat_o
  
  return(list(IPMmat_c=IPMmat_c, IPMmat_o=IPMmat_o, IPMmat_l=IPMmat_l))
}
#image(y,y,t(Tmat),main='fecundity kernel')
#image(y,y,t(Fmat),main='fecundity kernel')

bigmatrix(cholla,lower,upper,matsize)$IPMmat_o

## ----------------- Function that simulates population dynamics and returns lambdaS ------------- ############
lambda.fun(cholla,iter,matsize,extra.grid = 2,floor.extend = 1,ceiling.extend = 4)

big_list_c <- list()
lambda_c <- vector()
stable_c <- list()
big_list_o <- list()
lambda_o <- vector()
stable_o <- list()
big_list_l <- list()
lambda_l <- vector()
stable_l <- list()
for(i in 1:100){
  params = cholla[,i]
  big_list_c[[i]]<-bigmatrix(cholla, lower, upper, matsize)$IPMmat_c
  mat_c <- big_list_c[[i]]
  eig_c <- eigen(mat_c)
  lambda_c[i]<-Re(eig_c$values[1])
  
  stable_c[[i]] <- stable.stage(mat_c)
}
for(i in 1:100){  
  params = cholla[,i]
  big_list_o[[i]]<-bigmatrix(cholla, lower, upper, matsize)$IPMmat_o
  mat_o <- big_list_o[[i]]
  eig_o <- eigen(mat_o)
  lambda_o[i]<-Re(eig_o$values[1])
  
  stable_o[[i]] <- stable.stage(mat_o)
}
for(i in 1:100){  
  params = cholla[,i]
  big_list_l[[i]]<-bigmatrix(cholla, lower, upper, matsize)$IPMmat_l
  mat_l <- big_list_l[[i]]
  eig_l <- eigen(mat_l)
  lambda_l[i]<-Re(eig_l$values[1])
  
  stable_l[[i]] <- stable.stage(mat_l)
}
hist(lambda_c)
plot(density(lambda_c))

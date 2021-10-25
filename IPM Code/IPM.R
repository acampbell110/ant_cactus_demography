#########################################################################################################
##            This will be an IPM which allows you to choose how many ants are present
#########################################################################################################
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code")
source("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code/Params.R")
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
  return(list(g_crem=g_crem, g_liom=g_liom, g_other=g_other, g_vac=g_vac))
}


#SURVIVAL AT SIZE X.
sx<-function(x,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  s_crem = invlogit(params[11] + params[12]*xb)
  s_vac = invlogit(params[311] + params[312]*xb)
  s_other = invlogit(params[211] + params[212]*xb)
  s_liom = invlogit(params[111] + params[112]*xb)
  return(list(s_crem=s_crem, s_liom=s_liom, s_other=s_other, s_vac=s_vac))
}


#SURVIVAL*GROWTH
pxy<-function(x,y,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  p_crem = sx(x,params)$s_crem * gxy(x,y,params)$g_crem
  p_vac = sx(x,params)$s_vac * gxy(x,y,params)$g_vac
  p_other = sx(x,params)$s_other * gxy(x,y,params)$g_other
  p_liom = sx(x,params)$s_liom * gxy(x,y,params)$g_liom
  return(list(p_crem=p_crem, p_liom=p_liom, p_other=p_other, p_vac=p_vac))
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
  return(list(f_crem=f_crem, f_liom=f_liom, f_other=f_other, f_vac=f_vac))
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
transition.x<-function(x, i, j, num_ants, params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  ## IF only one ant present, there will be no transition values
  ## Now for if there are two ants aka length(ants) = 2
  if(num_ants == 2){
    occ_occ = (invlogit(params[108] + params[109]*xb))
    occ_vac = (1 - invlogit(params[108] + params[109]*xb))
    vac_occ = (invlogit(params[98] + params[99]*xb))
    vac_vac = (1 - invlogit(params[98] + params[99]*xb))
    ## Now return
    if((i == "crem" | i == "liom" | i == "other") & (j == "crem" | j == "liom" | j == "other")) return(occ_occ)
    if((i == "crem" | i == "liom" | i == "other") & j == "vac") return(occ_vac)
    if(i == "vac" & j == "vac") return(vac_vac)
    if(i == "vac" & (j == "crem" | j == "liom" | j == "other")) return(vac_occ)
  }
  ## Now if there are four ants
  if(num_ants == 4){
    cc = multi_data$beta.1.1 + multi_data$beta.2.1*x
    co 
    cl
    cv
  }
}
  
transition.x(4,i = "vac",j = "vac",num_ants = 4,cholla)
  



#GROWTH*SURVIVAL*ANT PROBABILITIES
ptxy<-function(x,y,params,i, j, ant){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  
  
  ## Crem
  vac_vac = sx(xb,params)$s_vac*gxy(xb,y,params)$g_vac*transition.x(xb,i,j) #i=vac
  vac_crem = sx(xb,params)$s_vac*gxy(xb,y,params)$g_vac*transition.x(xb,i,j) #i=vac
  crem_crem = sx(xb,params)$s_crem*gxy(xb,y,params)$g_crem*transition.x(xb,i,j) #i=crem
  crem_vac = sx(xb,params)$s_crem*gxy(xb,y,params)$g_crem*transition.x(xb,i,j) #i=crem
  if(i == "occ" & j == "occ" & ant == "crem") return(crem_crem)
  if(i == "occ" & j == "vac" & ant == "crem") return(crem_vac)
  if(i == "vac" & j == "vac" & ant == "crem") return(vac_vac)
  if(i == "vac" & j == "occ" & ant == "crem") return(vac_crem)
  ## Liom
  vac_vac = sx(xb,params)$s_vac*gxy(xb,y,params)$g_vac*transition.x(xb,i,j) #i=vac
  vac_liom = sx(xb,params)$s_vac*gxy(xb,y,params)$g_vac*transition.x(xb,i,j) #i=vac
  liom_liom = sx(xb,params)$s_liom*gxy(xb,y,params)$g_liom*transition.x(xb,i,j) #i=liom
  liom_vac = sx(xb,params)$s_liom*gxy(xb,y,params)$g_liom*transition.x(xb,i,j) #i=liom
  if(i == "occ" & j == "occ" & ant == "liom") return(liom_liom)
  if(i == "occ" & j == "vac" & ant == "liom") return(liom_vac)
  if(i == "vac" & j == "vac" & ant == "liom") return(vac_vac)
  if(i == "vac" & j == "occ" & ant == "liom") return(vac_liom)
  ## Other
  vac_vac = sx(xb,params)$s_vac*gxy(xb,y,params)$g_vac*transition.x(xb,i,j) #i=vac
  vac_other = sx(xb,params)$s_vac*gxy(xb,y,params)$g_vac*transition.x(xb,i,j) #i=vac
  other_other = sx(xb,params)$s_other*gxy(xb,y,params)$g_other*transition.x(xb,i,j) #i=other
  other_vac = sx(xb,params)$s_other*gxy(xb,y,params)$g_other*transition.x(xb,i,j) #i=other
  if(i == "occ" & j == "occ" & ant == "other") return(other_other)
  if(i == "occ" & j == "vac" & ant == "other") return(other_vac)
  if(i == "vac" & j == "vac" & ant == "other") return(vac_vac)
  if(i == "vac" & j == "occ" & ant == "other") return(vac_other)
}
ptxy(4,5,cholla,i = "occ",j = "occ", ant = "other")

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
  
  if(ants == c("vacant","crem")){
    
  }
  if(ants == c("vacant","liom")){
    
  }
  if(ants == c("vacant","other")){
    
  }
  ## Now if there are four ants
  if(ants == c("vacant","crem","liom","other")){
    
  }
  
  
  
  ###################################################################################################
  ##                      Crem
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
  vac_vac<-t(outer(y,y,ptxy,params=params,"vac","vac","crem"))*h
  vac_crem<-t(outer(y,y,ptxy,params=params,"vac","occ","crem"))*h
  crem_vac<-t(outer(y,y,ptxy,params=params,"occ","vac","crem"))*h
  crem_crem<-t(outer(y,y,ptxy,params=params,"occ","occ","crem"))*h
  Tmat_c[3:(n+2),3:(n+2)]<- vac_vac## Top left 
  Tmat_c[3:(n+2),(n+3):(2*n+2)]<-crem_vac ## Top Right 
  Tmat_c[(n+3):(2*n+2),3:(n+2)]<- crem_crem## Bottom Right 
  Tmat_c[(n+3):(2*n+2),(n+3):(2*n+2)]<- vac_crem## Bottom Right 
  
  # Put it all together
  IPMmat_c<-Fmat_c+Tmat_c  
  
  ###################################################################################################
  ##                      Liom
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
  vac_vac<-t(outer(y,y,ptxy,params=params,"vac","vac","liom"))*h
  vac_liom<-t(outer(y,y,ptxy,params=params,"vac","occ","liom"))*h
  liom_vac<-t(outer(y,y,ptxy,params=params,"occ","vac","liom"))*h
  liom_liom<-t(outer(y,y,ptxy,params=params,"occ","occ","liom"))*h
  Tmat_l[3:(n+2),3:(n+2)]<- vac_vac## Top left 
  Tmat_l[3:(n+2),(n+3):(2*n+2)]<-liom_vac ## Top Right 
  Tmat_l[(n+3):(2*n+2),3:(n+2)]<- liom_liom## Bottom Right 
  Tmat_l[(n+3):(2*n+2),(n+3):(2*n+2)]<- vac_liom## Bottom Right 
  
  # Put it all together
  IPMmat_l<-Fmat_l+Tmat_l
  
  ###################################################################################################
  ##                      Other
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
  vac_vac<-t(outer(y,y,ptxy,params=params,"vac","vac","other"))*h
  vac_other<-t(outer(y,y,ptxy,params=params,"vac","occ","other"))*h
  other_vac<-t(outer(y,y,ptxy,params=params,"occ","vac","other"))*h
  other_other<-t(outer(y,y,ptxy,params=params,"occ","occ","other"))*h
  Tmat_o[3:(n+2),3:(n+2)]<- vac_vac## Top left 
  Tmat_o[3:(n+2),(n+3):(2*n+2)]<-other_vac ## Top Right 
  Tmat_o[(n+3):(2*n+2),3:(n+2)]<- other_other## Bottom Right 
  Tmat_o[(n+3):(2*n+2),(n+3):(2*n+2)]<- vac_other## Bottom Right 
  
  # Put it all together
  IPMmat_o<-Fmat_o+Tmat_o
  
  return(list(IPMmat_c=IPMmat_c, IPMmat_l=IPMmat_l, IPMmat_o=IPMmat_o))
}
bigmatrix<-function(params,lower,upper,matsize,ants){  
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
  ##                      Crem
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
  vac_vac<-t(outer(y,y,ptxy,params=params,"vac","vac","crem"))*h
  vac_crem<-t(outer(y,y,ptxy,params=params,"vac","occ","crem"))*h
  crem_vac<-t(outer(y,y,ptxy,params=params,"occ","vac","crem"))*h
  crem_crem<-t(outer(y,y,ptxy,params=params,"occ","occ","crem"))*h
  Tmat_c[3:(n+2),3:(n+2)]<- vac_vac## Top left 
  Tmat_c[3:(n+2),(n+3):(2*n+2)]<-crem_vac ## Top Right 
  Tmat_c[(n+3):(2*n+2),3:(n+2)]<- crem_crem## Bottom Right 
  Tmat_c[(n+3):(2*n+2),(n+3):(2*n+2)]<- vac_crem## Bottom Right 
  
  # Put it all together
  IPMmat_c<-Fmat_c+Tmat_c  
  
  ###################################################################################################
  ##                      Liom
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
  vac_vac<-t(outer(y,y,ptxy,params=params,"vac","vac","liom"))*h
  vac_liom<-t(outer(y,y,ptxy,params=params,"vac","occ","liom"))*h
  liom_vac<-t(outer(y,y,ptxy,params=params,"occ","vac","liom"))*h
  liom_liom<-t(outer(y,y,ptxy,params=params,"occ","occ","liom"))*h
  Tmat_l[3:(n+2),3:(n+2)]<- vac_vac## Top left 
  Tmat_l[3:(n+2),(n+3):(2*n+2)]<-liom_vac ## Top Right 
  Tmat_l[(n+3):(2*n+2),3:(n+2)]<- liom_liom## Bottom Right 
  Tmat_l[(n+3):(2*n+2),(n+3):(2*n+2)]<- vac_liom## Bottom Right 
  
  # Put it all together
  IPMmat_l<-Fmat_l+Tmat_l
  
  ###################################################################################################
  ##                      Other
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
  vac_vac<-t(outer(y,y,ptxy,params=params,"vac","vac","other"))*h
  vac_other<-t(outer(y,y,ptxy,params=params,"vac","occ","other"))*h
  other_vac<-t(outer(y,y,ptxy,params=params,"occ","vac","other"))*h
  other_other<-t(outer(y,y,ptxy,params=params,"occ","occ","other"))*h
  Tmat_o[3:(n+2),3:(n+2)]<- vac_vac## Top left 
  Tmat_o[3:(n+2),(n+3):(2*n+2)]<-other_vac ## Top Right 
  Tmat_o[(n+3):(2*n+2),3:(n+2)]<- other_other## Bottom Right 
  Tmat_o[(n+3):(2*n+2),(n+3):(2*n+2)]<- vac_other## Bottom Right 
  
  # Put it all together
  IPMmat_o<-Fmat_o+Tmat_o
  
  return(list(IPMmat_c=IPMmat_c, IPMmat_l=IPMmat_l, IPMmat_o=IPMmat_o))
}
#image(y,y,t(Tmat),main='fecundity kernel')
#image(y,y,t(Fmat),main='fecundity kernel')

bigmatrix(cholla,lower,upper,matsize)$IPMmat

## ----------------- Function that simulates population dynamics and returns lambdaS ------------- ############




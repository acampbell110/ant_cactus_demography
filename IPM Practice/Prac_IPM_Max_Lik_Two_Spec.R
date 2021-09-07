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
  g_crem = dnorm(y,mean=int_grow+crem_grow + (vol_grow+crem_int_grow)*xb,sd=sd_grow)
  g_vac = dnorm(y,mean=int_grow+vac_grow + (vol_grow+vac_int_grow)*xb,sd=sd_grow)
  g_other = dnorm(y,mean=int_grow + (vol_grow)*xb ,sd=sd_grow)
  g_liom = dnorm(y,mean=int_grow+liom_grow + (vol_grow+liom_int_grow)*xb,sd=sd_grow)
  return(list(g_crem=g_crem, g_liom=g_liom, g_other=g_other, g_vac=g_vac))
}
gxy(x,y,cholla)

#SURVIVAL AT SIZE X.
sx<-function(x,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  s_crem = invlogit(int_surv+crem_surv + (vol_surv+crem_int_surv)*xb)
  s_vac = invlogit(int_surv+vac_surv + (vol_surv+vac_int_surv)*xb)
  s_other = invlogit(int_surv+ (vol_surv)*xb)
  s_liom = invlogit(int_surv+liom_surv + (vol_surv +liom_int_surv)*xb)
  return(list(s_crem=s_crem, s_liom=s_liom, s_other=s_other, s_vac=s_vac))
}
sx(x,params)

#SURVIVAL*GROWTH
pxy<-function(x,y,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  p_crem = sx(x,params)$s_crem * gxy(x,y,params)$g_crem
  p_vac = sx(x,params)$s_vac * gxy(x,y,params)$g_vac
  p_other = sx(x,params)$s_other * gxy(x,y,params)$g_other
  p_liom = sx(x,params)$s_liom * gxy(x,y,params)$g_liom
  return(list(p_crem=p_crem, p_liom=p_liom, p_other=p_other, p_vac=p_vac))
}
pxy(x,y,params)

#PRODUCTION OF 1-YO SEEDS IN THE SEED BANK FROM X-SIZED MOMS
fx<-function(x,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  p.flow<-invlogit(int_repro + vol_repro*xb)    ## Probability of Reproducing (repro)
  nflow<-exp(int_flower + vol_flower*xb)          ## Number of FLowers produced (tot Flow)
  flow.surv_crem<-invlogit(int_viab + crem_viab) ## Proportion of Flowers survive to fruit (viability)
  flow.surv_vac<-invlogit(int_viab + vac_viab) ## Proportion of Flowers survive to fruit
  flow.surv_other<-invlogit(int_viab) ## Proportion of Flowers survive to fruit
  flow.surv_liom<-invlogit(int_viab + liom_viab) ## Proportion of Flowers survive to fruit
  seeds.per.fruit_occ<-int_seed + ant_seed                     ## Number of Seeds per Fruit
  seeds.per.fruit<-int_seed                    ## Number of Seeds per Fruit
  seed.survival<-invlogit(params[61])^2           ## Seed per Fruit Survival ---------I measured 6-month seed survival; annual survival is its square
  f_crem = p.flow*nflow*flow.surv_crem*seeds.per.fruit_occ*seed.survival
  f_vac = p.flow*nflow*flow.surv_vac*seeds.per.fruit*seed.survival
  f_other = p.flow*nflow*flow.surv_other*seeds.per.fruit_occ*seed.survival
  f_liom = p.flow*nflow*flow.surv_liom*seeds.per.fruit_occ*seed.survival
  return(list(f_crem=f_crem, f_liom=f_liom, f_other=f_other, f_vac=f_vac))
}
fx(x,params)

#
recruits<-function(y,params){
  yb=pmin(pmax(y,cholla_min),cholla_max)
  dnorm(yb, mean = int_rec,sd = sd_rec)
}
recruits(y,params)

beta<-function(vac_rec){
  ifelse(vac_rec == TRUE, return(0), return(1))
}
beta(vac_rec)

#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE
transition.x<-function(x, i, j){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  ## From other to 
  p_o_o <- exp(other_other + vol_other * xb)/(1+exp(other_other + vol_other * xb))
  p_o_l <- exp(other_liom + vol_other * xb)/(1+exp(other_liom + vol_other * xb))
  p_o_c <- exp(other_crem + vol_other * xb)/(1+exp(other_crem + vol_other * xb))
  p_o_v <- vector()
  for(i in 1:length(xb)){
    p_o_v[i] <- 1 - sum(p_o_o[i],p_o_l[i],p_o_c[i])
  }
  ## From Liom to ...
  p_l_o <- exp(liom_other + vol_liom * xb + liom_vol_other* xb)/(1+exp(liom_other + vol_liom * xb + liom_vol_other* xb))
  p_l_l <- exp(liom_liom + vol_liom * xb + liom_vol_liom* xb)/(1+exp(liom_liom + vol_liom * xb + liom_vol_liom* xb))
  p_l_c <- exp(liom_crem + vol_liom * xb + liom_vol_crem* xb)/(1+exp(liom_crem + vol_liom * xb + liom_vol_crem* xb))
  p_l_v <- vector()
  for(i in 1:length(xb)){
    p_l_v[i] <- 1 - sum(p_l_o[i],p_l_l[i],p_l_c[i])
  }
  ## From Crem to ...
  p_c_o <- exp(crem_other + vol_crem * xb + crem_vol_other* xb)/(1+exp(crem_other + vol_crem * xb + crem_vol_other* xb))
  p_c_l <- exp(crem_liom + vol_crem * xb + crem_vol_liom* xb)/(1+exp(crem_liom + vol_crem * xb + crem_vol_liom* xb))
  p_c_c <- exp(crem_crem + vol_crem * xb + crem_vol_crem* xb)/(1+exp(crem_crem + vol_crem * xb + crem_vol_crem* xb))
  p_c_v <- vector()
  for(i in 1:length(xb)){
    p_c_v[i] <- 1 - sum(p_c_o[i],p_c_l[i],p_c_c[i])
  }
  ## From Vac to ...
  p_v_o <- exp(vac_other + vac_vol_other* xb)/(1+exp(vac_other + vac_vol_other* xb))
  p_v_l <- exp(vac_liom + vac_vol_liom* xb)/(1+exp(vac_liom + vac_vol_liom* xb))
  p_v_c <- exp(vac_crem + vac_vol_crem* xb)/(1+exp(vac_crem + vac_vol_crem* xb))
  p_v_v <- vector()
  for(i in 1:length(xb)){
    p_v_v[i] <- 1 - sum(p_v_o[i],p_v_l[i],p_v_c[i])
  }
  
  if(i == "other" & j == "other") return(p_o_o)
  if(i == "other" & j == "crem") return(p_o_c)
  if(i == "other" & j == "liom") return(p_o_l)
  if(i == "other" & j == "vac") return(p_o_v)
  if(i == "crem" & j == "other") return(p_c_o)
  if(i == "crem" & j == "crem") return(p_c_c)
  if(i == "crem" & j == "liom") return(p_c_l)
  if(i == "crem" & j == "vac") return(p_c_v)
  if(i == "liom" & j == "other") return(p_l_o)
  if(i == "liom" & j == "crem") return(p_l_c)
  if(i == "liom" & j == "liom") return(p_l_l)
  if(i == "liom" & j == "vac") return(p_l_v)
  if(i == "vac" & j == "other") return(p_v_o)
  if(i == "vac" & j == "crem") return(p_v_c)
  if(i == "vac" & j == "liom") return(p_v_l)
  if(i == "vac" & j == "vac") return(p_v_v)
  return(list(p_c_o=p_c_o , p_c_l=p_c_l , p_c_c=p_c_c , p_c_v=p_c_v , p_l_o=p_l_o , p_l_c=p_l_c , p_l_l=p_l_l , p_l_v=p_l_v, p_o_o=p_o_o , p_o_l=p_o_l , p_o_c=p_o_c , p_o_v=p_o_v , p_v_o=p_v_o , p_v_l=p_v_l , p_v_c=p_v_c , p_v_v=p_v_v))
}
transition.x(4, "liom","vac")


#GROWTH*SURVIVAL*ANT PROBABILITIES
ptxy<-function(x,y,params,i, j){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  ## Crem
  crem_other = sx(xb,params)$s_crem*gxy(xb,y,params)$g_crem*transition.x(xb,"crem","other")$p_c_o #i=vac
  crem_liom = sx(xb,params)$s_crem*gxy(xb,y,params)$g_crem*transition.x(xb,i,j)$p_c_l #i=vac
  crem_crem = sx(xb,params)$s_crem*gxy(xb,y,params)$g_crem*transition.x(xb,i,j)$p_c_c #i=crem
  crem_vac = sx(xb,params)$s_crem*gxy(xb,y,params)$g_crem*transition.x(xb,i,j)$p_c_v #i=crem
  if(i == "crem" & j == "other") return(crem_other)
  if(i == "crem" & j == "crem") return(crem_crem)
  if(i == "crem" & j == "liom") return(crem_liom)
  if(i == "crem" & j == "vac") return(crem_vac)
  ## Liom
  liom_other = sx(xb,params)$s_liom*gxy(xb,y,params)$g_liom*transition.x(xb,i,j)$p_l_o #i=vac
  liom_crem = sx(xb,params)$s_liom*gxy(xb,y,params)$g_liom*transition.x(xb,i,j)$p_l_c #i=vac
  liom_liom = sx(xb,params)$s_liom*gxy(xb,y,params)$g_liom*transition.x(xb,i,j)$p_l_l #i=liom
  liom_vac = sx(xb,params)$s_liom*gxy(xb,y,params)$g_liom*transition.x(xb,i,j)$p_l_v #i=liom
  if(i == "liom" & j == "other") return(liom_other)
  if(i == "liom" & j == "crem") return(liom_crem)
  if(i == "liom" & j == "liom") return(liom_other)
  if(i == "liom" & j == "vac") return(liom_vac)
  ## Other
  other_crem = sx(xb,params)$s_other*gxy(xb,y,params)$g_other*transition.x(xb,i,j)$p_o_c #i=other
  other_liom = sx(xb,params)$s_other*gxy(xb,y,params)$g_other*transition.x(xb,i,j)$p_o_l #i=other
  other_other = sx(xb,params)$s_other*gxy(xb,y,params)$g_other*transition.x(xb,i,j)$p_o_o #i=other
  other_vac = sx(xb,params)$s_other*gxy(xb,y,params)$g_other*transition.x(xb,i,j)$p_o_v #i=other
  if(i == "other" & j == "other") return(other_other)
  if(i == "other" & j == "crem") return(other_crem)
  if(i == "other" & j == "liom") return(other_liom)
  if(i == "other" & j == "vac") return(other_vac)
  ## Vac
  vac_crem = sx(xb,params)$s_vac*gxy(xb,y,params)$g_vac*transition.x(xb,i,j)$p_v_c #i=other
  vac_liom = sx(xb,params)$s_vac*gxy(xb,y,params)$g_vac*transition.x(xb,i,j)$p_v_l #i=other
  vac_other = sx(xb,params)$s_vac*gxy(xb,y,params)$g_vac*transition.x(xb,i,j)$p_v_o #i=other
  vac_vac = sx(xb,params)$s_vac*gxy(xb,y,params)$g_vac*transition.x(xb,i,j)$p_v_v #i=other
  if(i == "vac" & j == "other") return(vac_other)
  if(i == "vac" & j == "crem") return(vac_crem)
  if(i == "vac" & j == "liom") return(vac_liom)
  if(i == "vac" & j == "vac") return(vac_vac)
}
ptxy(4,5,cholla,i = "vac",j = "vac")

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
  
  return(list(IPMmat_c=IPMmat_c, IPMmat_l=IPMmat_l, IPMmat_o=IPMmat_o))
}
#image(y,y,t(Tmat),main='fecundity kernel')
#image(y,y,t(Fmat),main='fecundity kernel')

bigmatrix(cholla,lower,upper,matsize)$IPMmat_c

## ----------------- Function that simulates population dynamics and returns lambdaS ------------- ############
lambda.fun(cholla,iter,matsize,extra.grid = 2,floor.extend = 1,ceiling.extend = 4)
## Crem
big_list_c <- list()
lambda_c <- vector()
stable_c <- list()
## Liom
big_list_l <- list()
lambda_l <- vector()
stable_l <- list()
## Other
big_list_o <- list()
lambda_o <- vector()
stable_o <- list()
## Crem
big_list_c<-bigmatrix(cholla, lower, upper, matsize)$IPMmat_c
mat_c <- big_list_c
eig_c <- eigen(mat_c)
lambda_c<-Re(eig_c$values[1])

## Liom
big_list_l<-bigmatrix(cholla, lower, upper, matsize)$IPMmat_l
mat_l <- big_list_l
eig_l <- eigen(mat_l)
lambda_l<-Re(eig_l$values[1])

## Other
big_list_o<-bigmatrix(cholla, lower, upper, matsize)$IPMmat_o
mat_o <- big_list_o
eig_o <- eigen(mat_o)
lambda_o<-Re(eig_o$values[1])

barplot(c(lambda_c,lambda_l,lambda_o),col = c("red","pink","blue"), names.arg = c("Crem.","Liom.","Other"))


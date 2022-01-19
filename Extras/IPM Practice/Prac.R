#########################################################################################################
##            This will be an IPM which allows you to choose how many ants are present
#########################################################################################################
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code")
source("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code/Params.R")
## ----------- Miscellany...we'll need an inverse logit functions ------------- ##
invlogit<-function(x){exp(x)/(1+exp(x))}

## ----------- Vital rate functions. Parameter indices are hard-coded and must correspond to rows of MCMC matrix ------------- ##
##############################################
#GROWTH FROM SIZE X TO Y
gxy<-function(x,y,params,i){
  xb=pmin(pmax(x,cholla_min),cholla_max) #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  g_crem = dnorm(y,mean=params[1] + params[2]*xb,sd=params[3])
  g_vac = dnorm(y,mean=params[301] + params[302]*xb,sd=params[3])
  g_other = dnorm(y,mean=params[201] + params[202]*xb,sd=params[3])
  g_liom = dnorm(y,mean=params[101] + params[102]*xb,sd=params[3])
  if(i == "crem"){ return(g_crem)}
  if(i == "liom"){ return(g_liom)}
  if(i == "other"){ return(g_other)}
  if(i == "vac"){ return(g_vac)}
}

##Check that it works properly
i = c("liom","vac","crem","other")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
g <- vector()
for(n in seq(1:length(i))){
  g[n] <- gxy(x[n],y[n],cholla,i[n])
}
g

#################################################
#SURVIVAL AT SIZE X.
sx<-function(x,params,i){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  s_crem = invlogit(params[11] + params[12]*xb)
  s_vac = invlogit(params[311] + params[312]*xb)
  s_other = invlogit(params[211] + params[212]*xb)
  s_liom = invlogit(params[111] + params[112]*xb)
  if(i == "crem"){ return(s_crem)}
  if(i == "liom"){ return(s_liom)}
  if(i == "other"){ return(s_other)}
  if(i == "vac"){ return(s_vac)}
}

##Check that it works properly
i = c("liom","vac","crem","other")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
s <- vector()
for(n in seq(1:length(i))){
  s[n] <- sx(x[n],cholla,i[n])
}
s


#################################################
#SURVIVAL*GROWTH
pxy<-function(x,y,params,i){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  pxy = sx(x,params,i)*gxy(x,y,params,i)
  return(pxy)
  # p_crem = sx(x,params,i) * gxy(x,y,params,i)
  # p_vac = sx(x,params,i)$s_vac * gxy(x,y,params,i)$g_vac
  # p_other = sx(x,params,i)$s_other * gxy(x,y,params,i)$g_other
  # p_liom = sx(x,params,i)$s_liom * gxy(x,y,params,i)$g_liom
  # #return(list(p_crem=p_crem, p_liom=p_liom, p_other=p_other, p_vac=p_vac))
  # ifelse(i == "crem",
  #        ##yes
  #        return(p_crem),
  #        ##no
  #        ifelse(i == "liom",
  #               ##yes
  #               return(p_liom),
  #               ##no
  #               ifelse(i == "other",
  #                      ##yes
  #                      return(p_other),
  #                      ##no
  #                      return(p_vac))))
}

##Check that it works properly
i = c("liom","vac","crem","other")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
p <- vector()
for(n in 1:length(i)){
  p[n] <- pxy(x[n],y[n],cholla,i[n])
}
p


#################################################################
#PRODUCTION OF 1-YO SEEDS IN THE SEED BANK FROM X-SIZED MOMS
fx<-function(x,params,i){
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
  if(i == "crem"){ return(f_crem)}
  if(i == "liom"){ return(f_liom)}
  if(i == "other"){ return(f_other)}
  if(i == "vac"){ return(f_vac)}
}

## Check if it works
i = c("liom","vac","crem","other")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
f <- vector()
for(n in 1:length(i)){
  f[n] <- fx(x[n],cholla,i[n])
}
f


#####################################################
#### Recruitment
recruits<-function(y,params){
  yb=pmin(pmax(y,cholla_min),cholla_max)
  dnorm(yb, (params[96]),params[97])
}

## Check if it works
recruits(c(-4.99,-3.99,-0.99,0.01,5.01), cholla)



####################################################
beta<-function(vac_rec){
  ifelse(vac_rec == TRUE, return(0), return(1))
}

## Check if it works
beta(FALSE)



######################################################
#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE (OCC VS VAC)
transition.2<-function(x, i, j, params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  occ_occ = (invlogit(params[108] + params[109]*xb)) 
  occ_vac = (1 - invlogit(params[108] + params[109]*xb)) 
  vac_occ = (invlogit(params[98] + params[99]*xb)) 
  vac_vac = (1 - invlogit(params[98] + params[99]*xb)) 
  ## Return the probabilities
  ifelse((i == "crem" | i == "liom" | i == "other") & (j == "crem" | j == "liom" | j == "other"), return(occ_occ),
         ifelse((i == "crem" | i == "liom" | i == "other") & j == "vac", return(occ_vac),
                ifelse(i == "vac" & j == "vac", return(vac_vac),
                       return(vac_occ)
                )
         )
  ) 
}
## Check if it works
i = c("liom","vac","crem","other")
j = c("vac","crem","crem","liom")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
t2 <- vector()
for(n in 1:length(i)){
  t2[n] <- transition.2(x[n],i[n],j[n],cholla)
}
t2


##########################################################
#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE (THREE STATES)
transition.3<-function(x, i, j, params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  ##YES 4 ants -> Calculate the probabilities
  crem_crem = .5 
  crem_liom = .5 
  crem_other = .5 
  crem_vac = .5 
  liom_crem = .4 
  liom_liom = .4 
  liom_other = .4 
  liom_vac = .4 
  other_crem  = .3 
  other_liom = .3 
  other_other = .3 
  other_vac = .3 
  vac_crem = .2 
  vac_liom = .2 
  vac_other = .2 
  vac_vac = .2 
  ## Return the probabilities
  ifelse(i == "crem",
         ##YES CREM
         ifelse(j == "crem",
                ## YES J CREM
                return(crem_crem),
                ## NO J CREM
                ifelse(j == "liom",
                       ##YES J LIOM
                       return(crem_liom),
                       ##NO J LIOM
                       ifelse(j == "other",
                              ##YES J OTHER
                              return(crem_other),
                              ##NO J OTHER
                              return(crem_vac)
                       )
                )
         ),
         ##NO CREM
         ifelse(i == "liom",
                ## YES LIOM
                ifelse(j == "crem",
                       ##YES J CREM
                       return(liom_crem),
                       ##NO J CREM
                       ifelse(j == "liom",
                              ##YES J LIOM
                              return(liom_liom),
                              ##NO J LIOM
                              ifelse(j == "other",
                                     ##YES J OTHER
                                     return(liom_other),
                                     ##NO J OTHER
                                     return(liom_vac)
                              )
                       )
                ),
                ##NO LIOM
                ifelse(i == "other",
                       ##YES OTHER
                       ifelse(j == "crem",
                              ##YES J CREM
                              return(other_crem),
                              ##NO J CREM
                              ifelse(j == "liom",
                                     ##YES J LIOM
                                     return(other_liom),
                                     ##NO J LIOM
                                     ifelse(j == "other",
                                            ##YES J OTHER
                                            return(other_other),
                                            ##NO J OTHER
                                            return(other_vac)
                                     )
                              )
                       ),
                       ##NO OTHER
                       ifelse(i == "vac",
                              ##YES VAC
                              ifelse(j == "crem",
                                     ##YES J CREM
                                     return(vac_crem),
                                     ##NO J CREM
                                     ifelse(j == "liom",
                                            ##YES J LIOM
                                            return(vac_liom),
                                            ##NO J LIOM
                                            ifelse(j == "other",
                                                   ##YES J OTHER
                                                   return(vac_other),
                                                   ##NO J OTHER
                                                   return(vac_vac)
                                            )
                                     )
                              ),
                              ##NO VAC
                              
                       )
                )
         )
  )
}

## Check if it works
i = c("liom","vac","crem","other")
j = c("vac","crem","crem","liom")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
t3 <- vector()
for(n in 1:length(i)){
  t3[n] <- transition.3(x[n],i[n],j[n],cholla)
}
t3


#######################################################
#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE (ALL STATES)
transition.4<-function(x, i, j, params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  ##YES 4 ants -> Calculate the probabilities
  crem_crem = .5 
  crem_liom = .5 
  crem_other = .5 
  crem_vac = .5 
  liom_crem = .4 
  liom_liom = .4 
  liom_other = .4 
  liom_vac = .4 
  other_crem  = .3 
  other_liom = .3 
  other_other = .3 
  other_vac = .3 
  vac_crem = .2 
  vac_liom = .2 
  vac_other = .2 
  vac_vac = .2 
  ## Return the probabilities
  ifelse(i == "crem",
         ##YES CREM
         ifelse(j == "crem",
                ## YES J CREM
                return(crem_crem),
                ## NO J CREM
                ifelse(j == "liom",
                       ##YES J LIOM
                       return(crem_liom),
                       ##NO J LIOM
                       ifelse(j == "other",
                              ##YES J OTHER
                              return(crem_other),
                              ##NO J OTHER
                              return(crem_vac)
                       )
                )
         ),
         ##NO CREM
         ifelse(i == "liom",
                ## YES LIOM
                ifelse(j == "crem",
                       ##YES J CREM
                       return(liom_crem),
                       ##NO J CREM
                       ifelse(j == "liom",
                              ##YES J LIOM
                              return(liom_liom),
                              ##NO J LIOM
                              ifelse(j == "other",
                                     ##YES J OTHER
                                     return(liom_other),
                                     ##NO J OTHER
                                     return(liom_vac)
                              )
                       )
                ),
                ##NO LIOM
                ifelse(i == "other",
                       ##YES OTHER
                       ifelse(j == "crem",
                              ##YES J CREM
                              return(other_crem),
                              ##NO J CREM
                              ifelse(j == "liom",
                                     ##YES J LIOM
                                     return(other_liom),
                                     ##NO J LIOM
                                     ifelse(j == "other",
                                            ##YES J OTHER
                                            return(other_other),
                                            ##NO J OTHER
                                            return(other_vac)
                                     )
                              )
                       ),
                       ##NO OTHER
                       ifelse(i == "vac",
                              ##YES VAC
                              ifelse(j == "crem",
                                     ##YES J CREM
                                     return(vac_crem),
                                     ##NO J CREM
                                     ifelse(j == "liom",
                                            ##YES J LIOM
                                            return(vac_liom),
                                            ##NO J LIOM
                                            ifelse(j == "other",
                                                   ##YES J OTHER
                                                   return(vac_other),
                                                   ##NO J OTHER
                                                   return(vac_vac)
                                            )
                                     )
                              ),
                              ##NO VAC
                              
                       )
                )
         )
  )
}

## Chekc if it works
i = c("liom","vac","crem","other")
j = c("vac","crem","crem","liom")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
t4 <- vector()
for(n in 1:length(i)){
  t4[n] <- transition.4(x[n],i[n],j[n],cholla)
}
t4



#########################################################
#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE 
transition.x <- function(x,i,j,num_ants,params){
  ifelse(num_ants == 2, transition.2(x,i,j,params),
         ifelse(num_ants == 3, transition.3(x,i,j,params),
                transition.4(x,i,j,params)))
}

## Check if it works
i = c("liom","vac","crem","other")
j = c("vac","crem","crem","liom")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
t <- vector()
for(n in 1:length(i)){
  t[n] <- transition.x(x[n],i[n],j[n],2,cholla)
}
t


#####################################################
#GROWTH*SURVIVAL*ANT PROBABILITIES
ptxy <- function(x,y,i,j,num_ants,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  sx(xb,params,i)*gxy(xb,y,params,i)*transition.x(xb,i,j,num_ants,params)
}

## Check if it works
i = c("liom","vac","crem","other")
j = c("vac","crem","crem","liom")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
pt <- vector()
for(n in 1:length(i)){
  pt[n] <- ptxy(x[n],y[n],i[n],j[n],3,cholla)
}
pt



####################################################
#GROWTH*SURVIVAL - ONE ANT ONLY
pxy<-function(x,y,params,i){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  ifelse(
    i == "crem",
    return(sx(xb, params)$s_c*gxy(xb,y,params)$g_c),
    ifelse(
      i == "liom",
      return(sx(xb, params)$s_l*gxy(xb,y,params)$g_l),
      ifelse(
        i == "other",
        return(sx(xb, params)$s_o*gxy(xb,y,params)$g_o),
        return(sx(xb, params)$s_v*gxy(xb,y,params)$g_v)
      )
    )
  )
}

pxy(c(3,-1,5),c(3.5,-1,5.01),cholla,"crem")

bigmatrix.1 <- function(x,y,params,lower,upper,matsize,num_ants,i,j){
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
  
  # Fertility matricies -- One Ant
  Fmat<-matrix(0,(n+2),(n+2))
  # Growth/survival transition matricies -- One Ant
  Tmat<-matrix(0,(n+2),(n+2))
  ## Full Matricies
  IPMmat <- matrix()
  
  # Banked seeds go in top row
  Fmat[1,3:(n+2)]<-fx(y,params,i) 
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  Tmat[2,1]<-1-invlogit(params[71]) 
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)   
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)   
  # Growth/survival transitions among cts sizes
  Tmat[3:(n+2),3:(n+2)]<-t(outer(y,y,pxy,params=params,i))*h 
  # Put it all together
  IPMmat<-Fmat+Tmat  
  return(list(IPMmat=IPMmat, Fmat=Fmat, Tmat=Tmat))
}

bigmatrix.1(4,4.5,cholla,lower,upper,matsize,1,"crem","liom")
## Check that it works
i = c("liom","vac","crem","other")
j = c("vac","crem","crem","liom")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
big1 <- list()
lambda1 <- list()
for(n in 1:length(i)){
  big1[[n]] <- bigmatrix.1(x[n],y[n],cholla,lower,upper,matsize,1,i[n],j[n])
  lambda1[[n]] <- Re(eigen(big1[[n]]$IPMmat)$values[1])
}
lambda1

###############################################
bigmatrix.try2<-function(x,y,params,lower,upper,matsize,num_ants,i,j){
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
  
  # Fertility matricies -- Two Ant
  Fmat <- matrix(0,(2*n+2),(2*n+2))
  # Growth/survival transition matricies -- Two Ant
  Tmat <- matrix(0,(2*n+2),(2*n+2))
  ## Full Matricies
  IPMmat <- matrix()
  # Banked Seeds go in top row
  fec1_i<-fx(y,params=params,i)  
  fec1_j<-fx(y,params=params,j) 
  Fmat[1,3:(n+2)]<-fec1_j 
  Fmat[1,(n+3):(2*n+2)]<-fec1_i 
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  Tmat[2,1]<-1-invlogit(params[71])  
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  #Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)  
  beta2_j<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE) 
  beta2_i<- invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE) 
  Tmat[(n+3):(2*n+2),1]<-beta2_v 
  Tmat[(n+3):(2*n+2),2]<-beta2_c 
  # Growth/survival transitions among cts sizes
  vac_vac<-t(outer(y,y,ptxy,i,i,num_ants=2,params=params))*h 
  vac_occ<-t(outer(y,y,ptxy,i,j,num_ants=2,params=params))*h 
  occ_vac<-t(outer(y,y,ptxy,j,i,num_ants=2,params=params))*h 
  occ_occ<-t(outer(y,y,ptxy,j,j,num_ants=2,params=params))*h 
  Tmat[3:(n+2),3:(n+2)]<- vac_vac   ## Top left 
  Tmat[3:(n+2),(n+3):(2*n+2)]<-occ_vac   ## Top Right 
  Tmat[(n+3):(2*n+2),3:(n+2)]<- occ_occ   ## Bottom Left 
  Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- vac_occ   ## Bottom Right 
  # Put it all together
  IPMmat<-Fmat+Tmat 
  return(list(IPMmat=IPMmat, Fmat=Fmat, Tmat=Tmat))
}

## Check if it is working
i = c("liom","vac","other","vac","crem","vac")
j = c("vac","liom","vac","other","vac","crem")
x = c(-1,-5,4,-1,-5,4)
y = c(-1,-4,4.5,-1,-4,4.5)
big2 <- list()
lambda2 <- vector()
for(n in 1:length(i)){
  big2[[n]] <- bigmatrix.try2(x[n],y[n],cholla,lower,upper,matsize,2,i[n],j[n])
  lambda2[n] <- Re(eigen(big2[[n]]$IPMmat)$values[1])
}
lambda2


##Crem_Vac
Re(eigen(bigmatrix.2(x_dum,y_dum,cholla,lower,upper,matsize,2)$IPMmat_vc)$values[1])
##Liom_Vac
Re(eigen(bigmatrix.2(x_dum,y_dum,cholla,lower,upper,matsize,2)$IPMmat_vl)$values[1])
##Other_Vac
Re(eigen(bigmatrix.2(x_dum,y_dum,cholla,lower,upper,matsize,2)$IPMmat_vo)$values[1])

bigmatrix.try2 <- function(x,y,params,lower,upper,matsize,num_ants,i,j){
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
  
  # Fertility matricies -- Two Ant
  Fmat <- matrix(0,(2*n+2),(2*n+2))
  # Growth/survival transition matricies -- Two Ant
  Tmat <- matrix(0,(2*n+2),(2*n+2))
  ## Full Matricies
  IPMmat <- matrix()
  
  
  if(i == "crem" | j == "crem"){
    # Banked Seeds go in top row
    fec1_v<-fx(y,params=params,"vac")
    fec1_c<-fx(y,params=params,"crem")
    Fmat[1,3:(n+2)]<-fec1_c
    Fmat[1,(n+3):(2*n+2)]<-fec1_v
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit(params[71])
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    #Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)
    beta2_c<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
    beta2_v<- invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(n+3):(2*n+2),1]<-beta2_v
    Tmat[(n+3):(2*n+2),2]<-beta2_c
    # Growth/survival transitions among cts sizes
    vac_vac<-t(outer(y,y,ptxy,i="vac",j="vac",num_ants=2,params=params))*h
    vac_crem<-t(outer(y,y,ptxy,i="vac",j="crem",num_ants=2,params=params))*h
    crem_vac<-t(outer(y,y,ptxy,i="crem",j="vac",num_ants=2,params=params))*h
    crem_crem<-t(outer(y,y,ptxy,i="crem",j="crem",num_ants=2,params=params))*h
    Tmat[3:(n+2),3:(n+2)]<- vac_vac   ## Top left
    Tmat[3:(n+2),(n+3):(2*n+2)]<-crem_vac   ## Top Right
    Tmat[(n+3):(2*n+2),3:(n+2)]<- crem_crem   ## Bottom Right
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- vac_crem   ## Bottom Right
    # Put it all together
    IPMmat<-Fmat+Tmat
    return(list(IPMmat=IPMmat, Fmat=Fmat, Tmat=Tmat))
    #return("Crem,Vac")
  }
  
  if(i == "liom" | j == "liom"){
    ## LIOM
    # Banked seeds go in top row
    fec1_v<-fx(y,params=params,"vac")
    fec1_l<-fx(y,params=params, "liom")
    Fmat[1,3:(n+2)]<-fec1_l
    Fmat[1,(n+3):(2*n+2)]<-fec1_v
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit(params[71])
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    beta1_l<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
    beta1_v<- invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[3:(n+2),1]<-beta1_v
    Tmat[3:(n+2),2]<-beta1_l
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    #Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)
    beta2_l<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
    beta2_v<- invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(n+3):(2*n+2),1]<-beta2_v
    Tmat[(n+3):(2*n+2),2]<-beta2_l
    # Growth/survival transitions among cts sizes
    vac_vac<-t(outer(y,y,ptxy,i="vac",j="vac",num_ants=2,params=params))*h
    vac_liom<-t(outer(y,y,ptxy,i="vac",j="liom",num_ants=2,params=params))*h
    liom_vac<-t(outer(y,y,ptxy,i="liom",j="vac",num_ants=2,params=params))*h
    liom_liom<-t(outer(y,y,ptxy,i="liom",j="liom",num_ants=2,params=params))*h
    Tmat[3:(n+2),3:(n+2)]<- vac_vac   ## Top left
    Tmat[3:(n+2),(n+3):(2*n+2)]<-liom_vac  ## Top Right
    Tmat[(n+3):(2*n+2),3:(n+2)]<- liom_liom   ## Bottom Right
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- vac_liom   ## Bottom Right
    # Put it all together
    IPMmat<-Fmat+Tmat
    return(list(IPMmat=IPMmat, Fmat=Fmat, Tmat=Tmat))
    #return("Liom,Vac")
  }
  
  if(i == "other" | j == "other"){
    ## OTHER
    # Banked seeds go in top row
    fec1_v<-fx(y,params=params, "vac")
    fec1_o<-fx(y,params=params, "other")
    Fmat[1,3:(n+2)]<-fec1_o
    Fmat[1,(n+3):(2*n+2)]<-fec1_v
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
    vac_vac<-t(outer(y,y,ptxy,i="vac",j="vac",num_ants=2,params=params))*h
    vac_other<-t(outer(y,y,ptxy,i="vac",j="other",num_ants=2,params=params))*h
    other_vac<-t(outer(y,y,ptxy,i="other",j="vac",num_ants=2,params=params))*h
    other_other<-t(outer(y,y,ptxy,i="other",j="other",num_ants=2,params=params))*h
    Tmat[3:(n+2),3:(n+2)]<- vac_vac  ## Top left
    Tmat[3:(n+2),(n+3):(2*n+2)]<-other_vac  ## Top Right
    Tmat[(n+3):(2*n+2),3:(n+2)]<- other_other  ## Bottom Right
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- vac_other  ## Bottom Right
    # Put it all together
    IPMmat<-Fmat+Tmat
    return(list(IPMmat=IPMmat, Fmat=Fmat, Tmat=Tmat))
    #return("Other,Vac")
  }
}

bigmatrix.try2(3,4,cholla,lower,upper,matsize,2,"vac","other")

## Check if it is working
i = c("liom","vac","other","vac","crem","vac")
j = c("vac","liom","vac","other","vac","crem")
x = c(-1,-3,4,-1,-3,4)
y = c(-1,-4,4.5,-1,-4,4.5)
big2 <- list()
lambda2 <- vector()
for(n in 1:length(i)){
  big2[[n]] <- bigmatrix.try2(x[n],y[n],cholla,lower,upper,matsize,2,i[n],j[n])
  lambda2[n] <- Re(eigen(big2[[n]]$IPMmat)$values[1])
}
lambda2



##Crem_Vac
Re(eigen(bigmatrix.try2(x,y,cholla,lower,upper,matsize,2,"vac","crem")$IPMmat)$values[1])
##Liom_Vac
Re(eigen(bigmatrix.try2(x,y,cholla,lower,upper,matsize,2,"vac","liom")$IPMmat)$values[1])
##Other_Vac
Re(eigen(bigmatrix.try2(x,y,cholla,lower,upper,matsize,2,"vac","other")$IPMmat)$values[1])






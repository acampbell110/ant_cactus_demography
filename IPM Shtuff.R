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
  g_crem = dnorm(y,mean=params[4,] + params[5,]*xb,sd=params[1,])
  g_vac = dnorm(y,mean=params[10,] + params[11,]*xb,sd=params[1,])
  g_other = dnorm(y,mean=params[8,] + params[9,]*xb,sd=params[1,])
  g_liom = dnorm(y,mean=params[6,] + params[7,]*xb,sd=params[1,])
  if(i == "crem"){ return(g_crem)}
  if(i == "liom"){ return(g_liom)}
  if(i == "other"){ return(g_other)}
  if(i == "vac"){ return(g_vac)}
}

##Check that it works properly
i = c("liom","vac","crem","other")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
crem<-vector()
for(n in seq(1:length(i))){
  crem[n]<-gxy(x[n],y[n],cholla.grow,i[n])
  g[,n] <- gxy(x[n],y[n],cholla.grow,i[n])
}
crem

#################################################
#SURVIVAL AT SIZE X.
sx<-function(x,params,i){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  s_crem = invlogit(params[4] + params[5]*xb)
  s_vac = invlogit(params[10] + params[11]*xb)
  s_other = invlogit(params[8] + params[9]*xb)
  s_liom = invlogit(params[6] + params[7]*xb)
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
  s[n] <- sx(x[n],cholla.surv,i[n])
}
s


#################################################
#SURVIVAL*GROWTH
pxy<-function(x,y,params,i){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  pxy = sx(x,params,i)*gxy(x,y,params,i)
  return(pxy)
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




##################################################################################################
############################# ONE ANT MATRIX #####################################################
##################################################################################################

bigmatrix.1 <- function(x,y,params,lower,upper,matsize,num_ants,i){
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



bigmatrix.1(4,4.5,cholla,lower,upper,matsize,1,"crem")$IPMmat
## Check that it works
i = c("liom","vac","crem","other")
j = c("vac","crem","crem","liom")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
big1 <- list()
lambda1 <- list()
for(n in 1:length(i)){
  big1[[n]] <- bigmatrix.1(x[n],y[n],cholla,lower,upper,matsize,1,i[n])
  lambda1[[n]] <- Re(eigen(big1[[n]]$IPMmat)$values[1])
}
lambda1




#################################################################################################
##################################### One Ant Species and Vacant ################################
#################################################################################################
bigmatrix.2 <- function(x,y,params,lower,upper,matsize,num_ants,i,j){
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
  ############################################# LIOM ############################################
  if(i == "liom" | j == "liom"){
    ## Fecundity of plants
    Fmat[1,3:(n+2)]<-fx(y,params=params,"vac") ## Production of seeds from x sized mom with no ant visitor
    Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"liom") ## Production of seeds from x sized mom with ant visitor
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit(params[71])
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
    Tmat[(n+3):(2*n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
    Tmat[(n+3):(2*n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    # Growth/survival transitions among cts sizes
    Tmat[3:(n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="vac",j="vac",num_ants=2,params=params))*h   ## Top left
    Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,i="liom",j="vac",num_ants=2,params=params))*h   ## Top Right
    Tmat[(n+3):(2*n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="vac",j="liom",num_ants=2,params=params))*h   ## Bottom Left
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,i="liom",j="liom",num_ants=2,params=params))*h   ## Bottom Right
    # Put it all together
    IPMmat<-Fmat+Tmat
    return(list(IPMmat=IPMmat, Fmat=Fmat, Tmat=Tmat))
  }
  ############################################# CREM ###############################################
  if(i == "crem" | j == "crem"){
    ## Fecundity of plants
    Fmat[1,3:(n+2)]<-fx(y,params=params,"vac") ## Production of seeds from x sized mom with no ant visitor
    Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"crem") ## Production of seeds from x sized mom with ant visitor
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit(params[71])
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
    Tmat[(n+3):(2*n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
    Tmat[(n+3):(2*n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    # Growth/survival transitions among cts sizes
    Tmat[3:(n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="vac",j="vac",num_ants=2,params=params))*h   ## Top left
    Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,i="crem",j="vac",num_ants=2,params=params))*h   ## Top Right
    Tmat[(n+3):(2*n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="vac",j="crem",num_ants=2,params=params))*h   ## Bottom Left
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,i="crem",j="crem",num_ants=2,params=params))*h   ## Bottom Right
    # Put it all together
    IPMmat<-Fmat+Tmat
    return(list(IPMmat=IPMmat, Fmat=Fmat, Tmat=Tmat))
  }
  ############################################# OTHER ###########################################
  if(i == "other" | j == "other"){
    ## Fecundity of plants
    Fmat[1,3:(n+2)]<-fx(y,params=params,"vac") ## Production of seeds from x sized mom with no ant visitor
    Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"other") ## Production of seeds from x sized mom with ant visitor
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit(params[71])
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
    Tmat[(n+3):(2*n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
    Tmat[(n+3):(2*n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    # Growth/survival transitions among cts sizes
    Tmat[3:(n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="vac",j="vac",num_ants=2,params=params))*h   ## Top left
    Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,i="other",j="vac",num_ants=2,params=params))*h   ## Top Right
    Tmat[(n+3):(2*n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="vac",j="other",num_ants=2,params=params))*h   ## Bottom Left
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,i="other",j="other",num_ants=2,params=params))*h   ## Bottom Right
    # Put it all together
    IPMmat<-Fmat+Tmat
    return(list(IPMmat=IPMmat, Fmat=Fmat, Tmat=Tmat))
  }
  ############################################# LIOM & CREM ############################################
  if((i == "liom" | j == "liom") & (i == "crem" | j == "crem")){
    ## Fecundity of plants
    Fmat[1,3:(n+2)]<-fx(y,params=params,"liom") ## Production of seeds from x sized mom with no ant visitor
    Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"crem") ## Production of seeds from x sized mom with ant visitor
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit(params[71])
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(n+3):(2*n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(n+3):(2*n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    # Growth/survival transitions among cts sizes
    Tmat[3:(n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="liom",j="liom",num_ants=2,params=params))*h   ## Top left
    Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,i="crem",j="liom",num_ants=2,params=params))*h   ## Top Right
    Tmat[(n+3):(2*n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="liom",j="crem",num_ants=2,params=params))*h   ## Bottom Left
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,i="crem",j="crem",num_ants=2,params=params))*h   ## Bottom Right
    # Put it all together
    IPMmat<-Fmat+Tmat
    return(list(IPMmat=IPMmat, Fmat=Fmat, Tmat=Tmat))
  }
  ############################################# LIOM & OTHER ###############################################
  if((i == "liom" | j == "liom") & (i == "other" | j == "other")){
    ## Fecundity of plants
    Fmat[1,3:(n+2)]<-fx(y,params=params,"liom") ## Production of seeds from x sized mom with no ant visitor
    Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"other") ## Production of seeds from x sized mom with ant visitor
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit(params[71])
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(n+3):(2*n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(n+3):(2*n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    # Growth/survival transitions among cts sizes
    Tmat[3:(n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="liom",j="liom",num_ants=2,params=params))*h   ## Top left
    Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,i="other",j="liom",num_ants=2,params=params))*h   ## Top Right
    Tmat[(n+3):(2*n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="liom",j="other",num_ants=2,params=params))*h   ## Bottom Left
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,i="other",j="other",num_ants=2,params=params))*h   ## Bottom Right
    # Put it all together
    IPMmat<-Fmat+Tmat
    return(list(IPMmat=IPMmat, Fmat=Fmat, Tmat=Tmat))
  }
  ############################################# CREM & OTHER ###########################################
  if((i == "crem" | j == "crem") & (i == "other" | j == "other")){
    ## Fecundity of plants
    Fmat[1,3:(n+2)]<-fx(y,params=params,"crem") ## Production of seeds from x sized mom with no ant visitor
    Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"other") ## Production of seeds from x sized mom with ant visitor
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit(params[71])
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(n+3):(2*n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(n+3):(2*n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    # Growth/survival transitions among cts sizes
    Tmat[3:(n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="crem",j="crem",num_ants=2,params=params))*h   ## Top left
    Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,i="other",j="crem",num_ants=2,params=params))*h   ## Top Right
    Tmat[(n+3):(2*n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="crem",j="other",num_ants=2,params=params))*h   ## Bottom Left
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,i="other",j="other",num_ants=2,params=params))*h   ## Bottom Right
    # Put it all together
    IPMmat<-Fmat+Tmat
    return(list(IPMmat=IPMmat, Fmat=Fmat, Tmat=Tmat))
  }
}

Re(eigen(bigmatrix.2(4,5,cholla,lower,upper,matsize,2,"liom","vac")$IPMmat)$values[1])
Re(eigen(bigmatrix.2(4,5,cholla,lower,upper,matsize,2,"vac","crem")$IPMmat)$values[1])
Re(eigen(bigmatrix.2(4,5,cholla,lower,upper,matsize,2,"vac","other")$IPMmat)$values[1])
Re(eigen(bigmatrix.2(4,5,cholla,lower,upper,matsize,2,"other","liom")$IPMmat)$values[1])
Re(eigen(bigmatrix.2(4,5,cholla,lower,upper,matsize,2,"liom","crem")$IPMmat)$values[1])
Re(eigen(bigmatrix.2(4,5,cholla,lower,upper,matsize,2,"crem","other")$IPMmat)$values[1])







#################################################################################################
###################################### THREE ANTS ###############################################
#################################################################################################

bigmatrix.3 <- function(x,y,params,lower,upper,matsize,num_ants,i,j,scenario){
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
  Fmat <- matrix(0,(3*n+2),(3*n+2))
  # Growth/survival transition matricies -- Two Ant
  Tmat <- matrix(0,(3*n+2),(3*n+2))
  ## Full Matricies
  IPMmat <- matrix()
  ############################################# LIOM & CREM & OTHER ############################################
  if(scenario == "a"){
    ## Fecundity of plants
    Fmat[1,3:(n+2)]<-fx(y,params=params,"crem") ## Production of seeds from x sized mom with no ant visitor
    Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"liom") ## Production of seeds from x sized mom with ant visitor
    Fmat[1,(2*n+3):(3*n+2)]<-fx(y,params=params,"other")
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit(params[71])
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(n+3):(2*n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(2*n+3):(3*n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(n+3):(2*n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(2*n+3):(3*n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    # Growth/survival transitions among cts sizes
    ##Top Row
    Tmat[3:(n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="crem",j="crem",num_ants=2,params=params))*h   ## Top First
    Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,i="liom",j="crem",num_ants=2,params=params))*h   ## Top Second
    Tmat[3:(n+2),(2*n+3):(3*n+2)]<-t(outer(y,y,ptxy,i="other",j="crem",num_ants=2,params=params))*h   ## Top Third
    ##Middle Row
    Tmat[(n+3):(2*n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="crem",j="liom",num_ants=2,params=params))*h   ## Middle First
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,i="liom",j="liom",num_ants=2,params=params))*h   ## Middle Second
    Tmat[(n+3):(2*n+2),(2*n+3):(3*n+2)]<- t(outer(y,y,ptxy,i="other",j="liom",num_ants=2,params=params))*h   ## Middle Third
    ##Bottom Row
    Tmat[(2*n+3):(3*n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="crem",j="other",num_ants=2,params=params))*h   ## Bottom First
    Tmat[(2*n+3):(3*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,i="liom",j="other",num_ants=2,params=params))*h   ## Bottom Second
    Tmat[(2*n+3):(3*n+2),(2*n+3):(3*n+2)]<- t(outer(y,y,ptxy,i="other",j="other",num_ants=2,params=params))*h   ## Bottom Third
    # Put it all together
    IPMmat<-Fmat+Tmat
    return(list(IPMmat=IPMmat, Fmat=Fmat, Tmat=Tmat))
  }
  ############################################# LIOM & CREM & VAC ###############################################
  if(scenario == "b"){
    ## Fecundity of plants
    Fmat[1,3:(n+2)]<-fx(y,params=params,"crem") ## Production of seeds from x sized mom with no ant visitor
    Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"liom") ## Production of seeds from x sized mom with ant visitor
    Fmat[1,(2*n+3):(3*n+2)]<-fx(y,params=params,"vac")
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit(params[71])
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(n+3):(2*n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(2*n+3):(3*n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(n+3):(2*n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(2*n+3):(3*n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
    # Growth/survival transitions among cts sizes
    ##Top Row
    Tmat[3:(n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="crem",j="crem",num_ants=2,params=params))*h   ## Top First
    Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,i="liom",j="crem",num_ants=2,params=params))*h   ## Top Second
    Tmat[3:(n+2),(2*n+3):(3*n+2)]<-t(outer(y,y,ptxy,i="vac",j="crem",num_ants=2,params=params))*h   ## Top Third
    ##Middle Row
    Tmat[(n+3):(2*n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="crem",j="liom",num_ants=2,params=params))*h   ## Middle First
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,i="liom",j="liom",num_ants=2,params=params))*h   ## Middle Second
    Tmat[(n+3):(2*n+2),(2*n+3):(3*n+2)]<- t(outer(y,y,ptxy,i="vac",j="liom",num_ants=2,params=params))*h   ## Middle Third
    ##Bottom Row
    Tmat[(2*n+3):(3*n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="crem",j="vac",num_ants=2,params=params))*h   ## Bottom First
    Tmat[(2*n+3):(3*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,i="liom",j="vac",num_ants=2,params=params))*h   ## Bottom Second
    Tmat[(2*n+3):(3*n+2),(2*n+3):(3*n+2)]<- t(outer(y,y,ptxy,i="vac",j="vac",num_ants=2,params=params))*h   ## Bottom Third
    # Put it all together
    IPMmat<-Fmat+Tmat
    return(list(IPMmat=IPMmat, Fmat=Fmat, Tmat=Tmat))
  }
  ############################################# LIOM & OTHER & VAC ###########################################
  if(scenario == "c"){
    ## Fecundity of plants
    Fmat[1,3:(n+2)]<-fx(y,params=params,"liom") ## Production of seeds from x sized mom with no ant visitor
    Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"other") ## Production of seeds from x sized mom with ant visitor
    Fmat[1,(2*n+3):(3*n+2)]<-fx(y,params=params,"vac")
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit(params[71])
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(n+3):(2*n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(2*n+3):(3*n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(n+3):(2*n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(2*n+3):(3*n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
    # Growth/survival transitions among cts sizes
    ##Top Row
    Tmat[3:(n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="liom",j="liom",num_ants=2,params=params))*h   ## Top First
    Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,i="other",j="liom",num_ants=2,params=params))*h   ## Top Second
    Tmat[3:(n+2),(2*n+3):(3*n+2)]<-t(outer(y,y,ptxy,i="vac",j="liom",num_ants=2,params=params))*h   ## Top Third
    ##Middle Row
    Tmat[(n+3):(2*n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="liom",j="other",num_ants=2,params=params))*h   ## Middle First
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,i="other",j="other",num_ants=2,params=params))*h   ## Middle Second
    Tmat[(n+3):(2*n+2),(2*n+3):(3*n+2)]<- t(outer(y,y,ptxy,i="vac",j="other",num_ants=2,params=params))*h   ## Middle Third
    ##Bottom Row
    Tmat[(2*n+3):(3*n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="liom",j="vac",num_ants=2,params=params))*h   ## Bottom First
    Tmat[(2*n+3):(3*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,i="other",j="vac",num_ants=2,params=params))*h   ## Bottom Second
    Tmat[(2*n+3):(3*n+2),(2*n+3):(3*n+2)]<- t(outer(y,y,ptxy,i="vac",j="vac",num_ants=2,params=params))*h   ## Bottom Third
    # Put it all together
    IPMmat<-Fmat+Tmat
    return(list(IPMmat=IPMmat, Fmat=Fmat, Tmat=Tmat))
  }
  ############################################# CREM & OTHER & VAC ############################################
  if(scenario == "d"){
    ## Fecundity of plants
    Fmat[1,3:(n+2)]<-fx(y,params=params,"crem") ## Production of seeds from x sized mom with no ant visitor
    Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"other") ## Production of seeds from x sized mom with ant visitor
    Fmat[1,(2*n+3):(3*n+2)]<-fx(y,params=params,"vac")
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit(params[71])
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(n+3):(2*n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(2*n+3):(3*n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(n+3):(2*n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
    Tmat[(2*n+3):(3*n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
    # Growth/survival transitions among cts sizes
    ##Top Row
    Tmat[3:(n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="crem",j="crem",num_ants=2,params=params))*h   ## Top First
    Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,i="other",j="crem",num_ants=2,params=params))*h   ## Top Second
    Tmat[3:(n+2),(2*n+3):(3*n+2)]<-t(outer(y,y,ptxy,i="vac",j="crem",num_ants=2,params=params))*h   ## Top Third
    ##Middle Row
    Tmat[(n+3):(2*n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="crem",j="other",num_ants=2,params=params))*h   ## Middle First
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,i="other",j="other",num_ants=2,params=params))*h   ## Middle Second
    Tmat[(n+3):(2*n+2),(2*n+3):(3*n+2)]<- t(outer(y,y,ptxy,i="vac",j="other",num_ants=2,params=params))*h   ## Middle Third
    ##Bottom Row
    Tmat[(2*n+3):(3*n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="crem",j="vac",num_ants=2,params=params))*h   ## Bottom First
    Tmat[(2*n+3):(3*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,i="other",j="vac",num_ants=2,params=params))*h   ## Bottom Second
    Tmat[(2*n+3):(3*n+2),(2*n+3):(3*n+2)]<- t(outer(y,y,ptxy,i="vac",j="vac",num_ants=2,params=params))*h   ## Bottom Third
    # Put it all together
    IPMmat<-Fmat+Tmat
    return(list(IPMmat=IPMmat, Fmat=Fmat, Tmat=Tmat))
  }
}

Re(eigen(bigmatrix.3(4,5,cholla,lower,upper,matsize,3,"liom","other","a")$IPMmat)$values[1])
Re(eigen(bigmatrix.3(4,5,cholla,lower,upper,matsize,3,"liom","liom","b")$IPMmat)$values[1])
Re(eigen(bigmatrix.3(4,5,cholla,lower,upper,matsize,3,"vac","vac","c")$IPMmat)$values[1])
Re(eigen(bigmatrix.3(4,5,cholla,lower,upper,matsize,3,"other","crem","d")$IPMmat)$values[1])




##################################################################################################
######################################### ALL ANTS PRESENT #######################################
##################################################################################################

bigmatrix.4 <- function(x,y,params,lower,upper,matsize,num_ants,i,j,scenario){
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
  Fmat <- matrix(0,(4*n+2),(4*n+2))
  # Growth/survival transition matricies -- Two Ant
  Tmat <- matrix(0,(4*n+2),(4*n+2))
  ## Full Matricies
  IPMmat <- matrix()
  ############################################# LIOM & CREM & OTHER & VAC ############################################
  ## Fecundity of plants
  Fmat[1,3:(n+2)]<-fx(y,params=params,"crem") ## Production of seeds from x sized mom with no ant visitor
  Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"liom") ## Production of seeds from x sized mom with ant visitor
  Fmat[1,(2*n+3):(3*n+2)]<-fx(y,params=params,"other")
  Fmat[1,(3*n+3):(4*n+2)]<-fx(y,params=params,"vac")
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  Tmat[2,1]<-1-invlogit(params[71])
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
  Tmat[(n+3):(2*n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
  Tmat[(2*n+3):(3*n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
  Tmat[(3*n+3):(4*n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
  Tmat[(n+3):(2*n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
  Tmat[(2*n+3):(3*n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE)
  Tmat[(3*n+3):(4*n+2),1]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE)
  # Growth/survival transitions among cts sizes
  ##Top Row
  Tmat[3:(n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="crem",j="crem",num_ants=2,params=params))*h   ## Top First
  Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,i="liom",j="crem",num_ants=2,params=params))*h   ## Top Second
  Tmat[3:(n+2),(2*n+3):(3*n+2)]<-t(outer(y,y,ptxy,i="other",j="crem",num_ants=2,params=params))*h   ## Top Third
  Tmat[3:(n+2),(3*n+3):(4*n+2)]<-t(outer(y,y,ptxy,i="vac",j="crem",num_ants=2,params=params))*h   ## Top Fourth
  ##Middle Row
  Tmat[(n+3):(2*n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="crem",j="liom",num_ants=2,params=params))*h   ## Middle First
  Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,i="liom",j="liom",num_ants=2,params=params))*h   ## Middle Second
  Tmat[(n+3):(2*n+2),(2*n+3):(3*n+2)]<- t(outer(y,y,ptxy,i="other",j="liom",num_ants=2,params=params))*h   ## Middle Third
  Tmat[(n+3):(2*n+2),(3*n+3):(4*n+2)]<-t(outer(y,y,ptxy,i="vac",j="liom",num_ants=2,params=params))*h   ## Middle Fourth
  ##Middle 2 Row
  Tmat[(2*n+3):(3*n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="crem",j="other",num_ants=2,params=params))*h   ## Middle 2 First
  Tmat[(2*n+3):(3*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,i="liom",j="other",num_ants=2,params=params))*h   ## Middle 2 Second
  Tmat[(2*n+3):(3*n+2),(2*n+3):(3*n+2)]<- t(outer(y,y,ptxy,i="other",j="other",num_ants=2,params=params))*h   ## Middle 2 Third
  Tmat[(2*n+3):(3*n+2),(3*n+3):(4*n+2)]<-t(outer(y,y,ptxy,i="vac",j="other",num_ants=2,params=params))*h   ## Middle 2 Fourth
  #Bottom Row
  ##Middle 2 Row
  Tmat[(3*n+3):(4*n+2),3:(n+2)]<- t(outer(y,y,ptxy,i="crem",j="vac",num_ants=2,params=params))*h   ## Bottom First
  Tmat[(3*n+3):(4*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,i="liom",j="vac",num_ants=2,params=params))*h   ## Bottom Second
  Tmat[(3*n+3):(4*n+2),(2*n+3):(3*n+2)]<- t(outer(y,y,ptxy,i="other",j="vac",num_ants=2,params=params))*h   ## Bottom Third
  Tmat[(3*n+3):(4*n+2),(3*n+3):(4*n+2)]<-t(outer(y,y,ptxy,i="vac",j="vac",num_ants=2,params=params))*h   ## Bottom Fourth
  # Put it all together
  IPMmat<-Fmat+Tmat
  return(list(IPMmat=IPMmat, Fmat=Fmat, Tmat=Tmat))
}

Re(eigen(bigmatrix.4(4,5,cholla,lower,upper,matsize,4,"vac","vac")$IPMmat)$values[1])





#################################################################################################
############################ CHOOSE WHICH SCENARIO (COMBO OF ANTS) ##############################
#################################################################################################

bigmatrix<-function(x,y,params,lower,upper,matsize,num_ants){  
  ###################################################################################################
  ## returns the full IPM kernel (to be used in stochastic simulation), the F and T kernels, and meshpoints in the units of size
  ## params,yrfx,plotfx, and mwye get passed to the vital rate functions
  ## f.eps is fertility overdispersion. defaults to zero (see lambda.fun())
  ## lower and upper are the integration limits
  ## matsize is the dimension of the approximating matrix (it gets an additional 2 rows and columns for the seed banks)
  ###################################################################################################
  if(num_ants == 1){
    return(list(IPMmat=IPMmat,Fmat=Fmat,Tmat=Tmat))
  }
  if(num_ants == 2){
    
  }
  if(num_ants == 3){
    
  }
  if(num_ants == 4){
    
  }
} 

(bigmatrix(4,5,cholla[,1], lower, upper, matsize, 1))
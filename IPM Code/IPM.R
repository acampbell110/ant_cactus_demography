#########################################################################################################
##            This will be an IPM which allows you to choose how many ants are present
#########################################################################################################
#setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code")
#source("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code/Params.R")
## ----------- Miscellany...we'll need an inverse logit functions ------------- ##
invlogit<-function(x){exp(x)/(1+exp(x))}

## ----------- Vital rate functions. Parameter indices are hard-coded and must correspond to rows of MCMC matrix ------------- ##
#########################################################################################################
## GROWTH FROM SIZE X TO Y. Returns the probability of growth from size x to y based on ant state    ####
## This model accepts the input of previous size and ant state to determine the probability of being ####
## y size in the next year.                                                                          ####
## This function is vectorized so if you input a vector for x and y and a single ant species you     ####
## will get a vector of probabilities.                                                               ####
#########################################################################################################
gxy<-function(x,y,i,params){
  #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  xb=pmin(pmax(x,cholla_min),cholla_max) #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  #Density probability function which uses the parameters that are ant specific 
  g_vac = dlst(y,mu=mean(params$grow_beta01) + mean(params$grow_beta11)*xb + mean(params$grow_beta21)*xb^2, 
              sigma = exp(mean(params$grow_sig0) + mean(params$grow_sig1)*xb), 
              df = exp(mean(params$grow_alp0) + mean(params$grow_alp1)*xb))
  g_liom = dlst(y,mu=mean(params$grow_beta04) + mean(params$grow_beta14)*xb + mean(params$grow_beta24)*xb^2, 
               sigma = exp(mean(params$grow_sig0) + mean(params$grow_sig1)*xb), 
               df = exp(mean(params$grow_alp0) + mean(params$grow_alp1)*xb))
  g_crem = dlst(y,mu=mean(params$grow_beta03) + mean(params$grow_beta13)*xb + mean(params$grow_beta23)*xb^2, 
               sigma = exp(mean(params$grow_sig0) + mean(params$grow_sig1)*xb), 
               df = exp(mean(params$grow_alp0) + mean(params$grow_alp1)*xb))
  g_other = dlst(y,mu=mean(params$grow_beta02) + mean(params$grow_beta12)*xb + mean(params$grow_beta22)*xb^2, 
                sigma = exp(mean(params$grow_sig0) + mean(params$grow_sig1)*xb), 
                df = exp(mean(params$grow_alp0) + mean(params$grow_alp1)*xb))
  #Return the probability of growing from size x to y
  if(i == "crem"){ return(g_crem)}
  if(i == "liom"){ return(g_liom)}
  if(i == "other"){ return(g_other)}
  if(i == "vacant"){ return(g_vac)}
}

# gxy<-function(x,y,params){
#   #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
#   xb=pmin(pmax(x,cholla_min),cholla_max) #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
#   #Density probability function which uses the parameters that are ant specific 
#   dnorm(y,mean=mean(params$grow_beta01) + mean(params$grow_beta11)*xb + mean(params$grow_beta21)*xb^2, 
#                sd = exp(mean(params$grow_sig0) + mean(params$grow_sig1)*xb))
# }
# gxy(4,3,"crem",params)
# 
# x = c(1,2,-5,8,1,2,-5,8,1,2,-5,8,1,2,-5,8)
# y = c(2,3,0,7,2,3,0,7,2,3,0,7,2,3,0,7)
# i = c("crem","crem","crem","crem",
#       "liom","liom","liom","liom",
#       "other","other","other","other",
#       "vacant","vacant","vacant","vacant")
# gx <- vector()
# for(n in 1:length(i)){
#   gx[n] = gxy(x[n],y[n],i[n],params)
# }
# gx

hist(extraDistr::dlst(x=seq(-10,10,0.01),df=2,mu = -5))
integrate(dlst,-Inf,Inf, df = 10)


## Tom checking out growth model predictions from min and max initial sizes
n<-matsize
L<-lower
U<-upper
h<-(U-L)/n                   #Bin size
b<-L+c(0:n)*h;               #Lower boundaries of bins 
y<-0.5*(b[1:n]+b[2:(n+1)]);  #Bin midpoints
par(mfrow=c(2,1))
plot(y,gxy(0,y,"vacant",params),type="l",ylim = c(0,.5)
     ,col="blue",ylab="gxy()");abline(v=0,col="blue",lty=2,)
lines(y,gxy(10,y,"vacant",params),type="l",col="red");abline(v=10,col="red",lty=2)
hist(cactus$logsize_t1[cactus$logsize_t< 0],xlim=c(-5,15))
hist(cactus$logsize_t1[cactus$logsize_t>10],add=T)

#########################################################################################################
## SURVIVAL AT SIZE X. Returns the probability of survival of a cactus based on size and ant state   ####
## You input the size of the cactus and ant state and in return you get the probability of surviving ####
## to the next year.                                                                                 ####
## This function is vectorized so if you input a vector for x and y and a single ant species you     ####
## will get a vector of probabilities.                                                               ####
#########################################################################################################
sx<-function(x,i,params){
  #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  xb=pmin(pmax(x,cholla_min),cholla_max)
  #Transform the ant specific parameters to the probability of survival
  s_crem = invlogit(mean(params$surv_beta03) + mean(params$surv_beta13)*xb)
  s_vac = invlogit(mean(params$surv_beta01) + mean(params$surv_beta11)*xb)
  s_other = invlogit(mean(params$surv_beta02) + mean(params$surv_beta12)*xb)
  s_liom = invlogit(mean(params$surv_beta04) + mean(params$surv_beta14)*xb)
  #Return the survival probabilities
   if(i == "crem"){ return(s_crem)}
   if(i == "liom"){ return(s_liom)}
   if(i == "other"){ return(s_other)}
   if(i == "vacant"){ return(s_vac)}
}


#################################################
#SURVIVAL*GROWTH. Combine the survival and growth probabilities
pxy<-function(x,y,i,params){
  #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  xb=pmin(pmax(x,cholla_min),cholla_max)
  #Multiply the probabilities of survival and growth together to get the survival growth kernel
  #pxy =sx(xb,i,params)*gxy(xb,y,i,params)
  pxy = gxy(xb,y,i,params)
  return(pxy)
}


#################################################################
#PRODUCTION OF 1-YO SEEDS IN THE SEED BANK FROM X-SIZED MOMS
fx<-function(x,i,params){
  #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  xb=pmin(pmax(x,cholla_min),cholla_max)
  p.flow<-invlogit(mean(params$repro_beta0) + mean(params$repro_beta1)*xb)      ## Probability of Reproducing
  nflow<-exp(mean(params$flow_beta0) + mean(params$flow_beta1)*xb)      ## Number of FLowers produced
  flow.surv_crem<-invlogit(mean(params$viab_beta03))      ## Proportion of Flowers survive to fruit
  flow.surv_vac<-invlogit(mean(params$viab_beta01))       ## Proportion of Flowers survive to fruit
  flow.surv_other<-invlogit(mean(params$viab_beta02))       ## Proportion of Flowers survive to fruit
  flow.surv_liom<-invlogit(mean(params$viab_beta04))      ## Proportion of Flowers survive to fruit
  seeds.per.fruit_crem<-mean(params$seed_beta01)      ## Number of Seeds per Fruit
  seeds.per.fruit_liom<-mean(params$seed_beta03)      ## Number of Seeds per Fruit
  seeds.per.fruit_vac<-mean(params$seed_beta02)     ## Number of Seeds per Fruit
  seed.survival<-invlogit(mean(params$preseed_beta0))^2       ## Seed per Fruit Survival ---------I measured 6-month seed survival; annual survival is its square
  #Calculate the fecundity probabilities by ant species
  f_crem = p.flow*nflow*flow.surv_crem*seeds.per.fruit_crem*seed.survival
  f_vac = p.flow*nflow*flow.surv_vac*seeds.per.fruit_vac*seed.survival
  f_other = p.flow*nflow*flow.surv_other*seeds.per.fruit_vac*seed.survival
  f_liom = p.flow*nflow*flow.surv_liom*seeds.per.fruit_liom*seed.survival
  #Return the correct value
  if(i == "crem"){ return(f_crem)}
  if(i == "liom"){ return(f_liom)}
  if(i == "other"){ return(f_other)}
  if(i == "vacant"){ return(f_vac)}
}


#####################################################
#### Recruitment. Calculate the probability of a recruit being a certain size y
recruits<-function(y,params){
  #Calculate the probability and return it
  dnorm(y, mean(params$rec_beta0),mean(params$rec_sig))
  #dtnorm(y,mean(params$rec_beta0),mean(params$rec_sig),left=cholla_min)
}


######################################################
#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE. One ant option
transition.1<-function(x, i, j,params, scenario){
  #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  xb=pmin(pmax(x,cholla_min),cholla_max)
  ## Crem and Vac
  if(scenario == "cremvac"){
    #Denom of previously tended by None
    Denominator_vac <- exp(mean(params$multi_betavv) + xb*mean(params$multi_betav)) + 
      exp(mean(params$multi_betavc) + xb*mean(params$multi_betac))
    #Calculate the probabilities by next ant state
    vac_vac = exp(mean(params$multi_betavv) + xb*mean(params$multi_betav))/Denominator_vac
    vac_crem = exp(mean(params$multi_betavc) + xb*mean(params$multi_betac))/Denominator_vac
    #Denom of previously tended by Crem
    Denominator_crem <- exp(mean(params$multi_betacv) + xb*mean(params$multi_betav)) + 
      exp(mean(params$multi_betacc) + xb*mean(params$multi_betac))
    #Calculate the probabilities by next ant state
    crem_vac = exp(mean(params$multi_betacv) + xb*mean(params$multi_betav))/Denominator_crem
    crem_crem = exp(mean(params$multi_betacc) + xb*mean(params$multi_betac))/Denominator_crem
    #Return them
    if(i == "crem" & j == "crem"){return(crem_crem)}
    if(i == "crem" & j == "vacant"){return(crem_vac)}
    if(i == "vacant" & j == "crem"){return(vac_crem)}
    if(i == "vacant" & j == "vacant"){return(vac_vac)}
  }
  ## Liom and Vac
  if(scenario == "liomvac"){
    #Denom of previously tended by None
    Denominator_vac <- exp(mean(params$multi_betavv) + xb*mean(params$multi_betav)) + 
      exp(mean(params$multi_betavl) + xb*mean(params$multi_betal))
    #Calculate the probabilities by next ant state
    vac_vac = exp(mean(params$multi_betavv) + xb*mean(params$multi_betav))/Denominator_vac
    vac_liom = exp(mean(params$multi_betavl) + xb*mean(params$multi_betal))/Denominator_vac
    #Denom of previously tended by Liom
    Denominator_liom <- exp(mean(params$multi_betalv) + xb*mean(params$multi_betav)) + 
      exp(mean(params$multi_betall) + xb*mean(params$multi_betal))
    #Calculate the probabilities by next ant state
    liom_vac = exp(mean(params$multi_betalv) + xb*mean(params$multi_betav))/Denominator_liom
    liom_liom = exp(mean(params$multi_betall) + xb*mean(params$multi_betal))/Denominator_liom
    #Return the probabilities
    if(i == "liom" & j == "liom"){return(liom_liom)}
    if(i == "liom" & j == "vacant"){return(liom_vac)}
    if(i == "vacant" & j == "liom"){return(vac_liom)}
    if(i == "vacant" & j == "vacant"){return(vac_vac)}
  }
  ## Other and Vac
  if(scenario == "othervac"){
    #Denom of previously tended by None
    Denominator_vac <- exp(mean(params$multi_betavv) + xb*mean(params$multi_betav)) + 
      exp(mean(params$multi_betavo) + xb*mean(params$multi_betao))
    #Calculate the probabilities by next ant state
    vac_vac = exp(mean(params$multi_betavv) + xb*mean(params$multi_betav))/Denominator_vac
    vac_other = exp(mean(params$multi_betavo) + xb*mean(params$multi_betao))/Denominator_vac
    #Denom of previously tended by Liom
    Denominator_other <- exp(mean(params$multi_betaov) + xb*mean(params$multi_betav)) + 
      exp(mean(params$multi_betaoo) + xb*mean(params$multi_betao))
    #Calculate the probabilities by next ant state
    other_vac = exp(mean(params$multi_betaov) + xb*mean(params$multi_betav))/Denominator_other
    other_other = exp(mean(params$multi_betaoo) + xb*mean(params$multi_betao))/Denominator_other
    #Return the probabilities
    if(i == "other" & j == "other"){return(other_other)}
    if(i == "other" & j == "vacant"){return(other_vac)}
    if(i == "vacant" & j == "other"){return(vac_other)}
    if(i == "vacant" & j == "vacant"){return(vac_vac)}
  }
}



##########################################################
#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE (THREE STATES)
transition.2<-function(x, i, j, params,scenario){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  ## Liom, Vac, Other
  #Denom of previously tended by None
  if(scenario == "liomvacother"){
    Denominator_vac <- exp(mean(params$multi_betavv) + xb*mean(params$multi_betav)) + 
      exp(mean(params$multi_betavl) + xb*mean(params$multi_betal)) + 
      exp(mean(params$multi_betavo) + xb*mean(params$multi_betao))
    #Calculate the probabilities by next ant state
    vac_vac = exp(mean(params$multi_betavv) + xb*mean(params$multi_betav))/Denominator_vac
    vac_liom = exp(mean(params$multi_betavl) + xb*mean(params$multi_betal))/Denominator_vac
    vac_other = exp(mean(params$multi_betavo) + xb*mean(params$multi_betao))/Denominator_vac
    #Denom of previously tended by Liom
    Denominator_liom <- exp(mean(params$multi_betalv) + xb*mean(params$multi_betav)) + 
      exp(mean(params$multi_betall) + xb*mean(params$multi_betal)) + 
      exp(mean(params$multi_betalo) + xb*mean(params$multi_betao))
    #
    liom_vac = exp(mean(params$multi_betalv) + xb*mean(params$multi_betav))/Denominator_liom
    liom_liom = exp(mean(params$multi_betall) + xb*mean(params$multi_betal))/Denominator_liom
    liom_other = exp(mean(params$multi_betalo) + xb*mean(params$multi_betao))/Denominator_liom
    ## Previously tended by Other
    Denominator_other <- exp(mean(params$multi_betaov) + xb*mean(params$multi_betav)) + 
      exp(mean(params$multi_betaol) + xb*mean(params$multi_betal)) + 
      exp(mean(params$multi_betaoo) + xb*mean(params$multi_betao))
      other_vac = exp(mean(params$multi_betaov) + xb*mean(params$multi_betav))/Denominator_other
      other_liom = exp(mean(params$multi_betaol) + xb*mean(params$multi_betal))/Denominator_other
      other_other = exp(mean(params$multi_betaoo) + xb*mean(params$multi_betao))/Denominator_other
      if(i == "liom" & j == "liom"){return(liom_liom)}
      if(i == "liom" & j == "vacant"){return(liom_vac)}
      if(i == "liom" & j == "other"){return(liom_other)}
      if(i == "vacant" & j == "liom"){return(vac_liom)}
      if(i == "vacant" & j == "vacant"){return(vac_vac)}
      if(i == "vacant" & j == "other"){return(vac_other)}
      if(i == "other" & j == "liom"){return(other_liom)}
      if(i == "other" & j == "vacant"){return(other_vac)}
      if(i == "other" & j == "other"){return(other_other)}
  }
  ## Liom, Crem, vac
  ## Previously tended by None
  if(scenario == "liomcremvac"){
    Denominator_crem <- exp(mean(params$multi_betacc) + xb*mean(params$multi_betac)) + 
      exp(mean(params$multi_betacl) + xb*mean(params$multi_betal)) + 
      exp(mean(params$multi_betacv) + xb*mean(params$multi_betav))
    crem_crem = exp(mean(params$multi_betacc) + xb*mean(params$multi_betac))/Denominator_crem
    crem_liom = exp(mean(params$multi_betacl) + xb*mean(params$multi_betal))/Denominator_crem
    crem_vac = exp(mean(params$multi_betacv) + xb*mean(params$multi_betav))/Denominator_crem
    ## Previously tended by Liom
    Denominator_liom <- exp(mean(params$multi_betalc) + xb*mean(params$multi_betac)) + 
      exp(mean(params$multi_betall) + xb*mean(params$multi_betal)) + 
      exp(mean(params$multi_betalv) + xb*mean(params$multi_betav))
    liom_crem = exp(mean(params$multi_betalc) + xb*mean(params$multi_betac))/Denominator_liom
    liom_liom = exp(mean(params$multi_betall) + xb*mean(params$multi_betal))/Denominator_liom
    liom_vac = exp(mean(params$multi_betalv) + xb*mean(params$multi_betav))/Denominator_liom
    ## Previously tended by None
    Denominator_vac <- exp(mean(params$multi_betavc) + xb*mean(params$multi_betac)) + 
      exp(mean(params$multi_betavl) + xb*mean(params$multi_betal)) + 
      exp(mean(params$multi_betavv) + xb*mean(params$multi_betav))
    vac_crem = exp(mean(params$multi_betavc) + xb*mean(params$multi_betac))/Denominator_vac
    vac_liom = exp(mean(params$multi_betavl) + xb*mean(params$multi_betal))/Denominator_vac
    vac_vac = exp(mean(params$multi_betavv) + xb*mean(params$multi_betav))/Denominator_vac
    if(i == "liom" & j == "liom"){return(liom_liom)}
    if(i == "liom" & j == "crem"){return(liom_crem)}
    if(i == "liom" & j == "vacant"){return(liom_vac)}
    if(i == "crem" & j == "liom"){return(crem_liom)}
    if(i == "crem" & j == "crem"){return(crem_crem)}
    if(i == "crem" & j == "vacant"){return(crem_vac)}
    if(i == "vacant" & j == "liom"){return(vac_liom)}
    if(i == "vacant" & j == "crem"){return(vac_crem)}
    if(i == "vacant" & j == "vacant"){return(vac_vac)}
  }
  ## Other, Crem, vac
  ## Previously tended by None
  if(scenario == "othercremvac"){
    Denominator_crem <- exp(mean(params$multi_betacc) + xb*mean(params$multi_betac)) + 
      exp(mean(params$multi_betaco) + xb*mean(params$multi_betao)) + 
      exp(mean(params$multi_betacv) + xb*mean(params$multi_betav))
    crem_crem = exp(mean(params$multi_betacc) + xb*mean(params$multi_betac))/Denominator_crem
    crem_other = exp(mean(params$multi_betaco) + xb*mean(params$multi_betao))/Denominator_crem
    crem_vac = exp(mean(params$multi_betacv) + xb*mean(params$multi_betav))/Denominator_crem
    ## Previously tended by Liom
    Denominator_other <- exp(mean(params$multi_betaoc) + xb*mean(params$multi_betac)) + 
      exp(mean(params$multi_betaoo) + xb*mean(params$multi_betao)) + 
      exp(mean(params$multi_betaov) + xb*mean(params$multi_betav))
    other_crem = exp(mean(params$multi_betaoc) + xb*mean(params$multi_betac))/Denominator_other
    other_other = exp(mean(params$multi_betaoo) + xb*mean(params$multi_betao))/Denominator_other
    other_vac = exp(mean(params$multi_betaov) + xb*mean(params$multi_betav))/Denominator_other
    ## Previously tended by None
    Denominator_vac <- exp(mean(params$multi_betavc) + xb*mean(params$multi_betac)) + 
      exp(mean(params$multi_betavo) + xb*mean(params$multi_betao)) + 
      exp(mean(params$multi_betavv) + xb*mean(params$multi_betav))
    vac_crem = exp(mean(params$multi_betavc) + xb*mean(params$multi_betac))/Denominator_vac
    vac_other = exp(mean(params$multi_betavo) + xb*mean(params$multi_betao))/Denominator_vac
    vac_vac = exp(mean(params$multi_betavv) + xb*mean(params$multi_betav))/Denominator_vac
    if(i == "other" & j == "other"){return(other_other)}
    if(i == "other" & j == "crem"){return(other_crem)}
    if(i == "other" & j == "vacant"){return(other_vac)}
    if(i == "crem" & j == "other"){return(crem_other)}
    if(i == "crem" & j == "crem"){return(crem_crem)}
    if(i == "crem" & j == "vacant"){return(crem_vac)}
    if(i == "vacant" & j == "other"){return(vac_other)}
    if(i == "vacant" & j == "crem"){return(vac_crem)}
    if(i == "vacant" & j == "vacant"){return(vac_vac)}
  }
}


#######################################################
#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE (ALL STATES)
transition.3<-function(x, i, j,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  ## Previously tended by None
  Denominator_vac <- exp(mean(params$multi_betavv) + xb*mean(params$multi_betav)) + 
    exp(mean(params$multi_betavo) + xb*mean(params$multi_betao)) + 
    exp(mean(params$multi_betavc) + xb*mean(params$multi_betac)) + 
    exp(mean(params$multi_betavl) + xb*mean(params$multi_betal))
    vac_vac = exp(mean(params$multi_betavv) + xb*mean(params$multi_betav))/Denominator_vac
    vac_other = exp(mean(params$multi_betavo) + xb*mean(params$multi_betao))/Denominator_vac
    vac_crem = exp(mean(params$multi_betavc) + xb*mean(params$multi_betac))/Denominator_vac
    vac_liom = exp(mean(params$multi_betavl) + xb*mean(params$multi_betal))/Denominator_vac
  ## Previously tended by Other
  Denominator_other <- exp(mean(params$multi_betaov) + xb*mean(params$multi_betav)) + 
    exp(mean(params$multi_betaoo) + xb*mean(params$multi_betao)) + 
    exp(mean(params$multi_betaoc) + xb*mean(params$multi_betac)) + 
    exp(mean(params$multi_betaol) + xb*mean(params$multi_betal))
    other_vac = exp(mean(params$multi_betaov) + xb*mean(params$multi_betav))/Denominator_other
    other_other = exp(mean(params$multi_betaoo) + xb*mean(params$multi_betao))/Denominator_other 
    other_crem = exp(mean(params$multi_betaoc) + xb*mean(params$multi_betac))/Denominator_other
    other_liom = exp(mean(params$multi_betaol) + xb*mean(params$multi_betal))/Denominator_other
  ## Previously tended by Crem
  Denominator_crem <- exp(mean(params$multi_betacv) + xb*mean(params$multi_betav)) + 
    exp(mean(params$multi_betaco) + xb*mean(params$multi_betao)) + 
    exp(mean(params$multi_betacc) + xb*mean(params$multi_betac)) + 
    exp(mean(params$multi_betacl) + xb*mean(params$multi_betal))
    crem_vac = exp(mean(params$multi_betacv) + xb*mean(params$multi_betav))/Denominator_crem
    crem_other = exp(mean(params$multi_betaco) + xb*mean(params$multi_betao))/Denominator_crem 
    crem_crem = exp(mean(params$multi_betacc) + xb*mean(params$multi_betac))/Denominator_crem
    crem_liom = exp(mean(params$multi_betacl) + xb*mean(params$multi_betal))/Denominator_crem
  ## Previously tended by Liom
  Denominator_liom <- exp(mean(params$multi_betalv) + xb*mean(params$multi_betav)) + 
    exp(mean(params$multi_betalo) + xb*mean(params$multi_betao)) + 
    exp(mean(params$multi_betalc) + xb*mean(params$multi_betac)) + 
    exp(mean(params$multi_betall) + xb*mean(params$multi_betal))
    liom_vac = exp(mean(params$multi_betalv) + xb*mean(params$multi_betav))/Denominator_liom
    liom_other = exp(mean(params$multi_betalo) + xb*mean(params$multi_betao))/Denominator_liom
    liom_crem = exp(mean(params$multi_betalc) + xb*mean(params$multi_betac))/Denominator_liom
    liom_liom = exp(mean(params$multi_betall) + xb*mean(params$multi_betal))/Denominator_liom
    if(i == "liom" & j == "liom"){return(liom_liom)}
    if(i == "liom" & j == "other"){return(liom_other)}
    if(i == "liom" & j == "crem"){return(liom_crem)}
    if(i == "liom" & j == "vacant"){return(liom_vac)}
    if(i == "other" & j == "liom"){return(other_liom)}
    if(i == "other" & j == "other"){return(other_other)}
    if(i == "other" & j == "crem"){return(other_crem)}
    if(i == "other" & j == "vacant"){return(other_vac)}
    if(i == "crem" & j == "liom"){return(crem_liom)}
    if(i == "crem" & j == "other"){return(crem_other)}
    if(i == "crem" & j == "crem"){return(crem_crem)}
    if(i == "crem" & j == "vacant"){return(crem_vac)}
    if(i == "vacant" & j == "liom"){return(vac_liom)}
    if(i == "vacant" & j == "other"){return(vac_other)}
    if(i == "vacant" & j == "crem"){return(vac_crem)}
    if(i == "vacant" & j == "vacant"){return(vac_vac)}
}


#########################################################
#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE 
transition.x <- function(x,i,j,params,scenario){
  one <- transition.1(x,i,j,params,scenario)
  two <- transition.2(x,i,j,params,scenario)
  three <- transition.3(x,i,j,params)
  if(scenario == "cremvac"){return(one)}
  if(scenario == "liomvac"){return(one)}
  if(scenario == "othervac"){return(one)}
  if(scenario == "liomvacother"){return(two)}
  if(scenario ==  "liomcremvac"){return(two)}
  if(scenario == "othercremvac"){return(two)}
  if(scenario == "all"){return(three)}
}

#####################################################
#GROWTH*SURVIVAL*ANT PROBABILITIES
# ptxy <- function(x,y,i,j,params,scenario){
#   xb=pmin(pmax(x,cholla_min),cholla_max)
#   g = gxy(xb,y,i,params)
#   s = sx(xb,i,params)
#   t = transition.x(xb,i,j,params,scenario)
#   p = g*s*t
#   return(p)
# }


##################################################################################################
############################# ONE ANT MATRIX #####################################################
##################################################################################################
bigmatrix.1 <- function(params,lower,upper,matsize,lower.extension=0,upper.extension=0){
  ###################################################################################################
  ## returns the full IPM kernel (to be used in stochastic simulation), the F and T kernels, and meshpoints in the units of size
  ## params,yrfx,plotfx, and mwye get passed to the vital rate functions
  ## f.eps is fertility overdispersion. defaults to zero (see lambda.fun())
  ## lower and upper are the integration limits
  ## matsize is the dimension of the approximating matrix (it gets an additional 2 rows and columns for the seed banks)
  ###################################################################################################
  #Applying the midpoint rule
  n<-400
  L<-lower
  U<-upper+10
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
  Fmat[1,3:(n+2)]<-fx(y,"vacant",params) 
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  Tmat[2,1]<-1-invlogit(mean(params$germ1_beta0))
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))   
  # Growth/survival transitions among cts sizes
  Tmat[3:(n+2),3:(n+2)]<-t(outer(y,y,pxy,"vacant",params))*h 
  # Put it all together
  IPMmat<-Fmat+Tmat  
  colSums(Tmat[3:(n+2),3:(n+2)])
  return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat, y=y))
  #lambda = Re(eigen(IPMmat)$values[1])
  #return(lambda)
}

#################################################################################################
##################################### One Ant Species and Vacant ################################
#################################################################################################
bigmatrix.2 <- function(params,lower,upper,matsize,scenario){
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
  if(scenario == "liomvac"){
    # Banked seeds go in top row (1 == liom, 2 == vacant)
    Fmat[1,3:(n+2)]<-fx(y,"liom",params)
    Fmat[1,(n+3):(2*n+2)]<-fx(y,"vacant",params)
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit(mean(params$germ1_beta0))
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
    Tmat[(n+3):(2*n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
    Tmat[3:(n+2),1]<-0
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
    Tmat[(n+3):(2*n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
    Tmat[3:(n+2),2]<-0
    # Growth/survival transitions among cts sizes
    ## liom-liom
    Tmat[3:(n+2),3:(n+2)]<-(t(outer(y,y,pxy,i = "liom",params))*h)%*%diag(transition.x(y,i = "liom",j = "liom",params,"liomvac")) 
    ## liom-vacant
    Tmat[(n+3):(2*n+2),3:(n+2)]<-(t(outer(y,y,pxy,i = "liom",params))*h)%*%diag(transition.x(y,i = "liom",j = "vacant",params,"liomvac"))
    ## vacant-liom
    Tmat[3:(n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,i = "vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "liom",params,"liomvac"))
    ## vacant-vacant
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,i = "vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "vacant",params,"liomvac"))
    # Put it all together
    IPMmat<-Fmat+Tmat
    # Calculate the lambda
    # lambda = Re(eigen(IPMmat)$values[1])
    # return(lambda)
    return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
  }
    ############################################ CREM ###############################################
    if(scenario == "cremvac"){
      # Banked seeds go in top row (1 == crem, 2 == vacant)
      Fmat[1,3:(n+2)]<-fx(y,"crem",params)
      Fmat[1,(n+3):(2*n+2)]<-fx(y,"vacant",params)
      # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
      Tmat[2,1]<-1-invlogit(mean(params$germ1_beta0))
      # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
      # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
      Tmat[3:(n+2),1]<-0
      Tmat[(n+3):(2*n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
      # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
      # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
      Tmat[3:(n+2),2]<-0
      Tmat[(n+3):(2*n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
      # Growth/survival transitions among cts sizes
      Tmat[3:(n+2),3:(n+2)]<-(t(outer(y,y,pxy,i = "crem",params))*h)%*%diag(transition.x(y,i = "crem",j = "crem",params,"cremvac"))
      Tmat[3:(n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,i = "vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "crem",params,"cremvac"))
      Tmat[(n+3):(2*n+2),3:(n+2)]<-(t(outer(y,y,pxy,i = "crem",params))*h)%*%diag(transition.x(y,i = "crem",j = "vacant",params,"cremvac"))
      Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,i = "vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "vacant",params,"cremvac"))
      # Put it all together
      IPMmat<-Fmat+Tmat# Calculate the lambda
      # lambda = Re(eigen(IPMmat)$values[1])
      # return(lambda)
      return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
    }
    ############################################# OTHER ###########################################
    if(scenario == "othervac"){
      # Banked seeds go in top row (1 == other, 2 == vacant)
      Fmat[1,3:(n+2)]<-fx(y,"other",params)
      Fmat[1,(n+3):(2*n+2)]<-fx(y,"vacant",params)
      # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
      Tmat[2,1]<-1-invlogit(mean(params$germ1_beta0))
      # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
      # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
      Tmat[3:(n+2),1]<-0
      Tmat[(n+3):(2*n+2)]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
      # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
      # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
      Tmat[3:(n+2),2]<-0
      Tmat[(n+3):(2*n+2)]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
      # Growth/survival transitions among cts sizes
      Tmat[3:(n+2),3:(n+2)]<-(t(outer(y,y,pxy,i = "other",params))*h)%*%diag(transition.x(y,i = "other",j = "other",params,"othervac"))
      Tmat[3:(n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,"vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "other",params,"othervac"))
      Tmat[(n+3):(2*n+2),3:(n+2)]<-(t(outer(y,y,pxy,"other",params))*h)%*%diag(transition.x(y,i = "other",j = "vacant",params,"othervac"))
      Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,"vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "vacant",params,"othervac"))
      # Put it all together
      IPMmat<-Fmat+Tmat
      # Calculate the lambda
      # lambda = Re(eigen(IPMmat)$values[1])
      # return(lambda)
      return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
      }
}


#################################################################################################
###################################### THREE ANTS ###############################################
#################################################################################################

bigmatrix.3 <- function(params,lower,upper,matsize,scenario){
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
  ############################################# LIOM & CREM & VAC ###############################################
  if(scenario == "liomcremvac"){
    ## Fecundity of plants
    Fmat[1,3:(n+2)]<-fx(y,params=params,"crem") ## Production of seeds from x sized mom with no ant visitor
    Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"liom") ## Production of seeds from x sized mom with ant visitor
    Fmat[1,(2*n+3):(3*n+2)]<-fx(y,params=params,"vacant")
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit(mean(params$germ1_beta0))
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-0
    Tmat[(n+3):(2*n+2),1]<-0
    Tmat[(2*n+3):(3*n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-0
    Tmat[(n+3):(2*n+2),2]<-0
    Tmat[(2*n+3):(3*n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
    # Growth/survival transitions among cts sizes
    ##Top Row
    Tmat[3:(n+2),3:(n+2)]<- (t(outer(y,y,pxy,"crem",params))*h)%*%diag(transition.x(y,i = "crem",j = "crem",params,"liomcremvac")) ## Top First
    Tmat[3:(n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,"liom",params))*h)%*%diag(transition.x(y,i = "liom",j = "crem",params,"liomcremvac"))   ## Top Second
    Tmat[3:(n+2),(2*n+3):(3*n+2)]<-(t(outer(y,y,pxy,"vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "crem",params,"liomcremvac"))   ## Top Third
    ##Middle Row
    Tmat[(n+3):(2*n+2),3:(n+2)]<- (t(outer(y,y,pxy,"crem",params))*h)%*%diag(transition.x(y,i = "crem",j = "liom",params,"liomcremvac"))   ## Middle First
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- (t(outer(y,y,pxy,"liom",params))*h)%*%diag(transition.x(y,i = "liom",j = "liom",params,"liomcremvac"))   ## Middle Second
    Tmat[(n+3):(2*n+2),(2*n+3):(3*n+2)]<- (t(outer(y,y,pxy,"vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "liom",params,"liomcremvac"))   ## Middle Third
    ##Bottom Row
    Tmat[(2*n+3):(3*n+2),3:(n+2)]<- (t(outer(y,y,pxy,"crem",params))*h)%*%diag(transition.x(y,i = "crem",j = "vacant",params,"liomcremvac"))   ## Bottom First
    Tmat[(2*n+3):(3*n+2),(n+3):(2*n+2)]<- (t(outer(y,y,pxy,"liom",params))*h)%*%diag(transition.x(y,i = "liom",j = "vacant",params,"liomcremvac"))   ## Bottom Second
    Tmat[(2*n+3):(3*n+2),(2*n+3):(3*n+2)]<- (t(outer(y,y,pxy,"vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "vacant",params,"liomcremvac"))   ## Bottom Third
    # Put it all together
    IPMmat<-Fmat+Tmat
    # lambda = Re(eigen(IPMmat)$values[1])
    # return(lambda)
    return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
  }
  ############################################# LIOM & OTHER & VAC ###########################################
  if(scenario == "liomvacother"){
    ## Fecundity of plants
    Fmat[1,3:(n+2)]<-fx(y,params=params,"vacant") ## Production of seeds from x sized mom with no ant visitor
    Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"liom") ## Production of seeds from x sized mom with ant visitor
    Fmat[1,(2*n+3):(3*n+2)]<-fx(y,params=params,"other")
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit(mean(params$germ1_beta0))
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
    Tmat[(n+3):(2*n+2),1]<-0
    Tmat[(2*n+3):(3*n+2),1]<-0
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
    Tmat[(n+3):(2*n+2),2]<-0
    Tmat[(2*n+3):(3*n+2),2]<-0
    # Growth/survival transitions among cts sizes
    ##Top Row
    ## vacant-vacant
    Tmat[3:(n+2),3:(n+2)]<- (t(outer(y,y,pxy,"vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "vacant",params,"liomvacother"))   ## Top First
    #liom-vacant
    Tmat[3:(n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,"liom",params))*h)%*%diag(transition.x(y,i = "liom",j = "vacant",params,"liomvacother"))   ## Top Second
    #other-vacant
    Tmat[3:(n+2),(2*n+3):(3*n+2)]<-(t(outer(y,y,pxy,"other",params))*h)%*%diag(transition.x(y,i = "other",j = "vacant",params,"liomvacother"))   ## Top Third
    ##Middle Row
    #vacant-liom
    Tmat[(n+3):(2*n+2),3:(n+2)]<- (t(outer(y,y,pxy,"vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "liom",params,"liomvacother"))   ## Middle First
    #liom-liom
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- (t(outer(y,y,pxy,"liom",params))*h)%*%diag(transition.x(y,i = "liom",j = "liom",params,"liomvacother"))   ## Middle Second
    #other-liom
    Tmat[(n+3):(2*n+2),(2*n+3):(3*n+2)]<- (t(outer(y,y,pxy,"other",params))*h)%*%diag(transition.x(y,i = "other",j = "liom",params,"liomvacother"))   ## Middle Third
    ##Bottom Row
    #vacant-other
    Tmat[(2*n+3):(3*n+2),3:(n+2)]<- (t(outer(y,y,pxy,"vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "other",params,"liomvacother"))   ## Bottom First
    #liom-other
    Tmat[(2*n+3):(3*n+2),(n+3):(2*n+2)]<- (t(outer(y,y,pxy,"liom",params))*h)%*%diag(transition.x(y,i = "liom",j = "other",params,"liomvacother"))   ## Bottom Second
    #other-other
    Tmat[(2*n+3):(3*n+2),(2*n+3):(3*n+2)]<- (t(outer(y,y,pxy,"other",params))*h)%*%diag(transition.x(y,i = "other",j = "other",params,"liomvacother"))   ## Bottom Third
    # Put it all together
    IPMmat<-Fmat+Tmat
    # lambda = Re(eigen(IPMmat)$values[1])
    # return(lambda)
    return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
  }
  ############################################# CREM & OTHER & VAC ############################################
  if(scenario == "othercremvac"){
    ## Fecundity of plants
    Fmat[1,3:(n+2)]<-fx(y,params=params,"crem") ## Production of seeds from x sized mom with no ant visitor
    Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"vacant") ## Production of seeds from x sized mom with ant visitor
    Fmat[1,(2*n+3):(3*n+2)]<-fx(y,params=params,"other")
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit(mean(params$germ1_beta0))
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-0
    Tmat[(n+3):(2*n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
    Tmat[(2*n+3):(3*n+2),1]<-0
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-0
    Tmat[(n+3):(2*n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
    Tmat[(2*n+3):(3*n+2),2]<-0
    # Growth/survival transitions among cts sizes
    ##Top Row
    Tmat[3:(n+2),3:(n+2)]<- (t(outer(y,y,pxy,"crem",params))*h)%*%diag(transition.x(y,i = "crem",j = "crem",params,"othercremvac"))   ## Top First
    Tmat[3:(n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,"vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "crem",params,"othercremvac"))   ## Top Second
    Tmat[3:(n+2),(2*n+3):(3*n+2)]<-(t(outer(y,y,pxy,"other",params))*h)%*%diag(transition.x(y,i = "other",j = "crem",params,"othercremvac"))   ## Top Third
    ##Middle Row
    Tmat[(n+3):(2*n+2),3:(n+2)]<- (t(outer(y,y,pxy,"crem",params))*h)%*%diag(transition.x(y,i = "crem",j = "vacant",params,"othercremvac"))   ## Middle First
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- (t(outer(y,y,pxy,"vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "vacant",params,"othercremvac"))   ## Middle Second
    Tmat[(n+3):(2*n+2),(2*n+3):(3*n+2)]<- (t(outer(y,y,pxy,"other",params))*h)%*%diag(transition.x(y,i = "other",j = "vacant",params,"othercremvac"))   ## Middle Third
    ##Bottom Row
    Tmat[(2*n+3):(3*n+2),3:(n+2)]<- (t(outer(y,y,pxy,"crem",params))*h)%*%diag(transition.x(y,i = "crem",j = "other",params,"othercremvac"))   ## Bottom First
    Tmat[(2*n+3):(3*n+2),(n+3):(2*n+2)]<- (t(outer(y,y,pxy,"vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "other",params,"othercremvac"))   ## Bottom Second
    Tmat[(2*n+3):(3*n+2),(2*n+3):(3*n+2)]<- (t(outer(y,y,pxy,"other",params))*h)%*%diag(transition.x(y,i = "other",j = "other",params,"othercremvac"))   ## Bottom Third
    # Put it all together
    IPMmat<-Fmat+Tmat
    # lambda = Re(eigen(IPMmat)$values[1])
    # return(lambda)
    return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
  }
}


##################################################################################################
######################################### ALL ANTS PRESENT #######################################
##################################################################################################

bigmatrix.4 <- function(params,lower,upper,matsize,scenario){
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
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  Tmat[2,1]<-1-1 
  #Tmat[2,1]<-1-invlogit(mean(params$germ1_beta0)) 
############################################# LIOM & CREM & OTHER & VAC ############################################
    ## Fecundity of plants
    Fmat[1,3:(n+2)]<-fx(y,params=params,"crem") ## Production of seeds from x sized mom with no ant visitor
    Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"liom") ## Production of seeds from x sized mom with ant visitor
    Fmat[1,(2*n+3):(3*n+2)]<-fx(y,params=params,"other")
    Fmat[1,(3*n+3):(4*n+2)]<-fx(y,params=params,"vacant")
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-0
    Tmat[(n+3):(2*n+2),1]<-0
    Tmat[(2*n+3):(3*n+2),1]<-0
    Tmat[(3*n+3):(4*n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-0
    Tmat[(n+3):(2*n+2),2]<-0
    Tmat[(2*n+3):(3*n+2),2]<-0
    Tmat[(3*n+3):(4*n+2),1]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
    # Growth/survival transitions among cts sizes
    ##Top Row
    Tmat[3:(n+2),3:(n+2)]<- (t(outer(y,y,pxy,"crem",params))*h)%*%diag(transition.x(y,i = "crem",j = "crem",params,"all"))   ## Top First
    Tmat[3:(n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,"liom",params))*h)%*%diag(transition.x(y,i = "liom",j = "crem",params,"all"))   ## Top Second
    Tmat[3:(n+2),(2*n+3):(3*n+2)]<-(t(outer(y,y,pxy,"other",params))*h)%*%diag(transition.x(y,i = "other",j = "crem",params,"all"))   ## Top Third
    Tmat[3:(n+2),(3*n+3):(4*n+2)]<-(t(outer(y,y,pxy,"vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "crem",params,"all"))
    ##Second Row
    Tmat[(n+3):(2*n+2),3:(n+2)]<- (t(outer(y,y,pxy,"crem",params))*h)%*%diag(transition.x(y,i = "crem",j = "liom",params,"all"))   ## Top First
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,"liom",params))*h)%*%diag(transition.x(y,i = "liom",j = "liom",params,"all"))   ## Top Second
    Tmat[(n+3):(2*n+2),(2*n+3):(3*n+2)]<-(t(outer(y,y,pxy,"other",params))*h)%*%diag(transition.x(y,i = "other",j = "liom",params,"all"))   ## Top Third
    Tmat[(n+3):(2*n+2),(3*n+3):(4*n+2)]<-(t(outer(y,y,pxy,"vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "liom",params,"all"))
    ##Third Row
    Tmat[(2*n+3):(3*n+2),3:(n+2)]<- (t(outer(y,y,pxy,"crem",params))*h)%*%diag(transition.x(y,i = "crem",j = "other",params,"all"))   ## Top First
    Tmat[(2*n+3):(3*n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,"liom",params))*h)%*%diag(transition.x(y,i = "liom",j = "other",params,"all"))   ## Top Second
    Tmat[(2*n+3):(3*n+2),(2*n+3):(3*n+2)]<-(t(outer(y,y,pxy,"other",params))*h)%*%diag(transition.x(y,i = "other",j = "other",params,"all"))   ## Top Third
    Tmat[(2*n+3):(3*n+2),(3*n+3):(4*n+2)]<-(t(outer(y,y,pxy,"vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "other",params,"all"))
    ##Bottom Row
    Tmat[(3*n+3):(4*n+2),3:(n+2)]<- (t(outer(y,y,pxy,"crem",params))*h)%*%diag(transition.x(y,i = "crem",j = "vacant",params,"all"))   ## Top First
    Tmat[(3*n+3):(4*n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,"liom",params))*h)%*%diag(transition.x(y,i = "liom",j = "vacant",params,"all"))   ## Top Second
    Tmat[(3*n+3):(4*n+2),(2*n+3):(3*n+2)]<-(t(outer(y,y,pxy,"other",params))*h)%*%diag(transition.x(y,i = "other",j = "vacant",params,"all"))   ## Top Third
    Tmat[(3*n+3):(4*n+2),(3*n+3):(4*n+2)]<-(t(outer(y,y,pxy,"vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "vacant",params,"all"))
    # Put it all together
    IPMmat<-Fmat+Tmat
    # lambda = Re(eigen(IPMmat)$values[1])
    # return(lambda)
    return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
}



#################################################################################################
############################ CHOOSE WHICH SCENARIO (COMBO OF ANTS) ##############################
#################################################################################################

bigmatrix<-function(params,lower,upper,matsize,scenario){  
  ###################################################################################################
  ## returns the full IPM kernel (to be used in stochastic simulation), the F and T kernels, and meshpoints in the units of size
  ## params,yrfx,plotfx, and mwye get passed to the vital rate functions
  ## f.eps is fertility overdispersion. defaults to zero (see lambda.fun())
  ## lower and upper are the integration limits
  ## matsize is the dimension of the approximating matrix (it gets an additional 2 rows and columns for the seed banks)
  ###################################################################################################
  ## Scenario options are "liomvacother", "liomcremvac", "othercremvac", 
  ## "othervac", "liomvac", "cremvac", "all", "none"
  
  if(scenario == "none"){
    list = (bigmatrix.1(params,lower,upper,matsize))
    return(list)
  }
  if(scenario == "liomvac"){
    list = (bigmatrix.2(params,lower,upper,matsize,scenario))
    return(list)
  }
  if(scenario == "cremvac"){
    list = (bigmatrix.2(params,lower,upper,matsize,scenario))
    return(list)
  }
  if(scenario == "othervac"){
    list = (bigmatrix.2(params,lower,upper,matsize,scenario))
    return(list)
  }
  if(scenario == "liomvacother"){
    list = (bigmatrix.3(params,lower,upper,matsize,scenario))
    return(list)
  }
  if(scenario == "liomcremvac"){
    list = (bigmatrix.3(params,lower,upper,matsize,scenario))
    return(list)
  }
  if(scenario == "othercremvac"){
    list = (bigmatrix.3(params,lower,upper,matsize,scenario))
    return(list)
  }
  if(scenario == "all"){
    list = (bigmatrix.4(params,lower,upper,matsize,scenario))
    return(list)
  }
} 


#########################################################################################################
##            This will be an IPM which allows you to choose how many ants are present
#########################################################################################################
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code")
#source("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code/Params.R")
## ----------- Miscellany...we'll need an inverse logit functions ------------- ##
invlogit<-function(x){exp(x)/(1+exp(x))}

## ----------- Vital rate functions. Parameter indices are hard-coded and must correspond to rows of MCMC matrix ------------- ##
##############################################
#GROWTH FROM SIZE X TO Y. Returns the probability of growth from size x to y based on ant state
gxy<-function(x,y,i,params){
  #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  xb=pmin(pmax(x,cholla_min),cholla_max) 
  #Density probability function which uses the parameters that are ant specific 
  g_vac = dnorm(y,mean=mean(params$grow_beta01) + mean(params$grow_beta11)*xb,sd=exp(mean(params$grow_sig0) + mean(params$grow_sig1)*xb))
  g_liom = dnorm(y,mean=mean(params$grow_beta04) + mean(params$grow_beta14)*xb,sd=exp(mean(params$grow_sig0) + mean(params$grow_sig1)*xb))
  g_crem = dnorm(y,mean=mean(params$grow_beta03) + mean(params$grow_beta13)*xb,sd=exp(mean(params$grow_sig0) + mean(params$grow_sig1)*xb))
  g_other = dnorm(y,mean=mean(params$grow_beta02) + mean(params$grow_beta12)*xb,sd=exp(mean(params$grow_sig0) + mean(params$grow_sig1)*xb))
  #Return the probability of growing from size x to y
  if(i == "crem"){ return(g_crem)}
  if(i == "liom"){ return(g_liom)}
  if(i == "other"){ return(g_other)}
  if(i == "vacant"){ return(g_vac)}
}


#################################################
#SURVIVAL AT SIZE X. Returns the probability of survival of a cactus based on size and ant state
sx<-function(x,i,params){
  #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  xb=pmin(pmax(x,cholla_min),cholla_max)
  #Transform the ant specific parameters to the probability of survival
  s_crem = invlogit(mean(params$surv_beta03) + mean(params$surv_beta13)*xb)
  s_vac = invlogit(mean(params$surv_beta01) + mean(params$surv_beta11)*xb)
  s_other = invlogit(mean(params$surv_beta02) + mean(params$surv_beta12)*xb)
  s_liom = invlogit(mean(params$surv_beta04) + mean(params$surv_beta14)*xb)
  #Return the survival probabilities
  if(i == "crem"){ return(1)}
  if(i == "liom"){ return(1)}
  if(i == "other"){ return(1)}
  if(i == "vacant"){ return(1)}
}


#################################################
#SURVIVAL*GROWTH. Combine the survival and growth probabilities
pxy<-function(x,y,i,params){
  #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  xb=pmin(pmax(x,cholla_min),cholla_max)
  pxy = sx(xb,i,params)*gxy(xb,y,i,params)
  return(pxy)
}


#################################################################
#PRODUCTION OF 1-YO SEEDS IN THE SEED BANK FROM X-SIZED MOMS
fx<-function(x,i,params){
  #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  xb=pmin(pmax(x,cholla_min),cholla_max)
  p.flow<-invlogit(mean(params$repro_beta0) + mean(params$repro_beta1)*xb)      ## Probability of Reproducing
  nflow<-exp(mean(params$flow_beta0) + mean(params$flow_beta1)*xb)      ## Number of FLowers produced
  flow.surv_crem<-invlogit(mean(params$viab_beta01))      ## Proportion of Flowers survive to fruit
  flow.surv_vac<-invlogit(mean(params$viab_beta04))       ## Proportion of Flowers survive to fruit
  flow.surv_other<-invlogit(mean(params$viab_beta03))       ## Proportion of Flowers survive to fruit
  flow.surv_liom<-invlogit(mean(params$viab_beta02))      ## Proportion of Flowers survive to fruit
  seeds.per.fruit_crem<-mean(params$seed_beta01)      ## Number of Seeds per Fruit
  seeds.per.fruit_liom<-mean(params$seed_beta02)      ## Number of Seeds per Fruit
  seeds.per.fruit_vac<-mean(params$seed_beta03)     ## Number of Seeds per Fruit
  seed.survival<-invlogit(mean(params$preseed_beta0))^2       ## Seed per Fruit Survival ---------I measured 6-month seed survival; annual survival is its square
  #Calculate the fecundity probabilities by ant species
  f_crem = p.flow*nflow*flow.surv_crem*seeds.per.fruit_crem*seed.survival
  f_vac = p.flow*nflow*flow.surv_vac*seeds.per.fruit_vac*seed.survival
  f_other = p.flow*nflow*flow.surv_other*seeds.per.fruit_vac*seed.survival
  f_liom = p.flow*nflow*flow.surv_liom*seeds.per.fruit_liom*seed.survival
  if(i == "crem"){ return(f_crem)}
  if(i == "liom"){ return(f_liom)}
  if(i == "other"){ return(f_other)}
  if(i == "vacant"){ return(f_vac)}
}


#####################################################
#### Recruitment. Calculate the probability of a recruit being a certain size y
recruits<-function(y,params){
  #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  #yb=pmin(pmax(y,cholla_min),cholla_max)
  ## Calculate the probability and return it
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
    Denominator_crem <- exp(mean(params$multi_betacc) + xb*mean(params$multi_betav)) + 
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
    Denominator_crem <- exp(mean(params$multi_betacc) + xb*mean(params$multi_betav)) + 
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

## Scenario options are "liomvacother", "liomcremother", "liomcremvac", "othercremvac"

## Check if it works
i = c("liom","vacant","other","other")
j = c("vacant","liom","other","liom")
x = c(15,15,15,15)
y = c(-1,-4,4.5,3.01)
scenario = "liomvacother"
t2 <- vector()
for(n in 1:length(i)){
  t2[n] <- transition.2(x[n],i[n],j[n],params,scenario)
}
t2


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
transition.3(2,"crem","liom",params)

## Chekc if it works
i = c("liom","liom")
j = c("vacant","vacant")
x = c(-1,-5)
y = c(-1,-5)
t3 <- vector()
for(n in 1:length(i)){
  t3[n] <- transition.3(x[n],i[n],j[n],params)
}
t3



#########################################################
#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE 
transition.x <- function(x,i,j,num_ants,params,scenario){
  ifelse( num_ants == 1, transition.1(x,i,j,params,scenario), 
          ifelse( num_ants == 2, transition.2(x,i,j,params,scenario),
                  transition.3(x,i,j,params)))
}

## Check if it works
i = c("liom","vacant","liom","vacant")
j = c("vacant","liom","liom","liom")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
scenario = "all"
num_ants = 3
t <- vector()
for(n in 1:length(i)){
  t[n] <- transition.x(x[n],i[n],j[n],3,params,scenario)
}
t
i = c("liom","vacant","liom","vacant")
j = c("vacant","liom","liom","liom")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
scenario = "liomcremvac"
num_ants = 2
t <- vector()
for(n in 1:length(i)){
  t[n] <- transition.x(x[n],i[n],j[n],3,params,scenario)
}
t
i = c("liom","vacant","liom","vacant")
j = c("vacant","liom","liom","liom")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
scenario = "liomvac"
num_ants = 1
t <- vector()
for(n in 1:length(i)){
  t[n] <- transition.x(x[n],i[n],j[n],3,params,scenario)
}
t

#####################################################
#GROWTH*SURVIVAL*ANT PROBABILITIES
ptxy <- function(x,y,i,j,num_ants,params,scenario){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  g = gxy(xb,y,i,params)
  s = sx(xb,i,params)
  t = transition.x(xb,i,j,num_ants,params,scenario)
  p = g*s*t
  return(p)
}

## Check if it works
i = c("liom","vacant","crem","other")
j = c("vacant","crem","crem","liom")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
num_ants = 3
scenario = "all"
pt <- vector()
for(n in 1:length(i)){
  pt[n] <- ptxy(x[n],y[n],i[n],j[n],num_ants,params,scenario)
}
pt


##################################################################################################
############################# ONE ANT MATRIX #####################################################
##################################################################################################
bigmatrix.1 <- function(params,lower,upper,matsize,num_ants,i){
  ###################################################################################################
  ## returns the full IPM kernel (to be used in stochastic simulation), the F and T kernels, and meshpoints in the units of size
  ## params,yrfx,plotfx, and mwye get passed to the vital rate functions
  ## f.eps is fertility overdispersion. defaults to zero (see lambda.fun())
  ## lower and upper are the integration limits
  ## matsize is the dimension of the approximating matrix (it gets an additional 2 rows and columns for the seed banks)
  ###################################################################################################
  #Applying the midpoint rule
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
  Fmat[1,3:(n+2)]<-fx(y,i,params) 
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  Tmat[2,1]<-1-invlogit(mean(params$germ1_beta0))
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))   
  # Growth/survival transitions among cts sizes
  Tmat[3:(n+2),3:(n+2)]<-t(outer(y,y,pxy,i,params))*h 
  # Put it all together
  IPMmat<-Fmat+Tmat  
  return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
  #lambda = Re(eigen(IPMmat)$values[1])
  #return(lambda)
}

View(bigmatrix.1(params,lower,upper,matsize,1,"vacant")$IPMmat)

## Check that it works
i = c("vacant","vacant")
big1 <- list()
for(n in 1:length(i)){
  big1[[n]] <- lambda(bigmatrix.1(params,lower=cholla_min-0,upper=cholla_max+0,matsize,1,"vacant")$IPMmat)
}
big1

## diagnostics:
# This diagnostic shows that the columns should sum to the survival function of the vacant
testmat <- bigmatrix.1(params,lower=cholla_min-15,upper=cholla_max+2,matsize,1,"vacant")$Tmat
surv <- colSums(testmat)
plot(surv,ylim=c(0,1))



#################################################################################################
##################################### One Ant Species and Vacant ################################
#################################################################################################
bigmatrix.2 <- function(params,lower,upper,matsize,num_ants,i,j,scenario){
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
    Tmat[3:(n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
    Tmat[(n+3):(2*n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
    Tmat[(n+3):(2*n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
    # Growth/survival transitions among cts sizes
    Tmat[3:(n+2),3:(n+2)]<-t(outer(y,y,ptxy,i = "liom",j = "liom",num_ants,params,"liomvac"))*h
    Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,i = "liom",j = "vacant",num_ants,params,"liomvac"))*h
    Tmat[(n+3):(2*n+2),3:(n+2)]<-t(outer(y,y,ptxy,i = "vacant",j = "liom",num_ants,params,"liomvac"))*h
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,i = "vacant",j = "vacant",num_ants,params,"liomvac"))*h
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
      Tmat[3:(n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
      Tmat[(n+3):(2*n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
      # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
      Tmat[3:(n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
      Tmat[(n+3):(2*n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
      # Growth/survival transitions among cts sizes
      Tmat[3:(n+2),3:(n+2)]<-t(outer(y,y,ptxy,"crem","crem",num_ants,params,scenario))*h
      Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,"crem","vacant",num_ants,params,scenario))*h
      Tmat[(n+3):(2*n+2),3:(n+2)]<-t(outer(y,y,ptxy,"vacant","crem",num_ants,params,scenario))*h
      Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,"vacant","vacant",num_ants,params,scenario))*h
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
      Tmat[3:(n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
      Tmat[(n+3):(2*n+2)]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
      # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
      Tmat[3:(n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
      Tmat[(n+3):(2*n+2)]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
      # Growth/survival transitions among cts sizes
      Tmat[3:(n+2),3:(n+2)]<-t(outer(y,y,ptxy,"other","other",num_ants,params,scenario))*h
      Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,"other","vacant",num_ants,params,scenario))*h
      Tmat[(n+3):(2*n+2),3:(n+2)]<-t(outer(y,y,ptxy,"vacant","other",num_ants,params,scenario))*h
      Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,"vacant","vacant",num_ants,params,scenario))*h
      # Put it all together
      IPMmat<-Fmat+Tmat
      # Calculate the lambda
      # lambda = Re(eigen(IPMmat)$values[1])
      # return(lambda)
      return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
      }
}

bigmatrix.2(params,lower,upper,matsize,1,"crem","vacant","cremvac")$IPMmat


num_ants = 1
i = c("liom","vacant","other","crem")
j = c("vacant","other","vacant","vacant")
scenario <- c("liomvac","othervac","othervac","cremvac")
bmat <- vector()
for(n in 1:length(i)){
  bmat[n] <- lambda(bigmatrix.2(params,lower,upper,matsize,num_ants,i[n],j[n],scenario[n])$IPMmat)
}
bmat


#################################################################################################
###################################### THREE ANTS ###############################################
#################################################################################################

bigmatrix.3 <- function(params,lower,upper,matsize,num_ants,i,j,scenario){
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
    Tmat[3:(n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    Tmat[(n+3):(2*n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    Tmat[(2*n+3):(3*n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    Tmat[(n+3):(2*n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    Tmat[(2*n+3):(3*n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    # Growth/survival transitions among cts sizes
    ##Top Row
    Tmat[3:(n+2),3:(n+2)]<- t(outer(y,y,ptxy,"crem","crem",num_ants,params,scenario))*h   ## Top First
    Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,"liom","crem",num_ants,params,scenario))*h   ## Top Second
    Tmat[3:(n+2),(2*n+3):(3*n+2)]<-t(outer(y,y,ptxy,"vacant","crem",num_ants,params,scenario))*h   ## Top Third
    ##Middle Row
    Tmat[(n+3):(2*n+2),3:(n+2)]<- t(outer(y,y,ptxy,"crem","liom",num_ants,params,scenario))*h   ## Middle First
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,"liom","liom",num_ants,params,scenario))*h   ## Middle Second
    Tmat[(n+3):(2*n+2),(2*n+3):(3*n+2)]<- t(outer(y,y,ptxy,"vacant","liom",num_ants,params,scenario))*h   ## Middle Third
    ##Bottom Row
    Tmat[(2*n+3):(3*n+2),3:(n+2)]<- t(outer(y,y,ptxy,"crem","vacant",num_ants,params,scenario))*h   ## Bottom First
    Tmat[(2*n+3):(3*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,"liom","vacant",num_ants,params,scenario))*h   ## Bottom Second
    Tmat[(2*n+3):(3*n+2),(2*n+3):(3*n+2)]<- t(outer(y,y,ptxy,"vacant","vacant",num_ants,params,scenario))*h   ## Bottom Third
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
    Tmat[3:(n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    Tmat[(n+3):(2*n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    Tmat[(2*n+3):(3*n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    Tmat[(n+3):(2*n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    Tmat[(2*n+3):(3*n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    # Growth/survival transitions among cts sizes
    ##Top Row
    Tmat[3:(n+2),3:(n+2)]<- t(outer(y,y,ptxy,"vacant","vacant",num_ants,params,scenario))*h   ## Top First
    Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,"liom","vacant",num_ants,params,scenario))*h   ## Top Second
    Tmat[3:(n+2),(2*n+3):(3*n+2)]<-t(outer(y,y,ptxy,"other","vacant",num_ants,params,scenario))*h   ## Top Third
    ##Middle Row
    Tmat[(n+3):(2*n+2),3:(n+2)]<- t(outer(y,y,ptxy,"vacant","liom",num_ants,params,scenario))*h   ## Middle First
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,"liom","liom",num_ants,params,scenario))*h   ## Middle Second
    Tmat[(n+3):(2*n+2),(2*n+3):(3*n+2)]<- t(outer(y,y,ptxy,"other","liom",num_ants,params,scenario))*h   ## Middle Third
    ##Bottom Row
    Tmat[(2*n+3):(3*n+2),3:(n+2)]<- t(outer(y,y,ptxy,"vacant","other",num_ants,params,scenario))*h   ## Bottom First
    Tmat[(2*n+3):(3*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,"liom","other",num_ants,params,scenario))*h   ## Bottom Second
    Tmat[(2*n+3):(3*n+2),(2*n+3):(3*n+2)]<- t(outer(y,y,ptxy,"other","other",num_ants,params,scenario))*h   ## Bottom Third
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
    Tmat[3:(n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    Tmat[(n+3):(2*n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    Tmat[(2*n+3):(3*n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    Tmat[(n+3):(2*n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    Tmat[(2*n+3):(3*n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    # Growth/survival transitions among cts sizes
    ##Top Row
    Tmat[3:(n+2),3:(n+2)]<- t(outer(y,y,ptxy,"crem","crem",num_ants,params,scenario))*h   ## Top First
    Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,"vacant","crem",num_ants,params,scenario))*h   ## Top Second
    Tmat[3:(n+2),(2*n+3):(3*n+2)]<-t(outer(y,y,ptxy,"other","crem",num_ants,params,scenario))*h   ## Top Third
    ##Middle Row
    Tmat[(n+3):(2*n+2),3:(n+2)]<- t(outer(y,y,ptxy,"crem","vacant",num_ants,params,scenario))*h   ## Middle First
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,"vacant","vacant",num_ants,params,scenario))*h   ## Middle Second
    Tmat[(n+3):(2*n+2),(2*n+3):(3*n+2)]<- t(outer(y,y,ptxy,"other","vacant",num_ants,params,scenario))*h   ## Middle Third
    ##Bottom Row
    Tmat[(2*n+3):(3*n+2),3:(n+2)]<- t(outer(y,y,ptxy,"crem","other",num_ants,params,scenario))*h   ## Bottom First
    Tmat[(2*n+3):(3*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,"vacant","other",num_ants,params,scenario))*h   ## Bottom Second
    Tmat[(2*n+3):(3*n+2),(2*n+3):(3*n+2)]<- t(outer(y,y,ptxy,"other","other",num_ants,params,scenario))*h   ## Bottom Third
    # Put it all together
    IPMmat<-Fmat+Tmat
    # lambda = Re(eigen(IPMmat)$values[1])
    # return(lambda)
    return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
  }
}
## Scenario options are "liomvacother", "liomcremother", "liomcremvac", "othercremvac"


bigmatrix.3(params,lower,upper,matsize,2,"liom","liom","liomcremvac")
bigmatrix.3(params,lower,upper,matsize,2,"vacant","vacant","liomvacother")
bigmatrix.3(params,lower,upper,matsize,2,"other","crem","othercremvac")




##################################################################################################
######################################### ALL ANTS PRESENT #######################################
##################################################################################################

bigmatrix.4 <- function(params,lower,upper,matsize,num_ants,i,j,scenario){
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
  
  # Banked seeds go in top row
  Fmat[1,3:(n+2)]<-fx(y,i,params) 
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  Tmat[2,1]<-1-invlogit(mean(params$germ1_beta0)) 
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))   
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))   
  # Growth/survival transitions among cts sizes
  Tmat[3:(n+2),3:(n+2)]<-t(outer(y,y,pxy,i,params))*h 
  
  
  
  ############################################# LIOM & CREM & OTHER & VAC ############################################
    ## Fecundity of plants
    Fmat[1,3:(n+2)]<-fx(y,params=params,"crem") ## Production of seeds from x sized mom with no ant visitor
    Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"liom") ## Production of seeds from x sized mom with ant visitor
    Fmat[1,(2*n+3):(3*n+2)]<-fx(y,params=params,"other")
    Fmat[1,(3*n+3):(4*n+2)]<-fx(y,params=params,"vacant")
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit(mean(params$germ1_beta0))
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    Tmat[(n+3):(2*n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    Tmat[(2*n+3):(3*n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    Tmat[(3*n+3):(4*n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(TRUE)
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    Tmat[(n+3):(2*n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    Tmat[(2*n+3):(3*n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(FALSE)
    Tmat[(3*n+3):(4*n+2),1]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))*beta(TRUE)
    # Growth/survival transitions among cts sizes
    ##Top Row
    Tmat[3:(n+2),3:(n+2)]<- t(outer(y,y,ptxy,"crem","crem",4,params,scenario))*h   ## Top First
    Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,"liom","crem",4,params,scenario))*h   ## Top Second
    Tmat[3:(n+2),(2*n+3):(3*n+2)]<-t(outer(y,y,ptxy,"other","crem",4,params,scenario))*h   ## Top Third
    Tmat[3:(n+2),(3*n+3):(4*n+2)]<-t(outer(y,y,ptxy,"vacant","crem",4,params,scenario))*h   ## Top Fourth
    ##Middle Row
    Tmat[(n+3):(2*n+2),3:(n+2)]<- t(outer(y,y,ptxy,"crem","liom",4,params,scenario))*h   ## Middle First
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,"liom","liom",4,params,scenario))*h   ## Middle Second
    Tmat[(n+3):(2*n+2),(2*n+3):(3*n+2)]<- t(outer(y,y,ptxy,"other","liom",4,params,scenario))*h   ## Middle Third
    Tmat[(n+3):(2*n+2),(3*n+3):(4*n+2)]<-t(outer(y,y,ptxy,"vacant","liom",4,params,scenario))*h ## Middle Fourth
    ##Middle 2 Row
    Tmat[(2*n+3):(3*n+2),3:(n+2)]<- t(outer(y,y,ptxy,"crem","other",4,params,scenario))*h   ## Middle 2 First
    Tmat[(2*n+3):(3*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,"liom","other",4,params,scenario))*h   ## Middle 2 Second
    Tmat[(2*n+3):(3*n+2),(2*n+3):(3*n+2)]<- t(outer(y,y,ptxy,"other","other",4,params,scenario))*h   ## Middle 2 Third
    Tmat[(2*n+3):(3*n+2),(3*n+3):(4*n+2)]<-t(outer(y,y,ptxy,"vacant","other",4,params,scenario))*h   ## Middle 2 Fourth
    #Bottom Row
    Tmat[(3*n+3):(4*n+2),3:(n+2)]<- t(outer(y,y,ptxy,"crem","vacant",4,params,scenario))*h   ## Bottom First
    Tmat[(3*n+3):(4*n+2),(n+3):(2*n+2)]<- t(outer(y,y,ptxy,"liom","vacant",4,params,scenario))*h   ## Bottom Second
    Tmat[(3*n+3):(4*n+2),(2*n+3):(3*n+2)]<- t(outer(y,y,ptxy,"other","vacant",4,params,scenario))*h   ## Bottom Third
    Tmat[(3*n+3):(4*n+2),(3*n+3):(4*n+2)]<-t(outer(y,y,ptxy,"vacant","vacant",4,params,scenario))*h   ## Bottom Fourth
    # Put it all together
    IPMmat<-Fmat+Tmat
    # lambda = Re(eigen(IPMmat)$values[1])
    # return(lambda)
    return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
}

bigmatrix.4(params,lower,upper,matsize,3,"vacant","vacant","all")




#################################################################################################
############################ CHOOSE WHICH SCENARIO (COMBO OF ANTS) ##############################
#################################################################################################

bigmatrix<-function(params,lower,upper,matsize,num_ants,i,j,scenario){  
  ###################################################################################################
  ## returns the full IPM kernel (to be used in stochastic simulation), the F and T kernels, and meshpoints in the units of size
  ## params,yrfx,plotfx, and mwye get passed to the vital rate functions
  ## f.eps is fertility overdispersion. defaults to zero (see lambda.fun())
  ## lower and upper are the integration limits
  ## matsize is the dimension of the approximating matrix (it gets an additional 2 rows and columns for the seed banks)
  ###################################################################################################
  if(num_ants == 0){
    lambda = bigmatrix.1(params,lower,upper,matsize,num_ants,i)
    return(lambda)
  }
  if(num_ants == 1){
    lambda = bigmatrix.2(params,lower,upper,matsize,num_ants,i,j,scenario)
    return(lambda)
  }
  if(num_ants == 2){
    lambda = bigmatrix.3(params,lower,upper,matsize,num_ants,i,j,scenario)
    return(lambda)
  }
  if(num_ants == 3){
    lambda = bigmatrix.4(params,lower,upper,matsize,num_ants,i,j,scenario)
    return(lambda)
  }
} 
## One ant option
bigmatrix(params,lower,upper,matsize,0,i = "vacant",j = "vacant","none")
bigmatrix.1(params,lower,upper,matsize,0,"vacant")
## 2 ant options
bigmatrix(params,lower,upper,matsize,1,"crem","vacant","cremvac")
bigmatrix.2(params,lower,upper,matsize,1,"liom","vacant","liomvac")
## 3 ant options
bigmatrix(params,lower,upper,matsize,2,"crem","vacant","liomcremvac")
bigmatrix.3(params,lower,upper,matsize,2,"crem","vacant","liomcremvac")
## all ant options
bigmatrix(params,lower,upper,matsize,3,"crem","vacant","all")
bigmatrix.4(params,lower,upper,matsize,3,"crem","vacant","all")

## num_ants = 0 ----> no transitions (associated with bigmatrix1)
## num_ants = 1 ----> one transition (one ant and vacant) (associated with bigmatrix2)
## num_ants = 2 ----> two transitions (two ants and vacant) (associated with bigmatrix3)
## num_ants = 3 ----> three transitions (three ants and vacant) (associated with bigmatrix4)




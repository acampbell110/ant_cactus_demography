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
gxy<-function(x,y,i,params){
  xb=pmin(pmax(x,cholla_min),cholla_max) #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  g_vac = dnorm(y,mean=(params$grow_beta01) + (params$grow_beta11)*xb,sd=exp((params$grow_sig0) + (params$grow_sig1)*xb))
  g_liom = dnorm(y,mean=(params$grow_beta04) + (params$grow_beta14)*xb,sd=exp((params$grow_sig0) + (params$grow_sig1)*xb))
  g_crem = dnorm(y,mean=(params$grow_beta03) + (params$grow_beta13)*xb,sd=exp((params$grow_sig0) + (params$grow_sig1)*xb))
  g_other = dnorm(y,mean=(params$grow_beta02) + (params$grow_beta12)*xb,sd=exp((params$grow_sig0) + (params$grow_sig1)*xb))
  if(i == "crem"){ return(g_crem)}
  if(i == "liom"){ return(g_liom)}
  if(i == "other"){ return(g_other)}
  if(i == "vacant"){ return(g_vac)}
}

##Check that it works properly
i = c("vacant","crem","liom","other")
x = c(-1,-5,3,4)
y = c(-1,-4,3,4)
i = "crem"
x = 1
y = 3
g <- matrix(NA,ncol = length(i), nrow = 100)
for(m in 1:nrow(params)){
  for(n in seq(1:length(i))){
    g[m,n] <- gxy(x[n],y[n],i[n],params[m,])
    }
}

g


#################################################
#SURVIVAL AT SIZE X.
sx<-function(x,i,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  s_crem = invlogit((params$surv_beta03) + (params$surv_beta13)*xb)
  s_vac = invlogit((params$surv_beta01) + (params$surv_beta11)*xb)
  s_other = invlogit((params$surv_beta02) + (params$surv_beta12)*xb)
  s_liom = invlogit((params$surv_beta04) + (params$surv_beta14)*xb)
  if(i == "crem"){ return(s_crem)}
  if(i == "liom"){ return(s_liom)}
  if(i == "other"){ return(s_other)}
  if(i == "vacant"){ return(s_vac)}
}

##Check that it works properly
i = c("liom","vacant","crem","other")
x = c(-1,-5,4,3)
i = "liom"
s <- matrix(NA,ncol = length(i), nrow = 100)
for(m in 1:nrow(params)){
  for(n in seq(1:length(i))){
    s[m,n] <- sx(x[n],i[n],params[m,])
  }
}

s


#################################################
#SURVIVAL*GROWTH
pxy<-function(x,y,i,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  pxy = sx(x,i,params)*gxy(x,y,i,params)
  return(pxy)
}
pxy(2,3,"other",params[m,])

##Check that it works properly
i = c("liom","vacant","crem","other")
x = c(-1,-5,4,3)
y = c(-1,-4,4,3)
px <- matrix(NA,ncol = length(i), nrow = (Ndraws))
for(m in 1:Ndraws){
  for(n in 1:length(i)){
    px[m,n] <- pxy(x[n],y[n],i[n],params[m,])
  }
}

px

#################################################################
#PRODUCTION OF 1-YO SEEDS IN THE SEED BANK FROM X-SIZED MOMS
fx<-function(x,i,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  p.flow<-invlogit((params$repro_beta0) + (params$repro_beta1)*xb)     ## Probability of Reproducing
  nflow<-exp((params$flow_beta0) + (params$flow_beta1)*xb)     ## Number of FLowers produced
  flow.surv_crem<-invlogit((params$viab_beta01)) ## Proportion of Flowers survive to fruit
  flow.surv_vac<-invlogit((params$viab_beta04)) ## Proportion of Flowers survive to fruit
  flow.surv_other<-invlogit((params$viab_beta03)) ## Proportion of Flowers survive to fruit
  flow.surv_liom<-invlogit((params$viab_beta02)) ## Proportion of Flowers survive to fruit
  seeds.per.fruit_crem<-(params$seed_beta01)                     ## Number of Seeds per Fruit
  seeds.per.fruit_liom<-(params$seed_beta02)                     ## Number of Seeds per Fruit
  seeds.per.fruit_vac<-(params$seed_beta03)                     ## Number of Seeds per Fruit
  seed.survival<-invlogit((params$preseed_beta0))^2           ## Seed per Fruit Survival ---------I measured 6-month seed survival; annual survival is its square
  f_crem = p.flow*nflow*flow.surv_crem*seeds.per.fruit_crem*seed.survival
  f_vac = p.flow*nflow*flow.surv_vac*seeds.per.fruit_vac*seed.survival
  f_other = p.flow*nflow*flow.surv_other*seeds.per.fruit_vac*seed.survival
  f_liom = p.flow*nflow*flow.surv_liom*seeds.per.fruit_liom*seed.survival
  if(i == "crem"){ return(f_crem)}
  if(i == "liom"){ return(f_liom)}
  if(i == "other"){ return(f_other)}
  if(i == "vacant"){ return(f_vac)}
}

## Check if it works
i = c("liom","vacant","crem","other")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
f <- matrix(NA,ncol = length(i), nrow = 100)


for(m in seq(1:nrow(params))){
  for(n in seq(1:length(i))){
    f[m,n] <- fx(x[n],i[n],params[m,])
  }
}
f

#####################################################
#### Recruitment
recruits<-function(y,params){
  yb=pmin(pmax(y,cholla_min),cholla_max)
  dnorm(yb, (params$rec_beta0),(params$rec_sig))
}

## Check if it works
i = c("liom","vacant","crem","other")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
r <- matrix(NA,ncol = length(i), nrow = (Ndraws))
for(m in 1:nrow(params)){
  for(n in 1:length(i)){
  r[m,n] <- recruits(y[n],params[m,])
}
  }
r



####################################################
beta<-function(vac_rec){
  ifelse(vac_rec == TRUE, return(0), return(1))
}

## Check if it works
beta(FALSE)



######################################################
#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE (OCC VS VAC)
transition.1<-function(x, i, j,params, scenario){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  ## Crem and Vac
  if(scenario == "cremvac"){
    ## Previously tended by None
    Denominator_vac <- exp((params$multi_betavv) + xb*(params$multi_betav)) + 
      exp((params$multi_betavc) + xb*(params$multi_betac))
    vac_vac = exp((params$multi_betavv) + xb*(params$multi_betav))/Denominator_vac
    vac_crem = exp((params$multi_betavc) + xb*(params$multi_betac))/Denominator_vac
    ## Previously tended by Crem
    Denominator_crem <- exp((params$multi_betacv) + xb*(params$multi_betav)) + 
      exp((params$multi_betacc) + xb*(params$multi_betac))
    crem_vac = exp((params$multi_betacv) + xb*(params$multi_betav))/Denominator_crem
    crem_crem = exp((params$multi_betacc) + xb*(params$multi_betac))/Denominator_crem
    if(i == "crem" & j == "crem"){return(crem_crem)}
    if(i == "crem" & j == "vacant"){return(crem_vac)}
    if(i == "vacant" & j == "crem"){return(vac_crem)}
    if(i == "vacant" & j == "vacant"){return(vac_vac)}
  }
  ## Liom and Vac
  if(scenario == "liomvac"){
    ## Previously tended by None
    Denominator_vac <- exp((params$multi_betavv) + xb*(params$multi_betav)) + 
      exp((params$multi_betavl) + xb*(params$multi_betal))
    vac_vac = exp((params$multi_betavv) + xb*(params$multi_betav))/Denominator_vac
    vac_liom = exp((params$multi_betavl) + xb*(params$multi_betal))/Denominator_vac
    ## Previously tended by Liom
    Denominator_liom <- exp((params$multi_betalv) + xb*(params$multi_betav)) + 
      exp((params$multi_betall) + xb*(params$multi_betal))
    liom_vac = exp((params$multi_betalv) + xb*(params$multi_betav))/Denominator_liom
    liom_liom = exp((params$multi_betall) + xb*(params$multi_betal))/Denominator_liom
    if(i == "liom" & j == "liom"){return(liom_liom)}
    if(i == "liom" & j == "vacant"){return(liom_vac)}
    if(i == "vacant" & j == "liom"){return(vac_liom)}
    if(i == "vacant" & j == "vacant"){return(vac_vac)}
  }
  ## Other and Vac
  if(scenario == "othervac"){
    ## Previously tended by None
    Denominator_vac <- exp((params$multi_betavv) + xb*(params$multi_betav)) + 
      exp((params$multi_betavo) + xb*(params$multi_betao))
    vac_vac = exp((params$multi_betavv) + xb*(params$multi_betav))/Denominator_vac
    vac_other = exp((params$multi_betavo) + xb*(params$multi_betao))/Denominator_vac
    ## Previously tended by Liom
    Denominator_other <- exp((params$multi_betaov) + xb*(params$multi_betav)) + 
      exp((params$multi_betaoo) + xb*(params$multi_betao))
    other_vac = exp((params$multi_betaov) + xb*(params$multi_betav))/Denominator_other
    other_other = exp((params$multi_betaoo) + xb*(params$multi_betao))/Denominator_other
    if(i == "other" & j == "other"){return(other_other)}
    if(i == "other" & j == "vacant"){return(other_vac)}
    if(i == "vacant" & j == "other"){return(vac_other)}
    if(i == "vacant" & j == "vacant"){return(vac_vac)}
  }
}

## Scenario options == "othervac", "liomvac", "cremvac"

transition.1(15,"vacant","liom",params,scenario = "liomvac")
## Check if it works
i = c("liom","vacant","vacant")
j = c("vacant","liom","vacant")
x = c(-1,2,2)
y = c(-1,2,3)
scenario = c("liomvac")
t1 <- matrix(NA,ncol = length(i), nrow = (Ndraws))
for(m in 1:nrow(params)){
  for(n in 1:length(i)){
    t1[m,n] <- transition.1(x[n],i[n],j[n],params[m,],scenario)
  }
}
t1

##########################################################
#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE (THREE STATES)
transition.2<-function(x, i, j, params,scenario){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  ## Liom, Vac, Other
  ## Previously tended by None
  if(scenario == "liomvacother"){
    Denominator_vac <- exp((params$multi_betavv) + xb*(params$multi_betav)) + 
      exp((params$multi_betavl) + xb*(params$multi_betal)) + 
      exp((params$multi_betavo) + xb*(params$multi_betao))
    vac_vac = exp((params$multi_betavv) + xb*(params$multi_betav))/Denominator_vac
    vac_liom = exp((params$multi_betavl) + xb*(params$multi_betal))/Denominator_vac
    vac_other = exp((params$multi_betavo) + xb*(params$multi_betao))/Denominator_vac
    ## Previously tended by Liom
    Denominator_liom <- exp((params$multi_betalv) + xb*(params$multi_betav)) + 
      exp((params$multi_betall) + xb*(params$multi_betal)) + 
      exp((params$multi_betalo) + xb*(params$multi_betao))
    liom_vac = exp((params$multi_betalv) + xb*(params$multi_betav))/Denominator_liom
    liom_liom = exp((params$multi_betall) + xb*(params$multi_betal))/Denominator_liom
    liom_other = exp((params$multi_betalo) + xb*(params$multi_betao))/Denominator_liom
    ## Previously tended by Other
    Denominator_other <- exp((params$multi_betaov) + xb*(params$multi_betav)) + 
      exp((params$multi_betaol) + xb*(params$multi_betal)) + 
      exp((params$multi_betaoo) + xb*(params$multi_betao))
    other_vac = exp((params$multi_betaov) + xb*(params$multi_betav))/Denominator_other
    other_liom = exp((params$multi_betaol) + xb*(params$multi_betal))/Denominator_other
    other_other = exp((params$multi_betaoo) + xb*(params$multi_betao))/Denominator_other
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
    Denominator_crem <- exp((params$multi_betacc) + xb*(params$multi_betav)) + 
      exp((params$multi_betacl) + xb*(params$multi_betal)) + 
      exp((params$multi_betacv) + xb*(params$multi_betav))
    crem_crem = exp((params$multi_betacc) + xb*(params$multi_betac))/Denominator_crem
    crem_liom = exp((params$multi_betacl) + xb*(params$multi_betal))/Denominator_crem
    crem_vac = exp((params$multi_betacv) + xb*(params$multi_betav))/Denominator_crem
    ## Previously tended by Liom
    Denominator_liom <- exp((params$multi_betalc) + xb*(params$multi_betac)) + 
      exp((params$multi_betall) + xb*(params$multi_betal)) + 
      exp((params$multi_betalv) + xb*(params$multi_betav))
    liom_crem = exp((params$multi_betalc) + xb*(params$multi_betac))/Denominator_liom
    liom_liom = exp((params$multi_betall) + xb*(params$multi_betal))/Denominator_liom
    liom_vac = exp((params$multi_betalv) + xb*(params$multi_betav))/Denominator_liom
    ## Previously tended by None
    Denominator_vac <- exp((params$multi_betavc) + xb*(params$multi_betac)) + 
      exp((params$multi_betavl) + xb*(params$multi_betal)) + 
      exp((params$multi_betavv) + xb*(params$multi_betav))
    vac_crem = exp((params$multi_betavc) + xb*(params$multi_betac))/Denominator_vac
    vac_liom = exp((params$multi_betavl) + xb*(params$multi_betal))/Denominator_vac
    vac_vac = exp((params$multi_betavv) + xb*(params$multi_betav))/Denominator_vac
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
    Denominator_crem <- exp((params$multi_betacc) + xb*(params$multi_betav)) + 
      exp((params$multi_betaco) + xb*(params$multi_betao)) + 
      exp((params$multi_betacv) + xb*(params$multi_betav))
    crem_crem = exp((params$multi_betacc) + xb*(params$multi_betac))/Denominator_crem
    crem_other = exp((params$multi_betaco) + xb*(params$multi_betao))/Denominator_crem
    crem_vac = exp((params$multi_betacv) + xb*(params$multi_betav))/Denominator_crem
    ## Previously tended by Liom
    Denominator_other <- exp((params$multi_betaoc) + xb*(params$multi_betac)) + 
      exp((params$multi_betaoo) + xb*(params$multi_betao)) + 
      exp((params$multi_betaov) + xb*(params$multi_betav))
    other_crem = exp((params$multi_betaoc) + xb*(params$multi_betac))/Denominator_other
    other_other = exp((params$multi_betaoo) + xb*(params$multi_betao))/Denominator_other
    other_vac = exp((params$multi_betaov) + xb*(params$multi_betav))/Denominator_other
    ## Previously tended by None
    Denominator_vac <- exp((params$multi_betavc) + xb*(params$multi_betac)) + 
      exp((params$multi_betavo) + xb*(params$multi_betao)) + 
      exp((params$multi_betavv) + xb*(params$multi_betav))
    vac_crem = exp((params$multi_betavc) + xb*(params$multi_betac))/Denominator_vac
    vac_other = exp((params$multi_betavo) + xb*(params$multi_betao))/Denominator_vac
    vac_vac = exp((params$multi_betavv) + xb*(params$multi_betav))/Denominator_vac
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

transition.2(2,"crem","vacant",params[m,],"othercremvac")
## Check if it works
i = c("liom","vacant","other","other")
j = c("vacant","liom","other","liom")
x = c(15,15,15,15)
y = c(-1,-4,4.5,3.01)
scenario = "liomvacother"
t2 <- matrix(NA,ncol = length(i), nrow = (10))
for(m in 1:nrow(params)){
  for(n in 1:length(i)){
    t2[m,n] <- transition.2(x[n],i[n],j[n],params[m,],scenario)
  }
}
t2


#######################################################
#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE (ALL STATES)
transition.3<-function(x, i, j,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  ## Previously tended by None
  Denominator_vac <- exp((params$multi_betavv) + xb*(params$multi_betav)) + 
    exp((params$multi_betavo) + xb*(params$multi_betao)) + 
    exp((params$multi_betavc) + xb*(params$multi_betac)) + 
    exp((params$multi_betavl) + xb*(params$multi_betal))
  vac_vac = exp((params$multi_betavv) + xb*(params$multi_betav))/Denominator_vac
  vac_other = exp((params$multi_betavo) + xb*(params$multi_betao))/Denominator_vac
  vac_crem = exp((params$multi_betavc) + xb*(params$multi_betac))/Denominator_vac
  vac_liom = exp((params$multi_betavl) + xb*(params$multi_betal))/Denominator_vac
  ## Previously tended by Other
  Denominator_other <- exp((params$multi_betaov) + xb*(params$multi_betav)) + 
    exp((params$multi_betaoo) + xb*(params$multi_betao)) + 
    exp((params$multi_betaoc) + xb*(params$multi_betac)) + 
    exp((params$multi_betaol) + xb*(params$multi_betal))
  other_vac = exp((params$multi_betaov) + xb*(params$multi_betav))/Denominator_other
  other_other = exp((params$multi_betaoo) + xb*(params$multi_betao))/Denominator_other 
  other_crem = exp((params$multi_betaoc) + xb*(params$multi_betac))/Denominator_other
  other_liom = exp((params$multi_betaol) + xb*(params$multi_betal))/Denominator_other
  ## Previously tended by Crem
  Denominator_crem <- exp((params$multi_betacv) + xb*(params$multi_betav)) + 
    exp((params$multi_betaco) + xb*(params$multi_betao)) + 
    exp((params$multi_betacc) + xb*(params$multi_betac)) + 
    exp((params$multi_betacl) + xb*(params$multi_betal))
  crem_vac = exp((params$multi_betacv) + xb*(params$multi_betav))/Denominator_crem
  crem_other = exp((params$multi_betaco) + xb*(params$multi_betao))/Denominator_crem 
  crem_crem = exp((params$multi_betacc) + xb*(params$multi_betac))/Denominator_crem
  crem_liom = exp((params$multi_betacl) + xb*(params$multi_betal))/Denominator_crem
  ## Previously tended by Liom
  Denominator_liom <- exp((params$multi_betalv) + xb*(params$multi_betav)) + 
    exp((params$multi_betalo) + xb*(params$multi_betao)) + 
    exp((params$multi_betalc) + xb*(params$multi_betac)) + 
    exp((params$multi_betall) + xb*(params$multi_betal))
  liom_vac = exp((params$multi_betalv) + xb*(params$multi_betav))/Denominator_liom
  liom_other = exp((params$multi_betalo) + xb*(params$multi_betao))/Denominator_liom
  liom_crem = exp((params$multi_betalc) + xb*(params$multi_betac))/Denominator_liom
  liom_liom = exp((params$multi_betall) + xb*(params$multi_betal))/Denominator_liom
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
transition.3(2,"crem","liom",params[m,])

## Chekc if it works
i = c("liom","liom")
j = c("vacant","vacant")
x = c(-1,-5)
y = c(-1,-5)
t3 <- matrix(NA,ncol = length(i), nrow = (Ndraws))
for(m in 1:nrow(params)){
  for(n in 1:length(i)){
    t3[m,n] <- transition.3(x[n],i[n],j[n],params[m,])
  }
}
t3

#########################################################
#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE 
transition.x <- function(x,i,j,num_ants,params,scenario){
  ifelse(num_ants == 1, transition.1(x,i,j,params,scenario),
         ifelse(num_ants == 2, transition.2(x,i,j,params,scenario),
                transition.3(x,i,j,params)))
}
## Scenario options ( 1 ant ) == "othervac", "liomvac", "cremvac"
## Scenario options ( 2 ant ) are "liomvacother", "liomcremother", "liomcremvac", "othercremvac"
transition.1(2, "liom","vacant",params[m,],"liomvac")
transition.x(x,"liom","vacant",1,params[m,],"liomvac")

transition.2(2,"liom","vacant",params[m,],"liomvacother")
transition.x(x,"liom","vacant",2,params[m,],"liomvacother")

transition.3(2, "liom","vacant",params[m,])
transition.x(x, "liom","vacant",3,params[m,],"all")

## Check if it works
i = c("liom","vacant","crem","other")
j = c("vacant","crem","crem","liom")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
scenario = "all"
num_ants = 3
t <- matrix(NA,ncol = length(i), nrow = (Ndraws))
for(m in 1:nrow(params)){
  for(n in 1:length(i)){
    t[m,n] <- transition.x(x[n],i[n],j[n],num_ants,params[m,],scenario)
  }
}
t


#####################################################
#GROWTH*SURVIVAL*ANT PROBABILITIES
ptxy <- function(x,y,i,j,num_ants,params,scenario){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  sx(xb,i,params)*gxy(xb,y,i,params)*transition.x(xb,i,j,num_ants,params,scenario)
}
## Scenario options == "othervac", "liomvac", "cremvac"
## Scenario options are "liomvacother", "liomcremother", "liomcremvac", "othercremvac"
## Check if it works
i = c("liom","vacant")
j = c("vacant","liom")
x = c(-1,-5)
y = c(-1,-4)
scenario = "liomvac"
num_ants = 2
pt <- matrix(NA,ncol = length(i), nrow = (10))
t <- matrix(NA,ncol = length(i), nrow = (10))
for(m in 1:nrow(params)){
  for(n in 1:length(i)){
    t[m,n] <- transition.x(x[n],i[n],j[n],num_ants,params[m,],scenario)
    pt[m,n] <- ptxy(x[n],y[n],i[n],j[n],num_ants,params[m,],scenario)
  }
}
pt
t
ptxy(1,1,"liom","vacant",2,params[1,],"liomvac")


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
  Tmat[2,1]<-1-invlogit((params$germ1_beta0)) 
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),1]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))   
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),2]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))   
  # Growth/survival transitions among cts sizes
  Tmat[3:(n+2),3:(n+2)]<-t(outer(y,y,pxy,i,params))*h 
  # Put it all together
  IPMmat<-Fmat+Tmat  
  #return(IPMmat)
  lambda <- Re(eigen(IPMmat)$values[1])
  return(lambda)
}
i = c("liom","vacant")
x <- c(1,1)
num_ants <- 1
a <- matrix(NA,ncol = length(i), nrow = 10)
b <- matrix(NA,ncol = length(i), nrow = 10)
lambda <- matrix(NA,ncol = length(i), nrow = 10)
big_f <- list()
big_t <- list()
big <- list()
for(m in 1:10){
  for(z in 1:length(i)){
    lambda[m,z] <- bigmatrix.1(params[m,],lower,upper,matsize,num_ants,i[z])
  }
}
lambda



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
    Tmat[2,1]<-1-invlogit((params$germ1_beta0))
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))*beta(FALSE)
    Tmat[(n+3):(2*n+2),1]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))*beta(FALSE)
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))*beta(FALSE)
    Tmat[(n+3):(2*n+2),2]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))*beta(FALSE)
    # Growth/survival transitions among cts sizes
    Tmat[3:(n+2),3:(n+2)]<-t(outer(y,y,ptxy,i = "liom",j = "liom",num_ants,params,"liomvac"))*h
    Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,i = "liom",j = "vacant",num_ants,params,"liomvac"))*h
    Tmat[(n+3):(2*n+2),3:(n+2)]<-t(outer(y,y,ptxy,i = "vacant",j = "liom",num_ants,params,"liomvac"))*h
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,i = "vacant",j = "vacant",num_ants,params,"liomvac"))*h
    # Put it all together
    IPMmat<-Fmat+Tmat
    # Calculate the lambda
    lambda = Re(eigen(IPMmat)$values[1])
    return(lambda)
    #return(list(Fmat = Fmat, Tmat = Tmat))
  }
  ############################################ CREM ###############################################
  # if(scenario == "cremvac"){
  #   # Banked seeds go in top row (1 == crem, 2 == vacant)
  #   Fmat[1,3:(n+2)]<-fx(y,"crem",params)
  #   Fmat[1,(n+3):(2*n+2)]<-fx(y,"vacant",params)
  #   # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  #   Tmat[2,1]<-1-invlogit((params$germ1_beta0))
  #   # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  #   Tmat[3:(n+2),1]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))*beta(T)
  #   Tmat[(n+3):(2*n+2),1]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))*beta(F)
  #   # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  #   Tmat[3:(n+2),2]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))*beta(T)
  #   Tmat[(n+3):(2*n+2),2]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))*beta(F)
  #   # Growth/survival transitions among cts sizes
  #   Tmat[3:(n+2),3:(n+2)]<-t(outer(y,y,ptxy,"crem","crem",num_ants,params,scenario))*h
  #   Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,"crem","vacant",num_ants,params,scenario))*h
  #   Tmat[(n+3):(2*n+2),3:(n+2)]<-t(outer(y,y,ptxy,"vacant","crem",num_ants,params,scenario))*h
  #   Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,"vacant","vacant",num_ants,params,scenario))*h
  #   # Put it all together
  #   IPMmat<-Fmat+Tmat# Calculate the lambda
  #   lambda = Re(eigen(IPMmat)$values[1])
  #   return(lambda)
  # }
  # ############################################# OTHER ###########################################
  # if(scenario == "othervac"){
  #   # Banked seeds go in top row (1 == other, 2 == vacant)
  #   Fmat[1,3:(n+2)]<-fx(y,"other",params)
  #   Fmat[1,(n+3):(2*n+2)]<-fx(y,"vacant",params)
  #   # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  #   Tmat[2,1]<-1-invlogit((params$germ1_beta0))
  #   # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  #   Tmat[3:(n+2),1]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))*beta(T)
  #   Tmat[(n+3):(2*n+2)]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))*beta(F)
  #   # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  #   Tmat[3:(n+2),2]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))*beta(T)
  #   Tmat[(n+3):(2*n+2)]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))*beta(F)
  #   # Growth/survival transitions among cts sizes
  #   Tmat[3:(n+2),3:(n+2)]<-t(outer(y,y,ptxy,"other","other",num_ants,params,scenario))*h
  #   Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,"other","vacant",num_ants,params,scenario))*h
  #   Tmat[(n+3):(2*n+2),3:(n+2)]<-t(outer(y,y,ptxy,"vacant","other",num_ants,params,scenario))*h
  #   Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<-t(outer(y,y,ptxy,"vacant","vacant",num_ants,params,scenario))*h
  #   # Put it all together
  #   IPMmat<-Fmat+Tmat
  #   # Calculate the lambda
  #   lambda = Re(eigen(IPMmat)$values[1])
  #   return(lambda)
  # }
}
x = c(1,1)
y = c(1,1)
i = c("liom","vacant")
j = c("vacant","vacant")
scenario <- c("liomvac")
lambda <- matrix(NA,ncol = length(i), nrow = 10)
for(m in 1:10){
  for(z in 1:length(i)){
    # Fertility matricies -- Two Ant
    Fmat <- matrix(0,(2*n+2),(2*n+2))
    # Growth/survival transition matricies -- Two Ant
    Tmat <- matrix(0,(2*n+2),(2*n+2))
    ## Full Matricies
    IPMmat <- matrix()
    ############################################# LIOM ############################################
    if(scenario == "liomvac"){
      # Banked seeds go in top row (1 == liom, 2 == vacant)
      Fmat[1,3:(n+2)]<-fx(x[z],"liom",params[m,])
      Fmat[1,(n+3):(2*n+2)]<-fx(x[z],"vacant",params[m,])
      # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
      Tmat[2,1]<-1-invlogit((params$germ1_beta0[m]))
      # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
      Tmat[3:(n+2),1]<-invlogit((params$germ1_beta0[m]))*recruits(x[z],params[m,])*h*invlogit((params$preseed_beta0[m]))*beta(FALSE)
      Tmat[(n+3):(2*n+2),1]<-invlogit((params$germ1_beta0[m]))*recruits(x[z],params[m,])*h*invlogit((params$preseed_beta0[m]))*beta(FALSE)
      # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
      Tmat[3:(n+2),2]<-invlogit((params$germ2_beta0[m]))*recruits(x[z],params[m,])*h*invlogit((params$preseed_beta0[m]))*beta(FALSE)
      Tmat[(n+3):(2*n+2),2]<-invlogit((params$germ2_beta0[m]))*recruits(x[z],params[m,])*h*invlogit((params$preseed_beta0[m]))*beta(FALSE)
      # # Growth/survival transitions among cts sizes
       Tmat[3:(n+2),3:(n+2)]<-t(outer(x[z],y[z],ptxy,i = "liom",j = "liom",1,params[m,],"liomvac"))*h
       Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(x[z],y[z],ptxy,i = "liom",j = "vacant",1,params[m,],"liomvac"))*h
       Tmat[(n+3):(2*n+2),3:(n+2)]<-t(outer(x[z],x[z],ptxy,i = "vacant",j = "liom",1,params[m,],"liomvac"))*h
       Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<-t(outer(x[z],x[z],ptxy,i = "vacant",j = "vacant",1,params[m,],"liomvac"))*h
      # # Put it all together
       IPMmat<-Fmat+Tmat
      # # Calculate the lambda
       lambda[m,n] = Re(eigen(IPMmat)$values[1])
    }
    #lambda[m,n] <- bigmatrix.2(params[m,],lower,upper,matsize,2,i[n],j[n],scenario[n])
  }
}
lambda



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
    lambda = Re(eigen(IPMmat)$values[1])
    return(lambda)
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
    lambda = Re(eigen(IPMmat)$values[1])
    return(lambda)
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
    lambda = Re(eigen(IPMmat)$values[1])
    return(lambda)
  }
}
## Scenario options are "liomvacother", "liomcremother", "liomcremvac", "othercremvac"


bigmatrix.3(params,lower,upper,matsize,3,"liom","liom","liomcremvac")
bigmatrix.3(params,lower,upper,matsize,3,"vacant","vacant","liomvacother")
bigmatrix.3(params,lower,upper,matsize,3,"other","crem","othercremvac")




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
  lambda = Re(eigen(IPMmat)$values[1])
  return(lambda)
}

bigmatrix.4(params,lower,upper,matsize,4,"vacant","vacant","all")




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
  if(num_ants == 1){
    lambda = bigmatrix.1(params,lower,upper,matsize,num_ants)
    return(lambda)
  }
  if(num_ants == 2){
    lambda = bigmatrix.2(params,lower,upper,matsize,num_ants,i,j,scenario)
    return(lambda)
  }
  if(num_ants == 3){
    lambda = bigmatrix.3(params,lower,upper,matsize,num_ants,i,j,scenario)
    return(lambda)
  }
  if(num_ants == 4){
    lambda = bigmatrix.4(params,lower,upper,matsize,num_ants,i,j,scenario)
    return(lambda)
  }
} 
## One ant option
bigmatrix(params, lower, upper, matsize, 1,"vacant","vacant","none")
bigmatrix.1(params, lower, upper, matsize, 1)
## 2 ant options
bigmatrix(params,lower,upper,matsize,2,"crem","vacant","cremvac")
bigmatrix.2(params,lower,upper,matsize,2,"crem","vacant","cremvac")
## 3 ant options
bigmatrix(params,lower,upper,matsize,3,"crem","vacant","liomcremvac")
bigmatrix.3(params,lower,upper,matsize,3,"crem","vacant","liomcremvac")
## all ant options
bigmatrix(params,lower,upper,matsize,4,"crem","vacant","all")
bigmatrix.4(params,lower,upper,matsize,4,"crem","vacant","all")



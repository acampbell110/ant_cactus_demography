#########################################################################################################
##            This will be an IPM which allows you to choose how many ants are present
#########################################################################################################
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code")
source("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code/Params.R")
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
  g_vac = dlst(y,mu=(params$grow_beta01) + (params$grow_beta11)*xb + (params$grow_beta21)*xb^2, 
               sigma = exp((params$grow_sig0) + (params$grow_sig1)*xb), 
               df = exp((params$grow_alp0) + (params$grow_alp1)*xb))
  g_liom = dlst(y,mu=(params$grow_beta04) + (params$grow_beta14)*xb + (params$grow_beta24)*xb^2, 
                sigma = exp((params$grow_sig0) + (params$grow_sig1)*xb), 
                df = exp((params$grow_alp0) + (params$grow_alp1)*xb))
  g_crem = dlst(y,mu=(params$grow_beta03) + (params$grow_beta13)*xb + (params$grow_beta23)*xb^2, 
                sigma = exp((params$grow_sig0) + (params$grow_sig1)*xb), 
                df = exp((params$grow_alp0) + (params$grow_alp1)*xb))
  g_other = dlst(y,mu=(params$grow_beta02) + (params$grow_beta12)*xb + (params$grow_beta22)*xb^2, 
                 sigma = exp((params$grow_sig0) + (params$grow_sig1)*xb), 
                 df = exp((params$grow_alp0) + (params$grow_alp1)*xb))
  #Return the probability of growing from size x to y
  if(i == "crem"){ return(g_crem)}
  if(i == "liom"){ return(g_liom)}
  if(i == "other"){ return(g_other)}
  if(i == "vacant"){ return(g_vac)}
}

# ##Check that it works properly
# i = c("vacant","crem","liom","other")
# x = c(-1,-5,3,4)
# y = c(-1,-4,3,4)
# g <- matrix(NA,ncol = length(i), nrow = 1000)
# for(m in 1:nrow(params)){
#   for(n in seq(1:length(i))){
#     g[m,n] <- gxy(x[n],y[n],i[n],params[m,])
#     }
# }
# 
# g
# 
# #### Plot the outputs of this compared to the vital rate functions 
# size_dummy <- seq(min(cactus$logsize_t, na.rm = T), max(cactus$logsize_t, na.rm = TRUE), by = 0.1)
# l = rep("liom",length(size_dummy))
# o = rep("other",length(size_dummy))
# c = rep("crem",length(size_dummy))
# v = rep("vacant",length(size_dummy))
# g_l <- matrix(NA,ncol = length(l), nrow = 1000)
# g_c <- matrix(NA,ncol = length(l), nrow = 1000)
# g_o <- matrix(NA,ncol = length(l), nrow = 1000)
# g_v <- matrix(NA,ncol = length(l), nrow = 1000)
# for(m in 1:nrow(params)){
#   for(n in seq(1:length(i))){
#     g_l[m,n] <- gxy(size_dummy[n],size_dummy[n],l[n],params[m,])
#     g_o[m,n] <- gxy(size_dummy[n],size_dummy[n],o[n],params[m,])
#     g_c[m,n] <- gxy(size_dummy[n],size_dummy[n],c[n],params[m,])
#     g_v[m,n] <- gxy(size_dummy[n],size_dummy[n],v[n],params[m,])
#   }
# }
# plot(size_dummy,colMeans(g_l), col = liomcol, type = "l",lwd = 2, xlim = c(-3.4,-3.2), ylim = c(0.00000000000001,0.000000000005))
# lines(size_dummy, colMeans(g_o), col = othercol, lwd = 2)
# lines(size_dummy, colMeans(g_v), col = vaccol, lwd = 2)
# lines(size_dummy, colMeans(g_c), col = cremcol, lwd = 2)

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
  s_crem = invlogit((params$surv_beta03) + (params$surv_beta13)*xb)
  s_vac = invlogit((params$surv_beta01) + (params$surv_beta11)*xb)
  s_other = invlogit((params$surv_beta02) + (params$surv_beta12)*xb)
  s_liom = invlogit((params$surv_beta04) + (params$surv_beta14)*xb)
  #Return the survival probabilities
  if(i == "crem"){ return(s_crem)}
  if(i == "liom"){ return(s_liom)}
  if(i == "other"){ return(s_other)}
  if(i == "vacant"){ return(s_vac)}
}

# ##Check that it works properly
# i = c("liom","vacant","crem","other")
# x = c(-1,-5,4,3)
# s <- matrix(NA,ncol = length(i), nrow = 1000)
# for(m in 1:nrow(params)){
#   for(n in seq(1:length(i))){
#     s[m,n] <- sx(x[n],i[n],params[m,])
#   }
# }
# 
# s
# 
# ## Plot the outputs to make sure this is looking accurate
# size_dummy <- seq(min(cactus$logsize_t, na.rm = T), max(cactus$logsize_t, na.rm = TRUE), by = 0.1)
# l = rep("liom",length(size_dummy))
# o = rep("other",length(size_dummy))
# c = rep("crem",length(size_dummy))
# v = rep("vacant",length(size_dummy))
# s_l <- matrix(NA,ncol = length(l), nrow = 1000)
# s_c <- matrix(NA,ncol = length(l), nrow = 1000)
# s_o <- matrix(NA,ncol = length(l), nrow = 1000)
# s_v <- matrix(NA,ncol = length(l), nrow = 1000)
# for(m in 1:nrow(params)){
#   for(n in seq(1:length(i))){
#     s_l[m,n] <- sx(size_dummy[n],l[n],params[m,])
#     s_o[m,n] <- sx(size_dummy[n],o[n],params[m,])
#     s_c[m,n] <- sx(size_dummy[n],c[n],params[m,])
#     s_v[m,n] <- sx(size_dummy[n],v[n],params[m,])
#   }
# }
# 
# plot(size_dummy,colMeans(s_l), col = liomcol, type = "l", lwd = 2)
# lines(size_dummy, colMeans(s_o), col = othercol, lwd = 2)
# lines(size_dummy, colMeans(s_v), col = vaccol, lwd = 2)
# lines(size_dummy, colMeans(s_c), col = cremcol, lwd = 2)


#################################################
#SURVIVAL*GROWTH. Combine the survival and growth probabilities
pxy<-function(x,y,i,params){
  #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  xb=pmin(pmax(x,cholla_min),cholla_max)
  #Multiply the probabilities of survival and growth together to get the survival growth kernel
  pxy = sx(x,i,params)*gxy(x,y,i,params)
  #pxy = gxy(x,y,i,params)
  return(pxy)
}

# ##Check that it works properly
# i = c("liom","vacant","crem","other")
# x = c(-1,-5,4,3)
# y = c(-1,-4,4,3)
# px <- matrix(NA,ncol = length(i), nrow = (1000))
# for(m in 1:1000){
#   for(n in 1:length(i)){
#     px[m,n] <- pxy(x[n],y[n],i[n],params[m,])
#   }
# }
# 
# ## The output of this should have columns summing to 1 or less than 1
# colSums(px)

#################################################################
#PRODUCTION OF 1-YO SEEDS IN THE SEED BANK FROM X-SIZED MOMS
fx<-function(x,i,params){
  #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  xb=pmin(pmax(x,cholla_min),cholla_max)
  p.flow<-invlogit((params$repro_beta0) + (params$repro_beta1)*xb)      ## Probability of Reproducing
  nflow<-exp((params$flow_beta0) + (params$flow_beta1)*xb)      ## Number of FLowers produced
  flow.surv_crem<-invlogit((params$viab_beta03))      ## Proportion of Flowers survive to fruit
  flow.surv_vac<-invlogit((params$viab_beta01))       ## Proportion of Flowers survive to fruit
  flow.surv_other<-invlogit((params$viab_beta02))       ## Proportion of Flowers survive to fruit
  flow.surv_liom<-invlogit((params$viab_beta04))      ## Proportion of Flowers survive to fruit
  seeds.per.fruit_crem<-(params$seed_beta01)      ## Number of Seeds per Fruit
  seeds.per.fruit_liom<-(params$seed_beta03)      ## Number of Seeds per Fruit
  seeds.per.fruit_vac<-(params$seed_beta02)     ## Number of Seeds per Fruit
  seed.survival<-invlogit((params$preseed_beta0))^2       ## Seed per Fruit Survival ---------I measured 6-month seed survival; annual survival is its square
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


# ## Check if it works
# i = c("liom","vacant","crem","other")
# x = c(-1,-5,4,3)
# y = c(-1,-4,4.5,3.01)
# f <- matrix(NA,ncol = length(i), nrow = 100)
# for(m in seq(1:100)){
#   for(n in seq(1:length(i))){
#     f[m,n] <- fx(x[n],i[n],params[m,])
#   }
# }
# f
# 
# ## Plot the outputs to make sure this is looking accurate
# size_dummy <- seq(min(cactus$logsize_t, na.rm = T), max(cactus$logsize_t, na.rm = TRUE), by = 0.1)
# l = rep("liom",length(size_dummy))
# o = rep("other",length(size_dummy))
# c = rep("crem",length(size_dummy))
# v = rep("vacant",length(size_dummy))
# f_l <- matrix(NA,ncol = length(l), nrow = 1000)
# f_c <- matrix(NA,ncol = length(l), nrow = 1000)
# f_o <- matrix(NA,ncol = length(l), nrow = 1000)
# f_v <- matrix(NA,ncol = length(l), nrow = 1000)
# for(m in 1:1000){
#   for(n in seq(1:length(l))){
#     f_l[m,n] <- fx(size_dummy[n],l[n],params[m,])
#     f_o[m,n] <- fx(size_dummy[n],o[n],params[m,])
#     f_c[m,n] <- fx(size_dummy[n],c[n],params[m,])
#     f_v[m,n] <- fx(size_dummy[n],v[n],params[m,])
#   }
# }
# 
# plot(size_dummy,colMeans(f_l), col = liomcol, type = "l", lwd = 2, xlim = c(11,11.4), ylim = c(.5,.6))
# lines(size_dummy, colMeans(f_o), col = othercol, lwd = 2)
# lines(size_dummy, colMeans(f_v), col = vaccol, lwd = 2)
# lines(size_dummy, colMeans(f_c), col = cremcol, lwd = 2)


#####################################################
#### Recruitment
recruits<-function(y,params){
  yb=pmin(pmax(y,cholla_min),cholla_max)
  dnorm(yb, (params$rec_beta0),(params$rec_sig))
}

# ## Check if it works
# i = c("liom","vacant","crem","other")
# x = c(-1,-5,4,3)
# y = c(-1,-4,4.5,3.01)
# r <- matrix(NA,ncol = length(i), nrow = (1000))
# for(m in 1:nrow(params)){
#   for(n in 1:length(i)){
#   r[m,n] <- recruits(y[n],params[m,])
#     }
#   }
# r


######################################################
#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE. One ant option
transition.1<-function(x, i, j,params, scenario){
  #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  xb=pmin(pmax(x,cholla_min),cholla_max)
  ## Crem and Vac
  if(scenario == "cremvac"){
    #Denom of previously tended by None
    Denominator_vac <- exp((params$multi_betavv) + xb*(params$multi_betav)) + 
      exp((params$multi_betavc) + xb*(params$multi_betac))
    #Calculate the probabilities by next ant state
    vac_vac = exp((params$multi_betavv) + xb*(params$multi_betav))/Denominator_vac
    vac_crem = exp((params$multi_betavc) + xb*(params$multi_betac))/Denominator_vac
    #Denom of previously tended by Crem
    Denominator_crem <- exp((params$multi_betacv) + xb*(params$multi_betav)) + 
      exp((params$multi_betacc) + xb*(params$multi_betac))
    #Calculate the probabilities by next ant state
    crem_vac = exp((params$multi_betacv) + xb*(params$multi_betav))/Denominator_crem
    crem_crem = exp((params$multi_betacc) + xb*(params$multi_betac))/Denominator_crem
    #Return them
    if(i == "crem" & j == "crem"){return(crem_crem)}
    if(i == "crem" & j == "vacant"){return(crem_vac)}
    if(i == "vacant" & j == "crem"){return(vac_crem)}
    if(i == "vacant" & j == "vacant"){return(vac_vac)}
  }
  ## Liom and Vac
  if(scenario == "liomvac"){
    #Denom of previously tended by None
    Denominator_vac <- exp((params$multi_betavv) + xb*(params$multi_betav)) + 
      exp((params$multi_betavl) + xb*(params$multi_betal))
    #Calculate the probabilities by next ant state
    vac_vac = exp((params$multi_betavv) + xb*(params$multi_betav))/Denominator_vac
    vac_liom = exp((params$multi_betavl) + xb*(params$multi_betal))/Denominator_vac
    #Denom of previously tended by Liom
    Denominator_liom <- exp((params$multi_betalv) + xb*(params$multi_betav)) + 
      exp((params$multi_betall) + xb*(params$multi_betal))
    #Calculate the probabilities by next ant state
    liom_vac = exp((params$multi_betalv) + xb*(params$multi_betav))/Denominator_liom
    liom_liom = exp((params$multi_betall) + xb*(params$multi_betal))/Denominator_liom
    #Return the probabilities
    if(i == "liom" & j == "liom"){return(liom_liom)}
    if(i == "liom" & j == "vacant"){return(liom_vac)}
    if(i == "vacant" & j == "liom"){return(vac_liom)}
    if(i == "vacant" & j == "vacant"){return(vac_vac)}
  }
  ## Other and Vac
  if(scenario == "othervac"){
    #Denom of previously tended by None
    Denominator_vac <- exp((params$multi_betavv) + xb*(params$multi_betav)) + 
      exp((params$multi_betavo) + xb*(params$multi_betao))
    #Calculate the probabilities by next ant state
    vac_vac = exp((params$multi_betavv) + xb*(params$multi_betav))/Denominator_vac
    vac_other = exp((params$multi_betavo) + xb*(params$multi_betao))/Denominator_vac
    #Denom of previously tended by Liom
    Denominator_other <- exp((params$multi_betaov) + xb*(params$multi_betav)) + 
      exp((params$multi_betaoo) + xb*(params$multi_betao))
    #Calculate the probabilities by next ant state
    other_vac = exp((params$multi_betaov) + xb*(params$multi_betav))/Denominator_other
    other_other = exp((params$multi_betaoo) + xb*(params$multi_betao))/Denominator_other
    #Return the probabilities
    if(i == "other" & j == "other"){return(other_other)}
    if(i == "other" & j == "vacant"){return(other_vac)}
    if(i == "vacant" & j == "other"){return(vac_other)}
    if(i == "vacant" & j == "vacant"){return(vac_vac)}
  }
}

# ## Scenario options == "othervac", "liomvac", "cremvac"
# ## Check if it works
# i = c("liom","vacant","vacant")
# j = c("vacant","liom","vacant")
# x = c(-1,2,2)
# y = c(-1,2,3)
# scenario = c("liomvac")
# t1 <- matrix(NA,ncol = length(i), nrow = (Ndraws))
# for(m in 1:nrow(params)){
#   for(n in 1:length(i)){
#     t1[m,n] <- transition.1(x[n],i[n],j[n],params[m,],scenario)
#   }
# }
# t1

##########################################################
#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE (THREE STATES)
transition.2<-function(x, i, j, params,scenario){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  ## Liom, Vac, Other
  #Denom of previously tended by None
  if(scenario == "liomvacother"){
    Denominator_vac <- exp((params$multi_betavv) + xb*(params$multi_betav)) + 
      exp((params$multi_betavl) + xb*(params$multi_betal)) + 
      exp((params$multi_betavo) + xb*(params$multi_betao))
    #Calculate the probabilities by next ant state
    vac_vac = exp((params$multi_betavv) + xb*(params$multi_betav))/Denominator_vac
    vac_liom = exp((params$multi_betavl) + xb*(params$multi_betal))/Denominator_vac
    vac_other = exp((params$multi_betavo) + xb*(params$multi_betao))/Denominator_vac
    #Denom of previously tended by Liom
    Denominator_liom <- exp((params$multi_betalv) + xb*(params$multi_betav)) + 
      exp((params$multi_betall) + xb*(params$multi_betal)) + 
      exp((params$multi_betalo) + xb*(params$multi_betao))
    #
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
    Denominator_crem <- exp((params$multi_betacc) + xb*(params$multi_betac)) + 
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
    Denominator_crem <- exp((params$multi_betacc) + xb*(params$multi_betac)) + 
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
# ## Scenario options are "liomvacother", "liomcremother", "liomcremvac", "othercremvac"
# ## Check if it works
# i = c("liom","vacant","other","other")
# j = c("vacant","liom","other","liom")
# x = c(15,15,15,15)
# y = c(-1,-4,4.5,3.01)
# scenario = "liomvacother"
# t2 <- matrix(NA,ncol = length(i), nrow = (100))
# for(m in 1:nrow(params)){
#   for(n in 1:length(i)){
#     t2[m,n] <- transition.2(x[n],i[n],j[n],params[m,],scenario)
#   }
# }
# t2

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
# ## Chekc if it works
# i = c("liom","liom")
# j = c("vacant","vacant")
# x = c(-1,-5)
# y = c(-1,-5)
# t3 <- matrix(NA,ncol = length(i), nrow = (Ndraws))
# for(m in 1:nrow(params)){
#   for(n in 1:length(i)){
#     t3[m,n] <- transition.3(x[n],i[n],j[n],params[m,])
#   }
# }
# t3

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
# ## Check if it works
# i = c("liom","vacant","crem","other")
# j = c("vacant","crem","crem","liom")
# x = c(-1,-5,4,3)
# y = c(-1,-4,4.5,3.01)
# scenario = "all"
# t <- matrix(NA,ncol = length(i), nrow = (Ndraws))
# for(m in 1:nrow(params)){
#   for(n in 1:length(i)){
#     t[m,n] <- transition.x(x[n],i[n],j[n],params[m,],scenario)
#   }
# }
# t




##################################################################################################
############################# ONE ANT MATRIX #####################################################
##################################################################################################
bigmatrix.1 <- function(params,lower,upper,matsize){
  ###################################################################################################
  ## returns the full IPM kernel (to be used in stochastic simulation), the F and T kernels, and meshpoints in the units of size
  ## params,yrfx,plotfx, and mwye get passed to the vital rate functions
  ## f.eps is fertility overdispersion. defaults to zero (see lambda.fun())
  ## lower and upper are the integration limits
  ## matsize is the dimension of the approximating matrix (it gets an additional 2 rows and columns for the seed banks)
  ###################################################################################################
  #Applying the midpoint rule
  n<-400
  L<-lower - 30
  U<-upper + 10
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
  Tmat[2,1]<-1-invlogit((params$germ1_beta0))
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),1]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),2]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))   
  # Growth/survival transitions among cts sizes
  Tmat[3:(n+2),3:(n+2)]<-t(outer(y,y,pxy,"vacant",params))*h 
  # Put it all together
  IPMmat<-Fmat+Tmat  
  colSums(Tmat[3:(n+2),3:(n+2)])
  return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat, y=y))
}
# i = c("liom","vacant")
# x <- c(1,1)
# lambda <- vector()
# big <- list()
# for(m in 1:10){
#     lambda[m] <- lambda(bigmatrix.1(params[m,],lower,upper,matsize)$IPMmat)
# }
# lambda
# 
# 
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
  n<-300
  L<-lower - 25
  U<-upper + 15
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
    # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
    Tmat[(n+3):(2*n+2),1]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
    Tmat[3:(n+2),1]<-0
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
    Tmat[(n+3):(2*n+2),2]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
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
    colSums(Tmat[3:(2*n+2),3:(2*n+2)])
    IPMmat<-Fmat+Tmat
    # Calculate the lambda
    # lambda = Re(eigen(IPMmat)$values[1])
    # return(lambda)
    return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
  }
  ########################################### CREM ###############################################
  if(scenario == "cremvac"){
    # Banked seeds go in top row (1 == crem, 2 == vacant)
    Fmat[1,3:(n+2)]<-fx(y,"crem",params)
    Fmat[1,(n+3):(2*n+2)]<-fx(y,"vacant",params)
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit((params$germ1_beta0))
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
    Tmat[3:(n+2),1]<-0
    Tmat[(n+3):(2*n+2),1]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
    Tmat[3:(n+2),2]<-0
    Tmat[(n+3):(2*n+2),2]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
    # Growth/survival transitions among cts sizes
    Tmat[3:(n+2),3:(n+2)]<-(t(outer(y,y,pxy,i = "crem",params))*h)%*%diag(transition.x(y,i = "crem",j = "crem",params,"cremvac"))
    Tmat[3:(n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,i = "vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "crem",params,"cremvac"))
    Tmat[(n+3):(2*n+2),3:(n+2)]<-(t(outer(y,y,pxy,i = "crem",params))*h)%*%diag(transition.x(y,i = "crem",j = "vacant",params,"cremvac"))
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,i = "vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "vacant",params,"cremvac"))
    # Put it all together
    colSums(Tmat[3:(2*n+2),3:(2*n+2)])
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
    Tmat[2,1]<-1-invlogit((params$germ1_beta0))
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
    Tmat[3:(n+2),1]<-0
    Tmat[(n+3):(2*n+2)]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
    Tmat[3:(n+2),2]<-0
    Tmat[(n+3):(2*n+2)]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
    # Growth/survival transitions among cts sizes
    Tmat[3:(n+2),3:(n+2)]<-(t(outer(y,y,pxy,i = "other",params))*h)%*%diag(transition.x(y,i = "other",j = "other",params,"othervac"))
    Tmat[3:(n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,"vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "other",params,"othervac"))
    Tmat[(n+3):(2*n+2),3:(n+2)]<-(t(outer(y,y,pxy,"other",params))*h)%*%diag(transition.x(y,i = "other",j = "vacant",params,"othervac"))
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,"vacant",params))*h)%*%diag(transition.x(y,i = "vacant",j = "vacant",params,"othervac"))
    # Put it all together
    colSums(Tmat[3:(2*n+2),3:(2*n+2)])
    IPMmat<-Fmat+Tmat
    # Calculate the lambda
    # lambda = Re(eigen(IPMmat)$values[1])
    # return(lambda)
    return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
  }
}
# i = c("liom","vacant")
# x <- c(1,1)
# scenario = c("liomvac","othervac")
# lambda <- matrix(rep(NA,20),nrow = 10,ncol = 2)
# for(z in 1:length(i)){
#   for(m in 1:10){
#   lambda[m,z] <- lambda(bigmatrix.2(params[m,],lower,upper,matsize,scenario[z])$IPMmat)
#     }
#   }
# lambda


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
  n<-400
  L<-lower - 40
  U<-upper + 35
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
    Tmat[2,1]<-1-invlogit((params$germ1_beta0))
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-0
    Tmat[(n+3):(2*n+2),1]<-0
    Tmat[(2*n+3):(3*n+2),1]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-0
    Tmat[(n+3):(2*n+2),2]<-0
    Tmat[(2*n+3):(3*n+2),2]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
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
    colSums(Tmat[3:(3*n+2),3:(3*n+2)])
    IPMmat<-Fmat+Tmat
    # lambda = Re(eigen(IPMmat)$values[1])
    # return(lambda)
    return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
  }
  ############################################ LIOM & OTHER & VAC ###########################################
  if(scenario == "liomvacother"){
    ## Fecundity of plants
    Fmat[1,3:(n+2)]<-fx(y,params=params,"vacant") ## Production of seeds from x sized mom with no ant visitor
    Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"liom") ## Production of seeds from x sized mom with ant visitor
    Fmat[1,(2*n+3):(3*n+2)]<-fx(y,params=params,"other")
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit((params$germ1_beta0))
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
    Tmat[(n+3):(2*n+2),1]<-0
    Tmat[(2*n+3):(3*n+2),1]<-0
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
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
    colSums(Tmat[3:(3*n+2),3:(3*n+2)])
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
    Tmat[2,1]<-1-invlogit((params$germ1_beta0))
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-0
    Tmat[(n+3):(2*n+2),1]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
    Tmat[(2*n+3):(3*n+2),1]<-0
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-0
    Tmat[(n+3):(2*n+2),2]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
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
    colSums(Tmat[3:(3*n+2),3:(3*n+2)])
    IPMmat<-Fmat+Tmat
    # lambda = Re(eigen(IPMmat)$values[1])
    # return(lambda)
    return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
  }
}
# ## Scenario options are "liomvacother", "liomcremother", "liomcremvac", "othercremvac"
# i = c("liom","vacant")
# x <- c(1,1)
# scenario = c("liomvacother","othercremvac")
# lambda <- matrix(rep(NA,20),nrow = 10,ncol = 2)
# for(z in 1:length(i)){
#   for(m in 1:10){
#     lambda[m,z] <- lambda(bigmatrix.3(params[m,],lower,upper,matsize,scenario[z])$IPMmat)
#   }
# }
# lambda



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
  n<-400
  L<-lower - 40
  U<-upper + 35
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
  #Tmat[2,1]<-1-1 
  Tmat[2,1]<-1-invlogit((params$germ1_beta0)) 
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
  Tmat[(3*n+3):(4*n+2),1]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),2]<-0
  Tmat[(n+3):(2*n+2),2]<-0
  Tmat[(2*n+3):(3*n+2),2]<-0
  Tmat[(3*n+3):(4*n+2),1]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
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
  colSums(Tmat[3:(4*n+2),3:(4*n+2)])
  colSums(Tmat[3:(4*n+2),3:(4*n+2)])[1000:1200]
  IPMmat<-Fmat+Tmat
  # lambda = Re(eigen(IPMmat)$values[1])
  # return(lambda)
  return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
}

# i = c("liom","vacant")
# x <- c(1,1)
# scenario = c("all","all")
# lambda <- matrix(rep(NA,20),nrow = 10,ncol = 2)
# for(z in 1:length(i)){
#   for(m in 1:10){
#     lambda[m,z] <- lambda(bigmatrix.4(params[m,],lower,upper,matsize,scenario[z])$IPMmat)
#   }
# }
# lambda


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
## One ant option
lambda(bigmatrix(params[14,], lower, upper, matsize, "none")$IPMmat)
lambda(bigmatrix.1(params[14,], lower, upper, matsize)$IPMmat)
## 2 ant options
lambda(bigmatrix(params[14,],lower,upper,matsize,"cremvac")$IPMmat)
lambda(bigmatrix.2(params[14,],lower,upper,matsize,"cremvac")$IPMmat)
## 3 ant options
lambda(bigmatrix(params[14,],lower,upper,matsize,"liomcremvac")$IPMmat)
lambda(bigmatrix.3(params[14,],lower,upper,matsize,"liomcremvac")$IPMmat)
## all ant options
lambda(bigmatrix(params[14,],lower,upper,matsize,"all")$IPMmat)
lambda(bigmatrix.4(params[14,],lower,upper,matsize,"all")$IPMmat)



################################################################################ 
################################################################################
## The purpose of this script is to load the IPM functions and run simulations 
## where the ant proportions and dynamics are reversed
################################################################################
################################################################################
## Set the working directory 
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

################################################################################
## Set the new ant conditions -- in this case it means reversing Crem and Liom
################################################################################
##-------------------------Transition Parameters-------------------##
# Prev Vac
params$multi_betavv <- multi.params$beta[draws,4,4] ## intercept for vacant to vacant  
params$multi_betavo <- multi.params$beta[draws,4,3] ## intercept for vacant to other
params$multi_betavc <- multi.params$beta[draws,4,2] ## intercept for vacant to crem
params$multi_betavl <- multi.params$beta[draws,4,1] ## intercept for vacant to liom
params$multi_betav <- multi.params$beta[draws,5,4] ## Size specific vacant slope
# Prev Other
params$multi_betaov <- multi.params$beta[draws,3,4]
params$multi_betaoo <- multi.params$beta[draws,3,3]
params$multi_betaoc <- multi.params$beta[draws,3,2]
params$multi_betaol <- multi.params$beta[draws,3,1]
params$multi_betao <- multi.params$beta[draws,5,3]
# Prev Crem
params$multi_betacv <- multi.params$beta[draws,2,4]
params$multi_betaco <- multi.params$beta[draws,2,3]
params$multi_betacc <- multi.params$beta[draws,2,2]
params$multi_betacl <- multi.params$beta[draws,2,1]
params$multi_betac <- multi.params$beta[draws,5,2]
# Prev Liom
params$multi_betalv <- multi.params$beta[draws,1,4]
params$multi_betalo <- multi.params$beta[draws,1,3]
params$multi_betalc <- multi.params$beta[draws,1,2]
params$multi_betall <- multi.params$beta[draws,1,1]
params$multi_betal <- multi.params$beta[draws,5,1]

## Source the functions
source("03_cholla_ant_IPM_params_functions.R")

## Run the simulation to calculate lambdas
# Set the order or the scenarios
scenario = c("none","cremvac","liomvac","othervac","liomcremvac","liomvacother","othercremvac","all")
max_scenario = length(scenario)
# Choose the number of parameter iterations 
max_rep = 1 ## Posterior Draws from vital rate models
# Choose the number of years for stochasticity
max_yrs = 1 ## Years of randomly sampled annual effects
# Create an empty matrix to fill with the lambda estimations using functions defined in IPM_Stochastic_Post.R
# Rows correspond to parameter iterations
# Columns correspond to partner scenarios
lams_stoch_ants1 <- matrix(nrow = max_rep, ncol = max_scenario)
mats <- list()
for(n in 1:max_scenario){
  print(scenario[n])
  for(m in 1:max_rep){
    lams_stoch_ants1[m,n] <- lambdaSim(params=params[m,],
                                 lower=lower,
                                 upper=upper,
                                 scenario = scenario[n],
                                 floor=25,
                                 ceiling=4,
                                 matsize=500,
                                 grow_rfx1 = grow_rfx1[m,],
                                 grow_rfx2 = grow_rfx2[m,],
                                 grow_rfx3 = grow_rfx3[m,],
                                 grow_rfx4 = grow_rfx4[m,],
                                 surv_rfx1 = surv_rfx1[m,],
                                 surv_rfx2 = surv_rfx2[m,],
                                 surv_rfx3 = surv_rfx3[m,],
                                 surv_rfx4 = surv_rfx4[m,],
                                 flow_rfx = flow_rfx[m,],
                                 repro_rfx = repro_rfx[m,],
                                 viab_rfx1 = viab_rfx1[m,],
                                 viab_rfx2 = viab_rfx2[m,],
                                 viab_rfx3 = viab_rfx3[m,],
                                 viab_rfx4 = viab_rfx4[m,],## viability model year rfx
                                 max_yrs = max_yrs        ## the # years you want to iterate
    )$lambdaS
  }
}
# Set the names of each column to the corresponding partner scenario and save the results as a csv
colnames(lams_stoch_ants1) <- scenario
write.csv(lams_stoch_ants1,"stoch_post_lambda_ants1.csv")

################################################################################
## Set the new ant conditions -- in this case it means leaving vacant as is and 
## assigning all other ants equal probabilities
################################################################################
######################################################
#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE. One ant option
transition.1<-function(x, i, j,params, scenario){
  #Transforms all values below/above limits in min/max size
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
# t2 <- matrix(NA,ncol = length(i), nrow = (10))
# for(m in 1:10){
#   for(n in 1:length(i)){
#     t2[m,n] <- transition.2(x[n],i[n],j[n],params[m,],scenario)
#   }
# }
# t2
#
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
# t3 <- matrix(NA,ncol = length(i), nrow = (N_draws))
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
# t <- matrix(NA,ncol = length(i), nrow = (10))
# for(m in 1:10){
#   for(n in 1:length(i)){
#     t[m,n] <- transition.x(x[n],i[n],j[n],params[m,],scenario)
#   }
# }
# t
## Source the functions
source("03_cholla_ant_IPM_params_functions.R")

## Run the simulation to calculate lambdas
# Set the order or the scenarios
scenario = c("none","cremvac","liomvac","othervac","liomcremvac","liomvacother","othercremvac","all")
max_scenario = length(scenario)
# Choose the number of parameter iterations 
max_rep = 1 ## Posterior Draws from vital rate models
# Choose the number of years for stochasticity
max_yrs = 1 ## Years of randomly sampled annual effects
# Create an empty matrix to fill with the lambda estimations using functions defined in IPM_Stochastic_Post.R
# Rows correspond to parameter iterations
# Columns correspond to partner scenarios
lams_stoch_ants1 <- matrix(nrow = max_rep, ncol = max_scenario)
mats <- list()
for(n in 1:max_scenario){
  print(scenario[n])
  for(m in 1:max_rep){
    lams_stoch_ants1[m,n] <- lambdaSim(params=params[m,],
                                       lower=lower,
                                       upper=upper,
                                       scenario = scenario[n],
                                       floor=25,
                                       ceiling=4,
                                       matsize=500,
                                       grow_rfx1 = grow_rfx1[m,],
                                       grow_rfx2 = grow_rfx2[m,],
                                       grow_rfx3 = grow_rfx3[m,],
                                       grow_rfx4 = grow_rfx4[m,],
                                       surv_rfx1 = surv_rfx1[m,],
                                       surv_rfx2 = surv_rfx2[m,],
                                       surv_rfx3 = surv_rfx3[m,],
                                       surv_rfx4 = surv_rfx4[m,],
                                       flow_rfx = flow_rfx[m,],
                                       repro_rfx = repro_rfx[m,],
                                       viab_rfx1 = viab_rfx1[m,],
                                       viab_rfx2 = viab_rfx2[m,],
                                       viab_rfx3 = viab_rfx3[m,],
                                       viab_rfx4 = viab_rfx4[m,],## viability model year rfx
                                       max_yrs = max_yrs        ## the # years you want to iterate
    )$lambdaS
  }
}
# Set the names of each column to the corresponding partner scenario and save the results as a csv
colnames(lams_stoch_ants1) <- scenario
write.csv(lams_stoch_ants1,"stoch_post_lambda_ants1.csv")
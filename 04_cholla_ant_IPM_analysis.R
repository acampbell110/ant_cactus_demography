#######################################################################################################
#######################################################################################################
##                            Call the IPM and understand the outputs                                ##
#######################################################################################################
#######################################################################################################
source("03_cholla_ant_IPM_params_functions.R")
## Set the colors for the visuals
vcol <- "#ad90ec"
lcol <- "#084f98"
ccol <- "#e9a67a"
ocol <- "#93022f"
lccol <- "#5dc9cf"
locol <- "#cf3545"
cocol <- "#ab59c8"
acol <- "#5d906b"
cols <- c(vcol, ccol, lcol, ocol, lccol, locol, cocol, acol)
# mean_params=data.frame(t(colMeans(params)))
# mean_grow_rfx1=matrix(data = colMeans(grow_rfx1), nrow = 1)
# grow_rfx1 <- rbind(grow_rfx1,mean_grow_rfx1)
# mean_grow_rfx2=matrix(data = colMeans(grow_rfx2), nrow = 1)
# grow_rfx2 <- rbind(grow_rfx2,mean_grow_rfx2)
# mean_grow_rfx3=matrix(data = colMeans(grow_rfx3), nrow = 1)
# grow_rfx3 <- rbind(grow_rfx3,mean_grow_rfx3)
# mean_grow_rfx4=matrix(data = colMeans(grow_rfx4), nrow = 1)
# grow_rfx4 <- rbind(grow_rfx4,mean_grow_rfx4)
# mean_grow_rfx=matrix(data = colMeans(grow_rfx), nrow = 1)
# grow_rfx <- rbind(grow_rfx,mean_grow_rfx)
# mean_surv_rfx1=matrix(data = colMeans(surv_rfx1), nrow = 1)
# surv_rfx1 <- rbind(surv_rfx1,mean_surv_rfx1)
# mean_surv_rfx2=matrix(data = colMeans(surv_rfx2), nrow = 1)
# surv_rfx2 <- rbind(surv_rfx2,mean_surv_rfx2)
# mean_surv_rfx3=matrix(data = colMeans(surv_rfx3), nrow = 1)
# surv_rfx3 <- rbind(surv_rfx3,mean_surv_rfx3)
# mean_surv_rfx4=matrix(data = colMeans(surv_rfx4), nrow = 1)
# surv_rfx4 <- rbind(surv_rfx4,mean_surv_rfx4)
# mean_surv_rfx=matrix(data = colMeans(surv_rfx), nrow = 1)
# surv_rfx <- rbind(surv_rfx,mean_surv_rfx)
# mean_viab_rfx1=matrix(data = colMeans(viab_rfx1), nrow = 1)
# viab_rfx1 <- rbind(viab_rfx1,mean_viab_rfx1)
# mean_viab_rfx2=matrix(data = colMeans(viab_rfx2), nrow = 1)
# viab_rfx2 <- rbind(viab_rfx2,mean_viab_rfx2)
# mean_viab_rfx3=matrix(data = colMeans(viab_rfx3), nrow = 1)
# viab_rfx3 <- rbind(viab_rfx3,mean_viab_rfx3)
# mean_viab_rfx4=matrix(data = colMeans(viab_rfx4), nrow = 1)
# viab_rfx4 <- rbind(viab_rfx4,mean_viab_rfx4)
# mean_viab_rfx=matrix(data = colMeans(viab_rfx), nrow = 1)
# viab_rfx <- rbind(viab_rfx,mean_viab_rfx)
# mean_repro_rfx=matrix(data = colMeans(repro_rfx), nrow = 1)
# repro_rfx <- rbind(repro_rfx,mean_repro_rfx)
# mean_flow_rfx=matrix(data = colMeans(flow_rfx), nrow = 1)
# flow_rfx <- rbind(flow_rfx,mean_flow_rfx)
# 
# dim(grow_rfx1)
# dim(grow_rfx2)
# dim(grow_rfx3)
# dim(grow_rfx4)
# dim(grow_rfx)
# dim(surv_rfx1)
# dim(surv_rfx2)
# dim(surv_rfx3)
# dim(surv_rfx4)
# dim(surv_rfx)
# dim(viab_rfx1)
# dim(viab_rfx2)
# dim(viab_rfx3)
# dim(viab_rfx4)
# dim(viab_rfx)
# dim(repro_rfx)
# dim(flow_rfx)

######################################################################################################
############################### DETERMINISTIC POST IPM ###############################################
##                        Calculate the lambda posterior distributions                              ##
######################################################################################################
######################################################################################################
#### Calculate the lambda posterior distributions
# Create an empty matrix to fill with the lambda estimations using functions defined in IPM_Post.R
# Rows correspond to parameter iterations
# Columns correspond to partner scenarios
scenario = c("none","cremvac","liomvac","othervac","liomcremvac","liomvacother","othercremvac","all")
max_scenario = length(scenario)
max_rep = 1
lams_dpost <- matrix(rep(NA, max_rep*max_scenario), nrow = max_rep, ncol = max_scenario)
for(z in 1:max_scenario){
  print(z)
  for(m in 1:max_rep){
  lams_dpost[max_rep,z] <- lambda(bigmatrix(params=mean_params,
                                      lower=lower,
                                      upper=upper,
                                      scenario = scenario[z],
                                      floor=25,
                                      ceiling=4,
                                      matsize=500,
                                      grow_rfx1=0,grow_rfx2=0,grow_rfx3=0,grow_rfx4=0,
                                      surv_rfx1=0,surv_rfx2=0,surv_rfx3=0,surv_rfx4=0,
                                      flow_rfx=0,repro_rfx=0,
                                      viab_rfx1=0,viab_rfx2=0,viab_rfx3=0,viab_rfx4=0)$IPMmat)
  }
}
lams_dpost
# Set the names of each column to the corresponding partner scenario and save the results as a csv
colnames(lams_dpost) <- scenario
write.csv(lams_dpost,"det_post_lambda_mean.csv")

######################################################################################################
################################## STOCHASTIC POST IPM ###############################################
##                       Calculate the lambda posterior distributions                         ########
######################################################################################################
######################################################################################################
#### Calculate the lambda posterior distributions with stochasticity
# Set the order or the scenarios
scenario = c("none","cremvac","liomvac","othervac","liomcremvac","liomvacother","othercremvac","all")
max_scenario = length(scenario)
# Choose the number of parameter iterations 
max_rep = 1000 ## Posterior Draws from vital rate models
# Choose the number of years for stochasticity
max_yrs = 1000 ## Years of randomly sampled annual effects
# Create an empty matrix to fill with the lambda estimations using functions defined in IPM_Stochastic_Post.R
# Rows correspond to parameter iterations
# Columns correspond to partner scenarios
lams_stoch <- matrix(nrow = max_rep, ncol = max_scenario)
for(n in 1:max_scenario){
  print(scenario[n])
  for(m in 1:max_rep){
  lams_stoch[m,n] <- lambdaSim(params=mean_params,
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
                               )
  }
}
# Set the names of each column to the corresponding partner scenario and save the results as a csv
colnames(lams_stoch) <- scenario
write.csv(lams_stoch,"stoch_post_lambda.csv")

######################################################################################################
################################## STOCHASTIC NULL POST IPM ##########################################
##                       Calculate the lambda posterior distributions                               ##
######################################################################################################
######################################################################################################
#### Calculate the lambda posterior distributions with stochasticity and no possible synchronicity of 
#### ant effects
# Set the order or the scenarios
scenario = c("none","cremvac","liomvac","othervac","liomcremvac","liomvacother","othercremvac","all")
max_scenario = length(scenario)
# Choose the number of parameter iterations 
max_rep = 1 ## Posterior Draws from vital rate models
# Choose the number of years for stochasticity
max_yrs = 1000 ## Years of randomly sampled annual effects
# Create an empty matrix to fill with the lambda estimations using functions defined in IPM_Stochastic_Post.R
# Rows correspond to parameter iterations
# Columns correspond to partner scenarios
lams_stoch_null_mean <- matrix(nrow = 1, ncol = max_scenario)
for(n in 1:max_scenario){
  print(scenario[n])
  for(m in 1:max_rep){
    lams_stoch_null[m,n] <- lambdaSim(params=mean_params,
                                      lower=lower,
                                      upper=upper,
                                      scenario = scenario[n],
                                      floor=25,
                                      ceiling=4,
                                      matsize=500,
                                      grow_rfx1 = grow_rfx[m,],
                                      grow_rfx2 = grow_rfx[m,],
                                      grow_rfx3 = grow_rfx[m,],
                                      grow_rfx4 = grow_rfx[m,],
                                      surv_rfx1 = surv_rfx[m,],
                                      surv_rfx2 = surv_rfx[m,],
                                      surv_rfx3 = surv_rfx[m,],
                                      surv_rfx4 = surv_rfx[m,],
                                      flow_rfx = flow_rfx[m,],
                                      repro_rfx = repro_rfx[m,],
                                      viab_rfx1 = viab_rfx[m,],
                                      viab_rfx2 = viab_rfx[m,],
                                      viab_rfx3 = viab_rfx[m,],
                                      viab_rfx4 = viab_rfx[m,],
                                      max_yrs = max_yrs  )
  }
}
# Set the names of each column to the corresponding partner scenario and save the results as a csv
colnames(lams_stoch_null) <- scenario
write.csv(lams_stoch_null,"stoch_null_post_lambda.csv")


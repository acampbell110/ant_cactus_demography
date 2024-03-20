################################################################################
################################################################################
##                            Call the IPM and understand the outputs            
################################################################################
################################################################################
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

################################################################################
##                                    DETERMINISTIC POST IPM
##                        Calculate the lambda posterior distributions     
################################################################################
################################################################################
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

################################################################################
##                                   STOCHASTIC POST IPM
##                       Calculate the lambda posterior distributions       
################################################################################
################################################################################
#### Calculate the lambda posterior distributions with stochasticity
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
lams_stoch <- matrix(nrow = max_rep, ncol = max_scenario)
mats <- list()
for(n in 1:max_scenario){
  print(scenario[n])
  for(m in 1:max_rep){
  lams_stoch[m,n] <- lambdaSim(params=params[m,],
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
colnames(lams_stoch) <- scenario
write.csv(lams_stoch,"stoch_post_lambda.csv")
#### Check the stable stage distribution of sizes
# Pull out the mean matrix
params_mean <- matrix(rep(0,ncol(params)), nrow = 1)
for(i in 1:ncol(params)){
  params_mean[,i] <- mean(params[,i])
}
colMeans(params)
colnames(params_mean) <- colnames(params)
params_mean <- as.data.frame(params_mean)
all_stable <- stable.stage(bigmatrix(params = (params[m,]),
                                      scenario = "all",
                                      lower = lower,
                                      upper = upper,
                                      floor = floor,
                                      ceiling = ceiling,
                                      matsize = matsize,
                                      mean(grow_rfx1),mean(grow_rfx2),mean(grow_rfx3),mean(grow_rfx4),
                                      mean(surv_rfx1),mean(surv_rfx2),mean(surv_rfx3),mean(surv_rfx4),
                                      mean(flow_rfx),
                                      mean(repro_rfx),
                                      mean(viab_rfx1),mean(viab_rfx2),mean(viab_rfx3),mean(viab_rfx4))$IPMmat)
# standardize all_stable s.t. the seed banks are excluded (aka all_stable[3:2002] sums to 1)
all_stable_stan <- (all_stable[3:2002])
sum(all_stable_stan)

vac_IPM <- bigmatrix(params = (params[m,]),
                     scenario = "none",
                     lower = lower,
                     upper = upper,
                     floor = floor,
                     ceiling = ceiling,
                     matsize = matsize,
                     mean(grow_rfx1),mean(grow_rfx2),mean(grow_rfx3),mean(grow_rfx4),
                     mean(surv_rfx1),mean(surv_rfx2),mean(surv_rfx3),mean(surv_rfx4),
                     mean(flow_rfx),
                     mean(repro_rfx),
                     mean(viab_rfx1),mean(viab_rfx2),mean(viab_rfx3),mean(viab_rfx4))
vac_stable <- stable.stage(vac_IPM$IPMmat)



vac_stable_mod <- vac_stable[3:502]

sequence <- seq(lower,upper,length = 500)
png("Figures/tester.png")
plot(vac_IPM$y,vac_stable_mod, type= "b", xlim = c(-4,2))
dev.off()


liomvac_IPM <- bigmatrix(params = (params[m,]),
                     scenario = "liomvac",
                     lower = lower,
                     upper = upper,
                     floor = floor,
                     ceiling = ceiling,
                     matsize = matsize,
                     mean(grow_rfx1),mean(grow_rfx2),mean(grow_rfx3),mean(grow_rfx4),
                     mean(surv_rfx1),mean(surv_rfx2),mean(surv_rfx3),mean(surv_rfx4),
                     mean(flow_rfx),
                     mean(repro_rfx),
                     mean(viab_rfx1),mean(viab_rfx2),mean(viab_rfx3),mean(viab_rfx4))
liomvac_stable <- stable.stage(liomvac_IPM$IPMmat)

lambda(liomvac_IPM$IPMmat)



sequence <- seq(lower,upper,length = 500)
png("Figures/tester.png")
plot(liomvac_IPM$y,liomvac_stable[3:502], type= "b", xlim = c(-4,2), ylim = c(0,.2))
lines(liomvac_IPM$y,liomvac_stable[503:1002], col = vaccol, lwd = 3)
dev.off()
################################################################################
##                               STOCHASTIC NULL POST IPM
##                       Calculate the lambda posterior distributions           
################################################################################
################################################################################
#### Calculate the lambda posterior distributions with stochasticity and no possible synchronicity of 
#### ant effects
# Set the order or the scenarios
scenario = c("none","cremvac","liomvac","othervac","liomcremvac","liomvacother","othercremvac","all")
max_scenario = length(scenario)
# Choose the number of parameter iterations 
max_rep = 50 ## Posterior Draws from vital rate models
# Choose the number of years for stochasticity
max_yrs = 100 ## Years of randomly sampled annual effects
# Create an empty matrix to fill with the lambda estimations using functions defined in IPM_Stochastic_Post.R
# Rows correspond to parameter iterations
# Columns correspond to partner scenarios
lams_stoch_null <- matrix(nrow = max_rep, ncol = max_scenario)
for(n in 1:max_scenario){
  print(scenario[n])
  for(m in 1:max_rep){
    lams_stoch_null[m,n] <- lambdaSim(params=params[m,],
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


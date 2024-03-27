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

all_IPM <- bigmatrix(params = (params[m,]),
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
                     mean(viab_rfx1),mean(viab_rfx2),mean(viab_rfx3),mean(viab_rfx4))
# Pull out the exact size distribution from the matrix above
all_y <- all_IPM$y
## Check that the lambda makes sense before going any further
lambda(all_IPM$IPMmat) # Looks correct!
# Pull out the IPM from the matrix above
all_stable <- stable.stage(all_IPM$IPMmat)
# Remove the seed banks
all_stable_cut <- all_stable[3:2002]
# Standardize -- NOTE THIS DOES NOT APPEAR TO HAVE ANY AFFECT ON THE PLOT
all_stable_stan <- all_stable_cut/sum(all_stable_cut)
sum(all_stable_stan)
png("Figures/stable_all_test.png")
plot(all_y,(all_stable_stan[1:500]/sum(all_stable_stan)), col = cremcol, type = "l", xlim = c(-5,15), ylim = c(0,0.025))
lines(all_y, all_stable_stan[501:1000]/sum(all_stable_stan), col = liomcol)
lines(all_y, all_stable_stan[1001:1500]/sum(all_stable_stan), col = othercol)
lines(all_y, all_stable_stan[1501:2000]/sum(all_stable_stan), col = vaccol)
legend("topleft", legend = c("Crem.","Liom.","Other","Vac."), fill = c(cremcol,liomcol,othercol,vaccol))
dev.off()

png("Figures/stable_all_sum.png")
plot(all_y, (all_stable_stan[1:500]+all_stable_stan[501:1000]+ all_stable_stan[1001:1500]+ all_stable_stan[1501:2000]))
dev.off()


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
# Pull out the exact size distribution from the matrix above
vac_y <- vac_IPM$y
## Check that the lambda makes sense before going any further
lambda(vac_IPM$IPMmat) # Looks correct!
# Pull out the IPM from the matrix above
vac_stable <- stable.stage(vac_IPM$IPMmat)
# Remove the seed banks
vac_stable_cut <- vac_stable[3:502]
# Standardize -- NOTE THIS DOES NOT APPEAR TO HAVE ANY AFFECT ON THE PLOT
vac_stable_stan <- vac_stable_cut/sum(vac_stable_cut)
sum(vac_stable_stan)
png("Figures/stable_vac_test.png")
plot(vac_IPM$y,vac_stable_stan, type= "l", col = vaccol)
legend("topleft", legend = c("Crem.","Liom.","Other","Vac."), fill = c(cremcol,liomcol,othercol,vaccol))
abline(v = lower, col = "black", lty = 2)
abline(v = upper, col = "black", lty = 2)
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
# Pull out the exact size distribution from the matrix above
liomvac_y <- liomvac_IPM$y
## Check that the lambda makes sense before going any further
lambda(liomvac_IPM$IPMmat) # Looks correct!
# Pull out the IPM from the matrix above
liomvac_stable <- stable.stage(liomvac_IPM$IPMmat)
# Remove the seed banks
liomvac_stable_cut <- liomvac_stable[3:1002]
# Standardize -- NOTE THIS DOES NOT APPEAR TO HAVE ANY AFFECT ON THE PLOT
liomvac_stable_stan <- liomvac_stable_cut/sum(liomvac_stable_cut)
sum(liomvac_stable_stan)
png("Figures/stable_liomvac_test.png")
plot(liomvac_IPM$y,liomvac_stable_stan[1:500], type= "l", xlim = c(-5,15), col = liomcol, ylim = c(0,0.05))
lines(liomvac_IPM$y, liomvac_stable_stan[501:1000], col = vaccol)
legend("topleft", legend = c("Crem.","Liom.","Other","Vac."), fill = c(cremcol,liomcol,othercol,vaccol))
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



png("Figures/tester.png")
hist(cactus$logsize_t)
dev.off()
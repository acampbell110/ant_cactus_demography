################################################################################
################################################################################
##                            Call the IPM and understand the outputs            
################################################################################
################################################################################
source("03_cholla_ant_IPM_params_functions.R")
## Set the colors for the visuals
cremcol <- "#9239F6"
liomcol <- "#00A08A"
othercol <- "#FF0076"
vaccol <- "#F8B660"
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
#### Check the stable stage distribution of sizes
################################################################################
########## all partner scenario
# Pull out the mean matrix
params_mean <- matrix(rep(0,ncol(params)), nrow = 1)
for(i in 1:ncol(params)){
  params_mean[,i] <- mean(params[,i])
}
colMeans(params)
colnames(params_mean) <- colnames(params)
params_mean <- as.data.frame(params_mean)
m = 1
all_IPM <- bigmatrix(params = (params_mean[m,]),
                     scenario = "all",
                     lower = cholla_min - 5,
                     upper = cholla_max + 5,
                     floor = floor,
                     ceiling = ceiling,
                     matsize = matsize,
                     mean(grow_rfx1),mean(grow_rfx2),mean(grow_rfx3),mean(grow_rfx4),
                     mean(surv_rfx1),mean(surv_rfx2),mean(surv_rfx3),mean(surv_rfx4),
                     mean(flow_rfx),
                     mean(repro_rfx),
                     mean(viab_rfx1),mean(viab_rfx2),mean(viab_rfx3),mean(viab_rfx4))
##View(all_IPM$Tmat)
# Pull out the exact size distribution from the matrix above
all_y <- all_IPM$y
## Check that the lambda makes sense before going any further
all_mean <- lambda(all_IPM$IPMmat) # Looks correct!
# Pull out the IPM from the matrix above
# all_stable <- stable.stage(all_IPM$IPMmat)
# # Remove the seed banks
# all_stable_cut <- all_stable[3:2002]
# # Standardize -- NOTE THIS DOES NOT APPEAR TO HAVE ANY AFFECT ON THE PLOT
# all_stable_stan <- all_stable_cut/sum(all_stable_cut)
# sum(all_stable_stan)
# png("Figures/stable_all_test.png")
# plot(all_y,(all_stable_stan[1:500]/sum(all_stable_stan)), col = cremcol, type = "l", xlim = c(-5,15), ylim = c(0,0.015))
# lines(all_y, all_stable_stan[501:1000]/sum(all_stable_stan), col = liomcol)
# lines(all_y, all_stable_stan[1001:1500]/sum(all_stable_stan), col = othercol)
# lines(all_y, all_stable_stan[1501:2000]/sum(all_stable_stan), col = vaccol)
# legend("topleft", legend = c("Crem.","Liom.","Other","Vac."), fill = c(cremcol,liomcol,othercol,vaccol))
# dev.off()
# 
# png("Figures/stable_all_sum.png")
# plot(all_y, (all_stable_stan[1:500]+all_stable_stan[501:1000]+ all_stable_stan[1001:1500]+ all_stable_stan[1501:2000]))
# dev.off()

########## Vacant scenario
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
vac_mean <- lambda(vac_IPM$IPMmat) # Looks correct!
# Pull out the IPM from the matrix above
# vac_stable <- stable.stage(vac_IPM$IPMmat)
# # Remove the seed banks
# vac_stable_cut <- vac_stable[3:502]
# # Standardize -- NOTE THIS DOES NOT APPEAR TO HAVE ANY AFFECT ON THE PLOT
# vac_stable_stan <- vac_stable_cut/sum(vac_stable_cut)
# sum(vac_stable_stan)
# png("Figures/stable_vac_test.png")
# plot(vac_IPM$y,vac_stable_stan, type= "l", col = vaccol)
# legend("topleft", legend = c("Crem.","Liom.","Other","Vac."), fill = c(cremcol,liomcol,othercol,vaccol))
# abline(v = lower, col = "black", lty = 2)
# abline(v = upper, col = "black", lty = 2)
# dev.off()

########## Liom and Vac Scenario
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
liom_mean <- lambda(liomvac_IPM$IPMmat) # Looks correct!
# Pull out the IPM from the matrix above
# liomvac_stable <- stable.stage(liomvac_IPM$IPMmat)
# # Remove the seed banks
# liomvac_stable_cut <- liomvac_stable[3:1002]
# # Standardize -- NOTE THIS DOES NOT APPEAR TO HAVE ANY AFFECT ON THE PLOT
# liomvac_stable_stan <- liomvac_stable_cut/sum(liomvac_stable_cut)
# sum(liomvac_stable_stan)
# png("Figures/stable_liomvac_test.png")
# plot(liomvac_IPM$y,liomvac_stable_stan[1:500], type= "l", xlim = c(-5,15), col = liomcol, ylim = c(0,0.02))
# lines(liomvac_IPM$y, liomvac_stable_stan[501:1000], col = vaccol)
# legend("topleft", legend = c("Crem.","Liom.","Other","Vac."), fill = c(cremcol,liomcol,othercol,vaccol))
# dev.off()

########## Other Vacant Scenario
other_IPM <- bigmatrix(params = (params_mean[m,]),
                       scenario = "othervac",
                       lower = cholla_min - 5,
                       upper = cholla_max + 5,
                       floor = floor,
                       ceiling = ceiling,
                       matsize = matsize,
                       mean(grow_rfx1),mean(grow_rfx2),mean(grow_rfx3),mean(grow_rfx4),
                       mean(surv_rfx1),mean(surv_rfx2),mean(surv_rfx3),mean(surv_rfx4),
                       mean(flow_rfx),
                       mean(repro_rfx),
                       mean(viab_rfx1),mean(viab_rfx2),mean(viab_rfx3),mean(viab_rfx4))
##View(all_IPM$Tmat)
# Pull out the exact size distribution from the matrix above
other_y <- other_IPM$y
## Check that the lambda makes sense before going any further
other_mean <- lambda(other_IPM$IPMmat) # Looks correct!

########## Crem Vacant Scenario
crem_IPM <- bigmatrix(params = (params_mean[m,]),
                      scenario = "cremvac",
                      lower = cholla_min - 5,
                      upper = cholla_max + 5,
                      floor = floor,
                      ceiling = ceiling,
                      matsize = matsize,
                      mean(grow_rfx1),mean(grow_rfx2),mean(grow_rfx3),mean(grow_rfx4),
                      mean(surv_rfx1),mean(surv_rfx2),mean(surv_rfx3),mean(surv_rfx4),
                      mean(flow_rfx),
                      mean(repro_rfx),
                      mean(viab_rfx1),mean(viab_rfx2),mean(viab_rfx3),mean(viab_rfx4))
##View(all_IPM$Tmat)
# Pull out the exact size distribution from the matrix above
crem_y <- crem_IPM$y
## Check that the lambda makes sense before going any further
crem_mean <- lambda(crem_IPM$IPMmat) # Looks correct!

########## Liom Crem Vac Scenario
liomcrem_IPM <- bigmatrix(params = (params_mean[m,]),
                          scenario = "liomcremvac",
                          lower = cholla_min - 5,
                          upper = cholla_max + 5,
                          floor = floor,
                          ceiling = ceiling,
                          matsize = matsize,
                          mean(grow_rfx1),mean(grow_rfx2),mean(grow_rfx3),mean(grow_rfx4),
                          mean(surv_rfx1),mean(surv_rfx2),mean(surv_rfx3),mean(surv_rfx4),
                          mean(flow_rfx),
                          mean(repro_rfx),
                          mean(viab_rfx1),mean(viab_rfx2),mean(viab_rfx3),mean(viab_rfx4))
##View(all_IPM$Tmat)
# Pull out the exact size distribution from the matrix above
liomcrem_y <- liomcrem_IPM$y
## Check that the lambda makes sense before going any further
liomcrem_mean <- lambda(liomcrem_IPM$IPMmat) # Looks correct!

########## Liom Other Vac Scenario
liomother_IPM <- bigmatrix(params = (params_mean[m,]),
                          scenario = "liomvacother",
                          lower = cholla_min - 5,
                          upper = cholla_max + 5,
                          floor = floor,
                          ceiling = ceiling,
                          matsize = matsize,
                          mean(grow_rfx1),mean(grow_rfx2),mean(grow_rfx3),mean(grow_rfx4),
                          mean(surv_rfx1),mean(surv_rfx2),mean(surv_rfx3),mean(surv_rfx4),
                          mean(flow_rfx),
                          mean(repro_rfx),
                          mean(viab_rfx1),mean(viab_rfx2),mean(viab_rfx3),mean(viab_rfx4))
##View(all_IPM$Tmat)
# Pull out the exact size distribution from the matrix above
liomother_y <- liomother_IPM$y
## Check that the lambda makes sense before going any further
liomother_mean <- lambda(liomother_IPM$IPMmat) # Looks correct!

########## Other Crem Vac Scenario
othercrem_IPM <- bigmatrix(params = (params_mean[m,]),
                                            scenario = "othercremvac",
                                            lower = cholla_min - 5,
                                            upper = cholla_max + 5,
                                            floor = floor,
                                            ceiling = ceiling,
                                            matsize = matsize,
                                            mean(grow_rfx1),mean(grow_rfx2),mean(grow_rfx3),mean(grow_rfx4),
                                            mean(surv_rfx1),mean(surv_rfx2),mean(surv_rfx3),mean(surv_rfx4),
                                            mean(flow_rfx),
                                            mean(repro_rfx),
                                            mean(viab_rfx1),mean(viab_rfx2),mean(viab_rfx3),mean(viab_rfx4))
##View(all_IPM$Tmat)
# Pull out the exact size distribution from the matrix above
othercrem_y <- othercrem_IPM$y
## Check that the lambda makes sense before going any further
othercrem_mean <- lambda(othercrem_IPM$IPMmat) # Looks correct!

########## Plot the means of the lambda distributions
scenario_abv <- c("V","C","L","O","LC","LO","OC","LOC")
x_vals <- c(1,3,5,7,9,11,13,15)
lambdas <- c(vac_mean,crem_mean,liom_mean,other_mean,liomcrem_mean,liomother_mean,othercrem_mean,all_mean)
colors_lambdas <- c(vcol,ccol,lcol,ocol,lccol,locol,cocol,acol)
png("Manuscript/Figures/Lambda_means_equal.png")
plot(x_vals,lambdas,col = colors_lambdas, pch = 20, cex = 3, xlim = c(0,16),ylim = c(0.935,0.984),
     xlab = "",ylab = "",xaxt = "n")
text(x = c(1,3,5,7,9,11,13,15)-0.2, y = lambdas+0.003,cex = 1, labels = scenario_abv)
#legend("topleft",legend = scenario_abv,fill = colors_lambdas)
mtext("Diversity Scenario",side=1,line=-1.5,outer=TRUE,cex=1.5)
mtext("Lambda",side=2,line=-1.5,outer=TRUE,cex=1.5,las=0)
dev.off()

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
max_rep = 100 ## Posterior Draws from vital rate models
# Choose the number of years for stochasticity
max_yrs = 100 ## Years of randomly sampled annual effects
# Create an empty matrix to fill with the lambda estimations using functions defined in IPM_Stochastic_Post.R
# Rows correspond to parameter iterations
# Columns correspond to partner scenarios
lams_stoch <- matrix(nrow = max_rep, ncol = max_scenario)
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
                               )
  }
}
# Set the names of each column to the corresponding partner scenario and save the results as a csv
colnames(lams_stoch) <- scenario
write.csv(lams_stoch,"stoch_freq_lambda.csv")


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
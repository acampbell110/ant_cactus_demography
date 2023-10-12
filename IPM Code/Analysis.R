#######################################################################################################
#######################################################################################################
##                            Call the IPM and understand the outputs                                ##
#######################################################################################################
#######################################################################################################
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code")
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

######################################################################################################
############################### DETERMINISTIC POST IPM ###############################################
##                        Calculate the lambda posterior distributions                              ##
######################################################################################################
######################################################################################################
#### Calculate the lambda posterior distributions
# Create an empty matrix to fill with the lambda estimations using functions defined in IPM_Post.R
# Rows correspond to parameter iterations
# Columns correspond to partner scenarios
lams_dpost <- matrix(rep(NA, 100*8), nrow = 100, ncol = 8)
scenario = c("none","cremvac","liomvac","othervac","liomcremvac","liomvacother","othercremvac","all")
for(z in 1:length(scenario)){
  print(z)
  for(m in 1:100){
    lams_dpost[m,z] <- lambda(bigmatrix(params[m,],lower,upper,matsize,scenario[z])$IPMmat)
    }
}
lams_dpost
# Set the names of each column to the corresponding partner scenario and save the results as a csv
colnames(lams_dpost) <- scenario
write.csv(lams_dpost,"det_post_lambda.csv")

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
    lams_stoch[m,n] <- lambdaSim(params = params[m,],## parameters
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
                          max_yrs = max_yrs,        ## the # years you want to iterate
                          matsize=matsize,          ## size of transition matrix
                          scenario = scenario[n],   ## partner diversity scenario
                          lower=lower,upper=upper  )
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
max_rep = 100 ## Posterior Draws from vital rate models
# Choose the number of years for stochasticity
max_yrs = 100 ## Years of randomly sampled annual effects
# Create an empty matrix to fill with the lambda estimations using functions defined in IPM_Stochastic_Post.R
# Rows correspond to parameter iterations
# Columns correspond to partner scenarios
lams_stoch_null <- matrix(nrow = max_rep, ncol = max_scenario)
for(n in 1:max_scenario){
  print(scenario[n])
  for(m in 1:max_rep){
    lams_stoch_null[m,n] <- lambdaSim(params = params[m,],## parameters
                                 grow_rfx1 = grow_rfx1[m,],
                                 grow_rfx2 = grow_rfx1[m,],
                                 grow_rfx3 = grow_rfx1[m,],
                                 grow_rfx4 = grow_rfx1[m,],
                                 surv_rfx1 = surv_rfx1[m,],
                                 surv_rfx2 = surv_rfx1[m,],
                                 surv_rfx3 = surv_rfx1[m,],
                                 surv_rfx4 = surv_rfx1[m,],
                                 flow_rfx = flow_rfx[m,],
                                 repro_rfx = repro_rfx[m,],
                                 viab_rfx1 = viab_rfx1[m,],
                                 viab_rfx2 = viab_rfx1[m,],
                                 viab_rfx3 = viab_rfx1[m,],
                                 viab_rfx4 = viab_rfx1[m,],## viability model year rfx
                                 max_yrs = max_yrs,        ## the # years you want to iterate
                                 matsize=matsize,          ## size of transition matrix
                                 scenario = scenario[n],   ## partner diversity scenario
                                 lower=lower,upper=upper  )
  }
}
# Set the names of each column to the corresponding partner scenario and save the results as a csv
colnames(lams_stoch_null) <- scenario
write.csv(lams_stoch_null,"stoch_null_post_lambda.csv")

######################################################################################################
######################################################################################################
####                PULL IN THE DETERMINISTIC AND STOCHASTIC DISTRIBUTIONS                        ####
######################################################################################################
######################################################################################################
## Read in lambda estimates
## deterministic
lams_dpost <- read.csv("det_post_lambda.csv")
lams_dpost <- lams_dpost[,-c(1)]
## stochastic
lams_stoch <- read.csv("stoch_post_lambda.csv")
lams_stoch <- lams_stoch[,-c(1)]
## stochastic null
lams_stoch_null <- read.csv("stoch_null_post_lambda.csv")
lams_stoch_null <- lams_stoch_null[,-c(1)]

######################################################################################################
######################################################################################################
####                     VISUALIZE EACH OF THE POSTERIOR DISTRIBUTIONS                            ####
######################################################################################################
######################################################################################################
# Set the working directory to the figures folder
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")

## Plot the means of the deterministic and stochastic distributions together
png("lambda_means.png")
plot(c(1,3,5,7,9,11,13,15),colMeans(lams_dpost),
     col = cols, pch = 20, cex = 5,
     xlim = c(0,16), ylim = c(0.985,1.01),
     xaxt = "n",cex.lab = 2,
     xlab = "Ant Scenario", ylab = "Mean Lambda Value", main = "Full Partner Diversity Leads to \n Highest Fitness")
     text(x = c(1,3,5,7,9,11,13,15)-0.2, y = colMeans(lams_dpost)+0.002,cex = 2, labels = lams$scenario_abv,srt = 35)
     legend("topleft",legend = c("L = Liom.","C = Crem.","O = Other"),cex = 1.5)
points(c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5),colMeans(lams_stoch),
       col = cols, cex = 5)
points(c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5),colMeans(lams_stoch_null), cex = 5, pch = 20)
dev.off()

## plot the distributions of the deterministic distribution 
png("lambda_det.png")
par(mar=c(4,4,1.01,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(1,1,1,1))
plot(density(lams_dpost[,1]), col = vcol, xlab = "",ylab = "", lwd = 3,cex.main = 2,  main = "a)                                                                                                       ",
     ylim = c(0,100), xlim = c(.97,1.02))
abline(v = mean(lams_dpost[,1]),col = vcol, lty = 2, lwd =3)
legend("topright", legend = c("Vacant"), fill = c(vcol), cex = 1.5)
plot(density(lams_dpost[,2]), col = ccol, xlab = "",ylab = "", lwd = 3,cex.main = 2, main = "b)                                                                                                       ",
     ylim = c(0,100), xlim = c(.97,1.02))
abline(v = mean(lams_dpost[,2]),col = ccol, lty = 2, lwd =3)
lines(density(lams_dpost[,3]), col = lcol, lwd = 3)
abline(v = mean(lams_dpost[,3]),col = lcol, lty = 2, lwd =3)
lines(density(lams_dpost[,4]), col = ocol, lwd = 3)
abline(v = mean(lams_dpost[,4]),col = ocol, lty = 2, lwd =3)
legend("topright", legend = c("Crem.","Liom.", "Other"), fill = c(ccol,lcol,ocol), cex = 1.5)
plot(density(lams_dpost[,5]), col = lccol, xlab = "",ylab = "", lwd = 3,cex.main = 2, main = "c)                                                                                                       ",
     ylim = c(0,100), xlim = c(.97,1.02))
abline(v = mean(lams_dpost[,5]),col = lccol, lty = 4, lwd =3)
lines(density(lams_dpost[,6]), col = locol, lwd = 3)
abline(v = mean(lams_dpost[,6]),col = locol, lty = 2, lwd =3)
lines(density(lams_dpost[,7]), col = cocol, lwd = 3)
abline(v = mean(lams_dpost[,7]),col = cocol, lty = 2, lwd =3)
legend("topright", legend = c("Crem. and Liom.","Liom. and Other", "Crem. and Other"), fill = c(lccol,locol,cocol), cex = 1.5)
plot(density(lams_dpost[,8]), col = acol, xlab = "",ylab = "", lwd = 3,cex.main = 2, main = "d)                                                                                                       ",
     ylim = c(0,100), xlim = c(.97,1.02))
abline(v = mean(lams_dpost[,8]),col = acol, lty = 2, lwd =3)
mtext("Lambda",side=1,line=-2,outer=TRUE,cex=1.3)
mtext("Density",side=2,line=-2,outer=TRUE,cex=1.3,las=0)
legend("topright",legend = c("All Ants"),fill = c(acol),
       cex = 1.5)
dev.off()

## Plot the distributions of the stochastic lambdas
png("lambda_stoch.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(1,1,1,1))
plot(density(lams_stoch[,1]), col = vcol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "a)                                                                                                       ",ylim = c(0,100), xlim = c(0.97,1.02))
abline(v = mean(lams_stoch[,1]),col = vcol, lty = 2, lwd =3)
legend("topright", legend = c("Vacant"), fill = c(vcol), cex = 1.5)
plot(density(lams_stoch[,2]), col = ccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "b)                                                                                                       ",ylim = c(0,100), xlim = c(0.97,1.02))
abline(v = mean(lams_stoch[,2]),col = ccol, lty = 2, lwd =3)
lines(density(lams_stoch[,3]), col = lcol, lwd =3)
abline(v = mean(lams_stoch[,3]),col = lcol, lty = 2, lwd =3)
lines(density(lams_stoch[,4]), col = ocol, lwd =3)
abline(v = mean(lams_stoch[,4]),col = ocol, lty = 2, lwd =3)
legend("topright", legend = c("Crematogaster","Liometopum", "Other"), fill = c(ccol,lcol,ocol), cex = 1.5)
plot(density(lams_stoch[,5]), col = lccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "c)                                                                                                       ",ylim = c(0,100), xlim = c(0.97,1.02))
abline(v = mean(lams_stoch[,5]),col = lccol, lty = 2, lwd =3)
lines(density(lams_stoch[,6]), col = locol, lwd =3)
abline(v = mean(lams_stoch[,6]),col = locol, lty = 2, lwd =3)
lines(density(lams_stoch[,7]), col = cocol, lwd =3)
abline(v = mean(lams_stoch[,7]),col = cocol, lty = 2, lwd =3)
legend("topright", legend = c("Crematogaster and Liometopum","Liometopum and Other", "Crematogaster and Other"), fill = c(lccol,locol,cocol), cex = 1.5)
plot(density(lams_stoch[,8]), col = acol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "d)                                                                                                       ",ylim = c(0,100), xlim = c(0.97,1.02))
abline(v = mean(lams_stoch[,8]),col = acol, lty = 2, lwd =3)
mtext("Lambda",side=1,line=-2,outer=TRUE,cex=1.3)
mtext("Density",side=2,line=-2,outer=TRUE,cex=1.3,las=0)
legend("topright",legend = c("All Ants"),fill = c(acol),
       cex = 1.5)
dev.off()

## Plot the distributions of the stochastic null lambdas
png("lambda_stoch_null.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(1,1,1,1))
plot(density(lams_stoch_null[,1]), col = vcol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "a)                                                                                                       ",ylim = c(0,100), xlim = c(0.97,1.02))
abline(v = mean(lams_stoch_null[,1]),col = vcol, lty = 2, lwd =3)
legend("topright", legend = c("Vacant"), fill = c(vcol), cex = 1.5)
plot(density(lams_stoch_null[,2]), col = ccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "b)                                                                                                       ",ylim = c(0,100), xlim = c(0.97,1.02))
abline(v = mean(lams_stoch_null[,2]),col = ccol, lty = 2, lwd =3)
lines(density(lams_stoch_null[,3]), col = lcol, lwd =3)
abline(v = mean(lams_stoch_null[,3]),col = lcol, lty = 2, lwd =3)
lines(density(lams_stoch_null[,4]), col = ocol, lwd =3)
abline(v = mean(lams_stoch_null[,4]),col = ocol, lty = 2, lwd =3)
legend("topright", legend = c("Crematogaster","Liometopum", "Other"), fill = c(ccol,lcol,ocol), cex = 1.5)
plot(density(lams_stoch_null[,5]), col = lccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "c)                                                                                                       ",ylim = c(0,100), xlim = c(0.97,1.02))
abline(v = mean(lams_stoch_null[,5]),col = lccol, lty = 2, lwd =3)
lines(density(lams_stoch_null[,6]), col = locol, lwd =3)
abline(v = mean(lams_stoch_null[,6]),col = locol, lty = 2, lwd =3)
lines(density(lams_stoch_null[,7]), col = cocol, lwd =3)
abline(v = mean(lams_stoch_null[,7]),col = cocol, lty = 2, lwd =3)
legend("topright", legend = c("Crematogaster and Liometopum","Liometopum and Other", "Crematogaster and Other"), fill = c(lccol,locol,cocol), cex = 1.5)
plot(density(lams_stoch_null[,8]), col = acol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "d)                                                                                                       ",ylim = c(0,100), xlim = c(0.97,1.02))
abline(v = mean(lams_stoch_null[,8]),col = acol, lty = 2, lwd =3)
mtext("Lambda",side=1,line=-2,outer=TRUE,cex=1.3)
mtext("Density",side=2,line=-2,outer=TRUE,cex=1.3,las=0)
legend("topright",legend = c("All Ants"),fill = c(acol),
       cex = 1.5)
dev.off()

######################################################################################################
######################################################################################################
####                      COMPARE EACH OF THE POSTERIOR DISTRIBUTIONS                             ####
######################################################################################################
######################################################################################################

########################################### DETERMINISTIC ###########################################
# Compare deterministic posterior distributions to vacancy by subtracting from each other. 
# When the difference is 0 there is no difference
all_vac <- lams_dpost$all - lams_dpost$none
cl_vac <- lams_dpost$liomcremvac - lams_dpost$none
lo_vac <- lams_dpost$liomvacother - lams_dpost$none
co_vac <- lams_dpost$othercremvac - lams_dpost$none
c_vac <- lams_dpost$cremvac - lams_dpost$none
l_vac <- lams_dpost$liomvac - lams_dpost$none
o_vac <- lams_dpost$othervac - lams_dpost$none
vac_vac <- lams_dpost$none - lams_dpost$none
# Plot the differences between the lambda distributions and vacancy -- >0 means the partner 
# scenario lambda is greater than the vacant scenario lambda
png("lambda_det_difftovac.png")
par(mar=c(1,1,1,1))
layout(matrix(c(1,2,3,4,5,6,7),
              ncol = 1, nrow = 7), heights = c(1,1,1,1,1,1,1))
## All
plot(density(all_vac), col = acol, xlim = c(-.02,.05), lwd = 2, ylim = c(0,40))
abline(v = 0, col = acol, lty = 2)
## Crem and Liom
plot(density(cl_vac), col = lccol, xlim = c(-.02,.05), lwd = 2, ylim = c(0,40))
abline(v = 0, col = lccol, lty = 2)
## Other and Liom
plot(density(lo_vac), col = locol, xlim = c(-.02,.05), lwd = 2, ylim = c(0,40))
abline(v = 0, col = locol, lty = 2)
## Other and Crem
plot(density(co_vac), col = cocol, xlim = c(-.02,.05), lwd = 2, ylim = c(0,50))
abline(v = 0, col = cocol, lty = 2)
## Crem
plot(density(c_vac), col = ccol, xlim = c(-.02,.05), lwd = 2, ylim = c(0,80))
abline(v = 0, col = ccol, lty = 2)
## Liom
plot(density(l_vac), col = lcol, xlim = c(-.02,.05), lwd = 2, ylim = c(0,60))
abline(v = 0, col = lcol, lty = 2)
## Other
plot(density(o_vac), col = ocol, xlim = c(-.02,.05), lwd = 2, ylim = c(0,60))
abline(v = 0, col = ocol, lty = 2)
dev.off()
# calculate what proportion of each is > 0
# aka what proportion of lambda estimations are greater than when vacant
proportions <- vector()
proportions[1] <- 0
proportions[2] <- length(subset(c_vac, c_vac>0))/100
proportions[3] <- length(subset(l_vac, l_vac>0))/100
proportions[4] <- length(subset(o_vac, o_vac>0))/100
proportions[5] <- length(subset(cl_vac, cl_vac>0))/100
proportions[6] <- length(subset(lo_vac, lo_vac>0))/100
proportions[7] <- length(subset(co_vac, co_vac>0))/100
proportions[8] <- length(subset(all_vac, all_vac>0))/100
proportions
prop <- as.data.frame(matrix(rep(NA,8), ncol = 8))
colnames(prop) <- scenario
prop[1,] <- proportions
prop

## Compare deterministic posterior distributions to liom scenario
all_l <- lams_dpost$all - lams_dpost$liomvac
cl_l <- lams_dpost$liomcremvac - lams_dpost$liomvac
lo_l <- lams_dpost$liomvacother - lams_dpost$liomvac
co_l <- lams_dpost$othercremvac - lams_dpost$liomvac
c_l <- lams_dpost$cremvac - lams_dpost$liomvac
l_l <- lams_dpost$liomvac - lams_dpost$liomvac
o_l <- lams_dpost$othervac - lams_dpost$liomvac
vac_l <- lams_dpost$none - lams_dpost$liomvac
# calculate what proportion of each is > 0
# aka what proportion of lambda estimations are greater than when vacant
proportions <- vector()
proportions[1] <- length(subset(vac_l, vac_l>0))/100
proportions[2] <- length(subset(c_l, c_l>0))/100
proportions[3] <- length(subset(l_l, l_l>0))/100
proportions[4] <- length(subset(o_l, o_l>0))/100
proportions[5] <- length(subset(cl_l, cl_l>0))/100
proportions[6] <- length(subset(lo_l, lo_l>0))/100
proportions[7] <- length(subset(co_l, co_l>0))/100
proportions[8] <- length(subset(all_l, all_l>0))/100
proportions
prop <- as.data.frame(matrix(rep(NA,8), ncol = 8))
colnames(prop) <- scenario
prop[1,] <- proportions
prop

########################################### STOCHASTIC ##############################################
# Compare the stochastic posterior distributions to vacancy
# Calculate the difference in the between the posterior distributions of lambda
all_vac <- lams_stoch$all - lams_stoch$none
cl_vac <- lams_stoch$liomcremvac - lams_stoch$none
lo_vac <- lams_stoch$liomvacother - lams_stoch$none
co_vac <- lams_stoch$othercremvac - lams_stoch$none
c_vac <- lams_stoch$cremvac - lams_stoch$none
l_vac <- lams_stoch$liomvac - lams_stoch$none
o_vac <- lams_stoch$othervac - lams_stoch$none
vac_vac <- lams_stoch$none - lams_stoch$none
#plot them
png("lambda_stoch_difftovac.png")
par(mar=c(1,1,1,1))
layout(matrix(c(1,2,3,4,5,6,7),
              ncol = 1, nrow = 7), heights = c(1,1,1,1,1,1,1))
## All
plot(density(all_vac), col = acol, xlim = c(-0.025,0.04))
abline(v = 0, col = acol, lty = 2)
## Crem and Liom
plot(density(cl_vac), col = lccol, xlim = c(-0.025,0.04))
abline(v = 0, col = lccol, lty = 2)
## Other and Liom
plot(density(lo_vac), col = locol, xlim = c(-0.025,0.04))
abline(v = 0, col = locol, lty = 2)
## Other and Crem
plot(density(co_vac), col = cocol, xlim = c(-0.025,0.04))
abline(v = 0, col = cocol, lty = 2)
## Crem
plot(density(c_vac), col = ccol, xlim = c(-0.025,0.04))
abline(v = 0, col = ccol, lty = 2)
## Liom
plot(density(l_vac), col = lcol, xlim = c(-0.025,0.04))
abline(v = 0, col = lcol, lty = 2)
## Other
plot(density(o_vac), col = ocol, xlim = c(-0.025,0.04))
abline(v = 0, col = ocol, lty = 2)
dev.off()
#calculate what proportion of each is > 0
#aka what proportion of lambda estimations are greater than when vacant
proportions <- vector()
proportions[1] <- 0
proportions[2] <- length(subset(c_vac, c_vac>0))/100
proportions[3] <- length(subset(l_vac, l_vac>0))/100
proportions[4] <- length(subset(o_vac, o_vac>0))/100
proportions[5] <- length(subset(cl_vac, cl_vac>0))/100
proportions[6] <- length(subset(lo_vac, lo_vac>0))/100
proportions[7] <- length(subset(co_vac, co_vac>0))/100
proportions[8] <- length(subset(all_vac, all_vac>0))/100
proportions
prop <- as.data.frame(matrix(rep(NA,8), ncol = 8))
colnames(prop) <- scenario
prop[1,] <- proportions
prop

## Compare deterministic posterior distributions to liom scenario
all_l <- lams_stoch$all - lams_stoch$liomvac
cl_l <- lams_stoch$liomcremvac - lams_stoch$liomvac
lo_l <- lams_stoch$liomvacother - lams_stoch$liomvac
co_l <- lams_stoch$othercremvac - lams_stoch$liomvac
c_l <- lams_stoch$cremvac - lams_stoch$liomvac
l_l <- lams_stoch$liomvac - lams_stoch$liomvac
o_l <- lams_stoch$othervac - lams_stoch$liomvac
vac_l <- lams_stoch$none - lams_stoch$liomvac
# calculate what proportion of each is > 0
# aka what proportion of lambda estimations are greater than when vacant
proportions <- vector()
proportions[1] <- length(subset(vac_l, vac_l>0))/100
proportions[2] <- length(subset(c_l, c_l>0))/100
proportions[3] <- length(subset(l_l, l_l>0))/100
proportions[4] <- length(subset(o_l, o_l>0))/100
proportions[5] <- length(subset(cl_l, cl_l>0))/100
proportions[6] <- length(subset(lo_l, lo_l>0))/100
proportions[7] <- length(subset(co_l, co_l>0))/100
proportions[8] <- length(subset(all_l, all_l>0))/100
proportions
prop <- as.data.frame(matrix(rep(NA,8), ncol = 8))
colnames(prop) <- scenario
prop[1,] <- proportions
prop

########################################### STOCHASTIC AND DETERMINISTIC #############################
# Compare the deterministic difference distributions to the stochastic difference distributions
# to determine if portfolio effect is at play
all_vac_stoch_null <- lams_stoch_null$all - lams_stoch_null$none
all_vac_stoch <- lams_stoch$all - lams_stoch$none
# Plot the boost offered by the real ant scenario based on stochastic and deterministic lambda estimates
png("portfolio_effect.png")
plot(density(all_vac_stoch_null), lwd = 3, col = "chartreuse4", ylim = c(0,60))
lines(density(all_vac_stoch), lwd = 3, col = "violet")
abline(v = 0, lty = 2, lwd = 3)
legend("topleft",legend = c("Synchronicity Possible","Synchronicity Excluded"), fill = c("violet","chartreuse4"))
dev.off()
# check the mean density 
mean(all_vac_stoch_null>0)
mean(all_vac_stoch>0)
# there appears to be a stronger fitness effect when the ants can fluctuate independently -- not a very strong portfolio effect

# What proprotion of the difference in these is >0
# 62% confident that there is a fitness boost from partner diversity
length(subset((all_vac_stoch-all_vac_stoch_null), (all_vac_stoch-all_vac_stoch_null)>0))/100









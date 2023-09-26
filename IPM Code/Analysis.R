##########################################################################################################
##
##                            Call the IPM and understand the outputs
##
##########################################################################################################

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code")
source("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code/IPM.R")

vcol <- "#ad90ec"
lcol <- "#084f98"
ccol <- "#e9a67a"
ocol <- "#93022f"
lccol <- "#5dc9cf"
locol <- "#cf3545"
cocol <- "#ab59c8"
acol <- "#5d906b"
cols <- c(vcol, ccol, lcol, ocol, lccol, locol, cocol, acol)
#######################################################################################################
#####################DETERMINISTIC MEAN IPM############################################################
## Calculate the fitness for each of the different diversity scenarios                               ##
## This will allow me to compare the effects of diversity on the fitness of the tree cholla          ##
#######################################################################################################
#######################################################################################################
# Create an empty data frame to store the lambdas
lams <- as.data.frame(rep(NA,8))
######## calculate fitness of tree cholla with no ant partners ########
lams$means[1] <- lambda(bigmatrix(params,lower,upper,matsize,"none")$IPMmat)
######## caclulate fitness of tree cholla with one ant partner possible ########
lams$means[2] <- lambda(bigmatrix(params,lower,upper,matsize,scenario = "cremvac")$IPMmat)
lams$means[3] <- lambda(bigmatrix(params,lower,upper,matsize,scenario = "liomvac")$IPMmat)
lams$means[4] <- lambda(bigmatrix(params,lower,upper,matsize,scenario = "othervac")$IPMmat)
######## calculate fitness of tree cholla with two ant partners possible ########
i <- c("liom","other","vacant")
j <- c("liom","other", "vacant")
scenario = c("liomcremvac","liomvacother", "othercremvac" )
bmat <- vector()
for(n in 1:length(i)){
  bmat[n] <- lambda(bigmatrix(params,lower,upper,matsize,scenario[n])$IPMmat)
}
lams$means[5:7]<-bmat
######## calculate fitness of tree cholla with all ant partners possible ########
lams$means[8]<-lambda(bigmatrix(params,lower,upper,matsize,"all")$IPMmat)

######## Visualize the Deterministic Mean Lambdas
lams$scenario <- c("No Ants","Crem.","Liom.","Oth.",
                    "Liom. & Crem.", "Liom. & Oth.", "Oth. & Crem.", "All Ants")
lams$scenario_abv <- c("None","C","L","O","L,C","L,O","O,C","All")
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("lambda_det_mean.png")
plot(1:8,lams$means, col = cols, 
     pch = 20, cex = 6,xlim = c(0,9),ylim = c(0.985,1.01),cex.main = 2.3,
     xaxt = "n",cex.lab = 2,
     xlab = "Ant Scenario", ylab = "Mean Lambda Value", main = "Full Partner Diversity Leads to \n Highest Fitness")
 text(x = 1:8-0.2, y = lams$means+0.002,cex = 2,
      labels = lams$scenario_abv,
      srt = 35)
 legend("topleft",legend = c("L = Liom.","C = Crem.","O = Other"),
        cex = 1.5)
dev.off()

########################################################################################################
####################### DETERMINISTIC POST IPM  ########################################################
## Plot the lambda posterior distributions                                                            ##                                                                                  ##
########################################################################################################
########################################################################################################
lams_dpost <- matrix(rep(NA, 100*8), nrow = 100, ncol = 8)
scenario = c("none","cremvac","liomvac","othervac","liomcremvac","liomvacother","othercremvac","all")
for(z in 1:length(scenario)){
  print(z)
  for(m in 1:100){
    lams_dpost[m,z] <- lambda(bigmatrix(params[m,],lower,upper,matsize,scenario[z])$IPMmat)
    }
}
lams_dpost
colnames(lams_dpost) <- scenario
write.csv(lams_dpost,"det_post_lambda.csv")

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
## plot the distributions
png("lambda_post_bar.png")
boxplot(lams_dpost, xlim = c(-2,12),ylim = c(.8,1.1),
        main = "Fitness of Ants", ylab = "Lambda Values", xaxt = "n",
        at = c(1,4,5,6,8,9,10,12),
        col = c("Red","Blue","Green","Yellow","Orange","Brown","Black","Grey"))
legend("bottomleft",legend = scenario,fill = c("Red","Blue","Green","Yellow","Orange","Brown","Black","Grey"),
       cex = 1.5)
dev.off()

png("lambda_det_dist.png")
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





##############################################################################
###### STOCHASTIC POSTERIOR IPM ##############################################
##############################################################################
scenario = c("none","cremvac","liomvac","othervac","liomcremvac","liomvacother","othercremvac","all")
max_scenario = length(scenario)
max_rep = 100 ## Posterior Draws from vital rate models
#scenario = c("none","all")
max_yrs = 100 ## Years of randomly sampled annual effects
lams_stoch <- matrix(nrow = max_rep, ncol = max_scenario)
for(n in 1:max_scenario){
  print(scenario[n])
  for(m in 1:max_rep){
    lams_stoch[m,n] <- lambdaSim(params = params[m,],                                  ## parameters
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
                          max_yrs = max_yrs,                                 ## the # years you want to iterate
                          matsize=matsize,                                 ## size of transition matrix
                          scenario = scenario[n],                                ## partner diversity scenario
                          lower=lower,upper=upper  )
  }
}

write.csv(lams_stoch,"stoch_post_lambda.csv")

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
## Plot the distributions
png("lambda_st_full.png")
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




######################################################################################################
####                COMPARE EACH OF THE POSTERIOR DISTRIBUTIONS
######################################################################################################
## Read in lambda estimates
## deterministic
lams_dpost <- read.csv("det_post_lambda.csv")
lams_dpost <- lams_dpost[,-c(1)]
## stochastic
lams_stoch <- read.csv("stoch_post_lambda.csv")
lams_stoch <- lams_stoch[,-c(1)]

## Compare deterministic posterior distributions by subtracting from each other. When the difference 
## is 0 there is no difference
all_vac <- lams_dpost[,8] - lams_dpost[,1]
cl_vac <- lams_dpost[,5] - lams_dpost[,1]
lo_vac <- lams_dpost[,6] - lams_dpost[,1]
co_vac <- lams_dpost[,7] - lams_dpost[,1]
c_vac <- lams_dpost[,2] - lams_dpost[,1]
l_vac <- lams_dpost[,3] - lams_dpost[,1]
o_vac <- lams_dpost[,4] - lams_dpost[,1]
vac_vac <- lams_dpost[,1] - lams_dpost[,1]
## Plot them
plot("det_dist_diff.png")
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

#### Compare the stochastic posterior distributions
###### Calculate the difference in the between the posterior distributions of lambda
all_vac <- lams_stoch[,8] - lams_stoch[,1]
cl_vac <- lams_stoch[,5] - lams_stoch[,1]
lo_vac <- lams_stoch[,6] - lams_stoch[,1]
co_vac <- lams_stoch[,7] - lams_stoch[,1]
c_vac <- lams_stoch[,2] - lams_stoch[,1]
l_vac <- lams_stoch[,3] - lams_stoch[,1]
o_vac <- lams_stoch[,4] - lams_stoch[,1]
vac_vac <- lams_stoch[,1] - lams_stoch[,1]
#plot them
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


#### Compare the deterministic difference distributions to the stochastic difference distributions
#### to determine if portfolio effect is at play
all_vac_det <- lams_dpost[,8] - lams_dpost[,1]
all_vac_stoch <- lams_stoch[,8] - lams_stoch[,1]
## Plot them
png("portfolio_effect.png")
plot(density(all_vac_det), lwd = 3, col = "chartreuse4")
lines(density(all_vac_stoch), lwd = 3, col = "violet")
abline(v = 0, lty = 2, lwd = 3)
dev.off()
## What proprotion of the difference in these is > 0
length(subset((all_vac_stoch-all_vac_det), (all_vac_stoch- all_vac_det)>0))/100



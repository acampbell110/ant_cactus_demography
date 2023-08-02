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
##########################################################################################################
####################### DETERMINISTIC MEAN IPM    ########################################################
## Check that every function written in the IPM source code runs properly and that all of the outputs   ##
## make sense.                                                                                          ##
##########################################################################################################
################################
#################################
## Choose the proper bigmatrix based on ant scenario

## Check that the outputs are right
## One ant option
lambda(bigmatrix(params,lower,upper,matsize,"none")$IPMmat)
lambda(bigmatrix.1(params,lower,upper,matsize)$IPMmat)
## 2 ant options
lambda(bigmatrix(params,lower,upper,matsize,"cremvac")$IPMmat)
lambda(bigmatrix.2(params,lower,upper,matsize,"cremvac")$IPMmat)
## 3 ant options
lambda(bigmatrix(params,lower,upper,matsize,"liomcremvac")$IPMmat)
lambda(bigmatrix.3(params,lower,upper,matsize,"liomcremvac")$IPMmat)
## all ant options
lambda(bigmatrix(params,lower,upper,matsize,"all")$IPMmat)
lambda(bigmatrix.4(params,lower,upper,matsize,"all")$IPMmat)
## These all match!



#######################################################################################################
## Calculate the fitness for each of the different diversity scenarios                               ##
## This will allow me to compare the effects of diversity on the fitness of the tree cholla          ##
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
     pch = 20, cex = 6,xlim = c(0,9),ylim = c(0.950,1.1),cex.main = 2.3,
     xaxt = "n",cex.lab = 2,
     xlab = "Ant Scenario", ylab = "Mean Lambda Value", main = "Full Partner Diversity Leads to \n Highest Fitness")
 text(x = 1:8-0.2, y = lams$means+0.004,cex = 2,
      labels = lams$scenario_abv,
      srt = 35)
 legend("topleft",legend = c("L = Liom.","C = Crem.","O = Other"),
        cex = 1.5)
dev.off()

##########################################################################################################
####################### DETERMINISTIC POST IPM    ########################################################
## Plot the lambda posterior distributions                                                              ##                                                                                  ##
##########################################################################################################
lams <- matrix(rep(NA, 100*8), nrow = 100, ncol = 8)
scenario = c("none","cremvac","liomvac","othervac","liomcremvac","liomvacother","othercremvac","all")
for(z in 1:length(scenario)){
  print(z)
  for(m in 1:100){
    lams[m,z] <- lambda(bigmatrix(params[m,],lower,upper,matsize,scenario[z])$IPMmat)
    }
}
lams
colnames(lams) <- scenario
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")

png("lambda_post_bar.png")
boxplot(lams, xlim = c(-2,12),ylim = c(.8,1.1),
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
plot(density(lams[,1]), col = vcol, xlab = "",ylab = "", lwd = 3,cex.main = 2,  main = "a)                                                                                                       ",
     ylim = c(0,10), xlim = c(.9,1.2))
abline(v = mean(lams[,1]),col = vcol, lty = 2, lwd =3)
legend("topright", legend = c("Vacant"), fill = c(vcol), cex = 1.5)
plot(density(lams[,2]), col = ccol, xlab = "",ylab = "", lwd = 3,cex.main = 2, main = "b)                                                                                                       ",
     ylim = c(0,10), xlim = c(.9,1.2))
abline(v = mean(lams[,2]),col = ccol, lty = 2, lwd =3)
lines(density(lams[,3]), col = lcol, lwd = 3)
abline(v = mean(lams[,3]),col = lcol, lty = 2, lwd =3)
lines(density(lams[,4]), col = ocol, lwd = 3)
abline(v = mean(lams[,4]),col = ocol, lty = 2, lwd =3)
legend("topright", legend = c("Crem.","Liom.", "Other"), fill = c(ccol,lcol,ocol), cex = 1.5)
plot(density(lams[,5]), col = lccol, xlab = "",ylab = "", lwd = 3,cex.main = 2, main = "c)                                                                                                       ",
     ylim = c(0,10), xlim = c(.9,1.2))
abline(v = mean(lams[,5]),col = lccol, lty = 4, lwd =3)
lines(density(lams[,6]), col = locol, lwd = 3)
abline(v = mean(lams[,6]),col = locol, lty = 2, lwd =3)
lines(density(lams[,7]), col = cocol, lwd = 3)
abline(v = mean(lams[,7]),col = cocol, lty = 2, lwd =3)
legend("topright", legend = c("Crem. and Liom.","Liom. and Other", "Crem. and Other"), fill = c(lccol,locol,cocol), cex = 1.5)
plot(density(lams[,8]), col = acol, xlab = "",ylab = "", lwd = 3,cex.main = 2, main = "d)                                                                                                       ",
     ylim = c(0,10), xlim = c(.9,1.2))
abline(v = mean(lams[,8]),col = acol, lty = 2, lwd =3)
mtext("Lambda",side=1,line=-2,outer=TRUE,cex=1.3)
mtext("Density",side=2,line=-2,outer=TRUE,cex=1.3,las=0)
legend("topright",legend = c("All Ants"),fill = c(acol),
       cex = 1.5)
dev.off()





[##############################################################################
###### STOCHASTIC POSTERIOR IPM ##############################################
##############################################################################
scenario = c("none","cremvac","liomvac","othervac","liomcremvac","liomvacother","othercremvac","all")
max_scenario = length(scenario)
max_rep = 100 ## Posterior Draws from vital rate models
#scenario = c("none","all")
max_yrs = 1000 ## Years of randomly sampled annual effects
lam <- matrix(nrow = max_rep, ncol = max_scenario)
for(n in 1:max_scenario){
  print(n)
  for(m in 1:100){
    lam[m,n] <- lambdaSim(params = params[m,],                                  ## parameters
                          grow_rfx1=grow_rfx1,
                          grow_rfx2=grow_rfx2,
                          grow_rfx3=grow_rfx3,
                          grow_rfx4=grow_rfx4, ## growth model year rfx
                          surv_rfx1=surv_rfx1,
                          surv_rfx2=surv_rfx2,
                          surv_rfx3=surv_rfx3,
                          surv_rfx4=surv_rfx4, ## survival model year rfx
                          flow_rfx=flow_rfx,                                ## flower model year rfx
                          repro_rfx=repro_rfx,                               ## repro model year rfx
                          viab_rfx1=viab_rfx1,
                          viab_rfx2=viab_rfx2,
                          viab_rfx3=viab_rfx3,
                          viab_rfx4=viab_rfx4, ## viability model year rfx
                          max_yrs = 1000,                                 ## the # years you want to iterate
                          matsize=matsize,                                 ## size of transition matrix
                          scenario = scenario[n],                                ## partner diversity scenario
                          lower=lower,upper=upper  )
  }
}


setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")

png("lambda_st_full.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(1,1,1,1))
plot(density(lam[,1]), col = vcol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "a)                                                                                                       ",ylim = c(0,25), xlim = c(0.9,1.6))
abline(v = mean(lams[,1]),col = vcol, lty = 2, lwd =3)
legend("topright", legend = c("Vacant"), fill = c(vcol), cex = 1.5)
plot(density(lam[,2]), col = ccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "b)                                                                                                       ",ylim = c(0,25), xlim = c(0.9,1.6))
abline(v = mean(lams[,2]),col = ccol, lty = 2, lwd =3)
lines(density(lam[,3]), col = lcol, lwd =3)
abline(v = mean(lams[,3]),col = lcol, lty = 2, lwd =3)
lines(density(lam[,4]), col = ocol, lwd =3)
abline(v = mean(lams[,4]),col = ocol, lty = 2, lwd =3)
legend("topright", legend = c("Crematogaster","Liometopum", "Other"), fill = c(ccol,lcol,ocol), cex = 1.5)
plot(density(lam[,5]), col = lccol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "c)                                                                                                       ",ylim = c(0,25), xlim = c(0.9,1.6))
abline(v = mean(lams[,5]),col = lccol, lty = 2, lwd =3)
lines(density(lam[,6]), col = locol, lwd =3)
abline(v = mean(lams[,6]),col = locol, lty = 2, lwd =3)
lines(density(lam[,7]), col = cocol, lwd =3)
abline(v = mean(lams[,7]),col = cocol, lty = 2, lwd =3)
legend("topright", legend = c("Crematogaster and Liometopum","Liometopum and Other", "Crematogaster and Other"), fill = c(lccol,locol,cocol), cex = 1.5)
plot(density(lam[,8]), col = acol, xlab = "",ylab = "", lwd =3,cex.main = 2, main = "d)                                                                                                       ")
abline(v = mean(lams[,8]),col = acol, lty = 2, lwd =3)
mtext("Lambda",side=1,line=-2,outer=TRUE,cex=1.3)
mtext("Density",side=2,line=-2,outer=TRUE,cex=1.3,las=0)
legend("topright",legend = c("All Ants"),fill = c(acol),
       cex = 1.5)
dev.off()




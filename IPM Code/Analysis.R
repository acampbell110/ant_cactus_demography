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
##########################################################################################################
####################### DETERMINISTIC MEAN IPM    ########################################################
## Check that every function written in the IPM source code runs properly and that all of the outputs   ##
## make sense.                                                                                          ##
##########################################################################################################
################################
## Survival and Growth Kernel
##Check if it runs properly properly with fake data 
pxy(x=c(1,2,3),y=c(1,2,3),i="liom",params)
i = c("liom","vacant","crem","other")
x = c(14,14,14,14)
y = c(3,3,3,3)
p<- vector()
g <- vector()
s <- vector()
for(n in 1:length(i)){
  xb = pmin(pmax(x[n],cholla_min),cholla_max)
  s[n] <- sx(x[n], i[n], params)
  g[n] <- gxy(x[n], y[n], i[n], params)
  p[n] <- pxy(x[n],y[n],i[n], params)
}
g
s
p
## Check that the outputs make sense
# The sum of all future sizes from all previous sizes should sum to 1
x = rep(1,25)
y = seq(cholla_min,cholla_max,length = 25)
outer(x,y,
      dnorm(y,x,sd = 0))
sum(gxy(x,y,"vacant",params))
# This is not summing to 1. I would have assumed it would sum to less than 1 and get closer and closer as 
# the number increased. If I set it at 20, it is less than 1, but more values does not do that
integrate(gxy(x,y,"liom",params),lower=cholla_min,upper=cholla_max)
integrate(dnorm(y,x,sd = 0),lower = cholla_min, upper = cholla_max)
sum(pxy(x,y,"liom",params))
# This is also not summing to 1, but I am not actually sure that it should?
G=h*outer(y,y,gxy,i = "crem",params=params)  # growth kernel
image(y,y,t(G),main='Crem Tended')        # plot it
G=h*outer(y,y,gxy,i = "liom",params=params)  # growth kernel
image(y,y,t(G),main='Liom Tended')        # plot it
G=h*outer(y,y,gxy,i = "other",params=params)  # growth kernel
image(y,y,t(G),main='Other Tended')        # plot it
G=h*outer(y,y,gxy,i = "vacant",params=params)  # growth kernel
image(y,y,t(G),main='Not Tended')        # plot it
################################
## Fecundity Function
## Check if it runs properly with fake data
i = c("liom","vacant","crem","other")
x = c(13,13,13,13)
y = c(14,14,14,14)
f <- matrix(NA,ncol = length(i), nrow = (Ndraws))
f<-vector()
for(n in 1:length(i)){
  f[n] <- fx(x[n],i[n],params)
}
f
## Check that the outputs make sense
#################################
## Recruit Size Distribution Function
## Check if it runs properly with fake data
i = c("liom","vacant","crem","other")
x = c(13,13,13,13)
y = c(14,14,14,14)
r <- matrix(NA,ncol = length(i), nrow = (Ndraws))
r<-vector()
for(n in 1:length(i)){
  r[n] <- recruits(y[n],params)
}
r
## Check that the outputs make sense
## We expect to see a truncated normal distribution here 
plot(recruits(y,params))

################################
## Transition between occupied and vacancy
## Scenario options == "othervac", "liomvac", "cremvac"
x = seq(cholla_min,cholla_max,length=20)
y = seq(cholla_min,cholla_max,length=20)
transition.1(x,"vacant","liom",params,scenario = "liomvac")
transition.1(x,"vacant","other",params,scenario = "othervac")
transition.1(x,"vacant","crem",params,scenario = "cremvac")
## Check if it works
i = c("liom","vacant","other")
j = c("vacant","vacant","vacant")
x = c(-1,2,3)
scenario = c("liomvac","othervac","cremvac")
t1 <- vector()
for(n in 1:length(i)){
  t1[n] <- transition.1(x[n],i[n],j[n],params,scenario[n])
}
t1
## Check that the outputs make sense
x = c(1,1)
i = c("crem","crem")
j = c("crem","vacant")
scenario = "cremvac"
t1 <- vector()
for(n in 1:length(i)){
  t1[n] <- transition.1(x[n],i[n],j[n],params,scenario)
}
t1
sum(t1)
## This code works properly
#################################
## Transition between vacancy and two ant species
## Scenario options are "liomvacother", "liomcremvac", "othercremvac"
transition.2(x,"liom","other",params,"liomvacother")
transition.2(x,"liom","vacant",params,"liomcremvac")
transition.2(x,"crem","other",params,"othercremvac")
## Check if it works
i = c("liom","vacant","other","other")
j = c("vacant","liom","other","liom")
x = c(15,15,15,15)
y = c(-1,-4,4.5,3.01)
scenario = "liomvacother"
t2 <- vector()
for(n in 1:length(i)){
  t2[n] <- transition.2(x[n],i[n],j[n],params,scenario)
}
t2
## Check that the outputs make sense
# All transition rates from crem should sum to 1
x = c(1,1,1)
i = c("crem","crem","crem")
j = c("crem","liom","vacant")
scenario = "liomcremvac"
t2 <- vector()
for(n in 1:length(i)){
  t2[n] <- transition.2(x[n],i[n],j[n],params,scenario)
}
t2
sum(t2)
#################################
## Transition between vacancy and all ants
transition.3(x,"crem","liom",params)
## Chekc if it works
i = c("liom","liom")
j = c("vacant","vacant")
x = c(-1,-5)
y = c(-1,-5)
t3 <- vector()
for(n in 1:length(i)){
  t3[n] <- transition.3(x[n],i[n],j[n],params)
}
t3
## Check that the outputs make sense
# All transition rates from crem should sum to 1
x = c(1,1,1,1)
i = c("other","other","other","other")
j = c("crem","liom","vacant","other")
scenario = "all"
t3 <- vector()
for(n in 1:length(i)){
  t3[n] <- transition.3(x[n],i[n],j[n],params)
}
t3
sum(t3)
## All good
#################################
## Choose between all of the transition 
## Scenario options are "liomvacother", "liomcremvac", "othercremvac", 
## "othervac", "liomvac", "cremvac", "all", "none"
transition.x(x,"liom","vacant",params,"liomvac")
## Check if it works
i = c("liom","vacant","liom","vacant")
j = c("vacant","liom","liom","vacant")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
scenario = c("all","liomvac","liomvacother","none")
t <- vector()
for(n in 1:length(i)){
  t[n] <- transition.x(x[n],i[n],j[n],params,scenario[n])
}
t
#################################
## Calculate Matrix for no ant species -- vacant
## Check that it works
i = c("vacant","vacant")
big1 <- list()
for(n in 1:length(i)){
  big1[[n]] <- lambda(bigmatrix.1(params,lower=cholla_min-0,upper=cholla_max+0,matsize,"vacant")$IPMmat)
}
big1
## Check that the outputs make sense
# This diagnostic shows that the columns should sum to the survival function of the vacant
testmat <- bigmatrix.1(params,lower=cholla_min-15,upper=cholla_max+2,matsize,"vacant")$Tmat
surv <- colSums(testmat)
plot(surv,ylim=c(0,1))
# A second test is to change all survival rates in the bigmatrix.1 function to 100% and 
# then run this and everything should be near 1
# This looks good
#################################
## Calculate Matrix for one ant and vacant
## Check that it works
## Scenario options are "liomvac","cremvac","othervac"
scenario <- c("liomvac","othervac","othervac","cremvac")
bmat <- vector()
for(n in 1:length(scenario)){
  bmat[n] <- lambda(bigmatrix.2(params,lower,upper,matsize,scenario[n])$IPMmat)
}
bmat
## Check that the outputs make sense
# This diagnostic shows that the columsn should sum to the survival function of the vacant 
testmat <- bigmatrix.2(params,lower=cholla_min-20,upper=cholla_max+20,matsize,"liomvac")$Tmat
surv <- colSums(testmat)
plot(surv,ylim = c(0,1))
#################################
## Calculate Matrix for two ants and vacant
## Check that it works
bigmatrix.3(params,lower=cholla_min-0,upper=cholla_max+0,matsize,"othercremvac")
## Scenario options are "liomvacother", "liomcremvac", "othercremvac"
scenario <- c("liomvacother","liomcremvac","othercremvac")
bmat <- vector()
for(n in 1:length(i)){
  bmat[n] <- lambda(bigmatrix.3(params,lower,upper,matsize,scenario[n])$IPMmat)
}
bmat
## Check that the outputs make sense
# This diagnostic shows that the columns should sum to the survival function
testmat <- bigmatrix.3(params,lower=cholla_min-15,upper=cholla_max+2,matsize,"liomvacother")$Tmat
surv <- colSums(testmat)
plot(surv,ylim = c(0,1))
## This looks good!
#################################
## Calculate Matrix for three ants and vacant (real life scenario)
lambda(bigmatrix.4(params,lower,upper,matsize,"all")$IPMmat)
## Check that the outputs make sense
# This diagnostic shows that the columns should sum to the survival function
testmat <- bigmatrix.4(params,lower=cholla_min-15,upper=cholla_max+2,matsize,scenario="all")$Tmat
surv <- colSums(testmat)
plot(surv,ylim = c(0,1))
## This looks good!
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
lams$means[1] <- lambda(bigmatrix(params,lower=cholla_min-15,upper=cholla_max+2,matsize,"none")$IPMmat)
######## caclulate fitness of tree cholla with one ant partner possible ########
lams$means[2] <- lambda(bigmatrix(params, cholla_min-15,cholla_max+2,matsize,scenario = "cremvac")$IPMmat)
lams$means[3] <- lambda(bigmatrix(params, cholla_min-15,cholla_max+2,matsize,scenario = "liomvac")$IPMmat)
lams$means[4] <- lambda(bigmatrix(params, cholla_min-15,cholla_max+2,matsize,scenario = "othervac")$IPMmat)
######## calculate fitness of tree cholla with two ant partners possible ########
i <- c("liom","other","vacant")
j <- c("liom","other", "vacant")
scenario = c("liomcremvac","liomvacother", "othercremvac" )
bmat <- vector()
for(n in 1:length(i)){
  bmat[n] <- lambda(bigmatrix(params,lower=cholla_min-15,upper=cholla_max+2,matsize,scenario[n])$IPMmat)
}
lams$means[5:7]<-bmat
######## calculate fitness of tree cholla with all ant partners possible ########
lams$means[8]<-lambda(bigmatrix(params,lower=cholla_min-15,upper=cholla_max+2,matsize,"all")$IPMmat)

######## Visualize the Deterministic Mean Lambdas
lams$scenario <- c("No Ants","Crem.","Liom.","Oth.",
                    "Liom. & Crem.", "Liom. & Oth.", "Oth. & Crem.", "All Ants")
lams$scenario_abv <- c("None","C","L","O","L,C","L,O","O,C","All")
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("lambda_means3.png")
plot(1:8,lams$means, col = c("Red","Blue","Green","Yellow","Orange","Brown","Black","Grey"), 
     pch = 20, cex = 6,xlim = c(0,9),ylim = c(0.950,1.015),cex.main = 2.3,
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
bigmatrix(params,lower,upper,matsize,scenario)
scenario = c("none","cremvac","liomvac","othervac","liomcremvac","liomvacother","othercremvac","all")
for(z in 1:length(scenario)){
  for(m in 1:100){
    lams[m,z] <- lambda(bigmatrix(params[m,],lower,upper,matsize,scenario[z],grow_rfx1[m,a],grow_rfx2[m,a],grow_rfx3[m,a],grow_rfx4[m,a],surv_rfx1[m,a],surv_rfx2[m,a],surv_rfx3[m,a],surv_rfx4[m,a],flow_rfx[m,a],repro_rfx[m,a],viab_rfx1[m,a],viab_rfx2[m,a],viab_rfx3[m,a],viab_rfx4[m,a])$IPMmat)
  }
}
lams
colnames(lams) <- scenario
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("lambda_post_skew1.png")
par(mar=c(2,2,2,1),oma=c(2,2,0,0))
layout(matrix(c(1,2,3,4),
              ncol = 3, byrow = TRUE), heights = c(1), widths = c(3.9,3.9,3.9,3.9))
plot(density(lams[,1]), col = "Red",lwd = 2, 
     cex.main = 2.3, xlim = c(0.91,1.05), ylim = c(0,100), cex.lab = 2,
     xlab = "Lambda Values", ylab = "Density Probability", main = "Fitness of Ants")
plot(density(lams[,2]), col = "Blue", lwd = 2)
lines(density(lams[,3]), col = "Green", lwd = 2)
lines(density(lams[,4]), col = "Yellow", lwd = 2)
plot(density(lams[,5]), col = "Orange", lwd = 2)
lines(density(lams[,6]), col = "Brown", lwd = 2)
lines(density(lams[,7]), col = "Black", lwd = 2)
plot(density(lams[,8]), col = "Grey", lwd = 2)
legend("topleft",legend = scenario,fill = c("Red","Blue","Green","Yellow","Orange","Brown","Black","Grey"),
       cex = 1.5)
dev.off()

png("lambda_post_skew2.png")
boxplot(lams, xlim = c(-2,12),ylim = c(0, 1.1),
        main = "Fitness of Ants", ylab = "Lambda Values", xaxt = "n",
        at = c(1,4,5,6,8,9,10,12),
        col = c("Red","Blue","Green","Yellow","Orange","Brown","Black","Grey"))
legend("topleft",legend = scenario,fill = c("Red","Blue","Green","Yellow","Orange","Brown","Black","Grey"),
       cex = 1.5)
dev.off()


png("lambda_det_full_skew.png")
par(mar=c(4,4,1.01,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(1,1,1,1))
plot(density(lams[,1]), col = vcol, xlab = "",ylab = "", lwd = 3,cex.main = 2,  main = "a)                                                                                                       ",
     ylim = c(0,60), xlim = c(0.9,1.06))
abline(v = mean(lams[,1]),col = vcol, lty = 2, lwd =3)
legend("topright", legend = c("Vacant"), fill = c(vcol), cex = 1.5)
plot(density(lams[,2]), col = ccol, xlab = "",ylab = "", lwd = 3,cex.main = 2, main = "b)                                                                                                       ",
     ylim = c(0,60), xlim = c(0.9,1.06))
abline(v = mean(lams[,2]),col = ccol, lty = 2, lwd =3)
lines(density(lams[,3]), col = lcol, lwd = 3)
abline(v = mean(lams[,3]),col = lcol, lty = 2, lwd =3)
lines(density(lams[,4]), col = ocol, lwd = 3)
abline(v = mean(lams[,4]),col = ocol, lty = 2, lwd =3)
legend("topright", legend = c("Crem.","Liom.", "Other"), fill = c(ccol,lcol,ocol), cex = 1.5)
plot(density(lams[,5]), col = lccol, xlab = "",ylab = "", lwd = 3,cex.main = 2, main = "c)                                                                                                       ",
     ylim = c(0,60), xlim = c(0.9,1.06))
abline(v = mean(lams[,5]),col = lccol, lty = 4, lwd =3)
lines(density(lams[,6]), col = locol, lwd = 3)
abline(v = mean(lams[,6]),col = locol, lty = 2, lwd =3)
lines(density(lams[,7]), col = cocol, lwd = 3)
abline(v = mean(lams[,7]),col = cocol, lty = 2, lwd =3)
legend("topright", legend = c("Crem. and Liom.","Liom. and Other", "Crem. and Other"), fill = c(lccol,locol,cocol), cex = 1.5)
plot(density(lams[,8]), col = acol, xlab = "",ylab = "", lwd = 3,cex.main = 2, main = "d)                                                                                                       ",
     ylim = c(0,60), xlim = c(0.9,1.06))
abline(v = mean(lams[,8]),col = acol, lty = 2, lwd =3)
mtext("Lambda",side=1,line=-2,outer=TRUE,cex=1.3)
mtext("Density",side=2,line=-2,outer=TRUE,cex=1.3,las=0)
legend("topright",legend = c("All Ants"),fill = c(acol),
       cex = 1.5)
dev.off()

############################################################################################################
################################### STOCHASTIC IPM MEAN ####################################################
############################################################################################################
#### Calculate the lambda value for each scenario in each year with random effects
lams <- vector()
scenario = c("none","cremvac","liomvac","othervac","liomcremvac","liomvacother","othercremvac","all")
for(z in 1:length(scenario)){
    lams[z] <- lambdaSim(params = params,
                           grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4,
                           surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,
                           flow_rfx,
                           repro_rfx,
                           viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4,
                           max_yrs = 100,
                           matsize = matsize,
                           scenario = scenario[z],
                           lower = lower, upper = upper
    )
}
lams

lambdaSim(params = params,
          matrix(rep(0,1700),ncol = 17),matrix(rep(0,1700),ncol = 17),matrix(rep(0,1700),ncol = 17),matrix(rep(0,1700),ncol = 17),
          matrix(rep(0,1700),ncol = 17),matrix(rep(0,1700),ncol = 17),matrix(rep(0,1700),ncol = 17),matrix(rep(0,1700),ncol = 17),
          matrix(rep(0,1700),ncol = 17),
          matrix(rep(0,1700),ncol = 17),
          matrix(rep(0,1700),ncol = 17),matrix(rep(0,1700),ncol = 17),matrix(rep(0,1700),ncol = 17),matrix(rep(0,1700),ncol = 17),
          max_yrs = 100,
          matsize = matsize,
          scenario = scenario[z],
          lower = lower, upper = upper
)

yrs <- c("2004","2005","2006","2013","2014","2015","2016","2017","2018","2019")

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")

png("lambda_stoch_means.png")
plot(1:8,lams,col = c("Red","Blue","Green","Yellow","Orange","Brown","Black","Grey"),pch = 20,cex = 3,
     ylim = c(0.93,1),xlim = c(0,16),
     xlab = "Partner Diversity Scenario", ylab = "Stochastic Lambda",
     main = "Full Partner Diversity Leads to \n Increased Fitness",
     #xaxt = "n",at = c(1,4,5,6,8,8,10,12)
     )
legend("bottomright",legend = scenario,fill = c("Red","Blue","Green","Yellow","Orange","Brown","Black","Grey"),
       cex = 1.5)
dev.off()


####################################################################################
#### GRAPH THE CHANGES IN LAMBDA ACROSS YEARS
png("lambda_st_years.png")
plot(yrs,lams[,1], col = "Red",pch = 20, cex = 2, xlim = c(2003,2020),ylim = c(0.86,1.07),type = "b")
lines(yrs,lams[,2], col = "Blue",pch = 20, cex = 2, type = "b")
lines(yrs,lams[,3], col = "Green",pch = 20, cex = 2, type = "b")
lines(yrs,lams[,4], col = "Yellow",pch = 20, cex = 2, type = "b")
lines(yrs,lams[,5], col = "Orange",pch = 20, cex = 2, type = "b")
lines(yrs,lams[,6], col = "Brown",pch = 20, cex = 2, type = "b")
lines(yrs,lams[,7], col = "Black",pch = 20, cex = 2, type = "b")
lines(yrs,lams[,8], col = "Grey",pch = 20, cex = 2, type = "b")
legend("bottomleft",legend = scenario,fill = c("Red","Blue","Green","Yellow","Orange","Brown","Black","Grey"),
       cex = 1.5)
dev.off()


setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
#### GRAPH THE DISTRIBUTION OF LAMBDAS
lams <- lams[,-9]
png("lambda_st.png")
boxplot(lams, xlim = c(-8,12),ylim = c(0.86,1.1),
        main = "Fitness of Ants", ylab = "Lambda Values", xaxt = "n",
        at = c(1,4,5,6,8,9,10,12),
        col = c("Red","Blue","Green","Yellow","Orange","Brown","Black","Grey"))
legend("topleft",legend = scenario,fill = c("Red","Blue","Green","Yellow","Orange","Brown","Black","Grey"),
       cex = 1.5)
dev.off()





###################################################################################
######## STOCHASTIC PARAMS POSTERIOR
## Get stochastic distribution of lambda
lams <- matrix(rep(NA,80),nrow = 10)
scenario = c("none","cremvac","liomvac","othervac","liomcremvac","liomvacother","othercremvac","all")
for(n in 1:length(scenario)){
  for(m in 1:10){
    lams[m,n] <- lambdaSim(params = params,
                           grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4,
                           surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,
                           flow_rfx,
                           repro_rfx,
                           viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4,
                           max_yrs = 100,
                           matsize = matsize,
                           scenario = scenario[n],
                           lower = lower, upper = upper
    )
  }
}
lams

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")

png("lambda_st_full.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(1,1,1,1))
plot(density(lams[,1]), col = "Red", xlab = "",ylab = "",cex.main = 2, main = "a)                                                                                                       ",ylim = c(0,25), xlim = c(0.9,1.06))
legend("topright", legend = c("Vacant"), fill = c("Red"), cex = 1.5)
plot(density(lams[,2]), col = "Blue", xlab = "",ylab = "",cex.main = 2, main = "b)                                                                                                       ",ylim = c(0,25), xlim = c(0.9,1.06))
lines(density(lams[,3]), col = "Green")
lines(density(lams[,4]), col = "Yellow")
legend("topright", legend = c("Crematogaster","Liometopum", "Other"), fill = c("Blue","Green","Yellow"), cex = 1.5)
plot(density(lams[,5]), col = "Orange", xlab = "",ylab = "",cex.main = 2, main = "c)                                                                                                       ",ylim = c(0,25), xlim = c(0.9,1.06))
lines(density(lams[,6]), col = "Brown")
lines(density(lams[,7]), col = "Black")
legend("topright", legend = c("Crematogaster and Liometopum","Liometopum and Other", "Crematogaster and Other"), fill = c("Orange","Brown","Black"), cex = 1.5)
plot(density(lams[,8]), col = "Grey", xlab = "",ylab = "",cex.main = 2, main = "d)                                                                                                       ",ylim = c(0,25), xlim = c(0.9,1.06))
mtext("Lambda",side=1,line=-2,outer=TRUE,cex=1.3)
mtext("Density",side=2,line=-2,outer=TRUE,cex=1.3,las=0)
legend("topright",legend = c("All Ants"),fill = c("Grey"),
       cex = 1.5)
dev.off()

png("lambda_st_full2.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(1,1,1,1))
plot(density(lams[1,]), col = "Red", xlab = "",ylab = "",cex.main = 2, main = "a)                                                                                                       ",ylim = c(0,25), xlim = c(0.9,1.06))
legend("topright", legend = c("Vacant"), fill = c("Red"), cex = 1.5)
plot(density(lams[2,]), col = "Blue", xlab = "",ylab = "",cex.main = 2, main = "b)                                                                                                       ",ylim = c(0,25), xlim = c(0.9,1.06))
lines(density(lams[3,]), col = "Green")
lines(density(lams[4,]), col = "Yellow")
legend("topright", legend = c("C. opuntiae","L. apiculatum", "Other"), fill = c("Blue","Green","Yellow"), cex = 1.5)
plot(density(lams[5,]), col = "Orange", xlab = "",ylab = "",cex.main = 2, main = "c)                                                                                                       ",ylim = c(0,25), xlim = c(0.9,1.06))
lines(density(lams[6,]), col = "Brown")
lines(density(lams[7,]), col = "Black")
legend("topright", legend = c("C. opuntiae and L. apiculatum","L. apiculatum and Other", "C. opuntiae and Other"), fill = c("Orange","Brown","Black"), cex = 1.5)
plot(density(lams[8,]), col = "Grey", xlab = "",ylab = "",cex.main = 2, main = "d)                                                                                                       ",ylim = c(0,25), xlim = c(0.9,1.06))
mtext("Lambda",side=1,line=-2,outer=TRUE,cex=1.3)
mtext("Density",side=2,line=-2,outer=TRUE,cex=1.3,las=0)
legend("topright",legend = c("All Ants"),fill = c("Grey"),
       cex = 1.5)
dev.off()

png("lambda_st_full3.png")
par(mar=c(4,4,1.01,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(1,1,1,1))
plot(density(lams[,1]), col = vcol, xlab = "",ylab = "", lwd = 3,cex.main = 2,  main = "a)                                                                                                       ",
     ylim = c(0,80), xlim = c(0.9,1.06))
abline(v = mean(lams[,1]),col = vcol, lty = 2, lwd =3)
legend("topright", legend = c("Vacant"), fill = c(vcol), cex = 1.5)
plot(density(lams[,2]), col = ccol, xlab = "",ylab = "", lwd = 3,cex.main = 2, main = "b)                                                                                                       ",
     ylim = c(0,80), xlim = c(0.9,1.06))
abline(v = mean(lams[,2]),col = ccol, lty = 2, lwd =3)
lines(density(lams[,3]), col = lcol, lwd = 3)
abline(v = mean(lams[,3]),col = lcol, lty = 2, lwd =3)
lines(density(lams[,4]), col = ocol, lwd = 3)
abline(v = mean(lams[,4]),col = ocol, lty = 2, lwd =3)
legend("topright", legend = c("Crem.","Liom.", "Other"), fill = c(ccol,lcol,ocol), cex = 1.5)
plot(density(lams[,5]), col = lccol, xlab = "",ylab = "", lwd = 3,cex.main = 2, main = "c)                                                                                                       ",
     ylim = c(0,80), xlim = c(0.9,1.06))
abline(v = mean(lams[,5]),col = lccol, lty = 4, lwd =3)
lines(density(lams[,6]), col = locol, lwd = 3)
abline(v = mean(lams[,6]),col = locol, lty = 2, lwd =3)
lines(density(lams[,7]), col = cocol, lwd = 3)
abline(v = mean(lams[,7]),col = cocol, lty = 2, lwd =3)
legend("topright", legend = c("Crem. and Liom.","Liom. and Other", "Crem. and Other"), fill = c(lccol,locol,cocol), cex = 1.5)
plot(density(lams[,8]), col = acol, xlab = "",ylab = "", lwd = 3,cex.main = 2, main = "d)                                                                                                       ",
     ylim = c(0,80), xlim = c(0.9,1.06))
abline(v = mean(lams[,8]),col = acol, lty = 2, lwd =3)
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
max_rep = 100
scenario = c("none","all")
max_yrs = 10
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
                          max_yrs = 10,                                 ## the # years you want to iterate
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
plot(density(lam[,1]), col = "Red", xlab = "",ylab = "",cex.main = 2, main = "a)                                                                                                       ",ylim = c(0,25), xlim = c(0.,1.06))
legend("topright", legend = c("Vacant"), fill = c("Red"), cex = 1.5)
plot(density(lam[,2]), col = "Blue", xlab = "",ylab = "",cex.main = 2, main = "b)                                                                                                       ",ylim = c(0,25), xlim = c(0.,1.06))
lines(density(lam[,3]), col = "Green")
lines(density(lam[,4]), col = "Yellow")
legend("topright", legend = c("Crematogaster","Liometopum", "Other"), fill = c("Blue","Green","Yellow"), cex = 1.5)
plot(density(lam[,5]), col = "Orange", xlab = "",ylab = "",cex.main = 2, main = "c)                                                                                                       ",ylim = c(0,25), xlim = c(0.,1.06))
lines(density(lam[,6]), col = "Brown")
lines(density(lam[,7]), col = "Black")
legend("topright", legend = c("Crematogaster and Liometopum","Liometopum and Other", "Crematogaster and Other"), fill = c("Orange","Brown","Black"), cex = 1.5)
plot(density(lam[,8]), col = "Grey", xlab = "",ylab = "",cex.main = 2, main = "d)                                                                                                       ",ylim = c(0,25), xlim = c(0.,1.06))
mtext("Lambda",side=1,line=-2,outer=TRUE,cex=1.3)
mtext("Density",side=2,line=-2,outer=TRUE,cex=1.3,las=0)
legend("topright",legend = c("All Ants"),fill = c("Grey"),
       cex = 1.5)
dev.off()

png("lambda_st_full2.png")
par(mar=c(4,4,1,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(1,1,1,1))
plot(density(lams[1,]), col = "Red", xlab = "",ylab = "",cex.main = 2, main = "a)                                                                                                       ",ylim = c(0,25), xlim = c(0,1.06))
legend("topright", legend = c("Vacant"), fill = c("Red"), cex = 1.5)
plot(density(lams[2,]), col = "Blue", xlab = "",ylab = "",cex.main = 2, main = "b)                                                                                                       ",ylim = c(0,25), xlim = c(0,1.06))
lines(density(lams[3,]), col = "Green")
lines(density(lams[4,]), col = "Yellow")
legend("topright", legend = c("C. opuntiae","L. apiculatum", "Other"), fill = c("Blue","Green","Yellow"), cex = 1.5)
plot(density(lams[5,]), col = "Orange", xlab = "",ylab = "",cex.main = 2, main = "c)                                                                                                       ",ylim = c(0,25), xlim = c(0,1.06))
lines(density(lams[6,]), col = "Brown")
lines(density(lams[7,]), col = "Black")
legend("topright", legend = c("C. opuntiae and L. apiculatum","L. apiculatum and Other", "C. opuntiae and Other"), fill = c("Orange","Brown","Black"), cex = 1.5)
plot(density(lams[8,]), col = "Grey", xlab = "",ylab = "",cex.main = 2, main = "d)                                                                                                       ",ylim = c(0,25), xlim = c(0,1.06))
mtext("Lambda",side=1,line=-2,outer=TRUE,cex=1.3)
mtext("Density",side=2,line=-2,outer=TRUE,cex=1.3,las=0)
legend("topright",legend = c("All Ants"),fill = c("Grey"),
       cex = 1.5)
dev.off()

png("lambda_st_full3.png")
par(mar=c(4,4,1.01,1))
layout(matrix(c(1,2,3,4),
              ncol = 1, nrow = 4), heights = c(1,1,1,1))
plot(density(lams[,1]), col = vcol, xlab = "",ylab = "", lwd = 3,cex.main = 2,  main = "a)                                                                                                       ",
     ylim = c(0,80), xlim = c(0.9,1.06))
abline(v = mean(lams[,1]),col = vcol, lty = 2, lwd =3)
legend("topright", legend = c("Vacant"), fill = c(vcol), cex = 1.5)
plot(density(lams[,2]), col = ccol, xlab = "",ylab = "", lwd = 3,cex.main = 2, main = "b)                                                                                                       ",
     ylim = c(0,80), xlim = c(0.9,1.06))
abline(v = mean(lams[,2]),col = ccol, lty = 2, lwd =3)
lines(density(lams[,3]), col = lcol, lwd = 3)
abline(v = mean(lams[,3]),col = lcol, lty = 2, lwd =3)
lines(density(lams[,4]), col = ocol, lwd = 3)
abline(v = mean(lams[,4]),col = ocol, lty = 2, lwd =3)
legend("topright", legend = c("Crem.","Liom.", "Other"), fill = c(ccol,lcol,ocol), cex = 1.5)
plot(density(lams[,5]), col = lccol, xlab = "",ylab = "", lwd = 3,cex.main = 2, main = "c)                                                                                                       ",
     ylim = c(0,80), xlim = c(0.9,1.06))
abline(v = mean(lams[,5]),col = lccol, lty = 4, lwd =3)
lines(density(lams[,6]), col = locol, lwd = 3)
abline(v = mean(lams[,6]),col = locol, lty = 2, lwd =3)
lines(density(lams[,7]), col = cocol, lwd = 3)
abline(v = mean(lams[,7]),col = cocol, lty = 2, lwd =3)
legend("topright", legend = c("Crem. and Liom.","Liom. and Other", "Crem. and Other"), fill = c(lccol,locol,cocol), cex = 1.5)
plot(density(lams[,8]), col = acol, xlab = "",ylab = "", lwd = 3,cex.main = 2, main = "d)                                                                                                       ",
     ylim = c(0,80), xlim = c(0.9,1.06))
abline(v = mean(lams[,8]),col = acol, lty = 2, lwd =3)
mtext("Lambda",side=1,line=-2,outer=TRUE,cex=1.3)
mtext("Density",side=2,line=-2,outer=TRUE,cex=1.3,las=0)
legend("topright",legend = c("All Ants"),fill = c(acol),
       cex = 1.5)
dev.off()



##########################################################################################################
##
##                            Call the IPM and understand the outputs
##
##########################################################################################################

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code")
source("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code/IPM.R")


##################################################################################
## Check that all functions written in the IPM source code 
## run properly and the outputs make sense
##################################################################################
################################
## Survival and Growth Kernel
##Check if it runs properly properly with fake data 
pxy(x=c(1,2,3),y=c(1,2,3),i="liom",params)
i = c("liom","vacant","crem","other")
x = c(5,5,5,5)
y = c(6,6,6,6)
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
## Check that the outputs make sense
#################################
## Growth Survival Transition Kernel
## Check if it works
i = c("liom","vacant","crem","vacant")
j = c("vacant","crem","crem","vacant")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
scenario = c("all","cremvac","othercremvac","none")
pt <- vector()
for(n in 1:length(i)){
  pt[n] <- ptxy(x[n],y[n],i[n],j[n],params,scenario[n])
}
pt
## Check that the outputs make sense
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
i = c("liom","vacant","other","crem")
j = c("vacant","other","vacant","vacant")
scenario <- c("liomvac","othervac","othervac","cremvac")
bmat <- vector()
for(n in 1:length(i)){
  bmat[n] <- lambda(bigmatrix.2(params,lower,upper,matsize,i[n],j[n],scenario[n])$IPMmat)
}
bmat
## Check that the outputs make sense
# This diagnostic shows that the columsn should sum to the survival function of the vacant 
testmat <- bigmatrix.2(params,lower=cholla_min-20,upper=cholla_max+20,matsize,"vacant","liom","liomvac")$Tmat
surv <- colSums(testmat[3:(n+2),3:(n+2)])
plot(surv,ylim = c(0,1))
#################################
## Calculate Matrix for two ants and vacant
## Check that it works

## Check that the outputs make sense
#################################


lams <- as.data.frame(rep(NA,8))

######## No Ants ####
lams$means[1] <- lambda(bigmatrix.1(params,lower,upper,matsize,"vacant")$IPMmat)
## Vacant = 0.9344461


######## One Ant Results ####
#### Deterministic
i <- c("crem","liom","other")
i <- c("crem","liom","other")
scenario = c("cremvac","liomvac","othervac")
bmat <- vector()
for(n in 1:length(i)){
  bmat[n] <- lambda(bigmatrix(params,lower,upper,matsize,i[n],j[n],scenario[n])$IPMmat)
}
lams$means[2:4] <- bmat
lambda(bigmatrix)
## Crem & Vacant =  0.9369948
## Liom & Vacant = 0.9348296
## Other & Vacant = 0.9361773
#### W Posterior

######## Two Ant ####
######## One Ant Results ####
#### Deterministic
i <- c("liom","other","vacant")
j <- c("liom","other", "vacant")
scenario = c("liomcremvac","liomvacother", "othercremvac" )
bmat <- vector()
for(n in 1:length(i)){
  bmat[n] <- bigmatrix(params,lower,upper,matsize,2,i[n],j[n],scenario[n])
}
lams$means[5:7]<-bmat
## Liom & Crem & Vacant = 0.9369632
## Liom & Other & Vacant = 0.9365571
## Other & Crem & Vacant = 0.9382092

#### W Posterior

######## All Ant ####
######## One Ant Results ####
#### Deterministic
lams$means[8]<-bigmatrix(params,lower,upper,matsize,3,"crem","other","all")
## All = 0.9388964
#### W Posterior

## Visualize the Deterministic Mean Lambdas
## Vacant = 0.9344461
## Crem & Vacant =  0.9369948
## Liom & Vacant = 0.9348296
## Other & Vacant = 0.9361773
## Liom & Crem & Vacant = 0.9369632
## Liom & Other & Vacant = 0.9365571
## Other & Crem & Vacant = 0.9382092
## All = 0.9388964
lams$scenario <- c("No Ants","Crem.","Liom.","Oth.",
                    "Liom. & Crem.", "Liom. & Oth.", "Oth. & Crem.", "All Ants")
lams$scenario_abv <- c("None","C","L","O","L,C","L,O","O,C","All")
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("lambda_means3.png")
plot(lams$means, col = c("Red","Blue","Green","Yellow","Orange","Brown","Black","Grey"), 
     pch = 20, cex = 4, xlim = c(0.5,length(lams$scenario)), ylim = c(0.9740,0.9756),
     xaxt = "n",
     xlab = "Ant Scenario", ylab = "Mean Lambda Value", main = "Lambdas by Ant Scenario")
 text(x = 1:length(lams$scenario)-0.2, y = lams$means +.00015,
      labels = lams$scenario,
      srt = 35)
 legend("bottomright",legend = c("L = Liom.","C = Crem.","O = Other"))
# text(x = 1:length(lams$scenario),
#      ## Move labels to just below bottom of chart.
#      y = 0.9738,#par("usr")[3]-0.1,
#      ## Use names from the data list.
#      labels = (lams$scenario),
#      ## Change the clipping region.
#      xpd = NA,
#      ## Rotate the labels by 35 degrees.
#      srt = 20,
#      ## Adjust the labels to almost 100% right-justified.
#      adj = 0.965,
#      ## Increase label size.
#      cex = 1)
dev.off()





## A visual of the growth kernel
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("growth_heat.png")
par(mfrow=c(2,2)) 
G=h*outer(y,y,gxy,i = "crem",params=params)  # growth kernel
image(y,y,t(G),main='Crem Tended')        # plot it

G=h*outer(y,y,gxy,i = "liom",params=params)  # growth kernel
image(y,y,t(G),main='Liom Tended')        # plot it

G=h*outer(y,y,gxy,i = "other",params=params)  # growth kernel
image(y,y,t(G),main='Other Tended')        # plot it

G=h*outer(y,y,gxy,i = "vacant",params=params)  # growth kernel
image(y,y,t(G),main='Not Tended')        # plot it
dev.off()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code")




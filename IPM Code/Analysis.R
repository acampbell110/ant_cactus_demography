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
################################
## Transition between occupied and vacancy
## Scenario options == "othervac", "liomvac", "cremvac"
transition.1(15,"vacant","liom",params,scenario = "liomvac")
transition.1(15,"vacant","other",params,scenario = "othervac")
transition.1(15,"vacant","crem",params,scenario = "cremvac")

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


lams <- as.data.frame(rep(NA,8))

######## No Ants ####
lams$means[1] <- bigmatrix.1(params,lower,upper,matsize,0,"vacant")
## Vacant = 0.9344461


######## One Ant Results ####
#### Deterministic
i <- c("crem","liom","other")
i <- c("crem","liom","other")
scenario = c("cremvac","liomvac","othervac")
bmat <- vector()
for(n in 1:length(i)){
  bmat[n] <- bigmatrix(params,lower,upper,matsize,1,i[n],j[n],scenario[n])
}
lams$means[2:4] <- bmat
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




##########################################################################################################
##
##                            Call the IPM and understand the outputs
##
##########################################################################################################

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code")
source("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code/IPM.R")
######## No Ants ####
bigmatrix.1(params,lower,upper,matsize,1,"vacant")
## Vacant = 0.9344461


######## One Ant Results ####
#### Deterministic
i <- c("crem","liom","other")
i <- c("crem","liom","other")
scenario = c("cremvac","liomvac","othervac")
bmat <- vector()
for(n in 1:length(i)){
  bmat[n] <- bigmatrix(params,lower,upper,matsize,2,i[n],j[n],scenario[n])
}
bmat
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
  bmat[n] <- bigmatrix(params,lower,upper,matsize,3,i[n],j[n],scenario[n])
}
bmat
## Liom & Crem & Vacant = 0.9369632
## Liom & Other & Vacant = 0.9365571
## Other & Crem & Vacant = 0.9382092

#### W Posterior

######## All Ant ####
######## One Ant Results ####
#### Deterministic
bigmatrix(params,lower,upper,matsize,4,"crem","other","all")
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
lams <- as.data.frame(rep(NA,8))
lams$means <- c(0.9344461,0.9369948,0.9348296,0.9361773,0.9369632,0.9365571,0.9382092,0.9388964)
lams$scenario <- c("No Ants","Crem & Vacant","Liom & Vacant","Other & Vacant",
                    "Liom & Crem & Vacant", "Liom & Other & Vacant", "Other & Crem & Vacant", "All Ants")

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("lambda_means.png")
plot(lams$means, col = c("Red","Blue","Green","Yellow","Orange","Brown","Black","Grey"), 
     pch = 20, cex = 3, xlim = c(0,20),
     xlab = "Ant Scenario", ylab = "Mean Lambda Value", main = "Lambdas by Ant Scenario")
legend("topright",legend = lams$scenario, fill = c("Red","Blue","Green","Yellow","Orange","Brown","Black","Grey"))
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




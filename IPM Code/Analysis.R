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
lams$scenario_abv <- c("V","C,V","L,V","O,V","L,C,V","L,O,V","O,C,V","All")
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
png("lambda_means.png")
plot(lams$means, col = c("Red","Blue","Green","Yellow","Orange","Brown","Black","Grey"), 
     pch = 20, cex = 4, ylim = c(0.933,0.94), xlim = c(0.5,length(lams$scenario)),
     xaxt = "n",
     xlab = "Ant Scenario", ylab = "Mean Lambda Value", main = "Lambdas by Ant Scenario")
# text(x = 1:length(lams$scenario)-0.2, y = lams$means +.0006, 
#      labels = lams$scenario_abv,
#      srt = 35)
# legend("topright",legend = lams$scenario, fill = c("Red","Blue","Green","Yellow","Orange","Brown","Black","Grey"))
# axis(1, at = c(1,2,3,4,5,6,7,8),labels = lams$scenario, srt = 45)
# axis(side = 1, labels = FALSE)
# text(seq(1, 8, by=1), par("usr")[3] - 0.2, labels = lams$scenario, srt = 45, pos = 1, xpd = TRUE)
text(x = 1:length(lams$scenario),
     ## Move labels to just below bottom of chart.
     y = 0.932,#par("usr")[3]-0.1,
     ## Use names from the data list.
     labels = (lams$scenario_abv),
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 35,
     ## Adjust the labels to almost 100% right-justified.
     adj = 0.965,
     ## Increase label size.
     cex = 1.2)
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




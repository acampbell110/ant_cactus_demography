##########################################################################################################
##
##                            Call the IPM and understand the outputs
##
##########################################################################################################

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code")
source("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code/IPM.R")

#### One ant results
bigmatrix.1(4,4.5,lower,upper,matsize,1,"crem",(params))$IPMmat
## Check that it works
i = c("liom","vac","crem","other")
j = c("vac","crem","crem","liom")
x = c(-1,-5,4,3)
y = c(-1,-4,4.5,3.01)
big1 <- list()
lambda1 <- list()
for(n in 1:length(i)){
  big1[[n]] <- bigmatrix.1(x[n],y[n],lower,upper,matsize,1,i[n],params)
  lambda1[[n]] <- Re(eigen(big1[[n]]$IPMmat)$values[1])
}
lambda1

## Crem
crem_data <- subset(cactus, cactus$ant_t == "crem" & cactus$ant_t1 == "crem")
i <- crem_data$ant_t
j <- crem_data$ant_t1
x <- crem_data$logsize_t
y <- crem_data$logsize_t1
big1 <- list()
lambda1 <- list()
lambda <- vector()
for(n in 1:length(i)){
  big1[[n]] <- bigmatrix.1(x[n],y[n],lower,upper,matsize,1,i[n],params)
  lambda1[[n]] <- Re(eigen(big1[[n]]$IPMmat)$values[1])
}
lambda1
## 0.9929766

## Other
other_data <- subset(cactus, cactus$ant_t == "other" & cactus$ant_t1 == "other")
i <- other_data$ant_t
j <- other_data$ant_t1
x <- other_data$logsize_t
y <- other_data$logsize_t1
big1 <- list()
lambda1 <- list()
lambda <- vector()
for(n in 1:length(i)){
  big1[[n]] <- bigmatrix.1(x[n],y[n],lower,upper,matsize,1,i[n],params)
  lambda1[[n]] <- Re(eigen(big1[[n]]$IPMmat)$values[1])
}
lambda1
## 0.9968325

## Liom
liom_data <- subset(cactus, cactus$ant_t == "liom" & cactus$ant_t1 == "liom")
i <- liom_data$ant_t
j <- liom_data$ant_t1
x <- liom_data$logsize_t
y <- liom_data$logsize_t1
big1 <- list()
lambda1 <- list()
lambda <- vector()
for(n in 1:length(i)){
  big1[[n]] <- bigmatrix.1(x[n],y[n],lower,upper,matsize,1,i[n],params)
  lambda1[[n]] <- Re(eigen(big1[[n]]$IPMmat)$values[1])
}
lambda1
## 0.9969405

## vacant
vac_data <- subset(cactus, cactus$ant_t == "vacant" & cactus$ant_t1 == "vacant")
i <- vac_data$ant_t
j <- vac_data$ant_t1
x <- vac_data$logsize_t
y <- vac_data$logsize_t1
big1 <- list()
lambda1 <- list()
lambda <- vector()
for(n in 1:length(i)){
  big1[[n]] <- bigmatrix.1(x[n],y[n],lower,upper,matsize,1,i[n],params)
  lambda1[[n]] <- Re(eigen(big1[[n]]$IPMmat)$values[1])
}
lambda1
## 

#### Visualize the Growth Model
min.size=min(c(cactus$logsize_t,cactus$logsize_t1),na.rm = T)
max.size=max(c(cactus$logsize_t,cactus$logsize_t1),na.rm = T)
# number of cells in the discretized kernel
n=100 
# boundary points (the edges of the cells defining the kernel)
b=min.size+c(0:n)*(max.size-min.size)/n 
# mesh points (midpoints of the cells)
y=0.5*(b[1:n]+b[2:(n+1)])
# width of the cells
h=y[2]-y[1]


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




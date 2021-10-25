##########################################################################################################
##
##                            Call the IPM and understand the outputs
##
##########################################################################################################

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code")
source("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code/IPM.R")


##Crem
big_list_c <- list()
lambda_c <- vector()
stable_c <- list()
## Liom
big_list_l <- list()
lambda_l <- vector()
stable_l <- list()
## Other
big_list_o <- list()
lambda_o <- vector()
stable_o <- list()
## Crem
for(i in 1:100){
  params = cholla[,i]
  big_list_c[[i]]<-bigmatrix(cholla, lower, upper, matsize)$IPMmat_c
  mat_c <- big_list_c[[i]]
  eig_c <- eigen(mat_c)
  lambda_c[i]<-Re(eig_c$values[1])
  
  stable_c[[i]] <- stable.stage(mat_c)
}
## Liom
for(i in 1:100){
  params = cholla[,i]
  big_list_l[[i]]<-bigmatrix(cholla, lower, upper, matsize)$IPMmat_l
  mat_l <- big_list_l[[i]]
  eig_l <- eigen(mat_l)
  lambda_l[i]<-Re(eig_l$values[1])
  
  stable_l[[i]] <- stable.stage(mat_l)
}
## Other
for(i in 1:100){
  params = cholla[,i]
  big_list_o[[i]]<-bigmatrix(cholla, lower, upper, matsize)$IPMmat_o
  mat_o <- big_list_o[[i]]
  eig_o <- eigen(mat_o)
  lambda_o[i]<-Re(eig_o$values[1])
  
  stable_o[[i]] <- stable.stage(mat_o)
}

hist(lambda_c)
hist(lambda_l)
hist(lambda_o)

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")

#### Lambda dist
png("two_species_models.png")
plot(density(lambda_c), col = "red",xlim=c(0.5,0.75))
lines(density(lambda_l), col = "blue")
lines(density(lambda_o), col = "black")
dev.off()
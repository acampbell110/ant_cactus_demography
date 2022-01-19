##########################################################################################################
##
##                            Call the IPM and understand the outputs
##
##########################################################################################################

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code")
source("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code/IPM.R")

#### One ant results
##Crem
for(i in 1:100){
  params = cholla[,i]
  big_list_c[[i]]<-bigmatrix.1(x_dum,y_dum,cholla, lower, upper, matsize)$IPMmat_c
  mat_c <- big_list_c[[i]]
  eig_c <- eigen(mat_c)
  lambda_c[i]<-Re(eig_c$values[1])
  
  stable_c[[i]] <- stable.stage(mat_c)
}
Re(eigen(bigmatrix.1(x_dum,y_dum,cholla, lower, upper, matsize)$IPMmat_c)$values[1])
hist(lambda_c)
lambda_c

##Liom
Re(eigen(bigmatrix.1(x_dum,y_dum,cholla,lower,upper,matsize,1)$IPMmat_l)$values[1])

##Other
Re(eigen(bigmatrix.1(x_dum,y_dum,cholla,lower,upper,matsize,1)$IPMmat_o)$values[1])

##Vacant
Re(eigen(bigmatrix.1(x_dum,y_dum,cholla,lower,upper,matsize,1)$IPMmat_v)$values[1])


setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")

#### Lambda dist
png("two_species_models.png")
plot(density(lambda_c), col = "red",xlim=c(0.5,0.75))
lines(density(lambda_l), col = "blue")
lines(density(lambda_o), col = "black")
dev.off()


















##Crem_Vac
Re(eigen(bigmatrix.2(x_dum,y_dum,cholla,lower,upper,matsize,2)$IPMmat_vc)$values[1])
##Liom_Vac
Re(eigen(bigmatrix.2(x_dum,y_dum,cholla,lower,upper,matsize,2)$IPMmat_vl)$values[1])
##Other_Vac
Re(eigen(bigmatrix.2(x_dum,y_dum,cholla,lower,upper,matsize,2)$IPMmat_vo)$values[1])











################################################################################
## The purpose of this script is to validate the IPM functions defined in script
## 03 and validate the choices like matsize, floor extension, ceiling extension,
## etc
################################################################################
## Source the IPM functions and parameters from script 3
source("03_cholla_ant_IPM_params_functions.R")
# take the colmeans of the params and growth random effects
mean_params=data.frame(t(colMeans(params)))
grow_frx1_mean=data.frame(t(colMeans(grow_rfx1)))
grow_frx2_mean=data.frame(t(colMeans(grow_rfx2)))
grow_frx3_mean=data.frame(t(colMeans(grow_rfx3)))
grow_frx4_mean=data.frame(t(colMeans(grow_rfx4)))
# determine which year has the highest and lowest of the growth random effects
max(grow_frx1_mean) ## 2011
min(grow_frx1_mean) ## 2010
max(grow_frx2_mean) ## 2011
min(grow_frx2_mean) ## 2010
max(grow_frx3_mean) ## 2011
min(grow_frx3_mean) ## 2010
max(grow_frx4_mean) ## 2011
min(grow_frx4_mean) ## 2010
# the same for all ants (best year is 2011 and worst year is 2010)
#### Vacancy
## Determine floor and ceiling extensions that work for vacancy model
# These extensions work for both the worst and best year
# bigobj_vacant_worst<-bigmatrix.1(params=mean_params,
#                            lower=lower,
#                            upper=upper,
#                            floor=25,
#                            ceiling=4,
#                            matsize=400,
#                            grow_rfx1=-.23,grow_rfx2=-.3,
#                            grow_rfx3=-.25,grow_rfx4=-.34)
# plot(bigobj_vacant_worst$y,bigobj_vacant_worst$p,ylab="colsums of Tmat",ylim=c(0,1))
# abline(v=c(lower,upper),col="red")
# bigobj_vacant_best<-bigmatrix.1(params=mean_params,
#                                  lower=lower,
#                                  upper=upper,
#                                  floor=25,
#                                  ceiling=4,
#                                  matsize=400,
#                                  grow_rfx1=.25,grow_rfx2=.42,
#                                  grow_rfx3=.199,grow_rfx4=.411)
# plot(bigobj_vacant_best$y,bigobj_vacant_best$p,ylab="colsums of Tmat",ylim=c(0,1))
# abline(v=c(lower,upper),col="red")
## Run the model with different matsizes at the same extension levels and see where the lambdas stabilize
evicts_vacant<-lambdas_vacant<-c()
matsizes<-c(200,250,300,350,400,450,500,600,800,1000)
for(i in 1:length(matsizes)){
  bigobj_vacant<-bigmatrix.1(params=mean_params,
                             lower=lower,
                             upper=upper,
                             floor=25,
                             ceiling=4,
                             matsize=matsizes[i])
  evicts_vacant[i]<-bigobj_vacant$evict
  lambdas_vacant[i]<-popbio::lambda(bigobj_vacant$IPMmat)
}
# plot(matsizes,evicts_vacant) # the larger the matsize the higher the eviction
# plot(matsizes,lambdas_vacant,type="b") # the lambda stabilizes around 500-550

#### Liom and Vacancy
## Determine floor and ceiling extensions that work for vacancy model
# These extensions work for both the worst and best year
# bigobj_liomvacant_worst<-bigmatrix.2(params=mean_params,
#                                  scenario="liomvac",
#                                  lower=lower,
#                                  upper=upper,
#                                  floor=25,
#                                  ceiling=4,
#                                  matsize=400,
#                                  grow_rfx1=-.23,grow_rfx2=-.3,
#                                  grow_rfx3=-.25,grow_rfx4=-.34)
# plot(bigobj_liomvacant_worst$y,bigobj_liomvacant_worst$p_v,ylab="colsums of Tmat",ylim=c(0,1))
# points(bigobj_liomvacant_worst$y,bigobj_liomvacant_worst$p_v, type = "b", col = "blue")
# abline(v=c(lower,upper),col="red")
# bigobj_liomvacant_best<-bigmatrix.2(params=mean_params,
#                                 scenario="liomvac",
#                                 lower=lower,
#                                 upper=upper,
#                                 floor=25,
#                                 ceiling=4,
#                                 matsize=400,
#                                 grow_rfx1=.25,grow_rfx2=.42,
#                                 grow_rfx3=.199,grow_rfx4=.411)
# plot(bigobj_liomvacant_best$y,bigobj_liomvacant_best$p_l,ylab="colsums of Tmat",ylim=c(0,1))
# points(bigobj_liomvacant_best$y,bigobj_liomvacant_best$p_v, type = "b", col = "blue")
# abline(v=c(lower,upper),col="red")
## Run the model with different matsizes at the same extension levels and see where the lambdas stabilize
evicts_liomvac<-lambdas_liomvac<-c()
for(i in 1:length(matsizes)){
  bigobj_liomvac<-bigmatrix.2(params=mean_params,
                              scenario="liomvac",
                              lower=lower,
                              upper=upper,
                              floor=25,
                              ceiling=4,
                              matsize=matsizes[i])
  evicts_liomvac[i]<-bigobj_liomvac$evict
  lambdas_liomvac[i]<-popbio::lambda(bigobj_liomvac$IPMmat)
}
# plot(matsizes,evicts_liomvac) # the larger the matsize, the larger the ecivtion
# plot(matsizes,lambdas_liomvac,type="b",ylim=c(.98,1.01)) # stability reached around 400

#### Crem and Vacancy
## Determine floor and ceiling extensions that work for vacancy model
# These extensions work for both the worst and best year
# bigobj_cremvacant_worst<-bigmatrix.2(params=mean_params,
#                                      scenario="cremvac",
#                                      lower=lower,
#                                      upper=upper,
#                                      floor=25,
#                                      ceiling=4,
#                                      matsize=400,
#                                      grow_rfx1=-.23,grow_rfx2=-.3,
#                                      grow_rfx3=-.25,grow_rfx4=-.34)
# plot(bigobj_cremvacant_worst$y,bigobj_cremvacant_worst$p_c,ylab="colsums of Tmat",ylim=c(0,1))
# points(bigobj_cremvacant_worst$y,bigobj_cremvacant_worst$p_v, col = "blue")
# abline(v=c(lower,upper),col="red")
# bigobj_cremvacant_best<-bigmatrix.2(params=mean_params,
#                                     scenario="cremvac",
#                                     lower=lower,
#                                     upper=upper,
#                                     floor=25,
#                                     ceiling=4,
#                                     matsize=400,
#                                     grow_rfx1=.25,grow_rfx2=.42,
#                                     grow_rfx3=.199,grow_rfx4=.411)
# plot(bigobj_liomvacant_best$y,bigobj_liomvacant_best$p_c,ylab="colsums of Tmat",ylim=c(0,1))
# points(bigobj_cremvacant_best$y,bigobj_cremvacant_best$p_v, col = "blue")
# abline(v=c(lower,upper),col="red")
# ## Run the model with different matsizes at the same extension levels and see where the lambdas stabilize
# bigobj_cremvac<-bigmatrix.2(params=mean_params,
#                             scenario="cremvac",
#                             lower=lower,
#                             upper=upper,
#                             floor=25,
#                             ceiling=4,
#                             matsize=400)
# plot(bigobj_cremvac$y,bigobj_cremvac$p_c,ylab="colsums of Tmat",ylim=c(0,1))
# points(bigobj_cremvac$y,bigobj_cremvac$p_v, col = "blue")
# abline(v=c(lower,upper),col="red")
## Run the model with different matsizes at the same extension levels and see where the lambdas stabilize
evicts_cremvac<-lambdas_cremvac<-c()
for(i in 1:length(matsizes)){
  bigobj_cremvac<-bigmatrix.2(params=mean_params,
                              scenario="cremvac",
                              lower=lower,
                              upper=upper,
                              floor=25,
                              ceiling=4,
                              matsize=matsizes[i])
  evicts_cremvac[i]<-bigobj_cremvac$evict
  lambdas_cremvac[i]<-popbio::lambda(bigobj_cremvac$IPMmat)
}
# plot(matsizes,evicts_cremvac) # the larger the matsize, the larger the ecivtion
# plot(matsizes,lambdas_cremvac,type="b",ylim=c(.98,1.01)) # stability reached around 400

#### Other and Vacancy
## Determine floor and ceiling extensions that work for vacancy model
# These extensions work for both the worst and best year
# bigobj_othervacant_worst<-bigmatrix.2(params=mean_params,
#                                      scenario="othervac",
#                                      lower=lower,
#                                      upper=upper,
#                                      floor=25,
#                                      ceiling=4,
#                                      matsize=400,
#                                      grow_rfx1=-.23,grow_rfx2=-.3,
#                                      grow_rfx3=-.25,grow_rfx4=-.34)
# plot(bigobj_othervacant_worst$y,bigobj_othervacant_worst$p_o,ylab="colsums of Tmat",ylim=c(0,1))
# points(bigobj_othervacant_worst$y,bigobj_othervacant_worst$p_v, col = "blue")
# abline(v=c(lower,upper),col="red")
# bigobj_othervacant_best<-bigmatrix.2(params=mean_params,
#                                     scenario="othervac",
#                                     lower=lower,
#                                     upper=upper,
#                                     floor=25,
#                                     ceiling=4,
#                                     matsize=400,
#                                     grow_rfx1=.25,grow_rfx2=.42,
#                                     grow_rfx3=.199,grow_rfx4=.411)
# plot(bigobj_othervacant_best$y,bigobj_othervacant_best$p_o,ylab="colsums of Tmat",ylim=c(0,1))
# points(bigobj_othervacant_best$y,bigobj_othervacant_best$p_v, col = "blue")
# abline(v=c(lower,upper),col="red")
# ## Run the model with different matsizes at the same extension levels and see where the lambdas stabilize
# bigobj_othervac<-bigmatrix.2(params=mean_params,
#                             scenario="othervac",
#                             lower=lower,
#                             upper=upper,
#                             floor=25,
#                             ceiling=4,
#                             matsize=400)
# plot(bigobj_othervac$y,bigobj_othervac$p_o,ylab="colsums of Tmat",ylim=c(0,1))
# points(bigobj_othervac$y,bigobj_othervac$p_v, col = "blue")
# abline(v=c(lower,upper),col="red")
## Run the model with different matsizes at the same extension levels and see where the lambdas stabilize
evicts_othervac<-lambdas_othervac<-c()
for(i in 1:length(matsizes)){
  bigobj_othervac<-bigmatrix.2(params=mean_params,
                              scenario="othervac",
                              lower=lower,
                              upper=upper,
                              floor=25,
                              ceiling=4,
                              matsize=matsizes[i])
  evicts_othervac[i]<-bigobj_othervac$evict
  lambdas_othervac[i]<-popbio::lambda(bigobj_othervac$IPMmat)
}
# plot(matsizes,evicts_othervac) # the larger the matsize, the larger the ecivtion
# plot(matsizes,lambdas_othervac,type="b",ylim=c(.98,1.01)) # stability reached around 400


#### Liom and Crem and Vacancy
## Determine floor and ceiling extensions that work for vacancy model
# These extensions work for both the worst and best year
# bigobj_liomcremvacant_worst<-bigmatrix.3(params=mean_params,
#                                      scenario="liomcremvac",
#                                      lower=lower,
#                                      upper=upper,
#                                      floor=25,
#                                      ceiling=4,
#                                      matsize=400,
#                                      grow_rfx1=-.23,grow_rfx2=-.3,
#                                      grow_rfx3=-.25,grow_rfx4=-.34)
# plot(bigobj_liomcremvacant_worst$y,bigobj_liomcremvacant_worst$p_l,ylab="colsums of Tmat",ylim=c(0,1))
# points(bigobj_liomcremvacant_worst$y,bigobj_liomcremvacant_worst$p_c, col = "blue")
# points(bigobj_liomcremvacant_worst$y,bigobj_liomcremvacant_worst$p_v, col = "green")
# abline(v=c(lower,upper),col="red")
# bigobj_liomcremvacant_best<-bigmatrix.3(params=mean_params,
#                                     scenario="liomcremvac",
#                                     lower=lower,
#                                     upper=upper,
#                                     floor=25,
#                                     ceiling=4,
#                                     matsize=400,
#                                     grow_rfx1=.25,grow_rfx2=.42,
#                                     grow_rfx3=.199,grow_rfx4=.411)
# plot(bigobj_liomcremvacant_best$y,bigobj_liomcremvacant_best$p_l,ylab="colsums of Tmat",ylim=c(0,1))
# points(bigobj_liomcremvacant_best$y,bigobj_liomcremvacant_best$p_c, col = "blue")
# points(bigobj_liomcremvacant_best$y,bigobj_liomcremvacant_best$p_c, col = "green")
# abline(v=c(lower,upper),col="red")
# ## Run the model with different matsizes at the same extension levels and see where the lambdas stabilize
# bigobj_liomcremvac<-bigmatrix.3(params=mean_params,
#                             scenario="liomcremvac",
#                             lower=lower,
#                             upper=upper,
#                             floor=25,
#                             ceiling=4,
#                             matsize=400)
# plot(bigobj_liomcremvac$y,bigobj_liomcremvac$p_l,ylab="colsums of Tmat",ylim=c(0,1))
# points(bigobj_liomcremvac$y,bigobj_liomcremvac$p_c, col = "blue")
# points(bigobj_liomcremvac$y,bigobj_liomcremvac$p_v, col = "green")
# abline(v=c(lower,upper),col="red")
## Run the model with different matsizes at the same extension levels and see where the lambdas stabilize
evicts_liomcremvac<-lambdas_liomcremvac<-c()
for(i in 1:length(matsizes)){
  bigobj_liomcremvac<-bigmatrix.3(params=mean_params,
                               scenario="liomcremvac",
                               lower=lower,
                               upper=upper,
                               floor=25,
                               ceiling=4,
                               matsize=matsizes[i])
  evicts_liomcremvac[i]<-bigobj_liomcremvac$evict
  lambdas_liomcremvac[i]<-popbio::lambda(bigobj_liomcremvac$IPMmat)
}
# plot(matsizes,evicts_liomcremvac) # the larger the matsize, the larger the ecivtion
# plot(matsizes,lambdas_liomcremvac,type="b",ylim=c(.98,1.01)) # stability reached around 400

#### Liom and Other and Vacancy
## Determine floor and ceiling extensions that work for vacancy model
# These extensions work for both the worst and best year
# bigobj_liomothervacant_worst<-bigmatrix.3(params=mean_params,
#                                      scenario="liomvacother",
#                                      lower=lower,
#                                      upper=upper,
#                                      floor=25,
#                                      ceiling=4,
#                                      matsize=400,
#                                      grow_rfx1=-.23,grow_rfx2=-.3,
#                                      grow_rfx3=-.25,grow_rfx4=-.34)
# plot(bigobj_liomothervacant_worst$y,bigobj_liomothervacant_worst$p_l,ylab="colsums of Tmat",ylim=c(0,1))
# points(bigobj_liomothervacant_worst$y,bigobj_liomothervacant_worst$p_o, col = "blue")
# points(bigobj_liomothervacant_worst$y,bigobj_liomothervacant_worst$p_v, col = "green")
# abline(v=c(lower,upper),col="red")
# bigobj_liomothervacant_best<-bigmatrix.3(params=mean_params,
#                                     scenario="liomvacother",
#                                     lower=lower,
#                                     upper=upper,
#                                     floor=25,
#                                     ceiling=4,
#                                     matsize=400,
#                                     grow_rfx1=.25,grow_rfx2=.42,
#                                     grow_rfx3=.199,grow_rfx4=.411)
# plot(bigobj_liomothervacant_best$y,bigobj_liomothervacant_best$p_l,ylab="colsums of Tmat",ylim=c(0,1))
# points(bigobj_liomothervacant_best$y,bigobj_liomothervacant_best$p_o, col = "blue")
# points(bigobj_liomothervacant_best$y,bigobj_liomothervacant_best$p_v, col = "green")
# abline(v=c(lower,upper),col="red")
# ## Run the model with different matsizes at the same extension levels and see where the lambdas stabilize
# bigobj_liomothervac<-bigmatrix.3(params=mean_params,
#                             scenario="liomvacother",
#                             lower=lower,
#                             upper=upper,
#                             floor=25,
#                             ceiling=4,
#                             matsize=400)
# plot(bigobj_liomothervac$y,bigobj_liomothervac$p_l,ylab="colsums of Tmat",ylim=c(0,1))
# points(bigobj_liomothervac$y,bigobj_liomothervac$p_o, col = "blue")
# points(bigobj_liomothervac$y,bigobj_liomothervac$p_v, col = "green")
# abline(v=c(lower,upper),col="red")
## Run the model with different matsizes at the same extension levels and see where the lambdas stabilize
evicts_liomothervac<-lambdas_liomothervac<-c()
for(i in 1:length(matsizes)){
  bigobj_liomothervac<-bigmatrix.3(params=mean_params,
                                  scenario="liomvacother",
                                  lower=lower,
                                  upper=upper,
                                  floor=25,
                                  ceiling=4,
                                  matsize=matsizes[i])
  evicts_liomothervac[i]<-bigobj_liomothervac$evict
  lambdas_liomothervac[i]<-popbio::lambda(bigobj_liomothervac$IPMmat)
}
# plot(matsizes,evicts_liomothervac) # the larger the matsize, the larger the ecivtion
# plot(matsizes,lambdas_liomothervac,type="b",ylim=c(.98,1.01)) # stability reached around 400

#### Crem and Other and Vacancy
## Determine floor and ceiling extensions that work for vacancy model
# These extensions work for both the worst and best year
# bigobj_cremothervacant_worst<-bigmatrix.3(params=mean_params,
#                                      scenario="othercremvac",
#                                      lower=lower,
#                                      upper=upper,
#                                      floor=25,
#                                      ceiling=4,
#                                      matsize=400,
#                                      grow_rfx1=-.23,grow_rfx2=-.3,
#                                      grow_rfx3=-.25,grow_rfx4=-.34)
# plot(bigobj_cremothervacant_worst$y,bigobj_cremothervacant_worst$p_c,ylab="colsums of Tmat",ylim=c(0,1))
# points(bigobj_cremothervacant_worst$y,bigobj_cremothervacant_worst$p_o, col = "blue")
# points(bigobj_cremothervacant_worst$y,bigobj_cremothervacant_worst$p_v, col = "green")
# abline(v=c(lower,upper),col="red")
# bigobj_cremothervacant_best<-bigmatrix.3(params=mean_params,
#                                     scenario="othercremvac",
#                                     lower=lower,
#                                     upper=upper,
#                                     floor=25,
#                                     ceiling=4,
#                                     matsize=400,
#                                     grow_rfx1=.25,grow_rfx2=.42,
#                                     grow_rfx3=.199,grow_rfx4=.411)
# plot(bigobj_cremothervacant_best$y,bigobj_cremothervacant_best$p_c,ylab="colsums of Tmat",ylim=c(0,1))
# points(bigobj_cremothervacant_best$y,bigobj_cremothervacant_best$p_o, col = "blue")
# points(bigobj_cremothervacant_best$y,bigobj_cremothervacant_best$p_v, col = "green")
# abline(v=c(lower,upper),col="red")
# ## Run the model with different matsizes at the same extension levels and see where the lambdas stabilize
# bigobj_cremothervac<-bigmatrix.3(params=mean_params,
#                             scenario="othercremvac",
#                             lower=lower,
#                             upper=upper,
#                             floor=25,
#                             ceiling=4,
#                             matsize=400)
# plot(bigobj_cremothervac$y,bigobj_cremothervac$p_c,ylab="colsums of Tmat",ylim=c(0,1))
# points(bigobj_cremothervac$y,bigobj_cremothervac$p_o, col = "blue")
# points(bigobj_cremothervac$y,bigobj_cremothervac$p_v, col = "green")
# abline(v=c(lower,upper),col="red")
## Run the model with different matsizes at the same extension levels and see where the lambdas stabilize
evicts_cremothervac<-lambdas_cremothervac<-c()
for(i in 1:length(matsizes)){
  bigobj_cremothervac<-bigmatrix.3(params=mean_params,
                                   scenario="othercremvac",
                                   lower=lower,
                                   upper=upper,
                                   floor=25,
                                   ceiling=4,
                                   matsize=matsizes[i])
  evicts_cremothervac[i]<-bigobj_cremothervac$evict
  lambdas_cremothervac[i]<-popbio::lambda(bigobj_cremothervac$IPMmat)
}
# plot(matsizes,evicts_cremothervac) # the larger the matsize, the larger the ecivtion
# plot(matsizes,lambdas_cremothervac,type="b",ylim=c(.98,1.01)) # stability reached around 400

#### ALL
## Determine floor and ceiling extensions that work for vacancy model
# These extensions work for both the worst and best year
# bigobj_all_worst<-bigmatrix.4(params=mean_params,
#                                      scenario="all",
#                                      lower=lower,
#                                      upper=upper,
#                                      floor=25,
#                                      ceiling=4,
#                                      matsize=400,
#                                      grow_rfx1=-.23,grow_rfx2=-.3,
#                                      grow_rfx3=-.25,grow_rfx4=-.34)
# plot(bigobj_all_worst$y,bigobj_all_worst$p_c,ylab="colsums of Tmat",ylim=c(0,1))
# points(bigobj_all_worst$y,bigobj_all_worst$p_o, col = "blue")
# points(bigobj_all_worst$y,bigobj_all_worst$p_v, col = "green")
# points(bigobj_all_worst$y,bigobj_all_worst$p_l, col = "orange")
# abline(v=c(lower,upper),col="red")
# bigobj_all_best<-bigmatrix.4(params=mean_params,
#                                     scenario="all",
#                                     lower=lower,
#                                     upper=upper,
#                                     floor=25,
#                                     ceiling=4,
#                                     matsize=400,
#                                     grow_rfx1=.25,grow_rfx2=.42,
#                                     grow_rfx3=.199,grow_rfx4=.411)
# plot(bigobj_all_best$y,bigobj_all_best$p_c,ylab="colsums of Tmat",ylim=c(0,1))
# points(bigobj_all_best$y,bigobj_all_best$p_o, col = "blue")
# points(bigobj_all_best$y,bigobj_all_best$p_v, col = "green")
# points(bigobj_all_best$y,bigobj_all_best$p_l, col = "orange")
# abline(v=c(lower,upper),col="red")
# ## Run the model with different matsizes at the same extension levels and see where the lambdas stabilize
# bigobj_all<-bigmatrix.4(params=mean_params,
#                             scenario="all",
#                             lower=lower,
#                             upper=upper,
#                             floor=25,
#                             ceiling=4,
#                             matsize=400)
# plot(bigobj_all$y,bigobj_all$p_c,ylab="colsums of Tmat",ylim=c(0,1))
# points(bigobj_all$y,bigobj_all$p_o, col = "blue")
# points(bigobj_all$y,bigobj_all$p_v, col = "green")
# abline(v=c(lower,upper),col="red")
## Run the model with different matsizes at the same extension levels and see where the lambdas stabilize
evicts_all<-lambdas_all<-c()
for(i in 1:length(matsizes)){
  bigobj_all<-bigmatrix.4(params=mean_params,
                                   scenario="all",
                                   lower=lower,
                                   upper=upper,
                                   floor=25,
                                   ceiling=4,
                                   matsize=matsizes[i])
  evicts_all[i]<-bigobj_all$evict
  lambdas_all[i]<-popbio::lambda(bigobj_all$IPMmat)
}
# plot(matsizes,evicts_all) # the larger the matsize, the larger the ecivtion
# plot(matsizes,lambdas_all,type="b",ylim=c(.98,1.01)) # stability reached around 400


plot(matsizes, lambdas_vacant, type = "b", ylim = c(.98,1.01), col = vcol)
points(matsizes, lambdas_liomvac, type = "b", col = lcol)
points(matsizes, lambdas_cremvac, type = "b", col = ccol)
points(matsizes, lambdas_othervac, type = "b", col = ocol)
points(matsizes, lambdas_liomcremvac, type = "b", col = lccol)
points(matsizes, lambdas_liomothervac, type = "b", col = locol)
points(matsizes, lambdas_cremothervac, type = "b", col = cocol)
points(matsizes, lambdas_all, type = "b", col = acol)

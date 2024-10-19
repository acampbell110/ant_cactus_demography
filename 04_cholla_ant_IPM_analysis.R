################################################################################
################################################################################
##                            Call the IPM and understand the outputs            
################################################################################
################################################################################
source("03_cholla_ant_IPM_params_functions.R")
## Set the colors for the visuals
cremcol <- "#9239F6"
liomcol <- "#00A08A"
othercol <- "#FF0076"
vaccol <- "#F8B660"
vcol <- "#ad90ec"
lcol <- "#084f98"
ccol <- "#e9a67a"
ocol <- "#93022f"
lccol <- "#5dc9cf"
locol <- "#cf3545"
cocol <- "#ab59c8"
acol <- "#5d906b"
cols <- c(vcol, ccol, lcol, ocol, lccol, locol, cocol, acol)

################################################################################
#### Get Deterministic Estimates of Lambda for each ant scenario for each ant colonizing scenario
################################################################################
####### Set up parameters for all models
# Pull out the mean matrix
params_mean<-data.frame(matrix(colMeans(params),1,ncol(params)))
names(params_mean)<-names(params)
scenario = c("none","cremvac","liomvac","othervac","liomcremvac","liomvacother","othercremvac","all")
scenario_abv <- c("V","C","L","O","LC","LO","OC","LOC")
colors_lambdas <- c(vcol,ccol,lcol,ocol,lccol,locol,cocol,acol)
x_vals <- c(1,3,5,7,9,11,13,15)


#### check the extension values
lower.extension<-4
upper.extension<-0.5
vacant_mat<-bigmatrix(params = params_mean,
                      scenario = "none",
                      lower = cholla_min,
                      upper = cholla_max,
                      floor = lower.extension,
                      ceiling = upper.extension,
                      matsize = matsize)
full_mat<-bigmatrix(params = params_mean,
                    scenario = "all",
                    lower = cholla_min,
                    upper = cholla_max,
                    floor = lower.extension,
                    ceiling = upper.extension,
                    matsize = matsize)
par(mfrow=c(1,2))
plot(vacant_mat$y,vacant_mat$p,col=vaccol,ylim=c(0.75,1.05))
plot(full_mat$y,full_mat$p_v,col=vaccol,ylim=c(0.75,1.05))
points(full_mat$y,full_mat$p_l,col=liomcol)
points(full_mat$y,full_mat$p_o,col=othercol)
points(full_mat$y,full_mat$p_c,col=cremcol)
## They look good

## find the right matrix dimension
dims<-c(10,25,50,100,150,200,250,300)
vacant_lam<-full_lam<-c()
for(i in 1:length(dims)){
  vacant_lam[i]<-lambda(bigmatrix(params = params_mean,
                        scenario = "none",
                        lower = cholla_min,
                        upper = cholla_max,
                        floor = lower.extension,
                        ceiling = upper.extension,
                        matsize = dims[i])$IPMmat)
  full_lam[i]<-lambda(bigmatrix(params = params_mean,
                      scenario = "all",
                      lower = cholla_min,
                      upper = cholla_max,
                      floor = lower.extension,
                      ceiling = upper.extension,
                      matsize = dims[i])$IPMmat)
}
plot(dims,full_lam)
points(dims,vacant_lam,col="red")
## 150 is sufficient
matsize<-150

## look at stable size and ant structure
vacant_mat<-bigmatrix(params = params_mean,
                      scenario = "none",
                      lower = cholla_min,
                      upper = cholla_max,
                      floor = lower.extension,
                      ceiling = upper.extension,
                      matsize = matsize)
full_mat<-bigmatrix(params = params_mean,
                    scenario = "all",
                    lower = cholla_min,
                    upper = cholla_max,
                    floor = lower.extension,
                    ceiling = upper.extension,
                    matsize = matsize)
plot(vacant_mat$y,stable.stage(vacant_mat$IPMmat)[3:(matsize+2)])
ggplot(cactus)+
  geom_histogram(aes(x=logsize_t1))+facet_wrap("Year_t1")

stable_ants<-stable.stage(full_mat$IPMmat)
stable_ants<-stable_ants[-(1:2)]/sum(stable_ants[-(1:2)])
plot(full_mat$y,stable_ants[1:matsize],type="l",lwd=3,col=cremcol,ylim=c(0,.015))
lines(full_mat$y,stable_ants[(matsize+1):(2*matsize)],lwd=3,col=liomcol)
lines(full_mat$y,stable_ants[(2*matsize+1):(3*matsize)],lwd=3,col=othercol)
lines(full_mat$y,stable_ants[(3*matsize+1):(4*matsize)],lwd=3,col=vaccol)

sum(stable_ants[1:matsize])
sum(stable_ants[(matsize+1):(2*matsize)])
sum(stable_ants[(2*matsize+1):(3*matsize)])
sum(stable_ants[(3*matsize+1):(4*matsize)])
# looks good

cactus %>% 
  select(Year_t,ant_t) %>% drop_na() %>% 
  group_by(Year_t,ant_t) %>% 
  summarise(n=n()) %>%
  mutate(freq = n / sum(n)) -> ant_tally

# ant_tally %>% 
#   ggplot()+
#   geom_col(aes(ant_t,freq,fill=ant_t))+facet_wrap("Year_t")


## find how many years until lambdaS stabilizes
#these are the complete transition years that we want to sample from 2004:2023
trans_years<-c(1:3,10:15,18:19);(2004:2023)[trans_years]
## create a sequence of years using a fixed seed
max_yrs<-1000
set.seed(77005)
year_seq<-sample(trans_years,max_yrs,replace=T)
## subsample the year sequence at different lengths
years<-seq(100,1000,100)
vacant_lamS<-full_lamS<-c()
# for(i in 1:length(years)){
# vacant_lamS[i]<-lambdaSim(params = params_mean,
#           scenario = "none",
#           lower = cholla_min, upper = cholla_max, 
#           floor = lower.extension, ceiling = upper.extension, 
#           matsize = matsize,
#           grow_rfx1=colMeans(grow_rfx1),grow_rfx2=colMeans(grow_rfx2),
#           grow_rfx3=colMeans(grow_rfx3),grow_rfx4=colMeans(grow_rfx4),
#           surv_rfx1=colMeans(surv_rfx1),surv_rfx2=colMeans(surv_rfx2),
#           surv_rfx3=colMeans(surv_rfx3),surv_rfx4=colMeans(surv_rfx4),
#           flow_rfx=colMeans(flow_rfx),
#           repro_rfx= colMeans(repro_rfx),
#           viab_rfx1=colMeans(viab_rfx1),viab_rfx2=colMeans(viab_rfx2),
#           viab_rfx3=colMeans(viab_rfx3),viab_rfx4=colMeans(viab_rfx4),
#           max_yrs = years[i],
#           trans_years=year_seq[1:years[i]])
# print(i)}
# 
# plot(years,vacant_lamS,type="b")
# range(vacant_lamS)

## try all the scenarios with a shared sequence of years
sims <- c("comp","equal","freq")
lambdaS <- lambdaS_sync <- matrix(NA, nrow = length(sims))
for(n in 1:length(sims)){
  for(i in 1:length(scenario)){
    lambdaS[n,i] <- lambdaSim(params = params_mean,
                              scenario = scenario[i],
                              lower = cholla_min, upper = cholla_max, 
                              floor = lower.extension, ceiling = upper.extension, 
                              matsize = matsize,
                              grow_rfx1=colMeans(grow_rfx1),grow_rfx2=colMeans(grow_rfx2),
                              grow_rfx3=colMeans(grow_rfx3),grow_rfx4=colMeans(grow_rfx4),
                              surv_rfx1=colMeans(surv_rfx1),surv_rfx2=colMeans(surv_rfx2),
                              surv_rfx3=colMeans(surv_rfx3),surv_rfx4=colMeans(surv_rfx4),
                              flow_rfx=colMeans(flow_rfx),
                              repro_rfx= colMeans(repro_rfx),
                              viab_rfx1=colMeans(viab_rfx1),viab_rfx2=colMeans(viab_rfx2),
                              viab_rfx3=colMeans(viab_rfx3),viab_rfx4=colMeans(viab_rfx4),
                              trans_years=year_seq[1:500],simulation = sims[n])
    lambdaS_sync[n,i] <- lambdaSim(params = params_mean,
                              scenario = scenario[i],
                              lower = cholla_min, upper = cholla_max, 
                              floor = lower.extension, ceiling = upper.extension, 
                              matsize = matsize,
                              grow_rfx1=colMeans(grow_rfx),grow_rfx2=colMeans(grow_rfx),
                              grow_rfx3=colMeans(grow_rfx),grow_rfx4=colMeans(grow_rfx),
                              surv_rfx1=colMeans(surv_rfx),surv_rfx2=colMeans(surv_rfx),
                              surv_rfx3=colMeans(surv_rfx),surv_rfx4=colMeans(surv_rfx),
                              flow_rfx=colMeans(flow_rfx),
                              repro_rfx= colMeans(repro_rfx),
                              viab_rfx1=colMeans(viab_rfx),viab_rfx2=colMeans(viab_rfx),
                              viab_rfx3=colMeans(viab_rfx),viab_rfx4=colMeans(viab_rfx),
                              trans_years=year_seq[1:500], simulation = sims[n])
    print(scenario[i])
  }
}


pdf("Manuscript/Figures/lambdaS_freq_mean.pdf")
plot(c(1,3,5,7,9,11,13,15),lambdaS[3,], pch = 19, cex = 3,col = colors_lambdas,
     xlim = c(0,16), ylim = c(0.95,1),
     xaxt = "n",cex.lab = 2,
     xlab = "Ant Scenario", ylab = "Mean LambdaS", main = "")
points(c(1,3,5,7,9,11,13,15),lambdaS_sync[3,], pch = 1, cex = 3,col = colors_lambdas)
text(x = c(1,3,5,7,9,11,13,15)-0.2, y = lambdaS_sync[3,]+0.002,cex = 2, labels = scenario_abv)
legend("bottomright",legend=c("synchronized","non-synchronized"),pch=c(1,19),cex=1.5)
dev.off()

################################################################################
#### Determine how many years the stochastic model must run to be stable
################################################################################
## Competitive Exclusion
ipm_comp_100 <- c(0,0,0,0,0,0,0,0)
for(i in 1:length(scenario)){
  ipm_comp_100[i] <- lambdaSim(params = params_mean[m,],
                               scenario = scenario[i],
                               upper = upper, lower = lower, 
                               floor = floor, ceiling = ceiling, 
                               matsize = matsize,
                               grow_rfx1=colMeans(grow_rfx1),grow_rfx2=colMeans(grow_rfx2),
                               grow_rfx3=colMeans(grow_rfx3),grow_rfx4=colMeans(grow_rfx4),
                               surv_rfx1=colMeans(surv_rfx1),surv_rfx2=colMeans(surv_rfx2),
                               surv_rfx3=colMeans(surv_rfx3),surv_rfx4=colMeans(surv_rfx4),
                               flow_rfx=colMeans(flow_rfx),
                               repro_rfx= colMeans(repro_rfx),
                               viab_rfx1=colMeans(viab_rfx1),viab_rfx2=colMeans(viab_rfx2),
                               viab_rfx3=colMeans(viab_rfx3),viab_rfx4=colMeans(viab_rfx4),
                               max_yrs = 100  )
  
}
ipm_comp_100

ipm_comp_200 <- c(0,0,0,0,0,0,0,0)
for(i in 1:length(scenario)){
  ipm_comp_200[i] <- lambdaSim(params = params_mean[m,],
                               scenario = scenario[i],
                               upper = upper, lower = lower, 
                               floor = floor, ceiling = ceiling, 
                               matsize = matsize,
                               grow_rfx1=colMeans(grow_rfx1),grow_rfx2=colMeans(grow_rfx2),
                               grow_rfx3=colMeans(grow_rfx3),grow_rfx4=colMeans(grow_rfx4),
                               surv_rfx1=colMeans(surv_rfx1),surv_rfx2=colMeans(surv_rfx2),
                               surv_rfx3=colMeans(surv_rfx3),surv_rfx4=colMeans(surv_rfx4),
                               flow_rfx=colMeans(flow_rfx),
                               repro_rfx= colMeans(repro_rfx),
                               viab_rfx1=colMeans(viab_rfx1),viab_rfx2=colMeans(viab_rfx2),
                               viab_rfx3=colMeans(viab_rfx3),viab_rfx4=colMeans(viab_rfx4),
                               max_yrs = 200   )
  
}
ipm_comp_200

ipm_comp_300 <- c(0,0,0,0,0,0,0,0)
for(i in 1:length(scenario)){
  ipm_comp_300[i] <- lambdaSim(params = params_mean[m,],
                               scenario = scenario[i],
                               upper = upper, lower = lower,
                               floor = floor, ceiling = ceiling,
                               matsize = matsize,
                               grow_rfx1=colMeans(grow_rfx1),grow_rfx2=colMeans(grow_rfx2),
                               grow_rfx3=colMeans(grow_rfx3),grow_rfx4=colMeans(grow_rfx4),
                               surv_rfx1=colMeans(surv_rfx1),surv_rfx2=colMeans(surv_rfx2),
                               surv_rfx3=colMeans(surv_rfx3),surv_rfx4=colMeans(surv_rfx4),
                               flow_rfx=colMeans(flow_rfx),
                               repro_rfx= colMeans(repro_rfx),
                               viab_rfx1=colMeans(viab_rfx1),viab_rfx2=colMeans(viab_rfx2),
                               viab_rfx3=colMeans(viab_rfx3),viab_rfx4=colMeans(viab_rfx4),
                               max_yrs = 300   )
  
}
ipm_comp_300

# ipm_comp_700 <- c(0,0,0,0,0,0,0,0)
# for(i in 1:length(scenario)){
#   ipm_comp_700[i] <- lambdaSim(params = params_mean[m,],
#                                scenario = scenario[i],
#                                upper = upper, lower = lower, 
#                                floor = floor, ceiling = ceiling, 
#                                matsize = matsize,
#                                grow_rfx1=colMeans(grow_rfx1),grow_rfx2=colMeans(grow_rfx2),
#                                grow_rfx3=colMeans(grow_rfx3),grow_rfx4=colMeans(grow_rfx4),
#                                surv_rfx1=colMeans(surv_rfx1),surv_rfx2=colMeans(surv_rfx2),
#                                surv_rfx3=colMeans(surv_rfx3),surv_rfx4=colMeans(surv_rfx4),
#                                flow_rfx=colMeans(flow_rfx),
#                                repro_rfx= colMeans(repro_rfx),
#                                viab_rfx1=colMeans(viab_rfx1),viab_rfx2=colMeans(viab_rfx2),
#                                viab_rfx3=colMeans(viab_rfx3),viab_rfx4=colMeans(viab_rfx4),
#                                max_yrs = 700   )
#   
# }
# ipm_comp_700
# 
# ipm_comp_900 <- c(0,0,0,0,0,0,0,0)
# for(i in 1:length(scenario)){
#   ipm_comp_900[i] <- lambdaSim(params = params_mean[m,],
#                                scenario = scenario[i],
#                                upper = upper, lower = lower, 
#                                floor = floor, ceiling = ceiling, 
#                                matsize = matsize,
#                                grow_rfx1=colMeans(grow_rfx1),grow_rfx2=colMeans(grow_rfx2),
#                                grow_rfx3=colMeans(grow_rfx3),grow_rfx4=colMeans(grow_rfx4),
#                                surv_rfx1=colMeans(surv_rfx1),surv_rfx2=colMeans(surv_rfx2),
#                                surv_rfx3=colMeans(surv_rfx3),surv_rfx4=colMeans(surv_rfx4),
#                                flow_rfx=colMeans(flow_rfx),
#                                repro_rfx= colMeans(repro_rfx),
#                                viab_rfx1=colMeans(viab_rfx1),viab_rfx2=colMeans(viab_rfx2),
#                                viab_rfx3=colMeans(viab_rfx3),viab_rfx4=colMeans(viab_rfx4),
#                                max_yrs = 900   )
#   
# }
# ipm_comp_900
# 
# ipm_comp_1000 <- c(0,0,0,0,0,0,0,0)
# for(i in 1:length(scenario)){
#   ipm_comp_1000[i] <- lambdaSim(params = params_mean[m,],
#                                scenario = scenario[i],
#                                upper = upper, lower = lower, 
#                                floor = floor, ceiling = ceiling, 
#                                matsize = matsize,
#                                grow_rfx1=colMeans(grow_rfx1),grow_rfx2=colMeans(grow_rfx2),
#                                grow_rfx3=colMeans(grow_rfx3),grow_rfx4=colMeans(grow_rfx4),
#                                surv_rfx1=colMeans(surv_rfx1),surv_rfx2=colMeans(surv_rfx2),
#                                surv_rfx3=colMeans(surv_rfx3),surv_rfx4=colMeans(surv_rfx4),
#                                flow_rfx=colMeans(flow_rfx),
#                                repro_rfx= colMeans(repro_rfx),
#                                viab_rfx1=colMeans(viab_rfx1),viab_rfx2=colMeans(viab_rfx2),
#                                viab_rfx3=colMeans(viab_rfx3),viab_rfx4=colMeans(viab_rfx4),
#                                max_yrs = 1000   )
#   
# }
# ipm_comp_1000

## Plot the different runs against each other to determine how many years need to run to converge
png("Figures/Convergence_Comp.png")
plot(x = 1:8, y = ipm_det_comp, col = "red", cex = 3, pch = 16, ylim = c(0.91, 0.99))
lines(x = 1:8, y = ipm_comp_100, col = "orange", lwd = 3)
lines(x = 1:8, y = ipm_comp_200, col = "yellow", lwd = 3)
lines(x = 1:8, y = ipm_comp_300, col = "green", lwd = 3)
# lines(x = 1:8, y = ipm_comp_700, col = "blue", lwd = 3)
# lines(x = 1:8, y = ipm_comp_900, col = "purple", lwd = 3)
# lines(x = 1:8, y = ipm_comp_1000, col = "pink", lwd = 3)
legend("bottomright", legend = c("deterministic","100 yrs","200 yrs","500 yrs","700 yrs","900 yrs","1000 yrs"
), fill = c("red","orange","yellow","green","blue","purple","pink"))
dev.off()
## I would say 300 - 500 

## Frequency Based 
ipm_freq_100 <- c(0,0,0,0,0,0,0,0)
for(i in 1:length(scenario)){
  ipm_freq_100[i] <- lambdaSim(params = params_mean[m,],
                               scenario = scenario[i],
                               upper = upper, lower = lower, 
                               floor = floor, ceiling = ceiling, 
                               matsize = matsize,
                               grow_rfx1=colMeans(grow_rfx1),grow_rfx2=colMeans(grow_rfx2),
                               grow_rfx3=colMeans(grow_rfx3),grow_rfx4=colMeans(grow_rfx4),
                               surv_rfx1=colMeans(surv_rfx1),surv_rfx2=colMeans(surv_rfx2),
                               surv_rfx3=colMeans(surv_rfx3),surv_rfx4=colMeans(surv_rfx4),
                               flow_rfx=colMeans(flow_rfx),
                               repro_rfx= colMeans(repro_rfx),
                               viab_rfx1=colMeans(viab_rfx1),viab_rfx2=colMeans(viab_rfx2),
                               viab_rfx3=colMeans(viab_rfx3),viab_rfx4=colMeans(viab_rfx4),
                               max_yrs = 100  )
  
}
ipm_freq_100

ipm_freq_200 <- c(0,0,0,0,0,0,0,0)
for(i in 1:length(scenario)){
  ipm_freq_200[i] <- lambdaSim(params = params_mean[m,],
                               scenario = scenario[i],
                               upper = upper, lower = lower, 
                               floor = floor, ceiling = ceiling, 
                               matsize = matsize,
                               grow_rfx1=colMeans(grow_rfx1),grow_rfx2=colMeans(grow_rfx2),
                               grow_rfx3=colMeans(grow_rfx3),grow_rfx4=colMeans(grow_rfx4),
                               surv_rfx1=colMeans(surv_rfx1),surv_rfx2=colMeans(surv_rfx2),
                               surv_rfx3=colMeans(surv_rfx3),surv_rfx4=colMeans(surv_rfx4),
                               flow_rfx=colMeans(flow_rfx),
                               repro_rfx= colMeans(repro_rfx),
                               viab_rfx1=colMeans(viab_rfx1),viab_rfx2=colMeans(viab_rfx2),
                               viab_rfx3=colMeans(viab_rfx3),viab_rfx4=colMeans(viab_rfx4),
                               max_yrs = 200   )
  
}
ipm_freq_200

################################################################################
#### IPM Full Runs
################################################################################
# outputs: a list where each element is corresponding to a simulation
# and each element is a matrix of n rows and m columns. N is the 
# number of included iterations and m is the number of ant scenarios

lambda_list <- lambda_sync_list <- list()
lambdaS_full <- lambdaS_sync_full <- matrix(NA, nrow = iter, ncol = length(scenario))
for(i in 1:length(sims)){ # each model simulation (comp, equal, freq)
  for(m in 1:length(scenario)){ # each ant combination
    for(n in 1:iter){ # each iteration
      lambdaS_full[n,m] <- lambdaSim(params = params[n,],
                                scenario = scenario[m],
                                lower = cholla_min, upper = cholla_max, 
                                floor = lower.extension, ceiling = upper.extension, 
                                matsize = matsize,
                                grow_rfx1=(grow_rfx1[n,]),grow_rfx2=(grow_rfx2[n,]),
                                grow_rfx3=(grow_rfx3[n,]),grow_rfx4=(grow_rfx4[n,]),
                                surv_rfx1=(surv_rfx1[n,]),surv_rfx2=(surv_rfx2[n,]),
                                surv_rfx3=(surv_rfx3[n,]),surv_rfx4=(surv_rfx4[n,]),
                                flow_rfx=(flow_rfx[n,]),
                                repro_rfx= (repro_rfx[n,]),
                                viab_rfx1=(viab_rfx1[n,]),viab_rfx2=(viab_rfx2[n,]),
                                viab_rfx3=(viab_rfx3[n,]),viab_rfx4=(viab_rfx4[n,]),
                                trans_years=year_seq[1:500],simulation = sims[i])
      lambdaS_sync_full[n,m] <- lambdaSim(params = params[n,],
                                scenario = scenario[m],
                                lower = cholla_min, upper = cholla_max, 
                                floor = lower.extension, ceiling = upper.extension, 
                                matsize = matsize,
                                grow_rfx1=(grow_rfx[n,]),grow_rfx2=(grow_rfx[n,]),
                                grow_rfx3=(grow_rfx[n,]),grow_rfx4=(grow_rfx[n,]),
                                surv_rfx1=(surv_rfx[n,]),surv_rfx2=(surv_rfx[n,]),
                                surv_rfx3=(surv_rfx[n,]),surv_rfx4=(surv_rfx[n,]),
                                flow_rfx=(flow_rfx[n,]),
                                repro_rfx= (repro_rfx[n,]),
                                viab_rfx1=(viab_rfx[n,]),viab_rfx2=(viab_rfx[n,]),
                                viab_rfx3=(viab_rfx[n,]),viab_rfx4=(viab_rfx[n,]),
                                trans_years=year_seq[1:500], simulation = sims[i])
     
    } # end of iteration
    print(scenario[m])
  } # end ant combos
  lambda_list[[i]] <- lambdaS_full
  lambda_sync_list[[i]] <- lambdaS_sync_full
} # end simulations

lambda_list
lambda_sync_list
# Pull out the non-synchronous values
write.csv(lambda_list[[1]],"Model Outputs/lambda_comp.csv")
write.csv(lambda_list[[2]],"Model Outputs/lambda_equal.csv")
write.csv(lambda_list[[3]],"Model Outputs/lambda_freq.csv")
# pull out synchronous values
write.csv(lambda_sync_list[[1]],"Model Outputs/lambda_sync_comp.csv")
write.csv(lambda_sync_list[[2]],"Model Outputs/lambda_sync_equal.csv")
write.csv(lambda_sync_list[[3]],"Model Outputs/lambda_sync_freq.csv")

################################################################################
#### Check the stable stage distribution of sizes
################################################################################
# Pull out the mean matrix
params_mean <- matrix(rep(0,ncol(params)), nrow = 1)
for(i in 1:ncol(params)){
  params_mean[,i] <- mean(params[,i])
}
colMeans(params)
colnames(params_mean) <- colnames(params)
params_mean <- as.data.frame(params_mean)

scenario = c("none","cremvac","liomvac","othervac","liomcremvac","liomvacother","othercremvac","all")
m = 1
ipm_comp_n <- data.frame(None = "",Cremvac = "", Liomvac = "", Othervac = "",
                         Liomcremvac = "", Liomvacother = "", Othercremvac = "", All = "")
ipm_comp_n <- c()
for(i in 1:length(scenario)){
  ipm_comp_n[i] <-  lambda(bigmatrix(params = (params_mean[m,]),
            scenario = scenario[i],
            lower = cholla_min - 5,
            upper = cholla_max + 5,
            floor = floor,
            ceiling = ceiling,
            matsize = matsize,
            mean(grow_rfx1),mean(grow_rfx2),mean(grow_rfx3),mean(grow_rfx4),
            mean(surv_rfx1),mean(surv_rfx2),mean(surv_rfx3),mean(surv_rfx4),
            mean(flow_rfx),
            mean(repro_rfx),
            mean(viab_rfx1),mean(viab_rfx2),mean(viab_rfx3),mean(viab_rfx4))$IPMmat)
}

########## Plot the means of the lambda distributions
scenario_abv <- c("V","C","L","O","LC","LO","OC","LOC")
x_vals <- c(1,3,5,7,9,11,13,15)
lambdas <- ipm_comp_n
colors_lambdas <- c(vcol,ccol,lcol,ocol,lccol,locol,cocol,acol)
png("Manuscript/Figures/comp_parammeans.png")
plot(c(1,3,5,7,9,11,13,15),lambdas, pch = 19, cex = 3,col = colors_lambdas,
     xlim = c(0,16), ylim = c(0.930,0.98),
     xaxt = "n",cex.lab = 2,
     xlab = "Ant Scenario", ylab = "Mean Lambda Value", main = "")
text(x = c(1,3,5,7,9,11,13,15)-0.2, y = lambdas+0.002,cex = 2, labels = scenario_abv)
dev.off()
# 
# ipm_equal_n <- c()
# for(i in 1:length(scenario)){
#   ipm_equal_n[i] <-  lambda(bigmatrix(params = (params_mean[m,]),
#                                      scenario = scenario[i],
#                                      lower = cholla_min - 5,
#                                      upper = cholla_max + 5,
#                                      floor = floor,
#                                      ceiling = ceiling,
#                                      matsize = matsize,
#                                      mean(grow_rfx1),mean(grow_rfx2),mean(grow_rfx3),mean(grow_rfx4),
#                                      mean(surv_rfx1),mean(surv_rfx2),mean(surv_rfx3),mean(surv_rfx4),
#                                      mean(flow_rfx),
#                                      mean(repro_rfx),
#                                      mean(viab_rfx1),mean(viab_rfx2),mean(viab_rfx3),mean(viab_rfx4))$IPMmat)
# }

########## Plot the means of the lambda distributions
scenario_abv <- c("V","C","L","O","LC","LO","OC","LOC")
x_vals <- c(1,3,5,7,9,11,13,15)
lambdas <- ipm_equal_n
colors_lambdas <- c(vcol,ccol,lcol,ocol,lccol,locol,cocol,acol)
png("Manuscript/Figures/equal_parammeans.png")
plot(c(1,3,5,7,9,11,13,15),lambdas, pch = 19, cex = 3,col = colors_lambdas,
     xlim = c(0,16), ylim = c(0.930,0.98),
     xaxt = "n",cex.lab = 2,
     xlab = "Ant Scenario", ylab = "Mean Lambda Value", main = "")
text(x = c(1,3,5,7,9,11,13,15)-0.2, y = lambdas+0.002,cex = 2, labels = scenario_abv)
dev.off()
# 
# ipm_freq_n <- c()
# for(i in 1:length(scenario)){
#   ipm_freq_n[i] <-  lambda(bigmatrix(params = (params_mean[m,]),
#                                       scenario = scenario[i],
#                                       lower = cholla_min - 5,
#                                       upper = cholla_max + 5,
#                                       floor = floor,
#                                       ceiling = ceiling,
#                                       matsize = matsize,
#                                       mean(grow_rfx1),mean(grow_rfx2),mean(grow_rfx3),mean(grow_rfx4),
#                                       mean(surv_rfx1),mean(surv_rfx2),mean(surv_rfx3),mean(surv_rfx4),
#                                       mean(flow_rfx),
#                                       mean(repro_rfx),
#                                       mean(viab_rfx1),mean(viab_rfx2),mean(viab_rfx3),mean(viab_rfx4))$IPMmat)
# }

########## Plot the means of the lambda distributions
scenario_abv <- c("V","C","L","O","LC","LO","OC","LOC")
x_vals <- c(1,3,5,7,9,11,13,15)
lambdas <- ipm_freq_n
colors_lambdas <- c(vcol,ccol,lcol,ocol,lccol,locol,cocol,acol)
png("Manuscript/Figures/freq_parammeans.png")
plot(c(1,3,5,7,9,11,13,15),lambdas, pch = 19, cex = 3,col = colors_lambdas,
     xlim = c(0,16), ylim = c(0.930,0.98),
     xaxt = "n",cex.lab = 2,
     xlab = "Ant Scenario", ylab = "Mean Lambda Value", main = "")
text(x = c(1,3,5,7,9,11,13,15)-0.2, y = lambdas+0.002,cex = 2, labels = scenario_abv)
#legend("topleft",legend = scenario_abv,fill = colors_lambdas)
mtext("Diversity Scenario",side=1,line=-1.5,outer=TRUE,cex=1.5)
mtext("Lambda",side=2,line=-1.5,outer=TRUE,cex=1.5,las=0)
dev.off()








# m = 1
# all_IPM <- bigmatrix(params = (params_mean[m,]),
#                      scenario = "all",
#                      lower = cholla_min - 5,
#                      upper = cholla_max + 5,
#                      floor = floor,
#                      ceiling = ceiling,
#                      matsize = matsize,
#                      mean(grow_rfx1),mean(grow_rfx2),mean(grow_rfx3),mean(grow_rfx4),
#                      mean(surv_rfx1),mean(surv_rfx2),mean(surv_rfx3),mean(surv_rfx4),
#                      mean(flow_rfx),
#                      mean(repro_rfx),
#                      mean(viab_rfx1),mean(viab_rfx2),mean(viab_rfx3),mean(viab_rfx4))
# ##View(all_IPM$Tmat)
# # Pull out the exact size distribution from the matrix above
# all_y <- all_IPM$y
# ## Check that the lambda makes sense before going any further
# all_mean <- lambda(all_IPM$IPMmat) # Looks correct!
# # Pull out the IPM from the matrix above
# # all_stable <- stable.stage(all_IPM$IPMmat)
# # # Remove the seed banks
# # all_stable_cut <- all_stable[3:2002]
# # # Standardize -- NOTE THIS DOES NOT APPEAR TO HAVE ANY AFFECT ON THE PLOT
# # all_stable_stan <- all_stable_cut/sum(all_stable_cut)
# # sum(all_stable_stan)
# # png("Figures/stable_all_test.png")
# # plot(all_y,(all_stable_stan[1:500]/sum(all_stable_stan)), col = cremcol, type = "l", xlim = c(-5,15), ylim = c(0,0.015))
# # lines(all_y, all_stable_stan[501:1000]/sum(all_stable_stan), col = liomcol)
# # lines(all_y, all_stable_stan[1001:1500]/sum(all_stable_stan), col = othercol)
# # lines(all_y, all_stable_stan[1501:2000]/sum(all_stable_stan), col = vaccol)
# # legend("topleft", legend = c("Crem.","Liom.","Other","Vac."), fill = c(cremcol,liomcol,othercol,vaccol))
# # dev.off()
# # 
# # png("Figures/stable_all_sum.png")
# # plot(all_y, (all_stable_stan[1:500]+all_stable_stan[501:1000]+ all_stable_stan[1001:1500]+ all_stable_stan[1501:2000]))
# # dev.off()
# 
# ########## Vacant scenario
# vac_IPM <- bigmatrix(params = (params[m,]),
#                      scenario = "none",
#                      lower = lower,
#                      upper = upper,
#                      floor = floor,
#                      ceiling = ceiling,
#                      matsize = matsize,
#                      mean(grow_rfx1),mean(grow_rfx2),mean(grow_rfx3),mean(grow_rfx4),
#                      mean(surv_rfx1),mean(surv_rfx2),mean(surv_rfx3),mean(surv_rfx4),
#                      mean(flow_rfx),
#                      mean(repro_rfx),
#                      mean(viab_rfx1),mean(viab_rfx2),mean(viab_rfx3),mean(viab_rfx4))
# # Pull out the exact size distribution from the matrix above
# vac_y <- vac_IPM$y
# ## Check that the lambda makes sense before going any further
# vac_mean <- lambda(vac_IPM$IPMmat) # Looks correct!
# # Pull out the IPM from the matrix above
# # vac_stable <- stable.stage(vac_IPM$IPMmat)
# # # Remove the seed banks
# # vac_stable_cut <- vac_stable[3:502]
# # # Standardize -- NOTE THIS DOES NOT APPEAR TO HAVE ANY AFFECT ON THE PLOT
# # vac_stable_stan <- vac_stable_cut/sum(vac_stable_cut)
# # sum(vac_stable_stan)
# # png("Figures/stable_vac_test.png")
# # plot(vac_IPM$y,vac_stable_stan, type= "l", col = vaccol)
# # legend("topleft", legend = c("Crem.","Liom.","Other","Vac."), fill = c(cremcol,liomcol,othercol,vaccol))
# # abline(v = lower, col = "black", lty = 2)
# # abline(v = upper, col = "black", lty = 2)
# # dev.off()
# 
# ########## Liom and Vac Scenario
# liomvac_IPM <- bigmatrix(params = (params[m,]),
#                          scenario = "liomvac",
#                          lower = lower,
#                          upper = upper,
#                          floor = floor,
#                          ceiling = ceiling,
#                          matsize = matsize,
#                          mean(grow_rfx1),mean(grow_rfx2),mean(grow_rfx3),mean(grow_rfx4),
#                          mean(surv_rfx1),mean(surv_rfx2),mean(surv_rfx3),mean(surv_rfx4),
#                          mean(flow_rfx),
#                          mean(repro_rfx),
#                          mean(viab_rfx1),mean(viab_rfx2),mean(viab_rfx3),mean(viab_rfx4))
# # Pull out the exact size distribution from the matrix above
# liomvac_y <- liomvac_IPM$y
# ## Check that the lambda makes sense before going any further
# liom_mean <- lambda(liomvac_IPM$IPMmat) # Looks correct!
# # Pull out the IPM from the matrix above
# # liomvac_stable <- stable.stage(liomvac_IPM$IPMmat)
# # # Remove the seed banks
# # liomvac_stable_cut <- liomvac_stable[3:1002]
# # # Standardize -- NOTE THIS DOES NOT APPEAR TO HAVE ANY AFFECT ON THE PLOT
# # liomvac_stable_stan <- liomvac_stable_cut/sum(liomvac_stable_cut)
# # sum(liomvac_stable_stan)
# # png("Figures/stable_liomvac_test.png")
# # plot(liomvac_IPM$y,liomvac_stable_stan[1:500], type= "l", xlim = c(-5,15), col = liomcol, ylim = c(0,0.02))
# # lines(liomvac_IPM$y, liomvac_stable_stan[501:1000], col = vaccol)
# # legend("topleft", legend = c("Crem.","Liom.","Other","Vac."), fill = c(cremcol,liomcol,othercol,vaccol))
# # dev.off()
# 
# ########## Other Vacant Scenario
# other_IPM <- bigmatrix(params = (params_mean[m,]),
#                        scenario = "othervac",
#                        lower = cholla_min - 5,
#                        upper = cholla_max + 5,
#                        floor = floor,
#                        ceiling = ceiling,
#                        matsize = matsize,
#                        mean(grow_rfx1),mean(grow_rfx2),mean(grow_rfx3),mean(grow_rfx4),
#                        mean(surv_rfx1),mean(surv_rfx2),mean(surv_rfx3),mean(surv_rfx4),
#                        mean(flow_rfx),
#                        mean(repro_rfx),
#                        mean(viab_rfx1),mean(viab_rfx2),mean(viab_rfx3),mean(viab_rfx4))
# ##View(all_IPM$Tmat)
# # Pull out the exact size distribution from the matrix above
# other_y <- other_IPM$y
# ## Check that the lambda makes sense before going any further
# other_mean <- lambda(other_IPM$IPMmat) # Looks correct!
# 
# ########## Crem Vacant Scenario
# crem_IPM <- bigmatrix(params = (params_mean[m,]),
#                       scenario = "cremvac",
#                       lower = cholla_min - 5,
#                       upper = cholla_max + 5,
#                       floor = floor,
#                       ceiling = ceiling,
#                       matsize = matsize,
#                       mean(grow_rfx1),mean(grow_rfx2),mean(grow_rfx3),mean(grow_rfx4),
#                       mean(surv_rfx1),mean(surv_rfx2),mean(surv_rfx3),mean(surv_rfx4),
#                       mean(flow_rfx),
#                       mean(repro_rfx),
#                       mean(viab_rfx1),mean(viab_rfx2),mean(viab_rfx3),mean(viab_rfx4))
# ##View(all_IPM$Tmat)
# # Pull out the exact size distribution from the matrix above
# crem_y <- crem_IPM$y
# ## Check that the lambda makes sense before going any further
# crem_mean <- lambda(crem_IPM$IPMmat) # Looks correct!
# 
# ########## Liom Crem Vac Scenario
# liomcrem_IPM <- bigmatrix(params = (params_mean[m,]),
#                           scenario = "liomcremvac",
#                           lower = cholla_min - 5,
#                           upper = cholla_max + 5,
#                           floor = floor,
#                           ceiling = ceiling,
#                           matsize = matsize,
#                           mean(grow_rfx1),mean(grow_rfx2),mean(grow_rfx3),mean(grow_rfx4),
#                           mean(surv_rfx1),mean(surv_rfx2),mean(surv_rfx3),mean(surv_rfx4),
#                           mean(flow_rfx),
#                           mean(repro_rfx),
#                           mean(viab_rfx1),mean(viab_rfx2),mean(viab_rfx3),mean(viab_rfx4))
# ##View(all_IPM$Tmat)
# # Pull out the exact size distribution from the matrix above
# liomcrem_y <- liomcrem_IPM$y
# ## Check that the lambda makes sense before going any further
# liomcrem_mean <- lambda(liomcrem_IPM$IPMmat) # Looks correct!
# 
# ########## Liom Other Vac Scenario
# liomother_IPM <- bigmatrix(params = (params_mean[m,]),
#                           scenario = "liomvacother",
#                           lower = cholla_min - 5,
#                           upper = cholla_max + 5,
#                           floor = floor,
#                           ceiling = ceiling,
#                           matsize = matsize,
#                           mean(grow_rfx1),mean(grow_rfx2),mean(grow_rfx3),mean(grow_rfx4),
#                           mean(surv_rfx1),mean(surv_rfx2),mean(surv_rfx3),mean(surv_rfx4),
#                           mean(flow_rfx),
#                           mean(repro_rfx),
#                           mean(viab_rfx1),mean(viab_rfx2),mean(viab_rfx3),mean(viab_rfx4))
# ##View(all_IPM$Tmat)
# # Pull out the exact size distribution from the matrix above
# liomother_y <- liomother_IPM$y
# ## Check that the lambda makes sense before going any further
# liomother_mean <- lambda(liomother_IPM$IPMmat) # Looks correct!
# 
# ########## Other Crem Vac Scenario
# othercrem_IPM <- bigmatrix(params = (params_mean[m,]),
#                                             scenario = "othercremvac",
#                                             lower = cholla_min - 5,
#                                             upper = cholla_max + 5,
#                                             floor = floor,
#                                             ceiling = ceiling,
#                                             matsize = matsize,
#                                             mean(grow_rfx1),mean(grow_rfx2),mean(grow_rfx3),mean(grow_rfx4),
#                                             mean(surv_rfx1),mean(surv_rfx2),mean(surv_rfx3),mean(surv_rfx4),
#                                             mean(flow_rfx),
#                                             mean(repro_rfx),
#                                             mean(viab_rfx1),mean(viab_rfx2),mean(viab_rfx3),mean(viab_rfx4))
# ##View(all_IPM$Tmat)
# # Pull out the exact size distribution from the matrix above
# othercrem_y <- othercrem_IPM$y
# ## Check that the lambda makes sense before going any further
# othercrem_mean <- lambda(othercrem_IPM$IPMmat) # Looks correct!




################################################################################
################################################################################
######## SHOW THAT THE PORTFOLIO EFFECT IS POSSIBLE ############################
################################################################################
################################################################################
################################################################################
#### Mean Regular Model
################################################################################
lambdaS_full <- lambdaS_sync_full <- matrix(NA, nrow = 1, ncol = length(scenario))
for(m in 1:length(scenario)){ # each ant combination
  # nonsync
  lambdaS_full[1,m] <- lambdaSim(params = params_mean,
                                 scenario = scenario[m],
                                 lower = cholla_min, upper = cholla_max, 
                                 floor = lower.extension, ceiling = upper.extension, 
                                 matsize = matsize,
                                 grow_rfx1=colMeans(grow_rfx1),grow_rfx2=colMeans(grow_rfx2),
                                 grow_rfx3=colMeans(grow_rfx3),grow_rfx4=colMeans(grow_rfx4),
                                 surv_rfx1=colMeans(surv_rfx1),surv_rfx2=colMeans(surv_rfx2),
                                 surv_rfx3=colMeans(surv_rfx3),surv_rfx4=colMeans(surv_rfx4),
                                 flow_rfx=colMeans(flow_rfx),
                                 repro_rfx=colMeans(repro_rfx),
                                 viab_rfx1=colMeans(viab_rfx1),viab_rfx2=colMeans(viab_rfx2),
                                 viab_rfx3=colMeans(viab_rfx3),viab_rfx4=colMeans(viab_rfx4),
                                 trans_years=year_seq[1:5],simulation = sims[1])
  # sync
  lambdaS_sync_full[1,m] <- lambdaSim(params = params_mean,
                                      scenario = scenario[m],
                                      lower = cholla_min, upper = cholla_max,
                                      floor = lower.extension, ceiling = upper.extension,
                                      matsize = matsize,
                                      grow_rfx1=colMeans(grow_rfx),grow_rfx2=colMeans(grow_rfx),
                                      grow_rfx3=colMeans(grow_rfx),grow_rfx4=colMeans(grow_rfx),
                                      surv_rfx1=colMeans(surv_rfx),surv_rfx2=colMeans(surv_rfx),
                                      surv_rfx3=colMeans(surv_rfx),surv_rfx4=colMeans(surv_rfx),
                                      flow_rfx=colMeans(flow_rfx),
                                      repro_rfx=colMeans(repro_rfx),
                                      viab_rfx1=colMeans(viab_rfx),viab_rfx2=colMeans(viab_rfx),
                                      viab_rfx3=colMeans(viab_rfx),viab_rfx4=colMeans(viab_rfx),
                                      trans_years=year_seq[1:5], simulation = sims[1])
  print(scenario[m])
} # end ant combos
lambdaS_full
lambdaS_sync_full
# Pull out the non-synchronous values
write.csv(lambdaS_full,"Model Outputs/mean_lambda_comp.csv")
# pull out synchronous values
write.csv(lambdaS_sync_full,"Model Outputs/mean_lambda_sync_comp.csv")

################################################################################
#### Make Correlation as Close to 0 as Possible
################################################################################
yrs <- c(2004:2023)
## Growth RFX cor -> 0 ####
grow_rfx1_0 <- cbind(grow.params$w[draws,1,5],grow.params$w[draws,1,2],grow.params$w[draws,1,3],
                   rep(NA,N_draws),rep(NA,N_draws), ##2007,2008
                   grow.params$w[draws,1,6],grow.params$w[draws,1,8],grow.params$w[draws,1,7],
                   grow.params$w[draws,1,1],
                   grow.params$w[draws,1,4],grow.params$w[draws,1,12],grow.params$w[draws,1,11],
                   grow.params$w[draws,1,10],
                   grow.params$w[draws,1,9],grow.params$w[draws,1,13],
                   rep(NA,N_draws),rep(NA,N_draws), ##2019,2020
                   grow.params$w[draws,1,14],grow.params$w[draws,1,15],
                   rep(NA,N_draws))##2023
# Ant 2 (prev liom)
grow_rfx2_0 <- cbind(grow.params$w[draws,2,2],grow.params$w[draws,2,3],grow.params$w[draws,2,4],
                   rep(NA,N_draws),rep(NA,N_draws), ##2007,2008
                   grow.params$w[draws,2,5],grow.params$w[draws,2,1],grow.params$w[draws,2,6],grow.params$w[draws,2,7],
                   grow.params$w[draws,2,8],grow.params$w[draws,2,9],grow.params$w[draws,2,10],grow.params$w[draws,2,11],
                   grow.params$w[draws,2,12],grow.params$w[draws,2,13],
                   rep(NA,N_draws),rep(NA,N_draws), ##2019,2020
                   grow.params$w[draws,2,14],grow.params$w[draws,2,15],
                   rep(NA,N_draws))##2023
# Ant 3 (prev other)
grow_rfx3_0 <- cbind(grow.params$w[draws,3,1],grow.params$w[draws,3,2],grow.params$w[draws,3,3],
                   rep(NA,N_draws),rep(NA,N_draws), ##2007,2008
                   grow.params$w[draws,3,4],grow.params$w[draws,3,6],grow.params$w[draws,3,13],
                   grow.params$w[draws,3,7],
                   grow.params$w[draws,3,11],grow.params$w[draws,3,8],grow.params$w[draws,3,10],
                   grow.params$w[draws,3,9],
                   grow.params$w[draws,3,12],grow.params$w[draws,3,5],
                   rep(NA,N_draws),rep(NA,N_draws), ##2019,2020
                   grow.params$w[draws,3,14],grow.params$w[draws,3,15],
                   rep(NA,N_draws))##2023
# Ant 4 (prev vac)
grow_rfx4_0 <- cbind(grow.params$w[draws,4,6],grow.params$w[draws,4,3],grow.params$w[draws,4,10],
                   rep(NA,N_draws),rep(NA,N_draws), ##2007,2008
                   grow.params$w[draws,4,11],grow.params$w[draws,4,7],grow.params$w[draws,4,9],grow.params$w[draws,4,8],grow.params$w[draws,4,15],grow.params$w[draws,4,5],
                   grow.params$w[draws,4,4],
                   grow.params$w[draws,4,1],
                   grow.params$w[draws,4,13],grow.params$w[draws,4,12],
                   rep(NA,N_draws),rep(NA,N_draws), ##2019,2020
                   grow.params$w[draws,4,2],grow.params$w[draws,4,4],
                   rep(NA,N_draws))##2023
plot(x = yrs,y = colMeans(grow_rfx1_0), col = cremcol,
     type = "b",
     ylim = c(-.5,.5))
lines(x = yrs, y = colMeans(grow_rfx2_0), col = liomcol,type = "b")
lines(x = yrs, y = colMeans(grow_rfx3_0), col = othercol,type = "b")
lines(x = yrs, y = colMeans(grow_rfx4_0), col = vaccol,type = "b")
## Survival RFX cor -> 0 ###########
surv_rfx1 <- cbind(surv.params$w[draws,1,1],surv.params$w[draws,1,2],surv.params$w[draws,1,3],
                   rep(NA,N_draws),rep(NA,N_draws),
                   surv.params$w[draws,1,4],surv.params$w[draws,1,5],surv.params$w[draws,1,6],
                   surv.params$w[draws,1,7],
                   surv.params$w[draws,1,8],surv.params$w[draws,1,9],surv.params$w[draws,1,10],
                   surv.params$w[draws,1,11],
                   surv.params$w[draws,1,12],surv.params$w[draws,1,13],
                   rep(NA,N_draws),rep(NA,N_draws),
                   surv.params$w[draws,1,14],surv.params$w[draws,1,15],
                   rep(NA,N_draws))#2023
# Ant 2 (prev liom)
surv_rfx2 <- cbind(surv.params$w[draws,2,1],surv.params$w[draws,2,2],surv.params$w[draws,2,3],
                   rep(NA,N_draws),rep(NA,N_draws),
                   surv.params$w[draws,2,4],surv.params$w[draws,2,5],surv.params$w[draws,2,6],
                   surv.params$w[draws,2,7],
                   surv.params$w[draws,2,8],surv.params$w[draws,2,9],surv.params$w[draws,2,10],
                   surv.params$w[draws,2,11],
                   surv.params$w[draws,2,12],surv.params$w[draws,2,13],
                   rep(NA,N_draws),rep(NA,N_draws),
                   surv.params$w[draws,2,14],surv.params$w[draws,2,15],
                   rep(NA,N_draws))#2023
# Ant 3 (prev other)
surv_rfx3 <- cbind(surv.params$w[draws,3,1],surv.params$w[draws,3,2],surv.params$w[draws,3,3],
                   rep(NA,N_draws),rep(NA,N_draws),
                   surv.params$w[draws,3,4],surv.params$w[draws,3,5],surv.params$w[draws,3,6],
                   surv.params$w[draws,3,7],
                   surv.params$w[draws,3,8],surv.params$w[draws,3,9],surv.params$w[draws,3,10],
                   surv.params$w[draws,3,11],
                   surv.params$w[draws,3,12],surv.params$w[draws,3,13],
                   rep(NA,N_draws),rep(NA,N_draws),
                   surv.params$w[draws,3,14],surv.params$w[draws,3,15],
                   rep(NA,N_draws))#2023
# Ant 4 (prev vac)
surv_rfx4 <- cbind(surv.params$w[draws,4,1],surv.params$w[draws,4,2],surv.params$w[draws,4,3],
                   rep(NA,N_draws),rep(NA,N_draws),
                   surv.params$w[draws,4,4],surv.params$w[draws,4,7],surv.params$w[draws,4,10],
                   surv.params$w[draws,4,11],
                   surv.params$w[draws,4,8],surv.params$w[draws,4,9],surv.params$w[draws,4,6],
                   surv.params$w[draws,4,5],
                   surv.params$w[draws,4,12],surv.params$w[draws,4,13],
                   rep(NA,N_draws),rep(NA,N_draws),
                   surv.params$w[draws,4,14],surv.params$w[draws,4,15],
                   rep(NA,N_draws))#2023
plot(x = yrs,y = colMeans(surv_rfx4), col = vaccol,
     type = "b",
     ylim = c(-1.2,1))
lines(x = yrs, y = colMeans(surv_rfx2), col = liomcol,type = "b")
#lines(x = yrs, y = colMeans(surv_rfx3), col = othercol,type = "b")
#lines(x = yrs, y = colMeans(surv_rfx1), col = vaccol,type = "b")
## Viability  RFX cor -> 0 ##############
viab_rfx1 <- cbind(viab.params$w[draws,1,1],viab.params$w[draws,1,5],
                   viab.params$w[draws,1,10],
                   rep(NA,N_draws),rep(NA,N_draws),rep(NA,N_draws),
                   rep(NA,N_draws),rep(NA,N_draws),rep(NA,N_draws),#2007-2012
                   viab.params$w[draws,1,2],viab.params$w[draws,1,13],
                   viab.params$w[draws,1,11],viab.params$w[draws,1,7],
                   viab.params$w[draws,1,9],viab.params$w[draws,1,8],
                   viab.params$w[draws,1,3],
                   rep(NA,N_draws),#2020
                   viab.params$w[draws,1,6],viab.params$w[draws,1,12],
                   viab.params$w[draws,1,4])

viab_rfx2 <- cbind(viab.params$w[draws,2,2],viab.params$w[draws,2,1],
                   viab.params$w[draws,2,3],
                   rep(NA,N_draws),rep(NA,N_draws),rep(NA,N_draws),
                   rep(NA,N_draws),rep(NA,N_draws),rep(NA,N_draws),#2007-2012
                   viab.params$w[draws,2,7],viab.params$w[draws,2,5],
                   viab.params$w[draws,2,6],viab.params$w[draws,2,10],
                   viab.params$w[draws,2,11],viab.params$w[draws,2,9],
                   viab.params$w[draws,2,4],
                   rep(NA,N_draws),#2020
                   viab.params$w[draws,2,8],viab.params$w[draws,2,12],
                   viab.params$w[draws,2,13])

viab_rfx3 <- cbind(viab.params$w[draws,3,2],viab.params$w[draws,3,1],
                   viab.params$w[draws,3,3],
                   rep(NA,N_draws),rep(NA,N_draws),rep(NA,N_draws),
                   rep(NA,N_draws),rep(NA,N_draws),rep(NA,N_draws),#2007-2012
                   viab.params$w[draws,3,4],viab.params$w[draws,3,7],
                   viab.params$w[draws,3,6],viab.params$w[draws,3,5],
                   viab.params$w[draws,3,8],viab.params$w[draws,3,12],
                   viab.params$w[draws,3,10],
                   rep(NA,N_draws),#2020
                   viab.params$w[draws,3,11],viab.params$w[draws,3,9],
                   viab.params$w[draws,3,13])

viab_rfx4 <- cbind(viab.params$w[draws,4,1],viab.params$w[draws,4,2],viab.params$w[draws,4,6],
                   rep(NA,N_draws),rep(NA,N_draws),rep(NA,N_draws),
                   rep(NA,N_draws),rep(NA,N_draws),rep(NA,N_draws),#2007-2012
                   viab.params$w[draws,4,8],viab.params$w[draws,4,12],
                   viab.params$w[draws,4,13],viab.params$w[draws,4,7],
                   viab.params$w[draws,4,9],viab.params$w[draws,4,10],
                   viab.params$w[draws,4,4],
                   rep(NA,N_draws),#2020
                   viab.params$w[draws,4,5],viab.params$w[draws,4,11],
                   viab.params$w[draws,4,3])

plot(x = yrs,y = colMeans(viab_rfx3), col = othercol,
     type = "b",
     ylim = c(-2,2))
lines(x = yrs, y = colMeans(viab_rfx2), col = liomcol,type = "b")
lines(x = yrs, y = colMeans(viab_rfx4), col = vaccol,type = "b")
lines(x = yrs, y = colMeans(viab_rfx1), col = cremcol,type = "b")

class(viab_rfx1)
class(viab_rfx2)
class(viab_rfx3)
class(viab_rfx4)

# Check dimensions
dim(viab_rfx1)
dim(viab_rfx2)
dim(viab_rfx3)
dim(viab_rfx4)

# Check structure
str(viab_rfx1)
## Test correlations #######
## Show the correlation between ant and year -- from growth model random effects
g_crem <- colMeans((grow_rfx1))
g_liom <- colMeans((grow_rfx2))
g_other <- colMeans((grow_rfx3))
g_vac <- colMeans((grow_rfx4))
years_seq <- 2004:2023
## Show the correlation between ant and year -- from survival model random effects
s_crem <- colMeans((surv_rfx1))
s_liom <- colMeans((surv_rfx2))
s_other <- colMeans((surv_rfx3))
s_vac <- colMeans((surv_rfx4))
## Show the correlation between ant and year -- from growth model random effects
v_crem <- colMeans((viab_rfx1))
v_liom <- colMeans((viab_rfx2))
v_other <- colMeans((viab_rfx3))
v_vac <- colMeans((viab_rfx4))


##### CREM FIGS 
grow1 <- (grow_rfx1[,c(1:3,6:15,18,19)])
grow2 <- (grow_rfx2[,c(1:3,6:15,18,19)])
grow3 <- (grow_rfx3[,c(1:3,6:15,18,19)])
grow4 <- (grow_rfx4[,c(1:3,6:15,18,19)])
surv1 <- (surv_rfx1[,c(1:3,6:15,18,19)])
surv2 <- (surv_rfx2[,c(1:3,6:15,18,19)])
surv3 <- (surv_rfx3[,c(1:3,6:15,18,19)])
surv4 <- (surv_rfx4[,c(1:3,6:15,18,19)])
viab1 <- (viab_rfx1[,c(1:3,10:16,18,19,20)])
viab2 <- (viab_rfx2[,c(1:3,10:16,18,19,20)])
viab3 <- (viab_rfx3[,c(1:3,10:16,18,19,20)])
viab4 <- (viab_rfx4[,c(1:3,10:16,18,19,20)])
# Corr between crem and crem
ccg <- cor.test(grow1,grow1)
ccs <- cor.test(surv1,surv1)
ccv <- cor.test(viab1,viab1)
# Corr between crem and liom
clg <- cor.test(grow1,grow2)
cls <- cor.test(surv1,surv2)
clv <- cor.test(viab1,viab2)
# Corr between liom and liom
llg <- cor.test(grow2,grow2)
lls <- cor.test(surv2,surv2)
llv <- cor.test(viab2,viab2)
# Corr between crem and other
cog <- cor.test(grow1,grow3)
cos <- cor.test(surv1,surv3)
cov <- cor.test(viab1,viab3)
# Corr between liom and other
log <- cor.test(grow2,grow3)
los <- cor.test(surv2,surv3)
lov <- cor.test(viab2,viab3)
# Corr between other and other
oog <- cor.test(grow3,grow3)
oos <- cor.test(surv3,surv3)
oov <- cor.test(viab3,viab3)
# Corr between crem and vacant
cvg <- cor.test(grow1,grow4)
cvs <- cor.test(surv1,surv4)
cvv <- cor.test(viab1,viab4)
# Corr between liom and vacant
lvg <- cor.test(grow2,grow4)
lvs <- cor.test(surv2,surv4)
lvv <- cor.test(viab2,viab4)
# Corr between other and vacant
ovg <- cor.test(grow3,grow4)
ovs <- cor.test(surv3,surv4)
ovv <- cor.test(viab3,viab4)
# Corr between vacant and vacant
vvg <- cor.test(grow4,grow4)
vvs <- cor.test(surv4,surv4)
vvv <- cor.test(viab4,viab4)

# create matrix 1 = crem, 2 = liom, 3 = other, 4 = vac
a <- matrix(NA, nrow = 4, ncol = 4)
#crem col
a[1,1] <- ccg$estimate
a[1,2] <- clg$estimate
a[1,3] <- cog$estimate
a[1,4] <- cvg$estimate
#liom col
a[2,1] <- clg$estimate
a[2,2] <- llg$estimate
a[2,3] <- log$estimate
a[2,4] <- lvg$estimate
#other col
a[3,1] <- cog$estimate
a[3,2] <- log$estimate
a[3,3] <- oog$estimate
a[3,4] <- ovg$estimate
#vac col
a[4,1] <- cvg$estimate
a[4,2] <- lvg$estimate
a[4,3] <- ovg$estimate
a[4,4] <- vvg$estimate

# create matrix 1 = crem, 2 = liom, 3 = other, 4 = vac
b <- matrix(NA, nrow = 4, ncol = 4)
#crem col
b[1,1] <- ccs$estimate
b[1,2] <- cls$estimate
b[1,3] <- cos$estimate
b[1,4] <- cvs$estimate
#liom col
b[2,1] <- cls$estimate
b[2,2] <- lls$estimate
b[2,3] <- los$estimate
b[2,4] <- lvs$estimate
#other col
b[3,1] <- cos$estimate
b[3,2] <- los$estimate
b[3,3] <- oos$estimate
b[3,4] <- ovs$estimate
#vac col
b[4,1] <- cvs$estimate
b[4,2] <- lvs$estimate
b[4,3] <- ovs$estimate
b[4,4] <- vvs$estimate
colnames(b) <- c("C","L","O","V")
rownames(b) <- c("C","L","O","V")
# create matrix 1 = crem, 2 = liom, 3 = other, 4 = vac
c <- matrix(NA, nrow = 4, ncol = 4)
#crem col
c[1,1] <- ccv$estimate
c[1,2] <- clv$estimate
c[1,3] <- cov$estimate
c[1,4] <- cvv$estimate
#liom col
c[2,1] <- clv$estimate
c[2,2] <- llv$estimate
c[2,3] <- lov$estimate
c[2,4] <- lvv$estimate
#other col
c[3,1] <- cov$estimate
c[3,2] <- lov$estimate
c[3,3] <- oov$estimate
c[3,4] <- ovv$estimate
#vac col
c[4,1] <- cvv$estimate
c[4,2] <- lvv$estimate
c[4,3] <- ovv$estimate
c[4,4] <- vvv$estimate
colnames(c) <- c("C","L","O","V")
rownames(c) <- c("C","L","O","V")
c
# non diagonal means
non_diag <- row(a) != col(a)
a_no_diag <- a[non_diag]
mean(a_no_diag)
b_no_diag <- b[non_diag]
mean(b_no_diag)
c_no_diag <- c[non_diag]
mean(c_no_diag)

## Calculate the lambdas ############
# Enter all required info:
params_mean<-data.frame(matrix(colMeans(params),1,ncol(params)))
names(params_mean)<-names(params)
scenario = c("none","cremvac","liomvac","othervac","liomcremvac","liomvacother","othercremvac","all")
scenario_abv <- c("V","C","L","O","LC","LO","OC","LOC")
colors_lambdas <- c(vcol,ccol,lcol,ocol,lccol,locol,cocol,acol)
x_vals <- c(1,3,5,7,9,11,13,15)
lower.extension<-4
upper.extension<-0.5
trans_years<-c(1:3,10:15,18:19);(2004:2023)[trans_years]
max_yrs<-1000
set.seed(77005)
year_seq<-sample(trans_years,max_yrs,replace=T)
sims <- c("comp","equal","freq")
lambda_list <- lambda_sync_list <- list()
lambdaS_full <- lambdaS_sync_full <- matrix(NA, nrow = 1, ncol = length(scenario))
for(i in 1:3){ # each model simulation (comp, equal, freq)
  for(m in 1:length(scenario)){ # each ant combination
      lambdaS_full[1,m] <- lambdaSim(params = params_mean,
                                     scenario = scenario[m],
                                     lower = cholla_min, upper = cholla_max, 
                                     floor = lower.extension, ceiling = upper.extension, 
                                     matsize = matsize,
                                     grow_rfx1=colMeans(grow_rfx1),grow_rfx2=colMeans(grow_rfx2),
                                     grow_rfx3=colMeans(grow_rfx3),grow_rfx4=colMeans(grow_rfx4),
                                     surv_rfx1=colMeans(surv_rfx1),surv_rfx2=colMeans(surv_rfx2),
                                     surv_rfx3=colMeans(surv_rfx3),surv_rfx4=colMeans(surv_rfx4),
                                     flow_rfx=colMeans(flow_rfx),
                                     repro_rfx=colMeans(repro_rfx),
                                     viab_rfx1=colMeans(viab_rfx1),viab_rfx2=colMeans(viab_rfx2),
                                     viab_rfx3=colMeans(viab_rfx3),viab_rfx4=colMeans(viab_rfx4),
                                     trans_years=year_seq[1:5],simulation = sims[i])
      lambdaS_sync_full[1,m] <- lambdaSim(params = params_mean,
                                      scenario = scenario[m],
                                      lower = cholla_min, upper = cholla_max,
                                      floor = lower.extension, ceiling = upper.extension,
                                      matsize = matsize,
                                      grow_rfx1=colMeans(grow_rfx),grow_rfx2=colMeans(grow_rfx),
                                      grow_rfx3=colMeans(grow_rfx),grow_rfx4=colMeans(grow_rfx),
                                      surv_rfx1=colMeans(surv_rfx),surv_rfx2=colMeans(surv_rfx),
                                      surv_rfx3=colMeans(surv_rfx),surv_rfx4=colMeans(surv_rfx),
                                      flow_rfx=colMeans(flow_rfx),
                                      repro_rfx=colMeans(repro_rfx),
                                      viab_rfx1=colMeans(viab_rfx),viab_rfx2=colMeans(viab_rfx),
                                      viab_rfx3=colMeans(viab_rfx),viab_rfx4=colMeans(viab_rfx),
                                      trans_years=year_seq[1:5], simulation = sims[i])
    print(scenario[m])
  } # end ant combos
  lambda_list[[i]] <- lambdaS_full
  lambda_sync_list[[i]] <- lambdaS_sync_full
} # end simulations
lambda_list
lambda_sync_list
# Pull out the non-synchronous values
write.csv(lambda_list[[1]],"Model Outputs/cor0_lambda_comp.csv")
write.csv(lambda_list[[2]],"Model Outputs/cor0_lambda_equal.csv")
write.csv(lambda_list[[3]],"Model Outputs/cor0_lambda_freq.csv")
# pull out synchronous values
write.csv(lambda_sync_list[[1]],"Model Outputs/cor0_lambda_sync_comp.csv")
write.csv(lambda_sync_list[[2]],"Model Outputs/cor0_lambda_sync_equal.csv")
write.csv(lambda_sync_list[[3]],"Model Outputs/cor0_lambda_sync_freq.csv")

## Visualize ###############
## Read in lambda estimates
## Competitive Exclusion
lams_comp_stoch <- read.csv("Model Outputs/cor0_lambda_comp.csv")
lams_comp_stoch <- lams_comp_stoch[,-c(1)]
## Competitive Exclusion Null Stochastic
lams_comp_stoch_sync <- read.csv("Model Outputs/cor0_lambda_sync_comp.csv")
lams_comp_stoch_sync <- lams_comp_stoch_sync[,-c(1)]
## Frequency Based
lams_freq_stoch <- read.csv("Model Outputs/cor0_lambda_freq.csv")
lams_freq_stoch <- lams_freq_stoch[,-c(1)]
## Frequency Based sync
lams_freq_stoch_sync <- read.csv("Model Outputs/cor0_lambda_sync_freq.csv")
lams_freq_stoch_sync <- lams_freq_stoch_sync[,-c(1)]
## Equal Probability
lams_equal_stoch <- read.csv("Model Outputs/cor0_lambda_equal.csv")
lams_equal_stoch <- lams_equal_stoch[,-c(1)]
## Equal Probability sync Stochastic
lams_equal_stoch_sync <- read.csv("Model Outputs/cor0_lambda_sync_equal.csv")
lams_equal_stoch_sync <- lams_equal_stoch_sync[,-c(1)]
scenario_abv <- c("V","C","L","O","LC","LO","OC","LOC")
colnames(lams_comp_stoch_sync) <- scenario
colnames(lams_comp_stoch) <- scenario
x1 <- c(1,2)
y1 <- c(lams_comp_stoch_sync[1,8] - lams_comp_stoch_sync[1,1],
       lams_comp_stoch[1,8] - lams_comp_stoch[1,1])
x2 <- c(1,2)
y2 <- c(lams_freq_stoch_sync[1,8] - lams_freq_stoch_sync[1,1],
        lams_freq_stoch[1,8] - lams_freq_stoch[1,1])
x3 <- c(1,2)
y3 <- c(lams_equal_stoch_sync[1,8] - lams_equal_stoch_sync[1,1],
        lams_equal_stoch[1,8] - lams_equal_stoch[1,1])


png("Manuscript/Figures/cor0_Portfolio_Effect.png")
par(mar=c(5,5,1,1))
layout(matrix(c(1,2,3),
              ncol = 3, nrow = 1), heights = c(1,1,1))
plot(x1,y1,
     col = c("black","red"), pch = c(18,16),cex = 2,
     xlim = c(0,3),ylim = c(-.05,.15),
     xlab = "Competitive",ylab = "Difference between All \n and None Scenarios")
# legend("topright",legend = c("Non-synchronous","Synchronous"),col = c("Red","Black"), pch = c(16,18), cex = 1)
plot(x2,y2,
     col = c("black","red"), pch = c(18,16),cex = 2,
     xlim = c(0,3),ylim = c(-.05,.15),
     xlab = "Frequency Based",ylab = "")
# legend("topright",legend = c("Non-synchronous","Synchronous"),col = c("Red","Black"), pch = c(16,18), cex = 1)
plot(x3,y3,
     col = c("black","red"), pch = c(18,16),cex = 2,
     xlim = c(0,3),ylim = c(-.05,.15),
     xlab = "Equal Probability",ylab = "Difference between All and None Scenarios")
legend("topright",legend = c("Non-synchronous","Synchronous"),col = c("Red","Black"), pch = c(16,18), cex = 1)
dev.off()




################################################################################
#### Make Correlation as Close to -1 as Possible
################################################################################
yrs <- c(2004:2023)
## Growth RFX cor -> -1 ####
grow_rfx1_neg <- cbind(grow.params$w[draws,1,4],grow.params$w[draws,1,2],grow.params$w[draws,1,3],
                   rep(NA,N_draws),rep(NA,N_draws), ##2007,2008
                   grow.params$w[draws,1,15],grow.params$w[draws,1,12],grow.params$w[draws,1,14],
                   grow.params$w[draws,1,10],grow.params$w[draws,1,11],grow.params$w[draws,1,5],
                   grow.params$w[draws,1,9],grow.params$w[draws,1,1],grow.params$w[draws,1,13],
                   grow.params$w[draws,1,6],rep(NA,N_draws),rep(NA,N_draws), ##2019,2020
                   grow.params$w[draws,1,7],grow.params$w[draws,1,8],rep(NA,N_draws))##2023
# Ant 2 (prev liom)
grow_rfx2_neg <- cbind(grow.params$w[draws,2,8],grow.params$w[draws,2,9],grow.params$w[draws,2,5],
                   rep(NA,N_draws),rep(NA,N_draws), ##2007,2008
                   grow.params$w[draws,2,7],grow.params$w[draws,2,14],grow.params$w[draws,2,4],
                   grow.params$w[draws,2,2],grow.params$w[draws,2,6],grow.params$w[draws,2,11],
                   grow.params$w[draws,2,1],grow.params$w[draws,2,13],grow.params$w[draws,2,12],
                   grow.params$w[draws,2,15],rep(NA,N_draws),rep(NA,N_draws), ##2019,2020
                   grow.params$w[draws,2,7],grow.params$w[draws,2,3],rep(NA,N_draws))##2023
# Ant 3 (prev other)
grow_rfx3_neg <- cbind(grow.params$w[draws,3,7],grow.params$w[draws,3,2],grow.params$w[draws,3,15],
                   rep(NA,N_draws),rep(NA,N_draws), ##2007,2008
                   grow.params$w[draws,3,5],grow.params$w[draws,3,4],grow.params$w[draws,3,14],
                   grow.params$w[draws,3,1],grow.params$w[draws,3,8],grow.params$w[draws,3,13],
                   grow.params$w[draws,3,9],grow.params$w[draws,3,2],grow.params$w[draws,3,6],
                   grow.params$w[draws,3,11],rep(NA,N_draws),rep(NA,N_draws), ##2019,2020
                   grow.params$w[draws,3,10],grow.params$w[draws,3,12],rep(NA,N_draws))##2023
# Ant 4 (prev vac)
grow_rfx4_neg <- cbind(grow.params$w[draws,4,8],grow.params$w[draws,4,3],grow.params$w[draws,4,10],
                   rep(NA,N_draws),rep(NA,N_draws), ##2007,2008
                   grow.params$w[draws,4,7],grow.params$w[draws,4,13],grow.params$w[draws,4,15],
                   grow.params$w[draws,4,5],grow.params$w[draws,4,4],grow.params$w[draws,4,1],
                   grow.params$w[draws,4,9],grow.params$w[draws,4,2],grow.params$w[draws,4,2],
                   grow.params$w[draws,4,12],rep(NA,N_draws),rep(NA,N_draws), ##2019,2020
                   grow.params$w[draws,4,11],grow.params$w[draws,4,6],rep(NA,N_draws))##2023
plot(x = yrs,y = colMeans(grow_rfx1_neg), col = cremcol,
     type = "b",
     ylim = c(-.5,.5))
lines(x = yrs, y = colMeans(grow_rfx2_neg), col = liomcol,type = "b")
lines(x = yrs, y = colMeans(grow_rfx3_neg), col = othercol,type = "b")
lines(x = yrs, y = colMeans(grow_rfx4_neg), col = vaccol,type = "b")


## Survival RFX cor -> 0 ###########
surv_rfx1_neg <- cbind(surv.params$w[draws,1,9],surv.params$w[draws,1,2],surv.params$w[draws,1,10],
                   rep(NA,N_draws),rep(NA,N_draws),
                   surv.params$w[draws,1,13],surv.params$w[draws,1,1],surv.params$w[draws,1,5],
                   surv.params$w[draws,1,12],surv.params$w[draws,1,3],surv.params$w[draws,1,7],
                   surv.params$w[draws,1,4],surv.params$w[draws,1,11],surv.params$w[draws,1,14],
                   surv.params$w[draws,1,15],rep(NA,N_draws),rep(NA,N_draws),
                   surv.params$w[draws,1,6],surv.params$w[draws,1,8],rep(NA,N_draws))#2023
# Ant 2 (prev liom)
surv_rfx2_neg <- cbind(surv.params$w[draws,2,13],surv.params$w[draws,2,15],surv.params$w[draws,2,4],
                   rep(NA,N_draws),rep(NA,N_draws),
                   surv.params$w[draws,2,2],surv.params$w[draws,2,5],surv.params$w[draws,2,7],
                   surv.params$w[draws,2,9],surv.params$w[draws,2,12],surv.params$w[draws,2,6],
                   surv.params$w[draws,2,1],surv.params$w[draws,2,11],surv.params$w[draws,2,8],
                   surv.params$w[draws,2,10],rep(NA,N_draws),rep(NA,N_draws),
                   surv.params$w[draws,2,6],surv.params$w[draws,2,14],rep(NA,N_draws))#2023
# Ant 3 (prev other)
surv_rfx3_neg <- cbind(surv.params$w[draws,3,3],surv.params$w[draws,3,9],surv.params$w[draws,3,14],
                   rep(NA,N_draws),rep(NA,N_draws),
                   surv.params$w[draws,3,7],surv.params$w[draws,3,6],surv.params$w[draws,3,2],
                   surv.params$w[draws,3,8],surv.params$w[draws,3,10],surv.params$w[draws,3,14],
                   surv.params$w[draws,3,11], surv.params$w[draws,3,12],surv.params$w[draws,3,4],
                   surv.params$w[draws,3,15],rep(NA,N_draws),rep(NA,N_draws),
                   surv.params$w[draws,3,5],surv.params$w[draws,3,13],rep(NA,N_draws))#2023
# Ant 4 (prev vac)
surv_rfx4_neg <- cbind(surv.params$w[draws,4,8],surv.params$w[draws,4,2],surv.params$w[draws,4,10],
                   rep(NA,N_draws),rep(NA,N_draws),
                   surv.params$w[draws,4,14],surv.params$w[draws,4,5],surv.params$w[draws,4,15],
                   surv.params$w[draws,4,6],surv.params$w[draws,4,3],surv.params$w[draws,4,13],
                   surv.params$w[draws,4,12],surv.params$w[draws,4,1],surv.params$w[draws,4,4],
                   surv.params$w[draws,4,9],rep(NA,N_draws),rep(NA,N_draws),
                   surv.params$w[draws,4,11],surv.params$w[draws,4,7],rep(NA,N_draws))#2023


plot(x = yrs,y = colMeans(surv_rfx4_neg), col = vaccol,
     type = "b",
     ylim = c(-1.2,1))
lines(x = yrs, y = colMeans(surv_rfx2_neg), col = liomcol,type = "b")
lines(x = yrs, y = colMeans(surv_rfx3_neg), col = othercol,type = "b")
lines(x = yrs, y = colMeans(surv_rfx1_neg), col = vaccol,type = "b")
## Viability  RFX cor -> 0 ##############
viab_rfx1_neg <- cbind(viab.params$w[draws,1,9],viab.params$w[draws,1,1],
                   viab.params$w[draws,1,12],
                   rep(NA,N_draws),rep(NA,N_draws),rep(NA,N_draws),
                   rep(NA,N_draws),rep(NA,N_draws),rep(NA,N_draws),#2007-2012
                   viab.params$w[draws,1,5],viab.params$w[draws,1,6],
                   viab.params$w[draws,1,7],viab.params$w[draws,1,3],
                   viab.params$w[draws,1,8],viab.params$w[draws,1,11],
                   viab.params$w[draws,1,8],
                   rep(NA,N_draws),#2020
                   viab.params$w[draws,1,2],viab.params$w[draws,1,13],
                   viab.params$w[draws,1,10])

viab_rfx2_neg <- cbind(viab.params$w[draws,2,7],viab.params$w[draws,2,6],
                   viab.params$w[draws,2,8],
                   rep(NA,N_draws),rep(NA,N_draws),rep(NA,N_draws),
                   rep(NA,N_draws),rep(NA,N_draws),rep(NA,N_draws),#2007-2012
                   viab.params$w[draws,2,10],viab.params$w[draws,2,11],
                   viab.params$w[draws,2,8],viab.params$w[draws,2,9],
                   viab.params$w[draws,2,1],viab.params$w[draws,2,2],
                   viab.params$w[draws,2,4],
                   rep(NA,N_draws),#2020
                   viab.params$w[draws,2,12],viab.params$w[draws,2,13],
                   viab.params$w[draws,2,3])

viab_rfx3_neg <- cbind(viab.params$w[draws,3,13],viab.params$w[draws,3,2],
                   viab.params$w[draws,3,11],
                   rep(NA,N_draws),rep(NA,N_draws),rep(NA,N_draws),
                   rep(NA,N_draws),rep(NA,N_draws),rep(NA,N_draws),#2007-2012
                   viab.params$w[draws,3,9],viab.params$w[draws,3,10],
                   viab.params$w[draws,3,12],viab.params$w[draws,3,7],
                   viab.params$w[draws,3,8],viab.params$w[draws,3,3],
                   viab.params$w[draws,3,5],
                   rep(NA,N_draws),#2020
                   viab.params$w[draws,3,4],viab.params$w[draws,3,1],
                   viab.params$w[draws,3,6])

viab_rfx4_neg <- cbind(viab.params$w[draws,4,11],viab.params$w[draws,4,8],
                       viab.params$w[draws,4,2],
                   rep(NA,N_draws),rep(NA,N_draws),rep(NA,N_draws),
                   rep(NA,N_draws),rep(NA,N_draws),rep(NA,N_draws),#2007-2012
                   viab.params$w[draws,4,1],viab.params$w[draws,4,9],
                   viab.params$w[draws,4,7],viab.params$w[draws,4,4],
                   viab.params$w[draws,4,10],viab.params$w[draws,4,5],
                   viab.params$w[draws,4,6],
                   rep(NA,N_draws),#2020
                   viab.params$w[draws,4,13],viab.params$w[draws,4,3],
                   viab.params$w[draws,4,12])

plot(x = yrs,y = colMeans(viab_rfx3_neg), col = othercol,
     type = "b",
     ylim = c(-2,2))
lines(x = yrs, y = colMeans(viab_rfx2_neg), col = liomcol,type = "b")
lines(x = yrs, y = colMeans(viab_rfx4_neg), col = vaccol,type = "b")
lines(x = yrs, y = colMeans(viab_rfx1_neg), col = cremcol,type = "b")

## Test correlations #######
## Show the correlation between ant and year -- from growth model random effects
g_crem <- colMeans((grow_rfx1_neg))
g_liom <- colMeans((grow_rfx2_neg))
g_other <- colMeans((grow_rfx3_neg))
g_vac <- colMeans((grow_rfx4_neg))
years_seq <- 2004:2023
## Show the correlation between ant and year -- from survival model random effects
s_crem <- colMeans((surv_rfx1_neg))
s_liom <- colMeans((surv_rfx2_neg))
s_other <- colMeans((surv_rfx3_neg))
s_vac <- colMeans((surv_rfx4_neg))
## Show the correlation between ant and year -- from growth model random effects
v_crem <- colMeans((viab_rfx1_neg))
v_liom <- colMeans((viab_rfx2_neg))
v_other <- colMeans((viab_rfx3_neg))
v_vac <- colMeans((viab_rfx4_neg))


##### CREM FIGS 
grow1 <- (grow_rfx1_neg[,c(1:3,6:15,18,19)])
grow2 <- (grow_rfx2_neg[,c(1:3,6:15,18,19)])
grow3 <- (grow_rfx3_neg[,c(1:3,6:15,18,19)])
grow4 <- (grow_rfx4_neg[,c(1:3,6:15,18,19)])
surv1 <- (surv_rfx1_neg[,c(1:3,6:15,18,19)])
surv2 <- (surv_rfx2_neg[,c(1:3,6:15,18,19)])
surv3 <- (surv_rfx3_neg[,c(1:3,6:15,18,19)])
surv4 <- (surv_rfx4_neg[,c(1:3,6:15,18,19)])
viab1 <- (viab_rfx1_neg[,c(1:3,10:16,18,19,20)])
viab2 <- (viab_rfx2_neg[,c(1:3,10:16,18,19,20)])
viab3 <- (viab_rfx3_neg[,c(1:3,10:16,18,19,20)])
viab4 <- (viab_rfx4_neg[,c(1:3,10:16,18,19,20)])
# Corr between crem and crem
ccg <- cor.test(grow1,grow1)
ccs <- cor.test(surv1,surv1)
ccv <- cor.test(viab1,viab1)
# Corr between crem and liom
clg <- cor.test(grow1,grow2)
cls <- cor.test(surv1,surv2)
clv <- cor.test(viab1,viab2)
# Corr between liom and liom
llg <- cor.test(grow2,grow2)
lls <- cor.test(surv2,surv2)
llv <- cor.test(viab2,viab2)
# Corr between crem and other
cog <- cor.test(grow1,grow3)
cos <- cor.test(surv1,surv3)
cov <- cor.test(viab1,viab3)
# Corr between liom and other
log <- cor.test(grow2,grow3)
los <- cor.test(surv2,surv3)
lov <- cor.test(viab2,viab3)
# Corr between other and other
oog <- cor.test(grow3,grow3)
oos <- cor.test(surv3,surv3)
oov <- cor.test(viab3,viab3)
# Corr between crem and vacant
cvg <- cor.test(grow1,grow4)
cvs <- cor.test(surv1,surv4)
cvv <- cor.test(viab1,viab4)
# Corr between liom and vacant
lvg <- cor.test(grow2,grow4)
lvs <- cor.test(surv2,surv4)
lvv <- cor.test(viab2,viab4)
# Corr between other and vacant
ovg <- cor.test(grow3,grow4)
ovs <- cor.test(surv3,surv4)
ovv <- cor.test(viab3,viab4)
# Corr between vacant and vacant
vvg <- cor.test(grow4,grow4)
vvs <- cor.test(surv4,surv4)
vvv <- cor.test(viab4,viab4)

# create matrix 1 = crem, 2 = liom, 3 = other, 4 = vac
a <- matrix(NA, nrow = 4, ncol = 4)
#crem col
a[1,1] <- ccg$estimate
a[1,2] <- clg$estimate
a[1,3] <- cog$estimate
a[1,4] <- cvg$estimate
#liom col
a[2,1] <- clg$estimate
a[2,2] <- llg$estimate
a[2,3] <- log$estimate
a[2,4] <- lvg$estimate
#other col
a[3,1] <- cog$estimate
a[3,2] <- log$estimate
a[3,3] <- oog$estimate
a[3,4] <- ovg$estimate
#vac col
a[4,1] <- cvg$estimate
a[4,2] <- lvg$estimate
a[4,3] <- ovg$estimate
a[4,4] <- vvg$estimate

# create matrix 1 = crem, 2 = liom, 3 = other, 4 = vac
b <- matrix(NA, nrow = 4, ncol = 4)
#crem col
b[1,1] <- ccs$estimate
b[1,2] <- cls$estimate
b[1,3] <- cos$estimate
b[1,4] <- cvs$estimate
#liom col
b[2,1] <- cls$estimate
b[2,2] <- lls$estimate
b[2,3] <- los$estimate
b[2,4] <- lvs$estimate
#other col
b[3,1] <- cos$estimate
b[3,2] <- los$estimate
b[3,3] <- oos$estimate
b[3,4] <- ovs$estimate
#vac col
b[4,1] <- cvs$estimate
b[4,2] <- lvs$estimate
b[4,3] <- ovs$estimate
b[4,4] <- vvs$estimate
colnames(b) <- c("C","L","O","V")
rownames(b) <- c("C","L","O","V")
# create matrix 1 = crem, 2 = liom, 3 = other, 4 = vac
c <- matrix(NA, nrow = 4, ncol = 4)
#crem col
c[1,1] <- ccv$estimate
c[1,2] <- clv$estimate
c[1,3] <- cov$estimate
c[1,4] <- cvv$estimate
#liom col
c[2,1] <- clv$estimate
c[2,2] <- llv$estimate
c[2,3] <- lov$estimate
c[2,4] <- lvv$estimate
#other col
c[3,1] <- cov$estimate
c[3,2] <- lov$estimate
c[3,3] <- oov$estimate
c[3,4] <- ovv$estimate
#vac col
c[4,1] <- cvv$estimate
c[4,2] <- lvv$estimate
c[4,3] <- ovv$estimate
c[4,4] <- vvv$estimate
colnames(c) <- c("C","L","O","V")
rownames(c) <- c("C","L","O","V")
c
# non diagonal means
non_diag <- row(a) != col(a)
a_no_diag <- a[non_diag]
mean(a_no_diag)
b_no_diag <- b[non_diag]
mean(b_no_diag)
c_no_diag <- c[non_diag]
mean(c_no_diag)

## Calculate the lambdas ############
# Enter all required info:
params_mean<-data.frame(matrix(colMeans(params),1,ncol(params)))
names(params_mean)<-names(params)
scenario = c("none","cremvac","liomvac","othervac","liomcremvac","liomvacother","othercremvac","all")
scenario_abv <- c("V","C","L","O","LC","LO","OC","LOC")
colors_lambdas <- c(vcol,ccol,lcol,ocol,lccol,locol,cocol,acol)
x_vals <- c(1,3,5,7,9,11,13,15)
lower.extension<-4
upper.extension<-0.5
trans_years<-c(1:3,10:15,18:19);(2004:2023)[trans_years]
max_yrs<-1000
set.seed(77005)
year_seq<-sample(trans_years,max_yrs,replace=T)
sims <- c("comp","equal","freq")
lambdaS_full <- lambdaS_sync_full <- matrix(NA, nrow = 1, ncol = length(scenario))
  for(m in 1:length(scenario)){ # each ant combination
    # nonsync
    lambdaS_full[1,m] <- lambdaSim(params = params_mean,
                                   scenario = scenario[m],
                                   lower = cholla_min, upper = cholla_max, 
                                   floor = lower.extension, ceiling = upper.extension, 
                                   matsize = matsize,
                                   grow_rfx1=colMeans(grow_rfx1_neg),grow_rfx2=colMeans(grow_rfx2_neg),
                                   grow_rfx3=colMeans(grow_rfx3_neg),grow_rfx4=colMeans(grow_rfx4_neg),
                                   surv_rfx1=colMeans(surv_rfx1_neg),surv_rfx2=colMeans(surv_rfx2_neg),
                                   surv_rfx3=colMeans(surv_rfx3_neg),surv_rfx4=colMeans(surv_rfx4_neg),
                                   flow_rfx=colMeans(flow_rfx),
                                   repro_rfx=colMeans(repro_rfx),
                                   viab_rfx1=colMeans(viab_rfx1_neg),viab_rfx2=colMeans(viab_rfx2_neg),
                                   viab_rfx3=colMeans(viab_rfx3_neg),viab_rfx4=colMeans(viab_rfx4_neg),
                                   trans_years=year_seq[1:5],simulation = sims[1])
    # sync
    lambdaS_sync_full[1,m] <- lambdaSim(params = params_mean,
                                        scenario = scenario[m],
                                        lower = cholla_min, upper = cholla_max,
                                        floor = lower.extension, ceiling = upper.extension,
                                        matsize = matsize,
                                        grow_rfx1=colMeans(grow_rfx),grow_rfx2=colMeans(grow_rfx),
                                        grow_rfx3=colMeans(grow_rfx),grow_rfx4=colMeans(grow_rfx),
                                        surv_rfx1=colMeans(surv_rfx),surv_rfx2=colMeans(surv_rfx),
                                        surv_rfx3=colMeans(surv_rfx),surv_rfx4=colMeans(surv_rfx),
                                        flow_rfx=colMeans(flow_rfx),
                                        repro_rfx=colMeans(repro_rfx),
                                        viab_rfx1=colMeans(viab_rfx),viab_rfx2=colMeans(viab_rfx),
                                        viab_rfx3=colMeans(viab_rfx),viab_rfx4=colMeans(viab_rfx),
                                        trans_years=year_seq[1:5], simulation = sims[1])
    print(scenario[m])
  } # end ant combos
lambdaS_full
lambdaS_sync_full
# Pull out the non-synchronous values
write.csv(lambdaS_full,"Model Outputs/cor_neg_lambda_comp.csv")
# write.csv(lambda_list[[2]],"Model Outputs/cor_neg_lambda_equal.csv")
# write.csv(lambda_list[[3]],"Model Outputs/cor_neg_lambda_freq.csv")
# pull out synchronous values
write.csv(lambdaS_sync_full,"Model Outputs/cor_neg_lambda_sync_comp.csv")
# write.csv(lambda_sync_list[[2]],"Model Outputs/cor_neg_lambda_sync_equal.csv")
# write.csv(lambda_sync_list[[3]],"Model Outputs/cor_neg_lambda_sync_freq.csv")

################################################################################
#### Visual
################################################################################
## Visualize ###############
## Read in lambda estimates
##Regular
## Competitive Exclusion
lams_comp_stoch <- read.csv("Model Outputs/lambda_comp.csv")
lams_comp_stoch <- lams_comp_stoch[,-c(1)]
lams_comp_stoch <- colMeans(lams_comp_stoch)
lams_comp_stoch <- as.data.frame(t(lams_comp_stoch))
## Competitive Exclusion Null Stochastic
lams_comp_stoch_sync <- read.csv("Model Outputs/lambda_sync_comp.csv")
lams_comp_stoch_sync <- lams_comp_stoch_sync[,-c(1)]
lams_comp_stoch_sync <- colMeans(lams_comp_stoch_sync)
lams_comp_stoch_sync <- as.data.frame(t(lams_comp_stoch_sync))
## Cor = 0
## Competitive Exclusion
lams_comp_stoch_0 <- read.csv("Model Outputs/cor0_lambda_comp.csv")
lams_comp_stoch_0 <- lams_comp_stoch_0[,-c(1)]
## Competitive Exclusion Null Stochastic
lams_comp_stoch_sync_0 <- read.csv("Model Outputs/cor0_lambda_sync_comp.csv")
lams_comp_stoch_sync_0 <- lams_comp_stoch_sync_0[,-c(1)]
## Negative correlation
## Competitive Exclusion
lams_comp_stoch_neg <- read.csv("Model Outputs/cor_neg_lambda_comp.csv")
lams_comp_stoch_neg <- lams_comp_stoch_neg[,-c(1)]
## Competitive Exclusion Null Stochastic
lams_comp_stoch_sync_neg <- read.csv("Model Outputs/cor_neg_lambda_sync_comp.csv")
lams_comp_stoch_sync_neg <- lams_comp_stoch_sync_neg[,-c(1)]
scenario_abv <- c("V","C","L","O","LC","LO","OC","LOC")
colnames(lams_comp_stoch_sync_neg) <- colnames(lams_comp_stoch_neg) <- colnames(lams_comp_stoch) <- colnames(lams_comp_stoch_sync) <- colnames(lams_comp_stoch_0) <- colnames(lams_comp_stoch_sync_0) <- scenario


lams_realistic <- rbind(lams_comp_stoch,lams_comp_stoch_sync)
lams_0cor <- rbind(lams_comp_stoch_0,lams_comp_stoch_sync_0)
lams_negcor <- rbind(lams_comp_stoch_neg,lams_comp_stoch_sync_neg)
x1 <- c(1,1,2,2)
y1 <- c(lams_realistic$none,lams_realistic$all)
x2 <- c(1,1,2,2)
y2 <- c(lams_0cor$none,lams_0cor$all)
x3 <- c(1,1,2,2)
y3 <- c(lams_negcor$none,lams_negcor$all)

png("Manuscript/Figures/Extra_Portfolio_Effect.png", width = 500, height = 800)
par(mar=c(5,5,1,1), oma = c(2,4,1,1))
layout(matrix(c(1,2,3),
              ncol = 1, nrow = 3), heights = c(1,1,1))
## Realistic
plot(x1,y1,
     col = c("cornflowerblue","red","cornflowerblue","red"),pch = c(16),cex = 3,
     xlim = c(0.5,2.5), ylim = c(0.96,0.989),
     xaxt = "n", xlab = "", ylab = "")
mtext("Realistic",side = 2, cex = 2, line = 2)
mtext("None",side = 1, cex = 2, line = 1.5, adj = 0.25)
mtext("All",side = 1, cex = 2, line = 1.5, adj = 0.75)
legend("bottomright",legend = c("Synchronous","Non-Synchronous"),
       col = c("red","cornflowerblue"), pch = 16, cex = 1.5)
## 0 cor
plot(x2,y2,
     col = c("cornflowerblue","red","cornflowerblue","red"),pch = c(16),cex = 3,
     xlim = c(0.5,2.5), ylim = c(1.15,1.39),
     xaxt = "n", xlab = "", ylab = "")
mtext("Zero Correlation",side = 2, cex = 2, line = 2)
mtext("None",side = 1, cex = 2, line = 1.5, adj = 0.25)
mtext("All",side = 1, cex = 2, line = 1.5, adj = 0.75)
mtext(expression(paste(lambda)), side = 2, cex = 4, line = 4)

## Realistic
plot(x3,y3,
     col = c("cornflowerblue","red","cornflowerblue","red"),pch = c(16),cex = 3,
     xlim = c(0.5,2.5), ylim = c(1.05,1.42),
     xaxt = "n", xlab = "", ylab = "")
mtext("Negative Correlation",side = 2, cex = 2, line = 2)
mtext("None",side = 1, cex = 2, line = 1.5, adj = 0.25)
mtext("All",side = 1, cex = 2, line = 1.5, adj = 0.75)
dev.off()



x1 <- c(1,2,1,2)
y1 <- c(lams_realistic$none,lams_realistic$all)
x2 <- c(1,2,1,2)
y2 <- c(lams_0cor$none,lams_0cor$all)
x3 <- c(1,2,1,2)
y3 <- c(lams_negcor$none,lams_negcor$all)

png("Manuscript/Figures/Extra_Portfolio_Effect2.png", width = 500, height = 800)
par(mar=c(5,5,1,1), oma = c(2,4,1,1))
layout(matrix(c(1,2,3),
              ncol = 1, nrow = 3), heights = c(1,1,1))
## Realistic
plot(x1,y1,
     col = c("cornflowerblue","cornflowerblue","red","red"),pch = c(16),cex = 3,
     xlim = c(0.5,2.5), ylim = c(0.96,0.989),
     xaxt = "n", xlab = "", ylab = "")
mtext("Realistic",side = 2, cex = 2, line = 2)
mtext("Non-Sync.",side = 1, cex = 2, line = 1.5, adj = 0.25)
mtext("Sync.",side = 1, cex = 2, line = 1.5, adj = 0.75)
legend("bottomright",legend = c("None","All"),
       col = c("cornflowerblue","red"), pch = 16, cex = 1.5)
## 0 cor
plot(x2,y2,
     col = c("cornflowerblue","cornflowerblue","red","red"),pch = c(16),cex = 3,
     xlim = c(0.5,2.5), ylim = c(1.15,1.39),
     xaxt = "n", xlab = "", ylab = "")
mtext("Zero Correlation",side = 2, cex = 2, line = 2)
mtext("Non-Sync.",side = 1, cex = 2, line = 1.5, adj = 0.25)
mtext("Sync.",side = 1, cex = 2, line = 1.5, adj = 0.75)
mtext(expression(paste(lambda)), side = 2, cex = 4, line = 4)

## Realistic
plot(x3,y3,
     col = c("cornflowerblue","cornflowerblue","red","red"),pch = c(16),cex = 3,
     xlim = c(0.5,2.5), ylim = c(1.05,1.42),
     xaxt = "n", xlab = "", ylab = "")
mtext("Negative Correlation",side = 2, cex = 2, line = 2)
mtext("Non-Sync.",side = 1, cex = 2, line = 1.5, adj = 0.25)
mtext("Sync.",side = 1, cex = 2, line = 1.5, adj = 0.75)
dev.off()



######### FULL VISUALS ##############
## Competitive Exclusion
lams_comp_stoch <- read.csv("Model Outputs/cor0_lambda_comp.csv")
lams_comp_stoch <- lams_comp_stoch[,-c(1)]
## Competitive Exclusion Null Stochastic
lams_comp_stoch_sync <- read.csv("Model Outputs/cor0_lambda_sync_comp.csv")
lams_comp_stoch_sync <- lams_comp_stoch_sync[,-c(1)]

## Competitive Exclusion
lams_comp_stoch_1 <- read.csv("Model Outputs/lambda_comp.csv")
lams_comp_stoch_1 <- lams_comp_stoch_1[,-c(1)]
lams_comp_stoch_1 <- colMeans(lams_comp_stoch_1)
lams_comp_stoch_1 <- as.data.frame(t(lams_comp_stoch_1))
## Competitive Exclusion Null Stochastic
lams_comp_stoch_sync_1 <- read.csv("Model Outputs/lambda_sync_comp.csv")
lams_comp_stoch_sync_1 <- lams_comp_stoch_sync_1[,-c(1)]
lams_comp_stoch_sync_1 <- colMeans(lams_comp_stoch_sync_1)
lams_comp_stoch_sync_1 <- as.data.frame(t(lams_comp_stoch_sync_1))

a <- lams_comp_stoch_sync[1,8] - lams_comp_stoch_sync[1,1]
b <- lams_comp_stoch[1,8] - lams_comp_stoch[1,1]
c <- lams_comp_stoch_sync_1[1,8] - lams_comp_stoch_sync_1[1,1]
d <- lams_comp_stoch_1[1,8] - lams_comp_stoch_1[1,1]

png("Manuscript/Figures/cor0_Portfolio_Effect.png")
par(mar=c(5,5,1,1))
layout(matrix(c(1,2),
              ncol = 2, nrow = 1), heights = c(1,1))
plot(x1, c(a,b), col = c("hotpink","chartreuse4"),
     pch = 16, cex = 2,
     xlim = c(0.5,2.5),ylim = c(0,.13),
     ylab = "Effect of Partner Diversity",
     xlab = "No Ant Correlation")
legend("right",legend = c("Non-Synchronous","Synchronous"), fill = c("chartreuse4","hotpink"))
plot(x1, c(d,c), col = c("hotpink","chartreuse4"),
     pch = 16, cex = 2,
     xlim = c(0.5,2.5),ylim = c(0,.13),
     ylab = "Effect of Partner Diversity",
     xlab = "Negative Ant Correlation")
dev.off()




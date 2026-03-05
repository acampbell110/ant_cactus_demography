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

# Set up parameters for all models
# Pull out the mean matrix
params_mean<-data.frame(matrix(colMeans(params),1,ncol(params)))
names(params_mean)<-names(params)
scenario = c("none","cremvac","liomvac","othervac","liomcremvac","liomvacother","othercremvac","all")
scenario_abv <- c("V","C","L","O","LC","LO","OC","LOC")
colors_lambdas <- c(vcol,ccol,lcol,ocol,lccol,locol,cocol,acol)
x_vals <- c(1,3,5,7,9,11,13,15)
sims <- c("comp","equal","freq")
trans_years<-c(1:3,10:15,18:19);(2004:2023)[trans_years]
max_yrs<-1000
year_seq<-sample(trans_years,max_yrs,replace=T)



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




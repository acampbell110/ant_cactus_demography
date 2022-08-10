##########Fake transition function
transition <- function(x,i,j,params,scenario){
  occ_occ <- rep(0.5,length(x))
  occ_vac <- rep(0.5,length(x))
  vac_occ <- rep(0.5,length(x))
  vac_vac <- rep(0.5,length(x))
  #Return them
  if(i == "occ" & j == "occ"){return(occ_occ)}
  if(i == "occ" & j == "vacant"){return(occ_vac)}
  if(i == "vacant" & j == "occ"){return(vac_occ)}
  if(i == "vacant" & j == "vacant"){return(vac_vac)}
}

transition(c(1,2,3,4,5),"occ","vacant",params,"liomvac")



########## Try with a more basic bigmat 2 to get the survival to plot right
bigmatrix.2 <- function(params,lower,upper,matsize,i,j,scenario){
  ###################################################################################################
  ## returns the full IPM kernel (to be used in stochastic simulation), the F and T kernels, and meshpoints in the units of size
  ## params,yrfx,plotfx, and mwye get passed to the vital rate functions
  ## f.eps is fertility overdispersion. defaults to zero (see lambda.fun())
  ## lower and upper are the integration limits
  ## matsize is the dimension of the approximating matrix (it gets an additional 2 rows and columns for the seed banks)
  ###################################################################################################
  n<-matsize
  L<-lower; U<-upper
  h<-(U-L)/n                   #Bin size
  b<-L+c(0:n)*h;               #Lower boundaries of bins 
  y<-0.5*(b[1:n]+b[2:(n+1)]);  #Bin midpoints
  
  # Fertility matricies -- Two Ant
  Fmat <- matrix(0,(2*n+2),(2*n+2))
  # Growth/survival transition matricies -- Two Ant
  Tmat <- matrix(0,(2*n+2),(2*n+2))
  ## Full Matricies
  IPMmat <- matrix()
  ############################################# LIOM ############################################
  if(scenario == "liomvac"){
    # Banked seeds go in top row (1 == liom, 2 == vacant)
    Fmat[1,3:(n+2)]<-fx(y,"liom",params)
    Fmat[1,(n+3):(2*n+2)]<-fx(y,"vacant",params)
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    #    Tmat[2,1]<-1-invlogit(mean(params$germ1_beta0))
    Tmat[2,1]<-1-1
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
    #    Tmat[3:(n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
    #    Tmat[(n+3):(2*n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
    Tmat[3:(n+2),1]<-1*recruits(y,params)*h*1
    Tmat[(n+3):(2*n+2),1]<-1*recruits(y,params)*h*1
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
    #    Tmat[3:(n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
    #    Tmat[(n+3):(2*n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
    Tmat[3:(n+2),2]<-1*recruits(y,params)*h*1 ## size distribution of non-vacant recruits
    Tmat[(n+3):(2*n+2),2]<-1*recruits(y,params)*h*1 ## size distribution of vacant recruits
    # Growth/survival transitions among cts sizes
    #    Tmat[3:(n+2),3:(n+2)]<-t(outer(y,y,ptxy,i = "liom",j = "liom",params,"liomvac"))*h
    Tmat[3:(n+2),3:(n+2)]<-t(outer(y,y,pxy,i = "liom",params))*h*diag(transition(y,"occ","occ",params,"liomvac")) ## but will only work if transition.x is vectorized
    Tmat[3:(n+2),(n+3):(2*n+2)]<-t(outer(y,y,pxy,i = "liom",params))*h*diag(transition(y,i = "occ",j = "vacant",params,"liomvac"))
    Tmat[(n+3):(2*n+2),3:(n+2)]<-t(outer(y,y,pxy,i = "vacant",params))*h*diag(transition(y,i = "vacant",j = "occ",params,"liomvac"))
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<-t(outer(y,y,pxy,i = "vacant",params))*h*diag(transition(y,i = "vacant",j = "vacant",params,"liomvac"))
    # Put it all together
    IPMmat<-Fmat+Tmat
    # Calculate the lambda
    # lambda = Re(eigen(IPMmat)$values[1])
    # return(lambda)
    return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
  }
}
## liom liom
testmat <- bigmatrix.2(params,lower=cholla_min-15,upper=cholla_max+2,matsize,"vacant","liom","liomvac")$Tmat
surva <- colSums(testmat[3:(n+2),3:(n+2)])
plot(surva,ylim = c(0,1), type = "l")
## All together
surv <- colSums(testmat)
plot(surv,ylim = c(0,1))



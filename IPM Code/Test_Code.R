##########Fake transition function
transition <- function(x,i,j,params,scenario){
  occ1_occ1 <- rep(0.25,length(x))
  occ1_occ2 <- rep(0.25,length(x))
  occ1_vac <- rep(0.5,length(x))
  vac_occ1 <- rep(0.25,length(x))
  vac_occ2 <- rep(0.25,length(x))
  vac_vac <- rep(0.5,length(x))
  occ2_occ1 <- rep(0.25,length(x))
  occ2_occ2 <- rep(0.25,length(x))
  occ2_vac <- rep(0.5,length(x))
  #Return them
  if(i == "occ1" & j == "occ1"){return(occ1_occ1)}
  if(i == "occ1" & j == "occ2"){return(occ1_occ2)}
  if(i == "occ1" & j == "vacant"){return(occ1_vac)}
  if(i == "vacant" & j == "occ1"){return(vac_occ1)}
  if(i == "vacant" & j == "occ2"){return(vac_occ2)}
  if(i == "vacant" & j == "vacant"){return(vac_vac)}
  if(i == "occ2" & j == "occ1"){return(occ2_occ1)}
  if(i == "occ2" & j == "occ2"){return(occ2_occ2)}
  if(i == "occ2" & j == "vacant"){return(occ2_vac)}
}

transition(c(1,2,3,4,5),"occ1","vacant",params,"liomvac")
x = c(1,1,1)
i = c("vacant","vacant","vacant")
j = c("occ1","occ2","vacant")
scenario = "liomcremvac"
t2 <- vector()
for(n in 1:length(i)){
  t2[n] <- transition(x[n],i[n],j[n],params,scenario)
}
t2
sum(t2)


########## Try with a more basic bigmat 2 to get the survival to plot right
bigmatrix <- function(params,lower,upper,matsize,i,j,scenario){
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
  Fmat <- matrix(0,(3*n+2),(3*n+2))
  # Growth/survival transition matricies -- Two Ant
  Tmat <- matrix(0,(3*n+2),(3*n+2))
  ## Full Matricies
  IPMmat <- matrix()
  ############################################# LIOM ############################################
  if(scenario == "liomvac"){
    ## Fecundity of plants
    Fmat[1,3:(n+2)]<-fx(y,params=params,"crem") ## Production of seeds from x sized mom with no ant visitor
    Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"liom") ## Production of seeds from x sized mom with ant visitor
    Fmat[1,(2*n+3):(3*n+2)]<-fx(y,params=params,"vacant")
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    #Tmat[2,1]<-1-invlogit(mean(params$germ1_beta0))
    Tmat[2,1]<-1-1
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-0
    Tmat[(n+3):(2*n+2),1]<-0
    Tmat[(2*n+3):(3*n+2),1]<-1*recruits(y,params)*h*1
    #Tmat[(2*n+3):(3*n+2),1]<-invlogit(mean(params$germ1_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-0
    Tmat[(n+3):(2*n+2),2]<-0
    Tmat[(2*n+3):(3*n+2),2]<-1*recruits(y,params)*h*1
    #Tmat[(2*n+3):(3*n+2),2]<-invlogit(mean(params$germ2_beta0))*recruits(y,params)*h*invlogit(mean(params$preseed_beta0))
    # Growth/survival transitions among cts sizes
    ##Top Row
    Tmat[3:(n+2),3:(n+2)]<- (t(outer(y,y,pxy,"crem",params))*h)%*%diag(transition(y,i = "occ1",j = "occ1",params,"liomcremvac")) ## Top First
    Tmat[3:(n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,"crem",params))*h)%*%diag(transition(y,i = "occ1",j = "occ2",params,"liomcremvac"))   ## Top Second
    Tmat[3:(n+2),(2*n+3):(3*n+2)]<-(t(outer(y,y,pxy,"crem",params))*h)%*%diag(transition(y,i = "occ1",j = "vacant",params,"liomcremvac"))   ## Top Third
    ##Middle Row
    Tmat[(n+3):(2*n+2),3:(n+2)]<- (t(outer(y,y,pxy,"liom",params))*h)%*%diag(transition(y,i = "occ2",j = "occ1",params,"liomcremvac"))   ## Middle First
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- (t(outer(y,y,pxy,"liom",params))*h)%*%diag(transition(y,i = "occ2",j = "occ2",params,"liomcremvac"))   ## Middle Second
    Tmat[(n+3):(2*n+2),(2*n+3):(3*n+2)]<- (t(outer(y,y,pxy,"liom",params))*h)%*%diag(transition(y,i = "occ2",j = "vacant",params,"liomcremvac"))   ## Middle Third
    ##Bottom Row
    Tmat[(2*n+3):(3*n+2),3:(n+2)]<- (t(outer(y,y,pxy,"vacant",params))*h)%*%diag(transition(y,i = "vacant",j = "occ1",params,"liomcremvac"))   ## Bottom First
    Tmat[(2*n+3):(3*n+2),(n+3):(2*n+2)]<- (t(outer(y,y,pxy,"vacant",params))*h)%*%diag(transition(y,i = "vacant",j = "occ2",params,"liomcremvac"))   ## Bottom Second
    Tmat[(2*n+3):(3*n+2),(2*n+3):(3*n+2)]<- (t(outer(y,y,pxy,"vacant",params))*h)%*%diag(transition(y,i = "vacant",j = "vacant",params,"liomcremvac"))   ## Bottom Third
    # Put it all together
    IPMmat<-Fmat+Tmat
    # lambda = Re(eigen(IPMmat)$values[1])
    # return(lambda)
    return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
  }
}
## liom liom
testmat <- bigmatrix(params,lower=cholla_min-20,upper=cholla_max+2,matsize,"vacant","liom","liomvac")$Tmat
surva <- colSums(testmat)
plot(surva,ylim = c(0,10), type = "l")
## All together
surv <- colSums(testmat)
plot(surv,ylim = c(0,1))



bigmatrix <- function(params,lower,upper,matsize,i,j,scenario){
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
  Fmat <- matrix(0,(3*n+2),(3*n+2))
  # Growth/survival transition matricies -- Two Ant
  Tmat <- matrix(0,(3*n+2),(3*n+2))
  ## Full Matricies
  IPMmat <- matrix()
  ## Scenario = othercremvac
  # Banked seeds go in top row 
  Fmat[1,3:(n+2)]<-fx(y,"vacant",params)
  Fmat[1,(n+3):(2*n+2)]<-fx(y,"crem",params)
  Fmat[1,(2*n+3):(3*n+2)]<-fx(y,"other",params)
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  Tmat[2,1]<-1-1
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
  Tmat[3:(n+2),1]<-recruits(y,params)*h
  Tmat[(n+3):(2*n+2),1]<-0
  Tmat[(2*n+3):(3*n+2),1]<-0
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
  Tmat[3:(n+2),2]<-recruits(y,params)*h
  Tmat[(n+3):(2*n+2),2]<-0
  Tmat[(2*n+3):(3*n+2),2]<-0
  # Growth/survival transitions based on previous ant and size
  ## Top row
  Tmat[3:(n+2),3:(n+2)]<-(t(outer(y,y,pxy,i="vacant",params))*h)%*%diag(transition.x(y,i="vacant",j="vacant",params,"othercremvac"))
  Tmat[3:(n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,i="vacant",params))*h)%*%diag(transition.x(y,i="vacant",j="crem",params,"othercremvac"))
  Tmat[3:(n+2),(2*n+3):(3*n+2)]<-(t(outer(y,y,pxy,i="vacant",params))*h)%*%diag(transition.x(y,i="vacant",j="other",params,"othercremvac"))
  ## Middle row
  Tmat[(n+3):(2*n+2),3:(n+2)]<-(t(outer(y,y,pxy,i="crem",params))*h)%*%diag(transition.x(y,i="crem",j="vacant",params,"othercremvac"))
  Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,i="crem",params))*h)%*%diag(transition.x(y,i="crem",j="crem",params,"othercremvac"))
  Tmat[(n+3):(2*n+2),(2*n+3):(3*n+2)]<-(t(outer(y,y,pxy,i="crem",params))*h)%*%diag(transition.x(y,i="crem",j="other",params,"othercremvac"))
  ## Bottom row
  Tmat[(2*n+3):(3*n+2),3:(n+2)]<-(t(outer(y,y,pxy,i="other",params))*h)%*%diag(transition.x(y,i="other",j="vacant",params,"othercremvac"))
  Tmat[(2*n+3):(3*n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,i="other",params))*h)%*%diag(transition.x(y,i="other",j="crem",params,"othercremvac"))
  Tmat[(2*n+3):(3*n+2),(2*n+3):(3*n+2)]<-(t(outer(y,y,pxy,i="other",params))*h)%*%diag(transition.x(y,i="other",j="other",params,"othercremvac"))
  # Put it all together
  IPMmat<-Fmat+Tmat
  # Calculate the lambda
  # lambda = Re(eigen(IPMmat)$values[1])
  # return(lambda)
  return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
}
testmat <- bigmatrix(params,lower=cholla_min-20,upper=cholla_max+2,matsize,"crem","vacant","othercremvac")$Tmat
surv <- colSums(testmat)
plot(surv,ylim = c(0,10))


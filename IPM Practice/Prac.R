setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Practice")
#########################################################################################################
##            This will be an IPM which only includes one ant state -- Crem.
#########################################################################################################

## ----------- Miscellany...we'll need an inverse logit functions ------------- ##
invlogit<-function(x){exp(x)/(1+exp(x))}

## ----------- Vital rate functions. Parameter indices are hard-coded and must correspond to rows of MCMC matrix ------------- ##
#GROWTH FROM SIZE X TO Y
gxy<-function(x,y,params){
  xb=pmin(pmax(x,cholla_min),cholla_max) #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  g_c = dnorm(y,mean=params[1] + params[2]*xb,sd=params[3])
  g_l = dnorm(y,mean=params[101] + params[102]*xb,sd=params[3])
  g_o = dnorm(y,mean=params[201] + params[202]*xb,sd=params[3])
  g_v = dnorm(y,mean=params[301] + params[302]*xb,sd=params[3])
  return(list(g_c=g_c, g_l=g_l, g_o=g_o, g_v=g_v))
}
g <- list()
for(i in 1:10){
  params = cholla[,i]
  g[[i]]<-gxy(x,y,params)
}

#SURVIVAL AT SIZE X.
sx<-function(x,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  s_c = invlogit(params[11] + params[12]*xb)
  s_l = invlogit(params[111] + params[112]*xb)
  s_o = invlogit(params[211] + params[212]*xb)
  s_v = invlogit(params[311] + params[312]*xb)
  return(list(s_c=s_c, s_l=s_l, s_o=s_o, s_v=s_v))
}
s <- list()
for(i in 1:10){
  params = cholla[,i]
  s[[i]]<-sx(x,params)
}

#SURVIVAL*GROWTH
pxy<-function(x,y,params,i){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  ifelse(
    i == "crem",
    return(sx(xb, params)$s_c*gxy(xb,y,params)$g_c),
    ifelse(
      i == "liom",
      return(sx(xb, params)$s_l*gxy(xb,y,params)$g_l),
      ifelse(
        i == "other",
        return(sx(xb, params)$s_o*gxy(xb,y,params)$g_o),
        return(sx(xb, params)$s_v*gxy(xb,y,params)$g_v)
      )
    )
  )
}
p <- list()
for(i in 1:10){
  params = cholla[,i]
  p[[i]]<-pxy(x,y,params,"crem")
}

#PRODUCTION OF 1-YO SEEDS IN THE SEED BANK FROM X-SIZED MOMS
fx<-function(x,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)        ## X dummy variable
  p.flow<-invlogit(params[31] + params[32]*xb) 
  nflow<-exp(params[21] + params[22]*xb)          ## Number of FLowers produced
  flow.surv_c<-invlogit(params[41]) ## Proportion of Flowers survive to fruit
  flow.surv_l<-invlogit(params[141]) ## Proportion of Flowers survive to fruit
  flow.surv_o<-invlogit(params[241]) ## Proportion of Flowers survive to fruit
  flow.surv_v<-invlogit(params[341]) ## Proportion of Flowers survive to fruit
  seeds.per.fruit<-params[51]                     ## Number of Seeds per Fruit
  seed.survival<-invlogit(params[61])^2           ## Seed per Fruit Survival ---------I measured 6-month seed survival; annual survival is its square
  f_c = p.flow*nflow*flow.surv_c*seeds.per.fruit*seed.survival
  f_l = p.flow*nflow*flow.surv_l*seeds.per.fruit*seed.survival
  f_o = p.flow*nflow*flow.surv_o*seeds.per.fruit*seed.survival
  f_v = p.flow*nflow*flow.surv_v*seeds.per.fruit*seed.survival
  return(list(f_c=f_c, f_l=f_l, f_o=f_o, f_v=f_v))  
}
f <- list()
for(i in 1:10){
  params = cholla[,i]
  f[[i]]<-fx(x,params)
}

recruits<-function(y,params){
  yb=pmin(pmax(y,cholla_min),cholla_max)
  dnorm(yb, (cholla[96]),cholla[97])
}
r <- list()
for(i in 1:10){
  params = cholla[,i]
  r[[i]]<-recruits(y,params)
}

#beta<-function(vac_rec){
#  ifelse(vac_rec == TRUE, return(0), return(1))
#}
#b <- list()
#for(i in 1:10){
#  params = cholla[,i]
#  b[[i]]<-beta(vac_rec)
#}

bigmatrix<-function(params,lower,upper,matsize){  
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
  y_list = rep(y,100)
  xb=pmin(pmax(y,cholla_min),cholla_max)
  ##################################################### CREM ######################################
  # Fertility matrix
  Fmat_c<-matrix(0,(n+2),(n+2))
  # Banked seeds go in top row
  Fmat_c[1,3:(n+2)]<-fx(y,params)$f_c
  # Growth/survival transition matrix
  Tmat_c<-matrix(0,(n+2),(n+2))
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  Tmat_c[2,1]<-1-invlogit(params[71])
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  Tmat_c[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)   
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  Tmat_c[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)  
  # Growth/survival transitions among cts sizes
  Tmat_c[3:(n+2),3:(n+2)]<-t(outer(y,y,pxy,params=params,i="crem"))*h
  # Put it all together
  IPMmat_c<-Fmat_c+Tmat_c     
  ##################################################### LIOM ######################################
  # Fertility matrix
  Fmat_l<-matrix(0,(n+2),(n+2))
  # Banked seeds go in top row
  Fmat_l[1,3:(n+2)]<-fx(y,params)$f_l
  # Growth/survival transition matrix
  Tmat_l<-matrix(0,(n+2),(n+2))
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  Tmat_l[2,1]<-1-invlogit(params[71])
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  Tmat_l[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)   
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  Tmat_l[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)  
  # Growth/survival transitions among cts sizes
  Tmat_l[3:(n+2),3:(n+2)]<-t(outer(y,y,pxy,params=params,i="liom"))*h
  # Put it all together
  IPMmat_l<-Fmat_l+Tmat_l    
  ##################################################### OTHER ######################################
  # Fertility matrix
  Fmat_o<-matrix(0,(n+2),(n+2))
  # Banked seeds go in top row
  Fmat_o[1,3:(n+2)]<-fx(y,params)$f_o
  # Growth/survival transition matrix
  Tmat_o<-matrix(0,(n+2),(n+2))
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  Tmat_o[2,1]<-1-invlogit(params[71])
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  Tmat_o[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)   
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  Tmat_o[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)  
  # Growth/survival transitions among cts sizes
  Tmat_o[3:(n+2),3:(n+2)]<-t(outer(y,y,pxy,params=params,i="other"))*h
  # Put it all together
  IPMmat_o<-Fmat_o+Tmat_o    
  ##################################################### VAC ######################################
  # Fertility matrix
  Fmat_v<-matrix(0,(n+2),(n+2))
  # Banked seeds go in top row
  Fmat_v[1,3:(n+2)]<-fx(y,params)$f_v
  # Growth/survival transition matrix
  Tmat_v<-matrix(0,(n+2),(n+2))
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  Tmat_v[2,1]<-1-invlogit(params[71])
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  Tmat_v[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)   
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  Tmat_v[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)  
  # Growth/survival transitions among cts sizes
  Tmat_v[3:(n+2),3:(n+2)]<-t(outer(y,y,pxy,params=params,i="vac"))*h
  # Put it all together
  IPMmat_v<-Fmat_v+Tmat_v    
  ##### Returns
  return(list(IPMmat_c=IPMmat_c, IPMmat_l=IPMmat_l, IPMmat_o=IPMmat_o, IPMmat_v=IPMmat_v))
}

bigmatrix(params, lower, upper, matsize)

big_list_c <- list()
big_list_l <- list()
big_list_o <- list()
big_list_v <- list()
lambda_c <- vector()
lambda_l <- vector()
lambda_o <- vector()
lambda_v <- vector()
stable_c <- list()
stable_l <- list()
stable_o <- list()
stable_v <- list()
for(i in 1:100){
  params = cholla[,i]
  big_list_c[[i]]<-bigmatrix(params, lower, upper, matsize)$IPMmat_c
  big_list_l[[i]]<-bigmatrix(params, lower, upper, matsize)$IPMmat_l
  big_list_o[[i]]<-bigmatrix(params, lower, upper, matsize)$IPMmat_o
  big_list_v[[i]]<-bigmatrix(params, lower, upper, matsize)$IPMmat_v
  mat_c <- big_list_c[[i]]
  mat_l <- big_list_l[[i]]
  mat_o <- big_list_o[[i]]
  mat_v <- big_list_v[[i]]
  eig_c <- eigen(mat_c)
  eig_l <- eigen(mat_l)
  eig_o <- eigen(mat_o)
  eig_v <- eigen(mat_v)
  lambda_c[i]<-Re(eig_c$values[1])
  lambda_l[i]<-Re(eig_l$values[1])
  lambda_o[i]<-Re(eig_o$values[1])
  lambda_v[i]<-Re(eig_v$values[1])
  
  stable_c[[i]] <- stable.stage(mat_c)
  stable_l[[i]] <- stable.stage(mat_l)
  stable_o[[i]] <- stable.stage(mat_o)
  stable_v[[i]] <- stable.stage(mat_v)
}

  hist(lambda_c)
  plot(density(lambda_l))
  plot(density(lambda_v))
  
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
  png("Lambda_Dens_Dist.png")
  plot(density(lambda_c), col = "red", xlim = c(0.975, 1), ylim = c(0, 13000), main = "Lambda Density Distributions", xlab = "Lambda", ylab = "Density")
  lines(density(lambda_l), col = "blue")
  lines(density(lambda_o), col = "pink")
  lines(density(lambda_v), col = "black")
  legend(0.975, 13000, legend = c("Crem.","Liom.","Other","Vac."), col = c("red","blue","pink","black"), lty = 1)
  dev.off()
  
  
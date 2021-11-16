#########################################################################################################
##            This will be an IPM which allows you to choose how many ants are present
#########################################################################################################
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code")
source("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code/Params.R")
## ----------- Miscellany...we'll need an inverse logit functions ------------- ##
invlogit<-function(x){exp(x)/(1+exp(x))}

## ----------- Vital rate functions. Parameter indices are hard-coded and must correspond to rows of MCMC matrix ------------- ##
#GROWTH FROM SIZE X TO Y
gxy<-function(x,y,params){
  xb=pmin(pmax(x,cholla_min),cholla_max) #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  g_crem = dnorm(y,mean=params[1] + params[2]*xb,sd=params[3])
  g_vac = dnorm(y,mean=params[301] + params[302]*xb,sd=params[3])
  g_other = dnorm(y,mean=params[201] + params[202]*xb,sd=params[3])
  g_liom = dnorm(y,mean=params[101] + params[102]*xb,sd=params[3])
  return(list(g_crem=g_crem, g_liom=g_liom, g_other=g_other, g_vac=g_vac))
}


#SURVIVAL AT SIZE X.
sx<-function(x,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  s_crem = invlogit(params[11] + params[12]*xb)
  s_vac = invlogit(params[311] + params[312]*xb)
  s_other = invlogit(params[211] + params[212]*xb)
  s_liom = invlogit(params[111] + params[112]*xb)
  return(list(s_crem=s_crem, s_liom=s_liom, s_other=s_other, s_vac=s_vac))
}


#SURVIVAL*GROWTH
pxy<-function(x,y,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  p_crem = sx(x,params)$s_crem * gxy(x,y,params)$g_crem
  p_vac = sx(x,params)$s_vac * gxy(x,y,params)$g_vac
  p_other = sx(x,params)$s_other * gxy(x,y,params)$g_other
  p_liom = sx(x,params)$s_liom * gxy(x,y,params)$g_liom
  return(list(p_crem=p_crem, p_liom=p_liom, p_other=p_other, p_vac=p_vac))
}


#PRODUCTION OF 1-YO SEEDS IN THE SEED BANK FROM X-SIZED MOMS
fx<-function(x,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  p.flow<-invlogit(params[31] + params[32]*xb)    ## Probability of Reproducing
  nflow<-exp(params[21] + params[22]*xb)          ## Number of FLowers produced
  flow.surv_crem<-invlogit(params[41]) ## Proportion of Flowers survive to fruit
  flow.surv_vac<-invlogit(params[341]) ## Proportion of Flowers survive to fruit
  flow.surv_other<-invlogit(params[241]) ## Proportion of Flowers survive to fruit
  flow.surv_liom<-invlogit(params[141]) ## Proportion of Flowers survive to fruit
  seeds.per.fruit<-params[51]                     ## Number of Seeds per Fruit
  seed.survival<-invlogit(params[61])^2           ## Seed per Fruit Survival ---------I measured 6-month seed survival; annual survival is its square
  f_crem = p.flow*nflow*flow.surv_crem*seeds.per.fruit*seed.survival
  f_vac = p.flow*nflow*flow.surv_vac*seeds.per.fruit*seed.survival
  f_other = p.flow*nflow*flow.surv_other*seeds.per.fruit*seed.survival
  f_liom = p.flow*nflow*flow.surv_liom*seeds.per.fruit*seed.survival
  return(list(f_crem=f_crem, f_liom=f_liom, f_other=f_other, f_vac=f_vac))
}

fx(4,cholla)

#
recruits<-function(y,params){
  yb=pmin(pmax(x,cholla_min),cholla_max)
  dnorm(yb, (params[96]),params[97])
}


beta<-function(vac_rec){
  ifelse(vac_rec == TRUE, return(0), return(1))
}


#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE (OCC VS VAC)
transition.2<-function(x, i, j, params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
      occ_occ = (invlogit(params[108] + params[109]*xb)) 
      occ_vac = (1 - invlogit(params[108] + params[109]*xb)) 
      vac_occ = (invlogit(params[98] + params[99]*xb)) 
      vac_vac = (1 - invlogit(params[98] + params[99]*xb)) 
      ## Return the probabilities
      ifelse((i == "crem" | i == "liom" | i == "other") & (j == "crem" | j == "liom" | j == "other"), return(occ_occ),
             ifelse((i == "crem" | i == "liom" | i == "other") & j == "vac", return(occ_vac),
                    ifelse(i == "vac" & j == "vac", return(vac_vac),
                           return(vac_occ)
                    )
             )
      ) 
}
transition.2(x = 4, i = "vac",j = "crem", params = cholla)


#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE (THREE STATES)
transition.3<-function(x, i, j, params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  ##YES 4 ants -> Calculate the probabilities
  crem_crem = 5 
  crem_liom = 5 
  crem_other = 5 
  crem_vac = 5 
  liom_crem = 4 
  liom_liom = 4 
  liom_other = 4 
  liom_vac = 4 
  other_crem  = 3 
  other_liom = 3 
  other_other = 3 
  other_vac = 3 
  vac_crem = 2 
  vac_liom = 2 
  vac_other = 2 
  vac_vac = 2 
  ## Return the probabilities
  ifelse(i == "crem",
         ##YES CREM
         ifelse(j == "crem",
                ## YES J CREM
                return(crem_crem),
                ## NO J CREM
                ifelse(j == "liom",
                       ##YES J LIOM
                       return(crem_liom),
                       ##NO J LIOM
                       ifelse(j == "other",
                              ##YES J OTHER
                              return(crem_other),
                              ##NO J OTHER
                              return(crem_vac)
                       )
                )
         ),
         ##NO CREM
         ifelse(i == "liom",
                ## YES LIOM
                ifelse(j == "crem",
                       ##YES J CREM
                       return(liom_crem),
                       ##NO J CREM
                       ifelse(j == "liom",
                              ##YES J LIOM
                              return(liom_liom),
                              ##NO J LIOM
                              ifelse(j == "other",
                                     ##YES J OTHER
                                     return(liom_other),
                                     ##NO J OTHER
                                     return(liom_vac)
                              )
                       )
                ),
                ##NO LIOM
                ifelse(i == "other",
                       ##YES OTHER
                       ifelse(j == "crem",
                              ##YES J CREM
                              return(other_crem),
                              ##NO J CREM
                              ifelse(j == "liom",
                                     ##YES J LIOM
                                     return(other_liom),
                                     ##NO J LIOM
                                     ifelse(j == "other",
                                            ##YES J OTHER
                                            return(other_other),
                                            ##NO J OTHER
                                            return(other_vac)
                                     )
                              )
                       ),
                       ##NO OTHER
                       ifelse(i == "vac",
                              ##YES VAC
                              ifelse(j == "crem",
                                     ##YES J CREM
                                     return(vac_crem),
                                     ##NO J CREM
                                     ifelse(j == "liom",
                                            ##YES J LIOM
                                            return(vac_liom),
                                            ##NO J LIOM
                                            ifelse(j == "other",
                                                   ##YES J OTHER
                                                   return(vac_other),
                                                   ##NO J OTHER
                                                   return(vac_vac)
                                            )
                                     )
                              ),
                              ##NO VAC
                              
                       )
                )
         )
  )
}
transition.3(x = 4, i = "crem",j = "other", params = cholla)


#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE (ALL STATES)
transition.4<-function(x, i, j, params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  ##YES 4 ants -> Calculate the probabilities
  crem_crem = 5 
    crem_liom = 5 
    crem_other = 5 
    crem_vac = 5 
    liom_crem = 4 
    liom_liom = 4 
    liom_other = 4 
    liom_vac = 4 
    other_crem  = 3 
    other_liom = 3 
    other_other = 3 
    other_vac = 3 
    vac_crem = 2 
    vac_liom = 2 
    vac_other = 2 
    vac_vac = 2 
    ## Return the probabilities
    ifelse(i == "crem",
           ##YES CREM
           ifelse(j == "crem",
                  ## YES J CREM
                  return(crem_crem),
                  ## NO J CREM
                  ifelse(j == "liom",
                         ##YES J LIOM
                         return(crem_liom),
                         ##NO J LIOM
                         ifelse(j == "other",
                                ##YES J OTHER
                                return(crem_other),
                                ##NO J OTHER
                                return(crem_vac)
                         )
                  )
           ),
           ##NO CREM
           ifelse(i == "liom",
                  ## YES LIOM
                  ifelse(j == "crem",
                         ##YES J CREM
                         return(liom_crem),
                         ##NO J CREM
                         ifelse(j == "liom",
                                ##YES J LIOM
                                return(liom_liom),
                                ##NO J LIOM
                                ifelse(j == "other",
                                       ##YES J OTHER
                                       return(liom_other),
                                       ##NO J OTHER
                                       return(liom_vac)
                                )
                         )
                  ),
                  ##NO LIOM
                  ifelse(i == "other",
                         ##YES OTHER
                         ifelse(j == "crem",
                                ##YES J CREM
                                return(other_crem),
                                ##NO J CREM
                                ifelse(j == "liom",
                                       ##YES J LIOM
                                       return(other_liom),
                                       ##NO J LIOM
                                       ifelse(j == "other",
                                              ##YES J OTHER
                                              return(other_other),
                                              ##NO J OTHER
                                              return(other_vac)
                                       )
                                )
                         ),
                         ##NO OTHER
                         ifelse(i == "vac",
                                ##YES VAC
                                ifelse(j == "crem",
                                       ##YES J CREM
                                       return(vac_crem),
                                       ##NO J CREM
                                       ifelse(j == "liom",
                                              ##YES J LIOM
                                              return(vac_liom),
                                              ##NO J LIOM
                                              ifelse(j == "other",
                                                     ##YES J OTHER
                                                     return(vac_other),
                                                     ##NO J OTHER
                                                     return(vac_vac)
                                              )
                                       )
                                ),
                                ##NO VAC
                                
                         )
                  )
           )
    )
}
transition.4(x = 4, i = "crem",j = "other", params = cholla)

#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE 
transition.x <- function(x,i,j,num_ants,params){
  ifelse(num_ants == 2, transition.2(x,i,j,params),
         ifelse(num_ants == 3, transition.3(x,i,j,params),
         transition.4(x,i,j,params)))
}

transition.x(x = 4, i = "crem", j = "other", num_ants = 4, params = cholla)

#GROWTH*SURVIVAL*ANT PROBABILITIES
ptxy <- function(x,y,i,j,num_ants,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  sx(xb,params)$s_vac*gxy(xb,y,params)$g_vac*transition.x(xb,i,j,num_ants,params)
}
ptxy(x=4,y=5,i="liom",j="other",num_ants=3,params=cholla)

#GROWTH*SURVIVAL - ONE ANT ONLY
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

bigmatrix.1 <- function(x,y,params,lower,upper,matsize,num_ants){
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
  
  # Fertility matricies -- One Ant
  Fmat_c<-matrix(0,(n+2),(n+2))
  Fmat_l<-matrix(0,(n+2),(n+2))
  Fmat_o<-matrix(0,(n+2),(n+2))
  Fmat_v<-matrix(0,(n+2),(n+2))
  # Growth/survival transition matricies -- One Ant
  Tmat_c<-matrix(0,(n+2),(n+2))
  Tmat_l<-matrix(0,(n+2),(n+2))
  Tmat_o<-matrix(0,(n+2),(n+2))
  Tmat_v<-matrix(0,(n+2),(n+2))
  ## Full Matricies
  IPMmat_c <- matrix()
  IPMmat_l <- matrix()
  IPMmat_o <- matrix()
  IPMmat_v <- matrix()
  
    ## CREM
    # Banked seeds go in top row
    Fmat_c[1,3:(n+2)]<-fx(y,params)$f_c 
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
    ## LIOM
    # Banked seeds go in top row
    Fmat_l[1,3:(n+2)]<-fx(y,params)$f_l 
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo) &
    Tmat_l[2,1]<-1-invlogit(params[71]) 
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat_l[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb) 
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat_l[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)   
    # Growth/survival transitions among cts size
    Tmat_l[3:(n+2),3:(n+2)]<-t(outer(y,y,pxy,params=params,i="liom"))*h 
    # Put it all together
    IPMmat_l<-Fmat_l+Tmat_l 
    ## OTHER
    # Banked seeds go in top row
    Fmat_o[1,3:(n+2)]<-fx(y,params)$f_o 
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
    ## VACAN
    # Banked seeds go in top row
    Fmat_v[1,3:(n+2)]<-fx(y,params)$f_v 
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
  return(list(IPMmat_c=IPMmat_c, IPMmat_l=IPMmat_l, IPMmat_o=IPMmat_o, IPMmat_v=IPMmat_v))
}
bigmatrix.1(4,5,cholla,lower,upper,matsize,1)

bigmatrix.2 <- function(x,y,params,lower,upper,matsize,num_ants){
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
  Fmat_vc <- matrix(0,(2*n+2),(2*n+2))
  Fmat_vl <- matrix(0,(2*n+2),(2*n+2))
  Fmat_vo <- matrix(0,(2*n+2),(2*n+2))
  # Growth/survival transition matricies -- Two Ant
  Tmat_vc <- matrix(0,(2*n+2),(2*n+2))
  Tmat_vl <- matrix(0,(2*n+2),(2*n+2))
  Tmat_vo <- matrix(0,(2*n+2),(2*n+2))
  ## Full Matricies
  IPMmat_vc <- matrix()
  IPMmat_vl <- matrix()
  IPMmat_vo <- matrix()
  
  # Banked Seeds go in top row
  fec1_v<-fx(y,params=params)$f_vac & 
    fec1_c<-fx(y,params=params)$f_crem & 
    Fmat_vc[1,3:(n+2)]<-fec1_c & 
    Fmat_vc[1,(n+3):(2*n+2)]<-fec1_v &
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat_vc[2,1]<-1-invlogit(params[71]) & 
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    #Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)  
    beta2_c<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE) &
    beta2_v<- invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE) &
    Tmat_vc[(n+3):(2*n+2),1]<-beta2_v &
    Tmat_vc[(n+3):(2*n+2),2]<-beta2_c &
    # Growth/survival transitions among cts sizes
    vac_vac<-t(outer(y,y,ptxy,i="vac",j="vac",num_ants=2,params=params))*h &
    vac_crem<-t(outer(y,y,ptxy,i="vac",j="crem",num_ants=2,params=params))*h &
    crem_vac<-t(outer(y,y,ptxy,i="crem",j="vac",num_ants=2,params=params))*h &
    crem_crem<-t(outer(y,y,ptxy,i="crem",j="crem",num_ants=2,params=params))*h &
    Tmat_vc[3:(n+2),3:(n+2)]<- vac_vac &  ## Top left 
    Tmat_vc[3:(n+2),(n+3):(2*n+2)]<-crem_vac &  ## Top Right 
    Tmat_vc[(n+3):(2*n+2),3:(n+2)]<- crem_crem &  ## Bottom Right 
    Tmat_vc[(n+3):(2*n+2),(n+3):(2*n+2)]<- vac_crem &  ## Bottom Right 
    # Put it all together
    IPMmat_vc<-Fmat_vc+Tmat_vc & 
    ## LIOM
    # Banked seeds go in top row
    fec1_v<-fx(y,params=params)$f_vac &
    fec1_l<-fx(y,params=params)$f_liom &
    Fmat_vl[1,3:(n+2)]<-fec1_l &
    Fmat_vl[1,(n+3):(2*n+2)]<-fec1_v &
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat_vl[2,1]<-1-invlogit(params[71]) &
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    beta1_l<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE) &
    beta1_v<- invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE) &
    Tmat_vl[3:(n+2),1]<-beta1_v &
    Tmat_vl[3:(n+2),2]<-beta1_l &
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    #Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)  
    beta2_l<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE) &
    beta2_v<- invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE) &
    Tmat_vl[(n+3):(2*n+2),1]<-beta2_v &
    Tmat_vl[(n+3):(2*n+2),2]<-beta2_l &
    # Growth/survival transitions among cts sizes
    vac_vac<-t(outer(y,y,ptxy,i="vac",j="vac",num_ants=2,params=params))*h &
    vac_liom<-t(outer(y,y,ptxy,i="vac",j="liom",num_ants=2,params=params))*h &
    liom_vac<-t(outer(y,y,ptxy,i="liom",j="vac",num_ants=2,params=params))*h &
    liom_liom<-t(outer(y,y,ptxy,i="liom",j="liom",num_ants=2,params=params))*h &
    Tmat_vl[3:(n+2),3:(n+2)]<- vac_vac &  ## Top left 
    Tmat_vl[3:(n+2),(n+3):(2*n+2)]<-liom_vac &  ## Top Right 
    Tmat_vl[(n+3):(2*n+2),3:(n+2)]<- liom_liom &  ## Bottom Right 
    Tmat_vl[(n+3):(2*n+2),(n+3):(2*n+2)]<- vac_liom &  ## Bottom Right 
    # Put it all together
    IPMmat_vl<-Fmat_vl+Tmat_vl &
    ## OTHER
    # Banked seeds go in top row
    fec1_v<-fx(y,params=params)$f_vac &
    fec1_o<-fx(y,params=params)$f_other &
    Fmat_vo[1,3:(n+2)]<-fec1_o &
    Fmat_vo[1,(n+3):(2*n+2)]<-fec1_v &
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat_vo[2,1]<-1-invlogit(params[71]) &
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    beta1_o<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE) &
    beta1_v<- invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE) &
    Tmat_vo[3:(n+2),1]<-beta1_v &
    Tmat_vo[3:(n+2),2]<-beta1_o &
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    #Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)  
    beta2_o<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE) &
    beta2_v<- invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE) &
    Tmat_vo[(n+3):(2*n+2),1]<-beta2_v &
    Tmat_vo[(n+3):(2*n+2),2]<-beta2_o &
    # Growth/survival transitions among cts sizes
    vac_vac<-t(outer(y,y,ptxy,i="vac",j="vac",num_ants=2,params=params))*h &
    vac_other<-t(outer(y,y,ptxy,i="vac",j="other",num_ants=2,params=params))*h &
    other_vac<-t(outer(y,y,ptxy,i="other",j="vac",num_ants=2,params=params))*h &
    other_other<-t(outer(y,y,ptxy,i="other",j="other",num_ants=2,params=params))*h &
    Tmat_vo[3:(n+2),3:(n+2)]<- vac_vac & ## Top left 
    Tmat_vo[3:(n+2),(n+3):(2*n+2)]<-other_vac & ## Top Right 
    Tmat_vo[(n+3):(2*n+2),3:(n+2)]<- other_other & ## Bottom Right 
    Tmat_vo[(n+3):(2*n+2),(n+3):(2*n+2)]<- vac_other & ## Bottom Right 
    # Put it all together
    IPMmat_vo<-Fmat_vo+Tmat_vo
  
}
bigmatrix<-function(x,y,params,lower,upper,matsize,num_ants){  
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
  
  # Fertility matricies -- One Ant
  Fmat_c<-matrix(0,(n+2),(n+2))
  Fmat_l<-matrix(0,(n+2),(n+2))
  Fmat_o<-matrix(0,(n+2),(n+2))
  Fmat_v<-matrix(0,(n+2),(n+2))
  # Fertility matricies -- Two Ant
  Fmat_vc <- matrix(0,(2*n+2),(2*n+2))
  Fmat_vl <- matrix(0,(2*n+2),(2*n+2))
  Fmat_vo <- matrix(0,(2*n+2),(2*n+2))
  # Fertility matricies -- All Ants
  Fmat <- matrix(0,(4*n+2),(4*n+2))
  
  # Growth/survival transition matricies -- One Ant
  Tmat_c<-matrix(0,(n+2),(n+2))
  Tmat_l<-matrix(0,(n+2),(n+2))
  Tmat_o<-matrix(0,(n+2),(n+2))
  Tmat_v<-matrix(0,(n+2),(n+2))
  # Growth/survival transition matricies -- Two Ant
  Tmat_vc <- matrix(0,(2*n+2),(2*n+2))
  Tmat_vl <- matrix(0,(2*n+2),(2*n+2))
  Tmat_vo <- matrix(0,(2*n+2),(2*n+2))
  # Growth/survival transition matricies -- All Ants
  Tmat <- matrix(0,(4*n+2),(4*n+2))
  
  ## Full Matricies
  IPMmat_c <- matrix()
  IPMmat_l <- matrix()
  IPMmat_o <- matrix()
  IPMmat_vc <- matrix()
  IPMmat_vl <- matrix()
  IPMmat_vo <- matrix()
  
  ifelse(num_ants == 1, 
          ## CREM
          # Banked seeds go in top row
          Fmat_c[1,3:(n+2)]<-fx(y,params)$f_c &
          # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
          Tmat_c[2,1]<-1-invlogit(params[71]) &
          # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
          Tmat_c[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)  &  
          # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
          Tmat_c[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)  & 
          # Growth/survival transitions among cts sizes
          Tmat_c[3:(n+2),3:(n+2)]<-t(outer(y,y,pxy,params=params,i="crem"))*h &
          # Put it all together
          IPMmat_c<-Fmat_c+Tmat_c  & 
          ## LIOM
          # Banked seeds go in top row
          Fmat_l[1,3:(n+2)]<-fx(y,params)$f_l &
          # Graduation to 2-yo seed bank = pr(not germinating as 1-yo) &
          Tmat_l[2,1]<-1-invlogit(params[71]) &
          # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
          Tmat_l[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb) &
          # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
          Tmat_l[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)   &
          # Growth/survival transitions among cts sizes
          Tmat_l[3:(n+2),3:(n+2)]<-t(outer(y,y,pxy,params=params,i="liom"))*h &
          # Put it all together
          IPMmat_l<-Fmat_l+Tmat_l &
          ## OTHER
          # Banked seeds go in top row
          Fmat_o[1,3:(n+2)]<-fx(y,params)$f_o &
          # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
          Tmat_o[2,1]<-1-invlogit(params[71]) &
          # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
          Tmat_o[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)   & 
          # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
          Tmat_o[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)   &
          # Growth/survival transitions among cts sizes
          Tmat_o[3:(n+2),3:(n+2)]<-t(outer(y,y,pxy,params=params,i="other"))*h &
          # Put it all together
          IPMmat_o<-Fmat_o+Tmat_o &
          ## VACANT
          # Banked seeds go in top row
          Fmat_v[1,3:(n+2)]<-fx(y,params)$f_v &
          # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
          Tmat_v[2,1]<-1-invlogit(params[71]) &
          # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
          Tmat_v[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb) &  
          # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
          Tmat_v[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)  &
          # Growth/survival transitions among cts sizes
          Tmat_v[3:(n+2),3:(n+2)]<-t(outer(y,y,pxy,params=params,i="vac"))*h &
          # Put it all together
          IPMmat_v<-Fmat_v+Tmat_v,
          ## NO 1 ants
          ifelse(num_ants == 2,
                 # Banked Seeds go in top row
                 fec1_v<-fx(y,params=params)$f_vac & 
                 fec1_c<-fx(y,params=params)$f_crem & 
                 Fmat_vc[1,3:(n+2)]<-fec1_c & 
                 Fmat_vc[1,(n+3):(2*n+2)]<-fec1_v &
                 # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
                 Tmat_vc[2,1]<-1-invlogit(params[71]) & 
                 # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
                 #Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)  
                 beta2_c<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE) &
                 beta2_v<- invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE) &
                 Tmat_vc[(n+3):(2*n+2),1]<-beta2_v &
                 Tmat_vc[(n+3):(2*n+2),2]<-beta2_c &
                 # Growth/survival transitions among cts sizes
                 vac_vac<-t(outer(y,y,ptxy,i="vac",j="vac",num_ants=2,params=params))*h &
                 vac_crem<-t(outer(y,y,ptxy,i="vac",j="crem",num_ants=2,params=params))*h &
                 crem_vac<-t(outer(y,y,ptxy,i="crem",j="vac",num_ants=2,params=params))*h &
                 crem_crem<-t(outer(y,y,ptxy,i="crem",j="crem",num_ants=2,params=params))*h &
                 Tmat_vc[3:(n+2),3:(n+2)]<- vac_vac &  ## Top left 
                 Tmat_vc[3:(n+2),(n+3):(2*n+2)]<-crem_vac &  ## Top Right 
                 Tmat_vc[(n+3):(2*n+2),3:(n+2)]<- crem_crem &  ## Bottom Right 
                 Tmat_vc[(n+3):(2*n+2),(n+3):(2*n+2)]<- vac_crem &  ## Bottom Right 
                 # Put it all together
                 IPMmat_vc<-Fmat_vc+Tmat_vc & 
                 ## LIOM
                 # Banked seeds go in top row
                 fec1_v<-fx(y,params=params)$f_vac &
                 fec1_l<-fx(y,params=params)$f_liom &
                 Fmat_vl[1,3:(n+2)]<-fec1_l &
                 Fmat_vl[1,(n+3):(2*n+2)]<-fec1_v &
                 # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
                 Tmat_vl[2,1]<-1-invlogit(params[71]) &
                 # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
                 beta1_l<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE) &
                 beta1_v<- invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE) &
                 Tmat_vl[3:(n+2),1]<-beta1_v &
                 Tmat_vl[3:(n+2),2]<-beta1_l &
                 # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
                 #Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)  
                 beta2_l<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE) &
                 beta2_v<- invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE) &
                 Tmat_vl[(n+3):(2*n+2),1]<-beta2_v &
                 Tmat_vl[(n+3):(2*n+2),2]<-beta2_l &
                 # Growth/survival transitions among cts sizes
                 vac_vac<-t(outer(y,y,ptxy,i="vac",j="vac",num_ants=2,params=params))*h &
                 vac_liom<-t(outer(y,y,ptxy,i="vac",j="liom",num_ants=2,params=params))*h &
                 liom_vac<-t(outer(y,y,ptxy,i="liom",j="vac",num_ants=2,params=params))*h &
                 liom_liom<-t(outer(y,y,ptxy,i="liom",j="liom",num_ants=2,params=params))*h &
                 Tmat_vl[3:(n+2),3:(n+2)]<- vac_vac &  ## Top left 
                 Tmat_vl[3:(n+2),(n+3):(2*n+2)]<-liom_vac &  ## Top Right 
                 Tmat_vl[(n+3):(2*n+2),3:(n+2)]<- liom_liom &  ## Bottom Right 
                 Tmat_vl[(n+3):(2*n+2),(n+3):(2*n+2)]<- vac_liom &  ## Bottom Right 
                 # Put it all together
                 IPMmat_vl<-Fmat_vl+Tmat_vl &
                 ## OTHER
                 # Banked seeds go in top row
                 fec1_v<-fx(y,params=params)$f_vac &
                 fec1_o<-fx(y,params=params)$f_other &
                 Fmat_vo[1,3:(n+2)]<-fec1_o &
                 Fmat_vo[1,(n+3):(2*n+2)]<-fec1_v &
                 # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
                 Tmat_vo[2,1]<-1-invlogit(params[71]) &
                 # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
                 beta1_o<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE) &
                 beta1_v<- invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE) &
                 Tmat_vo[3:(n+2),1]<-beta1_v &
                 Tmat_vo[3:(n+2),2]<-beta1_o &
                 # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
                 #Tmat[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)  
                 beta2_o<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(TRUE) &
                 beta2_v<- invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92]*xb)*beta(FALSE) &
                 Tmat_vo[(n+3):(2*n+2),1]<-beta2_v &
                 Tmat_vo[(n+3):(2*n+2),2]<-beta2_o &
                 # Growth/survival transitions among cts sizes
                 vac_vac<-t(outer(y,y,ptxy,i="vac",j="vac",num_ants=2,params=params))*h &
                 vac_other<-t(outer(y,y,ptxy,i="vac",j="other",num_ants=2,params=params))*h &
                 other_vac<-t(outer(y,y,ptxy,i="other",j="vac",num_ants=2,params=params))*h &
                 other_other<-t(outer(y,y,ptxy,i="other",j="other",num_ants=2,params=params))*h &
                 Tmat_vo[3:(n+2),3:(n+2)]<- vac_vac & ## Top left 
                 Tmat_vo[3:(n+2),(n+3):(2*n+2)]<-other_vac & ## Top Right 
                 Tmat_vo[(n+3):(2*n+2),3:(n+2)]<- other_other & ## Bottom Right 
                 Tmat_vo[(n+3):(2*n+2),(n+3):(2*n+2)]<- vac_other & ## Bottom Right 
                 # Put it all together
                 IPMmat_vo<-Fmat_vo+Tmat_vo & 
                 return(list(IPMmat_vc=IPMmat_vc, IPMmat_vl=IPMmat_vl, IPMmat_vo=IPMmat_vo)),
                 ## NO 2 ants
                 ifelse(num_ants == 4,
                        ## YES 4 ants
                        return("Four Ants"),
                        ## NO 4 ants)
                        return("error")
                )
         )
  )
  
  
} 

(bigmatrix(4,5,cholla[,1], lower, upper, matsize, 1))

one_spec_matrix <- function(x,y,params,lower,upper,matsize){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  n<-matsize
  L<-lower; U<-upper
  h<-(U-L)/n                   #Bin size
  b<-L+c(0:n)*h;               #Lower boundaries of bins 
  y<-0.5*(b[1:n]+b[2:(n+1)]);  #Bin midpoints
  
  # Fertility matricies -- One Ant
  Fmat_c<-matrix(0,(n+2),(n+2))
  Fmat_l<-matrix(0,(n+2),(n+2))
  Fmat_o<-matrix(0,(n+2),(n+2))
  Fmat_v<-matrix(0,(n+2),(n+2))
  # Fertility matricies -- Two Ant
  Fmat_vc <- matrix(0,(2*n+2),(2*n+2))
  Fmat_vl <- matrix(0,(2*n+2),(2*n+2))
  Fmat_vo <- matrix(0,(2*n+2),(2*n+2))
  # Fertility matricies -- All Ants
  Fmat <- matrix(0,(4*n+2),(4*n+2))
  
  # Growth/survival transition matricies -- One Ant
  Tmat_c<-matrix(0,(n+2),(n+2))
  Tmat_l<-matrix(0,(n+2),(n+2))
  Tmat_o<-matrix(0,(n+2),(n+2))
  Tmat_v<-matrix(0,(n+2),(n+2))
  # Growth/survival transition matricies -- Two Ant
  Tmat_vc <- matrix(0,(2*n+2),(2*n+2))
  Tmat_vl <- matrix(0,(2*n+2),(2*n+2))
  Tmat_vo <- matrix(0,(2*n+2),(2*n+2))
  # Growth/survival transition matricies -- All Ants
  Tmat <- matrix(0,(4*n+2),(4*n+2))
  
  ## Full Matricies
  IPMmat_c <- matrix()
  IPMmat_l <- matrix()
  IPMmat_o <- matrix()
  IPMmat_vc <- matrix()
  IPMmat_vl <- matrix()
  IPMmat_vo <- matrix()
  ## CREM
  # Banked seeds go in top row
  Fmat_c[1,3:(n+2)]<-fx(y,params)$f_c 
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
  ## LIOM
  # Banked seeds go in top row
  Fmat_l[1,3:(n+2)]<-fx(y,params)$f_l 
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo) &
  Tmat_l[2,1]<-1-invlogit(params[71]) 
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  Tmat_l[3:(n+2),1]<-invlogit(params[71])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb) 
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  Tmat_l[3:(n+2),2]<-invlogit(params[81])*recruits(y,params)*h*invlogit(params[91] + params[92] * xb)   
  # Growth/survival transitions among cts sizes
  Tmat_l[3:(n+2),3:(n+2)]<-t(outer(y,y,pxy,params=params,i="liom"))*h 
  # Put it all together
  IPMmat_l<-Fmat_l+Tmat_l 
  ## OTHER
  # Banked seeds go in top row
  Fmat_o[1,3:(n+2)]<-fx(y,params)$f_o 
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
  ## VACANT
  # Banked seeds go in top row
  Fmat_v[1,3:(n+2)]<-fx(y,params)$f_v 
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
}

one_spec_matrix(x=4,y=5,params = cholla, lower, upper, matsize)

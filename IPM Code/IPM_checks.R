
## one-ant matrix (vacant-vacant)
Tmat.1 <- bigmatrix.1(params,lower=cholla_min-15,upper=cholla_max+2,matsize,"vacant")$Tmat
colSums(Tmat.1)
plot(colSums(Tmat.1),ylim=c(0,1))
## second column should sum to germination*preseed survival
colSums(Tmat.1)[2];invlogit(mean(params$germ2_beta0))*invlogit(mean(params$preseed_beta0))

## two-ant matrix (liom-vacant)
Tmat.2 <- bigmatrix.2(params,lower=cholla_min-15,upper=cholla_max+2,matsize,"vacant","liom","liomvac")$Tmat
colSums(Tmat.2)
plot(colSums(Tmat.2))

plot(transition.x(y,i = "liom",j = "liom",params,"liomvac"))
plot(transition.x(y,i = "liom",j = "vacant",params,"liomvac"))
plot(transition.x(y,i = "liom",j = "liom",params,"liomvac")+
       transition.x(y,i = "liom",j = "vacant",params,"liomvac"))

plot(transition.x(y,i = "vacant",j = "liom",params,"liomvac"))
plot(transition.x(y,i = "vacant",j = "vacant",params,"liomvac"))
plot(transition.x(y,i = "vacant",j = "liom",params,"liomvac")+
       transition.x(y,i = "vacant",j = "vacant",params,"liomvac"))

n<-matsize
L<-lower; U<-upper
h<-(U-L)/n                   #Bin size
b<-L+c(0:n)*h;               #Lower boundaries of bins 
y<-0.5*(b[1:n]+b[2:(n+1)]);  #Bin midpoints
# Growth/survival transition matricies -- Two Ant
Tmat <- matrix(0,(2*n+2),(2*n+2))
# i=liom
Tmat[3:(n+2),3:(n+2)]<-(t(outer(y,y,pxy,i = "liom",params))*h) %*% diag(rep(0.75,n)) #%*%diag(transition.x(y,i = "liom",j = "liom",params,"liomvac")) ## but will only work if transition.x is vectorized
Tmat[3:(n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,i = "liom",params))*h) %*% diag(rep(0.25,n)) #%*%diag(transition.x(y,i = "liom",j = "vacant",params,"liomvac"))
#i=vac
Tmat[(n+3):(2*n+2),3:(n+2)]<-(t(outer(y,y,pxy,i = "vacant",params))*h) %*% diag(rep(0.75,n)) #%*%diag(transition.x(y,i = "vacant",j = "liom",params,"liomvac"))
Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,i = "vacant",params))*h) %*% diag(rep(0.25,n)) #%*%diag(transition.x(y,i = "vacant",j = "vacant",params,"liomvac"))

colSums(Tmat)




matrix(1:4,2,2)*t(c(0.1,0.9))

matrix(1:4,2,2)%*%diag(c(0.1,0.9))

transition.x(x = y,i = "liom", j="liom", scenario = "liomvac", params=params)
fx(x=y, i = "liom", params=params)


t(outer(y,y,ptxy,i = "liom",j = "liom",params,"liomvac"))

t(outer(y,y,pxy,i = "liom",params))*transition.x(y,i = "vacant",j = "vacant",params,"liomvac")

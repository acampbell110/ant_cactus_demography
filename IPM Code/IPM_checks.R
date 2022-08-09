
## one-ant matrix (vacant-vacant)
Tmat.1 <- bigmatrix.1(params,lower=cholla_min-15,upper=cholla_max+2,matsize,"vacant")$Tmat
colSums(Tmat.1)
plot(colSums(Tmat.1),ylim=c(0,1))
## second column should sum to germination*preseed survival
colSums(Tmat.1)[2];invlogit(mean(params$germ2_beta0))*invlogit(mean(params$preseed_beta0))

## two-ant matrix (liom-vacant)
testmat <- bigmatrix.2(params,lower=cholla_min-15,upper=cholla_max+2,matsize,"vacant","liom","liomvac")$Tmat

matrix(1:4,2,2)*t(c(0.1,0.9))

matrix(1:4,2,2)%*%diag(c(0.1,0.9))

transition.x(x = y,i = "liom", j="liom", scenario = "liomvac", params=params)
fx(x=y, i = "liom", params=params)


t(outer(y,y,ptxy,i = "liom",j = "liom",params,"liomvac"))

t(outer(y,y,pxy,i = "liom",params))*transition.x(y,i = "vacant",j = "vacant",params,"liomvac")

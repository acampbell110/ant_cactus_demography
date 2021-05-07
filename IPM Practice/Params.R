Ndraws = 500

##----------------------Growth Parameters----------------##
cholla<-matrix(NA,nrow=100,ncol=Ndraws) 

cholla[1,]<-grow_data$beta0.1      	  ## growth intercept
cholla[2,]<-grow_data$beta1.1					## growth slope
cholla[3,]<-post.params$y.beta.2.				## growth y.beta
cholla[4,]<-post.params$g.eps					  ## SD of growth function
cholla[5,]<-post.params$sigma.yr.2.			## SD of interannual growth variance
cholla[6,]<-post.params$g.sigma.plot  	## SD of among-plot growth variance
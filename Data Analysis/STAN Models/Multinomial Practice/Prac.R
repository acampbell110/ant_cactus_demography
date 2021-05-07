setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Data Analysis/STAN Models/Multinomial Practice")
str(cactus)

#Outcome variable will be the Ant State in year t1
## Note that this needs to be releveled so that vacant is the reference variable
head(cactus)
cactus$ant_t1_relevel <- cactus$ant_t1 

cactus$ant_t1_relevel <- relevel(cactus$ant_t1_relevel, ref = "vacant")
## here is a base model, no predictor variables
ant_transition_models<-list()
## null model
ant_transition_models[[1]] <- multinom(ant_t1_relevel ~ 1, data = cactus)
## previous state dependence
ant_transition_models[[2]] <- multinom(ant_t1_relevel ~ 0+ant_t, data = cactus)
## previous state and size
ant_transition_models[[3]] <- multinom(ant_t1_relevel ~ 0+ant_t + log(volume_t), data = cactus)
ant_transition_models[[4]] <- multinom(ant_t1_relevel ~ 0+ant_t * log(volume_t), data = cactus)
summary(ant_transition_models[[1]])
exp(coef(ant_transition_models[[1]]))
summary(ant_transition_models[[2]])
summary(ant_transition_models[[3]])
summary(ant_transition_models[[4]])


table(cactus$ant_t, cactus$ant_t1)

############ Contraceptives Ex
cmcData <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/cmc/cmc.data", stringsAsFactors=FALSE, header=F)
colnames(cmcData) <- c("wife_age", "wife_edu", "hus_edu", "num_child", "wife_rel", "wife_work", "hus_occu", "sil", "media_exp", "cmc")
head(cmcData)
#Clean up the data
cmcData$wife_edu <- factor(cmcData$wife_edu, levels=sort(unique(cmcData$wife_edu)))
cmcData$hus_edu <- factor(cmcData$hus_edu, levels=sort(unique(cmcData$hus_edu)))
cmcData$wife_rel <- factor(cmcData$wife_rel, levels=sort(unique(cmcData$wife_rel)))
cmcData$wife_work <- factor(cmcData$wife_work, levels=sort(unique(cmcData$wife_work)))
cmcData$hus_occu <- factor(cmcData$hus_occu, levels=sort(unique(cmcData$hus_occu)))
cmcData$sil <- factor(cmcData$sil, levels=sort(unique(cmcData$sil)))
cmcData$media_exp <- factor(cmcData$media_exp, levels=sort(unique(cmcData$media_exp)))
cmcData$cmc <- factor(cmcData$cmc, levels=sort(unique(cmcData$cmc)))
#separate data
trainingRows <- sample(1:nrow(cmcData), 0.7*nrow(cmcData))
training <- cmcData[trainingRows, ]
test <- cmcData[-trainingRows, ]
#Build the model
#set the reference
training$cmc <- relevel(training$cmc, ref = "3")
multi0 <- multinom(cmc ~ 1, data = training)
summary(multi0)
multi1 <- multinom(cmc ~ wife_rel, data = training)
summary(multi1)


####################### Fishing EX
fish <- read.csv("multinomial_fishing.csv")
library(mlogit)
#reshape the data
fish$mode <- factor(fish$mode)
attach(fish)
table(mode)
mldata <-mlogit.data(fish, varying = 4:15, choice = "mode", shape = "wide")

############################ Whatever that crazy one is
# K = number of beta options
# J = number of alpha options
# beta = 
generate_mnl_data <- function(N=1000, K=1, J=4, beta=c(1), alpha=c(1,0,-1,2)){
  if(length(beta) != K) stop ("incorrect number of parameters")
  Y <- rep(NA, N)
  X <- list(NULL) 
  for (i in 1:N) {
    X[[i]] <- matrix(rnorm(J*K), ncol=K)
    Y[i] <- sample(x=J, size=1, prob=exp(alpha+X[[i]]%*%beta))
  }
  list(N=N, J=J, K=K, Y=Y, X=X, beta=beta, alpha=alpha)
}
d0 <- generate_mnl_data()



(exp(d0$alpha + d0$X[[1]] %*% d0$beta)/sum(exp(d0$alpha + d0$X[[1]] %*% d0$beta)))

write("data {
    int<lower=2> J; // of alternatives/outcomes
    int<lower=1> N; // of observations
    int<lower=1> K; // of covariates
    int<lower=0,upper=J> Y[N];
    matrix[J,K] X[N];
}

parameters {
    vector[K] beta;  // attribute effects 
}

model {
    for (i in 1:N)
        Y[i] ~ categorical_logit(X[i]*beta); // then our y here will be the log(prob ij/prob ij*) where j != j* and j* is the baseline/ref level
}
generated quantities{
    
}
","multi_1.stan")
stan_data_mod1 <- list(N = d0$N,
                       J = d0$J,
                       K = d0$K,
                       X = d0$X,
                       Y = d0$Y,
                       alpha = d0$alpha,
                       beta = d0$beta)
mod1 <- stan(file = "multi_1.stan",data = stan_data_mod1, warmup = 5, iter = 10, chains = 3, cores = 2, thin = 1)
mod1_outputs <- rstan::extract(mod1, pars = c("beta"))
write.csv(mod1_outputs, "mod1_outputs.csv")

mod1_outputs <- read.csv("mod1_outputs.csv")
plot(x = d0$X[[1]], y = invlogit(d0$X[[1]] %*% quantile(mod1_outputs$beta,0.5)), type = "l")
points(x = d0$X[[1]], y = invlogit(d0$X[[1]] %*% quantile(mod1_outputs$beta,0.5)))

plot(x = d0$X[[10]], y = invlogit(d0$X[[10]] %*% quantile(mod1_outputs$beta,0.5)), type = "l")
points(x = d0$X[[10]], y = invlogit(d0$X[[10]] %*% quantile(mod1_outputs$beta,0.5)))


##### Model 2 ######################################################################################
x <- matrix(NA, nrow = 1000, ncol = 1)
y <- rep(NA,1000)
for(j in 1:1){
for (i in 1:1000) {
  x[i] <- rnorm(1,mean = 20, sd = 3)
  y[i] <- sample(4, i, replace = TRUE)
}
}

stan_data_mod2 <- list(D = 1,
                       K = 4,
                       N = 1000,
                       x = x,
                       y = y)
mod2 <- stan(file = "multi_2.stan",data = stan_data_mod2, warmup = 5, iter = 10, chains = 3, cores = 2, thin = 1)
mod2_outputs <- rstan::extract(mod2, pars = c("beta"))
write.csv(mod2_outputs, "mod2_outputs.csv")
mod2_outputs <- read.csv("mod2_outputs.csv")

mu <- data.frame(beta1 = rep(NA,1000), beta2 = rep(NA,1000), beta3 = rep(NA,1000), beta4 = rep(NA,1000))
beta1 <- quantile(mod2_outputs$beta.1.1, 0.5)
beta2 <- quantile(mod2_outputs$beta.1.2, 0.5)
beta3 <- quantile(mod2_outputs$beta.1.3, 0.5)
beta4 <- quantile(mod2_outputs$beta.1.4, 0.5)
size_dummy <- seq(min(x),max(x), by = (max(x) - min(x))/999)
mu$beta1 <- beta1 * size_dummy
mu$beta2 <- beta2 * size_dummy
mu$beta3 <- beta3 * size_dummy
mu$beta4 <- beta4 * size_dummy
mu$x <- x
a <- data.frame(mu = rep(NA,4000), beta = rep(NA,4000))
for(i in 1:1000){
  a$beta[i] <- 1
}
for(i in 1001:2000){
  a$beta[i] <- 2
}
for(i in 2001:3000){
  a$beta[i] <- 3
}
for(i in 3001:4000){
  a$beta[i] <- 4
}
a$mu <- rbind(mu$beta1,mu$beta2, mu$beta3, mu$beta4)

boxplot(invlogit(a$mu) ~ a$beta)


par(mar=c(2,2,2,2))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(1,2,2), widths = c(4,4,4))
plot.new()
text(0.5,0.1,"Prac Multi with Only Size",cex=2,font=2)
plot(x = size_dummy, y = invlogit(mu$beta1), type = "l", col = "black")
plot(x = size_dummy, y = invlogit(mu$beta2), type = "l", col = "red")
plot(x = size_dummy, y = invlogit(mu$beta3), type = "l", col = "blue")
plot(x = size_dummy, y = invlogit(mu$beta4), type = "l", col = "pink")
plot(x = size_dummy, y = invlogit(mu$beta1), type = "l", col = "black", ylim = c(0,1))
lines(x = size_dummy, y = invlogit(mu$beta2), col = "red")
lines(x = size_dummy, y = invlogit(mu$beta3), col = "blue")
lines(x = size_dummy, y = invlogit(mu$beta4), col = "pink")

#OK, so I have pulled out the probabilities of being each, shows them all together in two different ways. Coooolcoolcool. 
## Now I hust have to run real data through this
multi_data <- cactus[,c("ant_t1","volume_t","Year_t","Plot")]
multi_data <- na.omit(multi_data)
multi_data$ant_stat <- as.numeric(as.factor(multi_data$ant_t1))
multi_data$Plot <- as.integer(as.factor(multi_data$Plot))
multi_data$Year_t <- as.integer(as.factor(multi_data$Year_t))
x_mat <- matrix(log(multi_data$volume_t))
stan_data_mod2 <- list(D = 1,
                       K = 4,
                       N = nrow(multi_data),
                       x = x_mat,
                       y = multi_data$ant_stat, 
                       N_Plot = max(unique(multi_data$Plot)),
                       N_Year = max(unique(multi_data$Year_t)),
                       plot = multi_data$Plot,
                       year = multi_data$Year_t)
mod2_data <- stan(file = "multi_2.stan",data = stan_data_mod2, warmup = 500, iter = 1000, chains = 3, cores = 2, thin = 1)
mod2_data_outputs <- rstan::extract(mod2_data, pars = c("beta"))
write.csv(mod2_data_outputs, "mod2_data_outputs.csv")
mod2_data_outputs <- read.csv("mod2_data_outputs.csv")

mu<- data.frame(beta1 = rep(NA,1000), beta2 = rep(NA,1000), beta3 = rep(NA,1000), beta4 = rep(NA,1000))
beta1 <- quantile(mod2_data_outputs$beta.1.1, 0.5)
beta2 <- quantile(mod2_data_outputs$beta.1.2, 0.5)
beta3 <- quantile(mod2_data_outputs$beta.1.3, 0.5)
beta4 <- quantile(mod2_data_outputs$beta.1.4, 0.5)
beta1_l <- quantile(mod2_data_outputs$beta.1.1, 0.05)
beta1_h <- quantile(mod2_data_outputs$beta.1.1, 0.95)
beta2_l <- quantile(mod2_data_outputs$beta.1.2, 0.05)
beta2_h <- quantile(mod2_data_outputs$beta.1.2, 0.95)
beta3_l <- quantile(mod2_data_outputs$beta.1.3, 0.05)
beta3_h <- quantile(mod2_data_outputs$beta.1.3, 0.95)
beta4_l <- quantile(mod2_data_outputs$beta.1.4, 0.05)
beta4_h <- quantile(mod2_data_outputs$beta.1.4, 0.95)
size_dummy <- seq(min(log(multi_data$volume_t)),max(log(multi_data$volume_t)), by = (max(log(multi_data$volume_t)) - min(log(multi_data$volume_t)))/999)
mu$beta1 <- beta1 * size_dummy
mu$beta2 <- beta2 * size_dummy
mu$beta3 <- beta3 * size_dummy
mu$beta4 <- beta4 * size_dummy
mu$beta1l <- beta1_l * size_dummy
mu$beta1h <- beta1_h * size_dummy
mu$beta2l <- beta2_l * size_dummy
mu$beta2h <- beta2_h * size_dummy
mu$beta3l <- beta3_l * size_dummy
mu$beta3h <- beta3_h * size_dummy
mu$beta4l <- beta4_l * size_dummy
mu$beta4h <- beta4_h * size_dummy
mu$x <- x
a <- data.frame(mu = rep(NA,4000), beta = rep(NA,4000))
for(i in 1:1000){
  a$beta[i] <- 1
}
for(i in 1001:2000){
  a$beta[i] <- 2
}
for(i in 2001:3000){
  a$beta[i] <- 3
}
for(i in 3001:4000){
  a$beta[i] <- 4
}
a$mu[1:1000] <- mu$beta1
a$mu[1001:2000] <- mu$beta2
a$mu[2001:3000] <- mu$beta3
a$mu[3001:4000] <- mu$beta4
#This figure
png("real_size_only_box.png")
boxplot(invlogit(a$mu) ~ a$beta, main = "Probability at any size of \n each ant state", xlab = "Ant Species", ylab = "Probability of being tended by this ant", col = c("red","blue","darkgrey","pink"))
legend("bottomright",legend = c("crem","liom","other","vacant"), col = c("red","blue","darkgrey","pink"), pch = 1)
dev.off()
png("real_size_only_lines.png")
par(mar=c(2,2,2,2))
layout(matrix(c(1,1,1,2,3,4,5,6,6),
              ncol = 3, byrow = TRUE), heights = c(1,2,2), widths = c(4,4,4))
plot.new()
text(0.5,0.3,"Prob of each ant state \n by size",cex=2,font=2)
plot(x = size_dummy, y = invlogit(mu$beta1), type = "l", col = "red") #Probability of 
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(mu$beta1h), rev(invlogit(mu$beta1l))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
plot(x = size_dummy, y = invlogit(mu$beta2), type = "l", col = "blue")
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(mu$beta2h), rev(invlogit(mu$beta2l))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
plot(x = size_dummy, y = invlogit(mu$beta3), type = "l", col = "black")
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(mu$beta3h), rev(invlogit(mu$beta3l))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
plot(x = size_dummy, y = invlogit(mu$beta4), type = "l", col = "pink")
polygon(c(size_dummy,rev(size_dummy)),c(invlogit(mu$beta4h), rev(invlogit(mu$beta4l))),
        col = rgb(red = 0.2, blue = 0.2, green = 0.2,alpha = 0.1), border = NA)
plot(x = size_dummy, y = invlogit(mu$beta1), type = "l", col = "red", ylim = c(0,1))
lines(x = size_dummy, y = invlogit(mu$beta2), col = "blue")
lines(x = size_dummy, y = invlogit(mu$beta3), col = "black")
lines(x = size_dummy, y = invlogit(mu$beta4), col = "pink")
dev.off()

#################### Include Ant State as a predictor
multi_data3 <- cactus[,c("ant_t1","volume_t","Year_t","Plot", "ant_t")]
multi_data3 <- na.omit(multi_data3)
multi_data3$ant_stat <- as.numeric(as.factor(multi_data3$ant_t1))
multi_data3$ant_last <- as.numeric(as.factor(multi_data3$ant_t))
multi_data3$Plot <- as.integer(as.factor(multi_data3$Plot))
multi_data3$Year_t <- as.integer(as.factor(multi_data3$Year_t))
x_mat <- matrix(log(multi_data3$volume_t))
stan_data_mod3 <- list(D = 1,
                       K = 4,
                       N = nrow(multi_data3),
                       x = x_mat,
                       ant = multi_data3$ant_last,
                       y = multi_data3$ant_stat, 
                       N_Plot = max(unique(multi_data3$Plot)),
                       N_Year = max(unique(multi_data3$Year_t)),
                       plot = multi_data3$Plot,
                       year = multi_data3$Year_t)
mod3_data <- stan(file = "multi_3.stan",data = stan_data_mod3, warmup = 5, iter = 10, chains = 3, cores = 2, thin = 1)
mod3_data_outputs <- rstan::extract(mod3_data, pars = c("beta"))
write.csv(mod3_data_outputs, "mod3_data_outputs.csv")
mod3_data_outputs <- read.csv("mod3_data_outputs.csv")


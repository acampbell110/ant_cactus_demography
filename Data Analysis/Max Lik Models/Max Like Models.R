setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography//Users/alicampbell/Documents/GitHub/ant_cactus_demography/Data Analysis/Max Lik Models")
library(bbmle)

cactus$logsize_t <- log(cactus$volume_t)
cactus$logsize_t1 <- log(cactus$volume_t1)

#### Growth #################################################################################### + (1|Year_t) + (1|Plot)
## What size the plant will be next year
grow_1 <- lm(logsize_t1 ~ logsize_t ,data=cactus)
grow_2 <- lm(logsize_t1 ~ logsize_t + ant_t ,data=cactus)
grow_3 <- lm(logsize_t1 ~ logsize_t * ant_t ,data=cactus) ##Selected model
bbmle::AICtab(grow_1,grow_2,grow_3)
## The selected model is grow_3
summary(grow_3)
int_grow <- coef(grow_3)[1]
vol_grow <- coef(grow_3)[2]
crem_grow <- coef(grow_3)[3]
liom_grow <- coef(grow_3)[4]
vac_grow <- coef(grow_3)[5]
crem_int_grow <- coef(grow_3)[6]
liom_int_grow <- coef(grow_3)[7]
vac_int_grow <- coef(grow_3)[8]
sd_grow <- 0.27067

#### Survival #################################################################################### + (1|Year_t) + (1|Plot)
## Prob of survival
surv_1 <- glm(Survival_t1 ~ logsize_t ,family="binomial",data=cactus)
surv_2 <- glm(Survival_t1 ~ logsize_t + ant_t , family="binomial",data=cactus)
surv_3 <- glm(Survival_t1 ~ logsize_t * ant_t ,family="binomial",data=cactus) ##Selected model
AICtab(surv_1,surv_2,surv_3)
## The selected model is surv_3
summary(surv_3)
int_surv <- coef(surv_3)[1]
vol_surv <- coef(surv_3)[2]
crem_surv <- coef(surv_3)[3]
liom_surv <- coef(surv_3)[4]
vac_surv <- coef(surv_3)[5]
crem_int_surv <- coef(surv_3)[6]
liom_int_surv <- coef(surv_3)[7]
vac_int_surv <- coef(surv_3)[8]
sd_surv <- 1.4232

#### Viability #################################################################################### + (1|Year_t) + (1|Plot)
## The proportion of flowers that will be viable
viab_1 <- glm(cbind(Goodbuds_t,ABFlowerbuds_t) ~ 1 ,family="binomial",data=cactus)
viab_2 <- glm(cbind(Goodbuds_t,ABFlowerbuds_t) ~ ant_t ,family="binomial",data=cactus)
AICtab(viab_1,viab_2)
## The selected model is viab_2
summary(viab_2)
int_viab <- coef(viab_2)[1]
crem_viab <- coef(viab_2)[2]
liom_viab <- coef(viab_2)[3]
vac_viab <- coef(viab_2)[4]
sd_viab <- 0.05737 
sd_crem_viab <- 0.08030
sd_liom_viab <- 0.06150
sd_vac_viab <- 0.07022

#### Repro #################################################################################### + (1|Year_t) + (1|Plot)
## The prob of producing flowers this year
repro_1 <- glm(as.numeric(cactus$flower1_YN) ~ log(volume_t1) , family = "binomial",data = cactus)
## The selected model is repro_1
summary(repro_1)
int_repro <- coef(repro_1)[1]
vol_repro <- coef(repro_1)[2]
sd_repro <- 0.56619
  
#### Flowering #################################################################################### + (1|Year_t) + (1|Plot)
## The number of flowers produced
flower_1 <- glm(TotFlowerbuds_t ~log(volume_t) , family = "poisson", data = cactus)
## The selected model is flower_1
summary(flower_1)
int_flower <- coef(flower_1)[1]
vol_flower <- coef(flower_1)[2]
sd_flower <- 0.070717

#### Germination yr 1 #####################################################################################################
## The prop of seeds that germinate in year 1
germ.dat<-read.csv("Germination.csv") 
germ.dat <- na.omit(germ.dat)
germ.dat$rate <- 0
for(i in 1:nrow(germ.dat)){
  if(germ.dat$Seedlings04[i] != 0){
    germ.dat$rate[i] <- (germ.dat$Seedlings04[i] - germ.dat$Seedlings05[i])/germ.dat$Seedlings04[i]
  }
}
germ.dat[-c(42,39,40),]
germ1_1 <- glm(cbind(Seedlings04,Input) ~ 1 ,family="binomial",data=germ.dat)
## The selected model is germ1_1
summary(germ1_1)
int_germ1 <- coef(germ1_1)[1]
sd_germ1 <- 0.1166

#### Germination yr 2 #####################################################################################################
## The prop of seeds that germinate in year 2
germ2_1 <- glm(cbind(Seedlings05,Input) ~ 1 ,family="binomial",data=germ.dat)
## The selected model is germ1_1
summary(germ2_1)
int_germ2 <- coef(germ2_1)[1]
sd_germ2 <- 0.1364

#### Precensus Survival ####################################################################################################
## The prob of surviving from germ to the first census
precensus.dat<-read.csv("PrecensusSurvival.csv") 
prec_1 <- glm(survive0405 ~ 1, family = "binomial", data = precensus.dat)
prec_2 <- glm(survive0405 ~ Log.size, family = "binomial", data = precensus.dat)
## The selected model is prec_1
summary(prec_1)
int_prec <- coef(prec_1)[1]
#vol_prec <- coef(prec_1)[2]
sd_prec <- 0.3154

#### Recruitment ####################################################################################################
## The size distribution of the recruits
seedling.dat <- cactus[,c("logsize_t","logsize_t1","Recruit")]
seedling.dat <- filter(seedling.dat, Recruit == 1)
rec_1 <- glm(logsize_t ~ 1, family = "gaussian", data = seedling.dat)
## The selected model is rec_1
summary(rec_1)$dispersion ## <- this is the variance so sd is sqrt of this
int_rec <- coef(rec_1)[1]
sd_rec <- 0.007995
  
#### Seeds Produced ####################################################################################################
## Number of seeds per fruit
seed_data <- seed
seed_data$occ <- "occupied"
seed_data$occ[seed_data$ant_state == "Vacant"] <- "vacant"
seed_data <- na.omit(seed_data)
seed_data$ant <- as.integer(as.factor(seed_data$occ))
seed_data$plant_fac <- as.integer(as.factor(seed_data$plant))
seed_data <- subset(seed_data, seed_count > 0)
seed_1 <- glm(seed_count ~ ant, family = "poisson", data = seed_data)
## The selected model is seed_1
summary(seed_1)
int_seed <- coef(seed_1)[1]
ant_seed <- coef(seed_1)[2]
sd_seed <- 0.02202
sd_ant_seed <- 0.01363

#### Seed Survival ####################################################################################################
## Prop of seeds which survived
fruit.surv<-read.csv("FruitSurvival.csv",header = TRUE,stringsAsFactors=T) %>% drop_na()
fruit.surv <- fruit.surv[which(fruit.surv$Fr.on.grnd.not.chewed > 0),]
fr_prop = fruit.surv$Fr.on.grnd.not.chewed/fruit.surv$Fr.on.plant
fruit_1 <- glm(cbind(Fr.on.plant,Fr.on.plant) ~ 1, family = "binomial", data = fruit.surv)
## The selected model is fruit_1
summary(fruit_1)
int_fruit <- coef(fruit_1)[1]
sd_fruit <- 0.06262

#### Transition ####################################################################################################
## What ant will be there next year
cactus$ant_t1_relevel <- relevel(cactus$ant_t1,ref = "vacant")
## here is a base model, no predictor variables
ant_transition_models<-list()
## null model
ant_transition_models[[1]] <- multinom(ant_t1_relevel ~ 1, data = cactus)
## previous state dependence
ant_transition_models[[2]] <- multinom(ant_t1_relevel ~ 0+ant_t, data = cactus)
## previous state and size
ant_transition_models[[3]] <- multinom(ant_t1_relevel ~ 0+ant_t + logsize_t, data = cactus)
ant_transition_models[[4]] <- multinom(ant_t1_relevel ~ 0+ant_t * logsize_t, data = cactus)
AICtab(ant_transition_models)
## The selected model is 4
predict(ant_transition_models[[4]],newdata=cactus[1,],type="probs")

summary(ant_transition_models[[4]])
other_other <- coef(ant_transition_models[[4]])[1]
other_crem <- coef(ant_transition_models[[4]])[2]
other_liom <- coef(ant_transition_models[[4]])[3]
other_vac 
crem_other <- coef(ant_transition_models[[4]])[4]
crem_crem <- coef(ant_transition_models[[4]])[5]
crem_liom <- coef(ant_transition_models[[4]])[6]
crem_vac 
liom_other <- coef(ant_transition_models[[4]])[7]
liom_crem <- coef(ant_transition_models[[4]])[8]
liom_liom <- coef(ant_transition_models[[4]])[9]
liom_vac 
vac_other <- coef(ant_transition_models[[4]])[10]
vac_crem <- coef(ant_transition_models[[4]])[11]
vac_liom <- coef(ant_transition_models[[4]])[12]
vac_vac
vol_other <- coef(ant_transition_models[[4]])[13]
vol_crem <- coef(ant_transition_models[[4]])[14]
vol_liom <- coef(ant_transition_models[[4]])[15]
vol_vac
other_vol_other
other_vol_crem
other_vol_liom
other_vol_vac
crem_vol_other <- coef(ant_transition_models[[4]])[16]
crem_vol_crem <- coef(ant_transition_models[[4]])[17]
crem_vol_liom <- coef(ant_transition_models[[4]])[18]
crem_vol_vac 
liom_vol_other <- coef(ant_transition_models[[4]])[19]
liom_vol_crem <- coef(ant_transition_models[[4]])[20]
liom_vol_liom <- coef(ant_transition_models[[4]])[21]
liom_vol_vac
vac_vol_other <- coef(ant_transition_models[[4]])[22]
vac_vol_crem <- coef(ant_transition_models[[4]])[23]
vac_vol_liom <- coef(ant_transition_models[[4]])[24]
vac_vol_vac 
## From Other to ...
x = 4
p_o_o <- exp(other_other + vol_other * x_vol)/(1+exp(other_other + vol_other * x_vol))
p_o_l <- exp(other_liom + vol_other * x_vol)/(1+exp(other_liom + vol_other * x_vol))
p_o_c <- exp(other_crem + vol_other * x_vol)/(1+exp(other_crem + vol_other * x_vol))
p_o_v <- vector()
for(i in 1:length(x_vol)){
  p_o_v[i] <- 1 - sum(p_o_o[i],p_o_l[i],p_o_c[i])
}
## From Liom to ...
p_l_o <- exp(liom_other + vol_liom * x_vol + liom_vol_other* x_vol)/(1+exp(liom_other + vol_liom * x_vol + liom_vol_other* x_vol))
p_l_l <- exp(liom_liom + vol_liom * x_vol + liom_vol_liom* x_vol)/(1+exp(liom_liom + vol_liom * x_vol + liom_vol_liom* x_vol))
p_l_c <- exp(liom_crem + vol_liom * x_vol + liom_vol_crem* x_vol)/(1+exp(liom_crem + vol_liom * x_vol + liom_vol_crem* x_vol))
p_l_v <- vector()
for(i in 1:length(x_vol)){
  p_l_v[i] <- 1 - sum(p_l_o[i],p_l_l[i],p_l_c[i])
}
## From Crem to ...
p_c_o <- exp(crem_other + vol_crem * x_vol + crem_vol_other* x_vol)/(1+exp(crem_other + vol_crem * x_vol + crem_vol_other* x_vol))
p_c_l <- exp(crem_liom + vol_crem * x_vol + crem_vol_liom* x_vol)/(1+exp(crem_liom + vol_crem * x_vol + crem_vol_liom* x_vol))
p_c_c <- exp(crem_crem + vol_crem * x_vol + crem_vol_crem* x_vol)/(1+exp(crem_crem + vol_crem * x_vol + crem_vol_crem* x_vol))
p_c_v <- vector()
for(i in 1:length(x_vol)){
  p_c_v[i] <- 1 - sum(p_c_o[i],p_c_l[i],p_c_c[i])
}
## From Vac to ...
p_v_o <- exp(vac_other + vac_vol_other* x_vol)/(1+exp(vac_other + vac_vol_other* x_vol))
p_v_l <- exp(vac_liom + vac_vol_liom* x_vol)/(1+exp(vac_liom + vac_vol_liom* x_vol))
p_v_c <- exp(vac_crem + vac_vol_crem* x_vol)/(1+exp(vac_crem + vac_vol_crem* x_vol))
p_v_v <- vector()
for(i in 1:length(x_vol)){
  p_v_v[i] <- 1 - sum(p_v_o[i],p_v_l[i],p_v_c[i])
}

################# Transition
ants <- cactus[,c("ant_t", "ant_t1", "logsize_t")]
ants$occ_YN_t <- as.numeric(ants$ant_t != "vacant")
ants$occ_YN_t1 <- as.numeric(ants$ant_t1 != "vacant")

ants_1 <- glm(occ_YN_t1 ~ occ_YN_t, family = "binomial", data = ants)
ants_2 <- glm(occ_YN_t1 ~ occ_YN_t + logsize_t, family = "binomial", data = ants)
ants_3 <- glm(occ_YN_t1 ~ occ_YN_t*logsize_t, family = "binomial", data = ants)
AICtab(ants_1,ants_2,ants_3)
## The selected model is ants_2
coef(ants_2)
int_ant <- coef(ants_3)[1]
int_ant_occ <- coef(ants_3)[2]
vol_ant <- coef(ants_3)[3]

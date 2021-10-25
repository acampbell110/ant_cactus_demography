########################################################################################################
##############          Load All Necessary Packages Here        ########################################
########################################################################################################
library(brms)  # for models
library(tidyverse)
library(boot)
library(rstan)
library(StanHeaders)
library(shinystan)
library(devtools)
library(tidyr)
library(lattice)
library(lme4)
library(dplyr)
library(tidyverse)
library(nnet)
library(fixest)
library(mlogit)
library(dfidx)
library(effects)
library(bbmle)
library(magrittr)
library(Gmisc)
library(RColorBrewer)
library(ggeffects)
library(heplots)
library("posterior")
library(Rlab)
library(extraDistr)
library(gridExtra)
library(grid)
library(gbm)
library(dismo)
library(popbio)
#library(sjPlot)
knitr::opts_chunk$set(echo = TRUE)
options(mc.cores = parallel::detectCores())
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Data Analysis")
#######################################################################################################
################        import the data -- Cacti (main)        #######################################################
#######################################################################################################
cactus_uncleaned <- read.csv("cholla_demography_20042019.csv", header = TRUE,stringsAsFactors=T)
str(cactus) ##<- problem: antcount is a factor
#levels(cactus$Antcount_t);levels(cactus$Antcount_t1)
## drop the offending rows -- this is a 2019 data entry problem
cactus <- cactus_uncleaned[-c(which(cactus_uncleaned$Antcount_t=="2-Jan"),which(cactus_uncleaned$Antcount_t1=="2-Jan")),]
## function for the volume of a cone
volume <- function(h, w, p){
  (1/3)*pi*h*(((w + p)/2)/2)^2
}
invlogit <- function(x){exp(x)/(1+exp(x))}
#Create volume columns
cactus$volume_t <- volume(cactus$Height_t,cactus$Width_t, cactus$Perp_t)
cactus$volume_t1 <- volume(cactus$Height_t1,cactus$Width_t1, cactus$Perp_t1)
cactus$logsize_t <- log(cactus$volume_t)
cactus$logsize_t1 <- log(cactus$volume_t1)

## assign ant counts of zero as vacant
cactus$Antsp_t[cactus$antcount_t==0] <- "vacant"
# here are the ordered levels of the current variable
Antsp_t_levels <- levels(cactus$Antsp_t)
# here is how I would like to collapse these into fewer bins -- most will be "other"
ant_t_levels <- rep("other",times=length(Antsp_t_levels))
## crem levels - elements 5,8,9
ant_t_levels[c(5,8,9)] <- "crem"
## liom levels - elements 15,18,19,20,21
ant_t_levels[c(15,18,19,20,21)] <- "liom"
## forelius levels - 12,13
#ant_t_levels[c(12,13)] <- "for"
## vacant - 31
ant_t_levels[31] <- "vacant"
## create new variable merging levels as above
cactus$ant_t <- factor(cactus$Antsp_t,levels=Antsp_t_levels,labels=ant_t_levels)
## first, there are no ant data from 2007:
cactus$Antcount_t[cactus$Year_t==2007]
#so give these observations NA for ant status:
cactus$ant_t[cactus$Year_t==2007] <- NA
# also, if a plant was new in year t+1 it's year t ant status should be NA
cactus$ant_t[cactus$Newplant==1] <- NA
# plots 7 and 8 were added in 2011, so their year_t==2010 ant status should be NA
cactus$ant_t[cactus$Year_t==2010 & cactus$Plot==7] <- NA
cactus$ant_t[cactus$Year_t==2010 & cactus$Plot==8] <- NA
## for now here is my workaround: year t ==2018, height_t ==NA, height_t1 !=NA
cactus$ant_t[cactus$Year_t==2018 & is.na(cactus$Height_t) & !is.na(cactus$Height_t1)] <- NA
## finally there are these plants with Antsp_t=="" for all kinds of reasons but bottom line is that we don't have ant status
cactus$ant_t[cactus$Antsp_t==""]<-NA
## inspect the "others" visually
cactus %>% filter(ant_t=="other") %>% select(Antsp_t,ant_t)
## I am satisfied that everything assigned other is correctly assigned
## assign ant counts of zero as vacant
cactus$Antsp_t1[cactus$antcount_t1==0] <- "vacant"
## repeat for t1 -- these indices are different
Antsp_t1_levels <- levels(cactus$Antsp_t1)
# here is how I would like to collapse these into fewer bins -- most will be "other"
ant_t1_levels <- rep("other",times=length(Antsp_t1_levels))
## crem levels 
ant_t1_levels[c(8,9,10)] <- "crem"
## liom levels 
ant_t1_levels[c(19,20,21,22)] <- "liom"
## forelius levels 
#ant_t1_levels[c(13,14,15)] <- "for"
## vacant 
ant_t1_levels[32] <- "vacant"
## create new variable merging levels as above
cactus$ant_t1 <- factor(cactus$Antsp_t1,levels=Antsp_t1_levels,labels=ant_t1_levels)
summary(cactus$ant_t1) ## there are too many "others" -- same deal as above
## first, there are no ant data from 2007 so give these observations NA for ant status:
cactus$ant_t1[cactus$Year_t1==2007] <- NA
## if we did not get size in t1 assume we did not get ant
cactus$ant_t1[is.na(cactus$Height_t1)] <- NA
# check to see how many others this leaves -- still a lot
summary(cactus$ant_t1)
## there are some Antsp_t1=="" that show up as "other"
cactus %>% filter(Antsp_t1=="" & ant_t1=="other")
## not sure why this happens but apparently there are some plants for which we have ant counts but no ant sp. Could be a mix in comments. For now make these NA.
cactus$ant_t1[cactus$Antsp_t1==""] <- NA
## inspect the "others" visually
cactus %>% filter(ant_t1=="other") %>% select(Antsp_t1,ant_t1)
## I am satisfied that everything assigned other in t1 is correctly assigned

## Relevel so that vacancy is the reference level
cactus$antcount_t <- as.numeric(as.character(cactus$Antcount_t))
cactus$antcount_t1 <- as.numeric(as.character(cactus$Antcount_t1))

## Fill in as many of the goodbuds, abortedbuds and total buds as possible
for(i in 1:8187){
  # if good buds and Aborted buds not NA set total = Sum
  if(is.na(cactus$TotFlowerbuds_t1[i]) == TRUE & is.na(cactus$Goodbuds_t1[i]) == FALSE & is.na(cactus$ABFlowerbuds_t1[i]) == FALSE){
    cactus$TotFlowerbuds_t1[i] <- cactus$Goodbuds_t1[i] + cactus$ABFlowerbuds_t1[i]
  }
  # if aborted buds and total buds is not NA, set good = total - aborted
  if(is.na(cactus$ABFlowerbuds_t1[i]) == FALSE & is.na(cactus$TotFlowerbuds_t1[i]) == FALSE & is.na(cactus$Goodbuds_t1[i]) == TRUE){
    cactus$Goodbuds_t1[i] <- cactus$TotFlowerbuds_t1[i] - cactus$ABFlowerbuds_t1[i]
  }
  # if total and good not NA, set aborted = total - good
  if(is.na(cactus$TotFlowerbuds_t1[i]) == FALSE & is.na(cactus$Goodbuds_t1[i]) == FALSE & is.na(cactus$ABFlowerbuds_t1[i]) == TRUE){
    cactus$ABFlowerbuds_t1[i] <- cactus$TotFlowerbuds_t1[i] - cactus$Goodbuds_t1[i]
  }
}
for(i in 1:8187){
  # if good buds and Aborted buds not NA set total = Sum
  if(is.na(cactus$TotFlowerbuds_t[i]) == TRUE & is.na(cactus$Goodbuds_t[i]) == FALSE & is.na(cactus$ABFlowerbuds_t[i]) == FALSE){
    cactus$TotFlowerbuds_t[i] <- cactus$Goodbuds_t[i] + cactus$ABFlowerbuds_t[i]
  }
  # if aborted buds and total buds is not NA, set good = total - aborted
  if(is.na(cactus$ABFlowerbuds_t[i]) == FALSE & is.na(cactus$TotFlowerbuds_t[i]) == FALSE & is.na(cactus$Goodbuds_t[i]) == TRUE){
    cactus$Goodbuds_t[i] <- cactus$TotFlowerbuds_t[i] - cactus$ABFlowerbuds_t[i]
  }
  # if total and good not NA, set aborted = total - good
  if(is.na(cactus$TotFlowerbuds_t[i]) == FALSE & is.na(cactus$Goodbuds_t[i]) == FALSE & is.na(cactus$ABFlowerbuds_t[i]) == TRUE){
    cactus$ABFlowerbuds_t[i] <- cactus$TotFlowerbuds_t[i] - cactus$Goodbuds_t[i]
  }
}

cactus$flower1_YN<-cactus$TotFlowerbuds_t1>0

## Export cactus to a csv
write.csv(cactus, "cholla_demography_20042019_cleaned.csv")

#######################################################################################################
################        import the data -- Seed Data        #######################################################
#######################################################################################################

seed_uncleaned <- read.csv("JO_fruit_data_final_dropplant0.csv", header = TRUE,stringsAsFactors=T)
## PEAA = Ant Access
## PAAA = Ant Access
## PEAE = Ant Exclusion
## PAAE = Ant Exclusion
seed <- subset(seed_uncleaned, treatment == "PAAA" | treatment == "PAAE")
#make the column for the ant state of the part of the plant producing seeds
for(i in 1:nrow(seed)){
  #If there is no ant access then vacant
  if(seed$ant.access[i] == "n" & is.na(seed$ant.access[i]) == FALSE){
    seed$ant_state[i] <- "Vacant"
  }
  #If there is ant access but it is still vacant then vacant
  if(seed$ant.access[i] == "y" & is.na(seed$ant.access[i]) == FALSE & seed$vacant[i] == "y"){
    seed$ant_state[i] <- "Vacant"
  }
  #if there is ant access and it is not vacant and the ant is crem then crem
  if(seed$ant.access[i] == "y" & is.na(seed$ant.access[i]) == FALSE & seed$vacant[i] == "n" & seed$species[i] == "c"){
    seed$ant_state[i] <- "Crem"
  }
  #if there is ant access and it is not vacant and the ant is liom then liom
  if(seed$ant.access[i] == "y" & is.na(seed$ant.access[i]) == FALSE & seed$vacant[i] == "n" & seed$species[i] == "l"){
    seed$ant_state[i] <- "Liom"
  }
}

#######################################################################################################
################        import the data -- Germination        #######################################################
#######################################################################################################
germ.dat<-read.csv("Germination.csv") 
germ.dat <- na.omit(germ.dat)
germ.dat$rate <- 0
for(i in 1:nrow(germ.dat)){
  if(germ.dat$Seedlings04[i] != 0){
    germ.dat$rate[i] <- (germ.dat$Seedlings04[i] - germ.dat$Seedlings05[i])/germ.dat$Seedlings04[i]
  }
}
germ.dat[-c(42,39,40),]

#######################################################################################################
################        import the data -- Fruit Survival        #######################################################
#######################################################################################################
seedling.dat <- cactus[,c("logsize_t","logsize_t1","Recruit")]
seedling.dat <- filter(seedling.dat, Recruit == 1)

#######################################################################################################
################        import the data -- Precensus Survival        #######################################################
#######################################################################################################
precensus.dat<-read.csv("PrecensusSurvival.csv") 

#######################################################################################################
################        import the data -- Cholla Dat        #######################################################
#######################################################################################################
cholla.dat$N_sdlgsize <- length(seedlings$standvol_t)
cholla.dat$y_sdlgsize <- seedlings$standvol_t

#######################################################################################################
################        Important Subsets -- Growth        #######################################################
#######################################################################################################
cactus$ant_t1_relevel <- relevel(cactus$ant_t1,ref = "vacant")
growth_data <- cactus[ ,c("Plot","Year_t","Survival_t1","ant_t","ant_t1","volume_t","volume_t1","flower1_YN")]
growth_data <- na.omit(growth_data)
growth_data$ant <- as.integer(growth_data$ant_t)
growth_data$ant1 <- as.integer(growth_data$ant_t1)
growth_data$Year_t <- as.factor(growth_data$Year_t)
growth_data$year <- as.integer(growth_data$Year_t)
growth_data$Plot <- as.factor(growth_data$Plot)
growth_data$plot <- as.integer(growth_data$Plot)


#######################################################################################################
################        Important Subsets -- Flowering Data        #######################################################
#######################################################################################################
flower_data <- cactus[ , c("TotFlowerbuds_t", "volume_t","Year_t","Plot")]
flower_data <- na.omit(flower_data)
flower_data$Year_t <- as.factor(flower_data$Year_t)
flower_data$year <- as.integer(flower_data$Year_t)
flower_data$Plot <- as.factor(flower_data$Plot)
flower_data$plot <- as.integer(flower_data$Plot)
flower_data <- subset(flower_data, TotFlowerbuds_t > 0)


#######################################################################################################
################        Important Subsets -- Survival        #######################################################
#######################################################################################################
survival_data <- cactus[ , c("Plot","Year_t","Survival_t1","ant_t","volume_t")]
survival_data <- na.omit(survival_data)
survival_data$ant <- as.integer(survival_data$ant_t)
survival_data$Year_t <- as.factor(survival_data$Year_t)
survival_data$year <- as.integer(survival_data$Year_t)
survival_data$Plot <- as.factor(survival_data$Plot)
survival_data$plot <- as.integer(survival_data$Plot)


#######################################################################################################
################        Important Subsets -- Repro        #######################################################
#######################################################################################################
## Repro Data Set
reproductive_data <- cactus[ , c("flower1_YN","volume_t","Year_t","Plot", "volume_t1")]
reproductive_data <- na.omit(reproductive_data)
reproductive_data$Year_t <- as.factor(reproductive_data$Year_t)
reproductive_data$year <- as.integer(reproductive_data$Year_t)
reproductive_data$Plot <- as.factor(reproductive_data$Plot)
reproductive_data$plot <- as.integer(reproductive_data$Plot)


#######################################################################################################
################        Important Subsets -- Viability        #######################################################
#######################################################################################################
viability_data <- cactus[ , c("TotFlowerbuds_t1","Goodbuds_t1","ABFlowerbuds_t1","ant_t", "volume_t","Year_t","Plot")]
viability_data <- na.omit(viability_data)
viability_data <- subset(viability_data, TotFlowerbuds_t1 > 0)
viability_data$ant <- as.integer(viability_data$ant_t)
viability_data$Year_t <- as.factor(viability_data$Year_t)
viability_data$year <- as.integer(viability_data$Year_t)
viability_data$Plot <- as.factor(viability_data$Plot)
viability_data$plot <- as.integer(viability_data$Plot)


#######################################################################################################
################        Important Subsets -- Seeds        #######################################################
#######################################################################################################
seed_data <- seed
seed_data <- na.omit(seed_data)
seed_data$ant <- as.integer(as.factor(seed_data$ant_state))
seed_data$plant_fac <- as.integer(as.factor(seed_data$plant))
seed_data <- subset(seed_data, seed_count > 0)



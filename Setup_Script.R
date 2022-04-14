########################################################################################################
########################################################################################################
##                                                                                                    ##
##              A script that creates subsets for each vital rate STAN model & loads                  ##
##              necessary packages                                                                    ##
########################################################################################################
########################################################################################################

## call the data cleaned in the Create_Clean_Data_Script
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Data Analysis")
source("Create_Clean_Data_Script.R")

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

## Import the data
cactus <- read.csv("cholla_demography_20042019_cleaned.csv", header = TRUE,stringsAsFactors=T)

#######################################################################################################
################        import the data -- Germination  (No ant state)      ###########################
#######################################################################################################
germ.dat_orig<-read.csv("Germination.csv") 
germ.dat_orig$rate <- 0
for(i in 1:nrow(germ.dat_orig)){
  if(germ.dat_orig$Seedlings04[i] != 0){
    germ.dat_orig$rate[i] <- (germ.dat_orig$Seedlings04[i] - germ.dat_orig$Seedlings05[i])/germ.dat_orig$Seedlings04[i]
  }
}
germ.dat <- na.omit(germ.dat_orig)
germ.dat <- germ.dat[-c(42,39,40),]
#Check that you are happy with the subsetting
plot(germ.dat$rate)
points(germ.dat_orig$rate, col = "red")
## Most of the rows are included. We only lose 3, but it is still a small dataset. 
nrow(germ.dat)
nrow(germ.dat_orig)
#######################################################################################################
################        import the data -- Fruit Survival  (No ant state)      ########333#############
#######################################################################################################
seedling.dat_orig <- subset(cactus, cactus$Recruit == 1)
seedling.dat_orig <- seedling.dat_orig[,c("logsize_t1","Recruit")]
nrow(seedling.dat_orig)
seedling.dat <- na.omit(seedling.dat_orig)
nrow(seedling.dat)
## losing 19 rows
# check that you are happy with the subsetting
plot(seedling.dat$logsize_t1, seedling.dat$Recruit, xlim = c(-5,15), ylim = c(0,1))
points(cactus$logsize_t1, cactus$Recruit, col = "red")
#######################################################################################################
################        import the data -- Precensus Survival  (No ant state)      ####################
#######################################################################################################
precensus.dat.orig<-read.csv("PrecensusSurvival.csv") 
precensus.dat <- precensus.dat.orig[ , c("Transect","Seed","Log.size","survive0405")]
precensus.dat <- na.omit(precensus.dat)
## You lose no data here, but it is still a small dataset. 
nrow(precensus.dat)
nrow(precensus.dat.orig)
# check that you're happy with the subsetting
plot(precensus.dat$Log.size, jitter(precensus.dat$survive0405))
points(precensus.dat.orig$Log.size, jitter(precensus.dat.orig$survive0405), col = "red")
#######################################################################################################
################        Important Subsets -- Growth  (Includes Ant)      ##############################
#######################################################################################################
## All Ant States
growth_data_orig <- cactus[ ,c("Plot","Year_t","Survival_t1","logsize_t","logsize_t1","ant_t")]
growth_data <- na.omit(growth_data_orig)
## Lose 2032 rows (due to plant death & recruit status)
nrow(growth_data_orig)
nrow(growth_data)
summary(growth_data$ant_t)
# check that you are happy with the subsetting
plot(growth_data$logsize_t, growth_data$logsize_t1)
points((cactus$logsize_t), (cactus$logsize_t1), col = "red")
#######################################################################################################
################        Important Subsets -- Flowering Data        #######################################################
#######################################################################################################
flower_data_orig <- cactus[ , c("TotFlowerbuds_t", "logsize_t","Year_t","Plot")]
flower_data <- na.omit(flower_data_orig)
flower_data <- subset(flower_data, TotFlowerbuds_t > 0)
## Lose 6605 rows of data due to no flower data
nrow(flower_data_orig)
nrow(flower_data)
# check that you're happy with the subsetting
plot(flower_data$logsize_t, flower_data$TotFlowerbuds_t)
points(cactus$logsize_t, cactus$TotFlowerbuds_t, col = "red")
#######################################################################################################
################        Important Subsets -- Survival  (Includes Ants)      #######################################################
#######################################################################################################
survival_data_orig <- cactus[ , c("Plot","Year_t","Survival_t1","ant_t","logsize_t")]
survival_data <- na.omit(survival_data_orig)
## Lose 1619 rows due to recruit status 
nrow(survival_data_orig)
nrow(survival_data)
# check that you're happy with the subsetting
plot(survival_data$logsize_t, (survival_data$Survival_t1))
points(cactus$logsize_t, cactus$Survival_t1, col = "red")
#######################################################################################################
################        Important Subsets -- Repro        #######################################################
#######################################################################################################
## Repro Data Set
reproductive_data_orig <- cactus[ , c("flower1_YN","logsize_t","Year_t","Plot", "logsize_t1")]
reproductive_data <- na.omit(reproductive_data_orig)
## Lose 3332 rows of data because only including 
nrow(reproductive_data_orig)
nrow(reproductive_data)
# check that you're happy with the subsetting
plot(reproductive_data$logsize_t, reproductive_data$flower1_YN)
points(cactus$logsize_t, cactus$flower1_YN, col = "red")
#######################################################################################################
################        Important Subsets -- Viability  (Includes Ants)      #######################################################
#######################################################################################################
viability_data_orig <- cactus[ , c("TotFlowerbuds_t1","Goodbuds_t1","ABFlowerbuds_t1","ant_t", "logsize_t","Year_t","Plot")]
viability_data <- na.omit(viability_data_orig)
## Lose 3619 rows of data because of missing flower data
nrow(viability_data_orig)
viability_data <- subset(viability_data, TotFlowerbuds_t1 > 0)
nrow(viability_data)
# check if you're happy with the subsetting
plot(viability_data$logsize_t, viability_data$ABFlowerbuds_t1, xlim = c(-5,15), ylim = c(0,60))
plot(cactus$logsize_t, cactus$ABFlowerbuds_t1, col = "red") ## This one is not ideal because there is missing ant data
#######################################################################################################
################        Import Seed Data and create Subsets -- Seeds        ###########################
#######################################################################################################
seed_uncleaned <- read.csv("JO_fruit_data_final_dropplant0.csv", header = TRUE,stringsAsFactors=T)
## PEAA = Ant Access
## PAAA = Ant Access
## PEAE = Ant Exclusion
## PAAE = Ant Exclusion
seed <- subset(seed_uncleaned, treatment == "PAAA" | treatment == "PAAE")
nrow(seed)
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
seed_data <- seed
seed_data <- na.omit(seed_data)
seed_data$ant <- as.integer(as.factor(seed_data$ant_state))
seed_data$plant_fac <- as.integer(as.factor(seed_data$plant))
seed_data <- subset(seed_data, seed_count > 0)
nrow(seed_uncleaned)
nrow(seed_data)
# check if you're happy with the subsetting
plot(seed$fruit_number)
points(seed_data$fruit_number, col = "red")


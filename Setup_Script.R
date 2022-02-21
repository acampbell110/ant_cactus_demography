########################################################################################################
########################################################################################################
##                                                                                                    ##
##              A script that creates subsets for each vital rate STAN model & loads                  ##
##              necessary packages                                                                    ##
########################################################################################################
########################################################################################################


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

######################################################################################################
################        Subset cactus data to different combinations of ants      ####################
######################################################################################################
## One ant only
cactus_crem <- subset(cactus, ant_t == "crem" & ant_t1 == "crem") ## 163 rows
cactus_liom <- subset(cactus, ant_t == "liom" & ant_t1 == "liom") ## 1753 rows
cactus_other <- subset(cactus, ant_t == "other" & ant_t1 == "other") ## 29 rows
cactus_vac <- subset(cactus, ant_t == "vac" & ant_t1 == "vac") ## 0

## Two ants only
cactus_crem_liom <- subset(cactus, (ant_t == "crem" | ant_t == "liom") & (ant_t1 == "crem" | ant_t1 == "liom")) ## 2106 rows
cactus_crem_other <- subset(cactus, (ant_t == "crem" | ant_t == "other") & (ant_t1 == "crem" | ant_t1 == "other")) ## 263 rows
cactus_crem_vac <- subset(cactus, (ant_t == "crem" | ant_t == "vac") & (ant_t1 == "crem" | ant_t1 == "vac")) ## 163 rows
cactus_liom_other <- subset(cactus, (ant_t == "other" | ant_t == "liom") & (ant_t1 == "other" | ant_t1 == "liom")) ## 1935 rows
cactus_liom_vac <- subset(cactus, (ant_t == "vac" | ant_t == "liom") & (ant_t1 == "vac" | ant_t1 == "liom")) ## 1753 rows
cactus_other_vac <- subset(cactus, (ant_t == "other" | ant_t == "vac") & (ant_t1 == "other" | ant_t1 == "vac")) ## 29 rows

## Three ants 
cactus_crem_liom_other <- subset(cactus, (ant_t == "crem" | ant_t == "liom" | ant_t == "other") & (ant_t1 == "crem" | ant_t1 == "liom" | ant_t1 == "other")) ## 2359 rows
cactus_crem_liom_vac <- subset(cactus, (ant_t == "crem" | ant_t == "liom" | ant_t == "vac") & (ant_t1 == "crem" | ant_t1 == "liom" | ant_t1 == "vac")) ## 2106 rows
cactus_crem_other_vac <- subset(cactus, (ant_t == "crem" | ant_t == "other" | ant_t == "vac") & (ant_t1 == "crem" | ant_t1 == "other" | ant_t1 == "vac")) ## 263 rows
cactus_liom_vac_other <- subset(cactus, (ant_t == "liom" | ant_t == "other" | ant_t == "vac") & (ant_t1 == "liom" | ant_t1 == "other" | ant_t1 == "vac")) ## 1935 rows


#######################################################################################################
################        import the data -- Germination  (No ant state)      ###########################
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
################        import the data -- Fruit Survival  (No ant state)      ########333#############
#######################################################################################################
seedling.dat <- cactus[,c("logsize_t","logsize_t1","Recruit")]
seedling.dat <- filter(seedling.dat, Recruit == 1)

#######################################################################################################
################        import the data -- Precensus Survival  (No ant state)      ####################
#######################################################################################################
precensus.dat<-read.csv("PrecensusSurvival.csv") 

#######################################################################################################
################        import the data -- Cholla Data        ###########################3#############
#######################################################################################################
cholla.dat$N_sdlgsize <- length(seedlings$standvol_t)
cholla.dat$y_sdlgsize <- seedlings$standvol_t

#######################################################################################################
################        Important Subsets -- Growth  (Includes Ant)      ##############################
#######################################################################################################
cactus$ant_t1_relevel <- relevel(cactus$ant_t1,ref = "vacant")

## All Ant States
growth_data <- cactus[ ,c("Plot","Year_t","Survival_t1","ant_t","logsize_t","logsize_t1","flower1_YN")]
growth_data <- na.omit(growth_data)
## One Ant State
growth_data_crem <- subset(growth_data, ant_t == "crem" & ant_t1 == "crem") ## 141 rows
growth_data_liom <- subset(growth_data, ant_t == "liom" & ant_t1 == "liom") ## 1399 rows
growth_data_other <- subset(growth_data, ant_t == "other" & ant_t1 == "other") ## 28 rows
growth_data_vac <- subset(growth_data, ant_t == "vac" & ant_t1 == "vac") ## 0

## Two Ant States
growth_data_crem_liom <- subset(growth_data, (ant_t == "crem" | ant_t == "liom") & (ant_t1 == "crem" | ant_t1 == "liom")) ## 1700 rows
growth_data_crem_other <- subset(growth_data, (ant_t == "crem" | ant_t == "other") & (ant_t1 == "crem" | ant_t1 == "other")) ## 226 rows
growth_data_crem_vac <- subset(growth_data, (ant_t == "crem" | ant_t == "vac") & (ant_t1 == "crem" | ant_t1 == "vac")) ## 141 rows
growth_data_liom_other <- subset(growth_data, (ant_t == "other" | ant_t == "liom") & (ant_t1 == "other" | ant_t1 == "liom")) ## 1560 rows
growth_data_liom_vac <- subset(growth_data, (ant_t == "vac" | ant_t == "liom") & (ant_t1 == "vac" | ant_t1 == "liom")) ## 1399 rows
growth_data_other_vac <- subset(growth_data, (ant_t == "other" | ant_t == "vac") & (ant_t1 == "other" | ant_t1 == "vac")) ## 28 rows

## Three Ant States
growth_data_crem_liom_other <- subset(growth_data, (ant_t == "crem" | ant_t == "liom" | ant_t == "other") & (ant_t1 == "crem" | ant_t1 == "liom" | ant_t1 == "other")) ## 1918 rows
growth_data_crem_liom_vac <- subset(growth_data, (ant_t == "crem" | ant_t == "liom" | ant_t == "vac") & (ant_t1 == "crem" | ant_t1 == "liom" | ant_t1 == "vac")) ## 1700 rows
growth_data_crem_other_vac <- subset(growth_data, (ant_t == "crem" | ant_t == "vac" | ant_t == "other") & (ant_t1 == "crem" | ant_t1 == "vac" | ant_t1 == "other")) ## 226 rows
growth_data_liom_other_vac <- subset(growth_data, (ant_t == "vac" | ant_t == "liom" | ant_t == "other") & (ant_t1 == "vac" | ant_t1 == "liom" | ant_t1 == "other")) ## 1560 rows


#######################################################################################################
################        Important Subsets -- Flowering Data        #######################################################
#######################################################################################################
flower_data <- cactus[ , c("TotFlowerbuds_t", "logsize_t","Year_t","Plot")]
flower_data <- na.omit(flower_data)
flower_data <- subset(flower_data, TotFlowerbuds_t > 0)


#######################################################################################################
################        Important Subsets -- Survival  (Includes Ants)      #######################################################
#######################################################################################################
survival_data <- cactus[ , c("Plot","Year_t","Survival_t1","ant_t","logsize_t")]
survival_data <- na.omit(survival_data)

## One Ant State
survival_data_crem <- subset(survival_data, ant_t == "crem") ## 587 rows
survival_data_liom <- subset(survival_data, ant_t == "liom") ## 2708 rows
survival_data_other <- subset(survival_data, ant_t == "other") ## 268 rows
survival_data_vac <- subset(survival_data, ant_t == "vac") ## 0

## Two Ant States
survival_data_crem_liom <- subset(survival_data, (ant_t == "crem" | ant_t == "liom")) ## 3286 rows
survival_data_crem_other <- subset(survival_data, (ant_t == "crem" | ant_t == "other")) ## 846 rows
survival_data_crem_vac <- subset(survival_data, (ant_t == "crem" | ant_t == "vac")) ## 578 rows
survival_data_liom_other <- subset(survival_data, (ant_t == "other" | ant_t == "liom")) ## 2976 rows
survival_data_liom_vac <- subset(survival_data, (ant_t == "vac" | ant_t == "liom")) ## 2708 rows
survival_data_other_vac <- subset(survival_data, (ant_t == "other" | ant_t == "vac")) ## 268 rows

## Three Ant States
survival_data_crem_liom_other <- subset(survival_data, (ant_t == "crem" | ant_t == "liom" | ant_t == "other")) ## 3554 rows
survival_data_crem_liom_vac <- subset(survival_data, (ant_t == "crem" | ant_t == "liom" | ant_t == "vac")) ## 3286 rows
survival_data_crem_other_vac <- subset(survival_data, (ant_t == "crem" | ant_t == "vac" | ant_t == "other")) ## 846 rows
survival_data_liom_other_vac <- subset(survival_data, (ant_t == "vac" | ant_t == "liom" | ant_t == "other")) ## 2976 rows


#######################################################################################################
################        Important Subsets -- Repro        #######################################################
#######################################################################################################
## Repro Data Set
reproductive_data <- cactus[ , c("flower1_YN","logsize_t","Year_t","Plot", "logsize_t1")]
reproductive_data <- na.omit(reproductive_data)

#######################################################################################################
################        Important Subsets -- Viability  (Includes Ants)      #######################################################
#######################################################################################################
viability_data <- cactus[ , c("TotFlowerbuds_t1","Goodbuds_t1","ABFlowerbuds_t1","ant_t", "logsize_t","Year_t","Plot")]
viability_data <- na.omit(viability_data)
viability_data <- subset(viability_data, TotFlowerbuds_t1 > 0)

## One Ant State
viability_data_crem <- subset(viability_data, ant_t == "crem") ## 159 rows
viability_data_liom <- subset(viability_data, ant_t == "liom") ## 1008 rows
viability_data_other <- subset(viability_data, ant_t == "other") ## 93 rows
viability_data_vac <- subset(viability_data, ant_t == "vac") ## 0

## Two Ant States
viability_data_crem_liom <- subset(viability_data, (ant_t == "crem" | ant_t == "liom")) ## 1167 rows
viability_data_crem_other <- subset(viability_data, (ant_t == "crem" | ant_t == "other")) ## 252 rows
viability_data_crem_vac <- subset(viability_data, (ant_t == "crem" | ant_t == "vac")) ## 159 rows
viability_data_liom_other <- subset(viability_data, (ant_t == "other" | ant_t == "liom")) ## 1101 rows
viability_data_liom_vac <- subset(viability_data, (ant_t == "vac" | ant_t == "liom")) ## 1008 rows
viability_data_other_vac <- subset(viability_data, (ant_t == "other" | ant_t == "vac")) ## 93 rows

## Three Ant States
viability_data_crem_liom_other <- subset(viability_data, (ant_t == "crem" | ant_t == "liom" | ant_t == "other")) ## 1260 rows
viability_data_crem_liom_vac <- subset(viability_data, (ant_t == "crem" | ant_t == "liom" | ant_t == "vac")) ## 1167 rows
viability_data_crem_other_vac <- subset(viability_data, (ant_t == "crem" | ant_t == "vac" | ant_t == "other")) ## 252 rows
viability_data_liom_other_vac <- subset(viability_data, (ant_t == "vac" | ant_t == "liom" | ant_t == "other")) ## 1101 rows


#######################################################################################################
################        Import Seed Data and create Subsets -- Seeds        ###########################
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
seed_data <- seed
seed_data <- na.omit(seed_data)
seed_data$ant <- as.integer(as.factor(seed_data$ant_state))
seed_data$plant_fac <- as.integer(as.factor(seed_data$plant))
seed_data <- subset(seed_data, seed_count > 0)

## One Ant State
seed_data_crem <- subset(seed_data, ant_state == "Crem") ## 28 rows
seed_data_liom <- subset(seed_data, ant_state == "Liom") ## 41 rows
seed_data_vac <- subset(seed_data, ant_state == "Vacant") ## 73 rows

## Two Ant States
seed_data_crem_liom <- subset(seed_data, ant_state == "Crem"| ant_state == "Liom") ## 69 rows
seed_data_crem_vacant <- subset(seed_data, ant_state == "Crem" | ant_state == "Vacant") ## 101 rows
seed_data_liom_vacant <- subset(seed_data, ant_state == "Liom" | ant_state == "Vacant") ## 114 rows


#############################################################################################################
################        Multinomial Subsets                ###############################################
########################################################################################################
## 2 ant species
binom_ant <- cactus[,c("occ_t", "occ_t1", "logsize_t")]
binom_ant <- na.omit(binom_ant)
plot((as.numeric(as.factor(binom_ant$occ_t1)))-1, ylim = c(0,1))
points((as.numeric(as.factor(cactus$occ_t1)))-1, col = "red")
## I am missing out on a lot of vacant plants in my subset for year t1.

## 3 ant species

## all ant species




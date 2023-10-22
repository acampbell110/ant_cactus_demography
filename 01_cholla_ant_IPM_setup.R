################################################################################
################################################################################
## The purpose of this script is to load all required packages, load the data 
## and fix any errors (such as excel taking numbers as dates or mistyping ant 
## species names etc.), write a "clean" data file which has all errors 
## fixed and can be called directly and run through all other r scripts, and 
## finally load in any other data files.
################################################################################
################################################################################
## Load All Necessary Packages Here 
# stan models
library(rstan)
library(sn)
library(sgt)
# ipm analysis
library(popbio)
# max likelihood models
library(lme4)
library(nnet)
library(extraDistr)
# visual model checks 
library(moments)
library("posterior")
library(gridExtra)
library(grid)
library(MASS)
library(patchwork)
library(tidyverse)
# data management
library(tidyverse)
library(dplyr)
library(magrittr)
# graphics
library(RColorBrewer)
library(corrplot)
library(ggpubr)
library(reshape)
library(reshape2)
knitr::opts_chunk$set(echo = TRUE)
options(mc.cores = parallel::detectCores())
# check the workign directory and make sure it is set to the right location
getwd()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")

## Create the necessary functions 
# function for the volume of a cone
volume <- function(h, w, p){
  (1/3)*pi*h*(((w + p)/2)/2)^2
}
# inverse logit function
invlogit <- function(x){exp(x)/(1+exp(x))}
# quantile-based moments for analyzing growth function
Q.mean<-function(q.25,q.50,q.75){(q.25+q.50+q.75)/3}
Q.sd<-function(q.25,q.75){(q.75-q.25)/1.35}
Q.skewness<-function(q.10,q.50,q.90){(q.10 + q.90 - 2*q.50)/(q.90 - q.10)}
Q.kurtosis<-function(q.05,q.25,q.75,q.95){
  qN = qnorm(c(0.05,0.25,0.75,0.95))
  KG = (qN[4]-qN[1])/(qN[3]-qN[2])
  return(((q.95-q.05)/(q.75-q.25))/KG - 1)
}


## import the data -- Cacti (main) 
cactus <- read.csv("Data Analysis/cholla_demography_20042021.csv", header = TRUE,stringsAsFactors=T)
str(cactus) ##<- problem: antcount is a factor

## re-assign the seedling plots ("HT1B1" etc) to transects 1-3
levels(cactus$Plot)<-c(levels(cactus$Plot),"T4")
cactus$Plot[cactus$Plot=="HT1B1"]<-"T1"
cactus$Plot[cactus$Plot=="HT2B3"]<-"T2"
cactus$Plot[cactus$Plot=="HT3B1" | cactus$Plot=="HT3B2" | cactus$Plot=="HT3B3"]<-"T3"
cactus$Plot[cactus$Plot=="HT4B1" | cactus$Plot=="HT4B2"]<-"T4"

## Format data for easier use
# Create volume columns that are log scaled
cactus$logsize_t <- log(volume(cactus$Height_t,cactus$Width_t, cactus$Perp_t))
cactus$logsize_t1 <- log(volume(cactus$Height_t1,cactus$Width_t1, cactus$Perp_t1))

## Cactus 2023 data cleaning ---- Ant Species
# Change ant counts to numeric (some random entries are different types of strings)
cactus$Antcount_t <- as.numeric(as.character(cactus$Antcount_t))
cactus$Antcount_t1 <- as.numeric(as.character(cactus$Antcount_t1))
#summary(as.factor(cactus$Antcount_t))
#summary(as.factor(cactus$Antcount_t1))
# assign ant counts of zero as vacant
cactus$Antsp_t[cactus$Antcount_t==0] <- "vacant"
summary(cactus$Antsp_t)
# here are the ordered levels of the current variable
Antsp_t_levels <- levels(cactus$Antsp_t)
# here is how I would like to collapse these into fewer bins -- most will be "other"
ant_t_levels <- rep("other",times=length(Antsp_t_levels))
# crem levels - elements
ant_t_levels[c(5,8,9,10,38)] <- "crem"
# liom levels - elements
ant_t_levels[c(17,21,22,23,24,25)] <- "liom"
# vacant - elements
ant_t_levels[c(1,36,37,38)] <- "vacant"
# create new variable merging levels as above
cactus$ant_t <- factor(cactus$Antsp_t,levels=Antsp_t_levels,labels=ant_t_levels)
# there are still some other problems with this data: first, there are no ant data from 2007:
#cactus$Antcount_t[cactus$Year_t==2007]
#so give these observations NA for ant status: should be 118 values of NA now
cactus$ant_t[cactus$Year_t==2007] <- NA
# also, if a plant was new in year t+1 it's year t ant status should be NA now up to 650 NA values
cactus$ant_t[cactus$Newplant==1] <- NA
# plots 7 and 8 were added in 2011, so their year_t==2010 ant status should be NA up to NAs
cactus$ant_t[cactus$Year_t==2010 & cactus$Plot==7] <- NA
cactus$ant_t[cactus$Year_t==2010 & cactus$Plot==8] <- NA
# for now here is my workaround: year t ==2018, height_t ==NA, height_t1 !=NA
cactus$ant_t[cactus$Year_t==2018 & is.na(cactus$Height_t) & !is.na(cactus$Height_t1)] <- NA
# finally there are these plants with Antsp_t=="" for all kinds of reasons but bottom line is that we don't have ant status
cactus$ant_t[cactus$Antsp_t==""]<-NA
# Check the distribution of the ant species
#summary(cactus$ant_t)
# 4440 vac, 614 other, 659 813, 3422 liom, 1316 NA
# repeat for t1 -- these indices are different
Antsp_t1_levels <- levels(cactus$Antsp_t1)
# here is how I would like to collapse these into fewer bins -- most will be "other"
ant_t1_levels <- rep("other",times=length(Antsp_t1_levels))
# crem levels
ant_t1_levels[c(9,10,11,12,42)] <- "crem"
# liom levels
ant_t1_levels[c(23,24,25,26,27)] <- "liom"
# vacant
ant_t1_levels[c(1,40,41,39)] <- "vacant"
# create new variable merging levels as above
cactus$ant_t1 <- factor(cactus$Antsp_t1,levels=Antsp_t1_levels,labels=ant_t1_levels)
# check the spread of these ants
#summary(cactus$ant_t1) ## there are too many "vacants" -- same deal as above
# vac 6097, other 373, crem 777, liom 3358, NA 0
# first, there are no ant data from 2007 so give these observations NA for ant status:
cactus$ant_t1[cactus$Year_t1==2007] <- NA
# if we did not get size in t1 assume we did not get ant
cactus$ant_t1[is.na(cactus$Height_t1)] <- NA
# check to see how many others this leaves -- still a lot
#summary(cactus$ant_t1)
# vac 4445, other 373, crem 777, liom 3357, NA 1653
# there are some Antsp_t1=="" that show up as "other"
#cactus %>% filter(Antsp_t1=="" & ant_t1=="other")
# finally there are these plants with Antsp_t=="" for all kinds of reasons but bottom line is that we don't have ant status
cactus$ant_t[cactus$Antsp_t==""]<-NA
# Relevel so that vacancy is the reference level
cactus$ant_t1 <- relevel(cactus$ant_t1,ref = "vacant")
cactus$ant_t <- relevel(cactus$ant_t, ref = "vacant")

## Cactus 2023 data cleaning ---- Flower Count Data
# some of the data is not showing up as integers
#str(cactus)
cactus$Goodbuds_t1 <- as.integer(as.character(cactus$Goodbuds_t1))
cactus$ABFlowerbuds_t1 <- as.integer(as.character(cactus$ABFlowerbuds_t1))
cactus$TotFlowerbuds_t1 <- as.integer(as.character(cactus$TotFlowerbuds_t1))
#str(cactus) # now all the data are of the right format
# If Goodbuds is NA but Abortbuds and Totalbuds is not, set goodbuds = Tot - Ab (0 rows for t and 0 rows for t1)
for(i in 1:nrow(cactus)){
  if(is.na(cactus$Goodbuds_t[i]) == TRUE & is.na(cactus$ABFlowerbuds_t[i]) == FALSE & is.na(cactus$TotFlowerbuds_t[i]) == FALSE){
    cactus$Goodbuds_t[i] <- cactus$TotFlowerbuds_t[i] - cactus$ABFlowerbuds_t[i]
  }
  if(is.na(cactus$Goodbuds_t1[i]) == TRUE & is.na(cactus$ABFlowerbuds_t1[i]) == FALSE & is.na(cactus$TotFlowerbuds_t1[i]) == FALSE){
    cactus$Goodbuds_t1[i] <- cactus$TotFlowerbuds_t1[i] - cactus$ABFlowerbuds_t1[i]
  }
}
# If Abortbuds is NA but Goodbuds and Totalbuds is not, set AB = Tot - Good (0 rows for t and 287 rows for t1)
for(i in 1:nrow(cactus)){
  if(is.na(cactus$ABFlowerbuds_t[i]) == TRUE & is.na(cactus$Goodbuds_t[i]) == FALSE & is.na(cactus$TotFlowerbuds_t[i]) == FALSE){
    cactus$ABFlowerbuds_t[i] <- cactus$TotFlowerbuds_t[i] - cactus$Goodbuds_t[i]
  }
  if(is.na(cactus$Goodbuds_t1[i]) == TRUE & is.na(cactus$ABFlowerbuds_t1[i]) == FALSE & is.na(cactus$TotFlowerbuds_t1[i]) == FALSE){
    cactus$ABFlowerbuds_t1[i] <- cactus$TotFlowerbuds_t1[i] - cactus$Goodbuds_t1[i]
  }
}
# If Totalbuds is NA but Goodbuds and Abortbuds is not, set Tot = AB + Good
for(i in 1:nrow(cactus)){
  if(is.na(cactus$TotFlowerbuds_t[i]) == TRUE & is.na(cactus$Goodbuds_t[i]) == FALSE & is.na(cactus$ABFlowerbuds_t[i]) == FALSE){
    cactus$TotFlowerbuds_t[i] <- cactus$ABFlowerbuds_t[i] + cactus$Goodbuds_t[i]
  }
  if(is.na(cactus$TotFlowerbuds_t1[i]) == TRUE & is.na(cactus$Goodbuds_t1[i]) == FALSE & is.na(cactus$ABFlowerbuds_t1[i]) == FALSE){
    cactus$TotFlowerbuds_t1[i] <- cactus$ABFlowerbuds_t1[i] + cactus$Goodbuds_t1[i]
  }
}
cactus$flower1_YN<-cactus$TotFlowerbuds_t1>0
#summary(cactus$flower1_YN)
cactus$ant_t1 <- relevel(cactus$ant_t1,ref = "vacant")

## Cactus 2023 data cleaning ---- Recruitment
# If the plant is a new plant or a recruit, make the survival status NA
cactus$Survival_t1[cactus$Recruit == 1] <- NA
cactus$Survival_t1[cactus$Newplant == 1] <- NA

## Export the data ---- Write a data file which has "cleaned" data with proper column headings
head(cactus) # some of the columns are not names properly
# rename all columns
colnames(cactus) <- c("Plot" ,            "TagID"     ,       "Transplant"   ,    "Year_t" ,         
                      "Height_t"     ,    "Width_t"     ,     "Perp_t"      ,     "NS_t"   ,         
                      "Goodbuds_t"    ,   "TotFlowerbuds_t" , "ABFlowerbuds_t" ,  "Antcount_t" ,     
                      "Antsp_t"     ,     "Year_t1"        ,  "Recruit"    ,      "Newplant"  ,      
                      "Survival_t1"   ,   "Height_t1"    ,    "Width_t1"    ,     "Perp_t1"  ,       
                      "NS_t1"      ,      "Goodbuds_t1"    ,  "TotFlowerbuds_t1", "ABFlowerbuds_t1" ,
                      "Antcount_t1"    ,  "Antsp_t1"    ,     "NP_adult"      ,   "NP_juv"    ,      
                      "CV"      ,         "WVL"         ,     "Damage"        ,   "MA"    ,          
                      "comments"    ,     "logsize_t"   ,     "logsize_t1"   ,    "ant_t"       ,    
                      "ant_t1"  ,         "flower1_YN" )     
# Remove extra columns
cactus <- cactus[ , c("Plot","TagID","Year_t","Goodbuds_t","TotFlowerbuds_t","ABFlowerbuds_t", "logsize_t","logsize_t1"                            ,"ant_t","ant_t1",
                      "Antcount_t","Year_t1","Recruit","Survival_t1","Goodbuds_t1","TotFlowerbuds_t1","ABFlowerbuds_t1"                            ,"Antcount_t1","flower1_YN","Newplant")]
# Export cactus to a csv
write.csv(cactus, "Data Analysis\cholla_demography_20042023_cleaned.csv")


## Set up functions and subsets of data ----
# 
# ## Germination Data (No ant state)  ----
# germ.dat_orig<-read.csv("Data Analysis/Germination.csv") 
# germ.dat_orig$rate <- 0
# for(i in 1:nrow(germ.dat_orig)){
#   if(germ.dat_orig$Seedlings04[i] != 0){
#     germ.dat_orig$rate[i] <- (germ.dat_orig$Seedlings04[i] - germ.dat_orig$Seedlings05[i])/germ.dat_orig$Seedlings04[i]
#   }
# }
# germ.dat <- na.omit(germ.dat_orig)
# germ.dat <- germ.dat[-c(42,39,40),]
# #Check that you are happy with the subsetting
# plot(germ.dat$rate)
# points(germ.dat_orig$rate, col = "red")
# # Most of the rows are included. We only lose 3, but it is still a small dataset. 
# nrow(germ.dat)
# nrow(germ.dat_orig)
# 
# ## Fruit Survival Data  (No ant state)  ----
# seedling.dat_orig <- subset(cactus, cactus$Recruit == 1)
# seedling.dat_orig <- seedling.dat_orig[,c("logsize_t1","Recruit","Year_t")]
# nrow(seedling.dat_orig)
# seedling.dat <- na.omit(seedling.dat_orig)
# nrow(seedling.dat)
# # losing 19 rows
# # check that you are happy with the subsetting
# plot(seedling.dat$logsize_t1, seedling.dat$Recruit, xlim = c(-5,15), ylim = c(0,1))
# points(cactus$logsize_t1, cactus$Recruit, col = "red")
# 
# ##    Precensus Survival Data (No ant state)  ----
# precensus.dat.orig<-read.csv("Data Analysis/PrecensusSurvival.csv") 
# precensus.dat <- precensus.dat.orig[ , c("Transect","Seed","Log.size","survive0405")]
# precensus.dat <- na.omit(precensus.dat)
# # You lose no data here, but it is still a small dataset. 
# nrow(precensus.dat)
# nrow(precensus.dat.orig)
# # check that you're happy with the subsetting
# plot(precensus.dat$Log.size, jitter(precensus.dat$survive0405))
# points(precensus.dat.orig$Log.size, jitter(precensus.dat.orig$survive0405), col = "red")
# 
# ## Growth Data (Includes Ant) ----   
# # All Ant States
# growth_data_orig <- cactus[ ,c("Plot","Year_t","Survival_t1","logsize_t","logsize_t1","ant_t")]
# growth_data <- na.omit(growth_data_orig)
# # Lose 2032 rows (due to plant death & recruit status)
# nrow(growth_data_orig)
# nrow(growth_data)
# summary(growth_data$ant_t)
# # check that you are happy with the subsetting
# plot(growth_data$logsize_t, growth_data$logsize_t1)
# points((cactus$logsize_t), (cactus$logsize_t1), col = "red")
# 
# ## Flowering Data  ----
# flower_data_orig <- cactus[ , c("TotFlowerbuds_t", "logsize_t","Year_t","Plot")]
# flower_data <- na.omit(flower_data_orig)
# flower_data <- subset(flower_data, TotFlowerbuds_t > 0)
# # Lose 6605 rows of data due to no flower data
# nrow(flower_data_orig)
# nrow(flower_data)
# # check that you're happy with the subsetting
# plot(flower_data$logsize_t, flower_data$TotFlowerbuds_t)
# points(cactus$logsize_t, cactus$TotFlowerbuds_t, col = "red")
# 
# ##  Survival Data (Includes Ants)  ----
# survival_data_orig <- cactus[ , c("Plot","Year_t","Survival_t1","ant_t","logsize_t")]
# survival_data <- na.omit(survival_data_orig)
# # Lose 1619 rows due to recruit status 
# nrow(survival_data_orig)
# nrow(survival_data)
# # check that you're happy with the subsetting
# plot(survival_data$logsize_t, (survival_data$Survival_t1))
# points(cactus$logsize_t, cactus$Survival_t1, col = "red")
# 
# ##   Repro  Data ----
# reproductive_data_orig <- cactus[ , c("flower1_YN","logsize_t","Year_t","Plot", "logsize_t1")]
# reproductive_data <- na.omit(reproductive_data_orig)
# # Lose 3332 rows of data because only including 
# nrow(reproductive_data_orig)
# nrow(reproductive_data)
# # check that you're happy with the subsetting
# plot(reproductive_data$logsize_t, reproductive_data$flower1_YN)
# points(cactus$logsize_t, cactus$flower1_YN, col = "red")
# 
# ## Viability Data (Includes Ants)   ----
# viability_data_orig <- cactus[ , c("TotFlowerbuds_t1","Goodbuds_t1","ABFlowerbuds_t1","ant_t", "logsize_t","Year_t","Plot")]
# viability_data <- na.omit(viability_data_orig)
# # Lose 3619 rows of data because of missing flower data
# nrow(viability_data_orig)
# viability_data <- subset(viability_data, TotFlowerbuds_t1 > 0)
# nrow(viability_data)
# # check if you're happy with the subsetting
# plot(viability_data$logsize_t, viability_data$ABFlowerbuds_t1, xlim = c(-5,15), ylim = c(0,60))
# plot(cactus$logsize_t, cactus$ABFlowerbuds_t1, col = "red") 
# # This one is not ideal because there is missing ant data
# 
# ##        Import Seed Data and create Subsets ----
# seed_uncleaned <- read.csv("Data Analysis/JO_fruit_data_final_dropplant0.csv", header = TRUE,stringsAsFactors=T)
# # PEAA = Ant Access
# # PAAA = Ant Access
# # PEAE = Ant Exclusion
# # PAAE = Ant Exclusion
# seed <- subset(seed_uncleaned, treatment == "PAAA" | treatment == "PAAE")
# nrow(seed)
# # make the column for the ant state of the part of the plant producing seeds
# for(i in 1:nrow(seed)){
#   #If there is no ant access then vacant
#   if(seed$ant.access[i] == "n" & is.na(seed$ant.access[i]) == FALSE){
#     seed$ant_state[i] <- "Vacant"
#   }
#   #If there is ant access but it is still vacant then vacant
#   if(seed$ant.access[i] == "y" & is.na(seed$ant.access[i]) == FALSE & seed$vacant[i] == "y"){
#     seed$ant_state[i] <- "Vacant"
#   }
#   #if there is ant access and it is not vacant and the ant is crem then crem
#   if(seed$ant.access[i] == "y" & is.na(seed$ant.access[i]) == FALSE & seed$vacant[i] == "n" & seed$species[i] == "c"){
#     seed$ant_state[i] <- "Crem"
#   }
#   #if there is ant access and it is not vacant and the ant is liom then liom
#   if(seed$ant.access[i] == "y" & is.na(seed$ant.access[i]) == FALSE & seed$vacant[i] == "n" & seed$species[i] == "l"){
#     seed$ant_state[i] <- "Liom"
#   }
# }
# seed_data <- seed
# seed_data <- na.omit(seed_data)
# seed_data$ant <- as.integer(as.factor(seed_data$ant_state))
# seed_data$plant_fac <- as.integer(as.factor(seed_data$plant))
# seed_data <- subset(seed_data, seed_count > 0)
# nrow(seed_uncleaned)
# nrow(seed_data)
# # check if you're happy with the subsetting
# #plot(seed$fruit_number)
# #points(seed_data$fruit_number, col = "red")
# 
# ## Further analyses ----
# # How many plants do we survey annually (on average)
# yr <- vector()
# for(i in 1:length(unique(cactus$Year_t))){
#   yr[i] <- nrow(subset(cactus, cactus$Year_t == 2003+i))
# }
# yr
# mean(yr[1:17])

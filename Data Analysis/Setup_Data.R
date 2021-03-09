library(rstanarm)
library(brms)  # for models
library(tidyverse)
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
#library(sjPlot)
knitr::opts_chunk$set(echo = TRUE)
options(mc.cores = parallel::detectCores())
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Data Analysis")

#import the data
cactus_uncleaned <- read.csv("cholla_demography_20042019.csv", header = TRUE,stringsAsFactors=T)
#str(cactus) ##<- problem: antcount is a factor
#levels(cactus$Antcount_t);levels(cactus$Antcount_t1)
## drop the offending rows -- this is a 2019 data entry problem
cactus <- cactus_uncleaned[-c(which(cactus_uncleaned$Antcount_t=="2-Jan"),which(cactus_uncleaned$Antcount_t1=="2-Jan")),]
cactus$antcount_t <- as.numeric(as.character(cactus_uncleaned$Antcount_t))
cactus$antcount_t1 <- as.numeric(as.character(cactus$Antcount_t1))
## function for the volume of a cone
volume <- function(h, w, p){
  (1/3)*pi*h*(((w + p)/2)/2)^2
}
invlogit <- function(x){exp(x)/(1+exp(x))}
#Create volume columns
cactus$volume_t <- volume(cactus$Height_t,cactus$Width_t, cactus$Perp_t)
cactus$volume_t1 <- volume(cactus$Height_t1,cactus$Width_t1, cactus$Perp_t1)
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
## Create a variable repro_state_t which tells you if the plant is reproducing in year t
cactus$repro_state_t <- NA
for(i in 1:8187){
  if(is.na(cactus$TotFlowerbuds_t[i]) == FALSE & cactus$TotFlowerbuds_t[i] > 0) {
    cactus$repro_state_t[i] <- 1
  }
  if(is.na(cactus$Goodbuds_t[i]) == FALSE & cactus$Goodbuds_t[i] > 0){
    cactus$repro_state_t[i] <- 1
  }
  if(is.na(cactus$Goodbuds_t[i]) == FALSE & is.na(cactus$TotFlowerbuds_t[i]) == FALSE & cactus$Goodbuds_t[i] < 1 & cactus$TotFlowerbuds_t[i] < 1){
    cactus$repro_state_t[i] <- 0
  }
  if(is.na(cactus$TotFlowerbuds_t1[i]) == FALSE & cactus$TotFlowerbuds_t1[i] > 0) {
    cactus$repro_state_t1[i] <- 1
  }
  if(is.na(cactus$Goodbuds_t1[i]) == FALSE & cactus$Goodbuds_t1[i] > 0){
    cactus$repro_state_t1[i] <- 1
  }
  if(is.na(cactus$Goodbuds_t1[i]) == FALSE & is.na(cactus$TotFlowerbuds_t1[i]) == FALSE & cactus$Goodbuds_t1[i] < 1 & cactus$TotFlowerbuds_t1[i] < 1){
    cactus$repro_state_t1[i] <- 0
  }
}

## Export cactus to a csv
write.csv(cactus, "cholla_demography_20042019_cleaned.csv")

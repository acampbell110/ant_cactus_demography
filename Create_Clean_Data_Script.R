########################################################################################################
########################################################################################################
##                                                                                                    ##
##              A script to clean the data of unsuspected errors                                      ##
##                                                                                                    ##
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

knitr::opts_chunk$set(echo = TRUE)
options(mc.cores = parallel::detectCores())
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Data Analysis")
#######################################################################################################
################        import the data -- Cacti (main)        #######################################################
#######################################################################################################
#cactus_uncleaned <- read.csv("cholla_demography_20042021.csv", header = TRUE,stringsAsFactors=T)
cactus <- read.csv("cholla_demography_20042019.csv", header = TRUE,stringsAsFactors=T)
str(cactus) ##<- problem: antcount is a factor
#levels(cactus$Antcount_t);levels(cactus$Antcount_t1)
## function for the volume of a cone
volume <- function(h, w, p){
  (1/3)*pi*h*(((w + p)/2)/2)^2
}
invlogit <- function(x){exp(x)/(1+exp(x))}
#Create volume columns
cactus$logsize_t <- log(volume(cactus$Height_t,cactus$Width_t, cactus$Perp_t))
cactus$logsize_t1 <- log(volume(cactus$Height_t1,cactus$Width_t1, cactus$Perp_t1))

#######################################################################################################
############           cactus 2021 data         #######################################################
#######################################################################################################
# ## assign ant counts of zero as vacant
# cactus$Antsp_t[cactus$antcount_t==0] <- "vacant"
# # here are the ordered levels of the current variable
# Antsp_t_levels <- levels(cactus$Antsp_t)
# # here is how I would like to collapse these into fewer bins -- most will be "other"
# ant_t_levels <- rep("other",times=length(Antsp_t_levels))
# ## crem levels - elements 5,8,9,10
# ant_t_levels[c(5,8,9,10,38)] <- "crem"
# ## liom levels - elements 15,18,19,20,21
# ant_t_levels[c(22,25,17,23,21,24)] <- "liom"
# ## vacant - 31
# ant_t_levels[c(1,37,36)] <- "vacant"
# ## create new variable merging levels as above
# cactus$ant_t <- factor(cactus$Antsp_t,levels=Antsp_t_levels,labels=ant_t_levels)
# ## first, there are no ant data from 2007:
# cactus$Antcount_t[cactus$Year_t==2007]
# #so give these observations NA for ant status:
# cactus$ant_t[cactus$Year_t==2007] <- NA
# # also, if a plant was new in year t+1 it's year t ant status should be NA
# cactus$ant_t[cactus$Newplant==1] <- NA
# # plots 7 and 8 were added in 2011, so their year_t==2010 ant status should be NA
# cactus$ant_t[cactus$Year_t==2010 & cactus$Plot==7] <- NA
# cactus$ant_t[cactus$Year_t==2010 & cactus$Plot==8] <- NA
# ## for now here is my workaround: year t ==2018, height_t ==NA, height_t1 !=NA
# cactus$ant_t[cactus$Year_t==2018 & is.na(cactus$Height_t) & !is.na(cactus$Height_t1)] <- NA
# ## finally there are these plants with Antsp_t=="" for all kinds of reasons but bottom line is that we don't have ant status
# cactus$ant_t[cactus$Antsp_t==""]<-NA
# ## inspect the "others" visually
# #cactus %>% filter(ant_t=="other") %>% select(Antsp_t,ant_t)
# ## I am satisfied that everything assigned other is correctly assigned
# ## assign ant counts of zero as vacant
# cactus$Antsp_t1[cactus$antcount_t1==0] <- "vacant"
# ## repeat for t1 -- these indices are different
# Antsp_t1_levels <- levels(cactus$Antsp_t1)
# # here is how I would like to collapse these into fewer bins -- most will be "other"
# ant_t1_levels <- rep("other",times=length(Antsp_t1_levels))
# ## crem levels 
# ant_t1_levels[c(10,8,11,9,39)] <- "crem"
# ## liom levels 
# ant_t1_levels[c(22,25,23,26,24)] <- "liom"
# ## vacant 
# ant_t1_levels[c(1,37,38)] <- "vacant"
# ## create new variable merging levels as above
# cactus$ant_t1 <- factor(cactus$Antsp_t1,levels=Antsp_t1_levels,labels=ant_t1_levels)
# summary(cactus$ant_t1) ## there are too many "others" -- same deal as above
# ## first, there are no ant data from 2007 so give these observations NA for ant status:
# cactus$ant_t1[cactus$Year_t1==2007] <- NA
# ## if we did not get size in t1 assume we did not get ant
# cactus$ant_t1[is.na(cactus$Height_t1)] <- NA
# # check to see how many others this leaves -- still a lot
# summary(cactus$ant_t1)
# ## there are some Antsp_t1=="" that show up as "other"
# #cactus %>% filter(cactus,ant_t1=="" & ant_t1=="other")
# ## not sure why this happens but apparently there are some plants for which we have ant counts but no ant sp. Could be a mix in comments. For now make these NA.
# cactus$ant_t1[cactus$Antsp_t1==""] <- NA
# ## inspect the "others" visually
# #cactus %>% filter(ant_t1=="other") %>% select(Antsp_t1,ant_t1)
# ## I am satisfied that everything assigned other in t1 is correctly assigned

####################################################################################################
########            Cactus 2019 data                    ############################################
####################################################################################################
## Change ant counts
cactus$Antcount_t <- as.numeric(as.character(cactus$Antcount_t))
cactus$Antcount_t1 <- as.numeric(as.character(cactus$Antcount_t1))
summary(as.factor(cactus$Antcount_t))
summary(as.factor(cactus$Antcount_t1))
## assign ant counts of zero as vacant
cactus$Antsp_t[cactus$Antcount_t==0] <- "vacant"
summary(cactus$Antsp_t)
# here are the ordered levels of the current variable
Antsp_t_levels <- levels(cactus$Antsp_t)
# here is how I would like to collapse these into fewer bins -- most will be "other"
ant_t_levels <- rep("other",times=length(Antsp_t_levels))
## crem levels - elements 5,8,9
ant_t_levels[c(5,8,9)] <- "crem"
## liom levels - elements 15,18,19,20,21
ant_t_levels[c(19,16,17,20,15,18,21)] <- "liom"
## vacant - 31
ant_t_levels[c(1,31)] <- "vacant"
## create new variable merging levels as above
cactus$ant_t <- factor(cactus$Antsp_t,levels=Antsp_t_levels,labels=ant_t_levels)
## first, there are no ant data from 2007:
cactus$Antcount_t[cactus$Year_t==2007]
#so give these observations NA for ant status: should be 118 values of NA now
cactus$ant_t[cactus$Year_t==2007] <- NA
# also, if a plant was new in year t+1 it's year t ant status should be NA now up to 503 NA values
cactus$ant_t[cactus$Newplant==1] <- NA
# plots 7 and 8 were added in 2011, so their year_t==2010 ant status should be NA up to 638 NAs
cactus$ant_t[cactus$Year_t==2010 & cactus$Plot==7] <- NA
cactus$ant_t[cactus$Year_t==2010 & cactus$Plot==8] <- NA
## for now here is my workaround: year t ==2018, height_t ==NA, height_t1 !=NA
cactus$ant_t[cactus$Year_t==2018 & is.na(cactus$Height_t) & !is.na(cactus$Height_t1)] <- NA
## finally there are these plants with Antsp_t=="" for all kinds of reasons but bottom line is that we don't have ant status
cactus$ant_t[cactus$Antsp_t==""]<-NA
## Check the distribution of the ant species
summary(cactus$ant_t)
## I am satisfied that everything assigned other is correctly assigned
## assign ant counts of zero as vacant
cactus$Antsp_t1[cactus$Antcount_t1==0] <- "vacant"

## repeat for t1 -- these indices are different
Antsp_t1_levels <- levels(cactus$Antsp_t1)
# here is how I would like to collapse these into fewer bins -- most will be "other"
ant_t1_levels <- rep("other",times=length(Antsp_t1_levels))
## crem levels 
ant_t1_levels[c(8,9,10)] <- "crem"
## liom levels 
ant_t1_levels[c(19,22,17,20,18,21)] <- "liom"
## forelius levels 
#ant_t1_levels[c(13,14,15)] <- "for"
## vacant 
ant_t1_levels[c(1,32)] <- "vacant"
## create new variable merging levels as above
cactus$ant_t1 <- factor(cactus$Antsp_t1,levels=Antsp_t1_levels,labels=ant_t1_levels)
## check the spread of these ants
summary(cactus$ant_t1) ## there are too many "vacants" -- same deal as above
## first, there are no ant data from 2007 so give these observations NA for ant status:
cactus$ant_t1[cactus$Year_t1==2007] <- NA
## if we did not get size in t1 assume we did not get ant
cactus$ant_t1[is.na(cactus$Height_t1)] <- NA
# check to see how many others this leaves -- still a lot
summary(cactus$ant_t1)
## there are some Antsp_t1=="" that show up as "other"
#cactus %>% filter(Antsp_t1=="" & ant_t1=="other")
## finally there are these plants with Antsp_t=="" for all kinds of reasons but bottom line is that we don't have ant status
cactus$ant_t[cactus$Antsp_t==""]<-NA
## Relevel so that vacancy is the reference level
summary(cactus$ant_t1)
cactus$ant_t1 <- relevel(cactus$ant_t1,ref = "vacant")
cactus$ant_t <- relevel(cactus$ant_t, ref = "vacant")

## Check the Flower data
summary(cactus$Goodbuds_t)
summary(cactus$Goodbuds_t1) ## Here you can see that there are a LOT of NAs
summary(cactus$TotFlowerbuds_t)
summary(cactus$TotFlowerbuds_t1) ## Again a LOT of NAs (The most)
summary(cactus$ABFlowerbuds_t)
summary(cactus$ABFlowerbuds_t1) ## Fewer, but still a LOT

#### If Goodbuds is NA but Abortbuds and Totalbuds is not, set goodbuds = Tot - Ab (0 rows for t and 0 rows for t1)
for(i in 1:nrow(cactus)){
  if(is.na(cactus$Goodbuds_t[i]) == TRUE & is.na(cactus$ABFlowerbuds_t[i]) == FALSE & is.na(cactus$TotFlowerbuds_t[i]) == FALSE){
    cactus$Goodbuds_t[i] <- cactus$TotFlowerbuds_t[i] - cactus$ABFlowerbuds_t[i]
  }
  if(is.na(cactus$Goodbuds_t1[i]) == TRUE & is.na(cactus$ABFlowerbuds_t1[i]) == FALSE & is.na(cactus$TotFlowerbuds_t1[i]) == FALSE){
    cactus$Goodbuds_t1[i] <- cactus$TotFlowerbuds_t1[i] - cactus$ABFlowerbuds_t1[i]
  }
}
#### If Abortbuds is NA but Goodbuds and Totalbuds is not, set AB = Tot - Good (0 rows for t and 287 rows for t1)
for(i in 1:nrow(cactus)){
  if(is.na(cactus$ABFlowerbuds_t[i]) == TRUE & is.na(cactus$Goodbuds_t[i]) == FALSE & is.na(cactus$TotFlowerbuds_t[i]) == FALSE){
    cactus$ABFlowerbuds_t[i] <- cactus$TotFlowerbuds_t[i] - cactus$Goodbuds_t[i]
  }
  if(is.na(cactus$Goodbuds_t1[i]) == TRUE & is.na(cactus$ABFlowerbuds_t1[i]) == FALSE & is.na(cactus$TotFlowerbuds_t1[i]) == FALSE){
    cactus$ABFlowerbuds_t1[i] <- cactus$TotFlowerbuds_t1[i] - cactus$Goodbuds_t1[i]
  }
}
#### If Totalbuds is NA but Goodbuds and Abortbuds is not, set Tot = AB + Good
for(i in 1:nrow(cactus)){
  if(is.na(cactus$TotFlowerbuds_t[i]) == TRUE & is.na(cactus$Goodbuds_t[i]) == FALSE & is.na(cactus$ABFlowerbuds_t[i]) == FALSE){
    cactus$TotFlowerbuds_t[i] <- cactus$ABFlowerbuds_t[i] + cactus$Goodbuds_t[i]
  }
  if(is.na(cactus$TotFlowerbuds_t1[i]) == TRUE & is.na(cactus$Goodbuds_t1[i]) == FALSE & is.na(cactus$ABFlowerbuds_t1[i]) == FALSE){
    cactus$TotFlowerbuds_t1[i] <- cactus$ABFlowerbuds_t1[i] + cactus$Goodbuds_t1[i]
  }
}



## Create a variable that shows if the cacti are flowering
cactus$flower1_YN<-cactus$TotFlowerbuds_t1>0
summary(cactus$flower1_YN)


## Create a variable that shows if the cacti are vacant or occupied
cactus$occ_t[cactus$ant_t == "vacant"] <- "vac"
cactus$occ_t[cactus$ant_t == "crem" | cactus$ant_t == "liom" | cactus$ant_t == "other"] <- "occ"
cactus$occ_t1[cactus$ant_t1 == "vacant"] <- "vac"
cactus$occ_t1[cactus$ant_t1 == "crem" | cactus$ant_t1 == "liom" | cactus$ant_t1 == "other"] <- "occ"

cactus$ant_t1 <- relevel(cactus$ant_t1,ref = "vacant")

## If the plant is a new plant or a recruit, make the survival status NA
cactus$Survival_t1[cactus$Recruit == 1] <- NA
cactus$Survival_t1[cactus$Newplant == 1] <- NA

## Remove extra columns
cactus <- cactus[ , c("Plot","TagID","Year_t","Goodbuds_t","TotFlowerbuds_t","ABFlowerbuds_t", "logsize_t","logsize_t1","ant_t","ant_t1",
                      "Antcount_t","Year_t1","Recruit","Survival_t1","Goodbuds_t1","TotFlowerbuds_t1","ABFlowerbuds_t1","Antcount_t1",
                      "occ_t","occ_t1","flower1_YN", "ant_t_relevel","ant_t1_relevel")]

cactus_herb <- cactus[, c("NP_adult","NP_juv","MA","ant_t")]
cactus_herb$NP_adult[cactus_herb$NP_adult == "dead"] <- NA
str(cactus_herb)
## Export cactus to a csv
write.csv(cactus, "cholla_demography_20042019_cleaned.csv")




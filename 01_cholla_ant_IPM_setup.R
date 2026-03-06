################################################################################ 
################################################################################
## The purpose of this script is to load all required packages, load the data 
## and fix any errors (such as excel taking numbers as dates or mistyping ant 
## species names etc.), write a "clean" data file which has all errors 
## fixed and can be called directly and run through all other r scripts, and 
## finally load in any other data files.
################################################################################
################################################################################
## Load All Necessary Packages Here ##############
# stan models
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
# distributions
library(sn)
library(sgt)
library(qgam)
library(brms)
library(loo)
library(simcausal)
# ipm analysis
library(popbio)
# visual model checks
library(moments)
library("posterior")
library(gridExtra)
library(grid)
library(MASS)
library(patchwork)
library(tidyverse)
# data management
library(dplyr)
library(magrittr)
# graphics
library(RColorBrewer)
library(corrplot)
library(ggpubr)
library(reshape)
library(reshape2)
library(stats)
# knitr::oknitr::oknitr::opts_chunk$set(echo = TRUE)

## Create the necessary functions ###############
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

## import the data ##############
cactus <- read.csv("Data/cholla_demography_20042023.csv", header = TRUE,stringsAsFactors=T)
## Fix Plot naming issues ##################
## re-assign the seedling plots ("HT1B1" etc) to transects 1-3
levels(cactus$Plot)<-c(levels(cactus$Plot),"T4")
cactus$Plot[cactus$Plot=="HT1B1"]<-"T1"
cactus$Plot[cactus$Plot=="HT2B3"]<-"T2"
cactus$Plot[cactus$Plot=="HT3B1" | cactus$Plot=="HT3B2" | cactus$Plot=="HT3B3"]<-"T3"
cactus$Plot[cactus$Plot=="HT4B1" | cactus$Plot=="HT4B2"]<-"T4"

## Size fixes ################
# Create volume columns that are log scaled
cactus$logsize_t <- log(volume(cactus$Height_t,cactus$Width_t, cactus$Perp_t))
cactus$logsize_t1 <- log(volume(cactus$Height_t1,cactus$Width_t1, cactus$Perp_t1))

## Ant Species Renaming #######################
# Change ant counts to numeric (some random entries are different types of strings)
cactus$Antcount_t <- as.numeric(as.character(cactus$Antcount_t))
cactus$Antcount_t1 <- as.numeric(as.character(cactus$Antcount_t1))
#summary(as.factor(cactus$Antcount_t))
#summary(as.factor(cactus$Antcount_t1))
# assign ant counts of zero as vacant
cactus$Ant_sp_t[cactus$Antcount_t==0] <- "vacant"
#summary(cactus$Ant_sp_t)
# here are the ordered levels of the current variable
Ant_sp_t_levels <- levels(cactus$Ant_sp_t)
# here is how I would like to collapse these into fewer bins -- most will be "other"
# Start with all "other"
ant_t_levels <- rep("other", times = length(Ant_sp_t_levels))  # 52 elements
# Assign "vacant" to correct positions
ant_t_levels[c(1,42,43,44)] <- "vacant"
# Assign "crem" to correct positions
ant_t_levels[c(5,8,9,10,11,22,45)] <- "crem"
# Assign "liom" to correct positions
ant_t_levels[c(19,25,26,27,28,29)] <- "liom"
# Now create the new factor using the mapping
cactus$ant_t <- factor(cactus$Ant_sp_t,
                       levels = Ant_sp_t_levels,
                       labels = ant_t_levels) 
# with this data: first, there are no ant data from 2007:
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
# finally there are these plants with Ant_sp_t=="" for all kinds of reasons but bottom line is that we don't have ant status
cactus$ant_t[cactus$Ant_sp_t==""]<-NA
# Check the distribution of the ant species
#summary(cactus$ant_t)
# 4440 vac, 614 other, 659 813, 3422 liom, 1316 NA
# repeat for t1 -- these indices are different
Ant_sp_t1_levels <- levels(cactus$Ant_sp_t1)
# here is how I would like to collapse these into fewer bins -- most will be "other"
ant_t1_levels <- rep("other",times=length(Ant_sp_t1_levels))
# crem levels
ant_t1_levels[c(7,8,9,10,20,42)] <- "crem"
# liom levels
ant_t1_levels[c(23,24,25,26,27)] <- "liom"
# vacant
ant_t1_levels[c(1,40,41)] <- "vacant"
# create new variable merging levels as above
cactus$ant_t1 <- factor(cactus$Ant_sp_t1,levels=Ant_sp_t1_levels,labels=ant_t1_levels)
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
# there are some Ant_sp_t1=="" that show up as "other"
#cactus %>% filter(Ant_sp_t1=="" & ant_t1=="other")
# finally there are these plants with Ant_sp_t=="" for all kinds of reasons but bottom line is that we don't have ant status
cactus$ant_t[cactus$Ant_sp_t==""]<-NA
# Relevel so that vacancy is the reference level
cactus$ant_t1 <- relevel(cactus$ant_t1,ref = "vacant")
cactus$ant_t <- relevel(cactus$ant_t, ref = "vacant")

## Flower Count Data Fixes ################
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


# Create a variable called Flower Yes No, that determines if there are flowers present
cactus$flower_YN<-as.integer(cactus$TotFlowerbuds_t > 0)
summary(cactus$flower_YN)
# View(cactus)

## Recruitment ################
# If the plant is a new plant or a recruit, make the survival status NA
cactus$Survival_t1[cactus$Recruit == 1] <- NA
cactus$Survival_t1[cactus$Newplant == 1] <- NA

## clean up recruit info
recruit_check<-cactus %>% filter(Recruit==1)
hist(recruit_check$Height_t1)
##for reference, the seedlings in the seed addition plots were "true" recruits
true_recruits<-recruit_check %>% filter(Plot %in% c("T1","T2","T3","T4"))
## note these sizes appear in year t
hist(true_recruits$Height_t)
## I don't believe that any recruits are taller than 5cm, so apply this rule
cactus$Recruit<-ifelse(cactus$Recruit==1 & cactus$Height_t1>5,0,cactus$Recruit)
hist(cactus$Height_t1[cactus$Recruit==1])

## Transition Year Fixes ####################
library(dplyr)
# break up the 2019-2021 transition year into two incomplete transition years
cactus %>% dplyr::filter(Year_t==2019) %>% dplyr::select(Year_t1)
#create the 2019-2020 transition year
##drop the rows added for 2021 recruits
cactus_2019temp<-cactus[cactus$Year_t==2019 & cactus$Newplant==0,]
cactus_2019temp$Year_t1<-2020
cactus_2019temp[,c("logsize_t1","Survival_t1","ant_t1",
                   "Goodbuds_t1","TotFlowerbuds_t1","ABFlowerbuds_t1",
                   "flower_YN")]<-NA
## create the 2020-2021 transition year
cactus_2020temp_notnew<-cactus[cactus$Year_t1==2021 & cactus$Newplant==0,]
cactus_2020temp_notnew$Year_t<-2020
cactus_2020temp_notnew[,c("Goodbuds_t","TotFlowerbuds_t","ABFlowerbuds_t",
                          "logsize_t","ant_t","Antcount_t")]<-NA
#note that 2021 survival is still here but we don't know whether mortality occurred in 2019 or 2020 transition year
cactus_2020temp_new<-cactus[cactus$Year_t1==2021 & cactus$Newplant==1,]
cactus_2020temp_new$Year_t<-2020
## put them together
cactus_2020temp<-rbind(cactus_2020temp_notnew,cactus_2020temp_new)
## now drop the 2019-2021 transition year and add these two new transition years
cactus <- rbind(cactus[cactus$Year_t!=2019,],cactus_2019temp,cactus_2020temp)


## Export the data ######################
#head(cactus) # some of the columns are not names properly
# rename all columns
colnames(cactus) <- c("Plot" ,            "TagID"     ,       "Transplant"   ,    "Year_t" ,         
                      "Height_t"     ,    "Width_t"     ,     "Perp_t"      ,     "NS_t"   ,         
                      "Goodbuds_t"    ,   "TotFlowerbuds_t" , "ABFlowerbuds_t" ,  "Antcount_t" ,     
                      "Ant_sp_t"     ,     "Year_t1"        ,  "Recruit"    ,      "Newplant"  ,      
                      "Survival_t1"   ,   "Height_t1"    ,    "Width_t1"    ,     "Perp_t1"  ,       
                      "NS_t1"      ,      "Goodbuds_t1"    ,  "TotFlowerbuds_t1", "ABFlowerbuds_t1" ,
                      "Antcount_t1"    ,  "Ant_sp_t1"    ,     "NP_adult"      ,   "NP_juv"    ,      
                      "CV"      ,         "WVL"         ,     "Damage"        ,   "MA"    ,          
                      "comments"    ,     "logsize_t"   ,     "logsize_t1"   ,    "ant_t"       ,    
                      "ant_t1"  ,         "flower_YN" )     

# Remove extra columns
cactus <- cactus[ , c("Plot","TagID","Year_t","Goodbuds_t","TotFlowerbuds_t",
                      "ABFlowerbuds_t", "logsize_t","logsize_t1","ant_t","ant_t1",
                      "Antcount_t","Year_t1","Recruit","Survival_t1","Goodbuds_t1",
                      "TotFlowerbuds_t1","ABFlowerbuds_t1","Antcount_t1",
                      "flower_YN","Newplant","Damage","NP_adult","NP_juv","CV","WVL","MA")]



# Export cactus to a csv
write.csv(cactus, "Data/cholla_demography_20042023_cleaned.csv")






## Set up all subsets of data needed ###############
## Growth model
## Pull all necessary variables together, remove NAs, and put them into a list so they are ready to feed into the stan model
growth_data_orig <- cactus[,c("Plot","Year_t","logsize_t","logsize_t1","ant_t")]
growth_data <- na.omit(growth_data_orig) # Lose 2032 rows (due to plant death & recruit status)

## Make a list of all necessary variables so they are properly formatted to feed into the stan model
stan_data_grow_skew <- list(N = nrow(growth_data),                                     ## number of observations
                            vol = (growth_data$logsize_t),                             ## predictor volume year t
                            vol2 = (growth_data$logsize_t)^2,                          ## non linear volume year t predictor
                            y = (growth_data$logsize_t1),                              ## response volume next year
                            ant = as.integer(as.factor(growth_data$ant_t)),            ## predictor ant state
                            K = 4,                                                     ## number of ant states
                            N_Year = max(as.integer(as.factor(growth_data$Year_t))),   ## number of years
                            N_Plot = max(as.integer(growth_data$Plot)),                ## number of plots
                            plot = as.integer(growth_data$Plot),                       ## predictor plots
                            year = as.integer(as.factor(growth_data$Year_t))           ## predictor years
)
## Survival model
## Pull all necessary variables together, remove NAs, and put them into a list so they are ready to feed into the stan model
#survival_data_orig <- subset(cactus, is.na(Survival_t1) == FALSE,c("Plot","Year_t","Survival_t1","ant_t","logsize_t"))
survival_data_orig <- cactus[,c("Plot","Year_t","Survival_t1","ant_t","logsize_t")]
survival_data <- na.omit(survival_data_orig)

## Create Stan Data
stan_data_surv <- list(N = nrow(survival_data),                                    ## number of observations
                       vol = (survival_data$logsize_t),                            ## predictors volume
                       y_surv = (survival_data$Survival_t1),                       ## response survival next year
                       ant = as.integer(as.factor(survival_data$ant_t)),           ## predictors ants
                       K = 4,                                                      ## number of ant states
                       N_Year = max(as.integer(as.factor(survival_data$Year_t))),  ## number of years
                       N_Plot = max(as.integer(as.factor(survival_data$Plot))),    ## number of plots
                       plot = as.integer(as.factor(survival_data$Plot)),           ## predictor plots
                       year = as.integer(as.factor(survival_data$Year_t))          ## predictor years
) 
## Repro model 
## Pull all necessary variables together, remove NAs, and put them into a list so they are ready to feed into the stan model
reproductive_data_orig <- cactus[ , c("TotFlowerbuds_t","logsize_t","Year_t","Plot")]
reproductive_data_orig$flower_YN<-reproductive_data_orig$TotFlowerbuds_t>0
reproductive_data <- na.omit(reproductive_data_orig)

## Create Stan Data
stan_data_repro <- list(N = nrow(reproductive_data),                                   ## number of observations
                        vol = reproductive_data$logsize_t,                            ## predictors volume
                        y_repro = reproductive_data$flower_YN,                        ## response volume next year
                        N_Year = max(as.integer(as.factor(reproductive_data$Year_t))), ## number of years
                        N_Plot = max(as.integer(as.factor(reproductive_data$Plot))),   ## number of plots
                        plot = as.integer(as.factor(reproductive_data$Plot)),          ## predictor plots
                        year = as.integer(as.factor(reproductive_data$Year_t))         ## predictor years
) 
## Viab model
## Pull all necessary variables together, remove NAs, and put them into a list so they are ready to feed into the stan model
viability_data_orig <- cactus[ , c("TotFlowerbuds_t","Goodbuds_t","ABFlowerbuds_t","ant_t", "logsize_t","Year_t","Plot")]
viability_data_orig <- subset(viability_data_orig, TotFlowerbuds_t > 0)
viability_data <- na.omit(viability_data_orig)

## Create stan data subset
stan_data_viab <- list(N = nrow(viability_data),                                   ## number of observations
                       good = viability_data$Goodbuds_t,                          ## number of good flowerbuds 
                       abort = viability_data$ABFlowerbuds_t,                     ## aborted buds data
                       tot = viability_data$TotFlowerbuds_t,                      ## number of trials
                       ant = as.integer(as.factor(viability_data$ant)),            ## predictors ants
                       K = 4,                                                      ## number of ant states
                       N_Year = max(as.integer(as.factor(viability_data$Year_t))), ## number of years
                       N_Plot = max(as.integer(as.factor(viability_data$Plot))),   ## number of plots
                       plot = as.integer(as.factor(viability_data$Plot)),          ## predictor plots
                       year = as.integer(as.factor(viability_data$Year_t))         ## predictor years
) 

## Flowers model
## Pull all necessary variables together, remove NAs, and put them into a list so they are ready to feed into the stan model
flower_data_orig <- cactus[ , c("TotFlowerbuds_t", "logsize_t","Year_t","Plot")]
flower_data_orig <- subset(flower_data_orig, TotFlowerbuds_t > 0)
flower_data <- na.omit(flower_data_orig)

## Create Stan Data
stan_data_flow_trunc <- list(N = nrow(flower_data),                                   ## number of observations
                             lower_limit = 1,                                         ## we want the 0s to be removed
                             vol = (flower_data$logsize_t),                           ## predictors volume
                             y_flow = flower_data$TotFlowerbuds_t,                    ## response flowers next year
                             N_Year = max(as.integer(as.factor(flower_data$Year_t))), ## number of years
                             N_Plot = max(as.integer(as.factor(flower_data$Plot))),   ## number of plots
                             plot = as.integer(as.factor(flower_data$Plot)),          ## predictor plots
                             year = as.integer(as.factor(flower_data$Year_t))         ## predictor years
)
## Germ model 
germ.dat_orig<-read.csv("Data/Germination.csv") 
germ.dat_orig$input04<-germ.dat_orig$Input
germ.dat_orig$input05<-germ.dat_orig$Input-germ.dat_orig$Seedlings04
germ.dat04<-germ.dat_orig[,c("input04","Seedlings04")];names(germ.dat04)<-c("seeds","seedlings")
germ.dat04$year<-1
germ.dat05<-germ.dat_orig[,c("input05","Seedlings05")];names(germ.dat05)<-c("seeds","seedlings")
germ.dat05$year<-2
germ.dat<-rbind(germ.dat04,germ.dat05)

## Create Stan Data
stan_data_germ <- list(N = nrow(germ.dat),
                       y_germ = germ.dat$seedlings,
                       trials = germ.dat$seeds,
                       year=germ.dat$year)

## Seeds model
## Pull all necessary variables together, remove NAs, and put them into a list so they are ready to feed into the stan model
seed_uncleaned <- read.csv("Data/JO_fruit_data_final_dropplant0.csv", header = TRUE,stringsAsFactors=T)
seed <- subset(seed_uncleaned, treatment == "PAAA" | treatment == "PAAE" | treatment == "PA")
seed$ant_state <- NA
seed$ant_state[seed$ant.access=="n"]<-"Vacant"
seed$ant_state[seed$ant.access=="y" & seed$species=="c"]<-"Crem"
seed$ant_state[seed$ant.access=="y" & seed$species=="l"]<-"Liom"
seed_data <- seed[,c("plant","ant_state","seed_count")]
seed_data <- drop_na(seed_data)

## Create Stan Data
stan_data_seed <- list(N = nrow(seed_data),                            ## number of observations
                       N_plants = length(unique(seed_data$plant)),
                       K = 3,                                          ## number of ant states
                       plant = as.integer(as.factor(seed_data$plant)),     ## ant partners data
                       ant = as.integer(as.factor(seed_data$ant_state)),     ## ant partners data
                       seed = seed_data$seed_count)                    ## number of seeds data
## Seedling surv model
## Pull all necessary variables together, remove NAs, and put them into a list so they are ready to feed into the stan model
precensus.dat.orig<-read.csv("Data/PrecensusSurvival.csv") 
precensus.dat <- precensus.dat.orig[ , c("Transect","Seed","Log.size","survive0405")]
precensus.dat <- na.omit(precensus.dat)
#
## Create Stan Data
stan_data_seed_surv <- list(N = nrow(precensus.dat),                                  ## number of observations
                            y = precensus.dat$survive0405)                            ## survival data

## Fruit surv model
## Pull all necessary variables together, remove NAs, and put them into a list so they are ready to feed into the stan model
fruit_data_orig <- read.csv("Data/FruitSurvival.csv", header = TRUE,stringsAsFactors=T)
fruit_data <- na.omit(fruit_data_orig)

## Create stan data subset
stan_data_fruit <- list(N = nrow(fruit_data),                                   ## number of observations
                        good = fruit_data$Fr.on.grnd.not.chewed,                 ## number of good flowerbuds 
                        tot = fruit_data$Fr.on.plant                             ## number of trials
) 

## Recruits model
## pull out the seedlings from the seed addition plots
# there is seedling size info in two places:
#1. the seed addition plots, where seedling size appears in year t
# these tag IDs are the only ones that start with H
seed_add<-cactus %>% filter(substr(TagID,1,1)=="H") %>% drop_na()
#2. the main census, where new plants get called recruits or not by observers
recruits <- cactus[,c("Recruit","logsize_t1")]
recruits <- subset(recruits,recruits$Recruit == 1)
recruits <- drop_na(recruits)
seedling.dat <- c(recruits$logsize_t1,seed_add$logsize_t)

## Create Stan Data
stan_data_rec <- list(N = length(seedling.dat),
                      y_rec = (seedling.dat)
)

## Ant transition model 
cactus_real <- cactus[,c("ant_t","ant_t1","logsize_t","Year_t","Plot")]
cactus_real <- na.omit(cactus_real)
cactus_real$ant_t1 <- relevel(cactus_real$ant_t1,ref = "vacant")
cactus_real$ant_t <- relevel(cactus_real$ant_t, ref = "vacant")
cactus_real <- cactus_real[,c("logsize_t", "ant_t", "ant_t1","Year_t","Plot")]

unique(cactus_real$Year_t)
cactus_real$ant_t1 <- factor(cactus_real$ant_t1, levels = c("crem","liom","other","vacant"))
cactus_real$ant_t <- factor(cactus_real$ant_t, levels = c("crem","liom","other","vacant"))
cactus_real <- subset(cactus_real, cactus_real$Year_t != 2022 & cactus_real$Year_t != 2021)


## make stan data set
stan_data_multi <- list(K = length(unique(cactus_real$ant_t1)), #number of possible ant species
                        N = dim(cactus_real)[1], #number of observations
                        D = 5, #number of predictors
                        P = 13, #number of random effect predictors
                        y = as.integer(as.factor(cactus_real$ant_t1)), #observations
                        x = model.matrix(~ 0 + (as.factor(ant_t)) + logsize_t, cactus_real), #design matrix
                        z = model.matrix(~0 + as.factor(Year_t), cactus_real),
                        N_Year = as.integer(length(unique(cactus_real$Year_t)))
)


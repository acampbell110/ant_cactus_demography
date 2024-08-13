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
library(qgam)
library(brms)
library(loo)
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
#getwd()
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
#setwd("C:/Users/tm9/Dropbox/github/ant_cactus_demography")

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
cactus <- read.csv("Data Analysis/cholla_demography_20042023.csv", header = TRUE,stringsAsFactors=T)
# str(cactus) ##<- problem: antcount is a factor

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

## Get the frequencies of ant species
Antsp_t_levels <- levels(cactus$Antsp_t)
# liom
a <- subset(cactus, cactus$Antsp_t == "Liom" | cactus$Antsp_t == "LIOM" | cactus$Antsp_t == "liom" | cactus$Antsp_t == "L" | cactus$Antsp_t == "LIOM " |  cactus$Antsp_t == "liom ")
nrow(a)
# crem
b <- subset(cactus, cactus$Antsp_t == "CREN" | cactus$Antsp_t == "LCREM" | cactus$Antsp_t == "crem" | cactus$Antsp_t == "Crem" | cactus$Antsp_t == "C" | cactus$Antsp_t == "CREM" | cactus$Antsp_t == "VCREM")
nrow(b)
# camp
c <- subset(cactus, cactus$Antsp_t == "camp" | cactus$Antsp_t == "CAMP" | cactus$Antsp_t == "CAMP" | cactus$Antsp_t == "large black shiny" | cactus$Antsp_t == "LARGE BLACK SHINY" | cactus$Antsp_t == "drpoff" | cactus$Antsp_t == "dropoff")
nrow(c)
# honeypot
d <- subset(cactus, cactus$Antsp_t ==  "HNEYPOT" | cactus$Antsp_t ==  "honeypot" | cactus$Antsp_t == "HONEYPOT")
nrow(d)
# phen
e <- subset(cactus, cactus$Antsp_t == "phen" | cactus$Antsp_t == "PHEN" | cactus$Antsp_t == "aph" | cactus$Antsp_t == "unk (Aphaeno?)")
nrow(e)
# tetra
h <- subset(cactus, cactus$Antsp_t == "tetra")
nrow(h)
# brachy
j <- subset(cactus, cactus$Antsp_t == "brachy")
nrow(j)
# unknown
k <- subset(cactus, cactus$Antsp_t == "unk" | cactus$Antsp_t == "lg unk" | cactus$Antsp_t == "UNK" | cactus$Antsp_t == "unk " | cactus$Antsp_t == "other" | cactus$Antsp_t == "SMALL RED-BROWN SPINDLY" | cactus$Antsp_t == "black shiny red thorax" | cactus$Antsp_t == "RED HEAD BLK BUTT" | cactus$Antsp_t == "shiny black red thorax" | cactus$Antsp_t == "shiny black, red thorax") 
nrow(k)
l <- subset(cactus,cactus$Antsp_t == "LFOR" | cactus$Antsp_t == "for" | cactus$Antsp_t == "FOR")
nrow(l)
## Cactus 2023 data cleaning ---- Ant Species
# Change ant counts to numeric (some random entries are different types of strings)
cactus$Antcount_t <- as.numeric(as.character(cactus$Antcount_t))
cactus$Antcount_t1 <- as.numeric(as.character(cactus$Antcount_t1))
#summary(as.factor(cactus$Antcount_t))
#summary(as.factor(cactus$Antcount_t1))
# assign ant counts of zero as vacant
cactus$Antsp_t[cactus$Antcount_t==0] <- "vacant"
#summary(cactus$Antsp_t)
# here are the ordered levels of the current variable
Antsp_t_levels <- levels(cactus$Antsp_t)
# here is how I would like to collapse these into fewer bins -- most will be "other"
ant_t_levels <- rep("other",times=length(Antsp_t_levels))
# crem levels - elements
ant_t_levels[c(5,8,9,10,11,22,45)] <- "crem"
# liom levels - elements
ant_t_levels[c(19,25,26,27,28,29)] <- "liom"
# vacant - elements
ant_t_levels[c(1,42,43,44)] <- "vacant"
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
ant_t1_levels[c(8,9,10,11,12,23,45)] <- "crem"
# liom levels
ant_t1_levels[c(26,27,28,29,30)] <- "liom"
# vacant
ant_t1_levels[c(1,43,44)] <- "vacant"
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
cactus$flower_YN<-cactus$TotFlowerbuds_t
summary(cactus$flower_YN)
View(cactus)
cactus$ant_t1 <- relevel(cactus$ant_t1,ref = "vacant")

## Cactus 2023 data cleaning ---- Recruitment
# If the plant is a new plant or a recruit, make the survival status NA
cactus$Survival_t1[cactus$Recruit == 1] <- NA
cactus$Survival_t1[cactus$Newplant == 1] <- NA


## Export the data ---- Write a data file which has "cleaned" data with proper column headings
#head(cactus) # some of the columns are not names properly
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
                      "Antcount_t","Year_t1","Recruit","Survival_t1","Goodbuds_t1","TotFlowerbuds_t1","ABFlowerbuds_t1"                            ,"Antcount_t1","flower1_YN","Newplant","Damage","NP_adult","NP_juv","CV","WVL","MA")]
# Export cactus to a csv
write.csv(cactus, "Data Analysis/cholla_demography_20042023_cleaned.csv")



## Further analyses ----
# How many plants do we survey annually (on average)
yr <- vector()
for(i in 1:length(unique(cactus$Year_t))){
  yr[i] <- nrow(subset(cactus, cactus$Year_t == 2003+i))
}
yr
mean(yr[1:17])


## Get numbers on years of data nd number of individual plants
# create a combined variable of plot and ID and check for unique combos
cactus$plot_ID <- paste(cactus$Plot, cactus$TagID)
length(unique(cactus$plot_ID))
# get the number of years
length(unique(cactus$Year_t1))

## Get the proportions of each species
# how many ant observations are there
cac <- subset(cactus, cactus$ant_t != "vacant")
nrow(cac)
liom_prop <- nrow(a)/nrow(cac)
crem_prop <- nrow(b)/nrow(cac)
camp_prop <- nrow(c)/nrow(cac)
honey_prop <- nrow(d)/nrow(cac)
phen_prop <- nrow(e)/nrow(cac)
tetra_prop <- nrow(h)/nrow(cac)
brach_prop <- nrow(j)/nrow(cac)
unk_prop <- nrow(k)/nrow(cac)
for_prop <- nrow(l)/nrow(cac)
other_prop <- sum(camp_prop,honey_prop,phen_prop,tetra_prop,brach_prop,unk_prop,for_prop)

nrow(cactus)

# length(unique(cactus$Year_t))
# 
# png("Figures/Ant_Props.png")
# barplot(c(liom_prop,crem_prop,other_prop), col = c(liomcol,cremcol,othercol))
# legend("topright",legend = c("Liom.","Crem.","Other"), fill = c(liomcol,cremcol,othercol))
# dev.off()







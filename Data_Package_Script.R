#############################################################
## The purpose of this script is to accompany a data package.
## This script will go through the process of cleaning errors 
## in the data entry. d
#############################################################

# Set the working directory 
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography")
#setwd("C:/Users/tm9/Dropbox/github/ant_cactus_demography")

## Create the necessary functions 
# function for the volume of a cone
volume <- function(h, w, p){
  (1/3)*pi*h*(((w + p)/2)/2)^2
}

## import the data -- Cacti
cactus <- read.csv("Data/cholla_demography_20042023.csv", header = TRUE,stringsAsFactors=T)
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

# ## Get the frequencies of ant species
# Antsp_t_levels <- levels(cactus$Antsp_t)
# # liom
# a <- subset(cactus, cactus$Antsp_t == "Liom" | cactus$Antsp_t == "LIOM" | cactus$Antsp_t == "liom" | cactus$Antsp_t == "L" | cactus$Antsp_t == "LIOM " |  cactus$Antsp_t == "liom ")
# nrow(a)
# # crem
# b <- subset(cactus, cactus$Antsp_t == "CREN" | cactus$Antsp_t == "LCREM" | cactus$Antsp_t == "crem" | cactus$Antsp_t == "Crem" | cactus$Antsp_t == "C" | cactus$Antsp_t == "CREM" | cactus$Antsp_t == "VCREM")
# nrow(b)
# # camp
# c <- subset(cactus, cactus$Antsp_t == "camp" | cactus$Antsp_t == "CAMP" | cactus$Antsp_t == "CAMP" | cactus$Antsp_t == "large black shiny" | cactus$Antsp_t == "LARGE BLACK SHINY" | cactus$Antsp_t == "drpoff" | cactus$Antsp_t == "dropoff")
# nrow(c)
# # honeypot
# d <- subset(cactus, cactus$Antsp_t ==  "HNEYPOT" | cactus$Antsp_t ==  "honeypot" | cactus$Antsp_t == "HONEYPOT")
# nrow(d)
# # phen
# e <- subset(cactus, cactus$Antsp_t == "phen" | cactus$Antsp_t == "PHEN" | cactus$Antsp_t == "aph" | cactus$Antsp_t == "unk (Aphaeno?)")
# nrow(e)
# # tetra
# h <- subset(cactus, cactus$Antsp_t == "tetra")
# nrow(h)
# # brachy
# j <- subset(cactus, cactus$Antsp_t == "brachy")
# nrow(j)

# # unknown
# k <- subset(cactus, cactus$Antsp_t == "unk" | cactus$Antsp_t == "lg unk" | cactus$Antsp_t == "UNK" | cactus$Antsp_t == "unk " | cactus$Antsp_t == "other" | cactus$Antsp_t == "SMALL RED-BROWN SPINDLY" | cactus$Antsp_t == "black shiny red thorax" | cactus$Antsp_t == "RED HEAD BLK BUTT" | cactus$Antsp_t == "shiny black red thorax" | cactus$Antsp_t == "shiny black, red thorax") 
# nrow(k)
# l <- subset(cactus,cactus$Antsp_t == "LFOR" | cactus$Antsp_t == "for" | cactus$Antsp_t == "FOR")
# nrow(l)

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
ant_t_levels <- rep("unknown",times=length(Antsp_t_levels))
# crem levels - elements
ant_t_levels[c(5,8,9,10,11,22,45)] <- "crem"
# liom levels - elements
ant_t_levels[c(19,25,26,27,28,29)] <- "liom"
# vacant - elements
ant_t_levels[c(1,42,43,44)] <- "vacant"
# Camp
ant_t_levels[c(6,7,20,21,12,13)] <- "camp"
# Honeypot
ant_t_levels[c(17,16,18)] <- "honeypot"
# Phenogaster
ant_t_levels[c(31,32,2,41)] <- "phen"
# Tetra
ant_t_levels[c(37)] <- "tetra"
# Brachy
ant_t_levels[c(4)] <- "brachy"
# forelius
ant_t_levels[c(23,15,14)] <- "for"

# Examine the levels
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
ant_t1_levels <- rep("unknown",times=length(Antsp_t1_levels))
# crem levels
ant_t1_levels[c(8,9,10,11,12,23,45)] <- "crem"
# liom levels
ant_t1_levels[c(26,27,28,29,30)] <- "liom"
# vacant
ant_t1_levels[c(1,43,44)] <- "vacant"
# Camp
ant_t1_levels[c(6,7,21,22,13,14)] <- "camp"
# Honepot
ant_t1_levels[c(18,19,20)] <- "honeypot"
# Phen
ant_t1_levels[c(32,33,3,42)] <- "phen"
# Tetra
ant_t1_levels[c(38)] <- "tetra"
# Brachy
ant_t1_levels[c(5)] <- "brachy"
# forelius
ant_t1_levels[c(15,16,17,2,24)] <- "for"

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
# Create a variable called Flower Yes No, that determines if there are flowers present
cactus$flower_YN<-as.integer(cactus$TotFlowerbuds_t > 0)
summary(cactus$flower_YN)
# View(cactus)
cactus$ant_t1 <- relevel(cactus$ant_t1,ref = "vacant")

## Cactus 2023 data cleaning ---- Recruitment
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
                      "ant_t1"  ,         "flower_YN" )     

# Remove extra columns
cactus <- cactus[ , c("Plot","TagID","Year_t","Goodbuds_t","TotFlowerbuds_t",
                      "ABFlowerbuds_t", "logsize_t","logsize_t1","ant_t","ant_t1",
                      "Antcount_t","Year_t1","Recruit","Survival_t1","Goodbuds_t1",
                      "TotFlowerbuds_t1","ABFlowerbuds_t1","Antcount_t1",
                      "flower_YN","Newplant","Damage","NP_adult","NP_juv","CV","WVL","MA")]

## break up the 2019-2021 transition year into two incomplete transition years
cactus %>% filter(Year_t==2019) %>% select(Year_t1)
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
cactus_final <- rbind(cactus[cactus$Year_t!=2019,],cactus_2019temp,cactus_2020temp)


# Export cactus to a csv
write.csv(cactus_final, "Data/cholla_demography_20042023_cleaned.csv")




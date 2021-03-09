options(mc.cores = parallel::detectCores())
setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Data Analysis")

# import the data
cactus <- read.csv("cholla_demography_20042019_cleaned.csv", header = TRUE,stringsAsFactors=T)
#### Create local data frames to call in the STAN models
cactus$ant_t1_relevel <- relevel(cactus$ant_t1,ref = "vacant")
data <- cactus[ ,c("Plot","Year_t","Survival_t1","ant_t","ant_t1","volume_t","volume_t1","repro_state_t1", "ant_t1_relevel")]
data <- na.omit(data)
data$ant <- as.integer(data$ant_t)
data$ant1 <- as.integer(data$ant_t1_relevel)
## add columns for each ant classification so they can be binary
data$crem <- 0 ##2
data$liom <- 0 ##3
data$other <- 0 ##1
data$vacant <- 0 ##4
for(i in 1:nrow(data)){
  if(data$ant[i] == 1){data$other[i] <- 1}
  if(data$ant[i] == 2){data$crem[i] <- 1}
  if(data$ant[i] == 3){data$liom[i] <- 1}
  if(data$ant[i] == 4){data$vacant[i] <- 1}
}
data$Year_t <- as.factor(data$Year_t)
data$year <- as.integer(data$Year_t)
data$Plot <- as.factor(data$Plot)
data$plot <- as.integer(data$Plot)
## Flower Data Set
flower <- cactus[ , c("TotFlowerbuds_t","ABFlowerbuds_t","Goodbuds_t","ant_t", "repro_state_t1","volume_t","volume_t1","Year_t","Plot")]
flower <- na.omit(flower)
flower$ant <- as.integer(flower$ant_t)
flower$Year_t <- as.factor(flower$Year_t)
flower$year <- as.integer(flower$Year_t)
flower$Plot <- as.factor(flower$Plot)
flower$plot <- as.integer(flower$Plot)


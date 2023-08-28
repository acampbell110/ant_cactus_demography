library(sgt)
library(cmdstanr)
library(bayesplot)


cactus <- read.csv("cholla_demography_20042023_cleaned.csv", header = TRUE,stringsAsFactors=T)
## re-assign the seedling plots ("HT1B1" etc) to transects 1-3
levels(cactus$Plot)<-c(levels(cactus$Plot),"T4")
cactus$Plot[cactus$Plot=="HT1B1"]<-"T1"
cactus$Plot[cactus$Plot=="HT2B3"]<-"T2"
cactus$Plot[cactus$Plot=="HT3B1" | cactus$Plot=="HT3B2" | cactus$Plot=="HT3B3"]<-"T3"
cactus$Plot[cactus$Plot=="HT4B1" | cactus$Plot=="HT4B2"]<-"T4"

##############################################################################################
##
##   Skew Growth Model -- What size will the cacti be next time step?
##
##############################################################################################
## Pull all necessary variables together, remove NAs, and put them into a list so they are
## ready to feed into the stan model
growth_data_orig <- cactus[,c("Plot","Year_t","logsize_t","logsize_t1","ant_t")]
growth_data <- na.omit(growth_data_orig)


## Lose 2032 rows (due to plant death & recruit status)
nrow(growth_data_orig)
nrow(growth_data)
# check that you are happy with the subsetting by plotting the original and cleaned data
plot(growth_data$logsize_t, growth_data$logsize_t1)
points((cactus$logsize_t), (cactus$logsize_t1), col = "red")
## Make a list of all necessary variables so they are properly formatted to feed into the stan model
stan_data_grow_skew <- list(N = nrow(growth_data),                                ## number of observations
                            vol = (growth_data$logsize_t), ## predictor volume year t
                            #vol2 = (growth_data$logsize_t)^2,
                            y = (growth_data$logsize_t1),                              ## response volume next year
                            ant = as.integer(as.factor(growth_data$ant_t)),            ## predictor ant state
                            K = 4,                                                     ## number of ant states
                            N_Year = max(as.integer(as.factor(growth_data$Year_t))),   ## number of years
                            N_Plot = max(as.integer(growth_data$Plot)),     ## number of plots
                            plot = as.integer(growth_data$Plot),            ## predictor plots
                            year = as.integer(as.factor(growth_data$Year_t))           ## predictor years
)


fp <- file.path("./Data Analysis/STAN Models/grow_sgt.stan")
mod <- cmdstan_model(fp, include_paths = "/Users/user/Downloads/helpful_stan_functions-main/functions/distribution")

## I think this is a dead end because the sgt stan function only takes y as a vector and mu, sigma, lambda, p, q as reals. 
## Unless I can figure out how to vectorize the parameters, I cannot use this

sgt_fit <- mod$sample(
  data = stan_data_grow_skew,
  chains = 2,
  parallel_chains = 2
)

setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Data Analysis")

### Fruit Data
fruit.dat<-read.csv("JO_fruit_data_final_dropplant0.csv",T)  %>% drop_na() %>% 
  ## taking the subset of pollinator+ant access, not vacant
  ## this is the subset used in Elderd&Miller
  filter(poll.access=="y" & treatment!="tc" & vacant=="n" & ant.access=="y")
### Fruit Surv
fruit.surv<-read.csv("FruitSurvival.csv",header = TRUE,stringsAsFactors=T) %>% drop_na()
fruit.surv <- fruit.surv[which(fruit.surv$Fr.on.grnd.not.chewed > 0),]
fruitsurv<-glm((Fr.on.grnd.not.chewed/Fr.on.plant~1),weights=Fr.on.plant,family="binomial",data=fruit.surv)
## this model fits the ca.-6mo seed survival rate. Squaring this estimates gives the ca. 1-year rate.
## But this may over-estimate mortality. Maybe most of it does happen in that 6-mo window...experiment with this
### Germ Data
germ.dat<-read.csv("Germination.csv") %>% drop_na()
### Pre-Census Surv
precensus.dat<-read.csv("PrecensusSurvival.csv") 
precensus.dat%>% select(survive0405) %>% drop_na()
seedlings <- cholla %>% 
  mutate(vol_t = log(volume(h = Height_t, w = Width_t, p = Perp_t)),
         standvol_t = (vol_t - mean(vol_t,na.rm=T))/sd(vol_t,na.rm=T)) %>% 
  filter(str_sub(Plot,1,1)=="H",
         Recruit==1)

################ MODELS #####################
stan_data_seed <- list(N_obs = nrow(fruit.dat), # Number of observations
                       seed = fruit.dat$seed_count, # Number of seeds
                       ant = as.numeric(as.factor(fruit.dat$ant_state)), # Ant state
                       fruit = fruit.dat$fruit_number, # Fruit number
                       N_ant = 3,
                       N_Plant_ID = length(unique(fruit.dat$plant)),
                       plant = as.numeric(factor(fruit.dat$plant))
)  
fit_seed_ant <- stan(file = "STAN Models/seed_prod.stan", data = stan_data_seed, warmup = 500, iter = 1000, chains = 3, cores = 2, thin = 1)
seed_outputs <- rstan::extract(fit_seed_ant, pars = c("beta0","y_rep"))
write.csv(seed_outputs, "seed_outputs.csv")



setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Figures")
seed_data <- read.csv("/Users/alicampbell/Cactus Dropbox/Ant-Demography-Project/Model Outputs/seed_outputs.csv", header = TRUE,stringsAsFactors=T)

## Formulas
y_seed = quantile(seed_data$beta0,0.5)
y_low_seed = quantile(seed_data$beta0,0.05) 
y_high_seed = quantile(seed_data$beta0,0.95) 
png("seed_panels1.png")
plot((seed_prod$seed_count) ~ factor(seed_prod$ant_state))
dev.off()
range(seed_prod$seed_count[seed_prod$ant_state == "Vacant"])
range(seed_prod$seed_count[seed_prod$ant_state == "Crem"])
range(seed_prod$seed_count[seed_prod$ant_state == "Liom"])

barplot(table(seed_prod$seed_count, factor(seed_prod$ant_state)))
barplot(seed_prod$seed_count, (seed_prod$ant_state))




setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/Data Analysis")
## Fruit Data Set Linear Models

## Proportion of seeds aborted based on the ant state
#view raw proportions ############################################################
fruit1 %>% 
  mutate(viab_rate = number.unblackened/(number.unblackened+number.blackened)) %>% 
  group_by(ant_state) %>% 
  summarise(mean_viab = mean(viab_rate,na.rm=T),
            n_viab = n()) 
fruit1$viab_rate <- fruit1$number.unblackened/(fruit1$number.unblackened+fruit1$number.blackened)

abort_mod1 <- glm(cbind(number.unblackened,number.blackened) ~ 1,family="binomial",data=fruit1)
abort_mod2 <- glm(cbind(number.unblackened,number.blackened) ~ ant_state,family="binomial",data=fruit1) 
abort_mod3 <- glm(cbind(number.unblackened,number.blackened) ~ ant_state + fruit_number,family="binomial",data=fruit1)
abort_mod4 <- glm(cbind(number.unblackened,number.blackened) ~ ant_state * fruit_number,family="binomial",data=fruit1) ## Chosen model
AICtab(abort_mod1,abort_mod2,abort_mod3,abort_mod4)

summary(abort_mod4)

ggplot(data = fruit1) +
  aes(x = fruit_number, y = number.unblackened/(number.unblackened+number.blackened), color = ant_state) + 
  geom_smooth()

## Production of seeds  ############################################################
seeds_mod1 <- glm(seed_count ~ 1, family = "poisson", data = fruit1)
seeds_mod2 <- glm(seed_count ~ ant_state, family = "poisson", data = fruit1)
AICtab(seeds_mod1, seeds_mod2,seeds_mod3,seeds_mod4)

summary(seeds_mod4)

ggplot(data = fruit1) +
  aes(x = fruit_number, y = seed_count, color = ant_state) + 
  geom_smooth()
## IPM Section
seed_prod <- fruit2[,c("seed_count","ant_state","plant")]
seed_prod <- na.omit(seed_prod)

stan_data_seed <- list(N_obs = nrow(seed_prod), # Number of observations
                      seed = seed_prod$seed_count, # Number of seeds
                      ant = as.numeric(as.factor(seed_prod$ant_state)), # Ant state
                      fruit = seed_prod$fruit_number, # Fruit number
                      N_ant = 3,
                      N_Plant_ID = length(unique(seed_prod$plant)),
                      plant = as.numeric(factor(seed_prod$plant))
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

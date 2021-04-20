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
seeds_mod3 <- glm(seed_count ~ ant_state + fruit_number, family = "poisson", data = fruit1)
seeds_mod4 <- glm(seed_count ~ ant_state * fruit_number, family = "poisson", data = fruit1)## Chosen model
AICtab(seeds_mod1, seeds_mod2,seeds_mod3,seeds_mod4)

summary(seeds_mod4)

ggplot(data = fruit1) +
  aes(x = fruit_number, y = seed_count, color = ant_state) + 
  geom_smooth()
## IPM Section
seed_prod <- fruit1[,c("seed_count","ant_state","fruit_number")]
seed_prod <- na.omit(seed_prod)

stan_data_tot <- list(N_obs = nrow(seed_prod), # Number of observations
                      seed = seed_prod$seed_count, # Number of seeds
                      ant = seed_prod$ant_state, # Ant state
                      fruit = seed_prod$fruit_number # Fruit number
                      )  

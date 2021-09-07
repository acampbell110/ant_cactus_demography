size_dummy = seq(min(na.omit(cactus$logsize_t)), max(na.omit(cactus$logsize_t)), by = 0.1)

############# Growth ################################################################
plot(cactus$logsize_t,cactus$logsize_t1,type="n", main = "Growth Rates by Ant Species",xlab = "Size at Year t", ylab = "Size at Year t1",family = "Helvetica",  cex.main = 1.5,cex.lab = 1.2, cex.axis = 1)
points(cactus$logsize_t[cactus$ant_t=="vacant"],cactus$logsize_t1[cactus$ant_t=="vacant"],col=alpha("pink",0.1))
abline(int_grow+vac_grow,vol_grow+vac_int_grow,col="pink",lwd = 2)
points(cactus$logsize_t[cactus$ant_t=="liom"],cactus$logsize_t1[cactus$ant_t=="liom"],col=alpha("blue",0))
abline(int_grow+liom_grow,vol_grow+liom_int_grow,col="blue",lwd = 2)
points(cactus$logsize_t[cactus$ant_t=="other"],cactus$logsize_t1[cactus$ant_t=="other"],col=alpha("black",0))
abline(int_grow,vol_grow,col="black",lwd = 2)
points(cactus$logsize_t[cactus$ant_t=="crem"],cactus$logsize_t1[cactus$ant_t=="crem"],col=alpha("red",0))
abline(int_grow+crem_grow,vol_grow+crem_int_grow,col="red", lwd = 1.5)
abline(0,1, lty = 2, col = "grey")
legend(9,4.5,legend = c("other","crem","liom","vacant","Growth Line"), col = c("pink","red","blue","black","grey"),lty = c(1,1,1,1,2), lwd = 2)
box()

plot(cactus$logsize_t, cactus$logsize_t1)
abline(int_grow + vac_grow, vol)

############# Survival
x_vol <- seq(min(cactus$logsize_t,na.rm=T),max(cactus$logsize_t,na.rm=T),0.1)
x_crem <- sample("crem", 198, replace = TRUE)
x_liom <- sample("liom", 198, replace = TRUE)
x_other <- sample("other", 198, replace = TRUE)
x_vac <- sample("vacant", 198, replace = TRUE)
y_crem <- predict(surv_3, list(logsize_t = x_vol, ant_t = x_crem),type="response", data = cactus)
y_liom  <- predict(surv_3, list(logsize_t = x_vol, ant_t = x_liom),type="response", data = cactus)
y_other <- predict(surv_3, list(logsize_t = x_vol, ant_t = x_other),type="response", data = cactus)
y_vac <- predict(surv_3, list(logsize_t = x_vol, ant_t = x_vac),type="response", data = cactus)

plot(x = cactus$logsize_t, y = cactus$Survival_t1, ylim = c(0.5,0.8))
lines(x_vol, invlogit(y_crem), col = "red")
lines(x_vol, invlogit(y_liom), col = "blue")
lines(x_vol, invlogit(y_other), col = "pink")
lines(x_vol, invlogit(y_vac), col = "black")

############ Viability
cactus$viab_rate = cactus$Goodbuds_t/(cactus$Goodbuds_t+cactus$ABFlowerbuds_t)
means = c(invlogit(int_viab+crem_viab), invlogit(int_viab+liom_viab), invlogit(int_viab+vac_viab), invlogit(int_viab))
barplot(means, col = c("red","blue","pink","black"), names.arg = c("crem","liom","vacant","other"), ylim = c(0,1))
arrows(x0 = 0.75, x1 = 0.75, y0 = means[1] - (sd_crem_viab + sd_viab), y1 = means[1] + (sd_crem_viab + sd_viab), angle = 90)
arrows(x0 = 1.9, x1 = 1.9, y0 = means[2] - (sd_liom_viab + sd_viab), y1 = 1, angle = 90)
arrows(x0 = 3, x1 = 3, y0 = means[3] - (sd_vac_viab + sd_viab), y1 = 1, angle = 90)
arrows(x0 = 4.2, x1 = 4.2, y0 = means[4] - sd_viab, y1 = means[4] + sd_viab, angle = 90)

############# Total Flowerbuds
x_vol <- seq(min(cactus$logsize_t,na.rm=T),max(cactus$logsize_t,na.rm=T),0.1)
y_flow1 <- exp(int_flower + vol_flower*x_vol)

plot(cactus$logsize_t, cactus$TotFlowerbuds_t, alpha = 0.5)
lines(x_vol, y_flow1, col = "green")

############# Reproductive State
x_vol <- seq(min(cactus$logsize_t,na.rm=T),max(cactus$logsize_t,na.rm=T),0.1)
y_repro <- invlogit(int_repro + vol_repro*x_vol)

plot(cactus$logsize_t, as.numeric(cactus$flower1_YN))
lines(x_vol, y_repro, col = "green")

############# Seeds/flower
int_seed <- coef(seed_1)[1]
ant_seed <- coef(seed_1)[2]
means <- c(exp(int_seed), exp(int_seed + ant_seed))
low = exp(int_seed) - sd_seed
high = exp(int_seed) + sd_seed

barplot(means, col = c("grey","white"), names.arg = c("vacant","occupied"), ylim = c(0,150))
arrows(x0 = 1, x1 = 1, y0 = low, y1 = high, angle = 90)

############# Seed Survival
plot(invlogit(int_fruit), ylim = c(0,1))
arrows(x0 = 1, x1 = 1, y0 = 0, y1 = invlogit(int_fruit) + sd_fruit, angle = 90)

############# Germ yr 1 & Germ yr 2
plot(c(invlogit(int_germ1),invlogit(int_germ2)), ylim = c(0,1), xlim = c(0,3))
arrows(x0 = 1, x1 = 1, y0 = invlogit(int_germ1) - sd_germ1, y1 = invlogit(int_germ1) + sd_germ1, angle = 90)
arrows(x0 = 2, x1 = 2, y0 = invlogit(int_germ2) - sd_germ2, y1 = invlogit(int_germ2) + sd_germ2, angle = 90)

############# Precen Survival
plot(invlogit(int_prec), ylim = c(-0.05,1))
arrows(x0 = 1, x1 = 1, y0 = 0, y1 = invlogit(int_prec) + sd_prec, angle = 90)

############# Recruit Sizes
plot(int_rec)
arrows(x0 = 1, x1 = 1, y0 = (int_rec) - sd_rec, y1 = (int_rec) + sd_rec, angle = 90)


############## Transition
model4.all <- allEffects(ant_transition_models[[4]], typical = median)
plot(model4.all, lines = list(multiline = T), colors = c("pink","black","red","blue"), main = "Ant State Probability in Year t1", ylab = "Probability of Ant State", rug = FALSE, family = "Helvetica")
plot(model4.all, style = "stacked",colors = c("pink","black","red","blue"), family = "Helvetica")

############## Transition 2 spec mod
x_vol <- seq(min(ants$logsize_t,na.rm=T),max(ants$logsize_t,na.rm=T),0.1)
y_vac <- invlogit(int_ant + vol_ant*x_vol)
y_occ <- invlogit(int_ant + int_ant_occ + vol_ant*x_vol)
plot(ants$logsize_t, as.numeric(ants$occ_YN_t1))
lines(x_vol, y_vac, col = "green")
lines(x_vol, y_occ, col = "red")

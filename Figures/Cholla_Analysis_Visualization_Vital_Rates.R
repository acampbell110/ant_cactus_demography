
#### Growth Visuals
## Overlay Plots
plot_title <- ggtitle("Mixed Ant Effects Growth Model Simulated Data")
bayesplot::ppc_dens_overlay(y = extract_grow$mean_y_rep,
                            yrep = predict(extract_grow$y_rep, draws = 5)) + plot_title
predict(fit_grow_mix_ant@stanmodel,draws = 5)
## Convergence Plots
bayesplot::mcmc_trace(As.mcmc.list(fit_grow_mix_ant, pars=c("beta0", "beta1")))
## Panels
par(mfrow = c(2,3))
##classic plot
# Other panel
plot(log(cactus$volume_t),log(cactus$volume_t1),type="n",main = "Other",family = "Helvetica", face = "bold", cex.main = 1.5,cex.lab = 1.2, cex.axis = 1)
points(log(cactus$volume_t[cactus$ant_t=="other"]),log(cactus$volume_t1[cactus$ant_t=="other"]),col=alpha("black",0))
abline(posterior_grow_mix_ant$`beta0[1]`,posterior_grow_mix_ant$`beta1[1]`,col="black",lwd = 2)
abline(0,1, lty = 2, col = "grey")
# Crem panel
plot(log(cactus$volume_t),log(cactus$volume_t1),type="n", main = "Crem",family = "Helvetica", face = "bold", cex.main = 1.5,cex.lab = 1.2, cex.axis = 1)
points(log(cactus$volume_t[cactus$ant_t=="crem"]),log(cactus$volume_t1[cactus$ant_t=="crem"]),col=alpha("red",0))
abline(posterior_grow_mix_ant$`beta0[2]`,posterior_grow_mix_ant$`beta1[2]`,col="red", lwd = 1.5)
abline(0,1, lty = 2, col = "grey")
#Liom panel
plot(log(cactus$volume_t),log(cactus$volume_t1),type="n", main = "Liom",family = "Helvetica", face = "bold", cex.main = 1.5,cex.lab = 1.2, cex.axis = 1)
points(log(cactus$volume_t[cactus$ant_t=="liom"]),log(cactus$volume_t1[cactus$ant_t=="liom"]),col=alpha("blue",0))
abline(posterior_grow_mix_ant$`beta0[3]`,posterior_grow_mix_ant$`beta1[3]`,col="blue",lwd = 2)
abline(0,1, lty = 2, col = "grey")
#vacant panel
plot(log(cactus$volume_t),log(cactus$volume_t1),type="n", main = "Vacant",family = "Helvetica", face = "bold", cex.main = 1.5,cex.lab = 1.2, cex.axis = 1)
points(log(cactus$volume_t[cactus$ant_t=="vacant"]),log(cactus$volume_t1[cactus$ant_t=="vacant"]),col=alpha("pink",0))
abline(posterior_grow_mix_ant$`beta0[4]`,posterior_grow_mix_ant$`beta1[4]`,col="pink",lwd = 2)
abline(0,1, lty = 2, col = "grey")
#all panel
plot(log(cactus$volume_t),log(cactus$volume_t1),type="n", main = "All",family = "Helvetica", face = "bold", cex.main = 1.5,cex.lab = 1.2, cex.axis = 1)
points(log(cactus$volume_t[cactus$ant_t=="other"]),log(cactus$volume_t1[cactus$ant_t=="other"]),col=alpha("black",0))
abline(posterior_grow_mix_ant$`beta0[1]`,posterior_grow_mix_ant$`beta1[1]`,col="black",lwd = 2)
points(log(cactus$volume_t[cactus$ant_t=="vacant"]),log(cactus$volume_t1[cactus$ant_t=="vacant"]),col=alpha("pink",0))
abline(posterior_grow_mix_ant$`beta0[4]`,posterior_grow_mix_ant$`beta1[4]`,col="pink",lwd = 2)
points(log(cactus$volume_t[cactus$ant_t=="liom"]),log(cactus$volume_t1[cactus$ant_t=="liom"]),col=alpha("blue",0))
abline(posterior_grow_mix_ant$`beta0[3]`,posterior_grow_mix_ant$`beta1[3]`,col="blue",lwd = 2)
points(log(cactus$volume_t[cactus$ant_t=="crem"]),log(cactus$volume_t1[cactus$ant_t=="crem"]),col=alpha("red",0))
abline(posterior_grow_mix_ant$`beta0[2]`,posterior_grow_mix_ant$`beta1[2]`,col="red", lwd = 1.5)
abline(0,1, lty = 2, col = "grey")
#legend("bottomright",legend = c("other","crem","liom","vacant","Growth Line"), col = c("pink","red","blue","black","grey"),lty = c(1,1,1,1,2), lwd = 2)
box()


#### Survival Visuals
## Overlay Plots
plot_title <- ggtitle("Mixed Ant Effects Survival Model Simulated Data")
bayesplot::ppc_dens_overlay(y = fit_surv_mix_ant@mean_y_rep,
                            yrep = posterior_predict(fit_surv_mix_ant@y_rep, draws = 5)) + plot_title
## Convergence Plots
bayesplot::mcmc_trace(As.mcmc.list(fit_surv_mix_ant, pars=c("beta0", "beta1")))
##Pamels
##Facetwrap binomials
quietgg(stan_trace(fit_surv_mix_ant))
plot(fit_surv_mix_ant, show_density = TRUE, ci_level = 0.5, fill_color = "purple")


#### Flowering Visuals
## Overlay Plots
plot_title <- ggtitle("Mixed Ant Effects Flowering Model Simulated Data")
bayesplot::ppc_dens_overlay(y = fit_flow_mix_ant@mean_y_rep,
                            yrep = posterior_predict(fit_flow_mix_ant@y_rep, draws = 5)) + plot_title
## Convergence Plots
bayesplot::mcmc_trace(As.mcmc.list(fit_flow_mix_ant, pars=c("beta0", "beta1")))

#### Reproductive Visuals
## Overlay Plots
plot_title <- ggtitle("Mixed Ant Effects Reproductive Model Simulated Data")
bayesplot::ppc_dens_overlay(y = fit_repro_mix_ant@mean_y_rep,
                            yrep = posterior_predict(fit_repro_mix_ant@y_rep, draws = 5)) + plot_title
## Convergence Plots
bayesplot::mcmc_trace(As.mcmc.list(fit_repro_mix_ant, pars=c("beta0", "beta1")))

#### Viability Visuals
## Overlay Plots
plot_title <- ggtitle("Mixed Ant Effects Viability Model Simulated Data")
bayesplot::ppc_dens_overlay(y = fit_viab_mix_ant@mean_y_rep,
                            yrep = posterior_predict(fit_viab_mix_ant@y_rep, draws = 5)) + plot_title
## Convergence Plots
bayesplot::mcmc_trace(As.mcmc.list(fit_viab_mix_ant, pars=c("beta0", "beta1")))

#### Multinomial 1
## Overlay Plots
plot_title <- ggtitle("Mixed Ant Effects Multinomial1 Model Simulated Data")
bayesplot::ppc_dens_overlay(y = fit_multi1_mix_ant@mean_y_rep,
                            yrep = posterior_predict(fit_multi1_mix_ant@y_rep, draws = 5)) + plot_title
## Convergence Plots
bayesplot::mcmc_trace(As.mcmc.list(fit_multi1_mix_ant, pars=c("beta0", "beta1")))

#### Multinomial 2
## Overlay Plots
plot_title <- ggtitle("Mixed Ant Effects Multinomial2 Model Simulated Data")
bayesplot::ppc_dens_overlay(y = fit_multi2_mix_ant@mean_y_rep,
                            yrep = posterior_predict(fit_multi2_mix_ant@y_rep, draws = 5)) + plot_title
## Convergence Plots
bayesplot::mcmc_trace(As.mcmc.list(fit_multi2_mix_ant, pars=c("beta0", "beta1")))

#### Multinomial 3
## Overlay Plots
plot_title <- ggtitle("Mixed Ant Effects Multinomial3 Model Simulated Data")
bayesplot::ppc_dens_overlay(y = fit_multi3_mix_ant@mean_y_rep,
                            yrep = posterior_predict(fit_multi3_mix_ant@y_rep, draws = 5)) + plot_title
## Convergence Plots
bayesplot::mcmc_trace(As.mcmc.list(fit_multi3_mix_ant, pars=c("beta0", "beta1")))





#### Growth Visuals
## Overlay Plots
plot_title <- ggtitle("Mixed Ant Effects Growth Model Simulated Data")
bayesplot::ppc_dens_overlay(y = fit_grow_mix_ant@mean_y_rep,
                            yrep = posterior_predict(fit_grow_mix_ant@y_rep, draws = 5)) + plot_title
predict(fit_grow_mix_ant@stanmodel,draws = 5)
## Convergence Plots
bayesplot::mcmc_trace(As.mcmc.list(fit_grow_mix_ant, pars=c("beta0", "beta1")))

#### Survival Visuals
## Overlay Plots
plot_title <- ggtitle("Mixed Ant Effects Survival Model Simulated Data")
bayesplot::ppc_dens_overlay(y = fit_surv_mix_ant@mean_y_rep,
                            yrep = posterior_predict(fit_surv_mix_ant@y_rep, draws = 5)) + plot_title
## Convergence Plots
bayesplot::mcmc_trace(As.mcmc.list(fit_surv_mix_ant, pars=c("beta0", "beta1")))

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




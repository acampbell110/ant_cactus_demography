Ndraws = 500
post.params <- read.csv("/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/params_outputs.csv", header = TRUE,stringsAsFactors=T)

##----------------------Growth Parameters----------------## Ant 1 (crem)
cholla<-data.frame(grow_int = rep(NA,15))

cholla$grow_int<-post.params$beta0_g.1      	  ## growth intercept
cholla$grow_slo<-post.params$beta1_g.1				## growth slope
cholla$grow_plotfx<-post.params$u_g.1				## growth plotfx
cholla$grow_yrfx<-post.params$w_g.1					  ## growth yrfx
cholla$grow_sig<-post.params$sigma_g			## growth error
cholla$grow_sig_plot<-post.params$sigma_u_g  	## growth plotfx error
cholla$grow_sig_yr<-post.params$sigma_w_g    ##growth yrfx error

##-----------------------Survival Parameters-----------------## Ant 1 (crem)
cholla$surv_int<-post.params$beta0_s.1      	  ## growth intercept
cholla$surv_slo<-post.params$beta1_s.1				## growth slope
cholla$surv_plotfx<-post.params$u_s.1				## growth plotfx
cholla$surv_yrfx<-post.params$w_s.1					  ## growth yrfx
cholla$surv_sig<-post.params$sigma_s			## growth error
cholla$surv_sig_plot<-post.params$sigma_u_s  	## growth plotfx error
cholla$surv_sig_yr<-post.params$sigma_w_s    ##growth yrfx error

##-----------------------Flowering/Fecundity Parameters-----------------## Ant 1 (crem)
cholla$flow_int<-post.params$beta0_f      	  ## growth intercept
cholla$flow_slo<-post.params$beta1_f				## growth slope
cholla$flow_plotfx<-post.params$u_f				## growth plotfx
cholla$flow_yrfx<-post.params$w_f					  ## growth yrfx
cholla$flow_sig<-post.params$sigma_f			## growth error
cholla$flow_sig_plot<-post.params$sigma_u_f  	## growth plotfx error
cholla$flow_sig_yr<-post.params$sigma_w_f    ##growth yrfx error
cholla$flow_phi<-post.params$phi_f

##-----------------------Reproductive State Parameters-----------------## Ant 1 (crem)
cholla$repro_int<-post.params$beta0_r.1      	  ## growth intercept
cholla$repro_slo<-post.params$beta1_r.1				## growth slope
cholla$repro_plotfx<-post.params$u_r.1				## growth plotfx
cholla$repro_yrfx<-post.params$w_r.1					  ## growth yrfx
cholla$repro_sig<-post.params$sigma_r			## growth error
cholla$repro_sig_plot<-post.params$sigma_u_r  	## growth plotfx error
cholla$repro_sig_yr<-post.params$sigma_w_r    ##growth yrfx error

##-----------------------Viability Parameters-----------------## Ant 1 (crem)
cholla$viab_slo<-post.params$beta0_v.1      	  ## growth 
cholla$viab_plotfx<-post.params$u_v.1				## growth plotfx
cholla$viab_yrfx<-post.params$w_v.1					  ## growth yrfx
cholla$viab_sig<-post.params$sigma_v			## growth error
cholla$viab_sig_plot<-post.params$sigma_u_v  	## growth plotfx error
cholla$viab_sig_yr<-post.params$sigma_w_v    ##growth yrfx error

##-----------------------Seeds Parameters-----------------## Ant 1 (crem)
cholla$seed_int<-post.params$beta0_seed.1      	  ## growth intercept
cholla$seed_plantfx<-post.params$u_seed.1				## growth plotfx
cholla$seed_sig<-post.params$sigma_seed			## growth error
cholla$seed_sig_plot<-post.params$sigma_u_seed  	## growth plotfx error
cholla$seed_phi<-post.params$phi_seed



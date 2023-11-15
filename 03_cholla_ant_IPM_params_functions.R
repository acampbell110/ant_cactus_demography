################################################################################
################################################################################
## The purpose of this script is to load all model fits, load the IPM functions,
## and load any values which need to be loaded to run the IPM
################################################################################
################################################################################
## Source the IPM vital rates code 
source("~/Documents/GitHub/ant_cactus_demography/02_cholla_ant_IPM_vital_rates.R")
## Set conditions for the IPM 
cholla_min<- min((cactus$logsize_t), na.rm = TRUE)  ## minsize 
cholla_max<- max((cactus$logsize_t), na.rm = TRUE)  ## maxsize 
Nplots <- length(unique(cactus$Plot))
Nyears <- length(unique(cactus$Year_t))
iter <- 1000
matsize<-400
lower<- cholla_min
upper<- cholla_max
ceiling <- 20
floor <- 10
set.seed(333) # picked random number
N_draws <- 1000
draws <- sample(7500,N_draws, replace=F)
years <- unique(cactus$Year_t)
## -------- read in MCMC output ---------------------- ##
## Choose your pathway to pull from 
#Ali
mcmc_dir <- "/Users/alicampbell/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/"
#Tom
mcmc_dir <- "C:/Users/tm9/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/"
#Lab
#mcmc_dir <- "/Users/Labuser/Dropbox/Ali and Tom -- cactus-ant mutualism project/Model Outputs/"
## These files contain all draws from the posterior distributions of all parameters
# growth model
fit_grow_stud<-readRDS(paste0(mcmc_dir,"fit_grow_student_t.rds"))
grow.params <- rstan::extract(fit_grow_stud)
# survival model
fit_surv<-readRDS(paste0(mcmc_dir,"fit_surv.rds"))
surv.params <- rstan::extract(fit_surv)
# flowers produced model
fit_flow<-readRDS(paste0(mcmc_dir,"fit_flow.rds"))
flow.params <- rstan::extract(fit_flow)
# viability of flowers model
fit_viab<-readRDS(paste0(mcmc_dir,"fit_viab.rds"))
viab.params <- rstan::extract(fit_viab)
# reproducing model
fit_repro<-readRDS(paste0(mcmc_dir,"fit_repro.rds"))
repro.params <- rstan::extract(fit_repro)
# seeds per flower model
fit_seed<-readRDS(paste0(mcmc_dir,"fit_seed.rds"))
seed.params <- rstan::extract(fit_seed)
# pre census seed survival model
fit_seed_surv<-readRDS(paste0(mcmc_dir,"fit_seed_surv.rds"))
pre.seed.params <- rstan::extract(fit_seed_surv)
# germination year 1 model
fit_germ1<-readRDS(paste0(mcmc_dir,"fit_germ1.rds"))
germ1.params <- rstan::extract(fit_germ1)
# germination year 2 model
fit_germ2<-readRDS(paste0(mcmc_dir,"fit_germ2.rds"))
germ2.params <- rstan::extract(fit_germ2)
# recruit size distribution model
fit_rec<-readRDS(paste0(mcmc_dir,"fit_rec.rds"))
rec.params <- rstan::extract(fit_rec)
# ant transitions model
fit_multi<-readRDS(paste0(mcmc_dir,"fit_multi.rds"))
multi.params <- rstan::extract(fit_multi)
## 'params' is a matrix where rows are vital rate coefficients and columns are posterior draws
# below, we will loop over columns, sending each set of coefficients into the IPM
params <- data.frame(matrix(NA,nrow=N_draws,ncol=1))
params<-params[,-1]
##----------------------Growth Parameters----------------## 
# No specific ant
params$grow_sig0 <- grow.params$d_0[draws]           ## growth error intercept
params$grow_sig1 <- grow.params$d_size[draws]        ## ## growth error size
params$grow_alp0 <- grow.params$a_0[draws]
params$grow_alp1 <- grow.params$a_size[draws]
params$grow_sig_u<-grow.params$sigma_u[draws]
params$grow_sig_w<-grow.params$sigma_w[draws]
# Ant 4 (vacant)
params$grow_beta04<-grow.params$beta0[draws,4]     	  ## growth intercept
params$grow_beta14<-grow.params$beta1[draws,4]				## growth slope
params$grow_beta24<-grow.params$beta2[draws,4]				## growth slope
# Ant 3 (other)
params$grow_beta03<-grow.params$beta0[draws,3]     	  ## growth intercept
params$grow_beta13<-grow.params$beta1[draws,3]				## growth slope
params$grow_beta23<-grow.params$beta2[draws,3]				## growth slope
# Ant 1 (crem)
params$grow_beta01<-grow.params$beta0[draws,1]     	  ## growth intercept
params$grow_beta11<-grow.params$beta1[draws,1]				## growth slope
params$grow_beta21<-grow.params$beta2[draws,1]				## growth slope
# Ant 2 (liom)
params$grow_beta02<-grow.params$beta0[draws,2]      	## growth intercept
params$grow_beta12<-grow.params$beta1[draws,2]				## growth slope
params$grow_beta22<-grow.params$beta2[draws,2]				## growth slope
## Year Random Effects
# Create a dataframe for random effects where the columns are the year name and the rows are the iterations
# Ant 1 (prev crem)
grow_rfx1 <- data.frame(matrix(0, nrow = N_draws, ncol = 18))
colnames(grow_rfx1) <- years
grow_rfx1$'2004' <- grow.params$w[draws,1,1];grow_rfx1$'2005' <- grow.params$w[draws,1,2];grow_rfx1$'2006' <- grow.params$w[draws,1,3];grow_rfx1$'2009' <- grow.params$w[draws,1,4];grow_rfx1$'2010' <- grow.params$w[draws,1,5];grow_rfx1$'20011' <- grow.params$w[draws,1,6];
grow_rfx1$'2012' <- grow.params$w[draws,1,7];grow_rfx1$'2013' <- grow.params$w[draws,1,8];grow_rfx1$'2014' <- grow.params$w[draws,1,9];grow_rfx1$'2015' <- grow.params$w[draws,1,10];grow_rfx1$'2016' <- grow.params$w[draws,1,11];grow_rfx1$'2017' <- grow.params$w[draws,1,12];grow_rfx1$'2018' <- grow.params$w[draws,1,13];grow_rfx1$'2019' <- grow.params$w[draws,1,14];grow_rfx1$'2021' <- grow.params$w[draws,1,15]
# Ant 2 (prev liom)
grow_rfx2 <- data.frame(matrix(0, nrow = N_draws, ncol = 19))
colnames(grow_rfx2) <- years
grow_rfx2$'2004' <- grow.params$w[draws,2,1];grow_rfx2$'2005' <- grow.params$w[draws,2,2];grow_rfx2$'2006' <- grow.params$w[draws,2,3];grow_rfx2$'2009' <- grow.params$w[draws,2,4];grow_rfx2$'2010' <- grow.params$w[draws,2,5];grow_rfx2$'20011' <- grow.params$w[draws,2,6];
grow_rfx2$'2012' <- grow.params$w[draws,2,7];grow_rfx2$'2013' <- grow.params$w[draws,2,8];grow_rfx2$'2014' <- grow.params$w[draws,2,9];grow_rfx2$'2015' <- grow.params$w[draws,2,10];grow_rfx2$'2016' <- grow.params$w[draws,2,11];grow_rfx2$'2017' <- grow.params$w[draws,2,12];grow_rfx2$'2018' <- grow.params$w[draws,2,13];grow_rfx2$'2019' <- grow.params$w[draws,2,14];grow_rfx2$'2021' <- grow.params$w[draws,2,15]
# Ant 3 (prev other)
grow_rfx3 <- data.frame(matrix(0, nrow = N_draws, ncol = 19))
colnames(grow_rfx3) <- years
grow_rfx3$'2004' <- grow.params$w[draws,3,1];grow_rfx3$'2005' <- grow.params$w[draws,3,2];grow_rfx3$'2006' <- grow.params$w[draws,3,3];grow_rfx3$'2009' <- grow.params$w[draws,3,4];grow_rfx3$'2010' <- grow.params$w[draws,3,5];grow_rfx3$'20011' <- grow.params$w[draws,3,6];
grow_rfx3$'2012' <- grow.params$w[draws,3,7];grow_rfx3$'2013' <- grow.params$w[draws,3,8];grow_rfx3$'2014' <- grow.params$w[draws,3,9];grow_rfx3$'2015' <- grow.params$w[draws,3,10];grow_rfx3$'2016' <- grow.params$w[draws,3,11];grow_rfx3$'2017' <- grow.params$w[draws,3,12];grow_rfx3$'2018' <- grow.params$w[draws,3,13];grow_rfx3$'2019' <- grow.params$w[draws,3,14];grow_rfx3$'2021' <- grow.params$w[draws,3,15]
# Ant 4 (prev vac)
grow_rfx4 <- data.frame(matrix(0, nrow = N_draws, ncol = 19))
colnames(grow_rfx4) <- years
grow_rfx4$'2004' <- grow.params$w[draws,4,1];grow_rfx4$'2005' <- grow.params$w[draws,4,2];grow_rfx4$'2006' <- grow.params$w[draws,4,3];grow_rfx4$'2009' <- grow.params$w[draws,4,4];grow_rfx4$'2010' <- grow.params$w[draws,4,5];grow_rfx4$'20011' <- grow.params$w[draws,4,6];
grow_rfx4$'2012' <- grow.params$w[draws,4,7];grow_rfx4$'2013' <- grow.params$w[draws,4,8];grow_rfx4$'2014' <- grow.params$w[draws,4,9];grow_rfx4$'2015' <- grow.params$w[draws,4,10];grow_rfx4$'2016' <- grow.params$w[draws,4,11];grow_rfx4$'2017' <- grow.params$w[draws,4,12];grow_rfx4$'2018' <- grow.params$w[draws,4,13];grow_rfx4$'2019' <- grow.params$w[draws,4,14];grow_rfx4$'2021' <- grow.params$w[draws,4,15]

##-----------------------Survival Parameters-----------------## 
## Check the names of the parameters
#head(surv.params)
#params$surv_sig_u<-surv.params$sigma_u[draws]        ## surv sigma u
#params$surv_sig_w<-surv.params$sigma_w[draws]        ## surv sigma w
# Ant 4 (vacant)
params$surv_beta04<-surv.params$beta0[draws,4]     	  ## surv intercept
params$surv_beta14<-surv.params$beta1[draws,4]				## surv slope
# Ant 3 (other)
params$surv_beta03<-surv.params$beta0[draws,3]     	  ## surv intercept
params$surv_beta13<-surv.params$beta1[draws,3]				## surv slope
# Ant 1 (crem)
params$surv_beta01<-surv.params$beta0[draws,1]     	  ## surv intercept
params$surv_beta11<-surv.params$beta1[draws,1]				## surv slope
# Ant 2 (liom)
params$surv_beta02<-surv.params$beta0[draws,2]     	  ## surv intercept
params$surv_beta12<-surv.params$beta1[draws,2]				##surv slope
## Year Random Effects
# Ant 1 (prev crem)
surv_rfx1 <- data.frame(matrix(0, nrow = N_draws, ncol = 18))
colnames(surv_rfx1) <- years
surv_rfx1$'2004' <- surv.params$w[draws,1,1]; surv_rfx1$'2005' <- surv.params$w[draws,1,2]; surv_rfx1$'2006' <- surv.params$w[draws,1,3];surv_rfx1$'2009' <- surv.params$w[draws,1,4];surv_rfx1$'2010' <- surv.params$w[draws,1,5]; surv_rfx1$'2011' <- surv.params$w[draws,1,6];
surv_rfx1$'2012' <- surv.params$w[draws,1,7];surv_rfx1$'2013' <- surv.params$w[draws,1,8];surv_rfx1$'2014' <- surv.params$w[draws,1,9];
surv_rfx1$'2015' <- surv.params$w[draws,1,10];surv_rfx1$'2016' <- surv.params$w[draws,1,11];surv_rfx1$'2017' <- surv.params$w[draws,1,12];
surv_rfx1$'2018' <- surv.params$w[draws,1,13];surv_rfx1$'2019' <- surv.params$w[draws,1,14];surv_rfx1$'2021' <- surv.params$w[draws,1,15];surv_rfx1$'2022' <- surv.params$w[draws,1,16]
# Ant 2 (prev liom)
surv_rfx2 <- data.frame(matrix(0, nrow = N_draws, ncol = 19))
colnames(surv_rfx2) <- years
surv_rfx2$'2004' <- surv.params$w[draws,2,1]; surv_rfx2$'2005' <- surv.params$w[draws,2,2]; surv_rfx2$'2006' <- surv.params$w[draws,2,3];surv_rfx2$'2009' <- surv.params$w[draws,2,4];surv_rfx2$'2010' <- surv.params$w[draws,2,5]; surv_rfx2$'2011' <- surv.params$w[draws,2,6];
surv_rfx2$'2012' <- surv.params$w[draws,2,7];surv_rfx2$'2013' <- surv.params$w[draws,2,8];surv_rfx2$'2014' <- surv.params$w[draws,2,9];
surv_rfx2$'2015' <- surv.params$w[draws,2,10];surv_rfx2$'2016' <- surv.params$w[draws,2,11];surv_rfx2$'2017' <- surv.params$w[draws,2,12];
surv_rfx2$'2018' <- surv.params$w[draws,2,13];surv_rfx2$'2019' <- surv.params$w[draws,2,14];surv_rfx2$'2021' <- surv.params$w[draws,2,15];surv_rfx2$'2022' <- surv.params$w[draws,2,16]
# Ant 3 (prev other)
surv_rfx3 <- data.frame(matrix(0, nrow = N_draws, ncol = 19))
colnames(surv_rfx3) <- years
surv_rfx3$'2004' <- surv.params$w[draws,3,1]; surv_rfx3$'2005' <- surv.params$w[draws,3,2]; surv_rfx3$'2006' <- surv.params$w[draws,3,3];surv_rfx3$'2009' <- surv.params$w[draws,3,4];surv_rfx3$'2010' <- surv.params$w[draws,3,5]; surv_rfx3$'2011' <- surv.params$w[draws,3,6];
surv_rfx3$'2012' <- surv.params$w[draws,3,7];surv_rfx3$'2013' <- surv.params$w[draws,3,8];surv_rfx3$'2014' <- surv.params$w[draws,3,9];
surv_rfx3$'2015' <- surv.params$w[draws,3,10];surv_rfx3$'2016' <- surv.params$w[draws,3,11];surv_rfx3$'2017' <- surv.params$w[draws,3,12];
surv_rfx3$'2018' <- surv.params$w[draws,3,13];surv_rfx3$'2019' <- surv.params$w[draws,3,14];surv_rfx3$'2021' <- surv.params$w[draws,3,15];surv_rfx3$'2022' <- surv.params$w[draws,3,16]
# Ant 4 (prev vac)
surv_rfx4 <- data.frame(matrix(0, nrow = N_draws, ncol = 19))
colnames(surv_rfx4) <- years
surv_rfx4$'2004' <- surv.params$w[draws,4,1]; surv_rfx4$'2005' <- surv.params$w[draws,4,2]; surv_rfx4$'2006' <- surv.params$w[draws,4,3];surv_rfx4$'2009' <- surv.params$w[draws,4,4];surv_rfx4$'2010' <- surv.params$w[draws,4,5]; surv_rfx4$'2011' <- surv.params$w[draws,4,6];
surv_rfx4$'2012' <- surv.params$w[draws,4,7];surv_rfx4$'2013' <- surv.params$w[draws,4,8];surv_rfx4$'2014' <- surv.params$w[draws,4,9];
surv_rfx4$'2015' <- surv.params$w[draws,4,10];surv_rfx4$'2016' <- surv.params$w[draws,4,11];surv_rfx4$'2017' <- surv.params$w[draws,4,12];
surv_rfx4$'2018' <- surv.params$w[draws,4,13];surv_rfx4$'2019' <- surv.params$w[draws,4,14];surv_rfx4$'2021' <- surv.params$w[draws,4,15];surv_rfx4$'2022' <- surv.params$w[draws,4,16]

##-----------------------Flowering/Fecundity Parameters-----------------## 
## Check the names of the parameters
#head(flow.params)
params$flow_phi<-flow.params$phi[draws]   	      ## flow phi
params$flow_sig_u<-flow.params$sigma_u[draws]        ## flow sigma u
params$flow_sig_w<-flow.params$sigma_w[draws]        ## flow sigma w

params$flow_beta0<-flow.params$beta0[draws]          ## flow intercept
params$flow_beta1<-flow.params$beta1[draws]          ## flow slopes
#### --- Year Random Effects --- ####
flow_rfx <- data.frame(matrix(0, nrow = N_draws, ncol = 19))
colnames(flow_rfx) <- years
flow_rfx$'2004' <- flow.params$w[draws,1];flow_rfx$'2005' <- flow.params$w[draws,2];flow_rfx$'2006' <- flow.params$w[draws,3];flow_rfx$'2007' <- flow.params$w[draws,4];flow_rfx$'2013' <- flow.params$w[draws,5];flow_rfx$'2014' <- flow.params$w[draws,6];flow_rfx$'2015' <- flow.params$w[draws,7];flow_rfx$'2016' <- flow.params$w[draws,8];flow_rfx$'2017' <- flow.params$w[draws,9];flow_rfx$'2018' <- flow.params$w[draws,10];flow_rfx$'2019' <- flow.params$w[draws,11];flow_rfx$'2021' <- flow.params$w[draws,12];flow_rfx$'2022' <- flow.params$w[draws,13];flow_rfx$'2023' <- flow.params$w[draws,14];

##-----------------------Reproductive State Parameters-----------------## 
## Check the names of the parameters
#head(repro.params)
params$repro_beta0<-repro.params$beta0[draws]      ## repro intercept
params$repro_beta1<-repro.params$beta1[draws]      ## repro slope
params$repro_sig_u<-repro.params$sigma_u[draws]    ## repro sigma u
params$repro_sig_w<-repro.params$sigma_w[draws]    ## repro sigma w
## --- Year Random Effects --- ####
repro_rfx <- data.frame(matrix(0, nrow = N_draws, ncol = 19))
colnames(repro_rfx) <- years
repro_rfx$'2005' <- repro.params$w[draws,1];repro_rfx$'2006' <- repro.params$w[draws,2];repro_rfx$'2007' <- repro.params$w[draws,3];repro_rfx$'2008' <- repro.params$w[draws,4];repro_rfx$'2013' <- repro.params$w[draws,5];repro_rfx$'2014' <- repro.params$w[draws,6];repro_rfx$'2015' <- repro.params$w[draws,7];repro_rfx$'2016' <- repro.params$w[draws,8];repro_rfx$'2017' <- repro.params$w[draws,9];repro_rfx$'2018' <- repro.params$w[draws,10];repro_rfx$'2019' <- repro.params$w[draws,11];repro_rfx$'2021' <- repro.params$w[draws,12];repro_rfx$'2022' <- repro.params$w[draws,13];repro_rfx$'2023' <- repro.params$w[draws,14];
##-----------------------Viability Parameters-----------------## 
## Check the names of the parameters
#head(viab.params) 
params$viab_sig<-viab.params$sigma[draws]              ## viab sigma
params$viab_sig_u<-viab.params$sigma_u[draws]          ## viab sigma u
params$viab_sig_w<-viab.params$sigma_w[draws]          ## viab sigma w
# Ant 4 (vacant)
params$viab_beta04<-viab.params$beta0[draws,4]     	  ## viab intercept
# Ant 3 (other)
params$viab_beta03<-viab.params$beta0[draws,3]     	  ## viab intercept
# Ant 1 (crem)
params$viab_beta01<-viab.params$beta0[draws,1]     	  ## viab intercept
# Ant 2 (liom)
params$viab_beta02<-viab.params$beta0[draws,2]     	  ## viab intercept
## Year random effects
# Ant 1 (prev crem)
viab_rfx1 <- data.frame(matrix(0, nrow = N_draws, ncol = 19))
viab_rfx1$'2004' <- viab.params$w[draws,1,1];viab_rfx1$'2005' <- viab.params$w[draws,1,2];viab_rfx1$'2006' <- viab.params$w[draws,1,3];viab_rfx1$'2012' <- viab.params$w[draws,1,4];viab_rfx1$'2013' <- viab.params$w[draws,1,5];viab_rfx1$'2014' <- viab.params$w[draws,1,6];viab_rfx1$'2015' <- viab.params$w[draws,1,7];viab_rfx1$'2016' <- viab.params$w[draws,1,8];viab_rfx1$'2017' <- viab.params$w[draws,1,9];viab_rfx1$'2018' <- viab.params$w[draws,1,10];viab_rfx1$'2019' <- viab.params$w[draws,1,11];viab_rfx1$'2021' <- viab.params$w[draws,1,12];viab_rfx1$'2022' <- viab.params$w[draws,1,13];
# Ant 2 (prev liom)
viab_rfx2 <- data.frame(matrix(0, nrow = N_draws, ncol = 19))
viab_rfx2$'2004' <- viab.params$w[draws,2,1];viab_rfx2$'2005' <- viab.params$w[draws,2,2];viab_rfx2$'2006' <- viab.params$w[draws,2,3];viab_rfx2$'2012' <- viab.params$w[draws,2,4];viab_rfx2$'2013' <- viab.params$w[draws,2,5];viab_rfx2$'2014' <- viab.params$w[draws,2,6];viab_rfx2$'2015' <- viab.params$w[draws,2,7];viab_rfx2$'2016' <- viab.params$w[draws,2,8];viab_rfx2$'2017' <- viab.params$w[draws,2,9];viab_rfx2$'2018' <- viab.params$w[draws,2,10];viab_rfx2$'2019' <- viab.params$w[draws,2,11];viab_rfx2$'2021' <- viab.params$w[draws,2,12];viab_rfx2$'2022' <- viab.params$w[draws,2,13];
# Ant 3 (prev other)
viab_rfx3 <- data.frame(matrix(0, nrow = N_draws, ncol = 19))
viab_rfx3$'2004' <- viab.params$w[draws,3,1];viab_rfx3$'2005' <- viab.params$w[draws,3,2];viab_rfx2$'2006' <- viab.params$w[draws,2,3];viab_rfx3$'2012' <- viab.params$w[draws,3,4];viab_rfx3$'2013' <- viab.params$w[draws,3,5];viab_rfx2$'2014' <- viab.params$w[draws,2,6];viab_rfx3$'2015' <- viab.params$w[draws,3,7];viab_rfx3$'2016' <- viab.params$w[draws,3,8];viab_rfx2$'2017' <- viab.params$w[draws,2,9];viab_rfx3$'2018' <- viab.params$w[draws,3,10];viab_rfx3$'2019' <- viab.params$w[draws,3,11];viab_rfx2$'2021' <- viab.params$w[draws,2,12];viab_rfx3$'2022' <- viab.params$w[draws,3,13];
# Ant 4 (prev vac)
viab_rfx4 <- data.frame(matrix(0, nrow = N_draws, ncol = 19))
viab_rfx4$'2004' <- viab.params$w[draws,4,1];viab_rfx4$'2005' <- viab.params$w[draws,4,2];viab_rfx4$'2006' <- viab.params$w[draws,4,3];viab_rfx4$'2012' <- viab.params$w[draws,4,4];viab_rfx4$'2013' <- viab.params$w[draws,4,5];viab_rfx4$'2014' <- viab.params$w[draws,4,6];viab_rfx4$'2015' <- viab.params$w[draws,4,7];viab_rfx4$'2016' <- viab.params$w[draws,4,8];viab_rfx4$'2017' <- viab.params$w[draws,4,9];viab_rfx4$'2018' <- viab.params$w[draws,4,10];viab_rfx4$'2019' <- viab.params$w[draws,4,11];viab_rfx4$'2021' <- viab.params$w[draws,4,12];viab_rfx4$'2022' <- viab.params$w[draws,4,13];

##-----------------------Seeds Prod Parameters-----------------## 
## Check the names of the parameters
#head(seed.params)
params$seed_phi<-seed.params$phi[draws]          ## seed phi
params$seed_sig<-seed.params$sigma[draws]        ## seed sigma
# Ant 1 (Crem)
params$seed_beta01<-seed.params$beta0[draws,1]      ## seed intercept 
# Ant 2 (Liom)
params$seed_beta02<-seed.params$beta0[draws,2]      ## seed intercept
# Ant 3 (Vac)
params$seed_beta03<-seed.params$beta0[draws,3]      ## seed intercept

##-----------------------Seeds Precensus Surv Parameters-----------------## 
## Check the names of the parameters
#head(pre.seed.params)
params$preseed_sig<-pre.seed.params$sigma[draws]         ## pre seed sigma
params$preseed_sig_w<-pre.seed.params$sigma_w[draws]       ## pre seed sigma w
params$preseed_beta0<-pre.seed.params$beta0[draws]       ## pre seed intercept

##-----------------------Germ1 Parameters-----------------## Ant 1 (crem)
## Check the names of the parameters
#head(germ1.params)
params$germ1_phi<-germ1.params$phi[draws]            ## germ 1 phi
params$germ1_sig<-germ1.params$sigma[draws]            ## germ 1 sigma
params$germ1_beta0<-germ1.params$beta0[draws]        ## germ 1 intercept

##-----------------------Germ2 Parameters-----------------## Ant 1 (crem)
## Check the names of the parameters
#head(germ2.params)
params$germ2_phi<-germ2.params$phi[draws]            ## germ 2 phi
params$germ2_sig<-germ2.params$sigma[draws]        ## germ 2 sigma
params$germ2_beta0<-germ2.params$beta0[draws]        ## germ 2 intercept

##------------------------- Recruitment-------------------##
## Check the names of the parameters
#head(rec.params)
params$rec_beta0<-rec.params$beta0[draws]        ## Rec intercept
params$rec_sig<-rec.params$sigma[draws]         ## Rec error

##-------------------------Transition Parameters-------------------##
# Prev Vac
params$multi_betavv <- multi.params$beta[draws,4,4] ## intercept for vacant to vacant  
params$multi_betavo <- multi.params$beta[draws,4,3] ## intercept for vacant to other
params$multi_betavc <- multi.params$beta[draws,4,1] ## intercept for vacant to crem
params$multi_betavl <- multi.params$beta[draws,4,2] ## intercept for vacant to liom
params$multi_betav <- multi.params$beta[draws,5,4] ## Size specific vacant slope
# Prev Other
params$multi_betaov <- multi.params$beta[draws,3,4]
params$multi_betaoo <- multi.params$beta[draws,3,3]
params$multi_betaoc <- multi.params$beta[draws,3,1]
params$multi_betaol <- multi.params$beta[draws,3,2]
params$multi_betao <- multi.params$beta[draws,5,2]
# Prev Crem
params$multi_betacv <- multi.params$beta[draws,1,4]
params$multi_betaco <- multi.params$beta[draws,1,3]
params$multi_betacc <- multi.params$beta[draws,1,1]
params$multi_betacl <- multi.params$beta[draws,1,2]
params$multi_betac <- multi.params$beta[draws,5,1]
# Prev Liom
params$multi_betalv <- multi.params$beta[draws,2,4]
params$multi_betalo <- multi.params$beta[draws,2,3]
params$multi_betalc <- multi.params$beta[draws,2,1]
params$multi_betall <- multi.params$beta[draws,2,2]
params$multi_betal <- multi.params$beta[draws,5,2]

#########################################################################################################
##            This will be an IPM which allows you to choose how many ants are present
#########################################################################################################
#setwd("/Users/alicampbell/Documents/GitHub/ant_cactus_demography/IPM Code")
## ----------- Miscellany...we'll need an inverse logit functions ------------- ##
invlogit<-function(x){exp(x)/(1+exp(x))}

## ----------- Vital rate functions. Parameter indices are hard-coded and must correspond to rows of MCMC matrix ------------- ##
#########################################################################################################
## GROWTH FROM SIZE X TO Y. Returns the probability of growth from size x to y based on ant state    ####
## This model accepts the input of previous size and ant state to determine the probability of being ####
## y size in the next year.                                                                          ####
## This function is vectorized so if you input a vector for x and y and a single ant species you     ####
## will get a vector of probabilities.                                                               ####
#########################################################################################################

gxy<-function(x,y,i,params,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4){
  #Transform all values below/above limits in min/max size
  xb=pmin(pmax(x,cholla_min),cholla_max)
  #Density probability function which uses the parameters that are ant specific
  g_crem = dlst(y,mu=(params$grow_beta01) + (params$grow_beta11)*xb + (params$grow_beta21)*xb^2 + grow_rfx1,
                sigma = exp((params$grow_sig0) + (params$grow_sig1)*xb),
                df = exp((params$grow_alp0) + (params$grow_alp1)*xb))
  g_vac = dlst(y, mu = (params$grow_beta04) + (params$grow_beta14)*xb + (params$grow_beta24)*xb^2 + grow_rfx4,
               sigma = exp((params$grow_sig0) + (params$grow_sig1)*xb),
               df = exp((params$grow_alp0) + (params$grow_alp1)*xb))
  g_other = dlst(y, mu = (params$grow_beta03) + (params$grow_beta13)*xb + (params$grow_beta23)*xb^2 + grow_rfx3,
                 sigma = exp((params$grow_sig0) + (params$grow_sig1)*xb),
                 df = exp((params$grow_alp0) + (params$grow_alp1)*xb))
  g_liom = dlst(y, mu = (params$grow_beta02) + (params$grow_beta12)*xb + (params$grow_beta22)*xb^2 + grow_rfx2,
                sigma = exp((params$grow_sig0) + (params$grow_sig1)*xb),
                df = exp((params$grow_alp0) + (params$grow_alp1)*xb))
  #Return the probability of growing from size x to y
  if(i == "crem"){ return(g_crem)}
  if(i == "liom"){ return(g_liom)}
  if(i == "other"){ return(g_other)}
  if(i == "vacant"){ return(g_vac)}
}

# # ##Check that it works properly
# i = c("vacant","crem","liom","other")
# x = c(-1,-5,3,4)
# y = c(-1,-4,3,4)
# g <- matrix(NA,ncol = length(i), nrow = 10)
# l <- list()
#
# for(a in 1:17){ ## year
# for(m in 1:10){ ## iteration
#   for(n in seq(1:length(i))){ ## input info
#     xb=pmin(pmax(x,cholla_min),cholla_max) #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
#     g[m,n] <- gxy(x[n],y[n],i[n],params[m,],grow_rfx1[m,a],grow_rfx2[m,a],grow_rfx3[m,a],grow_rfx4[m,a])
#     }
# }
#   l[[a]] <- g
# }
# g
# l

#########################################################################################################
## SURVIVAL AT SIZE X. Returns the probability of survival of a cactus based on size and ant state   ####
## You input the size of the cactus and ant state and in return you get the probability of surviving ####
## to the next year.                                                                                 ####
## This function is vectorized so if you input a vector for x and y and a single ant species you     ####
## will get a vector of probabilities.                                                               ####
#########################################################################################################
sx<-function(x,i,params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4){
  #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  xb=pmin(pmax(x,cholla_min),cholla_max)
  #Transform the ant specific parameters to the probability of survival
  s_other = invlogit((params$surv_beta03) + (params$surv_beta13)*xb)
  s_crem = invlogit((params$surv_beta01) + (params$surv_beta11)*xb)
  s_liom = invlogit((params$surv_beta02) + (params$surv_beta12)*xb)
  s_vac = invlogit((params$surv_beta04) + (params$surv_beta14)*xb)
  #Return the survival probabilities
  if(i == "crem"){ return(s_crem)}
  if(i == "liom"){ return(s_liom)}
  if(i == "other"){ return(s_other)}
  if(i == "vacant"){ return(s_vac)}
}

# # ##Check that it works properly
# i = c("liom","vacant","crem","other")
# x = c(-1,2,4,3)
# s <- matrix(NA,ncol = length(i), nrow = 1)
# l <- list()
# #for(a in 1:2){ ## year
# for(m in 1:1){ ## iteration
#   for(n in seq(1:length(i))){ ## input info
#     s[m,n] <- sx(x[n],i[n],params[m,],surv_rfx1[m,a],surv_rfx2[m,a],surv_rfx3[m,a],surv_rfx4[m,a])
#     }
# }
#   l[[a]] <- s
# #}
#
# s
# l

#################################################
#SURVIVAL*GROWTH. Combine the survival and growth probabilities
pxy<-function(x,y,i,params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4){
  #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  xb=pmin(pmax(x,cholla_min),cholla_max)
  #Multiply the probabilities of survival and growth together to get the survival growth kernel
  px = sx(x,i,params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4)*gxy(x,y,i,params,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4)
  return(px)
}

# ##Check that it works properly
# i = c("liom","vacant","crem","other")
# x = c(-1,-5,4,3)
# y = c(-1,-4,4,3)
# px <- matrix(NA,ncol = length(i), nrow = (10))
# l <- list()
# for(a in 1:17){ ## year
# for(m in 1:10){ ## iteration
#   for(n in 1:length(i)){ ## input info
#     px[m,n] <- pxy(x[n],y[n],i[n],params[m,],surv_rfx1[m,a],surv_rfx2[m,a],surv_rfx3[m,a],surv_rfx4[m,a],grow_rfx1[m,a],grow_rfx2[m,a],grow_rfx3[m,a],grow_rfx4[m,a])
#     }
# }
#   l[[a]] <- px
# }
#
# px
# l

#################################################################
#PRODUCTION OF 1-YO SEEDS IN THE SEED BANK FROM X-SIZED MOMS
fx<-function(x,i,params,flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4){

  #Transforms all values below/above limits in min/max size (So the params are the minimums and maximums of size?)
  xb=pmin(pmax(x,cholla_min),cholla_max)
  p.flow<-invlogit((params$repro_beta0) + (params$repro_beta1)*xb + repro_rfx)      ## Probability of Reproducing
  nflow<-exp((params$flow_beta0) + (params$flow_beta1)*xb+ flow_rfx)      ## Number of FLowers produced
  flow.surv_other<-invlogit((params$viab_beta03 + viab_rfx3))      ## Proportion of Flowers survive to fruit
  flow.surv_crem<-invlogit((params$viab_beta01 + viab_rfx1))       ## Proportion of Flowers survive to fruit
  flow.surv_liom<-invlogit((params$viab_beta02 + viab_rfx2))       ## Proportion of Flowers survive to fruit
  flow.surv_vac<-invlogit((params$viab_beta04 + viab_rfx4))      ## Proportion of Flowers survive to fruit
  seeds.per.fruit_crem<-(params$seed_beta01)      ## Number of Seeds per Fruit
  seeds.per.fruit_liom<-(params$seed_beta03)      ## Number of Seeds per Fruit
  seeds.per.fruit_vac<-(params$seed_beta02)     ## Number of Seeds per Fruit
  seed.survival<-invlogit((params$preseed_beta0))^2       ## Seed per Fruit Survival ---------I measured 6-month seed survival; annual survival is its square
  #Calculate the fecundity probabilities by ant species
  f_crem = p.flow*nflow*flow.surv_crem*seeds.per.fruit_crem*seed.survival
  f_vac = p.flow*nflow*flow.surv_vac*seeds.per.fruit_vac*seed.survival
  f_other = p.flow*nflow*flow.surv_other*seeds.per.fruit_vac*seed.survival
  f_liom = p.flow*nflow*flow.surv_liom*seeds.per.fruit_liom*seed.survival
  #Return the correct value
  if(i == "crem"){ return(f_crem)}
  if(i == "liom"){ return(f_liom)}
  if(i == "other"){ return(f_other)}
  if(i == "vacant"){ return(f_vac)}
}

## Check if it works
# i = c("liom","vacant","crem","other")
# x = c(-1,-5,4,3)
# y = c(-1,-4,4.5,3.01)
# f <- matrix(NA,ncol = length(i), nrow = 100)
# l <- list()
# for(a in 1:17){ ## year
# for(m in seq(1:100)){ ## iteration
#   for(n in seq(1:length(i))){ ## input info
#     f[m,n] <- fx(x[n],i[n],params[m,],flow_rfx[m,a],repro_rfx[m,a],viab_rfx1[m,a],viab_rfx2[m,a],viab_rfx3[m,a],viab_rfx4[m,a])
#     }
# }
#   l[[a]] <- f
# }
# f
# l

####################################################
#### Recruitment
recruits<-function(y,params){
  yb=pmin(pmax(y,cholla_min),cholla_max)
  dnorm(yb, (params$rec_beta0),(params$rec_sig))
}

# # Check if it works
# i = c("liom","vacant","crem","other")
# x = c(-1,-5,4,3)
# y = c(-1,-4,4.5,3.01)
# r <- matrix(NA,ncol = length(i), nrow = (10))
# for(m in 1:10){
#   for(n in 1:length(i)){
#     r[m,n] <- recruits(y[n],params[m,])
#   }
# }
# r


######################################################
#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE. One ant option
transition.1<-function(x, i, j,params, scenario){
  #Transforms all values below/above limits in min/max size
  xb=pmin(pmax(x,cholla_min),cholla_max)
  ## Crem and Vac
  if(scenario == "cremvac"){
    #Denom of previously tended by None
    Denominator_vac <- exp((params$multi_betavv) + xb*(params$multi_betav)) +
      exp((params$multi_betavc) + xb*(params$multi_betac))
    #Calculate the probabilities by next ant state
    vac_vac = exp((params$multi_betavv) + xb*(params$multi_betav))/Denominator_vac
    vac_crem = exp((params$multi_betavc) + xb*(params$multi_betac))/Denominator_vac
    #Denom of previously tended by Crem
    Denominator_crem <- exp((params$multi_betacv) + xb*(params$multi_betav)) +
      exp((params$multi_betacc) + xb*(params$multi_betac))
    #Calculate the probabilities by next ant state
    crem_vac = exp((params$multi_betacv) + xb*(params$multi_betav))/Denominator_crem
    crem_crem = exp((params$multi_betacc) + xb*(params$multi_betac))/Denominator_crem
    #Return them
    if(i == "crem" & j == "crem"){return(crem_crem)}
    if(i == "crem" & j == "vacant"){return(crem_vac)}
    if(i == "vacant" & j == "crem"){return(vac_crem)}
    if(i == "vacant" & j == "vacant"){return(vac_vac)}
  }
  ## Liom and Vac
  if(scenario == "liomvac"){
    #Denom of previously tended by None
    Denominator_vac <- exp((params$multi_betavv) + xb*(params$multi_betav)) +
      exp((params$multi_betavl) + xb*(params$multi_betal))
    #Calculate the probabilities by next ant state
    vac_vac = exp((params$multi_betavv) + xb*(params$multi_betav))/Denominator_vac
    vac_liom = exp((params$multi_betavl) + xb*(params$multi_betal))/Denominator_vac
    #Denom of previously tended by Liom
    Denominator_liom <- exp((params$multi_betalv) + xb*(params$multi_betav)) +
      exp((params$multi_betall) + xb*(params$multi_betal))
    #Calculate the probabilities by next ant state
    liom_vac = exp((params$multi_betalv) + xb*(params$multi_betav))/Denominator_liom
    liom_liom = exp((params$multi_betall) + xb*(params$multi_betal))/Denominator_liom
    #Return the probabilities
    if(i == "liom" & j == "liom"){return(liom_liom)}
    if(i == "liom" & j == "vacant"){return(liom_vac)}
    if(i == "vacant" & j == "liom"){return(vac_liom)}
    if(i == "vacant" & j == "vacant"){return(vac_vac)}
  }
  ## Other and Vac
  if(scenario == "othervac"){
    #Denom of previously tended by None
    Denominator_vac <- exp((params$multi_betavv) + xb*(params$multi_betav)) +
      exp((params$multi_betavo) + xb*(params$multi_betao))
    #Calculate the probabilities by next ant state
    vac_vac = exp((params$multi_betavv) + xb*(params$multi_betav))/Denominator_vac
    vac_other = exp((params$multi_betavo) + xb*(params$multi_betao))/Denominator_vac
    #Denom of previously tended by Liom
    Denominator_other <- exp((params$multi_betaov) + xb*(params$multi_betav)) +
      exp((params$multi_betaoo) + xb*(params$multi_betao))
    #Calculate the probabilities by next ant state
    other_vac = exp((params$multi_betaov) + xb*(params$multi_betav))/Denominator_other
    other_other = exp((params$multi_betaoo) + xb*(params$multi_betao))/Denominator_other
    #Return the probabilities
    if(i == "other" & j == "other"){return(other_other)}
    if(i == "other" & j == "vacant"){return(other_vac)}
    if(i == "vacant" & j == "other"){return(vac_other)}
    if(i == "vacant" & j == "vacant"){return(vac_vac)}
  }
}



##########################################################
#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE (THREE STATES)
transition.2<-function(x, i, j, params,scenario){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  ## Liom, Vac, Other
  #Denom of previously tended by None
  if(scenario == "liomvacother"){
    Denominator_vac <- exp((params$multi_betavv) + xb*(params$multi_betav)) +
      exp((params$multi_betavl) + xb*(params$multi_betal)) +
      exp((params$multi_betavo) + xb*(params$multi_betao))
    #Calculate the probabilities by next ant state
    vac_vac = exp((params$multi_betavv) + xb*(params$multi_betav))/Denominator_vac
    vac_liom = exp((params$multi_betavl) + xb*(params$multi_betal))/Denominator_vac
    vac_other = exp((params$multi_betavo) + xb*(params$multi_betao))/Denominator_vac
    #Denom of previously tended by Liom
    Denominator_liom <- exp((params$multi_betalv) + xb*(params$multi_betav)) +
      exp((params$multi_betall) + xb*(params$multi_betal)) +
      exp((params$multi_betalo) + xb*(params$multi_betao))
    #
    liom_vac = exp((params$multi_betalv) + xb*(params$multi_betav))/Denominator_liom
    liom_liom = exp((params$multi_betall) + xb*(params$multi_betal))/Denominator_liom
    liom_other = exp((params$multi_betalo) + xb*(params$multi_betao))/Denominator_liom
    ## Previously tended by Other
    Denominator_other <- exp((params$multi_betaov) + xb*(params$multi_betav)) +
      exp((params$multi_betaol) + xb*(params$multi_betal)) +
      exp((params$multi_betaoo) + xb*(params$multi_betao))
    other_vac = exp((params$multi_betaov) + xb*(params$multi_betav))/Denominator_other
    other_liom = exp((params$multi_betaol) + xb*(params$multi_betal))/Denominator_other
    other_other = exp((params$multi_betaoo) + xb*(params$multi_betao))/Denominator_other
    if(i == "liom" & j == "liom"){return(liom_liom)}
    if(i == "liom" & j == "vacant"){return(liom_vac)}
    if(i == "liom" & j == "other"){return(liom_other)}
    if(i == "vacant" & j == "liom"){return(vac_liom)}
    if(i == "vacant" & j == "vacant"){return(vac_vac)}
    if(i == "vacant" & j == "other"){return(vac_other)}
    if(i == "other" & j == "liom"){return(other_liom)}
    if(i == "other" & j == "vacant"){return(other_vac)}
    if(i == "other" & j == "other"){return(other_other)}
  }
  ## Liom, Crem, vac
  ## Previously tended by None
  if(scenario == "liomcremvac"){
    Denominator_crem <- exp((params$multi_betacc) + xb*(params$multi_betac)) +
      exp((params$multi_betacl) + xb*(params$multi_betal)) +
      exp((params$multi_betacv) + xb*(params$multi_betav))
    crem_crem = exp((params$multi_betacc) + xb*(params$multi_betac))/Denominator_crem
    crem_liom = exp((params$multi_betacl) + xb*(params$multi_betal))/Denominator_crem
    crem_vac = exp((params$multi_betacv) + xb*(params$multi_betav))/Denominator_crem
    ## Previously tended by Liom
    Denominator_liom <- exp((params$multi_betalc) + xb*(params$multi_betac)) +
      exp((params$multi_betall) + xb*(params$multi_betal)) +
      exp((params$multi_betalv) + xb*(params$multi_betav))
    liom_crem = exp((params$multi_betalc) + xb*(params$multi_betac))/Denominator_liom
    liom_liom = exp((params$multi_betall) + xb*(params$multi_betal))/Denominator_liom
    liom_vac = exp((params$multi_betalv) + xb*(params$multi_betav))/Denominator_liom
    ## Previously tended by None
    Denominator_vac <- exp((params$multi_betavc) + xb*(params$multi_betac)) +
      exp((params$multi_betavl) + xb*(params$multi_betal)) +
      exp((params$multi_betavv) + xb*(params$multi_betav))
    vac_crem = exp((params$multi_betavc) + xb*(params$multi_betac))/Denominator_vac
    vac_liom = exp((params$multi_betavl) + xb*(params$multi_betal))/Denominator_vac
    vac_vac = exp((params$multi_betavv) + xb*(params$multi_betav))/Denominator_vac
    if(i == "liom" & j == "liom"){return(liom_liom)}
    if(i == "liom" & j == "crem"){return(liom_crem)}
    if(i == "liom" & j == "vacant"){return(liom_vac)}
    if(i == "crem" & j == "liom"){return(crem_liom)}
    if(i == "crem" & j == "crem"){return(crem_crem)}
    if(i == "crem" & j == "vacant"){return(crem_vac)}
    if(i == "vacant" & j == "liom"){return(vac_liom)}
    if(i == "vacant" & j == "crem"){return(vac_crem)}
    if(i == "vacant" & j == "vacant"){return(vac_vac)}
  }
  ## Other, Crem, vac
  ## Previously tended by None
  if(scenario == "othercremvac"){
    Denominator_crem <- exp((params$multi_betacc) + xb*(params$multi_betac)) +
      exp((params$multi_betaco) + xb*(params$multi_betao)) +
      exp((params$multi_betacv) + xb*(params$multi_betav))
    crem_crem = exp((params$multi_betacc) + xb*(params$multi_betac))/Denominator_crem
    crem_other = exp((params$multi_betaco) + xb*(params$multi_betao))/Denominator_crem
    crem_vac = exp((params$multi_betacv) + xb*(params$multi_betav))/Denominator_crem
    ## Previously tended by Liom
    Denominator_other <- exp((params$multi_betaoc) + xb*(params$multi_betac)) +
      exp((params$multi_betaoo) + xb*(params$multi_betao)) +
      exp((params$multi_betaov) + xb*(params$multi_betav))
    other_crem = exp((params$multi_betaoc) + xb*(params$multi_betac))/Denominator_other
    other_other = exp((params$multi_betaoo) + xb*(params$multi_betao))/Denominator_other
    other_vac = exp((params$multi_betaov) + xb*(params$multi_betav))/Denominator_other
    ## Previously tended by None
    Denominator_vac <- exp((params$multi_betavc) + xb*(params$multi_betac)) +
      exp((params$multi_betavo) + xb*(params$multi_betao)) +
      exp((params$multi_betavv) + xb*(params$multi_betav))
    vac_crem = exp((params$multi_betavc) + xb*(params$multi_betac))/Denominator_vac
    vac_other = exp((params$multi_betavo) + xb*(params$multi_betao))/Denominator_vac
    vac_vac = exp((params$multi_betavv) + xb*(params$multi_betav))/Denominator_vac
    if(i == "other" & j == "other"){return(other_other)}
    if(i == "other" & j == "crem"){return(other_crem)}
    if(i == "other" & j == "vacant"){return(other_vac)}
    if(i == "crem" & j == "other"){return(crem_other)}
    if(i == "crem" & j == "crem"){return(crem_crem)}
    if(i == "crem" & j == "vacant"){return(crem_vac)}
    if(i == "vacant" & j == "other"){return(vac_other)}
    if(i == "vacant" & j == "crem"){return(vac_crem)}
    if(i == "vacant" & j == "vacant"){return(vac_vac)}
  }
}
# ## Scenario options are "liomvacother", "liomcremother", "liomcremvac", "othercremvac"
# ## Check if it works
# i = c("liom","vacant","other","other")
# j = c("vacant","liom","other","liom")
# x = c(15,15,15,15)
# y = c(-1,-4,4.5,3.01)
# scenario = "liomvacother"
# t2 <- matrix(NA,ncol = length(i), nrow = (10))
# for(m in 1:10){
#   for(n in 1:length(i)){
#     t2[m,n] <- transition.2(x[n],i[n],j[n],params[m,],scenario)
#   }
# }
# t2
#
#######################################################
#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE (ALL STATES)
transition.3<-function(x, i, j,params){
  xb=pmin(pmax(x,cholla_min),cholla_max)
  ## Previously tended by None
  Denominator_vac <- exp((params$multi_betavv) + xb*(params$multi_betav)) +
    exp((params$multi_betavo) + xb*(params$multi_betao)) +
    exp((params$multi_betavc) + xb*(params$multi_betac)) +
    exp((params$multi_betavl) + xb*(params$multi_betal))
  vac_vac = exp((params$multi_betavv) + xb*(params$multi_betav))/Denominator_vac
  vac_other = exp((params$multi_betavo) + xb*(params$multi_betao))/Denominator_vac
  vac_crem = exp((params$multi_betavc) + xb*(params$multi_betac))/Denominator_vac
  vac_liom = exp((params$multi_betavl) + xb*(params$multi_betal))/Denominator_vac
  ## Previously tended by Other
  Denominator_other <- exp((params$multi_betaov) + xb*(params$multi_betav)) +
    exp((params$multi_betaoo) + xb*(params$multi_betao)) +
    exp((params$multi_betaoc) + xb*(params$multi_betac)) +
    exp((params$multi_betaol) + xb*(params$multi_betal))
  other_vac = exp((params$multi_betaov) + xb*(params$multi_betav))/Denominator_other
  other_other = exp((params$multi_betaoo) + xb*(params$multi_betao))/Denominator_other
  other_crem = exp((params$multi_betaoc) + xb*(params$multi_betac))/Denominator_other
  other_liom = exp((params$multi_betaol) + xb*(params$multi_betal))/Denominator_other
  ## Previously tended by Crem
  Denominator_crem <- exp((params$multi_betacv) + xb*(params$multi_betav)) +
    exp((params$multi_betaco) + xb*(params$multi_betao)) +
    exp((params$multi_betacc) + xb*(params$multi_betac)) +
    exp((params$multi_betacl) + xb*(params$multi_betal))
  crem_vac = exp((params$multi_betacv) + xb*(params$multi_betav))/Denominator_crem
  crem_other = exp((params$multi_betaco) + xb*(params$multi_betao))/Denominator_crem
  crem_crem = exp((params$multi_betacc) + xb*(params$multi_betac))/Denominator_crem
  crem_liom = exp((params$multi_betacl) + xb*(params$multi_betal))/Denominator_crem
  ## Previously tended by Liom
  Denominator_liom <- exp((params$multi_betalv) + xb*(params$multi_betav)) +
    exp((params$multi_betalo) + xb*(params$multi_betao)) +
    exp((params$multi_betalc) + xb*(params$multi_betac)) +
    exp((params$multi_betall) + xb*(params$multi_betal))
  liom_vac = exp((params$multi_betalv) + xb*(params$multi_betav))/Denominator_liom
  liom_other = exp((params$multi_betalo) + xb*(params$multi_betao))/Denominator_liom
  liom_crem = exp((params$multi_betalc) + xb*(params$multi_betac))/Denominator_liom
  liom_liom = exp((params$multi_betall) + xb*(params$multi_betal))/Denominator_liom
  if(i == "liom" & j == "liom"){return(liom_liom)}
  if(i == "liom" & j == "other"){return(liom_other)}
  if(i == "liom" & j == "crem"){return(liom_crem)}
  if(i == "liom" & j == "vacant"){return(liom_vac)}
  if(i == "other" & j == "liom"){return(other_liom)}
  if(i == "other" & j == "other"){return(other_other)}
  if(i == "other" & j == "crem"){return(other_crem)}
  if(i == "other" & j == "vacant"){return(other_vac)}
  if(i == "crem" & j == "liom"){return(crem_liom)}
  if(i == "crem" & j == "other"){return(crem_other)}
  if(i == "crem" & j == "crem"){return(crem_crem)}
  if(i == "crem" & j == "vacant"){return(crem_vac)}
  if(i == "vacant" & j == "liom"){return(vac_liom)}
  if(i == "vacant" & j == "other"){return(vac_other)}
  if(i == "vacant" & j == "crem"){return(vac_crem)}
  if(i == "vacant" & j == "vacant"){return(vac_vac)}
}
# ## Chekc if it works
# i = c("liom","liom")
# j = c("vacant","vacant")
# x = c(-1,-5)
# y = c(-1,-5)
# t3 <- matrix(NA,ncol = length(i), nrow = (N_draws))
# for(m in 1:nrow(params)){
#   for(n in 1:length(i)){
#     t3[m,n] <- transition.3(x[n],i[n],j[n],params[m,])
#   }
# }
# t3

#########################################################
#PROBABILITY OF BEING TENDED BY ANT J BASED ON PREVIOUS VOLUME AND ANT STATE
transition.x <- function(x,i,j,params,scenario){
  one <- transition.1(x,i,j,params,scenario)
  two <- transition.2(x,i,j,params,scenario)
  three <- transition.3(x,i,j,params)
  if(scenario == "cremvac"){return(one)}
  if(scenario == "liomvac"){return(one)}
  if(scenario == "othervac"){return(one)}
  if(scenario == "liomvacother"){return(two)}
  if(scenario ==  "liomcremvac"){return(two)}
  if(scenario == "othercremvac"){return(two)}
  if(scenario == "all"){return(three)}
}
# ## Check if it works
# i = c("liom","vacant","crem","other")
# j = c("vacant","crem","crem","liom")
# x = c(-1,-5,4,3)
# y = c(-1,-4,4.5,3.01)
# scenario = "all"
# t <- matrix(NA,ncol = length(i), nrow = (10))
# for(m in 1:10){
#   for(n in 1:length(i)){
#     t[m,n] <- transition.x(x[n],i[n],j[n],params[m,],scenario)
#   }
# }
# t


##################################################################################################
############################# ONE ANT MATRIX #####################################################
##################################################################################################
bigmatrix.1 <- function(params,lower,upper,matsize,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4){
  ###################################################################################################
  ## returns the full IPM kernel (to be used in stochastic simulation), the F and T kernels, and meshpoints in the units of size
  ## params,yrfx,plotfx, and mwye get passed to the vital rate functions
  ## f.eps is fertility overdispersion. defaults to zero (see lambda.fun())
  ## lower and upper are the integration limits
  ## matsize is the dimension of the approximating matrix (it gets an additional 2 rows and columns for the seed banks)
  ###################################################################################################
  #Applying the midpoint rule
  n<-matsize
  L<-lower - floor
  U<-upper + ceiling
  h<-(U-L)/n                   #Bin size
  b<-L+c(0:n)*h;               #Lower boundaries of bins
  y<-0.5*(b[1:n]+b[2:(n+1)]);  #Bin midpoints

  # Fertility matricies -- One Ant
  Fmat<-matrix(0,(n+2),(n+2))
  # Growth/survival transition matricies -- One Ant
  Tmat<-matrix(0,(n+2),(n+2))
  ## Full Matricies
  IPMmat <- matrix()

  # Banked seeds go in top row
  Fmat[1,3:(n+2)]<-fx(y,"vacant",params,flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4)
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  Tmat[2,1]<-1-invlogit((params$germ1_beta0))
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),1]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),2]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
  # Growth/survival transitions among cts sizes
  Tmat[3:(n+2),3:(n+2)]<-t(outer(y,y,pxy,"vacant",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h
  # Put it all together
  IPMmat<-Fmat+Tmat
  p <- colSums(Tmat[3:(n+2),3:(n+2)])
  evict<-matsize-sum(colSums(Tmat[3:(n+2),3:(n+2)]))
  return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat, y=y, evict=evict, p=p))
  #lambda = Re(eigen(IPMmat)$values[1])
  #return(lambda)
}
#  x <- c(1,1)
#  lam <- matrix(rep(NA,120), nrow = 10)
#  l <- list()
#  for(a in 1:1){ ## years
#  for(m in 1:10){ ## params
#   #lam[m,a] <- lambda(bigmatrix.1(params[m,],lower,upper,matsize,grow_rfx1[m,a],grow_rfx2[m,a],grow_rfx3[m,a],grow_rfx4[m,a],surv_rfx1[m,a],surv_rfx2[m,a],surv_rfx3[m,a],surv_rfx4[m,a],flow_rfx[m,a],repro_rfx[m,a],viab_rfx1[m,a],viab_rfx2[m,a],viab_rfx3[m,a],viab_rfx4[m,a])$IPMmat)
#    l[[m]] <- bigmatrix.1(params[m,],lower,upper,matsize,grow_rfx1[m,a],grow_rfx2[m,a],grow_rfx3[m,a],grow_rfx4[m,a],surv_rfx1[m,a],surv_rfx2[m,a],surv_rfx3[m,a],surv_rfx4[m,a],flow_rfx[m,a],repro_rfx[m,a],viab_rfx1[m,a],viab_rfx2[m,a],viab_rfx3[m,a],viab_rfx4[m,a])$p
#  }
#  }
# #lam # each row is different iteration and each column is a year
# l[1]
#################################################################################################
##################################### One Ant Species and Vacant ################################
#################################################################################################
bigmatrix.2 <- function(params,lower,upper,matsize,scenario,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4){
  ###################################################################################################
  ## returns the full IPM kernel (to be used in stochastic simulation), the F and T kernels, and meshpoints in the units of size
  ## params,yrfx,plotfx, and mwye get passed to the vital rate functions
  ## f.eps is fertility overdispersion. defaults to zero (see lambda.fun())
  ## lower and upper are the integration limits
  ## matsize is the dimension of the approximating matrix (it gets an additional 2 rows and columns for the seed banks)
  ###################################################################################################
  n<-matsize
  L<-lower - floor
  U<-upper + ceiling
  h<-(U-L)/n                   #Bin size
  b<-L+c(0:n)*h;               #Lower boundaries of bins
  y<-0.5*(b[1:n]+b[2:(n+1)]);  #Bin midpoints

  # Fertility matricies -- Two Ant
  Fmat <- matrix(0,(2*n+2),(2*n+2))
  # Growth/survival transition matricies -- Two Ant
  Tmat <- matrix(0,(2*n+2),(2*n+2))
  ## Full Matricies
  IPMmat <- matrix()
  ############################################# LIOM ############################################
  if(scenario == "liomvac"){
    # Banked seeds go in top row (1 == liom, 2 == vacant)
    Fmat[1,3:(n+2)]<-fx(y,"liom",params,flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4)
    Fmat[1,(n+3):(2*n+2)]<-fx(y,"vacant",params,flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4)
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit((params$germ1_beta0))
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
    Tmat[(n+3):(2*n+2),1]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
    Tmat[3:(n+2),1]<-0
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
    Tmat[(n+3):(2*n+2),2]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
    Tmat[3:(n+2),2]<-0
    # Growth/survival transitions among cts sizes
    ## liom-liom
    Tmat[3:(n+2),3:(n+2)]<-(t(outer(y,y,pxy,i = "liom",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "liom",j = "liom",params,"liomvac"))
    ## liom-vacant
    Tmat[(n+3):(2*n+2),3:(n+2)]<-(t(outer(y,y,pxy,i = "liom",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "liom",j = "vacant",params,"liomvac"))
    ## vacant-liom
    Tmat[3:(n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,i = "vacant",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "vacant",j = "liom",params,"liomvac"))
    ## vacant-vacant
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,i = "vacant",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "vacant",j = "vacant",params,"liomvac"))
    # Put it all together
    colSums(Tmat[3:(2*n+2),3:(2*n+2)])
    IPMmat<-Fmat+Tmat
    # Calculate the lambda
    # lambda = Re(eigen(IPMmat)$values[1])
    # return(lambda)
    return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
  }
  ########################################### CREM ###############################################
  if(scenario == "cremvac"){
    # Banked seeds go in top row (1 == crem, 2 == vacant)
    Fmat[1,3:(n+2)]<-fx(y,"crem",params,flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4)
    Fmat[1,(n+3):(2*n+2)]<-fx(y,"vacant",params,flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4)
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit((params$germ1_beta0))
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
    Tmat[3:(n+2),1]<-0
    Tmat[(n+3):(2*n+2),1]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
    Tmat[3:(n+2),2]<-0
    Tmat[(n+3):(2*n+2),2]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
    # Growth/survival transitions among cts sizes
    Tmat[3:(n+2),3:(n+2)]<-(t(outer(y,y,pxy,i = "crem",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "crem",j = "crem",params,"cremvac"))
    Tmat[3:(n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,i = "vacant",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "vacant",j = "crem",params,"cremvac"))
    Tmat[(n+3):(2*n+2),3:(n+2)]<-(t(outer(y,y,pxy,i = "crem",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "crem",j = "vacant",params,"cremvac"))
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,i = "vacant",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "vacant",j = "vacant",params,"cremvac"))
    # Put it all together
    colSums(Tmat[3:(2*n+2),3:(2*n+2)])
    IPMmat<-Fmat+Tmat# Calculate the lambda
    # lambda = Re(eigen(IPMmat)$values[1])
    # return(lambda)
    return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
  }
  ############################################# OTHER ###########################################
  if(scenario == "othervac"){
    # Banked seeds go in top row (1 == other, 2 == vacant)
    Fmat[1,3:(n+2)]<-fx(y,"other",params,flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4)
    Fmat[1,(n+3):(2*n+2)]<-fx(y,"vacant",params,flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4)
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit((params$germ1_beta0))
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
    Tmat[3:(n+2),1]<-0
    Tmat[(n+3):(2*n+2)]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    # Set the non-vacant recruit size to 0 because we are forcing all new plants to be vacant
    Tmat[3:(n+2),2]<-0
    Tmat[(n+3):(2*n+2)]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
    # Growth/survival transitions among cts sizes
    Tmat[3:(n+2),3:(n+2)]<-(t(outer(y,y,pxy,i = "other",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "other",j = "other",params,"othervac"))
    Tmat[3:(n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,"vacant",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "vacant",j = "other",params,"othervac"))
    Tmat[(n+3):(2*n+2),3:(n+2)]<-(t(outer(y,y,pxy,"other",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "other",j = "vacant",params,"othervac"))
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,"vacant",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "vacant",j = "vacant",params,"othervac"))
    # Put it all together
    colSums(Tmat[3:(2*n+2),3:(2*n+2)])
    IPMmat<-Fmat+Tmat
    # Calculate the lambda
    # lambda = Re(eigen(IPMmat)$values[1])
    # return(lambda)
    return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
  }
}


# i = c("liom","vacant")
# x <- c(1,1)
# scenario = c("liomvac","othervac")
# lam <- matrix(rep(NA,20),nrow = 10,ncol = 2)
# big <- list()
#
#   for(a in 1:7){ ## year
#     for(m in 1:10){ ## iter
#       for(n in 1:length(i)){ ## input info
#       lam[m,n] <- lambda(bigmatrix.2(params[m,],lower,upper,matsize,scenario[n],grow_rfx1[m,a],grow_rfx2[m,a],grow_rfx3[m,a],grow_rfx4[m,a],surv_rfx1[m,a],surv_rfx2[m,a],surv_rfx3[m,a],surv_rfx4[m,a],flow_rfx[m,a],repro_rfx[m,a],viab_rfx1[m,a],viab_rfx2[m,a],viab_rfx3[m,a],viab_rfx4[m,a])$IPMmat)
#     }
#   }
#   big[[a]] <- lam
#
# }
# lam
# big

#################################################################################################
###################################### THREE ANTS ###############################################
#################################################################################################

bigmatrix.3 <- function(params,lower,upper,matsize,scenario,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4){
  ###################################################################################################
  ## returns the full IPM kernel (to be used in stochastic simulation), the F and T kernels, and meshpoints in the units of size
  ## params,yrfx,plotfx, and mwye get passed to the vital rate functions
  ## f.eps is fertility overdispersion. defaults to zero (see lambda.fun())
  ## lower and upper are the integration limits
  ## matsize is the dimension of the approximating matrix (it gets an additional 2 rows and columns for the seed banks)
  ###################################################################################################
  n<-matsize
  L<-lower - floor
  U<-upper + ceiling
  h<-(U-L)/n                   #Bin size
  b<-L+c(0:n)*h;               #Lower boundaries of bins
  y<-0.5*(b[1:n]+b[2:(n+1)]);  #Bin midpoints

  # Fertility matricies -- Two Ant
  Fmat <- matrix(0,(3*n+2),(3*n+2))
  # Growth/survival transition matricies -- Two Ant
  Tmat <- matrix(0,(3*n+2),(3*n+2))
  ## Full Matricies
  IPMmat <- matrix()
  ############################################# LIOM & CREM & VAC ###############################################
  if(scenario == "liomcremvac"){
    ## Fecundity of plants
    Fmat[1,3:(n+2)]<-fx(y,params=params,"crem",flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4) ## Production of seeds from x sized mom with no ant visitor
    Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"liom",flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4) ## Production of seeds from x sized mom with ant visitor
    Fmat[1,(2*n+3):(3*n+2)]<-fx(y,params=params,"vacant",flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4)
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit((params$germ1_beta0))
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-0
    Tmat[(n+3):(2*n+2),1]<-0
    Tmat[(2*n+3):(3*n+2),1]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-0
    Tmat[(n+3):(2*n+2),2]<-0
    Tmat[(2*n+3):(3*n+2),2]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
    # Growth/survival transitions among cts sizes
    ##Top Row
    Tmat[3:(n+2),3:(n+2)]<- (t(outer(y,y,pxy,"crem",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "crem",j = "crem",params,"liomcremvac")) ## Top First
    Tmat[3:(n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,"liom",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "liom",j = "crem",params,"liomcremvac"))   ## Top Second
    Tmat[3:(n+2),(2*n+3):(3*n+2)]<-(t(outer(y,y,pxy,"vacant",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "vacant",j = "crem",params,"liomcremvac"))   ## Top Third
    ##Middle Row
    Tmat[(n+3):(2*n+2),3:(n+2)]<- (t(outer(y,y,pxy,"crem",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "crem",j = "liom",params,"liomcremvac"))   ## Middle First
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- (t(outer(y,y,pxy,"liom",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "liom",j = "liom",params,"liomcremvac"))   ## Middle Second
    Tmat[(n+3):(2*n+2),(2*n+3):(3*n+2)]<- (t(outer(y,y,pxy,"vacant",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "vacant",j = "liom",params,"liomcremvac"))   ## Middle Third
    ##Bottom Row
    Tmat[(2*n+3):(3*n+2),3:(n+2)]<- (t(outer(y,y,pxy,"crem",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "crem",j = "vacant",params,"liomcremvac"))   ## Bottom First
    Tmat[(2*n+3):(3*n+2),(n+3):(2*n+2)]<- (t(outer(y,y,pxy,"liom",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "liom",j = "vacant",params,"liomcremvac"))   ## Bottom Second
    Tmat[(2*n+3):(3*n+2),(2*n+3):(3*n+2)]<- (t(outer(y,y,pxy,"vacant",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "vacant",j = "vacant",params,"liomcremvac"))   ## Bottom Third
    # Put it all together
    colSums(Tmat[3:(3*n+2),3:(3*n+2)])
    IPMmat<-Fmat+Tmat
    # lambda = Re(eigen(IPMmat)$values[1])
    # return(lambda)
    return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
  }
  ############################################ LIOM & OTHER & VAC ###########################################
  if(scenario == "liomvacother"){
    ## Fecundity of plants
    Fmat[1,3:(n+2)]<-fx(y,params=params,"vacant",flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4) ## Production of seeds from x sized mom with no ant visitor
    Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"liom",flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4) ## Production of seeds from x sized mom with ant visitor
    Fmat[1,(2*n+3):(3*n+2)]<-fx(y,params=params,"other",flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4)
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit((params$germ1_beta0))
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
    Tmat[(n+3):(2*n+2),1]<-0
    Tmat[(2*n+3):(3*n+2),1]<-0
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
    Tmat[(n+3):(2*n+2),2]<-0
    Tmat[(2*n+3):(3*n+2),2]<-0
    # Growth/survival transitions among cts sizes
    ##Top Row
    ## vacant-vacant
    Tmat[3:(n+2),3:(n+2)]<- (t(outer(y,y,pxy,"vacant",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "vacant",j = "vacant",params,"liomvacother"))   ## Top First
    #liom-vacant
    Tmat[3:(n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,"liom",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "liom",j = "vacant",params,"liomvacother"))   ## Top Second
    #other-vacant
    Tmat[3:(n+2),(2*n+3):(3*n+2)]<-(t(outer(y,y,pxy,"other",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "other",j = "vacant",params,"liomvacother"))   ## Top Third
    ##Middle Row
    #vacant-liom
    Tmat[(n+3):(2*n+2),3:(n+2)]<- (t(outer(y,y,pxy,"vacant",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "vacant",j = "liom",params,"liomvacother"))   ## Middle First
    #liom-liom
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- (t(outer(y,y,pxy,"liom",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "liom",j = "liom",params,"liomvacother"))   ## Middle Second
    #other-liom
    Tmat[(n+3):(2*n+2),(2*n+3):(3*n+2)]<- (t(outer(y,y,pxy,"other",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "other",j = "liom",params,"liomvacother"))   ## Middle Third
    ##Bottom Row
    #vacant-other
    Tmat[(2*n+3):(3*n+2),3:(n+2)]<- (t(outer(y,y,pxy,"vacant",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "vacant",j = "other",params,"liomvacother"))   ## Bottom First
    #liom-other
    Tmat[(2*n+3):(3*n+2),(n+3):(2*n+2)]<- (t(outer(y,y,pxy,"liom",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "liom",j = "other",params,"liomvacother"))   ## Bottom Second
    #other-other
    Tmat[(2*n+3):(3*n+2),(2*n+3):(3*n+2)]<- (t(outer(y,y,pxy,"other",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "other",j = "other",params,"liomvacother"))   ## Bottom Third
    # Put it all together
    colSums(Tmat[3:(3*n+2),3:(3*n+2)])
    IPMmat<-Fmat+Tmat
    # lambda = Re(eigen(IPMmat)$values[1])
    # return(lambda)
    return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
  }
  ############################################# CREM & OTHER & VAC ############################################
  if(scenario == "othercremvac"){
    ## Fecundity of plants
    Fmat[1,3:(n+2)]<-fx(y,params=params,"crem",flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4) ## Production of seeds from x sized mom with no ant visitor
    Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"vacant",flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4) ## Production of seeds from x sized mom with ant visitor
    Fmat[1,(2*n+3):(3*n+2)]<-fx(y,params=params,"other",flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4)
    # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
    Tmat[2,1]<-1-invlogit((params$germ1_beta0))
    # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),1]<-0
    Tmat[(n+3):(2*n+2),1]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
    Tmat[(2*n+3):(3*n+2),1]<-0
    # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
    Tmat[3:(n+2),2]<-0
    Tmat[(n+3):(2*n+2),2]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
    Tmat[(2*n+3):(3*n+2),2]<-0
    # Growth/survival transitions among cts sizes
    ##Top Row
    Tmat[3:(n+2),3:(n+2)]<- (t(outer(y,y,pxy,"crem",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "crem",j = "crem",params,"othercremvac"))   ## Top First
    Tmat[3:(n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,"vacant",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "vacant",j = "crem",params,"othercremvac"))   ## Top Second
    Tmat[3:(n+2),(2*n+3):(3*n+2)]<-(t(outer(y,y,pxy,"other",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "other",j = "crem",params,"othercremvac"))   ## Top Third
    ##Middle Row
    Tmat[(n+3):(2*n+2),3:(n+2)]<- (t(outer(y,y,pxy,"crem",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "crem",j = "vacant",params,"othercremvac"))   ## Middle First
    Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<- (t(outer(y,y,pxy,"vacant",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "vacant",j = "vacant",params,"othercremvac"))   ## Middle Second
    Tmat[(n+3):(2*n+2),(2*n+3):(3*n+2)]<- (t(outer(y,y,pxy,"other",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "other",j = "vacant",params,"othercremvac"))   ## Middle Third
    ##Bottom Row
    Tmat[(2*n+3):(3*n+2),3:(n+2)]<- (t(outer(y,y,pxy,"crem",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "crem",j = "other",params,"othercremvac"))   ## Bottom First
    Tmat[(2*n+3):(3*n+2),(n+3):(2*n+2)]<- (t(outer(y,y,pxy,"vacant",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "vacant",j = "other",params,"othercremvac"))   ## Bottom Second
    Tmat[(2*n+3):(3*n+2),(2*n+3):(3*n+2)]<- (t(outer(y,y,pxy,"other",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "other",j = "other",params,"othercremvac"))   ## Bottom Third
    # Put it all together
    colSums(Tmat[3:(3*n+2),3:(3*n+2)])
    IPMmat<-Fmat+Tmat
    # lambda = Re(eigen(IPMmat)$values[1])
    # return(lambda)
    return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
  }
}
# # Scenario options are "liomvacother", "liomcremother", "liomcremvac", "othercremvac"
# i = c("liom","vacant")
# x <- c(1,1)
# scenario = c("liomvacother","liomcremvac")
# lam <- matrix(rep(NA,10*length(i)),nrow = 10,ncol = length(i))
# big <- list()
# for(a in 1:2){ ## year
#   for(m in 1:10){ ## iter
#     for(n in 1:length(i)){ ## input info
#       lam[m,n] <- lambda(bigmatrix.3(params[m,],lower,upper,matsize,scenario[n],grow_rfx1[m,a],grow_rfx2[m,a],grow_rfx3[m,a],grow_rfx4[m,a],surv_rfx1[m,a],surv_rfx2[m,a],surv_rfx3[m,a],surv_rfx4[m,a],flow_rfx[m,a],repro_rfx[m,a],viab_rfx1[m,a],viab_rfx2[m,a],viab_rfx3[m,a],viab_rfx4[m,a])$IPMmat)
#     }
#   }
#   big[[a]] <- lam
# }
# lam
# big


##################################################################################################
######################################### ALL ANTS PRESENT #######################################
##################################################################################################
bigmatrix.4 <- function(params,lower,upper,matsize,scenario,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4){
  ###################################################################################################
  ## returns the full IPM kernel (to be used in stochastic simulation), the F and T kernels, and meshpoints in the units of size
  ## params,yrfx,plotfx, and mwye get passed to the vital rate functions
  ## f.eps is fertility overdispersion. defaults to zero (see lambda.fun())
  ## lower and upper are the integration limits
  ## matsize is the dimension of the approximating matrix (it gets an additional 2 rows and columns for the seed banks)
  ###################################################################################################
  n<-matsize
  L<-lower - floor
  U<-upper + ceiling
  h<-(U-L)/n                   #Bin size
  b<-L+c(0:n)*h;               #Lower boundaries of bins
  y<-0.5*(b[1:n]+b[2:(n+1)]);  #Bin midpoints

  # Fertility matricies -- Two Ant
  Fmat <- matrix(0,(4*n+2),(4*n+2))
  # Growth/survival transition matricies -- Two Ant
  Tmat <- matrix(0,(4*n+2),(4*n+2))
  ## Full Matricies
  IPMmat <- matrix()
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  #Tmat[2,1]<-1-1
  Tmat[2,1]<-1-invlogit((params$germ1_beta0))
  ############################################# LIOM & CREM & OTHER & VAC ############################################
  ## Fecundity of plants
  Fmat[1,3:(n+2)]<-fx(y,params=params,"crem",flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4) ## Production of seeds from x sized mom with no ant visitor
  Fmat[1,(n+3):(2*n+2)]<-fx(y,params=params,"liom",flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4) ## Production of seeds from x sized mom with ant visitor
  Fmat[1,(2*n+3):(3*n+2)]<-fx(y,params=params,"other",flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4)
  Fmat[1,(3*n+3):(4*n+2)]<-fx(y,params=params,"vacant",flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4)
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),1]<-0
  Tmat[(n+3):(2*n+2),1]<-0
  Tmat[(2*n+3):(3*n+2),1]<-0
  Tmat[(3*n+3):(4*n+2),1]<-invlogit((params$germ1_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),2]<-0
  Tmat[(n+3):(2*n+2),2]<-0
  Tmat[(2*n+3):(3*n+2),2]<-0
  Tmat[(3*n+3):(4*n+2),1]<-invlogit((params$germ2_beta0))*recruits(y,params)*h*invlogit((params$preseed_beta0))
  # Growth/survival transitions among cts sizes
  ##Top Row
  Tmat[3:(n+2),3:(n+2)]<- (t(outer(y,y,pxy,"crem",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "crem",j = "crem",params,"all"))   ## Top First
  Tmat[3:(n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,"liom",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "liom",j = "crem",params,"all"))   ## Top Second
  Tmat[3:(n+2),(2*n+3):(3*n+2)]<-(t(outer(y,y,pxy,"other",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "other",j = "crem",params,"all"))   ## Top Third
  Tmat[3:(n+2),(3*n+3):(4*n+2)]<-(t(outer(y,y,pxy,"vacant",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "vacant",j = "crem",params,"all"))
  ##Second Row
  Tmat[(n+3):(2*n+2),3:(n+2)]<- (t(outer(y,y,pxy,"crem",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "crem",j = "liom",params,"all"))   ## Top First
  Tmat[(n+3):(2*n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,"liom",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "liom",j = "liom",params,"all"))   ## Top Second
  Tmat[(n+3):(2*n+2),(2*n+3):(3*n+2)]<-(t(outer(y,y,pxy,"other",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "other",j = "liom",params,"all"))   ## Top Third
  Tmat[(n+3):(2*n+2),(3*n+3):(4*n+2)]<-(t(outer(y,y,pxy,"vacant",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "vacant",j = "liom",params,"all"))
  ##Third Row
  Tmat[(2*n+3):(3*n+2),3:(n+2)]<- (t(outer(y,y,pxy,"crem",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "crem",j = "other",params,"all"))   ## Top First
  Tmat[(2*n+3):(3*n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,"liom",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "liom",j = "other",params,"all"))   ## Top Second
  Tmat[(2*n+3):(3*n+2),(2*n+3):(3*n+2)]<-(t(outer(y,y,pxy,"other",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "other",j = "other",params,"all"))   ## Top Third
  Tmat[(2*n+3):(3*n+2),(3*n+3):(4*n+2)]<-(t(outer(y,y,pxy,"vacant",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "vacant",j = "other",params,"all"))
  ##Bottom Row
  Tmat[(3*n+3):(4*n+2),3:(n+2)]<- (t(outer(y,y,pxy,"crem",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "crem",j = "vacant",params,"all"))   ## Top First
  Tmat[(3*n+3):(4*n+2),(n+3):(2*n+2)]<-(t(outer(y,y,pxy,"liom",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "liom",j = "vacant",params,"all"))   ## Top Second
  Tmat[(3*n+3):(4*n+2),(2*n+3):(3*n+2)]<-(t(outer(y,y,pxy,"other",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "other",j = "vacant",params,"all"))   ## Top Third
  Tmat[(3*n+3):(4*n+2),(3*n+3):(4*n+2)]<-(t(outer(y,y,pxy,"vacant",params,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4))*h)%*%diag(transition.x(y,i = "vacant",j = "vacant",params,"all"))
  # Put it all together
  colSums(Tmat[3:(4*n+2),3:(4*n+2)])
  colSums(Tmat[3:(4*n+2),3:(4*n+2)])[1000:1200]
  IPMmat<-Fmat+Tmat
  # lambda = Re(eigen(IPMmat)$values[1])
  # return(lambda)
  return(list(IPMmat = IPMmat, Tmat = Tmat, Fmat = Fmat))
}

# i = c("liom","vacant")
# x <- c(1,1)
# scenario = c("all","all")
# lam <- matrix(rep(NA,10*length(i)),nrow = 10,ncol = length(i))
# big <- list()
# for(a in 1:17){ ## year
#   for(m in 1:10){ ## iter
#     for(n in 1:length(i)){ ## input info
#       lam[m,n] <- lambda(bigmatrix.4(params[m,],lower,upper,matsize,scenario[n],grow_rfx1[m,a],grow_rfx2[m,a],grow_rfx3[m,a],grow_rfx4[m,a],surv_rfx1[m,a],surv_rfx2[m,a],surv_rfx3[m,a],surv_rfx4[m,a],flow_rfx[m,a],repro_rfx[m,a],viab_rfx1[m,a],viab_rfx2[m,a],viab_rfx3[m,a],viab_rfx4[m,a])$IPMmat)
#     }
#   }
#   big[[a]] <- lam
# }
# lam
# big

#################################################################################################
############################ CHOOSE WHICH SCENARIO (COMBO OF ANTS) ##############################
#################################################################################################

bigmatrix<-function(params,lower,upper,matsize,scenario,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4){
  ###################################################################################################
  ## returns the full IPM kernel (to be used in stochastic simulation), the F and T kernels, and meshpoints in the units of size
  ## params,yrfx,plotfx, and mwye get passed to the vital rate functions
  ## f.eps is fertility overdispersion. defaults to zero (see lambda.fun())
  ## lower and upper are the integration limits
  ## matsize is the dimension of the approximating matrix (it gets an additional 2 rows and columns for the seed banks)
  ###################################################################################################
  ## Scenario options are "liomvacother", "liomcremvac", "othercremvac",
  ## "othervac", "liomvac", "cremvac", "all", "none"

  if(scenario == "none"){
    list = (bigmatrix.1(params,lower,upper,matsize,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4))
    return(list)
  }
  if(scenario == "liomvac"){
    list = (bigmatrix.2(params,lower,upper,matsize,scenario,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4))
    return(list)
  }
  if(scenario == "cremvac"){
    list = (bigmatrix.2(params,lower,upper,matsize,scenario,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4))
    return(list)
  }
  if(scenario == "othervac"){
    list = (bigmatrix.2(params,lower,upper,matsize,scenario,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4))
    return(list)
  }
  if(scenario == "liomvacother"){
    list = (bigmatrix.3(params,lower,upper,matsize,scenario,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4))
    return(list)
  }
  if(scenario == "liomcremvac"){
    list = (bigmatrix.3(params,lower,upper,matsize,scenario,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4))
    return(list)
  }
  if(scenario == "othercremvac"){
    list = (bigmatrix.3(params,lower,upper,matsize,scenario,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4))
    return(list)
  }
  if(scenario == "all"){
    list = (bigmatrix.4(params,lower,upper,matsize,scenario,grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4,surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4,flow_rfx,repro_rfx,viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4))
    return(list)
  }
}
# i = c("liom","vacant","vacant","crem")
# x <- c(1,1,1,1)
# scenario = c("all","all","none","liomcremvac")
# lam <- matrix(rep(NA,3),nrow = 3,ncol = 4)
# big <- list()
# for(a in 1:2){ ## year
#   for(m in 1:1){ ## iter
#     for(n in 1:length(i)){ ## input info
#       lam[m,n] <- lambda(bigmatrix(params[m,],lower,upper,matsize,scenario[n],grow_rfx1[m,a],grow_rfx2[m,a],grow_rfx3[m,a],grow_rfx4[m,a],surv_rfx1[m,a],surv_rfx2[m,a],surv_rfx3[m,a],surv_rfx4[m,a],flow_rfx[m,a],repro_rfx[m,a],viab_rfx1[m,a],viab_rfx2[m,a],viab_rfx3[m,a],viab_rfx4[m,a])$IPMmat)
#     }
#   }
#   big[[a]] <- lam
# }
# lam
# big


# ## One ant option
# lambda(bigmatrix(params[14,], lower, upper, matsize, "none",grow_rfx1=0,grow_rfx2=0,grow_rfx3=0,grow_rfx4=0,surv_rfx1=0,surv_rfx2=0,surv_rfx3=0,surv_rfx4=0,flow_rfx=0,repro_rfx=0,viab_rfx1=0,viab_rfx2=0,viab_rfx3=0,viab_rfx4=0)$IPMmat)
# ## 2 ant options
# lambda(bigmatrix(params[14,],lower,upper,matsize,"cremvac",grow_rfx1=0,grow_rfx2=0,grow_rfx3=0,grow_rfx4=0,surv_rfx1=0,surv_rfx2=0,surv_rfx3=0,surv_rfx4=0,flow_rfx=0,repro_rfx=0,viab_rfx1=0,viab_rfx2=0,viab_rfx3=0,viab_rfx4=0)$IPMmat)
# lambda(bigmatrix(params[14,],lower,upper,matsize,"liomvac",grow_rfx1=0,grow_rfx2=0,grow_rfx3=0,grow_rfx4=0,surv_rfx1=0,surv_rfx2=0,surv_rfx3=0,surv_rfx4=0,flow_rfx=0,repro_rfx=0,viab_rfx1=0,viab_rfx2=0,viab_rfx3=0,viab_rfx4=0)$IPMmat)
# lambda(bigmatrix(params[14,],lower,upper,matsize,"othervac",grow_rfx1=0,grow_rfx2=0,grow_rfx3=0,grow_rfx4=0,surv_rfx1=0,surv_rfx2=0,surv_rfx3=0,surv_rfx4=0,flow_rfx=0,repro_rfx=0,viab_rfx1=0,viab_rfx2=0,viab_rfx3=0,viab_rfx4=0)$IPMmat)
# ## 3 ant options
# lambda(bigmatrix(params[14,],lower,upper,matsize,"liomcremvac",grow_rfx1=0,grow_rfx2=0,grow_rfx3=0,grow_rfx4=0,surv_rfx1=0,surv_rfx2=0,surv_rfx3=0,surv_rfx4=0,flow_rfx=0,repro_rfx=0,viab_rfx1=0,viab_rfx2=0,viab_rfx3=0,viab_rfx4=0)$IPMmat)
# lambda(bigmatrix(params[14,],lower,upper,matsize,"liomvacother",grow_rfx1=0,grow_rfx2=0,grow_rfx3=0,grow_rfx4=0,surv_rfx1=0,surv_rfx2=0,surv_rfx3=0,surv_rfx4=0,flow_rfx=0,repro_rfx=0,viab_rfx1=0,viab_rfx2=0,viab_rfx3=0,viab_rfx4=0)$IPMmat)
# lambda(bigmatrix(params[14,],lower,upper,matsize,"othercremvac",grow_rfx1=0,grow_rfx2=0,grow_rfx3=0,grow_rfx4=0,surv_rfx1=0,surv_rfx2=0,surv_rfx3=0,surv_rfx4=0,flow_rfx=0,repro_rfx=0,viab_rfx1=0,viab_rfx2=0,viab_rfx3=0,viab_rfx4=0)$IPMmat)
# ## all ant options
# lambda(bigmatrix(params[14,],lower,upper,matsize,"all",grow_rfx1=0,grow_rfx2=0,grow_rfx3=0,grow_rfx4=0,surv_rfx1=0,surv_rfx2=0,surv_rfx3=0,surv_rfx4=0,flow_rfx=0,repro_rfx=0,viab_rfx1=0,viab_rfx2=0,viab_rfx3=0,viab_rfx4=0)$IPMmat)


#########################################################################################################
# lambdaS Simulations for different Years Rands #########################################################
# m indicates the number of iterations from MCMC chains #################################################
# n indicates the diversity scenario ####################################################################
# For every iteration included there should be a single lambda value estimated ##########################
# For every diversity scenario analyzed there should be a posterior distribution of lambdas estimated ###
#########################################################################################################
lambdaSim=function(params,                                  ## parameters
                   grow_rfx1,grow_rfx2,grow_rfx3,grow_rfx4, ## growth model year rfx
                   surv_rfx1,surv_rfx2,surv_rfx3,surv_rfx4, ## survival model year rfx
                   flow_rfx,                                ## flower model year rfx
                   repro_rfx,                               ## repro model year rfx
                   viab_rfx1,viab_rfx2,viab_rfx3,viab_rfx4, ## viability model year rfx
                   max_yrs,                                 ## the # years you want to iterate
                   matsize,                                 ## size of transition matrix
                   scenario,                                ## partner diversity scenario
                   lower,upper                              ## extensions to avoid eviction
){

  ## Create an actual matrix filled with 0 of the right size based on scenarios
  if(scenario == "none"){K_t <- matrix(0,matsize+2,matsize+2)}
  if(scenario == "cremvac"|scenario == "liomvac"|scenario == "othervac"){K_t <- matrix(0,2*matsize+2,2*matsize+2)}
  if(scenario == "liomcremvac"|scenario == "liomvacother"|scenario == "othercremvac"){K_t <- matrix(0,3*matsize+2,3*matsize+2)}
  if(scenario == "all"){K_t <- matrix(0,4*matsize+2,4*matsize+2)}
  matdim        <- ncol(K_t)
  rtracker      <- (rep(0,max_yrs))  ## Empty vector to store growth rates in
  n0            <- rep(1/matdim,matdim)  ## Create dummy initial growth rate vector that sums to 1
  #
  for(t in 1:max_yrs){ ## In this loop I call the IPMmat and store it in the K_t matrix then
    #   ## scale this to the stochastic growth rate
    #   ## Randomly sample the years we have data for by calling column r in all matricies of
    #   ## the year random effects
    r <- sample(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),1,replace = TRUE,prob = NULL)

    ## Create and store matrix
    K_t[,]<-bigmatrix(params,lower,upper,matsize,scenario,
                      grow_rfx1[r],grow_rfx2[r],grow_rfx3[r],grow_rfx4[r],
                      surv_rfx1[r],surv_rfx2[r],surv_rfx3[r],surv_rfx4[r],
                      flow_rfx[r],
                      repro_rfx[r],
                      viab_rfx1[r],viab_rfx2[r],viab_rfx3[r],viab_rfx4[r])$IPMmat
    ## At each time step call the IPM for the proper Year
    #
    n0 <- K_t[,] %*% n0 ## This is a vector of population structure. Numerical trick to keep pop sizes managable
    N  <- sum(n0) ## This gives the growth rate of the population
    rtracker[t]<-log(N) ## Store the growth rate for each year in the r tracker vector?
    n0 <-n0/N ## Update scaling for next iteration
  }
  #
  #discard initial values (to get rid of transient)
  burnin    <- round(max_yrs*0.1)
  rtracker  <- rtracker[-c(1:burnin)]

  #Finish and return
  #print(proc.time() - ptm)
  lambdaS<-exp(mean(rtracker))
  return(lambdaS)
}

# scenario = c("none","cremvac","liomvac","othervac","liomcremvac","liomvacother","othercremvac","all")
# max_scenario = length(scenario) #n
# max_yrs = 100
# max_rep = 1
# lam <- matrix(data = NA, nrow = max_rep, ncol = max_scenario)
# for(n in 1:max_scenario){
#   print(scenario[n])
#   for(m in 1:max_rep){
#     lam[m,n] <- lambdaSim(params = params[m,],
#                           grow_rfx1 = grow_rfx1[m,],
#                           grow_rfx2 = grow_rfx2[m,],
#                           grow_rfx3 = grow_rfx3[m,],
#                           grow_rfx4 = grow_rfx4[m,],
#                           surv_rfx1 = surv_rfx1[m,],
#                           surv_rfx2 = surv_rfx2[m,],
#                           surv_rfx3 = surv_rfx3[m,],
#                           surv_rfx4 = surv_rfx4[m,],
#                           flow_rfx = flow_rfx[m,],
#                           repro_rfx = repro_rfx[m,],
#                           viab_rfx1 = viab_rfx1[m,],
#                           viab_rfx2 = viab_rfx2[m,],
#                           viab_rfx3 = viab_rfx3[m,],
#                           viab_rfx4 = viab_rfx4[m,],
#                           max_yrs,
#                           matsize = 400,
#                           scenario = scenario[n],
#                           lower,
#                           upper
#     )
#   }
# }
# lam


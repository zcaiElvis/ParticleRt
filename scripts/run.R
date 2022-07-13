library("tidyverse")
library("dplyr")
library("zoo")
library("pomp")

source("functions/run_pfilter.R")
source("functions/r_process.R")
source("functions/d_measure.R")
source("scripts/read_data.R")
source("constants/file_loc.R")



# covid <- read_data(covid_file_loc)
# covid$y[700:745] <- rep(10, 46)
# covid <- covid[1:600,]
covid<- read_owid(covid_file_loc, inter = c(30:900))

### Normal unconstraint ###
normal_filter <- run_pfilter(rprocess_config = tran_normal_const, dmeasure_config = meas_pois, data = covid, Np=300)
normal_result <- as.data.frame(normal_filter)
plot(normal_result$pred.mean.x)

### Normal constraint ###
normal_un_filter <- run_pfilter(rprocess_config = tran_normal_unconst, dmeasure_config = meas_pois, data = covid, Np=300)
normal_un_result <- as.data.frame(normal_un_filter)
plot(normal_un_result$pred.mean.x)
plot(normal_un_result$ess)

### Normal epifilter style ###
normal_epifilter <- run_pfilter(rprocess_config = tran_normal_epifilter, dmeasure_config = meas_pois, data = covid, Np=1000)
epifilter_result <- as.data.frame(normal_epifilter)
plot(epifilter_result$pred.mean.x)

### Log normal ###
lnormal_filter <- run_pfilter(rprocess_config = tran_lognormal, dmeasure_config = meas_pois, 
                              data=covid, Np=100, x_init = 1, verbose= FALSE)
lnormal_result <- as.data.frame(lnormal_filter)
logLik(lnormal_filter)
plot(lnormal_result$pred.mean.x)


### Simulated normal random walk ###
sim_rw <- gen_normal_rw(500)
sim_x <- sim_rw$x
sim_rw <- data.frame(idx = 1:nrow(sim_rw), y = sim_rw$y)

normal_sim <- run_pfilter(rprocess_config = tran_normal_unconst, dmeasure_config = meas_norm, data = sim_rw, Np=1000)
normal_sim_result <- as.data.frame(normal_sim)
plot(normal_sim_result$pred.mean.x)

plot(sim_x, normal_sim_result$pred.mean.x)




### Simulated covid ###
sim_covid <- gen_lnormal_pois_rw(size = 1000, sd_log = 0.02, init_x = 1, g_shape = 2, g_scale = 2)
plot(sim_covid$y)
sim_covid_x <- sim_covid$x
sim_covid <- data.frame(idx = sim_covid$idx, y = sim_covid$y)

covid_sim <- run_pfilter(rprocess_config = tran_lognormal, dmeasure_config = meas_pois_lnorm, data = sim_covid, Np=1000)
covid_sim_result <- as.data.frame(covid_sim)


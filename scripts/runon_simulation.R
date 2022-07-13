source("functions/simulate_data.R")
source("functions/run_pfilter.R")

data_rt <- gen_lnormal_pois_rw(1000, 0.02, 1, 2, 2)
plot(data_rt$x)

data_rt %>%
  pivot_longer(cols = c(x, y))%>%
  ggplot(aes(x=idx, y = value))+
  geom_line()+
  facet_wrap(vars(name), scales = "free")

simulate <- run_pfilter(rprocess_config = tran_lognormal, dmeasure_config = meas_pois_lnorm,
                        data = select(data_rt, c("idx", "y")), Np = 1, verbose=FALSE)

simulate_result <- as.data.frame(simulate)
plot(simulate_result$pred.mean.x)


sim_lambda <- simulate_result$pred.mean.x

y_recon <- rep(1, length(sim_lambda))
y_recon[1] <- 1
w <- disc_gamma(1:length(sim_lambda))
for(q in 2:length(sim_lambda)){
  y_recon[q] <- sim_lambda[q-1]*sum(y_recon[(q-1):1]*w[1:(q-1)])
}
plot(y_recon)
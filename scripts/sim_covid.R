source("functions/disc_gamma.R")



#### Simulate Transition ####
sim_size <- 1000
sim_rt <- rep(0, sim_size)
sim_rt[1] <- 1


for(m in 2:sim_size){
  sim_rt[m] <- rlnorm(1, meanlog = log(sim_rt[m-1]), sdlog = 0.05)
}
plot(sim_rt)



#### Simulate Case Count ####
sim_i <- rep(0, sim_size)
g_shape <- 2
g_scale <- 2
sim_i[1] <- 5

rand_spike <- c(200, 400, 600, 800)

for(m in 2:sim_size){
  sim_lamb <- sim_rt[m]*sum(sim_i[(m-1):1]*disc_gamma(1:(m-1), shape = g_shape, scale = g_scale))
  sim_i[m] <- rpois(1, lambda=sim_lamb)
  if(m %in% rand_spike){
    sim_i[m] <- 1000
  }
  # sim_i[m] <- rnorm(1, sim_lamb, sqrt(sim_lamb))

}

plot(sim_i)


### Get R_t back ###
pred_rt <- rep(0, sim_size)
pred_rt[1] <- 1
for(m in 2:sim_size){
  pred_rt[m] <- sim_i[m]/sum(sim_i[(m-1):1]*disc_gamma(1:(m-1), shape = g_shape, scale= g_scale))
}

plot(pred_rt)

library("tidyverse")
library("dplyr")
library("nimble")
library("nimbleSMC")
library("zoo")


### Read data ###

covid_data <- read.csv("data/case_death.csv", header=TRUE)
covid_data <- covid_data %>%
  filter(Country_code == "CA") %>%
  select(c("Date_reported", "New_cases", "Cumulative_cases", "New_deaths", "Cumulative_deaths"))

ca <- rollmean(covid_data$New_cases,7)[1:700]


### Particle filtering ###

R_t_prediction <- nimbleCode({
  # Initial values & Priors
  r0 ~ dunif(1, 3)
  sigma ~ dunif(1, 10)
  
  r[1] ~ dnorm(r0, 1/sigma)
  i[1] ~ dpois(r[1])
  
  for(t in 2:700){
    r[t] ~ dnorm(r[t-1], 1/sigma)
    i[t] ~ dpois(r[t]*sum(i[(t-1):1]*dgamma(1:(t-1), shape= 2.3669, scale= 2.7463)))
  }
})


model <- nimbleModel(code = R_t_prediction, data <-list(i = ca))
model$r0 <- 1
model$sigma <- 1

cmodel <- compileNimble(model)

# model$simulate(nodes = model$getNodeNames(includeData = FALSE))


bsf <- buildBootstrapFilter(model = model, "r")
cbs <- compileNimble(bsf, project = model)

cbs



## FUNCTION: check i[date] and lambda = \Sum I*w
check_pois_rate <- function(date, shape=2.3669, scale = 2.7463){
  lambda <- sum(ca[(date-1):1]*dgamma(1:(date-1), shape=shape, scale= scale))
  return(c(ca[date], lambda))
}

## FUNCTION: check possibility 
# <- change this, because prob of it taking 1 value is really small
# <- change to, prob of "around" that value
check_psb <- function(pr){
  return(dpois(x=round(pr[1]), lambda=pr[2]))
}

## FUNCTION get list of possibilities
get_lop <- function(from, to){
  output<- rep(0, to - from)
  for(j in from:to){
    output[j] <- check_psb(check_pois_rate(j))
  }
  return(output)
}



### Doesn't work because:
### 1. Example: taking case[101:400], sum(i[1:399]*lambda(399:1, shape=2.36, scale=2.74)) is 4600,
###   while case[399] is 2145. dpois(2145, 4600) = 0. because poisson has variance lambda, too small
###   So it makes r as big as possible, to allow more variance, leading to diverging.

### Solution: overdispersed poisson or negbin | gamma shape and scale parameter changes
### *variance of poisson and R_t absorbed together


### 2. Need to start from time point 1. or else sum Iw is not right



### Mock example showing nimble works


gen_rw <- function(len, sd){
  x <- rep(0, len)
  y <- rep(0, len)
  x[1] = 1
  y[1] = 1

  for(i in 2:len){
    x[i] <- rnorm(1, x[i-1], sd)
    y[i] <- rpois(1, lambda=exp(x[i]))
  }

  return(cbind(x,y))
}


mock_len <- 200
tr_sd <- 0.2

gen_seq <- gen_rw(mock_len, tr_sd)
gen_x <- gen_seq[,1]
gen_y <- gen_seq[,2]

plot(gen_x)




### Two ways:
# 1. Compute R_t directly with particle filter

# 2. Compute R_t using Kalman filter, p and q from kalman filter 
# computed with particle filter




R_t_prediction <- nimbleCode({

  r0 ~ dunif(0.5, 5)
  
  r[1] ~ dnorm(r0, 100)
  i[1] ~ dpois(r[1])
  
  for(t in 2:5){
    r[t] ~ dnorm(r[t-1], 1/sigma)
    sum_i_w <- sum(i[(t-1):1]*dgamma(1:(t-1), shape= 2.3669, scale= 2.7463))
    i[t] ~ dpois(r[t]*sum_i_w)
  }
})


model <- nimbleModel(code = R_t_prediction, data=list(i = ca[1:5]), 
                     inits = list(r0 = 1, sigma = 1))
# model$plotGraph()
# model$simulate(nodes = model$getNodeNames(includeData = FALSE))

model$simulate("r")










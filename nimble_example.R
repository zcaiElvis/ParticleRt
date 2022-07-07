library(nimble)
library(nimbleSMC)




### Create example ###

t <- 25; mu_0 <- 1
x <- rnorm(1 ,mu_0, 0.1)
y <- rnorm(1, x, 1)

for(i in 2:t){
  x[i] <- rnorm(1, x[i-1], 1)
  y[i] <- rpois(1, lambda = abs(x[i]))
}


### Build Nimble Model

timeModelCode <- nimbleCode({
  # Initial values
  x[1] ~ dnorm(mu_0, 1)
  y[1] ~ dpois(abs(x[1]))
  
  # Create models
  for(i in 2:t){
    x[i] ~ dnorm(x[i-1], 1)
    y[i] ~ dpois(abs(x[i]))
  }
  
  # Prior for parameters, initial mu_0
  mu_0 ~ dnorm(0, 1)
})



### Compile/Run model

rTimeModel <- nimbleModel(timeModelCode, constants = list(t = t),
                          data <- list(y = y), check = FALSE)


rTimeModel$mu_0 <- 1

cTimeModel <- compileNimble(rTimeModel)
rBootF <- buildBootstrapFilter(rTimeModel, "x",
                               control = list(thresh = 0.8, saveAll = TRUE,
                                              smoothing = FALSE))
cBootF <- compileNimble(rBootF,project = rTimeModel)

parNum <- 5000
bootLLEst <- cBootF$run(parNum)

bootESS <- cBootF$returnESS()
bootEWSamp <- as.matrix(cBootF$mvEWSamples)


y_pred <- apply(bootEWSamp, MARGIN = 2, FUN = mean)




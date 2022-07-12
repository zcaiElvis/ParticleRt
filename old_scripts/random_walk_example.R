library(nimble)
library(nimbleSMC)


gen_rw <- function(len, sd){
  x <- rep(0, len)
  y <- rep(0, len)
  x[1] = 1
  y[1] = 1
  
  for(i in 2:len){
    x[i] <- rnorm(1, x[i-1], sd)
    
    while(x[i] < 0){
      x[i] <- rnorm(1, x[i-1], sd)
    }
    y[i] <- rpois(1, lambda=x[i])
  }
  
  return(cbind(x,y))
}

ss <- gen_rw(20, 5)
x <- ss[,1]
y <- ss[,2]


rw_recovery <- nimbleCode({
  # Initial values
  x0 ~ dunif(0.5, 5)
  y0 ~ dpois(x0)
  
  # Build chain
  for(i in 2:len){
    x[i] ~ dnorm(x[i-1], 1)
    y[i] ~ dpois(abs(x[i]))
  }
})

rTimeModel <- nimbleModel(code = rw_recovery, data = list(y = y), inits = list(x0 = 1, y0 = 1),  constants = list(len=length(y)))

cTimeModel <- compileNimble(rTimeModel)



rBootF <- buildBootstrapFilter(rTimeModel, "x")

cBootF <- compileNimble(rBootF,project = rTimeModel)

parNum <- 5000

bootLLEst <- cBootF$run(200)

bootESS <- cBootF$returnESS()


bootEWSamp <- as.matrix(cBootF$mvEWSamples)






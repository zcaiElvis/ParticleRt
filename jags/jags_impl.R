require(rjags)
require(MCMCvis)


### Read Data
alldata = read.csv('data/WHO-COVID-19-global-data.csv')
idcountry = which(alldata$Country == 'New Zealand')
Iday = alldata$New_cases[idcountry]
covid_nz <- data.frame(y = Iday, idx = 1:length(Iday))

w <- disc_gamma(1:nrow(covid_nz), shape = 2.3669, scale = 2.7463)

### Jags scripts doesnt support opposite indexing y[n:1]
w <- w[length(w):1]
###


model.loc <- "jags/models/jags_model.txt"

jagsscript <- cat("
  model {  

  # Modeling
  rt[1] ~ dnorm(1, 10)
  y[1] ~ dpois(rt[1])
  
  for (t in 2:N) {
    rt[t] ~ dnorm(rt[t-1], 1/(0.1*sqrt(rt[t-1])))
    lambda[t] <- sum(y[1:(t-1)]*(w[1:(t-1)]/sum(w[1:(t-1)])))
    y[t] ~ dpois(rt[t]*lambda[t])
  }
}  
",  file = model.loc)



jags.data <- list("y" = covid_nz$y, "N" = nrow(covid_nz), "w" = w)
mod <- jags.model(file = model.loc, data=jags.data)

run_jag <- coda.samples(model, c("rt"), n.iter=100000)


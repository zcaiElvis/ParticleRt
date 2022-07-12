library("tidyverse")
library("dplyr")
library("zoo")
library("pomp")

### Read Data ###
covid_data <- read.csv("data/case_death.csv", header=TRUE)
covid_data <- covid_data %>%
  filter(Country_code == "CA") %>%
  select(c("Date_reported", "New_cases")) %>%
  mutate(y = New_cases)

covid_data <- covid_data[50:nrow(covid_data)-5,]
covid_data$y <- rollmean(covid_data$y, k = 7, fill =NA)
covid_data <- na.omit(covid_data)
covid_data$y <- round(covid_data$y)
covid_data$idx <- 1:nrow(covid_data)


covid_data_small <- covid_data[1:800,]

### Build Plop ###
covid_data_small |>
  pomp(
    times="idx", t0 = 1,
    rinit = function(x0, ...){
      c(x = x0)
    },
    rprocess = discrete_time(
      function(x, sigma1, ...){
        xnext <- rnorm(1, mean=x, sd = sigma1)
        while(xnext < 0){
          xnext <- rnorm(1, mean=x, sd = sigma1)
        }
        c(x = xnext)
      },
      delta.t = 1
    ),
    
    rmeasure = function(x, Y, T, t, ...){
      lambda = x*sum(Y[t-1:1] * dgamma(1:t-1, shape = 2.8, scale = 2.6))
      ynext <- rpois(lambda = lambda)
      c(y = ynext)
    },
    
    Y = covid_data_small$New_cases,
    
    T = t,
    
    statenames = "x",
    
    paramnames = c("sigma1", "x0"),
    
    verbose = TRUE
    
  ) -> pomp_covid

pomp_covid |>
  pfilter(
    Np = 1000,
    params = c(sigma1 = 0.3, x0=1),
    dmeasure = function(y, x, sigma1, t, T, Y, ... , log){
      lambda = x*sum(Y[t-1:1] * dgamma(1:t-1, shape = 2.5, scale = 3))
      dpois(y, lambda = lambda)
    },
    T = t,
    Y = covid_data_small$New_cases,
    statenames = "x",
    pred.mean = TRUE,
    pred.var = TRUE,
    filter.mean = TRUE,
    filter.traj = TRUE,
    verbose=FALSE
  ) -> filter_covid


covid_result <- as.data.frame(filter_covid)

compare_case_rt <- data.frame(idx = 1:nrow(covid_data_small), case=covid_data_small$New_cases, rt <- covid_result$pred.mean.x)

compare_case_rt %>%
  pivot_longer(cols=c(2,3))%>%
  ggplot(aes(x = idx))+
  geom_line(aes(y=value))+
  facet_wrap(facets = vars(name), scales="free_y")



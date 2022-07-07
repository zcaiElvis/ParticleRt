library("tidyverse")
library("dplyr")
library("zoo")
library("pomp")

### Read Data ###
covid_data <- read.csv("data/case_death.csv", header=TRUE)
covid_data <- covid_data %>%
  filter(Country_code == "CA") %>%
  # select(c("Date_reported", "New_cases")) %>%
  select(c("New_cases")) %>%
  mutate(y = New_cases)

covid_data <- covid_data[50:nrow(covid_data)-5,]
covid_data$y <- rollmean(covid_data$y, k = 7, fill =NA)
covid_data <- na.omit(covid_data)
covid_data$y <- round(covid_data$y)
covid_data$idx <- 1:nrow(covid_data)



covid_simple<- pfilter(
  Np = 1000,
  times = "idx",
  t0 = 1,
  data = covid_data,
  rinit = function(x0, ...){
    c(x = x0)
  },
  
  ### Plugging in initial values here
  params = c(sigma1 = 1, x0=1.5),
  
  ### rprocess ###
  rprocess = discrete_time(
    function(x, sigma1, ...){
      xnext <- rnorm(1, x, sd = 0.2)
      while(xnext < 0.5 || xnext > 5 ){
        xnext <- rnorm(1, x, sd = 0.2)
      }
      c(x = xnext)
    },
    delta.t = 1
  ),
  
  ### dmeasure ###
  dmeasure = function (t, x, T, Y, y, ..., log) {
    lambda = x*sum(Y[t-1:1] * dgamma(1:t-1, shape = 2, scale = 2))
    dpois(y, lambda = lambda)
  },
  T = t, Y = covid_data$New_cases,
  statenames = "x",
  pred.mean = TRUE,
  pred.var = TRUE,
  filter.mean = TRUE,
  filter.traj = TRUE,
  verbose=FALSE
)

est_rt <- get_est_Rt(case = covid_data$New_cases)

plot_case_Rt(covid_data, data.frame(pred.mean.x = est_rt))



result_covid_simple <- as.data.frame(covid_simple)

plot(result_covid_simple$pred.mean.x)

plot_case_Rt(covid_data, result_covid_simple)

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


l1 <- sum(covid_data$New_cases[nrow(covid_data):1] * dgamma(1:nrow(covid_data), shape=2, scale=2))
l2 <- sum(covid_data$New_cases[nrow(covid_data):1] * dgamma(1:nrow(covid_data), shape=2.1, scale=2.1))



ggplot(data=data.frame(g1 = dgamma(seq(1,20, 0.1), shape = 2, scale =2),
                       g2 = dgamma(seq(1,20, 0.1), shape = 2.1, scale = 2.1),
                       idx = seq(1,20, 0.1)
                       ), aes(x = idx))+
  geom_line(aes(x=idx, y = g1), colour = "blue")+
  geom_line(aes(x=idx, y = g2), colour = "red")


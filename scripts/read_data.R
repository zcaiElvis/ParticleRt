library("dplyr")
library("zoo")


read_data <- function(file_loc, country_code = "CA", selected_col = c("New_cases"), inter = c(50:800)){
  covid_data <- read.csv(file_loc, header=TRUE)
  covid_data <- covid_data %>%
    filter(Country_code == "CA") %>%
    select(selected_col) %>%
    mutate(y = New_cases)
  
  covid_data <- covid_data[inter,]
  covid_data$y <- rollmean(covid_data$y, k=7, fill = NA)
  covid_data <- na.omit(covid_data)
  covid_data$y <- round(covid_data$y)
  covid_data$idx <- 1:nrow(covid_data)
  
  return(covid_data)
}

read_owid <- function(file_loc, iso = "CAN", selected_col = c("new_cases"), inter = c(50:800)){
  covid_data<- read.csv(covid_file_loc, header=TRUE)
  covid_data <- covid_data%>%
    filter(iso_code==iso)%>%
    select(all_of(selected_col))%>%
    mutate(y=new_cases)
  
  covid_data <- covid_data[inter,]
  covid_data$y <- rollmean(covid_data$y, k=7, fill = NA)
  covid_data <- na.omit(covid_data)
  covid_data$y <- round(covid_data$y)
  covid_data$idx <- 1:nrow(covid_data)
  return(covid_data)
}

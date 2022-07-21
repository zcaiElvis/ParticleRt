library("dplyr")
library("zoo")


read_data <- function(file_loc, country_code = "CA", selected_col = c("New_cases"), inter = c(50:800)){
  covid_data <- read.csv(file_loc, header=TRUE)
  covid_data <- covid_data %>%
    filter(Country_code == country_code) %>%
    select(selected_col) %>%
    mutate(y = New_cases)
  
  covid_data <- covid_data[inter,]
  covid_data$y <- rollmean(covid_data$y, k=7, fill = NA)
  covid_data <- na.omit(covid_data)
  covid_data$y <- round(covid_data$y)
  covid_data$idx <- 1:nrow(covid_data)
  
  return(covid_data)
}

read_owid <- function(file_loc, iso = "CAN", selected_col = c("new_cases", "date"), inter = c(50:800)){
  covid_data<- read.csv(covid_file_loc, header=TRUE)
  covid_data <- covid_data%>%
    filter(iso_code==iso)%>%
    select(all_of(selected_col))%>%
    mutate(y=new_cases)
  
  covid_data <- covid_data[inter,]
  covid_data$y <- rollmean(covid_data$y, k=7, fill = NA)
  # covid_data <- na.omit(covid_data)
  covid_data$y <- round(covid_data$y)
  covid_data$idx <- 1:nrow(covid_data)
  return(covid_data)
}


read_owid_mob <- function(file_loc, iso = "CAN", selected_col = c("new_cases", "date")){
  covid_data<- read.csv(covid_file_loc, header=TRUE)
  head(covid_data)
  covid_data <- covid_data%>%
    filter(iso_code==iso)%>%
    select(all_of(selected_col))%>%
    mutate(y=new_cases)
  
  covid_data$y <- rollmean(covid_data$y, k=7, fill = NA)
  covid_data<- na.locf(covid_data)
  covid_data$y <- round(covid_data$y)
  covid_data$idx <- 1:nrow(covid_data)
  return(covid_data)
}


read_who<- function(country, smooth=FALSE, k = 7){
  alldata = read.csv('data/WHO-COVID-19-global-data.csv')
  idcountry = which(alldata$Country == country)
  Iday = alldata$New_cases[idcountry]
  if(smooth){
    Iday <- rollmean(Iday, k=k, fill=NA)
    Iday <- na.omit(Iday)
  }
  covid <- data.frame(y = round(Iday), idx = 1:length(Iday))
  return(covid)
}

library("dplyr")
library("tidyverse")
library("zoo")
library("pomp")

### Import required packages ###
source("functions/d_measure.R")
source("functions/r_process.R")
source("functions/run_pfilter.R")
source("scripts/read_data.R")
source("constants/file_loc.R")


### Get data ###
covid <- read_owid_mob(covid_file_loc)
mob <- read.csv(mob_file_loc, header=TRUE)

covid <- covid%>%
  mutate(date_format = as.Date(date)) %>%
  subset(date_format >= "2020-03-01" & date_format <= "2022-05-27")
  

plot(mob$parks_percent_change_from_baseline, covid$new_cases)

fit <- lm(covid$y ~mob$parks_percent_change_from_baseline)

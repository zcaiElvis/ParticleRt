library(testthat)
library(extraDistr)

source("constants/file_loc.R")

covid <- read_owid(covid_file_loc)


### Testing discrete gamma distribution ###


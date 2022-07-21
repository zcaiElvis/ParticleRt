library(testthat)
library(extraDistr)


source("constants/file_loc.R")
source("functions/disc_gamma.R")
source("functions/get_renewal.R")
source("scripts/read_data.R")
source("functions/pred_case.R")

covid <- read_owid(covid_file_loc)


### Testing discrete gamma distribution ###
w <- disc_gamma(1:20, shape = 2, scale = 2)
test_that("Discrete gamma multiplies incidence length correct", {
  expect_equal(length(covid$y[(20-1):1]*w[1:(20-1)]), 19)
})


### Testing renewal equation
test_that("Renewal equation gives the correct answer",{
  w <- disc_gamma(1:50, scale = 2, shape = 2)
  fun_output <- get_renewal(y=covid$y[1:50], w = w)
  cal_output <- sum(covid$y[49:1]*w[1:49] )
  expect_equal(fun_output, cal_output)
})


### Testing prediction function
# test_that("Predict function gives the right answer",{
#   test_rt <- c(1.2,1.5,1.7,1.9)
#   test_y <- c(1,2,2,2)
#   
#   lambda <- get_lambda(test_y, shape = 1, scale = 1)
#   
#   
# })

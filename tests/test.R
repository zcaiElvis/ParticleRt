library(testthat)
library(extraDistr)

source("constants/file_loc.R")

covid <- read_owid(covid_file_loc)


### Testing discrete gamma distribution ###
w <- disc_gamma(1:20, shape = 2, scale = 2)
test_that("Discrete gamma multiplies incidence length correct", {
  expect_equal(length(covid$y[(20-1):1]*w[1:(20-1)]), 19)
})

library(testthat)
campaign_data <- readRDS("Data/campaign_data_example.RDS")
getwd()
source("onboarding-analysis/acceptance_rate.R")

test_that("check function for calculating acceptance rate", {
  acceptance_rate <- calculate_acceptance_rate(campaign_data, "students")
  
  # Test that the result is numeric
  expect_type(acceptance_rate, "double")
  
  # Test that the result is between 0 and 1 (valid proportion)
  expect_gte(acceptance_rate, 0)
  expect_lte(acceptance_rate, 1)
  
  # Test that the result is not NA
  expect_false(is.na(acceptance_rate))
})
library(testthat)
source("Functions/testDuration.R")

test_that("test_duration returns correct average duration by company size", {
  
  # Load campaign data
  campaign_data <- readRDS("Data/campaign_data_example.RDS")
  
  # Test with small companies
  small_company_avg <- test_duration(campaign_data, "small")
  big_company_avg <- test_duration(campaign_data, "big")
  
  # Expect that the function returns numeric values
  expect_type(small_company_avg, "double")
  expect_type(big_company_avg, "double")
  
  # Expect non-negative durations (can't have negative campaign duration)
  expect_gte(small_company_avg, 0)
  expect_gte(big_company_avg, 0)
  
  # Expect that big companies have higher average duration than small companies
  expect_gt(big_company_avg, small_company_avg)
  
  # Expect reasonable duration values (not impossibly high)
  expect_lt(small_company_avg, 365)  # Less than a year
  expect_lt(big_company_avg, 365)    # Less than a year
})
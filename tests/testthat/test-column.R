library(testthat)
library(here)
# Load campaign data for testing
campaign_data <- readRDS("Data/campaign_data_example.RDS")
getwd()
print(campaign_data)

test_that("Campaign data loads successfully", {
  expect_true(file.exists(here::here("Data", "campaign_data_example.RDS")))
  expect_s3_class(campaign_data, "data.frame")
  expect_gt(nrow(campaign_data), 0)
  expect_gt(ncol(campaign_data), 0)
})

test_that("Campaign data has required column structure", {
  # Test that data frame has essential columns for business hypothesis testing
  column_names <- names(campaign_data)
  
  # Check that we have some form of identifier columns
  expect_true(any(grepl("id|ID|identifier", column_names, ignore.case = TRUE)))
  
  # Check for target audience indicators (for Hypothesis 1: Student offers)
  expect_true(any(grepl("student|audience|target", column_names, ignore.case = TRUE)))
  
  # Check for company size data (for Hypotheses 2 & 3)
  expect_true(any(grepl("company|size|employee", column_names, ignore.case = TRUE)))
  
  # Check for a start_date (for Hypothesis 2)
  expect_true(any(grepl("duration|Start_Date|campaign", column_names, ignore.case = TRUE)))
  
  # Check for offer acceptance/closing data (for Hypotheses 1 & 4)
  expect_true(any(grepl("accept|close|Offer_Status|result", column_names, ignore.case = TRUE)))
})

test_that("Campaign data types are appropriate", {
  # Check that we don't have all character columns (suggests data loading issues)
  column_types <- sapply(campaign_data, class)
  expect_false(all(column_types == "character"))
  
  # Check for reasonable mix of data types
  expect_true(any(column_types %in% c("numeric", "integer")))
  expect_true(any(column_types %in% c("character", "factor")))
})

test_that("Campaign data has no completely missing columns", {
  # Check that no column is entirely NA
  for (col_name in names(campaign_data)) {
    expect_false(all(is.na(campaign_data[[col_name]])), 
                 info = paste("Column", col_name, "is entirely missing"))
  }
})

test_that("Campaign data dimensions are reasonable", {
  # Check minimum data requirements for statistical analysis
  expect_gte(nrow(campaign_data), 30, 
            info = "Need at least 30 rows for meaningful statistical analysis")
  expect_gte(ncol(campaign_data), 5, 
            info = "Need at least 5 columns for hypothesis testing")
  expect_lte(ncol(campaign_data), 100, 
            info = "Too many columns might indicate data structure issues")
})

test_that("Campaign data has reasonable value ranges", {
  numeric_cols <- names(campaign_data)[sapply(campaign_data, is.numeric)]
  
  if (length(numeric_cols) > 0) {
    for (col in numeric_cols) {
      # Check for reasonable numeric ranges (no extreme outliers that suggest data errors)
      col_data <- campaign_data[[col]][!is.na(campaign_data[[col]])]
      if (length(col_data) > 0) {
        expect_true(all(is.finite(col_data)), 
                   info = paste("Column", col, "contains infinite values"))
        
        # Check that values aren't suspiciously large (potential data loading errors)
        expect_lt(max(col_data), 1e10, 
                 info = paste("Column", col, "has suspiciously large values"))
      }
    }
  }
})

test_that("Campaign data categorical variables have reasonable levels", {
  factor_cols <- names(campaign_data)[sapply(campaign_data, function(x) is.factor(x) || is.character(x))]
  
  if (length(factor_cols) > 0) {
    for (col in factor_cols) {
      unique_vals <- unique(campaign_data[[col]][!is.na(campaign_data[[col]])])
      
      # Check that categorical variables don't have too many unique values
      # (might indicate ID columns being treated as categorical)
      expect_lte(length(unique_vals), nrow(campaign_data) * 0.5, 
                info = paste("Column", col, "has too many unique values for a categorical variable"))
      
      # Check that categorical variables have at least 2 levels for analysis
      expect_gte(length(unique_vals), 2, 
                info = paste("Column", col, "needs at least 2 categories for analysis"))
    }
  }
})

test_that("Campaign data supports business hypothesis requirements", {
  # Test for sufficient data to support each hypothesis
  
  # Hypothesis 1: Student offers acceptance rates
  # Need at least 2 groups (student vs non-student) with sufficient sample sizes
  if (any(grepl("student|audience|target", names(campaign_data), ignore.case = TRUE))) {
    target_col <- names(campaign_data)[grepl("student|audience|target", names(campaign_data), ignore.case = TRUE)][1]
    target_groups <- table(campaign_data[[target_col]], useNA = "ifany")
    expect_gte(length(target_groups), 2, 
              info = "Need at least 2 target audience groups for Hypothesis 1")
    expect_true(all(target_groups >= 5), 
               info = "Each target group needs at least 5 observations")
  }
  
  # Hypothesis 4: Time trend analysis
  # Need temporal data with sufficient time points
  date_cols <- names(campaign_data)[sapply(campaign_data, function(x) 
    inherits(x, "Date") || inherits(x, "POSIXt") || 
    any(grepl("date|time", names(campaign_data), ignore.case = TRUE)))]
  
  if (length(date_cols) > 0) {
    # Check for reasonable time range
    time_col <- date_cols[1]
    if (inherits(campaign_data[[time_col]], c("Date", "POSIXt"))) {
      time_range <- range(campaign_data[[time_col]], na.rm = TRUE)
      expect_gt(as.numeric(diff(time_range)), 0, 
               info = "Need time variation for trend analysis")
    }
  }
})

test_that("Campaign data is ready for statistical analysis", {
  # Check for basic data quality requirements
  
  # No duplicate rows (unless expected)
  expect_lte(sum(duplicated(campaign_data)), nrow(campaign_data) * 0.1, 
            info = "Too many duplicate rows might indicate data quality issues")
  
  # Reasonable missing data percentage
  missing_percentage <- sum(is.na(campaign_data)) / (nrow(campaign_data) * ncol(campaign_data))
  expect_lt(missing_percentage, 0.5, 
           info = "More than 50% missing data will hinder analysis")
  
  # Check that we have variation in the data (not all identical values)
  expect_gt(length(unique(as.matrix(campaign_data))), 1, 
           info = "Data needs variation for meaningful analysis")
})
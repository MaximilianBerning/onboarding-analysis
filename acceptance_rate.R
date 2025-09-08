# Issue 2: Student/Pupil Acceptance Rate Analysis
# Load required libraries
library(tidyverse)

# Load campaign data
campaign_data <- readRDS("Data/campaign_data_example.RDS")

#' Calculate Acceptance Rate by Target Group
#'
#' Filters campaign data for specific target audience and calculates 
#' acceptance rate based on Offer_Status (Won vs Lost).
#'
#' @param data Campaign dataset
#' @param target_group Target audience: "students" or "pupils"
#' @return Numeric acceptance rate (proportion of Won offers)
#'
calculate_acceptance_rate <- function(data, target_group) {
  
  acceptance_rate <- data %>%
    filter(target == target_group) %>%
    summarise(
      total_offers = n(),
      won_offers = sum(Offer_Status == "Won", na.rm = TRUE),
      acceptance_rate = won_offers / total_offers
    ) %>%
    pull(acceptance_rate)
  
  return(acceptance_rate)
}

# Example usage
students_rate <- calculate_acceptance_rate(campaign_data, "Students")
pupils_rate <- calculate_acceptance_rate(campaign_data, "pupils")

cat("Students acceptance rate:", students_rate, "\n")
cat("Pupils acceptance rate:", pupils_rate, "\n")
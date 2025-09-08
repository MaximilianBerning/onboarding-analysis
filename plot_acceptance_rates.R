# Plot Acceptance Rates - Students vs Pupils Analysis
# Load required libraries
library(tidyverse)
library(ggplot2)

# Source the acceptance rate function
source("onboarding-analysis/acceptance_rate.R")

# Load campaign data
campaign_data <- readRDS("Data/campaign_data_example.RDS")

# Calculate acceptance rates for both target groups
students_rate <- calculate_acceptance_rate(campaign_data, "Students")
pupils_rate <- calculate_acceptance_rate(campaign_data, "Pupils")

# Create data frame for plotting
acceptance_data <- data.frame(
  Target_Group = c("Students", "Pupils"),
  Acceptance_Rate = c(students_rate, pupils_rate)
)

# Convert to percentages for display
acceptance_data$Percentage <- acceptance_data$Acceptance_Rate * 100

# Create professional bar chart
acceptance_plot <- ggplot(acceptance_data, aes(x = Target_Group, y = Percentage, fill = Target_Group)) +
  geom_col(width = 0.6, alpha = 0.8) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  
  # Custom color scheme
  scale_fill_manual(values = c("Students" = "#2E86AB", "Pupils" = "#A23B72")) +
  
  # Styling and labels
  labs(
    title = "Campaign Acceptance Rates by Target Audience",
    subtitle = "Comparison of offer acceptance rates between Students and Pupils",
    x = "Target Audience",
    y = "Acceptance Rate (%)",
    fill = "Target Group"
  ) +
  
  # Professional theme
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray60"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  
  # Set y-axis limits with some padding
  ylim(0, max(acceptance_data$Percentage) * 1.15)

# Display the plot
print(acceptance_plot)

# Print summary statistics
cat("\n=== ACCEPTANCE RATE ANALYSIS ===\n")
cat("Students acceptance rate:", round(students_rate * 100, 2), "%\n")
cat("Pupils acceptance rate:", round(pupils_rate * 100, 2), "%\n")
cat("Difference:", round((students_rate - pupils_rate) * 100, 2), "percentage points\n")

# Optional: Save the plot
# ggsave("acceptance_rates_comparison.png", plot = acceptance_plot, 
#        width = 10, height = 6, dpi = 300, bg = "white")
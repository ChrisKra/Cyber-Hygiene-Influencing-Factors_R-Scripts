library(gtsummary)
library(dplyr)

# Create summary table grouped by dem3, excluding specific columns, ignoring "Unknown", adding means and p-values
summary_table <- survey_dim_min %>%
  filter(dem3 != "Unknown") %>%                # Exclude "Unknown" from dem3
  select(-survey_name, -id, -seed) %>%         # Exclude unwanted columns
  tbl_summary(
    by = dem3,                                 # Group by dem3
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",      # Mean and SD for continuous vars
      all_categorical() ~ "{n} ({p}%)"         # Count and percentage for categorical vars
    ),
    digits = all_continuous() ~ 2              # Two decimal points for continuous vars
  ) %>%
  add_n() %>%                                  # Add sample size (n)
  add_p()                                      # Add p-values comparing groups

# Print summary table
print(summary_table)

# Save the summary table as an HTML file
summary_table %>%
  as_gt() %>%
  gt::gtsave(filename = "de_vs_uk_table.html", path = "~/MA/03_output/reports")

# Remove object from workspace
rm(summary_table)
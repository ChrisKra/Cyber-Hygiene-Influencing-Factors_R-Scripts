library(dplyr)

survey_dim_min_split <- survey_dim_min
survey_dim_min_split <- survey_dim_min_split %>%
  mutate(
    exercise_avg = rowMeans(select(., cch01, cch02, cch03), na.rm = TRUE),
    exercise_performance = ifelse(exercise_avg > median(exercise_avg, na.rm = TRUE), "high", "low")
  )

# Create summary table grouped by dem3, excluding specific columns, ignoring "Unknown", adding means and p-values
summary_table <- survey_dim_min_split %>%
  select(-survey_name, -id, -seed, -group_time_exercise) %>%         # Exclude unwanted columns
  tbl_summary(
    by = exercise_performance,                                 # Group by dem3
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
  gt::gtsave(filename = "exerc_perf_split_table.html", path = "~/MA/03_output/reports")

# Remove object from workspace
rm(summary_table)
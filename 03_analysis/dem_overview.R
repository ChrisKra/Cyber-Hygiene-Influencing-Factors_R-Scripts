# Create the summary table
summary_table <- survey_min %>%
  select(dem1:dem13) %>%  # Exclude unwanted columns
  tbl_summary(
    statistic = all_continuous() ~ "{mean} ({sd})",
    # missing = "no",
    type = list(where(is.numeric) ~ "continuous")
  )

# Save the summary table as an HTML file
summary_table %>%
  as_gt() %>%
  gt::gtsave(filename = "demographics_table.html", path = "~/MA/03_output/reports")

rm(summary_table)
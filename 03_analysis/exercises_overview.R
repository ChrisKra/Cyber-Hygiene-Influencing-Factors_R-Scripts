# install.packages("gtsummary")
library(gtsummary)

# Create the summary table
summary_table <- survey_min %>% 
  select(cch01_score, cch012_score_r, cch013_score, cch021_cch021_r,
         cch022_cch022, cch023_cch023_r, cch031_cch031_r, cch032_cch032_r, cch033_cch033) %>% 
  mutate(across(everything(), ~ifelse(is.na(.) | . %in% c(-99, -999), NA, as.numeric(.)))) %>%
  drop_na() %>%
  tbl_summary(
    type = list(everything() ~ "continuous"),
    statistic = list(everything() ~ "{mean} ({sd}), {median} [{min}, {max}]"),
    digits = list(everything() ~ 2)
  ) %>%
  add_n()

print(summary_table)

# Save the summary table as an HTML file
summary_table %>%
  as_gt() %>%
  gt::gtsave(filename = "exercises_table.html", path = "~/MA/03_output/reports")

rm(summary_table)
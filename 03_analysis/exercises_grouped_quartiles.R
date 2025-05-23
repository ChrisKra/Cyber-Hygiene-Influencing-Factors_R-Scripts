library(gtsummary)
library(dplyr)
library(gt)

# Function to add significance stars
add_significance_stars <- function(x) {
  dplyr::case_when(
    x < 0.001 ~ paste0("<mark>", style_pvalue(x), "***</mark>"),
    x < 0.01 ~ paste0("<mark>", style_pvalue(x), "**</mark>"),
    x < 0.05 ~ paste0("<mark>", style_pvalue(x), "*</mark>"),
    TRUE ~ style_pvalue(x)
  )
}

summary_table <- survey_dim_min %>%
  mutate(cch02_scaled = cch02 * 20,
         cch03_scaled = cch03 * 20,
         cch_mean_score = rowMeans(across(c(cch01, cch02_scaled, cch03_scaled)), na.rm = TRUE)) %>%
  mutate(cch_group = cut(cch_mean_score, 
                         breaks = quantile(cch_mean_score, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = TRUE), 
                         labels = c("0-25", "25-50", "50-75", "75-100"), 
                         include.lowest = TRUE)) %>%
  select(-survey_name, -id, -seed) %>%  # Exclude unwanted columns
  tbl_summary(
    by = cch_group,  # Group by the new categorical variable
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing = "no",
    type = list(where(is.numeric) ~ "continuous")
  ) %>%
  add_p(pvalue_fun = add_significance_stars)

print(summary_table)

summary_table %>%
  as_gt() %>%
  gt::gtsave(filename = "exercises_quartiles_table.html", path = "~/MA/03_output/reports")

rm(add_significance_stars, summary_table)
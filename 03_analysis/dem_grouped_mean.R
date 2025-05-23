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

# ALL ITEMS
# Loop through dem1 to dem13
for (i in 1:13) {
  # Construct the dynamic column name (e.g., dem1, dem2, ..., dem13)
  dem_col <- paste0("dem", i)
  
  # Create the summary table
  summary_table <- survey_min %>%
    select(-survey_name, -id, -seed, -g01q48, -dem14) %>%  # Exclude unwanted columns
    tbl_summary(
      by = !!sym(dem_col),  # Use dynamic column name
      statistic = all_continuous() ~ "{mean} ({sd})",
      missing = "no",
      type = list(where(is.numeric) ~ "continuous")
    ) %>%
    add_p(pvalue_fun = add_significance_stars)
  
  # Save the summary table as an HTML file
  summary_table %>%
    as_gt() %>%
    gt::gtsave(filename = paste0(dem_col, "_table.html"), path = "~/MA/03_output/reports")
}

# Remove object from workspace
rm(summary_table, dem_col, i, add_significance_stars)

# CONSTRUCTS ONLY
# Loop through dem1 to dem13
for (i in 1:13) {
  # Construct the dynamic column name (e.g., dem1, dem2, ..., dem13)
  dem_col <- paste0("dem", i)
  
  # Create the summary table
  summary_table <- survey_dim_min %>%
    select(-survey_name, -id, -seed) %>%  # Exclude unwanted columns
    tbl_summary(
      by = !!sym(dem_col),  # Use dynamic column name
      statistic = all_continuous() ~ "{mean} ({sd})",
      missing = "no",
      type = list(where(is.numeric) ~ "continuous")
    ) %>%
    add_p(pvalue_fun = add_significance_stars)
  
  # Save the summary table as an HTML file
  summary_table %>%
    as_gt() %>%
    gt::gtsave(filename = paste0(dem_col, "_min_table.html"), path = "~/MA/03_output/reports")
}

# Remove object from workspace
rm(summary_table, dem_col, i)

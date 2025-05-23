# Function to add significance stars
add_significance_stars <- function(x) {
  dplyr::case_when(
    x < 0.001 ~ paste0("<mark>", style_pvalue(x), "***</mark>"),
    x < 0.01 ~ paste0("<mark>", style_pvalue(x), "**</mark>"),
    x < 0.05 ~ paste0("<mark>", style_pvalue(x), "*</mark>"),
    TRUE ~ style_pvalue(x)
  )
}

# Full
event_list <- list(
  "pe_pe1" = "pe_1_p_account_breach_table.html",
  "pe_pe2" = "pe_2_a_account_hacked_table.html",
  "pe_pe3" = "pe_3_p_received_phishing_table.html",
  "pe_pe4" = "pe_4_a_clicked_phishing_table.html",
  "pe_pe5" = "pe_5_p_target_harassment_table.html",
  "pe_pe6" = "pe_6_a_account_blocked_table.html",
  "pe_pe7" = "pe_7_p_harassment_because_of_gender.html"
)

for (event in names(event_list)) {
  filename <- event_list[[event]]
  
  summary_table <- survey_min %>%
    select(-survey_name, -id, -seed, -g01q48, -dem14) %>%  # Exclude unwanted columns
    tbl_summary(
      by = !!sym(event),
      statistic = all_continuous() ~ "{mean} ({sd})",
      missing = "no",
      type = list(where(is.numeric) ~ "continuous")
    ) %>%
    add_p(pvalue_fun = add_significance_stars)
  
  print(summary_table)
  
  summary_table %>%
    as_gt() %>%
    gt::gtsave(filename = filename, path = "~/MA/03_output/reports")
}

# Min Version
event_list <- list(
  "pe_pe1" = "pe_1_p_min_account_breach_table.html",
  "pe_pe2" = "pe_2_a_min_account_hacked_table.html",
  "pe_pe3" = "pe_3_p_min_received_phishing_table.html",
  "pe_pe4" = "pe_4_a_min_clicked_phishing_table.html",
  "pe_pe5" = "pe_5_p_min_target_harassment_table.html",
  "pe_pe6" = "pe_6_a_min_account_blocked_table.html",
  "pe_pe7" = "pe_7_p_min_harassment_because_of_gender.html"
)

survey_dim_min_pe <- survey_dim_min
survey_dim_min_pe$pe_pe1 <- survey_min$pe_pe1
survey_dim_min_pe$pe_pe2 <- survey_min$pe_pe2
survey_dim_min_pe$pe_pe3 <- survey_min$pe_pe3
survey_dim_min_pe$pe_pe4 <- survey_min$pe_pe4
survey_dim_min_pe$pe_pe5 <- survey_min$pe_pe5
survey_dim_min_pe$pe_pe6 <- survey_min$pe_pe6
survey_dim_min_pe$pe_pe7 <- survey_min$pe_pe7


for (event in names(event_list)) {
  filename <- event_list[[event]]
  
  summary_table <- survey_dim_min_pe %>%
    select(-survey_name, -id, -seed) %>%  # Exclude unwanted columns
    tbl_summary(
      by = !!sym(event),
      statistic = all_continuous() ~ "{mean} ({sd})",
      missing = "no",
      type = list(where(is.numeric) ~ "continuous")
    ) %>%
    add_p(pvalue_fun = add_significance_stars)
  
  print(summary_table)
  
  summary_table %>%
    as_gt() %>%
    gt::gtsave(filename = filename, path = "~/MA/03_output/reports")
}

rm(summary_table, filename, event_list, add_significance_stars, survey_dim_min_pe, event)
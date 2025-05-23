# Function to add significance stars
add_significance_stars <- function(x) {
  dplyr::case_when(
    x < 0.001 ~ style_pvalue(x),
    x < 0.01 ~ style_pvalue(x),
    x < 0.05 ~ style_pvalue(x),
    TRUE ~ style_pvalue(x)
  )
}

# Min Version
event_list <- list(
  "pe_pe1" = "pe_1_p_min_account_breach_table.docx",
  "pe_pe2" = "pe_2_a_min_account_hacked_table.docx",
  "pe_pe3" = "pe_3_p_min_received_phishing_table.docx",
  "pe_pe4" = "pe_4_a_min_clicked_phishing_table.docx",
  "pe_pe5" = "pe_5_p_min_target_harassment_table.docx",
  "pe_pe6" = "pe_6_a_min_account_blocked_table.docx",
  "pe_pe7" = "pe_7_p_min_harassment_because_of_gender.docx"
)

label_list <- list(
  cch01 ~ "CCH_PW",
  cch02 ~ "CCH_PH",
  cch03 ~ "CCH_WS",
  big5_big1 ~ "BIG5_EXTRA",
  big5_big2 ~ "BIG5_AGREE",
  big5_big3 ~ "BIG5_CONSC",
  big5_big4 ~ "BIG5_NEURO",
  big5_big5 ~ "BIG5_OPENN",
  ra ~ "RA",
  fomo1 ~ "FOMO_S",
  fomo3 ~ "NS",
  fomo2 ~ "SMU",
  csm ~ "CSM",
  pv ~ "PV",
  pe ~ "PE",
  tue ~ "TUE",
  tud ~ "TUD",
  tr ~ "TR",
  cchk ~ "CCH_K",
  ccha ~ "CCH_A",
  cchb ~ "CCH_B",
  si ~ "SI",
  co ~ "CO",
  ori ~ "ORI",
  orh ~ "ORH",
  dem1 ~ "DEM1",
  dem2 ~ "DEM2",
  dem3 ~ "DEM3",
  dem4 ~ "DEM4",
  dem5 ~ "DEM5",
  dem6 ~ "DEM6",
  dem7 ~ "DEM7",
  dem8 ~ "DEM8",
  dem9 ~ "DEM9",
  dem10 ~ "DEM10",
  dem11 ~ "DEM11",
  dem12 ~ "DEM12",
  dem13 ~ "DEM13"
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
      type = list(where(is.numeric) ~ "continuous"),
      label = label_list
    ) %>%
    add_p(pvalue_fun = add_significance_stars)
  
  print(summary_table)
  
  summary_table %>%
    as_gt() %>%
    gt::gtsave(filename = filename, path = "~/MA/03_output/reports")
}

rm(summary_table, filename, event_list, add_significance_stars, survey_dim_min_pe, event)
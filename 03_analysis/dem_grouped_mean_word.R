library(gtsummary)
library(dplyr)
library(gt)

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

# Function to add significance stars
add_significance_stars <- function(x) {
  dplyr::case_when(
    x < 0.001 ~ style_pvalue(x),
    x < 0.01 ~ style_pvalue(x),
    x < 0.05 ~ style_pvalue(x),
    TRUE ~ style_pvalue(x)
  )
}

# CONSTRUCTS ONLY
# Loop through dem1 to dem13
for (i in 1:13) {
  # Construct the dynamic column name (e.g., dem1, dem2, ..., dem13)
  dem_col <- paste0("dem", i)
  
  # Create the summary table
  summary_table <- survey_dim_min %>%
    select(-survey_name, -id, -seed, -group_time_exercise) %>%  # Exclude unwanted columns
    tbl_summary(
      by = !!sym(dem_col),  # Use dynamic column name
      statistic = all_continuous() ~ "{mean} ({sd})",
      missing = "no",
      type = list(where(is.numeric) ~ "continuous"),
      label = purrr::keep(label_list, ~ rlang::as_name(rlang::f_lhs(.x)) != dem_col)
    ) %>%
    add_p(pvalue_fun = add_significance_stars)
  
  # Save the summary table as an HTML file
  summary_table %>%
    as_gt() %>%
    gt::gtsave(filename = paste0(dem_col, "_min_table.docx"), path = "~/MA/03_output/reports")
}
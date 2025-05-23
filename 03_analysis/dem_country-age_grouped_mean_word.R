library(MatchIt)
library(gtsummary)
###
# Uses stratified sampling to reduce bias and control for confounding between age and country #
###

# Choose dataset: options are "survey_dim_min" or "survey_min"
use_dataset <- "survey_dim_min"  # change to "survey_min" if needed
survey_data <- get(use_dataset)

# Set suffix for filenames depending on dataset
file_suffix <- ifelse(use_dataset == "survey_dim_min", "_min", "")

# Filter for NA and convert country to factor
survey_sample <- survey_data %>%
  filter(!is.na(dem3), !is.na(dem1))
survey_sample$dem3 <- factor(survey_sample$dem3)

# Matching
matched <- matchit(dem3 ~ dem1, exact = ~ dem1, data = survey_sample)
survey_sample <- match.data(matched)

# Collapse Age into Broader Bands
survey_sample$age_band <- dplyr::case_when(
  survey_sample$dem1 %in% c("younger than 20 years old", "20 - 25 years old", "26 - 30 years old") ~ "younger (≤ 30)",
  survey_sample$dem1 %in% c("31 - 35 years old", "36 - 40 years old", "41 - 45 years old", "46 - 50 years old") ~ "middle-aged (31–50)",
  survey_sample$dem1 %in% c("51 - 55 years old", "56 - 60 years old", "61 - 65 years old", "66 - 70 years old", "older than 70 years old") ~ "older (≥ 51)",
  TRUE ~ NA_character_
)

# Make it a factor with ordered levels
survey_sample$age_band <- factor(survey_sample$age_band, levels = c("younger (≤ 30)", "middle-aged (31–50)", "older (≥ 51)"))

# Function to add significance stars
add_significance_stars <- function(x) {
  dplyr::case_when(
    x < 0.001 ~ style_pvalue(x),
    x < 0.01 ~ style_pvalue(x),
    x < 0.05 ~ style_pvalue(x),
    TRUE ~ style_pvalue(x)
  )
}

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

# Columns to exclude
base_exclude <- c("survey_name", "id", "seed", "distance", "weights", "subclass")
extra_exclude <- if (use_dataset != "survey_dim_min") c("g01q48", "dem14", "group_time_exercise") else character(0)
exclude_vars <- c(base_exclude, extra_exclude)

# Age Band Summary Table
summary_table_age <- survey_sample %>% 
  select(-any_of(exclude_vars)) %>%  
  tbl_summary(
    by = age_band,
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing = "no",
    type = list(where(is.numeric) ~ "continuous"),
    label = label_list
  ) %>%
  add_p(pvalue_fun = add_significance_stars)

# Save Age Band Table
summary_table_age %>%
  as_gt() %>%
  gt::gtsave(
    filename = paste0("dem1_band_sample", file_suffix, "_table.docx"),
    path = "~/MA/03_output/reports"
  )

# Country Summary Table
summary_table_country <- survey_sample %>% 
  select(-any_of(exclude_vars)) %>%
  tbl_summary(
    by = dem3,
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing = "no",
    type = list(where(is.numeric) ~ "continuous"),
    label = purrr::keep(label_list, ~ rlang::as_name(rlang::f_lhs(.x)) != "dem3")
  ) %>%
  add_p(pvalue_fun = add_significance_stars)

# Save Country Table
summary_table_country %>%
  as_gt() %>%
  gt::gtsave(
    filename = paste0("dem3_sample", file_suffix, "_table.docx"),
    path = "~/MA/03_output/reports"
  )

# Clean up
rm(summary_table_age, summary_table_country, add_significance_stars, survey_sample, matched, file_suffix, use_dataset, base_exclude, extra_exclude, exclude_vars)

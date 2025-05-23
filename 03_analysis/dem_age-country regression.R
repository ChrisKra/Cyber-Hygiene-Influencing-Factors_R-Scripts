###
# Control for confounding between age and country automatically by including both in the linear regression #
###

survey_dim_min_age_lin <- survey_dim_min

# Convert Age Group to Midpoints
survey_dim_min_age_lin$age_midpoint <- dplyr::case_when(
  survey_dim_min_age_lin$dem1 == "younger than 20 years old" ~ 19,
  survey_dim_min_age_lin$dem1 == "20 - 25 years old" ~ 22.5,
  survey_dim_min_age_lin$dem1 == "26 - 30 years old" ~ 28,
  survey_dim_min_age_lin$dem1 == "31 - 35 years old" ~ 33,
  survey_dim_min_age_lin$dem1 == "36 - 40 years old" ~ 38,
  survey_dim_min_age_lin$dem1 == "41 - 45 years old" ~ 43,
  survey_dim_min_age_lin$dem1 == "46 - 50 years old" ~ 48,
  survey_dim_min_age_lin$dem1 == "51 - 55 years old" ~ 53,
  survey_dim_min_age_lin$dem1 == "56 - 60 years old" ~ 58,
  survey_dim_min_age_lin$dem1 == "61 - 65 years old" ~ 63,
  survey_dim_min_age_lin$dem1 == "66 - 70 years old" ~ 68,
  survey_dim_min_age_lin$dem1 == "older than 70 years old" ~ 75,
  TRUE ~ NA_real_
)

# DO LINEAR REGRESSION
summary(lm(cch01 ~ age_midpoint + dem3, data = survey_dim_min_age_lin))
summary(lm(cch02 ~ age_midpoint + dem3, data = survey_dim_min_age_lin))
summary(lm(cch03 ~ age_midpoint + dem3, data = survey_dim_min_age_lin))
summary(lm(csm ~ age_midpoint + dem3, data = survey_dim_min_age_lin))
summary(lm(cchk ~ age_midpoint + dem3, data = survey_dim_min_age_lin))
summary(lm(ccha ~ age_midpoint + dem3, data = survey_dim_min_age_lin))
summary(lm(cchb ~ age_midpoint + dem3, data = survey_dim_min_age_lin))

rm(survey_dim_min_age_lin)
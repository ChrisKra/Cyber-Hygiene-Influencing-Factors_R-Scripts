survey_duplicates <- survey_min %>%
  filter(!is.na(g01q48)) %>%  # Exclude NA values
  group_by(g01q48) %>%
  filter(n() > 1) %>%
  ungroup()

survey_duplicates %>%
  select(-id, -seed, -g01q48, -dem14) %>%  # Exclude unwanted columns
  tbl_summary(
    by = survey_name,
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing = "no",
    type = list(where(is.numeric) ~ "continuous")
  ) %>%
  add_p()

#data.frame(table(survey_duplicates$g01q48)) %>% filter(Freq > 1)

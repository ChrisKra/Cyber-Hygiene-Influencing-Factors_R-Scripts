# For Prior Experience aggregate columns into active and passive bahaviors and sum
survey <- survey %>%
  mutate(across(pe_pe1:pe_pe7, ~ ifelse(is.na(.), NA, ifelse(. == 1, "Yes", "No")))) %>%
  mutate(
    pe_passive = ifelse(rowSums(is.na(select(., pe_pe1, pe_pe3, pe_pe5))) > 0, NA, rowSums(select(., pe_pe1, pe_pe3, pe_pe5) == "Yes")),
    pe_active  = ifelse(rowSums(is.na(select(., pe_pe2, pe_pe4, pe_pe6))) > 0, NA, rowSums(select(., pe_pe2, pe_pe4, pe_pe6) == "Yes")),
    pe     = ifelse(is.na(pe_passive) | is.na(pe_active), NA, pe_passive + pe_active)
  ) %>%
  relocate(pe_passive, pe_active, pe, .after = pe_pe6)  # Moves new columns after pe_pe6
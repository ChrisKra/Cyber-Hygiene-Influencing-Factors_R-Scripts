# Define the constructs for cchk, ccha, and cchb
cchk <- constructs$cchk
ccha <- constructs$ccha
cchb <- constructs$cchb

# Initialize an empty list to store the gaps
gaps_list <- list()

# Loop through each pair of columns and calculate the gaps
for (i in seq_along(cchk)) {
  # Generate gap column names
  gap_name_ka <- paste0("knowledge_attitude_gap_cch", i)
  gap_name_kb <- paste0("knowledge_behavior_gap_cch", i)
  gap_name_ab <- paste0("attitude_behavior_gap_cch", i)
  
  # Calculate the gaps
  gaps_list[[gap_name_ka]] <- survey_min[[cchk[i]]] - survey_min[[ccha[i]]]
  gaps_list[[gap_name_kb]] <- survey_min[[cchk[i]]] - survey_min[[cchb[i]]]
  gaps_list[[gap_name_ab]] <- survey_min[[ccha[i]]] - survey_min[[cchb[i]]]
}

# Convert the list to a dataframe
gaps_df <- as.data.frame(gaps_list)

# Display the first few rows of the gaps dataframe
head(gaps_df)

# Calculate summary statistics for the gaps
summary(gaps_df)

# Loop through the dataframe in chunks of 3 columns
for (i in seq(1, ncol(gaps_df), by = 3)) {
  print(summary(gaps_df[, i:min(i + 3 - 1, ncol(gaps_df)), drop = FALSE]))
}

rm(i, gaps_df, gaps_list, ccha, cchb, cchk, gap_name_ab, gap_name_ka, gap_name_kb)

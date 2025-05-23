# CYBER SECURITY MEASURES
# Calculate column means, ignoring NA values
column_means <- colMeans(survey[, c(paste0("csb_csm", 1:10), paste0("csb2_csm", 11:18))], na.rm = TRUE)

# Calculate column medians, ignoring NA values
column_medians <- apply(survey[, c(paste0("csb_csm", 1:10), paste0("csb2_csm", 11:18))], 2, median, na.rm = TRUE)

# Calculate column standard deviations, ignoring NA values
column_sds <- apply(survey[, c(paste0("csb_csm", 1:10), paste0("csb2_csm", 11:18))], 2, sd, na.rm = TRUE)

# Calculate 25th and 75th percentiles, ignoring NA values
column_percentiles <- apply(survey[, c(paste0("csb_csm", 1:10), paste0("csb2_csm", 11:18))], 2, function(x) paste0(round(quantile(x, 0.25, na.rm = TRUE), 2), ", ", round(quantile(x, 0.75, na.rm = TRUE), 2)))

# Create a data frame with column names and statistics
df_column_stats <- data.frame(
  Column = names(column_means),
  Mean = column_means,
  Median = column_medians,
  SD = column_sds,
  `25th-75th Percentiles` = column_percentiles
)

# Sort the data frame by mean in descending order
sorted_df <- df_column_stats[order(-df_column_stats$Mean), ]

# Print the sorted data frame
view(sorted_df)

# Calculate column means and standard deviations for each group, ignoring NA values
mean_cchk <- colMeans(survey[, cols_cchk], na.rm = TRUE)
sd_cchk <- apply(survey[, cols_cchk], 2, sd, na.rm = TRUE)

mean_ccha <- colMeans(survey[, cols_ccha], na.rm = TRUE)
sd_ccha <- apply(survey[, cols_ccha], 2, sd, na.rm = TRUE)

mean_cchb <- colMeans(survey[, cols_cchb], na.rm = TRUE)
sd_cchb <- apply(survey[, cols_cchb], 2, sd, na.rm = TRUE)

# Format means with SD in brackets
format_stat <- function(mean, sd) {
  paste0(round(mean, 2), " (", round(sd, 2), ")")
}

# Combine into a single dataframe
df_means <- data.frame(
  Column = cols_cchk,
  Mean_CCHK = mapply(format_stat, mean_cchk, sd_cchk),
  Mean_CCHA = mapply(format_stat, mean_ccha, sd_ccha),
  Mean_CCHB = mapply(format_stat, mean_cchb, sd_cchb)
)

# Sort dataframe by Mean_CCHK descending
sorted_df_means <- df_means[order(-mean_cchk), ]

# Print the final sorted dataframe
view(sorted_df_means)


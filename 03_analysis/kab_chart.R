# Load required libraries
library(ggplot2)
library(ggbump)
library(dplyr)
library(tidyr)

# COMPREHENSIVE CYBER HYGIENE
# Define column groups
cols_cchk <- c("cchk_cch11", "cchk_cch21", "cchk_cch31", "cchk_cch41_r",
               "cchk_cch51_r", "cchk_cch61_r", "cchk_cch71", "cchk_cch81")

cols_ccha <- c("ccha_cch12_r", "ccha_cch22_r", "ccha_cch32", "ccha_cch42_r",
               "ccha_cch52_r", "ccha_cch62_r", "ccha_cch72_r", "ccha_cch82_r")

cols_cchb <- c("cchb_cch13", "cchb_cch23_r", "cchb_cch33_r", "cchb_cch43",
               "cchb_cch53", "cchb_cch63", "cchb_cch73_r", "cchb_cch83")

# Calculate column means for each group, ignoring NA values
Knowledge <- colMeans(survey[, cols_cchk], na.rm = TRUE)
Attitude <- colMeans(survey[, cols_ccha], na.rm = TRUE)
Behavior <- colMeans(survey[, cols_cchb], na.rm = TRUE)

# Combine into a single dataframe
df_means <- data.frame(
  Column = cols_cchk,
  Knowledge = Knowledge,
  Attitude = Attitude,
  Behavior = Behavior
)

# Sort dataframe by Knowledge descending
#sorted_df_means <- df_means[order(-df_means$Knowledge), ]

# Convert scores to ranks (higher scores get lower ranks)
df_long <- df_means %>%
  pivot_longer(cols = Knowledge:Behavior, names_to = "Category", values_to = "Score") %>%
  group_by(Category) %>%
  mutate(Rank = rank(-Score))  # Rank in descending order

# Reorder the Category factor explicitly (K, A, B)
df_long$Category <- factor(df_long$Category, levels = c("Knowledge", "Attitude", "Behavior"))

# Make sub-area names more friendly
df_long$Column <- sub("^cchk_", "", df_long$Column)  # Remove "cchk_"
df_long$Column <- substr(df_long$Column, 1, 4)       # Limit to the first 4 characters

# Create a new 'Legend' column with the code in [] and the full name
df_long$Legend <- case_when(
  df_long$Column == "cch1" ~ "[cch1] Using a strong password",
  df_long$Column == "cch2" ~ "[cch2] Clicking on links in emails from unknown senders",
  df_long$Column == "cch3" ~ "[cch3] Social media privacy settings",
  df_long$Column == "cch5" ~ "[cch5] Considering social media consequences",
  df_long$Column == "cch6" ~ "[cch6] Viruses on smartphones",
  df_long$Column == "cch4" ~ "[cch4] Entering information online",
  df_long$Column == "cch7" ~ "[cch7] Sending sensitive information via insecure Wi-Fi",
  df_long$Column == "cch8" ~ "[cch8] Reporting online incidents"
)

# Create the bump chart
ggplot(df_long, aes(x = Category, y = Score, color = Legend, group = Column)) +
  geom_bump(size = 2) +          # Bump lines
  geom_point(size = 6) +         # Large points on ranks
  geom_text(aes(label = Column), nudge_y = 0.07, fontface = "bold", size = 3) +  # Add labels
  scale_color_brewer(palette = "Paired") +  # Use a nice color palette
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  #scale_y_reverse() +  # Ensure Rank 1 is at the top
  labs(
    title = "Change in Cyber Hygiene across Knowledge, Attitude, and Behavior",
    x = "Dimension",
    y = "Mean Score", 
    color = "Sub-area"
  )

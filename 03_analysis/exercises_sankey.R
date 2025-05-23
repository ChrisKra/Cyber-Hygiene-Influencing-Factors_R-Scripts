# Load required libraries
library(ggplot2)
library(ggalluvial)
library(dplyr)
library(tidyr)

# PASSWORDS
# Select and reshape data
df_sankey2 <- survey_min %>%
  select(cch01_score, cch012_score_r, cch013_score) %>%
  drop_na() %>%
  mutate(id = row_number(),  # Create an identifier for individuals
         cch01_score_bin = cut(cch01_score, breaks = c(0, 20, 40, 60, 80, 100), labels = c("0-20", "21-40", "41-60", "61-80", "81-100"), include.lowest = TRUE),
         cch012_score_r_bin = cut(cch012_score_r, breaks = c(0, 20, 40, 60, 80, 100), labels = c("0-20", "21-40", "41-60", "61-80", "81-100"), include.lowest = TRUE),
         cch013_score_bin = cut(cch013_score, breaks = c(0, 20, 40, 60, 80, 100), labels = c("0-20", "21-40", "41-60", "61-80", "81-100"), include.lowest = TRUE)) %>%
  select(id, cch01_score_bin, cch012_score_r_bin, cch013_score_bin)

# Convert data into long format for ggalluvial
df_long2 <- df_sankey2 %>%
  pivot_longer(cols = -id,  
               names_to = "Stage", 
               values_to = "Score") %>%
  mutate(Score = as.factor(Score),  
         Stage = factor(Stage, levels = c("cch01_score_bin", "cch012_score_r_bin", "cch013_score_bin")))

# Create Sankey Diagram
ggplot(df_long2, aes(x = Stage, stratum = Score, alluvium = id, y = 1)) + 
  geom_flow(aes(fill = Score), alpha = 0.7) + 
  geom_stratum(width = 0.3, fill = "grey", color = "black") + 
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) + 
  theme_minimal() + 
  labs(title = "Sankey Diagram of CCH Score Variables (Binned)",
       x = "Password Exercises",
       y = "Count") + 
  scale_fill_brewer(palette = "Set2")

# PASSWORDS (NO UNSECURE)
# Select and reshape data
df_sankey2 <- df_sankey2 %>%
  select(id, cch01_score_bin, cch013_score_bin)

# Convert data into long format for ggalluvial
df_long2 <- df_sankey2 %>%
  pivot_longer(cols = -id,  
               names_to = "Stage", 
               values_to = "Score") %>%
  mutate(Score = as.factor(Score),  
         Stage = factor(Stage, levels = c("cch013_score_bin", "cch01_score_bin")))

# Create Sankey Diagram
ggplot(df_long2, aes(x = Stage, stratum = Score, alluvium = id, y = 1)) + 
  geom_flow(aes(fill = Score), alpha = 0.7) + 
  geom_stratum(width = 0.3, fill = "grey", color = "black") + 
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) + 
  theme_minimal() + 
  labs(title = "Sankey Diagram of CCH Score Variables (Binned)",
       x = "Password Exercises",
       y = "Count") + 
  scale_fill_brewer(palette = "Set2")

# PHISHING
# Select and reshape data
df_sankey <- survey_min %>%
  select(cch021_cch021_r, cch022_cch022, cch023_cch023_r) %>%
  drop_na() %>%
  mutate(id = row_number())  # Create an identifier for individuals

# Convert data into long format for ggalluvial
df_long <- df_sankey %>%
  pivot_longer(cols = -id,  # Keep the id column
               names_to = "Stage", 
               values_to = "Rating") %>%
  mutate(Rating = as.factor(Rating),  # Convert scores to categorical
         Stage = factor(Stage, levels = c("cch021_cch021_r", "cch022_cch022", "cch023_cch023_r")))  # Set x-axis order

# Create Sankey Diagram
ggplot(df_long, aes(x = Stage, stratum = Rating, alluvium = id, y = 1)) + 
  geom_flow(aes(fill = Rating), alpha = 0.7) + 
  geom_stratum(width = 0.3, fill = "grey", color = "black") + 
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) + 
  theme_minimal() + 
  labs(title = "Sankey Diagram of Phishing Examples",
       x = "Phishing Examples",
       y = "Count") + 
  scale_fill_brewer(palette = "Set2")  # Categorical colors

# WEBSITE SECURITY
# Select and reshape data
df_sankey1 <- survey_min %>%
  select(cch031_cch031_r, cch032_cch032_r, cch033_cch033) %>%
  drop_na() %>%
  mutate(id = row_number())  # Create an identifier for individuals

# Convert data into long format for ggalluvial
df_long1 <- df_sankey1 %>%
  pivot_longer(cols = -id,  
               names_to = "Stage", 
               values_to = "Rating") %>%
  mutate(Rating = as.factor(Rating),  
         Stage = factor(Stage, levels = c("cch031_cch031_r", "cch032_cch032_r", "cch033_cch033")))  

# Create Sankey Diagram
ggplot(df_long1, aes(x = Stage, stratum = Rating, alluvium = id, y = 1)) + 
  geom_flow(aes(fill = Rating), alpha = 0.7) + 
  geom_stratum(width = 0.3, fill = "grey", color = "black") + 
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) + 
  theme_minimal() + 
  labs(title = "Sankey Diagram of Website Examples",
       x = "Website Examples",
       y = "Count") + 
  scale_fill_brewer(palette = "Set2")
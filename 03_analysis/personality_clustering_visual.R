library(tidyverse)
library(ggtext)

## BIG 5
# Step 1: Convert to long format
df_long <- survey_clust_min %>%
  drop_na(cluster) %>%
  pivot_longer(
    cols = starts_with("big5_"),
    names_to = "trait",
    values_to = "score"
  )

# Step 2: Standardize scores (Z-scores within each trait)
df_long <- df_long %>%
  group_by(trait) %>%
  mutate(z_score = scale(score)[,1]) %>%
  ungroup()

# Step 3: Rename traits and factor them in Big Five order: E, A, C, N, O
df_long <- df_long %>%
  mutate(trait = dplyr::recode(trait,
                        "big5_extra" = "E", 
                        "big5_agree" = "A", 
                        "big5_consc" = "C", 
                        "big5_neuro" = "N", 
                        "big5_imag"  = "O"),
         trait = factor(trait, levels = c("E", "A", "C", "N", "O")))
         #trait = factor(trait, levels = c("N", "E", "O", "A", "C")))

# Step 4: Calculate mean and standard deviation per cluster x trait
summary_df <- df_long %>%
  group_by(cluster, trait) %>%
  summarise(
    mean = mean(z_score),
    sd = sd(z_score),
    .groups = 'drop'
  )

# Optional: Create custom labels for each cluster
#cluster_labels <- c("1" = "Average", "2" = "Self-centred", "3" = "Reserved", "4" = "Role model")
cluster_labels <- c(
  "1" = "<span style='color:darkorange'>Quietly Grounded</span>",
  "2" = "<span style='color:forestgreen'>Outgoing Entertainers</span>",
  "3" = "<span style='color:steelblue'>Focused Introverts</span>",
  "4" = "<span style='color:firebrick'>Balanced Intellectuals</span>"
)

# Step 5: Plot
ggplot(summary_df, aes(x = trait, y = mean, color = as.factor(cluster))) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, linewidth = 1) +
  geom_point(size = 5) +
  facet_wrap(~ cluster, labeller = as_labeller(cluster_labels), ncol = 2) +
  #scale_color_manual(values = c("1" = "steelblue", "2" = "darkorange", "3" = "forestgreen", "4" = "firebrick")) +
  scale_color_manual(values = c("1" = "darkorange", "2" = "forestgreen", "3" = "steelblue", "4" = "firebrick")) +
  theme_minimal(base_size = 14) +
  labs(x = "Big Five Traits", y = "Cluster Z score") +
  theme(
    legend.position = "none",
    panel.spacing = unit(1.5, "lines"),  # More space between panels
    strip.text = element_markdown(size = 12),
    axis.title.x = element_text(margin = margin(t = 15)), 
    axis.title.y = element_text(margin = margin(r = 12))  
  )

## Other attributes BY CHARACTERISTIC
cols <- c("cch01", "cch02", "cch03", "ra", "fomo1", "fomo3", "fomo2", "csm", "pv", "pe", "tue", "tud", "tr", "cchk", "ccha", "cchb", "si", "co", "ori", "orh")
# Step 1: Convert to long format
df_long <- survey_clust_min %>%
  drop_na(c("cluster", cols)) %>%
  pivot_longer(
    cols = cols,
    names_to = "characteristic",
    values_to = "score"
  )

# Step 2: Standardize scores (Z-scores within each trait)
df_long <- df_long %>%
  group_by(characteristic) %>%
  mutate(z_score = scale(score)[,1]) %>%
  ungroup()

df_long <- df_long %>%
  mutate(characteristic = dplyr::recode(characteristic,
                                        "cch01" = "CCH_PW", 
                                        "cch02" = "CCH_PH", 
                                        "cch03" = "CCH_WS", 
                                        "ra" = "RA", 
                                        "fomo1" = "FOMO_S", 
                                        "fomo3" = "SMU", 
                                        "fomo2" = "NS", 
                                        "csm" = "CSM", 
                                        "pv" = "PV", 
                                        "pe" = "PE", 
                                        "tue" = "TUE", 
                                        "tud" = "TUD", 
                                        "tr" = "TR", 
                                        "cchk" = "CCH_K", 
                                        "ccha" = "CCH_A", 
                                        "cchb" = "CCH_B", 
                                        "si" = "SI", 
                                        "co" = "CO", 
                                        "ori" = "ORI", 
                                        "orh" = "ORH"),
         characteristic = factor(characteristic, levels = c("CCH_PW","CCH_PH","CCH_WS","RA","FOMO_S","SMU","NS","PV","PE", 
                                                            "TUE","TUD","TR","SI","CO","ORI","ORH","CCH_K","CCH_A","CCH_B","CSM")))

# Step 4: Calculate mean and standard deviation per cluster x trait
summary_df <- df_long %>%
  group_by(cluster, characteristic) %>%
  summarise(
    mean = mean(z_score, na.rm = TRUE),
    sd = sd(z_score, na.rm = TRUE),
    .groups = 'drop'
  )

ggplot(summary_df, aes(x = as.factor(cluster), y = mean, fill = as.factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_hline(yintercept = 0, color = "gray45", linewidth = 0.6) + 
  facet_wrap(~ characteristic, ncol = 5, scales = "fixed") +
  scale_fill_manual(values = c("1" = "darkorange", "2" = "forestgreen", "3" = "steelblue", "4" = "firebrick")) +
  theme_minimal(base_size = 14) +
  labs(x = "Cluster", y = "Z score") +
  theme(
    legend.position = "none",
    panel.spacing = unit(1.2, "lines"),
    strip.text = element_text(size = 10),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 12)),
    panel.grid.major.x = element_blank(),  # remove vertical grid lines
    panel.grid.minor.x = element_blank()
  )
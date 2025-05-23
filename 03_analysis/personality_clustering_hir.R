# Load required libraries
library(dplyr)
library(factoextra)
library(cluster)
library(ggplot2)

# Remove other columns and handle missing values
df_clustering <- survey_fac_min[c("seed", "big5_extra", "big5_agree", "big5_consc", "big5_neuro", "big5_imag")]
df_clustering <- df_clustering %>% drop_na()

# Measuring the cluster tendency
library("hopkins")
set.seed(1)
hopkins(df_clustering[c("big5_extra", "big5_agree", "big5_consc", "big5_neuro", "big5_imag")])

# Summary statistics
summary(df_clustering[c("big5_extra", "big5_agree", "big5_consc", "big5_neuro", "big5_imag")])

# Compute the distance matrix (Euclidean)
distance <- dist(df_clustering[c("big5_extra", "big5_agree", "big5_consc", "big5_neuro", "big5_imag")], method = "euclidean")
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Determine optimal number of clusters using silhouette method
fviz_nbclust(df_clustering[c("big5_extra", "big5_agree", "big5_consc", "big5_neuro", "big5_imag")], 
             FUN = hcut, method = "silhouette")

# Perform hierarchical clustering using Ward's method
hc <- hclust(distance, method = "ward.D2")

# Plot the dendrogram
plot(hc, labels = FALSE, hang = -1, main = "Hierarchical Clustering Dendrogram")

# Cut the dendrogram into 2 clusters
df_clustering$cluster <- cutree(hc, k = 4)

# Visualize clusters
fviz_cluster(list(data = df_clustering[c("big5_extra", "big5_agree", "big5_consc", "big5_neuro", "big5_imag")], 
                  cluster = df_clustering$cluster))

# Join df_clustering with survey_min based on the seed column
survey_clust <- survey_min %>% 
  select(-c(constructs$big5_extra, constructs$big5_agree, constructs$big5_consc, constructs$big5_neuro, constructs$big5_imag)) %>% 
  left_join(df_clustering, by = "seed")

# Summary table by cluster
summary_table <- survey_clust %>%
  select(-survey_name, -id, -seed, -g01q48, -dem14) %>%  # Exclude unwanted columns
  tbl_summary(
    by = cluster,  
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing = "no",
    type = list(where(is.numeric) ~ "continuous")
  ) %>%
  add_p()

# Print summary table
print(summary_table)

# Save the summary table as an HTML file
summary_table %>%
  as_gt() %>%
  gt::gtsave(filename = "pers_cluster_4_hir_table.html", path = "~/MA/03_output/reports")

# Remove object from workspace
rm(summary_table)

# Remove other columns
df_clustering <- survey_dim_min[c("seed", "big5_big1", "big5_big2", "big5_big3", "big5_big4", "big5_big5")]

names(df_clustering)[names(df_clustering) == 'big5_big1'] <- 'big5_extra'
names(df_clustering)[names(df_clustering) == 'big5_big2'] <- 'big5_agree'
names(df_clustering)[names(df_clustering) == 'big5_big3'] <- 'big5_consc'
names(df_clustering)[names(df_clustering) == 'big5_big4'] <- 'big5_neuro'
names(df_clustering)[names(df_clustering) == 'big5_big5'] <- 'big5_imag'

df_clustering <- df_clustering %>% drop_na()

# Do kmeans clustering (https://rpubs.com/mlorens/bigfive-clustering)

## Analysis of variables to be clustered
summary(df_clustering[c("big5_extra", "big5_agree", "big5_consc", "big5_neuro", "big5_imag")])

## Measuring the cluster tendency
library("hopkins")
set.seed(123)
hopkins(df_clustering[c("big5_extra", "big5_agree", "big5_consc", "big5_neuro", "big5_imag")])

## Ordered Dissimilarity Matrix
library(factoextra)
distance <- get_dist(df_clustering[c("big5_extra", "big5_agree", "big5_consc", "big5_neuro", "big5_imag")])
fviz_dist(distance, show_labels = FALSE, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Determine number of clusters
fviz_nbclust(df_clustering[c("big5_extra", "big5_agree", "big5_consc", "big5_neuro", "big5_imag")], kmeans, method = "silhouette")

fviz_nbclust(df_clustering[, c("big5_extra", "big5_agree", "big5_consc", "big5_neuro", "big5_imag")], 
             kmeans, method = "wss")

gap_stat <- clusGap(df_clustering[, c("big5_extra", "big5_agree", "big5_consc", "big5_neuro", "big5_imag")], 
                    FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

# Cluster
kmeans_clust <- eclust(df_clustering[c("big5_extra", "big5_agree", "big5_consc", "big5_neuro", "big5_imag")], k=4, FUNcluster="kmeans", hc_metric="euclidean", graph=FALSE)

# PAM
pam_clust <- eclust(df_clustering[c("big5_extra", "big5_agree", "big5_consc", "big5_neuro", "big5_imag")], k=4, FUNcluster='pam', hc_metric="euclidean", graph=FALSE)
fviz_cluster(pam_clust)
fviz_silhouette(pam_clust, palette = "jco", ggtheme = theme_classic())

# GMM 
install.packages("ClusterR")
library(ClusterR)
gmm = GMM(df_clustering[c("big5_extra", "big5_agree", "big5_consc", "big5_neuro", "big5_imag")], gaussian_comps=4, dist_mode="eucl_dist", seed=1)
GMM_clusters = predict(gmm, newdata=df_clustering[c("big5_extra", "big5_agree", "big5_consc", "big5_neuro", "big5_imag")])

Optimal_Clusters_GMM(df_clustering[c("big5_extra", "big5_agree", "big5_consc", "big5_neuro", "big5_imag")], max_clusters = 10, criterion = "BIC", 
                     
                     dist_mode = "maha_dist", seed_mode = "random_subset",
                     
                     km_iter = 10, em_iter = 10, var_floor = 1e-10, 
                     
                     plot_data = T)

# Rebind columns
df_clustering["cluster"] <- kmeans_clust[["cluster"]]

# Viz 
fviz_cluster(kmeans_clust)
fviz_silhouette(kmeans_clust, palette = "jco", ggtheme = theme_classic())
# Join df_clustering with copy_df based on the seed column
survey_clust <- survey_min %>% select(-c(constructs$big5_extra, constructs$big5_agree, constructs$big5_consc, constructs$big5_neuro, constructs$big5_imag))
survey_clust <- survey_clust %>% left_join(df_clustering, by = "seed")

survey_clust_min <- survey_dim_min %>% select(-c("big5_big1", "big5_big2", "big5_big3", "big5_big4", "big5_big5"))
survey_clust_min <- survey_clust_min %>% left_join(df_clustering, by = "seed")

# Function to add significance stars
add_significance_stars <- function(x) {
  dplyr::case_when(
    x < 0.001 ~ paste0("<mark>", style_pvalue(x), "***</mark>"),
    x < 0.01 ~ paste0("<mark>", style_pvalue(x), "**</mark>"),
    x < 0.05 ~ paste0("<mark>", style_pvalue(x), "*</mark>"),
    TRUE ~ style_pvalue(x)
  )
}

# FULL Variant
summary_table <- survey_clust %>%
  select(-survey_name, -id, -seed, -g01q48, -dem14) %>%  # Exclude unwanted columns
  tbl_summary(
    by = cluster,  # Use dynamic column name
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing = "no",
    type = list(where(is.numeric) ~ "continuous")
  ) %>%
  add_p(pvalue_fun = add_significance_stars)

# Print summary table
print(summary_table)

# Save the summary table as an HTML file
summary_table %>%
  as_gt() %>%
  gt::gtsave(filename = "pers_cluster_4_table.html", path = "~/MA/03_output/reports")

# MIN Variant
summary_table <- survey_clust_min %>%
  select(-survey_name, -id, -seed) %>%  # Exclude unwanted columns
  tbl_summary(
    by = cluster,  # Use dynamic column name
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing = "no",
    type = list(where(is.numeric) ~ "continuous")
  ) %>%
  add_p(pvalue_fun = add_significance_stars)

# Print summary table
print(summary_table)

# Save the summary table as an HTML file
summary_table %>%
  as_gt() %>%
  gt::gtsave(filename = "pers_cluster_4_min_table.html", path = "~/MA/03_output/reports")

# Remove object from workspace
rm(summary_table)

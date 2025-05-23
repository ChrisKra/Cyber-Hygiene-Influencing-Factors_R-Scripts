library(tidyr)

survey_cor <- na.omit(survey_dim_min)

# Rename columns
names(survey_cor)[names(survey_cor) == 'big5_big1'] <- 'big5_extra'
names(survey_cor)[names(survey_cor) == 'big5_big2'] <- 'big5_agree'
names(survey_cor)[names(survey_cor) == 'big5_big3'] <- 'big5_consc'
names(survey_cor)[names(survey_cor) == 'big5_big4'] <- 'big5_neuro'
names(survey_cor)[names(survey_cor) == 'big5_big5'] <- 'big5_imag'
names(survey_cor)[names(survey_cor) == 'fomo1'] <- 'fomo_social'
names(survey_cor)[names(survey_cor) == 'fomo3'] <- 'fomo_socmed'
names(survey_cor)[names(survey_cor) == 'fomo2'] <- 'fomo_novel'

# Constructs only
corrplot(cor(survey_cor %>% 
               select(where(is.numeric)) %>% 
               select(-group_time_exercise) %>% 
               drop_na()), 
         method = "color",
         tl.col = "black")  # Set text labels to black

# List correlations
survey_cor %>%
  select(where(is.numeric), -group_time_exercise) %>%
  drop_na() %>%
  cor() %>%
  as.table() %>%
  as.data.frame() %>%
  filter(Var1 != Var2) %>%
  rowwise() %>%
  mutate(pair = paste(sort(c(Var1, Var2)), collapse = " - ")) %>%
  distinct(pair, .keep_all = TRUE) %>%
  ungroup() %>%
  arrange(desc(Freq)) %>%
  print(n=30)


# Full plot
corrplot(cor(survey_min %>% 
               select(where(is.numeric)) %>% 
               drop_na()), 
         method = "color",
         tl.col = "black",
         tl.cex = 0.5)  # Set text labels to black
#rm(survey_cor)
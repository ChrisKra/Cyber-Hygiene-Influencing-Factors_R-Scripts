# Prepare dataframe the removes outliers
#survey_reg <- survey_dim_min[-c(4, 174, 220, 203, 154), ]
survey_reg <- na.omit(survey_dim_min)

# Rename columns
names(survey_reg)[names(survey_reg) == 'big5_big1'] <- 'big5_extra'
names(survey_reg)[names(survey_reg) == 'big5_big2'] <- 'big5_agree'
names(survey_reg)[names(survey_reg) == 'big5_big3'] <- 'big5_consc'
names(survey_reg)[names(survey_reg) == 'big5_big4'] <- 'big5_neuro'
names(survey_reg)[names(survey_reg) == 'big5_big5'] <- 'big5_imag'
names(survey_reg)[names(survey_reg) == 'fomo1'] <- 'fomo_social'
names(survey_reg)[names(survey_reg) == 'fomo3'] <- 'fomo_socmed'
names(survey_reg)[names(survey_reg) == 'fomo2'] <- 'fomo_novel'

#survey_reg <- survey_reg %>% filter(!survey_name %in% c("survey967691_main_uk_1", "survey226331_main_de_1"))
survey_reg <- subset(survey_reg, !(rownames(survey_reg) %in% c("444", "4", "352", "174", "478", "220", "677", "203", "208", "627")))

# KNOWLEDGE
model_mmr <- lm(cbind(cchk, ccha, cchb) ~ big5_extra + big5_agree + big5_consc + big5_neuro + big5_imag +
                  ra + fomo_social + fomo_socmed + fomo_novel +
                  pv + pe + tue + tud + tr + si + co + ori + orh + 
                  dem1 + dem2 + dem3 + dem4 + dem5 + dem6 + dem7 + dem8 + dem9 + dem10 + dem11 + dem12 + dem13, data = survey_reg)
summary(manova(model_mmr))
summary(model_mmr)

library(broom)
library(dplyr)
# Create formatted table
regression_table <- tidy(model_mmr) %>%
  mutate(
    `β (SE)` = sprintf("%.2f (%.2f)", estimate, std.error),
    `t, p` = sprintf("%.2f, %s%s",
                     statistic,
                     ifelse(p.value < 0.001, "<.001", sprintf("%.3f", p.value)),
                     case_when(
                       p.value < 0.001 ~ "***",
                       p.value < 0.01 ~ "**",
                       p.value < 0.05 ~ "*",
                       p.value < 0.1 ~ ".",
                       TRUE ~ ""
                     )
    )
  ) %>%
  select(term, `β (SE)`, `t, p`)

# View the result
print(regression_table)

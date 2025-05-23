# Prepare dataframe the removes outliers
survey_reg <- survey_dim_min[-c(4, 174, 220, 203, 154), ]

# Rename columns
names(survey_reg)[names(survey_reg) == 'big5_big1'] <- 'big5_extra'
names(survey_reg)[names(survey_reg) == 'big5_big2'] <- 'big5_agree'
names(survey_reg)[names(survey_reg) == 'big5_big3'] <- 'big5_consc'
names(survey_reg)[names(survey_reg) == 'big5_big4'] <- 'big5_neuro'
names(survey_reg)[names(survey_reg) == 'big5_big5'] <- 'big5_imag'
names(survey_reg)[names(survey_reg) == 'fomo1'] <- 'fomo_social'
names(survey_reg)[names(survey_reg) == 'fomo3'] <- 'fomo_socmed'
names(survey_reg)[names(survey_reg) == 'fomo2'] <- 'fomo_novel'

# KNOWLEDGE
lm_cchk <- lm(cchk ~ big5_extra + big5_agree + big5_consc + big5_neuro + big5_imag +
               ra + fomo_social + fomo_socmed + fomo_novel +
               pv + pe + tue + tud + tr + si + co + ori + orh , data = survey_reg)
summary(lm_cchk)

# ATTITUDE 
lm_ccha <- lm(ccha ~ big5_extra + big5_agree + big5_consc + big5_neuro + big5_imag +
                ra + fomo_social + fomo_socmed + fomo_novel +
                pv + pe + tue + tud + tr + si + co + ori + orh, data = survey_reg)
summary(lm_ccha)

# BEHAVIOR
lm_cchb <- lm(cchb ~ big5_extra + big5_agree + big5_consc + big5_neuro + big5_imag +
                ra + fomo_social + fomo_socmed + fomo_novel +
                pv + pe + tue + tud + tr + si + co + ori + orh, data = survey_reg, na.action = na.exclude)
summary(lm_cchb)

#Other
# Multicollinearity check
library(car)
vif(lm_cchb)  # Variance Inflation Factor (VIF) should be < 5

# Residual diagnostics
#par(mfrow=c(2,2))
par(mar = c(4, 4, 2, 2))  # Adjust margins to be smaller
plot(lm_cchb)  # Check for normality, homoscedasticity, and influential points

# Clean up 
rm(lm_ccha, lm_cchb, lm_cchk, lmTemp, survey_reg)

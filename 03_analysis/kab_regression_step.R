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
lm_cchk <- lm(cchk ~ big5_extra + big5_agree + big5_consc + big5_neuro + big5_imag +
                ra + fomo_social + fomo_socmed + fomo_novel +
                pv + pe + tue + tud + tr + si + co + ori + orh + 
                dem1 + dem2 + dem3 + dem4 + dem5 + dem6 + dem7 + dem8 + dem9 + dem10 + dem11 + dem12 + dem13, data = survey_reg)
summary(lm_cchk)
# lm_cchk_step <- step(lm_cchk, direction = "both")
# summary(lm_cchk_step)

# ATTITUDE 
lm_ccha <- lm(ccha ~ big5_extra + big5_agree + big5_consc + big5_neuro + big5_imag +
                ra + fomo_social + fomo_socmed + fomo_novel +
                pv + pe + tue + tud + tr + si + co + ori + orh + 
                dem1 + dem2 + dem3 + dem4 + dem5 + dem6 + dem7 + dem8 + dem9 + dem10 + dem11 + dem12 + dem13, data = survey_reg)
summary(lm_ccha)
# lm_ccha_step <- step(lm_ccha, direction = "both")
# summary(lm_ccha_step)

# BEHAVIOR
lm_cchb <- lm(cchb ~ big5_extra + big5_agree + big5_consc + big5_neuro + big5_imag +
                ra + fomo_social + fomo_socmed + fomo_novel +
                pv + pe + tue + tud + tr + si + co + ori + orh + 
                dem1 + dem2 + dem3 + dem4 + dem5 + dem6 + dem7 + dem8 + dem9 + dem10 + dem11 + dem12 + dem13, data = survey_reg, na.action = na.exclude)
summary(lm_cchb)
# lm_cchb_step <- step(lm_cchb, direction = "both")
# summary(lm_cchb_step)

#Other
# Multicollinearity check
library(car)
view(vif(lm_cchb))  # Variance Inflation Factor (VIF) should be < 5

# Residual diagnostics
par(mfrow=c(2,2))
par(mar = c(4, 4, 2, 2))  # Adjust margins to be smaller
plot(lm_cchb_step)  # Check for normality, homoscedasticity, and influential points

# Clean up 
rm(lm_ccha, lm_ccha_step, lm_cchb, lm_cchb_step, lm_cchk, lm_cchk_step, survey_reg)
dev.off()

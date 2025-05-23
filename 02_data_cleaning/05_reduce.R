library(lavaan)

# Generate survey_min with unnecessary columns removed
survey_min <- survey

columns_to_drop <- c(
  "submitdate",
  "lastpage",
  "startlanguage",
  "startdate",
  "datestamp",
  "refurl",
  "cch01",
  "cch01_zxcvbn",
  "cch01_password-meter",
  "cch012",
  "cch012_zxcvbn",
  "cch012_password-meter",
  "cch012_score",
  "cch013",
  "cch013_zxcvbn",
  "cch013_password-meter",
  "cch012_score",
  "cch021_cch021",
  "cch023_cch023",
  "cch031_cch031",
  "cch032_cch032",
  "big5_big13",
  "big5_big14",
  "big5_big23",
  "big5_big24",
  "big5_big33",
  "big5_big34",
  "big5_big43",
  "big5_big44",
  "big5_big52",
  "big5_big53",
  "big5_big54",
  "ra_ra1",
  "ra_ra2",
  "ra_ra3",
  "ra_ra5",
  "cchk_cch41",
  "cchk_cch51",
  "cchk_cch61",
  "ccha_cch12",
  "ccha_cch22",
  "ccha_cch42",
  "ccha_cch52",
  "ccha_cch62",
  "ccha_cch72",
  "ccha_cch82",
  "cchb_cch23",
  "cchb_cch33",
  "cchb_cch73",
  "g01q46",
  "cch01time",
  "cch012time",
  "cch013time",
  "cch021time",
  "cch022time",
  "cch023time",
  "cch031time",
  "cch032time",
  "cch033time",
  "group_time473",
  "big5time",
  "ra7time",
  "ra_time",
  "group_time472",
  "fomo1time",
  "fomo3time",
  "fomo2time",
  "group_time481",
  "csb_time",
  "csb2time",
  "group_time477",
  "pv_time",
  "pe_time",
  "group_time474",
  "tue_time",
  "tud_time",
  "tr_time",
  "group_time531",
  "cchk_time",
  "ccha_time",
  "cchb_time",
  "group_time479",
  "si_time",
  "co_time",
  "group_time480",
  "ori_time",
  "orh_time",
  "group_time475",
  "g01q48time",
  "dem1time",
  "dem2time",
  "dem3time",
  "dem4time",
  "dem5time",
  "dem6time",
  "dem7time",
  "dem8time",
  "dem9time",
  "dem10time",
  "dem11time",
  "dem12time",
  "dem13time",
  "dem14time",
  #"pe_pe7",
  #"dem15_dem15",
  "group_time810",
  "group_time803",
  "group_time802",
  "group_time809",
  "group_time806",
  "group_time804",
  "group_time811",
  "group_time807",
  "group_time808",
  "group_time805",
  "dem15time",
  "group_time827",
  "group_time826", 
  "group_time833", 
  "group_time830", 
  "group_time828", 
  "group_time835", 
  "group_time831", 
  "group_time832", 
  "group_time829", 
  "group_time837", 
  "group_time836", 
  "group_time843", 
  "group_time840", 
  "group_time838", 
  "group_time845", 
  "group_time841", 
  "group_time842", 
  "group_time839"
)

survey_min <- survey_min[, !(names(survey_min) %in% columns_to_drop)]
rm(columns_to_drop)

# Generate survey_dim_min with unnecessary columns removed and dimensions reduced
# AVERAGING APPROACH: assumes that all indicators contribute equally to the latent variable.
survey_dim_min <- survey_min[, c("survey_name", "id", "seed")]

# Practical excersizes
survey_dim_min["cch01"] <- rowMeans(survey_min[, c("cch01_score", "cch012_score_r", "cch013_score")], na.rm =
                                      TRUE)
survey_dim_min["cch02"] <- rowMeans(survey_min[, c("cch021_cch021_r", "cch022_cch022", "cch023_cch023_r")], na.rm =
                                      TRUE)
survey_dim_min["cch03"] <- rowMeans(survey_min[, c("cch031_cch031_r", "cch032_cch032_r", "cch033_cch033")], na.rm =
                                      TRUE)

# Personality Big 5
survey_dim_min["big5_big1"] <- rowMeans(survey_min[, c("big5_big11", "big5_big12", "big5_big13_r", "big5_big14_r")], na.rm = TRUE)
survey_dim_min["big5_big2"] <- rowMeans(survey_min[, c("big5_big21", "big5_big22", "big5_big23_r", "big5_big24_r")], na.rm = TRUE)
survey_dim_min["big5_big3"] <- rowMeans(survey_min[, c("big5_big31", "big5_big32", "big5_big33_r", "big5_big34_r")], na.rm = TRUE)
survey_dim_min["big5_big4"] <- rowMeans(survey_min[, c("big5_big41", "big5_big42", "big5_big43_r", "big5_big44_r")], na.rm = TRUE)
survey_dim_min["big5_big5"] <- rowMeans(survey_min[, c("big5_big51", "big5_big52_r", "big5_big53_r", "big5_big54_r")], na.rm = TRUE)

# Risk affinity
survey_dim_min["ra"] <- rowMeans(survey_min[, c("ra7_ra1",
                                                "ra_ra1_r",
                                                "ra_ra2_r",
                                                "ra_ra3_r",
                                                "ra_ra4",
                                                "ra_ra5_r",
                                                "ra_ra6")], na.rm = TRUE)

# Fomo
survey_dim_min["fomo1"] <- rowMeans(survey_min[, c("fomo1_fomo11",
                                                   "fomo1_fomo12",
                                                   "fomo1_fomo13",
                                                   "fomo1_fomo14")], na.rm = TRUE)
survey_dim_min["fomo3"] <- rowMeans(survey_min[, c("fomo3_fomo31",
                                                   "fomo3_fomo32",
                                                   "fomo3_fomo33",
                                                   "fomo3_fomo34",
                                                   "fomo3_fomo35")], na.rm = TRUE)
survey_dim_min["fomo2"] <- rowMeans(survey_min[, c(
  "fomo2_fom021",
  "fomo2_fom022",
  "fomo2_fom023",
  "fomo2_fom024",
  "fomo2_fom025",
  "fomo2_fom026",
  "fomo2_fom027"
)], na.rm = TRUE)

# Cyber security measures
survey_dim_min["csm"] <- rowMeans(survey_min[, c(
  "csb_csm4",
  "csb_csm6",
  "csb_csm5",
  "csb_csm2",
  "csb_csm1",
  "csb_csm3",
  "csb_csm10",
  "csb_csm7",
  "csb_csm8",
  "csb_csm9",
  "csb2_csm11",
  "csb2_csm12",
  "csb2_csm13",
  "csb2_csm14",
  "csb2_csm15",
  "csb2_csm16",
  "csb2_csm17",
  "csb2_csm18"
)], na.rm = TRUE)

survey_dim_min["pv"] <- rowMeans(survey_min[, c("pv_pv1", "pv_pv2", "pv_pv3", "pv_pv4")], na.rm = TRUE)
# survey_dim_min["pe"] <- rowMeans(survey_min[, c("pe_pe1", "pe_pe2", "pe_pe3", "pe_pe4", "pe_pe5", "pe_pe6")], na.rm = TRUE)
survey_dim_min["pe"] <- survey_min$pe
survey_dim_min["tue"] <- rowMeans(survey_min[, c("tue_tue1",
                                                 "tue_tue2",
                                                 "tue_tue3",
                                                 "tue_tue4",
                                                 "tue_tue5",
                                                 "tue_tue6")], na.rm = TRUE)
survey_dim_min["tud"] <- rowMeans(survey_min[, c("tud_tud1", "tud_tud2", "tud_tud3")], na.rm = TRUE)
survey_dim_min["tr"] <- rowMeans(survey_min[, c("tr_tr1", "tr_tr2", "tr_tr3", "tr_tr4")], na.rm = TRUE)
survey_dim_min["cchk"] <- rowMeans(survey_min[, c(
  "cchk_cch11",
  "cchk_cch21",
  "cchk_cch31",
  "cchk_cch41_r",
  "cchk_cch51_r",
  "cchk_cch61_r",
  "cchk_cch71",
  "cchk_cch81"
)], na.rm = TRUE)
survey_dim_min["ccha"] <- rowMeans(survey_min[, c(
  "ccha_cch12_r",
  "ccha_cch22_r",
  "ccha_cch32",
  "ccha_cch42_r",
  "ccha_cch52_r",
  "ccha_cch62_r",
  "ccha_cch72_r",
  "ccha_cch82_r"
)], na.rm = TRUE)
survey_dim_min["cchb"] <- rowMeans(survey_min[, c(
  "cchb_cch13",
  "cchb_cch23_r",
  "cchb_cch33_r",
  "cchb_cch43",
  "cchb_cch53",
  "cchb_cch63",
  "cchb_cch73_r",
  "cchb_cch83"
)], na.rm = TRUE)
survey_dim_min["si"] <- rowMeans(survey_min[, c("si_si1", "si_si2", "si_si3", "si_si4")], na.rm = TRUE)
survey_dim_min["co"] <- rowMeans(survey_min[, c("co_co2", "co_co1", "co_co3", "co_co4")], na.rm = TRUE)
survey_dim_min["ori"] <- rowMeans(survey_min[, c("ori_ori1", "ori_ori2", "ori_ori3")], na.rm = TRUE)
survey_dim_min["orh"] <- rowMeans(survey_min[, c("orh_orh1", "orh_orh2", "orh_orh3")], na.rm = TRUE)

survey_dim_min <- survey_dim_min %>%
  bind_cols(survey_min[, c("dem1", "dem2", "dem3", "dem4", "dem5", "dem6", "dem7", "dem8", "dem9", "dem10", "dem11", "dem12", "dem13", "group_time_exercise")])

# Generate survey_fac_min with unnecessary columns removed and dimensions reduced
# FACTOR SCORES FROM CFA: accounts for the relationships between indicators and the latent variable.

survey_fac_min <- survey_min[, c("survey_name", "id", "seed")]

## Using the construct map
for (construct in names(constructs)[!names(constructs) %in% c("pe", "dem", "other")]) {
  # Define the model
  model <- paste(construct, paste(constructs[[construct]], collapse = "+"), sep = "=~")
  print(model)
  
  # Run CFA
  fit <- cfa(model, data = survey_min)
  
  # Extract factor scores
  factor_scores <- lavPredict(fit)
  
  # Create a vector to store scores, initialized with NA
  scores <- rep(NA, nrow(survey_min))
  
  # Align factor scores with the original dataset
  complete_cases <- complete.cases(survey_min[, constructs[[construct]]])
  scores[complete_cases] <- factor_scores
  
  # Add factor scores to the dataframe
  survey_fac_min[[construct]] <- scores
  
  # Clean up 
  rm(model, construct, complete_cases, fit, factor_scores, scores)
}

survey_fac_min <- survey_fac_min %>%
  bind_cols(survey_min[, c("pe", "dem1", "dem2", "dem3", "dem4", "dem5", "dem6", "dem7", "dem8", "dem9", "dem10", "dem11", "dem12", "dem13", "group_time_exercise")])

# Generate survey_fac with unnecessary columns removed, dimensions reduced but still containing some individual items
survey_fac <- subset(survey_fac_min, select = -c(ex_passw, ex_phish, ex_web, pe, tud))
survey_fac <- survey_fac %>%
  add_column(cch01_score = survey_min$cch01_score, .after = "seed")
survey_fac <- survey_fac %>%
  add_column(cch012_score_r = survey_min$cch012_score_r, .after = "cch01_score")
survey_fac <- survey_fac %>%
  add_column(cch013_score = survey_min$cch013_score, .after = "cch012_score_r")
survey_fac <- survey_fac %>%
  add_column(cch021_cch021_r = survey_min$cch021_cch021_r, .after = "cch013_score")
survey_fac <- survey_fac %>%
  add_column(cch022_cch022 = survey_min$cch022_cch022, .after = "cch021_cch021_r")
survey_fac <- survey_fac %>%
  add_column(cch023_cch023_r = survey_min$cch023_cch023_r, .after = "cch022_cch022")
survey_fac <- survey_fac %>%
  add_column(cch031_cch031_r = survey_min$cch031_cch031_r, .after = "cch023_cch023_r")
survey_fac <- survey_fac %>%
  add_column(cch032_cch032_r = survey_min$cch032_cch032_r, .after = "cch031_cch031_r")
survey_fac <- survey_fac %>%
  add_column(cch033_cch033 = survey_min$cch033_cch033, .after = "cch032_cch032_r")

survey_fac <- survey_fac %>%
  add_column(pe_pe1 = survey_min$pe_pe1, .after = "pv")
survey_fac <- survey_fac %>%
  add_column(pe_pe2 = survey_min$pe_pe2, .after = "pe_pe1")
survey_fac <- survey_fac %>%
  add_column(pe_pe3 = survey_min$pe_pe3, .after = "pe_pe2")
survey_fac <- survey_fac %>%
  add_column(pe_pe4 = survey_min$pe_pe4, .after = "pe_pe3")
survey_fac <- survey_fac %>%
  add_column(pe_pe5 = survey_min$pe_pe5, .after = "pe_pe4")
survey_fac <- survey_fac %>%
  add_column(pe_pe6 = survey_min$pe_pe6, .after = "pe_pe5")

survey_fac <- survey_fac %>%
  add_column(tud_tud1 = survey_min$tud_tud1, .after = "tue")
survey_fac <- survey_fac %>%
  add_column(tud_tud2 = survey_min$tud_tud2, .after = "tud_tud1")
survey_fac <- survey_fac %>%
  add_column(tud_tud3 = survey_min$tud_tud3, .after = "tud_tud2")